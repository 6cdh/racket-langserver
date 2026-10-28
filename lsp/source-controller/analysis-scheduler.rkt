#lang racket/base

;; Source attempts and owner acceptance for Check Syntax and Resyntax.
;; Each kind has its own lazy pool for FIFO admission and physical capacity.
;; This module owns same-revision sharing, supersession, and
;; acceptance-before-completion. `settle!` removes a source attempt.
;; A pool's `settle-pool-task!` is logical task settlement and may notify
;; before physical cleanup. Cross-file rules are in ../analysis-lifecycle.md.
(require racket/async-channel
         racket/contract
         "../../common/source.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "completion-event.rkt"
         "../../common/time.rkt"
         "place-runner.rkt")

(provide Analysis-Scheduler?
         make-analysis-scheduler
         scheduled-analysis-event
         request-analysis!
         schedule-resyntax!
         cancel-source-analysis!
         stop-analysis-scheduler!
         cancel-ineligible-analysis!
         Analysis-Completed?
         finish-analysis-task!)

;; One source attempt until owner acceptance. Distinct from the pool task that
;; executes it: this registry is revoked on source changes and outlives
;; execution until accept returns. The pool's task table owns per-task
;; deadlines and logical settlement; merging them would mix source lifetime
;; with physical capacity.
;;
;; parent-completion: Installed analysis that a Resyntax attempt must still match.
;; completion: Waiters join this attempt's event; resolved when `settle!` runs.
;; deadline: Absolute milliseconds captured at enqueue, including FIFO wait.
;; task: Opaque pool handle for cancellation; not a worker identity.
(struct/contract Analysis-Attempt
  ([snapshot Source-Snapshot?]
   [kind (or/c 'check-syntax 'resyntax)]
   [parent-completion (or/c #f evt?)]
   [completion Completion-Event?]
   [deadline real?]
   [(task #:mutable) (or/c #f Place-Task?)]))

;; Source-attempt registry and per-kind pool handles. Does not own FIFO or slots.
;;
;; events: Logical settlement envelopes for owner acceptance.
;; analyze: `#f` uses the production Check Syntax adapter; a procedure is a
;;   local injected task in that pool.
;; check-syntax-runner: Check Syntax pool; `#:slots` is its capacity.
;; resyntax-runner: Resyntax pool; always one slot.
;; resyntax: `#f` disables follow-on work; `'place` uses the production adapter;
;;   a procedure is a local injected task in the Resyntax pool.
;; timeout: Relative seconds; `enqueue!` captures one absolute deadline.
;; attempts: Active attempts keyed by source id and kind.
(struct/contract Analysis-Scheduler
  ([events async-channel?]
   [analyze (or/c #f (-> Source-Snapshot? Analysis-Product?))]
   [check-syntax-runner Place-Runner?]
   [resyntax-runner Place-Runner?]
   [resyntax (or/c #f 'place (-> string? string? list?))]
   [timeout (and/c real? (>=/c 0))]
   [attempts (hash/c (cons/c Source-Id? (or/c 'check-syntax 'resyntax))
                     Analysis-Attempt? #:immutable #f)]
   [(stopped? #:mutable) boolean?]))

;; The event envelope carries an identity privately; replay cannot revive an
;; attempt removed by completion, cancellation, or a same-revision retry.
(struct/contract Analysis-Completed
  ([attempt Analysis-Attempt?]
   [result any/c]))

(define (make-analysis-scheduler events #:analyze [analyze #f] #:resyntax resyntax
                                 #:timeout [timeout 90] #:slots [slots 4]
                                 #:place-start-guard [place-start-guard (lambda (thunk) (thunk))])
  (Analysis-Scheduler events analyze
                      (make-place-runner #:slots slots #:place-start-guard place-start-guard)
                      (make-place-runner #:slots 1 #:place-start-guard place-start-guard)
                      resyntax timeout (make-hash) #f))

(define (attempt-key attempt)
  (cons (Source-Snapshot-id (Analysis-Attempt-snapshot attempt))
        (Analysis-Attempt-kind attempt)))

(define (registered? scheduler attempt)
  (eq? attempt (hash-ref (Analysis-Scheduler-attempts scheduler) (attempt-key attempt) #f)))

(define (scheduled-analysis-event scheduler source)
  (define attempt
    (hash-ref (Analysis-Scheduler-attempts scheduler) (cons (source-id source) 'check-syntax) #f))
  (and attempt (Analysis-Attempt-completion attempt)))

(define (enqueue! scheduler snapshot kind parent-completion)
  (define deadline
    (+ (current-deadline-milliseconds) (* 1000 (Analysis-Scheduler-timeout scheduler))))
  (define attempt
    (Analysis-Attempt snapshot kind parent-completion (make-completion-event) deadline #f))
  (hash-set! (Analysis-Scheduler-attempts scheduler) (attempt-key attempt) attempt)
  (define injection
    (if (eq? kind 'check-syntax)
        (and (Analysis-Scheduler-analyze scheduler)
             (lambda () ((Analysis-Scheduler-analyze scheduler) snapshot)))
        (and (procedure? (Analysis-Scheduler-resyntax scheduler))
             (lambda () ((Analysis-Scheduler-resyntax scheduler)
                         (Source-Snapshot-text snapshot) (Source-Snapshot-uri snapshot))))))
  (set-Analysis-Attempt-task!
    attempt
    (submit-place-task! (if (eq? kind 'check-syntax)
                            (Analysis-Scheduler-check-syntax-runner scheduler)
                            (Analysis-Scheduler-resyntax-runner scheduler))
                        kind
                        snapshot (Analysis-Scheduler-events scheduler) deadline
                        (lambda (result) (Analysis-Completed attempt result)) #:work injection))
  (Analysis-Attempt-completion attempt))

;; Requests for the same revision share an attempt. A successor revision
;; supersedes outstanding work for this source, including its Resyntax.
(define (request-analysis! scheduler snapshot)
  (define source (Source-Snapshot-source snapshot))
  (define active
    (hash-ref (Analysis-Scheduler-attempts scheduler) (cons (source-id source) 'check-syntax) #f))
  (cond
    [(Analysis-Scheduler-stopped? scheduler) (ready-evt (Analysis-Outcome 'stopped 'shutdown))]
    [(and active
          (eq? (Source-Snapshot-revision snapshot)
               (Source-Snapshot-revision (Analysis-Attempt-snapshot active))))
     (Analysis-Attempt-completion active)]
    [else
     (cancel-source-analysis! scheduler source)
     (enqueue! scheduler snapshot 'check-syntax #f)]))

(define (schedule-resyntax! scheduler snapshot parent-completion)
  (when (and (not (Analysis-Scheduler-stopped? scheduler))
             (Analysis-Scheduler-resyntax scheduler))
    (define previous
      (hash-ref (Analysis-Scheduler-attempts scheduler)
                (cons (Source-Snapshot-id snapshot) 'resyntax) #f))
    (when previous (cancel-attempt! scheduler previous 'superseded))
    (enqueue! scheduler snapshot 'resyntax parent-completion))
  (void))

(define (settle! scheduler attempt outcome)
  (when (registered? scheduler attempt)
    (hash-remove! (Analysis-Scheduler-attempts scheduler) (attempt-key attempt))
    (resolve-completion! (Analysis-Attempt-completion attempt) outcome)))

;; Revoke acceptance on the owner immediately. That kind's pool handles physical cleanup.
(define (cancel-attempt! scheduler attempt reason)
  (when (registered? scheduler attempt)
    (cancel-place-task! (Analysis-Attempt-task attempt))
    (settle! scheduler attempt (Analysis-Outcome 'stopped reason))))

(define (cancel-source-analysis! scheduler source [reason 'source-changed])
  (for ([kind (in-list '(check-syntax resyntax))])
    (define attempt
      (hash-ref (Analysis-Scheduler-attempts scheduler) (cons (source-id source) kind) #f))
    (when attempt (cancel-attempt! scheduler attempt reason)))
  (void))

(define (stop-analysis-scheduler! scheduler)
  (set-Analysis-Scheduler-stopped?! scheduler #t)
  (for ([attempt (in-list (hash-values (Analysis-Scheduler-attempts scheduler)))])
    (cancel-attempt! scheduler attempt 'shutdown))
  (stop-place-runner! (Analysis-Scheduler-check-syntax-runner scheduler))
  (stop-place-runner! (Analysis-Scheduler-resyntax-runner scheduler)))

;; Source eligibility remains an owner decision; this does no admission work.
(define (cancel-ineligible-analysis! scheduler current?)
  (for ([attempt (in-list (hash-values (Analysis-Scheduler-attempts scheduler)))]
        #:unless (current? (Analysis-Attempt-snapshot attempt)))
    (cancel-attempt! scheduler attempt 'source-changed)))

;; `accept` runs inline for the current registered attempt only. It receives
;; snapshot, kind, completion, parent-completion, and worker result, and returns
;; an `Analysis-Outcome` after installing or discarding the result and
;; translating domain failures. Only then may this attempt's completion become ready.
(define (finish-analysis-task! scheduler message accept)
  (define attempt (Analysis-Completed-attempt message))
  (when (registered? scheduler attempt)
    ;; Posted at logical settlement, which can precede worker cleanup.
    ;; Acceptance may change source state; it does not wait for the worker.
    (define outcome
      (accept (Analysis-Attempt-snapshot attempt) (Analysis-Attempt-kind attempt)
              (Analysis-Attempt-completion attempt) (Analysis-Attempt-parent-completion attempt)
              (if (>= (current-deadline-milliseconds) (Analysis-Attempt-deadline attempt))
                  (Analysis-Outcome 'failed 'analysis-timeout)
                  (Analysis-Completed-result message))))
    (settle! scheduler attempt outcome)))
