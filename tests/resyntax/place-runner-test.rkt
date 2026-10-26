#lang racket/base

;; Pool transitions and fallback with a test-controlled real place. Barriers
;; are explicit receipts, not sleeps. These tests do not prove persistent
;; library isolation; see shared-worker-test.rkt.

(module+ test
  (require rackunit racket/async-channel racket/class racket/match racket/place racket/runtime-path
           "../../common/source.rkt" "../../common/time.rkt"
           "../../lsp/safedoc.rkt" "../../lsp/source-controller.rkt"
           (submod "../../lsp/source-controller.rkt" owner)
           (only-in "../lsp/controller-support.rkt" scratch drive-controller)
           "../../workspace/source-store.rkt" "../../lsp/analysis.rkt"
           "../../lsp/source-controller/place-runner.rkt"
           (submod "../../lsp/source-controller/place-runner.rkt" test-support))

  (define-runtime-path peer "../fixtures/analysis-place-peer.rkt")
  (define (await event [label 'await])
    (define result (sync/timeout 30 (wrap-evt event list)))
    (unless result (error label "event did not complete"))
    (car result))
  (define (snapshot [text "#lang racket/base\n(define value 1)\nvalue\n"])
    (Source-Snapshot (uri->source "untitled:runner.rkt") (gensym) 'buffer text))
  (define (with-runner proc #:slots [slots 1] #:manual-prepare? [manual? #f]
                       #:startup-entered [entered #f])
    (define custodian (make-custodian))
    (define spawned (make-async-channel))
    (define events (make-async-channel))
    (define (factory guard)
      (define worker (guard (lambda () (dynamic-place peer 'run-peer))))
      (define-values (control remote) (place-channel))
      (place-channel-put worker remote)
      (when manual?
        (place-channel-put control 'manual-prepare)
        (await control))
      (async-channel-put spawned (cons worker control))
      (when entered
        (async-channel-put entered #t)
        (sync never-evt))
      worker)
    ;; Nested controllers/schedulers constructed by proc use the same peer seam.
    (parameterize ([current-custodian custodian] [current-cleanup-seconds 0.15]
                   [current-place-factory factory]
                   [current-analysis-restorer (lambda (_input _success data) data)])
      (define runner (make-place-runner #:slots slots #:place-factory factory))
      (dynamic-wind
        void
        (lambda () (proc runner events spawned))
        (lambda ()
          (stop-place-runner! runner)
          (await (place-runner-shutdown-evt runner))
          (custodian-shutdown-all custodian)))))
  (define (start runner events [seconds 30] [input (snapshot)])
    (start-place-task! runner input events seconds values))
  (define (submit runner events kind [seconds 30])
    (submit-place-task! runner kind (snapshot) events
                        (+ (current-deadline-milliseconds) (* seconds 1000)) values))
  (define (await-worker spawned)
    (match-define (cons worker control) (await spawned))
    (values worker control))
  (define (run-real-job! control)
    (place-channel-put control 'run)
    (await control))
  (define (finish! control) (place-channel-put control 'finish))
  (define (check-stopped event)
    (check-eq? (Analysis-Outcome-state (await event)) 'stopped))

  (test-case "the pool factory is fixed at construction, including later cold slots"
    (with-runner
      (lambda (runner events spawned)
        (parameterize ([current-place-factory
                        (lambda (_guard) (error 'submission "must use the pool factory"))])
          (start runner events)
          (start runner events))
        (define-values (first-worker first-control) (await-worker spawned))
        (define-values (second-worker second-control) (await-worker spawned))
        (check-false (eq? first-worker second-worker))
        (await first-control)
        (await second-control)
        (finish! first-control)
        (finish! second-control)
        (check-equal? (await events) #hasheq())
        (check-equal? (await events) #hasheq()))
      #:slots 2))

  (test-case "queued tasks expire while the only slot executes, without another place"
    (with-runner
      (lambda (runner events spawned)
        (start runner events)
        (define-values (_worker control) (await-worker spawned))
        (await control)
        (start runner events 0.05)
        (define outcome (await events))
        (check-eq? (Analysis-Outcome-value outcome) 'analysis-timeout)
        (check-false (sync/timeout 0 spawned))
        (finish! control)
        (check-equal? (await events) #hasheq())
        (check-false (sync/timeout 0 control)))))

  (test-case "normal cancellation waits for cleanup and reuses the same worker for both kinds"
    (with-runner
      (lambda (runner events spawned)
        (define old (start runner events))
        (define-values (worker control) (await-worker spawned))
        (match-define (list 'start first-id _ _ _ _) (await control))
        (cancel-place-task! old)
        (check-stopped events)
        (check-equal? (await control) (list 'cancel first-id))
        (submit runner events 'resyntax)
        (check-false (sync/timeout 0 control))
        (place-channel-put control 'cancelled)
        (match-define (list 'start next-id next-kind _ _ _) (await control))
        (check-eq? next-kind 'resyntax)
        (check-not-equal? first-id next-id)
        (cancel-place-task! old)
        (finish! control)
        (check-equal? (await events) '())
        (start runner events)
        (await control)
        (finish! control)
        (check-equal? (await events) #hasheq())
        (check-false (sync/timeout 0 spawned))
        (check-false (sync/timeout 0 (place-dead-evt worker)))
        (check-false (sync/timeout 0 events)))))

  (for ([mode '(hang wrong-id bad-id wrong-kind die)])
    (test-case (format "~a retires only its slot before lazy replacement" mode)
      (with-runner
        (lambda (runner events spawned)
          (define task (start runner events))
          (define-values (worker control) (await-worker spawned))
          (await control)
          (cond
            [(eq? mode 'hang)
             (place-channel-put control 'hang)
             (cancel-place-task! task)
             (check-stopped events)]
            [else
             (place-channel-put control mode)
             (check-eq? (Analysis-Outcome-state (await events)) 'failed)])
          (start runner events)
          (define-values (replacement next) (await-worker spawned))
          (check-not-eq? worker replacement)
          (check-not-false (sync/timeout 0 (place-dead-evt worker)))
          (await next)
          (cancel-place-task! task)
          (finish! next)
          (check-equal? (await events) #hasheq())))))

  (test-case "restoration holds its slot; cancellation cleans it and preserves the worker"
    (with-runner
      (lambda (runner events spawned)
        (define entered (make-async-channel))
        (define task
          (parameterize ([current-analysis-restorer
                          (lambda args (async-channel-put entered #t) (sync never-evt))])
            (start runner events)))
        (define-values (worker control) (await-worker spawned))
        (await control)
        (finish! control)
        (await entered)
        (submit runner events 'resyntax 0.05)
        (check-eq? (Analysis-Outcome-value (await events)) 'analysis-timeout)
        (check-false (sync/timeout 0 control))
        (cancel-place-task! task)
        (check-stopped events)
        (start runner events)
        (await control)
        (finish! control)
        (check-equal? (await events) #hasheq())
        (check-false (sync/timeout 0 spawned))
        (check-false (sync/timeout 0 (place-dead-evt worker))))))

  (test-case "deadline expiry settles during held parent cleanup without releasing capacity"
    (define cleaning (make-async-channel))
    (define release-cleanup (make-semaphore 0))
    (define hold-cleanup? #f)
    (parameterize ([current-parent-cleaner
                    (lambda (custodian)
                      (when hold-cleanup?
                        (set! hold-cleanup? #f)
                        (async-channel-put cleaning #t)
                        (sync release-cleanup))
                      (custodian-shutdown-all custodian))])
      (with-runner
        (lambda (runner events spawned)
          ;; Warm the worker so the short budget below measures parent cleanup.
          (start runner events)
          (define-values (_worker control) (await-worker spawned))
          (await control)
          (finish! control)
          (await events)
          (set! hold-cleanup? #t)
          (start runner events 5)
          (await control)
          (finish! control)
          (await cleaning)
          (check-equal? (await events) (Analysis-Outcome 'failed 'analysis-timeout))
          (start runner events)
          (check-false (sync/timeout 0 control))
          (check-false (sync/timeout 0 spawned))
          (semaphore-post release-cleanup)
          (await control)
          (finish! control)
          (check-equal? (await events) #hasheq())
          (check-false (sync/timeout 0 events))))))

  (test-case "worker death during parent cleanup preserves the result and reservation"
    (define cleaning (make-async-channel))
    (define release-cleanup (make-semaphore 0))
    (define first-cleanup? #t)
    (parameterize ([current-parent-cleaner
                    (lambda (custodian)
                      (when first-cleanup?
                        (set! first-cleanup? #f)
                        (async-channel-put cleaning #t)
                        (sync release-cleanup))
                      (custodian-shutdown-all custodian))])
      (with-runner
        (lambda (runner events spawned)
          (start runner events)
          (define-values (worker control) (await-worker spawned))
          (await control)
          (finish! control)
          ;; This message comes from the cleanup helper, after restoration.
          (await cleaning)
          (place-kill worker)
          (await (place-dead-evt worker))
          (start runner events)
          ;; Death cannot settle the restored result or release its reservation
          ;; while the parent cleanup helper is held at the explicit barrier.
          (check-false (sync/timeout 1 events))
          (check-false (sync/timeout 0 spawned))
          (semaphore-post release-cleanup)
          (check-equal? (await events) #hasheq())
          (define-values (replacement next-control) (await-worker spawned))
          (check-not-eq? replacement worker)
          (await next-control)
          (finish! next-control)
          (check-equal? (await events) #hasheq())))))

  (test-case "job and restoration failures retain their cleaned worker"
    (with-runner
      (lambda (runner events spawned)
        (start runner events)
        (define-values (_worker control) (await-worker spawned))
        (await control)
        (place-channel-put control 'failure)
        (check-eq? (Analysis-Outcome-state (await events)) 'failed)
        (for ([restorer (in-list (list (lambda args (error 'restore "failure"))
                                       (lambda args
                                         (Analysis-Outcome 'failed 'restoration-failed))))])
          (parameterize ([current-analysis-restorer restorer])
            (start runner events))
          (await control)
          (finish! control)
          (check-eq? (Analysis-Outcome-state (await events)) 'failed))
        (start runner events)
        (await control)
        (finish! control)
        (check-equal? (await events) #hasheq())
        (check-false (sync/timeout 0 spawned)))))

  (test-case "mixed kinds share the configured physical bound and FIFO"
    (with-runner
      (lambda (runner events spawned)
        (submit runner events 'check-syntax)
        (define-values (_a ca) (await-worker spawned))
        (await ca)
        (submit runner events 'resyntax)
        (define-values (_b cb) (await-worker spawned))
        (match-define (list 'start _ kind-b _ _ _) (await cb))
        (check-eq? kind-b 'resyntax)
        (submit runner events 'resyntax)
        (submit runner events 'check-syntax)
        (finish! ca)
        (await events)
        (match-define (list 'start _ next-kind-a _ _ _) (await ca))
        (check-eq? next-kind-a 'resyntax)
        (finish! ca)
        (await events)
        (match-define (list 'start _ final-kind-a _ _ _) (await ca))
        (check-eq? final-kind-a 'check-syntax)
        (finish! ca)
        (await events)
        (finish! cb)
        (await events)
        (check-false (sync/timeout 0 spawned)))
      #:slots 2))

  (test-case "cancel during initialization settles promptly but retains startup for reuse"
    (with-runner
      (lambda (runner events spawned)
        (define task (start runner events))
        (define-values (worker control) (await-worker spawned))
        (check-equal? (await control) '(preparing check-syntax))
        (cancel-place-task! task)
        (check-stopped events)
        (start runner events)
        (place-channel-put control 'ready)
        (match-define (list 'start _ kind _ _ _) (await control))
        (check-eq? kind 'check-syntax)
        (finish! control)
        (await events)
        (check-false (sync/timeout 0 spawned))
        (check-false (sync/timeout 0 (place-dead-evt worker))))
      #:manual-prepare? #t))

  (test-case "native startup and first initialization share one watchdog"
    (define custodian (make-custodian))
    (define created (make-async-channel))
    (define release-startup (make-semaphore 0))
    (define events (make-async-channel))
    (parameterize ([current-custodian custodian]
                   [current-startup-seconds 8]
                   [current-place-factory
                    (lambda (guard)
                      (define worker (guard (lambda () (dynamic-place peer 'run-unready-peer))))
                      (define-values (control remote) (place-channel))
                      (place-channel-put worker remote)
                      (async-channel-put created (cons worker control))
                      (sync release-startup)
                      worker)])
      (define runner (make-place-runner))
      (dynamic-wind
        void
        (lambda ()
          (start runner events)
          (define-values (worker control) (await-worker created))
          ;; Native creation is known to have started. Consume five seconds of
          ;; its eight-second budget before allowing library initialization.
          (sync (deadline-evt (+ (current-deadline-milliseconds) 5000)))
          (semaphore-post release-startup)
          (check-equal? (await control) '(preparing check-syntax))
          ;; The remaining budget is at most three seconds. A renewed budget
          ;; would take eight seconds here and fail this bounded observation.
          (define outcome (sync/timeout 4 events))
          (check-equal? outcome (Analysis-Outcome 'failed 'worker-watchdog))
          (void (await (place-dead-evt worker))))
        (lambda ()
          (stop-place-runner! runner)
          (await (place-runner-shutdown-evt runner))
          (custodian-shutdown-all custodian)))))

  (for ([phase '(startup initialization)])
    (test-case (format "shutdown during ~a observes death and prevents replacement" phase)
      (define entered (and (eq? phase 'startup) (make-async-channel)))
      (with-runner
        (lambda (runner events spawned)
          (start runner events)
          (define-values (worker control) (await-worker spawned))
          (if entered (await entered) (await control))
          (start runner events)
          (stop-place-runner! runner)
          (check-stopped events)
          (check-stopped events)
          ;; Shutdown readiness observes manager death as well as retirement.
          (await (place-runner-shutdown-evt runner))
          (check-not-false (sync/timeout 0 (place-dead-evt worker)))
          (start runner events)
          (check-stopped events)
          (stop-place-runner! runner)
          (check-false (sync/timeout 0 spawned)))
        #:startup-entered entered
        #:manual-prepare? (eq? phase 'initialization)))))
(module+ test
  (test-case "controllers retain their own startup guards across worker reuse"
    (define custodian (make-custodian))
    (define startups (make-async-channel))
    (define caller (current-thread))
    (parameterize ([current-custodian custodian])
      (define controllers
        (for/list ([name '(first second)])
          (make-source-controller
            #:analysis-slots 1 #:resyntax #f
            #:place-start-guard
            (lambda (thunk)
              (async-channel-put startups (cons name (current-thread)))
              (thunk)))))
      (dynamic-wind
        void
        (lambda ()
          (check-false (sync/timeout 0 startups))
          (for ([controller (in-list controllers)] [name '(first second)])
            (for ([assignment '(initial reused)])
              (define source (uri->source (format "untitled:guard-~a.rkt" assignment)))
              (define sd
                (new-safedoc (source-uri source) "#lang racket/base\n(define value 1)\n" 0))
              (source-controller-open! controller source sd)
              (define completion (with-read-safedoc sd SafeDoc-analysis-completion))
              (check-eq? (Analysis-Outcome-state (drive-controller controller completion 30))
                         'succeeded)
              (when (eq? assignment 'initial)
                (match-define (cons observed-name startup-thread) (await startups))
                (check-eq? observed-name name)
                (check-not-eq? startup-thread caller))
              (check-false (sync/timeout 0 startups)))))
        (lambda ()
          (for ([controller (in-list controllers)])
            (source-controller-shutdown! controller))
          (custodian-shutdown-all custodian)))))

  (test-case "pooled controller restores retained aliases and publishes before fresh completion"
    (with-runner
      (lambda (_runner _events spawned)
        (current-analysis-restorer restore-analysis)
        (define controller (make-source-controller #:analysis-slots 2 #:resyntax #f))
        (dynamic-wind
          void
          (lambda ()
            (define path (build-path scratch "pooled-retention.rkt"))
            (define source (path->source path))
            (define uri (source-uri source))
            (define alias (regexp-replace #rx"^file://" uri "file://localhost"))
            (define text "#lang racket/base\n(define value 1)\nvalue\n")
            (define sd (new-safedoc uri text 7))
            (define publications '())
            (define pending #f)
            (source-controller-set-notifier!
              controller
              (lambda (_method params)
                (when pending
                  (check-false (sync/timeout 0 pending)
                               "publication precedes shared completion"))
                (set! publications (cons params publications))))
            (source-controller-open! controller source sd)
            (set! pending (with-read-safedoc sd SafeDoc-analysis-completion))
            (define-values (worker-a control-a) (await-worker spawned))
            (await control-a)
            ;; Keep a second source running across close/reopen and restoration.
            (define other-source (uri->source "untitled:pooled-peer.rkt"))
            (define other (new-safedoc (source-uri other-source) text 0))
            (source-controller-open! controller other-source other)
            (define other-done (with-read-safedoc other SafeDoc-analysis-completion))
            (define-values (_worker-b control-b) (await-worker spawned))
            (await control-b)
            (run-real-job! control-a)
            (check-eq? (Analysis-Outcome-state (drive-controller controller pending 30)) 'succeeded)
            (set! pending #f)
            (define trace (Analysis-Product-trace
                            (Available-Analysis-product (with-read-safedoc sd SafeDoc-available))))
            (source-controller-close! controller source sd)
            (define reopened (new-safedoc alias text 7))
            (source-controller-open! controller (uri->source alias) reopened)
            (set! pending (with-read-safedoc reopened SafeDoc-analysis-completion))
            (await control-a)
            (check-eq? trace (Analysis-Product-trace
                               (Available-Analysis-product
                                 (with-read-safedoc reopened SafeDoc-available))))
            (check-equal? (hash-ref (car publications) 'uri) alias)
            (check-false (sync/timeout 0 pending))
            (run-real-job! control-a)
            (check-eq? (Analysis-Outcome-state (drive-controller controller pending 30)) 'succeeded)
            (check-equal? (hash-ref (car publications) 'uri) alias)
            (check-equal? (length publications) 4)
            (set! pending other-done)
            (run-real-job! control-b)
            (check-eq? (Analysis-Outcome-state (drive-controller controller other-done 30)) 'succeeded)
            (check-false (sync/timeout 0 spawned))
            (check-false (sync/timeout 0 (place-dead-evt worker-a))))
          (lambda () (source-controller-shutdown! controller))))
      #:slots 2)))

(module+ test
  (require "../../lsp/source-controller/analysis-scheduler.rkt")

  (test-case "same-revision retry rejects an already queued old result without controller admission"
    (with-runner
      (lambda (_runner events spawned)
        (define scheduler (make-analysis-scheduler events #:slots 1 #:resyntax #f))
        (dynamic-wind
          void
          (lambda ()
            (define input (snapshot))
            (define first (request-analysis! scheduler input))
            (check-eq? first (request-analysis! scheduler input))
            (define-values (_worker control) (await-worker spawned))
            (await control 'scheduler-request)
            (finish! control)
            (define old (await events 'scheduler-old))
            (cancel-source-analysis! scheduler (Source-Snapshot-source input))
            (check-stopped first)
            (define retry (request-analysis! scheduler input))
            (await control 'scheduler-retry)
            (finish-analysis-task! scheduler old (lambda args (error 'stale "old reply accepted")))
            (check-false (sync/timeout 0 retry))
            (finish! control)
            (finish-analysis-task!
              scheduler (await events 'scheduler-new)
              (lambda (_snapshot _kind completion _parent result)
                (check-false (sync/timeout 0 completion))
                (check-equal? result #hasheq())
                (Analysis-Outcome 'succeeded #f)))
            (check-eq? (Analysis-Outcome-state (await retry 'scheduler-completion)) 'succeeded)
            (check-false (sync/timeout 0 spawned)))
          (lambda () (stop-analysis-scheduler! scheduler))))))

  (test-case "execution deadline cancels cleanup but rejects a late success exactly once"
    (with-runner
      (lambda (runner events spawned)
        (define task (start runner events 5))
        (define-values (worker control) (await-worker spawned))
        (match-define (list 'start id _ _ _ _) (await control))
        (define timeout (await events))
        (check-eq? (Analysis-Outcome-value timeout) 'analysis-timeout)
        (check-equal? (await control) (list 'cancel id))
        ;; A result racing cancellation certifies cleanup but cannot revive the task.
        (finish! control)
        (start runner events)
        (await control)
        (cancel-place-task! task)
        (finish! control)
        (check-equal? (await events) #hasheq())
        (check-false (sync/timeout 0 events))
        (check-false (sync/timeout 0 spawned))
        (check-false (sync/timeout 0 (place-dead-evt worker)))))))
