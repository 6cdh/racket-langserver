#lang racket/base

;; Persistent worker for Check Syntax and Resyntax. The control thread receives
;; commands, loads each adapter under a worker-owned lifetime, and runs every
;; job on a fresh thread and child custodian. It sends a terminal reply only
;; after that job's cleanup. Cross-file rules are in analysis-lifecycle.md.
(require racket/async-channel
         racket/class
         racket/contract
         racket/match
         racket/place
         racket/port
         racket/runtime-path)

(provide run)

(define-runtime-path doc-path "../doclib/doc.rkt")
(define-runtime-path syntax-path "../doclib/check-syntax.rkt")
(define-runtime-path resyntax-path "../doclib/external/resyntax.rkt")
(define-runtime-path json-path "../common/json-util.rkt")

(define (failure-detail reason)
  (if (exn? reason) (exn-message reason) "analysis raised a non-exception value"))

;; Adapters and loaded modules belong to the worker, never to a job.
;; Check Syntax makes a fresh namespace, document, trace, and traversal per call.
;; The documentation caches loaded by doc.rkt serve queries, not this path.
;; Resyntax binds its optional API once when its adapter module loads; its
;; source and result objects are local to a call. Third-party module caches and
;; arbitrary readers or macros are not transactional: custodian shutdown cannot
;; roll back shared mutation or preempt non-yielding native code. The parent's
;; cleanup watchdog must retain hard place retirement for those cases.
(define (load-adapter kind)
  (case kind
    [(check-syntax)
     (define make-doc (dynamic-require doc-path 'make-doc))
     (define check (dynamic-require doc-path 'doc-check-syntax))
     (define trace (dynamic-require syntax-path 'CSResult-trace))
     (define succeeded? (dynamic-require syntax-path 'CSResult-succeed?))
     (lambda (uri text)
       (define result (check (make-doc uri text)))
       (list 'result (succeeded? result) (send (trace result) serialize)))]
    [(resyntax)
     (define analyze (dynamic-require resyntax-path 'run-resyntax))
     (define ->jsexpr (dynamic-require json-path '->jsexpr))
     (lambda (uri text) (list 'result (->jsexpr (analyze text uri))))]
    [else (error 'analysis-place "unknown analysis kind: ~e" kind)]))

;; One initializing stack or one job. The producer always enqueues before
;; dying. On a death event the controller polls this queue first, because both
;; events can become ready at the same time.
;;
;; id: The kind while initializing; the assignment ID while a job is active.
;; outcomes: Tagged initialization or job results (`'adapter`, `'failure`, or a
;;   transferable job reply).
(struct/contract Work
  ([id (or/c 'check-syntax 'resyntax exact-positive-integer?)]
   [custodian custodian?]
   [thread thread?]
   [outcomes async-channel?])
  #:transparent)

;; Retain the initialization custodian along with its adapter for the entire
;; worker lifetime, including any library helper resources it manages.
(struct/contract Stack
  ([adapter (-> string? string? list?)]
   [custodian custodian?])
  #:transparent)

(define (spawn-work id thunk)
  (define custodian (make-custodian))
  (define outcomes (make-async-channel))
  (define worker
    (parameterize ([current-custodian custodian])
      (thread
        (lambda ()
          (async-channel-put
            outcomes
            (with-handlers ([(lambda (_) #t)
                             (lambda (reason) (list 'failure (failure-detail reason)))])
              (thunk)))))))
  (Work id custodian worker outcomes))

(define (work-event work tag)
  (if work
      (choice-evt
        (handle-evt (Work-outcomes work) (lambda (value) (list tag value)))
        (handle-evt (thread-dead-evt (Work-thread work))
                    (lambda (_)
                      (list tag (or (async-channel-try-get (Work-outcomes work))
                                    '(failure "analysis thread died without an outcome"))))))
      never-evt))

(define (run channel)
  ;; Suppression precedes even module initialization, including optional imports.
  (parameterize ([current-output-port (open-output-nowhere)])
    (define adapters (make-hasheq))
    (let loop ([initializing #f] [active #f])
      ;; Capture the active binding, not a Work argument that would retain job
      ;; resources through output. Clearing the binding precedes all wire I/O.
      (define (terminate-job! id reply)
        (custodian-shutdown-all (Work-custodian active))
        (set! active #f)
        ;; Preserve reader/macro stderr before reuse or place retirement.
        (flush-output (current-error-port))
        (place-channel-put channel (list 'terminal id reply))
        (loop initializing #f))
      (define event
        (sync (handle-evt channel (lambda (command) (list 'command command)))
              (work-event initializing 'initialized)
              (work-event active 'finished)))
      ;; Protocol errors escape this controller and end the place. Only the
      ;; job-thread execution boundary translates arbitrary failures to replies.
      (match event
        [(list 'command (list 'prepare kind))
         (cond
           [(hash-has-key? adapters kind)
            (place-channel-put channel (list 'ready kind))
            (loop initializing active)]
           [initializing (error 'analysis-place "overlapping initialization")]
           [else
            ;; Initialization has its own persistent lifetime. In particular,
            ;; it is never a descendant of a cancellable job custodian.
            (loop (spawn-work kind (lambda () (list 'adapter (load-adapter kind)))) active)])]
        [(list 'initialized (list 'adapter adapter))
         (define kind (Work-id initializing))
         ;; Keep initialization's custodian alive: libraries may own helpers.
         (hash-set! adapters kind (Stack adapter (Work-custodian initializing)))
         (place-channel-put channel (list 'ready kind))
         (loop #f active)]
        [(list 'initialized (list 'failure detail))
         ;; Send the failure, then return to end the controller and place: a
         ;; partially initialized stack must not remain alive for later jobs.
         (place-channel-put channel (list 'init-failure detail))]
        [(list 'command (list 'start id kind uri text directory))
         (when active (error 'analysis-place "worker already has an active job"))
         (define adapter (Stack-adapter (hash-ref adapters kind)))
         (loop initializing
               (spawn-work
                 id
                 (lambda ()
                   (parameterize ([current-directory directory])
                     (define reply (adapter uri text))
                     (unless (place-message-allowed? reply)
                       (error 'analysis-place "result is not transferable"))
                     reply))))]
        [(list 'command (list 'cancel id))
         (if (and active (equal? id (Work-id active)))
             (terminate-job! id '(cancelled))
             (loop initializing active))]
        [(list 'finished reply) (terminate-job! (Work-id active) reply)]))))
