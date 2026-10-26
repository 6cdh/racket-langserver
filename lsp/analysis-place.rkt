#lang racket/base

;; Persistent worker for Check Syntax and Resyntax. Production gives each kind
;; its own pool, but one place can still load both adapters. The control thread
;; receives commands, loads each adapter under a worker-owned lifetime, and
;; runs every job on a fresh thread and child custodian. It sends a terminal
;; reply only after that job's cleanup. Cross-file rules are in
;; analysis-lifecycle.md.
(require "analysis-place-protocol.rkt"
         racket/async-channel
         racket/class
         racket/contract
         racket/match
         racket/place
         racket/port
         racket/runtime-path)

(provide run)

(struct/contract Worker-Command
  ([message (or/c Place-Prepare? Place-Start? Place-Cancel?)])
  #:transparent)

(struct/contract Loaded-Adapter
  ([adapter (-> string? string?
                (or/c Place-Check-Syntax-Result? Place-Resyntax-Result?))])
  #:transparent)

(struct/contract Stack-Initialized
  ([outcome (or/c Loaded-Adapter? Place-Failure?)])
  #:transparent)

(struct/contract Job-Finished
  ([outcome (or/c Place-Failure? Place-Check-Syntax-Result? Place-Resyntax-Result?)])
  #:transparent)

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
       (Place-Check-Syntax-Result (succeeded? result) (send (trace result) serialize)))]
    [(resyntax)
     (define analyze (dynamic-require resyntax-path 'run-resyntax))
     (define ->jsexpr (dynamic-require json-path '->jsexpr))
     (lambda (uri text) (Place-Resyntax-Result (->jsexpr (analyze text uri))))]
    [else (error 'analysis-place "unknown analysis kind: ~e" kind)]))

;; One initializing stack or one job. The producer always enqueues before
;; dying. On a death event the control thread polls this queue first, because
;; both events can become ready at the same time.
;;
;; id: The kind while initializing; the assignment ID while a job is active.
;; outcomes: Loaded-Adapter, Place-Failure, or a transferable job result.
(struct/contract Work
  ([id (or/c 'check-syntax 'resyntax exact-positive-integer?)]
   [custodian custodian?]
   [thread thread?]
   [outcomes async-channel?])
  #:transparent)

;; Retain the initialization custodian along with its adapter for the entire
;; worker lifetime, including any library helper resources it manages.
(struct/contract Stack
  ([adapter (-> string? string?
                (or/c Place-Check-Syntax-Result? Place-Resyntax-Result?))]
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
                             (lambda (reason) (Place-Failure (failure-detail reason)))])
              (thunk)))))))
  (Work id custodian worker outcomes))

(define (work-event work wrap)
  (if work
      (choice-evt
        (handle-evt (Work-outcomes work) wrap)
        (handle-evt (thread-dead-evt (Work-thread work))
                    (lambda (_)
                      (wrap (or (async-channel-try-get (Work-outcomes work))
                                (Place-Failure "analysis thread died without an outcome"))))))
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
        (place-channel-put channel (Place-Terminal id reply))
        (loop initializing #f))
      (define event
        (sync (handle-evt channel Worker-Command)
              (work-event initializing Stack-Initialized)
              (work-event active Job-Finished)))
      ;; Protocol errors escape this control thread and end the place. Only the
      ;; job-thread execution boundary translates arbitrary failures to replies.
      (match event
        [(Worker-Command (Place-Prepare kind))
         (cond
           [(hash-has-key? adapters kind)
            (place-channel-put channel (Place-Ready kind))
            (loop initializing active)]
           [initializing (error 'analysis-place "overlapping initialization")]
           [else
            ;; Initialization has its own persistent lifetime, never a descendant
            ;; of a cancellable job custodian.
            (loop (spawn-work kind (lambda () (Loaded-Adapter (load-adapter kind)))) active)])]
        [(Stack-Initialized (Loaded-Adapter adapter))
         (define kind (Work-id initializing))
         ;; Keep initialization's custodian alive: libraries may own helpers.
         (hash-set! adapters kind (Stack adapter (Work-custodian initializing)))
         (place-channel-put channel (Place-Ready kind))
         (loop #f active)]
        [(Stack-Initialized (Place-Failure detail))
         ;; Send the failure, then return to end the control thread and place:
         ;; a partially initialized stack must not remain alive for later jobs.
         (place-channel-put channel (Place-Init-Failure detail))]
        [(Worker-Command (struct* Place-Start ([id id] [kind kind] [uri uri]
                                               [text text] [directory directory])))
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
        [(Worker-Command (Place-Cancel id))
         (if (and active (equal? id (Work-id active)))
             (terminate-job! id (Place-Cancelled))
             (loop initializing active))]
        [(Job-Finished reply) (terminate-job! (Work-id active) reply)]))))
