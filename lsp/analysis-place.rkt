#lang racket/base

;; Persistent worker place for background analysis.
;;
;; Each place instance executes one analysis kind ('check-syntax or 'resyntax).
;; The control thread receives commands from the pool manager, dynamically loads
;; its adapter under a persistent lifetime, and executes each analysis job on a
;; dedicated thread and child custodian.
;;
;; Key lifecycle invariants:
;; - The place control thread stays outside job custody.
;; - Adapter initialization has its own persistent custodian, retained across jobs
;;   so library background helpers remain alive.
;; - Every job runs under a fresh child custodian.
;; - Job references and child custodian resources are completely cleaned and cleared
;;   before terminal replies are sent over the wire.
;; - Standard output is discarded, while standard error is flushed before terminal replies.
;;
;; Canonical cross-file ownership and lifecycle contracts are defined in analysis-lifecycle.md.

(require "analysis-place-protocol.rkt"
         racket/async-channel
         racket/class
         racket/contract
         racket/match
         racket/place
         racket/port
         racket/runtime-path)

(provide run)

;;; Message and state records

;; Wraps an incoming protocol command delivered across the place channel.
(struct/contract Worker-Command
  ([message (or/c Place-Prepare? Place-Start? Place-Cancel?)])
  #:transparent)

;; Holds a dynamically loaded analysis adapter procedure.
(struct/contract Loaded-Adapter
  ([adapter (-> string? string?
                (or/c Place-Check-Syntax-Result? Place-Resyntax-Result?))])
  #:transparent)

;; Internal event envelope delivering the result of adapter stack initialization.
(struct/contract Initialization-Finished
  ([outcome (or/c Loaded-Adapter? Place-Failure?)])
  #:transparent)

;; Internal event envelope delivering the result of an analysis job execution.
(struct/contract Job-Finished
  ([outcome (or/c Place-Failure? Place-Check-Syntax-Result? Place-Resyntax-Result?)])
  #:transparent)

;; Tracks an in-flight asynchronous operation (adapter initialization or job execution).
;;
;; id: The analysis kind during initialization; the numeric assignment ID during a job.
;; custodian: Scoping custodian for the operation thread and its transient resources.
;; thread: Thread executing the operation.
;; outcomes: Channel delivering the computed result or caught failure.
(struct/contract In-Flight-Work
  ([id (or/c 'check-syntax 'resyntax exact-positive-integer?)]
   [custodian custodian?]
   [thread thread?]
   [outcomes async-channel?])
  #:transparent)

;; Holds the initialized adapter and its persistent custodian for the worker's lifetime.
(struct/contract Analysis-Stack
  ([adapter (-> string? string?
                (or/c Place-Check-Syntax-Result? Place-Resyntax-Result?))]
   [custodian custodian?])
  #:transparent)

(define-runtime-path doc-path "../doclib/doc.rkt")
(define-runtime-path syntax-path "../doclib/check-syntax.rkt")
(define-runtime-path resyntax-path "../doclib/external/resyntax.rkt")
(define-runtime-path json-path "../common/json-util.rkt")

(define (failure-detail reason)
  (if (exn? reason) (exn-message reason) "analysis raised a non-exception value"))

;; Adapters and loaded modules belong to the worker, never to a transient job.
;; Check Syntax creates a fresh namespace, document, trace, and traversal per invocation.
;; The documentation caches loaded by doc.rkt serve editor queries, not this execution path.
;; Resyntax binds its optional API once when its adapter module loads; its source and result
;; structures are local to each call.
;;
;; Third-party module caches and arbitrary readers or macros are not transactional:
;; custodian shutdown cannot roll back shared mutation or preempt non-yielding native code.
;; The manager's cleanup watchdog retains hard place retirement as a fallback for those cases.
(define (load-adapter kind)
  (match kind
    ['check-syntax
     (define make-doc (dynamic-require doc-path 'make-doc))
     (define check (dynamic-require doc-path 'doc-check-syntax))
     (define trace (dynamic-require syntax-path 'CSResult-trace))
     (define succeeded? (dynamic-require syntax-path 'CSResult-succeed?))
     (lambda (uri text)
       (define result (check (make-doc uri text)))
       (Place-Check-Syntax-Result (succeeded? result)
                                  (send (trace result) serialize)))]

    ['resyntax
     (define analyze (dynamic-require resyntax-path 'run-resyntax))
     (define ->jsexpr (dynamic-require json-path '->jsexpr))
     (lambda (uri text)
       (Place-Resyntax-Result (->jsexpr (analyze text uri))))]))

;; Spawns a background operation under a dedicated child custodian.
;; Any exception raised by the thunk is caught and translated to a Place-Failure.
(define (spawn-in-flight-work id thunk)
  (define op-custodian (make-custodian))
  (define outcomes (make-async-channel))
  (define op-thread
    (parameterize ([current-custodian op-custodian])
      (thread
        (lambda ()
          (async-channel-put
            outcomes
            (with-handlers ([(lambda (_) #t)
                             (lambda (reason)
                               (Place-Failure (failure-detail reason)))])
              (thunk)))))))
  (In-Flight-Work id op-custodian op-thread outcomes))

;; Constructs a sync event for an in-flight operation.
;; If the operation thread terminates concurrently with delivering its result,
;; async-channel-try-get polls outcomes first so a valid result is never dropped
;; in favor of a spurious failure.
(define (in-flight-work-event work wrap)
  (if work
      (choice-evt
        (handle-evt (In-Flight-Work-outcomes work) wrap)
        (handle-evt (thread-dead-evt (In-Flight-Work-thread work))
                    (lambda (_)
                      (wrap (or (async-channel-try-get (In-Flight-Work-outcomes work))
                                (Place-Failure "analysis thread died without an outcome"))))))
      never-evt))

;; Main entry point executed when the place is dynamically spawned.
(define (run channel)
  ;; Suppress stdout from the start so loaded modules or user code cannot corrupt the channel.
  (parameterize ([current-output-port (open-output-nowhere)])
    (let loop ([initializing #f] [active #f] [stack #f])

      ;; Terminates the active job by shutting down its custodian and clearing references
      ;; before transmitting the terminal reply across the place channel.
      (define (terminate-job! terminal-payload)
        (custodian-shutdown-all (In-Flight-Work-custodian active))
        ;; Clear the active reference before wire I/O to ensure job resources are not retained.
        (set! active #f)
        ;; Flush stderr so macro/reader diagnostics are preserved before reuse or retirement.
        (flush-output (current-error-port))
        (place-channel-put channel (Place-Terminal terminal-payload))
        (loop initializing #f stack))

      (define event
        (sync (handle-evt channel Worker-Command)
              (in-flight-work-event initializing Initialization-Finished)
              (in-flight-work-event active Job-Finished)))

      (match event
        [(Worker-Command (Place-Prepare kind))
         ;; Adapter loading runs under its own persistent custodian, independent of any job.
         (define init-work
           (spawn-in-flight-work kind (lambda () (Loaded-Adapter (load-adapter kind)))))
         (loop init-work active stack)]

        [(Initialization-Finished (Loaded-Adapter adapter))
         ;; Retain the initialization custodian: libraries may own background helpers.
         (define loaded-stack (Analysis-Stack adapter (In-Flight-Work-custodian initializing)))
         (place-channel-put channel (Place-Ready))
         (loop #f active loaded-stack)]

        [(Initialization-Finished (Place-Failure detail))
         ;; Report the initialization failure and exit the control loop to terminate the place;
         ;; an uninitialized or broken stack must never be reused.
         (place-channel-put channel (Place-Init-Failure detail))]

        [(Worker-Command (struct* Place-Start ([id id]
                                               [uri uri]
                                               [text text]
                                               [directory directory])))
         (define adapter (Analysis-Stack-adapter stack))
         (define job-work
           (spawn-in-flight-work
             id
             (lambda ()
               (parameterize ([current-directory directory])
                 (define result (adapter uri text))
                 (unless (place-message-allowed? result)
                   (error 'analysis-place "result is not transferable"))
                 result))))
         (loop initializing job-work stack)]

        [(Worker-Command (Place-Cancel id))
         (if (and active (equal? id (In-Flight-Work-id active)))
             (terminate-job! (Place-Cancelled))
             (loop initializing active stack))]

        [(Job-Finished outcome)
         (terminate-job! outcome)]))))
