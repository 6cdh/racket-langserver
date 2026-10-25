#lang racket/base

(module+ test
  (require rackunit racket/async-channel racket/class racket/match racket/place racket/runtime-path
           "../../common/source.rkt"
           "../../lsp/safedoc.rkt"
           "../../lsp/source-controller.rkt"
           (submod "../../lsp/source-controller.rkt" owner)
           (only-in "controller-support.rkt" scratch drive-controller)
           "../../workspace/source-store.rkt"
           "../../lsp/analysis.rkt"
           "../../lsp/source-controller/place-runner.rkt"
           (submod "../../lsp/source-controller/place-runner.rkt" test-support)
           "../../lsp/source-controller/analysis-scheduler.rkt")

  (define-runtime-path peer "../fixtures/analysis-place-peer.rkt")
  (define (await event)
    (define result (sync/timeout 30 (wrap-evt event list)))
    (unless result (error 'await "event did not complete"))
    (car result))
  (define (snapshot [text "#lang racket/base\n(define value 1)\nvalue\n"])
    (Source-Snapshot (uri->source "untitled:runner.rkt") (gensym) 'buffer text))

  (define (with-runner proc #:slots [slots 1])
    (define custodian (make-custodian))
    (define spawned (make-async-channel))
    (define events (make-async-channel))
    (parameterize ([current-custodian custodian])
      (define runner (make-place-runner #:slots slots))
      (dynamic-wind
        void
        (lambda ()
          (parameterize ([current-place-factory
                          (lambda (_guard)
                            (define worker (dynamic-place peer 'run-peer))
                            (define-values (control remote) (place-channel))
                            (place-channel-put worker remote)
                            (async-channel-put spawned (cons worker control))
                            worker)])
            (proc runner events spawned)))
        (lambda () (stop-place-runner! runner) (custodian-shutdown-all custodian)))))

  (define (start runner events [seconds 30] [input (snapshot)])
    (start-place-task! runner input events seconds values))

  (define (await-worker spawned)
    (match-define (cons worker control) (await spawned))
    (values worker control))

  ;; Returns the peer's managed-resource count after the worker cleans up.
  (define (release-and-count-resources! control [command 'run])
    (place-channel-put control command)
    (await control))

  (test-case "successive assignments reuse a place with fresh namespaces and job resources"
    (with-runner
      (lambda (runner events spawned)
        (define first (start runner events))
        (define-values (worker control) (await-worker spawned))
        (define request (await control))
        (define baseline (release-and-count-resources! control))
        (check-true (Analysis-Product-succeeded? (await events)))
        (check-true (place-runner-available? runner))
        (define resource-probe
#<<RACKET
#lang racket/base
(require (for-syntax racket/base))
(begin-for-syntax
  (namespace-set-variable-value! 'job-secret 42 #t)
  (thread (lambda () (sync never-evt))))
(define other 2)
RACKET
          )
        (start runner events 30 (snapshot resource-probe))
        (define next (await control))
        (check-not-equal? (car request) (car next))
        ;; Old cancellation must not stop the new assignment on this same place.
        (cancel-place-task! first)
        (check-equal? (release-and-count-resources! control) baseline)
        (define changed (await events))
        (check-true (Analysis-Product-succeeded? changed))
        (check-not-false (memq 'other (send (Analysis-Product-trace changed) get-completions)))
        (check-false (memq 'value (send (Analysis-Product-trace changed) get-completions)))
        (define namespace-probe
#<<RACKET
#lang racket/base
(require (for-syntax racket/base))
(begin-for-syntax
  (when (namespace-variable-value 'job-secret #t (lambda () #f))
    (error 'leak)))
(define clean 3)
RACKET
          )
        (start runner events 30 (snapshot namespace-probe))
        (await control)
        (check-equal? (release-and-count-resources! control) baseline)
        (check-true (Analysis-Product-succeeded? (await events)))
        (check-false (sync/timeout 0 spawned))
        (stop-place-runner! runner)
        (void (await (place-dead-evt worker))))))

  (for ([mode '(cancel timeout die wrong-id)])
    (test-case (format "~a retires its place and subsequent work succeeds" mode)
      (with-runner
        (lambda (runner events spawned)
          (define task (start runner events (if (eq? mode 'timeout) 10 30)))
          (define-values (worker control) (await-worker spawned))
          (await control)
          (case mode
            [(cancel) (cancel-place-task! task)]
            [(timeout) (place-channel-put control 'hang)]
            [(die) (place-channel-put control 'die)]
            [(wrong-id) (release-and-count-resources! control 'wrong-id)])
          (define outcome (await events))
          (check-eq? (Analysis-Outcome-state outcome) (if (eq? mode 'cancel) 'stopped 'failed))
          (when (eq? mode 'timeout)
            (check-eq? (Analysis-Outcome-value outcome) 'analysis-timeout))
          (check-not-false (sync/timeout 0 (place-dead-evt worker)))
          (start runner events)
          (define-values (_replacement-worker replacement-control) (await-worker spawned))
          (await replacement-control)
          (cancel-place-task! task)
          (release-and-count-resources! replacement-control)
          (check-true (Analysis-Product-succeeded? (await events)))))))

  (test-case "restoration failure keeps the cleaned worker reusable"
    (with-runner
      (lambda (runner events spawned)
        (start runner events)
        (define-values (_worker control) (await-worker spawned))
        (await control)
        (release-and-count-resources! control 'bad-snapshot)
        (check-eq? (Analysis-Outcome-state (await events)) 'failed)
        (start runner events)
        (await control)
        (release-and-count-resources! control)
        (check-true (Analysis-Product-succeeded? (await events)))
        (check-false (sync/timeout 0 spawned)))))

  (test-case "startup failure and cancellation release capacity without blocking the caller"
    (with-runner
      (lambda (runner events spawned)
        (define factory (current-place-factory))
        (parameterize ([current-place-factory (lambda (_guard) (error 'startup "failed"))])
          (start runner events))
        (check-eq? (Analysis-Outcome-state (await events)) 'failed)
        (define entered (make-async-channel))
        (define task
          (parameterize ([current-place-factory
                          (lambda (guard)
                            (define worker (factory guard))
                            (async-channel-put entered #t)
                            (sync never-evt)
                            worker)])
            (start runner events)))
        (define-values (old-worker _old-control) (await-worker spawned))
        (await entered)
        (cancel-place-task! task)
        (check-eq? (Analysis-Outcome-state (await events)) 'stopped)
        (check-not-false (sync/timeout 0 (place-dead-evt old-worker)))
        (start runner events)
        (define-values (_replacement-worker replacement-control) (await-worker spawned))
        (await replacement-control)
        (release-and-count-resources! replacement-control)
        (check-true (Analysis-Product-succeeded? (await events))))))

  (test-case "scheduler discards a completed old reply after a same-revision retry"
    (with-runner
      (lambda (_runner events spawned)
        (define scheduler (make-analysis-scheduler events #:slots 2 #:resyntax #f))
        (define input (snapshot))
        (define first (request-analysis! scheduler input))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (define-values (_worker control) (await-worker spawned))
        (await control)
        (release-and-count-resources! control)
        (define old (await events))
        (cancel-source-analysis! scheduler (Source-Snapshot-source input))
        (check-eq? (Analysis-Outcome-state (await first)) 'stopped)
        (define retry (request-analysis! scheduler input))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (await control)
        (finish-analysis-task! scheduler old (lambda args (error 'stale "accepted old reply")))
        (check-false (sync/timeout 0 retry))
        (release-and-count-resources! control)
        (finish-analysis-task! scheduler (await events)
                               (lambda (_snapshot _kind completion _parent product)
                                 (check-false (sync/timeout 0 completion))
                                 (check-true (Analysis-Product-succeeded? product))
                                 (Analysis-Outcome 'succeeded (Analysis-Product-summary product))))
        (check-eq? (Analysis-Outcome-state (await retry)) 'succeeded)
        (stop-analysis-scheduler! scheduler))))

  (test-case "a final result is accepted and an idle dead place is replaced"
    (with-runner
      (lambda (runner events spawned)
        (start runner events)
        (define-values (original-worker original-control) (await-worker spawned))
        (define original-request (await original-control))
        (release-and-count-resources! original-control 'reply-and-die)
        (check-true (Analysis-Product-succeeded? (await events)))
        (await (place-dead-evt original-worker))
        (start runner events)
        (define-values (_replacement-worker replacement-control) (await-worker spawned))
        (define replacement-request (await replacement-control))
        (check-not-equal? (car original-request) (car replacement-request))
        (release-and-count-resources! replacement-control)
        (check-true (Analysis-Product-succeeded? (await events))))))

  (test-case "retiring place work keeps its shared slot until its completion is consumed"
    (with-runner
      (lambda (_runner events spawned)
        (define started (make-async-channel))
        (define scheduler
          (make-analysis-scheduler events #:slots 1
                                   #:resyntax (lambda (_text _uri)
                                                (async-channel-put started #t)
                                                '())))
        (define input (snapshot))
        (request-analysis! scheduler input)
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (define-values (original-worker original-control) (await-worker spawned))
        (await original-control)
        (cancel-source-analysis! scheduler (Source-Snapshot-source input))
        (schedule-resyntax! scheduler input always-evt)
        ;; Eligibility is checked synchronously only when dispatch reserves a
        ;; slot. No callback may run until the cancelled bridge is accounted for.
        (pump-analysis-scheduler! scheduler
                                  (lambda (_) (error 'capacity "premature dispatch")))
        (finish-analysis-task! scheduler (await events)
                               (lambda args (error 'stale "accepted cancelled analysis")))
        (check-not-false (sync/timeout 0 (place-dead-evt original-worker)))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (await started)
        (finish-analysis-task! scheduler (await events)
                               (lambda args (Analysis-Outcome 'succeeded #f)))
        (stop-analysis-scheduler! scheduler))))

  (test-case "shutdown cleanup survives disposal of the consumer custodian"
    (with-runner
      (lambda (runner events spawned)
        (define consumer (make-custodian))
        (parameterize ([current-custodian consumer]) (start runner events))
        (define-values (original-worker original-control) (await-worker spawned))
        (await original-control)
        (custodian-shutdown-all consumer)
        (stop-place-runner! runner)
        (check-eq? (Analysis-Outcome-state (await events)) 'stopped)
        (check-not-false (sync/timeout 0 (place-dead-evt original-worker)))
        (check-false (place-runner-available? runner))))))

(module+ test
  (test-case "configured slots run concurrently and share FIFO capacity with Resyntax"
    (with-runner
      (lambda (_runner events spawned)
        (define resyntax-started (make-async-channel))
        (define release-resyntax (make-semaphore 0))
        (define scheduler
          (make-analysis-scheduler events #:slots 2
                                   #:resyntax (lambda (_text _uri)
                                                (async-channel-put resyntax-started #t)
                                                (sync release-resyntax)
                                                '())))
        (define a (snapshot))
        (define b (struct-copy Source-Snapshot (snapshot)
                    [source (uri->source "untitled:second.rkt")]))
        (define c (struct-copy Source-Snapshot (snapshot)
                    [source (uri->source "untitled:third.rkt")]))
        (define first (request-analysis! scheduler a))
        (define second (request-analysis! scheduler b))
        (schedule-resyntax! scheduler a first)
        (define third (request-analysis! scheduler c))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (define-values (worker-a control-a) (await-worker spawned))
        (define-values (worker-b control-b) (await-worker spawned))
        ;; Both assignments arrive before either worker is released. Job IDs
        ;; remain distinct across slots, not just successive uses of one place.
        (define request-a (await control-a))
        (define request-b (await control-b))
        (check-not-equal? (car request-a) (car request-b))
        (pump-analysis-scheduler! scheduler (lambda (_) (error 'bound "third dispatch")))
        (check-false (sync/timeout 0 spawned))
        (release-and-count-resources! control-b)
        (finish-analysis-task! scheduler (await events)
                               (lambda args (Analysis-Outcome 'succeeded #f)))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (await resyntax-started)
        ;; The freed worker is idle, but Resyntax owns the shared slot.
        (pump-analysis-scheduler! scheduler (lambda (_) (error 'bound "Resyntax slot reused")))
        (semaphore-post release-resyntax)
        (finish-analysis-task! scheduler (await events)
                               (lambda args (Analysis-Outcome 'succeeded #f)))
        (pump-analysis-scheduler! scheduler (lambda (_) #t))
        (await control-b)
        (check-false (sync/timeout 0 spawned) "third analysis reuses the idle worker")
        (release-and-count-resources! control-b)
        (finish-analysis-task! scheduler (await events)
                               (lambda args (Analysis-Outcome 'succeeded #f)))
        (check-eq? (Analysis-Outcome-state (await third)) 'succeeded)
        (release-and-count-resources! control-a)
        (finish-analysis-task! scheduler (await events)
                               (lambda args (Analysis-Outcome 'succeeded #f)))
        (for ([completion (in-list (list first second))])
          (check-eq? (Analysis-Outcome-state (await completion)) 'succeeded))
        (stop-analysis-scheduler! scheduler)
        (await (place-dead-evt worker-a))
        (void (await (place-dead-evt worker-b))))
      #:slots 2))

  (test-case "restoration reserves capacity and cancellation preserves only its own cleaned place"
    (with-runner
      (lambda (runner events spawned)
        (define restoring (make-async-channel))
        (define old
          (parameterize ([current-analysis-restorer
                          (lambda args
                            (async-channel-put restoring #t)
                            (sync never-evt))])
            (start runner events)))
        (define-values (worker-a control-a) (await-worker spawned))
        (await control-a)
        (start runner events)
        (define-values (worker-b control-b) (await-worker spawned))
        (await control-b)
        (release-and-count-resources! control-a)
        (await restoring)
        (check-false (place-runner-available? runner))
        (check-exn #rx"unavailable" (lambda () (start runner events)))
        (cancel-place-task! old)
        (check-eq? (Analysis-Outcome-state (await events)) 'stopped)
        (check-false (sync/timeout 0 (place-dead-evt worker-a)))
        (start runner events)
        (await control-a)
        (cancel-place-task! old)
        (release-and-count-resources! control-a)
        (check-true (Analysis-Product-succeeded? (await events)))
        (release-and-count-resources! control-b)
        (check-true (Analysis-Product-succeeded? (await events)))
        (check-false (sync/timeout 0 spawned))
        (stop-place-runner! runner)
        (await (place-dead-evt worker-a))
        (void (await (place-dead-evt worker-b))))
      #:slots 2))

  (test-case "shutdown during concurrent startup kills all places and prevents replacement"
    (with-runner
      (lambda (runner events spawned)
        (define factory (current-place-factory))
        (define entered (make-async-channel))
        (parameterize ([current-place-factory
                        (lambda (guard)
                          (define worker (factory guard))
                          (async-channel-put entered #t)
                          (sync never-evt)
                          worker)])
          (start runner events)
          (start runner events))
        (define-values (worker-a _control-a) (await-worker spawned))
        (define-values (worker-b _control-b) (await-worker spawned))
        (await entered)
        (await entered)
        (stop-place-runner! runner)
        (for ([_ (in-range 2)])
          (check-eq? (Analysis-Outcome-state (await events)) 'stopped))
        (check-not-false (sync/timeout 0 (place-dead-evt worker-a)))
        (check-not-false (sync/timeout 0 (place-dead-evt worker-b)))
        (check-false (place-runner-available? runner))
        (check-exn #rx"unavailable" (lambda () (start runner events)))
        (check-false (sync/timeout 0 spawned)))
      #:slots 2)))

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
            (release-and-count-resources! control-a)
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
            (release-and-count-resources! control-a)
            (check-eq? (Analysis-Outcome-state (drive-controller controller pending 30)) 'succeeded)
            (check-equal? (hash-ref (car publications) 'uri) alias)
            (check-equal? (length publications) 4)
            (set! pending other-done)
            (release-and-count-resources! control-b)
            (check-eq? (Analysis-Outcome-state (drive-controller controller other-done 30)) 'succeeded)
            (check-false (sync/timeout 0 spawned))
            (check-false (sync/timeout 0 (place-dead-evt worker-a))))
          (lambda () (source-controller-shutdown! controller))))
      #:slots 2)))

(module+ test
  (test-case "hard cancellation replaces only its slot while a peer assignment stays live"
    (with-runner
      (lambda (runner events spawned)
        (define cancelled (start runner events))
        (define-values (worker-a control-a) (await-worker spawned))
        (await control-a)
        (start runner events)
        (define-values (worker-b control-b) (await-worker spawned))
        (await control-b)
        (cancel-place-task! cancelled)
        (check-eq? (Analysis-Outcome-state (await events)) 'stopped)
        (check-not-false (sync/timeout 0 (place-dead-evt worker-a)))
        (check-false (sync/timeout 0 (place-dead-evt worker-b)))
        (start runner events)
        (define-values (_replacement control-next) (await-worker spawned))
        (await control-next)
        (check-false (place-runner-available? runner))
        (cancel-place-task! cancelled)
        (release-and-count-resources! control-next)
        (check-true (Analysis-Product-succeeded? (await events)))
        (release-and-count-resources! control-b)
        (check-true (Analysis-Product-succeeded? (await events))))
      #:slots 2)))
