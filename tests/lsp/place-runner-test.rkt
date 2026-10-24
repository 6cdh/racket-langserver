#lang racket/base

(module+ test
  (require rackunit racket/async-channel racket/class racket/match racket/place racket/runtime-path
           "../../common/source.rkt"
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

  (define (with-runner proc)
    (define custodian (make-custodian))
    (define spawned (make-async-channel))
    (define events (make-async-channel))
    (parameterize ([current-custodian custodian])
      (define runner (make-place-runner))
      (dynamic-wind
        void
        (lambda ()
          (parameterize ([current-place-factory
                          (lambda ()
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
        (check-equal? (car request) (car next))
        (check-not-equal? (cadr request) (cadr next))
        ;; Old settlement must not stop the new assignment on this same place.
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
        (parameterize ([current-place-factory (lambda () (error 'startup "failed"))])
          (start runner events))
        (check-eq? (Analysis-Outcome-state (await events)) 'failed)
        (define entered (make-async-channel))
        (define task
          (parameterize ([current-place-factory
                          (lambda ()
                            (define worker (factory))
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
        (await original-control)
        (release-and-count-resources! original-control 'reply-and-die)
        (check-true (Analysis-Product-succeeded? (await events)))
        (await (place-dead-evt original-worker))
        (start runner events)
        (define-values (_replacement-worker replacement-control) (await-worker spawned))
        (await replacement-control)
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
