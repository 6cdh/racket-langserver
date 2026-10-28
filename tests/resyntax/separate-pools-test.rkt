#lang racket/base

;; Separate pools: places start on submission, Check Syntax capacity does not
;; block Resyntax, and Resyntax is scheduled only after successful Check Syntax.
;; Cross-file rules are in ../../lsp/analysis-lifecycle.md.

(require rackunit
         racket/async-channel
         racket/class
         racket/match
         racket/place
         racket/runtime-path
         "../../common/settings.rkt"
         "../../common/source.rkt"
         "../../workspace/source-store.rkt"
         "../../lsp/analysis.rkt"
         (submod "../../lsp/analysis.rkt" test-support)
         "../../lsp/safedoc.rkt"
         "../../lsp/source-controller.rkt"
         (submod "../../lsp/source-controller.rkt" owner)
         "../../lsp/source-controller/analysis-scheduler.rkt"
         (submod "../../lsp/source-controller/analysis-scheduler.rkt" test-support)
         (submod "../../lsp/source-controller/place-runner.rkt" test-support)
         (only-in "../lsp/controller-support.rkt" drive-controller)
         (only-in "../fixtures/analysis-place-peer.rkt" Peer-Preparing))

(define-runtime-path peer "../fixtures/analysis-place-peer.rkt")

(define (await event)
  (define result (sync/timeout 15 (wrap-evt event list)))
  (unless result (error 'await "event did not complete"))
  (car result))

(define (snapshot name)
  (Source-Snapshot (uri->source (string-append "untitled:" name))
                   (gensym 'revision) 'buffer "#lang racket/base\n(define value 1)\n"))

(module+ test
  (test-case "both pools create places only on submission and shutdown retires both kinds"
    (define custodian (make-custodian))
    (define created (make-async-channel))
    (define (factory guard)
      (define worker (guard (lambda () (dynamic-place peer 'run-unready-peer))))
      (define-values (control remote) (place-channel))
      (place-channel-put worker remote)
      (async-channel-put created (cons worker control))
      worker)
    (parameterize ([current-custodian custodian])
      (define scheduler
        (make-analysis-scheduler/with-runners
          (make-async-channel)
          (make-place-runner/configured #:slots 2 #:factory factory)
          (make-place-runner/configured #:slots 1 #:factory factory)
          #:resyntax 'place))
      (dynamic-wind
        void
        (lambda ()
          (check-false (sync/timeout 0 created))
          (define a (request-analysis! scheduler (snapshot "first.rkt")))
          (match-define (cons check-a control-a) (await created))
          (check-equal? (await control-a) (Peer-Preparing 'check-syntax))
          (check-false (sync/timeout 0 created))
          (define b (request-analysis! scheduler (snapshot "second.rkt")))
          (match-define (cons check-b control-b) (await created))
          (check-equal? (await control-b) (Peer-Preparing 'check-syntax))
          ;; Both Check Syntax slots are occupied. Resyntax still starts its own place.
          (define input (snapshot "resyntax.rkt"))
          (schedule-resyntax! scheduler input always-evt)
          (match-define (cons resyntax control-r) (await created))
          (check-equal? (await control-r) (Peer-Preparing 'resyntax))
          (check-false (sync/timeout 0 created))
          (stop-analysis-scheduler! scheduler)
          (check-eq? (Analysis-Outcome-state (await a)) 'stopped)
          (check-eq? (Analysis-Outcome-state (await b)) 'stopped)
          (for ([worker (in-list (list check-a check-b resyntax))])
            (await (place-dead-evt worker))))
        (lambda ()
          (stop-analysis-scheduler! scheduler)
          (custodian-shutdown-all custodian)))))

  (test-case "Check Syntax and Resyntax make progress while the other pool is occupied"
    (define custodian (make-custodian))
    (define events (make-async-channel))
    (define checks (make-async-channel))
    (define rewrites (make-async-channel))
    (define inputs (for/list ([name '("a" "b" "c" "r1")]) (snapshot name)))
    (define gates
      (for/hash ([input (in-list inputs)])
        (values (Source-Snapshot-uri input) (make-semaphore 0))))
    (define (release input) (semaphore-post (hash-ref gates (Source-Snapshot-uri input))))
    (parameterize ([current-custodian custodian])
      (define scheduler
        (make-analysis-scheduler
          events #:slots 2
          #:analyze (lambda (input)
                      (async-channel-put checks input)
                      (sync (hash-ref gates (Source-Snapshot-uri input)))
                      (analyze-source input))
          #:resyntax (lambda (_text uri)
                       (async-channel-put rewrites uri)
                       (sync (hash-ref gates uri))
                       '())))
      (dynamic-wind
        void
        (lambda ()
          (match-define (list a b c r1) inputs)
          (define first (request-analysis! scheduler a))
          (check-eq? (await checks) a)
          (define second (request-analysis! scheduler b))
          (check-eq? (await checks) b)
          (schedule-resyntax! scheduler r1 always-evt)
          (check-equal? (await rewrites) (Source-Snapshot-uri r1))
          (define third (request-analysis! scheduler c))
          (check-false (sync/timeout 0 checks))
          (release a)
          (check-eq? (await checks) c)
          (release r1)
          (for ([input (in-list (list b c))]) (release input))
          (for ([_ (in-range 4)])
            (finish-analysis-task!
              scheduler (await events)
              (lambda (_snapshot kind _completion parent result)
                (case kind
                  [(check-syntax)
                   (check-false parent)
                   (check-true (Analysis-Product-succeeded? result))]
                  [(resyntax)
                   (check-eq? parent always-evt)
                   (check-equal? result '())])
                (Analysis-Outcome 'succeeded #f))))
          (for ([done (in-list (list first second third))])
            (check-eq? (Analysis-Outcome-state (await done)) 'succeeded)))
        (lambda ()
          (stop-analysis-scheduler! scheduler)
          (custodian-shutdown-all custodian)))))

  (test-case "Check Syntax errors do not submit Resyntax; success does"
    (define custodian (make-custodian))
    (define rewrites (make-async-channel))
    (define previously-enabled? (get-resyntax-enabled))
    (parameterize ([current-custodian custodian])
      (define controller
        (make-source-controller
          #:analyze analyze-source
          #:resyntax (lambda (_text uri) (async-channel-put rewrites uri) '())))
      (dynamic-wind
        (lambda () (set-resyntax-enabled! #t))
        (lambda ()
          (define bad-uri "untitled:bad.rkt")
          (define bad (new-safedoc bad-uri "#lang racket/base\nunknown\n" 0))
          (source-controller-open! controller (uri->source bad-uri) bad)
          (define bad-done (with-read-safedoc bad SafeDoc-analysis-completion))
          (check-eq? (Analysis-Outcome-state (drive-controller controller bad-done 15)) 'errors)
          (define good-uri "untitled:good.rkt")
          (define good (new-safedoc good-uri "#lang racket/base\n(define value 1)\n" 0))
          (source-controller-open! controller (uri->source good-uri) good)
          (define good-done (with-read-safedoc good SafeDoc-analysis-completion))
          (check-eq? (Analysis-Outcome-state (drive-controller controller good-done 15)) 'succeeded)
          (check-equal? (await rewrites) good-uri))
        (lambda ()
          (set-resyntax-enabled! previously-enabled?)
          (source-controller-shutdown! controller)
          (custodian-shutdown-all custodian))))))
