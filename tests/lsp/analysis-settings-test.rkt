#lang racket/base

(require json
         rackunit
         racket/async-channel
         "../../common/settings.rkt"
         "../../common/interfaces.rkt"
         "../../lsp/analysis.rkt"
         "../../lsp/analysis-query.rkt"
         "../../lsp/source-controller.rkt"
         (submod "../../lsp/source-controller.rkt" owner)
         "../../lsp/workspace.rkt"
         "controller-support.rkt")

(define (push! session analysis)
  (notify session "workspace/didChangeConfiguration"
          (hasheq 'settings (hasheq 'racket-langserver (hasheq 'analysis analysis))))
  (barrier session))

(module+ test
  (test-case "configuration accepts inclusive limits and rejects out-of-range values"
    (for ([limits (in-list (list '(1 1/100 1 0) '(1 0.01 1 0) '(600 120 128 1024) '(90.5 1.5 2 64)))])
      (apply Analysis-Settings limits)
      (jsexpr->Analysis-Configuration
        (for/hasheq ([key '(taskTimeout requestTimeout workers closedFileCacheSize)]
                     [value (in-list limits)])
          (values key value))))
    (for* ([key '(taskTimeout requestTimeout)]
           [value (in-list (list 0 -1 +inf.0 +nan.0 "30" #f (json-null)))])
      (check-exn exn:fail?
                 (lambda () (jsexpr->Analysis-Configuration (hasheq key value)))))
    (for* ([key '(workers closedFileCacheSize)] [value '(-1 1.5 "2" #f)])
      (check-exn exn:fail?
                 (lambda () (jsexpr->Analysis-Configuration (hasheq key value)))))
    (for ([key '(taskTimeout requestTimeout workers closedFileCacheSize)]
          [invalid-values (in-list '((0.99 600.01) (0.0099 120.01) (0 129) (-1 1025)))]
          [field-index (in-naturals)])
      (for ([value (in-list invalid-values)])
        (check-exn exn:fail?
                   (lambda () (jsexpr->Analysis-Configuration (hasheq key value))))
        (check-exn exn:fail:contract?
                   (lambda ()
                     (apply Analysis-Settings
                            (for/list ([default '(90 30 2 64)] [index (in-naturals)])
                              (if (= index field-index) value default))))))))

  (test-case "pull responses, omitted fields, null, and malformed snapshots"
    (define custom-controller (make-source-controller #:resyntax #f #:request-timeout 15))
    (check-equal? (source-controller-request-timeout custom-controller) 15)
    (source-controller-shutdown! custom-controller)
    (check-exn exn:fail:contract? (lambda () (current-analysis-request-timeout -5)))
    (check-exn exn:fail:contract? (lambda () (current-analysis-request-timeout "bad")))
    (define controller (make-source-controller #:resyntax #f))
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([current-source-controller controller])
          (check-equal? (current-analysis-request-timeout) 30)
          (define settings (hasheq 'analysis (hasheq 'requestTimeout 0.5)))
          (parameterize ([client-capability-workspace/configuration? #t])
            (fetch-configuration
              (lambda (method params callback)
                (check-equal? method "workspace/configuration")
                (callback (list settings)))))
          (check-equal? (current-analysis-request-timeout) 0.5)
          (update-configuration (hasheq 'analysis (hasheq 'requestTimeout "bad")))
          (check-equal? (current-analysis-request-timeout) 0.5)
          (for ([invalid (in-list (list (hasheq 'taskTimeout 601)
                                        (hasheq 'requestTimeout 0.009)
                                        (hasheq 'workers 129)
                                        (hasheq 'closedFileCacheSize 1025)))])
            ;; When another field is invalid, a valid request budget must not apply.
            (update-configuration
              (hasheq 'analysis (hash-set invalid 'requestTimeout
                                          (hash-ref invalid 'requestTimeout 2))))
            (check-equal? (current-analysis-request-timeout) 0.5))
          (parameterize ([current-analysis-request-timeout 7])
            (check-equal? (current-analysis-request-timeout) 7))
          (for ([empty (in-list (list (hasheq) (json-null) (list (json-null))))])
            (update-configuration settings)
            (update-configuration empty)
            (check-equal? (current-analysis-request-timeout) 30))
          (parameterize ([client-capability-workspace/configuration? #f])
            (fetch-configuration (lambda args (error 'unexpected "configuration pull"))))
          (check-equal? (current-analysis-request-timeout) 30)))
      (lambda () (source-controller-shutdown! controller))))

  (test-case "pushed settings resize active capacity and task/query deadlines are captured"
    (define gates (make-async-channel))
    (with-session
      (make-source-controller
        #:analysis-slots 1 #:resyntax #f
        #:analyze (lambda (snapshot)
                    (wait-at-gate gates snapshot)
                    (raise 'released)))
      (lambda (session)
        (define a (build-path scratch "settings-a.rkt"))
        (define b (build-path scratch "settings-b.rkt"))
        (open! session a "#lang racket/base\n(define x 1)\n")
        (define gate-a (await gates))
        (define completion-a (acquire session a))
        ;; An old task retains its long budget; the new task gets a short one.
        (push! session (hasheq 'workers 2 'taskTimeout 1 'requestTimeout 0.1))
        (open! session b "#lang racket/base\n(define y 1)\n")
        (await gates) ; proves the pushed worker count opened a second slot
        (define completion-b (acquire session b))
        (request! session 901 "textDocument/semanticTokens/full" a)
        (push! session (hasheq 'workers 2 'taskTimeout 90 'requestTimeout 30))
        (check-error (response session 901) ErrorCode-RequestFailed
                     "Analysis request deadline expired")
        (define expired (await completion-b))
        (check-eq? (Analysis-Outcome-state expired) 'failed)
        (check-eq? (Analysis-Outcome-value expired) 'analysis-timeout)
        (check-false (sync/timeout 0 completion-a))
        (release-gate! gate-a)
        (check-eq? (Analysis-Outcome-state (await completion-a)) 'failed)))))
