#lang racket/base

;; Test fixture worker for simulating Resyntax loading conditions in place processes.
;; Supports delayed initialization, cancellation during loading, and simulated broken installations.

(module observation racket/base
  (provide channel set-channel!)
  (define channel #f)
  (define (set-channel! value) (set! channel value)))

(require racket/place
         racket/runtime-path
         (submod "." observation))

(provide run run-broken)

(define-runtime-path worker "../../lsp/analysis-place.rkt")
(define-runtime-path self "resyntax-loading-worker.rkt")

;; Every optional import resolves to this module. Initialization and release
;; use a separate channel so cancellation is exercised while loading is blocked.
(module engine racket/base
  (require racket/place (submod ".." observation))
  (provide string-source file-source modified-source source->string string-replacement-start
           string-replacement-original-end string-replacement-new-text
           refactoring-result-string-replacement refactoring-result-message
           refactoring-result-rule-name refactoring-result-set-results resyntax-analyze)
  (define helper (thread (lambda () (sync never-evt))))
  (place-channel-put channel 'engine-initializing)
  (void (place-channel-get channel))
  ;; Initialization cannot finish until the control thread has drained runnable
  ;; command processing, including cancellation queued before the release.
  (void (sync (system-idle-evt)))
  (define (resyntax-analyze source)
    (when (thread-dead? helper)
      (error 'stub "initialization helper was killed"))
    (when (equal? source "fail")
      (error 'stub "runtime failure"))
    '())
  (define (string-source text) text)
  (define (file-source path) path)
  (define (modified-source _orig text) text)
  (define (source->string text) text)
  (define (refactoring-result-set-results results) results)
  (define (string-replacement-start _) 0)
  (define (string-replacement-original-end _) 0)
  (define (string-replacement-new-text _ text) text)
  (define (refactoring-result-string-replacement _) #f)
  (define (refactoring-result-message _) "")
  (define (refactoring-result-rule-name _) 'stub))

(define (run-broken channel)
  (run channel #t))

(define (resyntax-module-path? path)
  (and (symbol? path)
       (regexp-match? #rx"^resyntax(/|$)" (symbol->string path))))

(define (run channel [broken? #f])
  (set-channel! (place-channel-get channel))
  (define resolver (current-module-name-resolver))
  (parameterize ([current-module-name-resolver
                  (case-lambda
                    [(path relative stx load?)
                     (define target
                       (if (resyntax-module-path? path)
                           (if broken? 'racket/base `(submod ,self engine))
                           path))
                     (resolver target relative stx load?)]
                    [(path namespace)
                     (resolver path namespace)])])
    ((dynamic-require worker 'run) channel)))
