#lang racket/base

;; Grouped `dynamic-require` for optional libraries. Failures escape unless
;; the caller supplies `#:fail`.
(require "debug.rkt")

(provide dynamic-imports)

(define-syntax-rule (import-once mod name fail-thunk)
  (define name
    (with-handlers ([exn:fail?
                     (lambda (reason)
                       (log-racket-langserver-info
                         "symbol '~a' from module '~a' failed to load: ~a"
                         'name mod (exn-message reason))
                       (fail-thunk))])
      (dynamic-require mod 'name))))

(define-syntax dynamic-import-mod
  (syntax-rules ()
    [(_ mod names ... #:fail fail-thunk)
     (begin (import-once mod names fail-thunk) ...)]
    [(_ mod names ...)
     (begin (define names (dynamic-require mod 'names)) ...)]))

;; Imports propagate errors by default. `#:fail` logs the failure and recovers;
;; that thunk's result becomes the failed binding's value.
(define-syntax dynamic-imports
  (syntax-rules ()
    [(_ (mod names ...) ... #:fail fail-thunk)
     (begin (dynamic-import-mod mod names ... #:fail fail-thunk) ...)]
    [(_ (mod names ...) ...)
     (begin (dynamic-import-mod mod names ...) ...)]))
