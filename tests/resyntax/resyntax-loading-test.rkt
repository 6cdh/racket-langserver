#lang racket/base

;; Eager optional imports, cached availability, package absence, and installed
;; stacks whose imports fail. Does not start a worker place.

(require rackunit racket/logging racket/runtime-path)

(define-runtime-path doc "../../doclib/doc.rkt")
(define-runtime-path adapter "../../doclib/external/resyntax.rkt")

(define (resyntax-path? path)
  (and (symbol? path) (regexp-match? #rx"^resyntax(/|$)" (symbol->string path))))

;; Isolate the module registry so one case's cached availability cannot leak.
(define (with-module-path-mapping map-path proc)
  (parameterize ([current-namespace (make-base-namespace)])
    (define resolver (current-module-name-resolver))
    (parameterize ([current-module-name-resolver
                    (case-lambda
                      [(path relative stx load?) (resolver (map-path path) relative stx load?)]
                      [(path namespace) (resolver path namespace)])])
      (proc))))

;; A present module with missing exports avoids depending on installed Resyntax.
(define (stub-resyntax-path path)
  (if (resyntax-path? path) 'racket/base path))

(module+ test
  (test-case "loading Doc imports Resyntax once and availability reads the cached flag"
    (define imports 0)
    (with-module-path-mapping
      (lambda (path)
        (when (eq? path 'resyntax/grimoire/source) (set! imports (add1 imports)))
        (stub-resyntax-path path))
      (lambda ()
        (dynamic-require doc #f)
        (check-true (positive? imports))
        (define imported imports)
        (define available? (dynamic-require adapter 'resyntax-available?))
        (check-false (available?))
        (check-false (available?))
        (check-equal? ((dynamic-require adapter 'run-resyntax)
                       "#lang racket/base\n" "untitled:test")
                      '())
        (check-equal? imports imported))))

  (test-case "absent optional Resyntax disables the adapter and returns empty findings"
    (with-module-path-mapping
      (lambda (path)
        (when (resyntax-path? path)
          (raise (exn:fail:filesystem:missing-module
                   "optional Resyntax is absent" (current-continuation-marks) path)))
        path)
      (lambda ()
        (check-false ((dynamic-require adapter 'resyntax-available?)))
        (check-equal? ((dynamic-require adapter 'run-resyntax) "#lang racket\n" "untitled:test")
                      '()))))

  (test-case "installed-stack initialization errors log and disable the adapter"
    (define logs '())
    (with-intercepted-logging
      (lambda (event) (set! logs (cons (vector-ref event 1) logs)))
      (lambda ()
        (with-module-path-mapping
          (lambda (path)
            (when (eq? path 'resyntax/grimoire/source)
              (error 'installed-stack "initialization failed"))
            (stub-resyntax-path path))
          (lambda ()
            (check-false ((dynamic-require adapter 'resyntax-available?)))
            (check-equal? ((dynamic-require adapter 'run-resyntax) "#lang racket\n" "untitled:test")
                          '()))))
      'info)
    (check-not-false (ormap (lambda (message) (regexp-match? #rx"resyntax/grimoire/source" message))
                            logs))
    (check-not-false (ormap (lambda (message) (regexp-match? #rx"initialization failed" message))
                            logs)))

  (test-case "missing exports log and disable the adapter without escaping to callers"
    (define logs '())
    (with-intercepted-logging
      (lambda (event) (set! logs (cons (vector-ref event 1) logs)))
      (lambda ()
        (with-module-path-mapping
          stub-resyntax-path
          (lambda ()
            (check-false ((dynamic-require adapter 'resyntax-available?)))
            (check-equal? ((dynamic-require adapter 'run-resyntax) "#lang racket\n" "untitled:test")
                          '()))))
      'info)
    (check-not-false (ormap (lambda (message) (regexp-match? #rx"string-source" message)) logs))))
