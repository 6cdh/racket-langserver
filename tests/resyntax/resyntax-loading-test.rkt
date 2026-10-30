#lang racket/base

;; Lazy optional imports, cached availability, package absence, and installed
;; stacks whose imports fail. Does not start a worker place.

(require rackunit racket/file racket/logging racket/place racket/runtime-path
         (only-in "../../doclib/external/resyntax.rkt" resyntax-collection-present?))

(define-runtime-path doc "../../doclib/doc.rkt")
(define-runtime-path controller-path "../../lsp/source-controller.rkt")
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
  (test-case "loading Doc defers Resyntax imports until an explicit availability query"
    (define imports 0)
    (with-module-path-mapping
      (lambda (path)
        (when (eq? path 'resyntax/grimoire/source) (set! imports (add1 imports)))
        (stub-resyntax-path path))
      (lambda ()
        (dynamic-require doc #f)
        (check-equal? imports 0)
        (define available? (dynamic-require adapter 'resyntax-available?))
        (check-false (available?))
        (check-true (positive? imports))
        (define imported imports)
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

(module+ test
  (test-case "controller construction and facade preparation have distinct loading boundaries"
    (define imports 0)
    (with-module-path-mapping
      (lambda (path)
        (when (resyntax-path? path)
          (set! imports (add1 imports)))
        (stub-resyntax-path path))
      (lambda ()
        (define make-controller
          (dynamic-require controller-path 'make-source-controller))
        (define stop (dynamic-require `(submod ,controller-path owner)
                                      'source-controller-shutdown!))
        (define controller (make-controller))
        (check-equal? imports 0)
        (stop controller)
        ;; Explicit injection bypasses even the default presence lookup.
        (parameterize ([current-library-collection-paths '()]
                       [current-library-collection-links '(#f)])
          (define injected (make-controller #:resyntax (lambda (_text _uri) '())))
          (stop injected))
        (check-equal? imports 0)
        ((dynamic-require adapter 'prepare-resyntax!))
        (check-true (positive? imports)))))

  (test-case "collection hint accepts compiled-only lookup and rejects absence"
    (define root (make-temporary-file "resyntax-collection~a" 'directory))
    (dynamic-wind
      void
      (lambda ()
        (parameterize ([current-library-collection-paths (list root)]
                       [current-library-collection-links '(#f)])
          (define absent-root (build-path root "absent"))
          (make-directory absent-root)
          (parameterize ([current-library-collection-paths (list absent-root)])
            (check-false (resyntax-collection-present?)))
          (make-directory (build-path root "resyntax"))
          ;; Lookup returns a prospective source path; source existence is not
          ;; a valid extra requirement for compiled-only installations.
          (make-directory (build-path root "resyntax" "compiled"))
          (call-with-output-file (build-path root "resyntax" "compiled" "main_rkt.zo") void)
          (check-true (resyntax-collection-present?))))
      (lambda () (delete-directory/files root)))))

(define-runtime-path stub-worker "../fixtures/resyntax-loading-worker.rkt")

(module+ test
  (test-case "concurrent direct calls share engine initialization and facade namespace"
    (with-module-path-mapping
      (lambda (path)
        (if (resyntax-path? path)
            `(submod ,stub-worker engine)
            path))
      (lambda ()
        (define-values (control remote) (place-channel))
        ((dynamic-require `(submod ,stub-worker observation) 'set-channel!) remote)
        (define available? (dynamic-require adapter 'resyntax-available?))
        (define analyze (dynamic-require adapter 'run-resyntax))
        (define results (make-channel))
        (define (report thunk)
          (thread (lambda ()
                    (channel-put results
                                 (with-handlers ([exn? values]) (thunk))))))
        (define first (report available?))
        (check-equal? (sync/timeout 10 control) 'engine-initializing)
        (define second
          (report (lambda ()
                    (parameterize ([current-namespace (make-base-namespace)])
                      (analyze "working" "untitled:test")))))
        ;; Both callers must be blocked before initialization is released. An
        ;; early return or exception would instead be observable on results.
        (sync (system-idle-evt))
        (check-false (sync/timeout 0 results))
        (place-channel-put control 'release)
        (define outcomes (list (sync/timeout 10 results) (sync/timeout 10 results)))
        (check-not-false (member #t outcomes))
        (check-not-false (member '() outcomes))
        (thread-wait first)
        (thread-wait second)
        (check-equal? (analyze "repeated" "untitled:test") '())
        (check-false (sync/timeout 0 control))))))

(module+ test
  (test-case "direct Doc calls preserve result identity across caller namespaces"
    (define available? (dynamic-require adapter 'resyntax-available?))
    (when (available?)
      (define make-doc (dynamic-require doc 'make-doc))
      (define analyze (dynamic-require doc 'doc-resyntax))
      (define document (make-doc "file:///namespace-test.rkt"
                                 "#lang racket\n(or 1 (or 2 3))"))
      (parameterize ([current-namespace (make-base-namespace)])
        (check-equal? (length (analyze document)) 1)))))
