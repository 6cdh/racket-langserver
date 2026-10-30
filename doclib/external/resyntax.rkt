#lang racket/base

;; Lightweight facade for optional Resyntax refactoring support.
;;
;; Requiring this module does not load the Resyntax engine or its dependencies.
;; The engine is loaded on demand when explicitly prepared or queried.

(require racket/runtime-path)

(provide prepare-resyntax!
         run-resyntax
         resyntax-available?
         resyntax-collection-present?)

;; Module instantiation owns initialization and cached availability. Requiring
;; this facade (including through Doc) does not instantiate the engine.
(define-runtime-module-path-index engine-path "resyntax-engine.rkt")

;; Preserve module and result-structure identity when callers change namespaces.
(define-namespace-anchor facade-anchor)
(define facade-namespace (namespace-anchor->namespace facade-anchor))

;; dynamic-require does not serialize concurrent module initialization.
;; Protect binding lookup, while leaving analyzer execution outside the lock.
(define engine-load-lock (make-semaphore 1))

(define (engine-export name)
  (parameterize ([current-namespace facade-namespace])
    (call-with-semaphore
      engine-load-lock
      (lambda () (dynamic-require engine-path name)))))

(define (prepare-resyntax!)
  (engine-export #f))

(define (resyntax-available?)
  ((engine-export 'resyntax-available?)))

(define (run-resyntax text [uri #f])
  ((engine-export 'run-resyntax) text uri))

;; Cheap check for collection presence before spawning a dedicated worker place.
;; Only engine imports inside that place establish true usability.
;; Collection lookup also supports compiled-only installations.
(define (resyntax-collection-present?)
  (and (collection-file-path "main.rkt" "resyntax" #:fail (lambda (_) #f)) #t))
