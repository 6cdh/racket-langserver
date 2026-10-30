#lang racket/base

;; Engine implementation for optional Resyntax refactoring analysis.
;;
;; Dynamically imports Resyntax libraries when instantiated. If any dependency is
;; missing or fails to load, logs a message and disables the engine, allowing the
;; language server to continue running without Resyntax support.

(require "../../common/dynamic-import.rkt"
         "../../common/interfaces.rkt"
         "../../common/path-util.rkt")

(provide run-resyntax
         resyntax-available?)

;; Bind the optional API once when the engine is instantiated.
;; Any failed import logs and disables the adapter; callers inspect the
;; cached flag instead of handling import exceptions.
(define has-resyntax? #t)

(define (resyntax-available?)
  has-resyntax?)

(define (disable-resyntax!)
  (set! has-resyntax? #f))

(dynamic-imports
  ('resyntax/grimoire/source
    file-source
    modified-source
    string-source
    source->string)
  ('resyntax/grimoire/string-replacement
    string-replacement-start string-replacement-original-end string-replacement-new-text)
  ('resyntax/private/refactoring-result
    refactoring-result-string-replacement refactoring-result-message
    refactoring-result-rule-name refactoring-result-set-results)
  ('resyntax resyntax-analyze)
  #:fail disable-resyntax!)

(define (run-resyntax text [uri #f])
  (if has-resyntax?
      (run-resyntax-impl text uri)
      '()))

(define (run-resyntax-impl text uri)
  (define source (make-resyntax-source text uri))
  (define result-set (resyntax-analyze source))
  (for/list ([result (in-list (refactoring-result-set-results result-set))])
    (refactoring-result->resyntax-result result source)))

;; Resolves the file path from a document URI so Resyntax parameterizes
;; current-directory to the file's directory, enabling relative requires to resolve.
(define (make-resyntax-source text uri)
  (define file-path (and (string? uri) (file-uri? uri) (uri->path uri)))
  (if file-path
      (with-handlers ([exn:fail? (lambda (_) (string-source text))])
        (modified-source (file-source file-path) text))
      (string-source text)))

(define (refactoring-result->resyntax-result result source)
  (define replacement (refactoring-result-string-replacement result))
  (Resyntax-Result (string-replacement-start replacement)
                   (string-replacement-original-end replacement)
                   (refactoring-result-message result)
                   (refactoring-result-rule-name result)
                   (string-replacement-new-text replacement (source->string source))))
