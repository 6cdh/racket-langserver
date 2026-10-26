#lang racket/base

(require rackunit
         racket/logging)

(provide install-test-log-capture!)

;; Require this before server modules create their loggers. Explicit log
;; receivers still see expected errors; stderr gets them only if the test fails.
(define logger
  (make-logger #f (current-logger) 'none 'racket-langserver 'debug))
(current-logger logger)

;; Each test module installs the wrapper in its own execution thread, including
;; when raco test --direct reuses this module in a shared namespace.
(define (install-test-log-capture!)
  (define original-around (current-test-case-around))
  (current-test-case-around
    (lambda (thunk)
      (original-around
        (lambda ()
          (define errors (open-output-string))
          (with-handlers ([(lambda (_) #t)
                           (lambda (failure)
                             (display (get-output-string errors) (current-error-port))
                             (raise failure))])
            (with-logging-to-port errors thunk #:logger logger 'error 'racket-langserver)))))))

