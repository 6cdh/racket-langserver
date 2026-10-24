#lang racket/base

;; Test-only one-shot executor. Load the analysis stack only after redirecting
;; output, so language/lexer startup and analysis cannot reach LSP stdout.
(require racket/class
         racket/match
         racket/place
         racket/port
         racket/runtime-path)

(provide run echo)

(define-runtime-path doc-path "../../doclib/doc.rkt")
(define-runtime-path check-syntax-path "../../doclib/check-syntax.rkt")

(define (echo channel)
  (place-channel-put channel (place-channel-get channel)))

(define (run channel)
  (with-handlers ([exn:fail?
                   (lambda (exn) (place-channel-put channel (list 'failure (exn-message exn))))])
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (match-define (list uri text directory) (place-channel-get channel))
      (parameterize ([current-directory directory])
        (define make-doc (dynamic-require doc-path 'make-doc))
        (define doc-check-syntax (dynamic-require doc-path 'doc-check-syntax))
        (define result-trace (dynamic-require check-syntax-path 'CSResult-trace))
        (define result-succeeded? (dynamic-require check-syntax-path 'CSResult-succeed?))
        (define custodian (make-custodian))
        (define reply
          (dynamic-wind
            void
            (lambda ()
              (parameterize ([current-custodian custodian])
                (define result (doc-check-syntax (make-doc uri text)))
                (define data (send (result-trace result) serialize))
                (list (result-succeeded? result) data)))
            (lambda () (custodian-shutdown-all custodian))))
        ;; Resources are gone before the snapshot crosses the boundary.
        (place-channel-put channel reply)))))
