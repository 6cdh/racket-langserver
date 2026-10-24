#lang racket/base

;; Load the analysis stack only after stdout is redirected. Each assignment
;; owns its namespace and resources; the place retains only loaded libraries.
(require racket/class
         racket/match
         racket/place
         racket/port
         racket/runtime-path)

(provide run)

(define-runtime-path doc-path "../doclib/doc.rkt")
(define-runtime-path syntax-path "../doclib/check-syntax.rkt")

(define (failure-detail reason)
  (if (exn? reason) (exn-message reason) "analysis raised a non-exception value"))

(define (run channel)
  (parameterize ([current-output-port (open-output-nowhere)])
    (define make-doc (dynamic-require doc-path 'make-doc))
    (define doc-check-syntax (dynamic-require doc-path 'doc-check-syntax))
    (define CSResult-trace (dynamic-require syntax-path 'CSResult-trace))
    (define CSResult-succeed? (dynamic-require syntax-path 'CSResult-succeed?))
    (let loop ()
      (match-define (list generation job uri text directory) (place-channel-get channel))
      (define custodian (make-custodian))
      (define reply
        (with-handlers ([(lambda (_) #t)
                         (lambda (reason) (list 'failure (failure-detail reason)))])
          (parameterize ([current-custodian custodian]
                         [current-directory directory])
            (define result (doc-check-syntax (make-doc uri text)))
            (define data (send (CSResult-trace result) serialize))
            (unless (place-message-allowed? data)
              (error 'analysis-place "trace is not transferable"))
            (list 'result (CSResult-succeed? result) data))))
      ;; Place stderr is buffered independently of the parent's error port.
      ;; Preserve reader output before reuse or shutdown can kill this place.
      (flush-output (current-error-port))
      ;; Never return a worker to the parent before job resources are gone.
      (custodian-shutdown-all custodian)
      (place-channel-put channel (list generation job reply))
      (loop))))
