#lang racket/base

;; Interrupted real Check Syntax and installed Resyntax must clean job
;; resources and reuse their own places. An availability skip does not
;; establish Resyntax integration.

(require "../../lsp/analysis-place-protocol.rkt"
         rackunit
         racket/file
         racket/format
         racket/match
         racket/place
         racket/port
         racket/runtime-path
         racket/tcp
         "../../doclib/external/resyntax.rkt")

(struct Resources-Started (kind port) #:prefab)

(define-runtime-path worker-path "../../lsp/analysis-place.rkt")

(define (receive worker)
  (or (sync/timeout 60 worker) (error 'test "worker response timed out")))

;; Source shared by the reader and expansion programs: announce that child
;; resources exist, then block until the job custodian shuts them down.
(define (blocking-handshake-source port started-tag)
  (~a "  (struct Resources-Started (kind port) #:prefab)\n"
      "  (define owned (tcp-listen 0 4 #t \"127.0.0.1\"))\n"
      "  (define-values (h p r rp) (tcp-addresses owned #t))\n"
      "  (define-values (i o) (tcp-connect \"127.0.0.1\" " port "))\n"
      "  (thread (lambda () (sync never-evt)))\n"
      "  (write (Resources-Started '" started-tag " p) o) (newline o) (flush-output o)\n"
      "  (sync never-evt)"))

(module+ test
  (test-case "real workers clean cancelled jobs and reuse each analysis stack"
    (define root (make-temporary-file "shared-worker~a" 'directory))
    (define listener (tcp-listen 0 4 #t "127.0.0.1"))
    (define-values (_host port _remote _remote-port) (tcp-addresses listener #t))
    (define worker (dynamic-place worker-path 'run))
    (define resyntax-worker (dynamic-place worker-path 'run))
    (dynamic-wind
      void
      (lambda ()
        (place-channel-put worker (Place-Prepare 'check-syntax))
        ;; A stale cancel during first initialization must not own its lifetime.
        (place-channel-put worker (Place-Cancel 'obsolete))
        (check-equal? (receive worker) (Place-Ready))
        (define reader (build-path root "reader.rkt"))
        (call-with-output-file reader
          (lambda (out)
            (display
              (~a "#lang racket/base\n(require racket/tcp)\n"
                  "(provide read-syntax read)\n"
                  "(define (read in) (syntax->datum (read-syntax #f in)))\n"
                  "(define (read-syntax src in)\n"
                  (blocking-handshake-source port 'reader-start)
                  ")\n")
              out)))
        (define uri (~a "file://" (build-path root "source.rkt")))
        (place-channel-put worker
                           (Place-Start 1 uri
                                        (~a "#reader (file " (~s (path->string reader)) ")\n")
                                        root))
        ;; This message is emitted inside read-syntax after child resources exist.
        (check-not-false (sync/timeout 60 listener))
        (define-values (reader-in reader-out) (tcp-accept listener))
        (match-define (Resources-Started 'reader-start owned-port) (read reader-in))
        (place-channel-put worker (Place-Cancel 1))
        (check-equal? (receive worker) (Place-Terminal (Place-Cancelled)))
        (check-not-false (sync/timeout 5 reader-in))
        (check-true (eof-object? (read reader-in)))
        (check-exn exn:fail:network?
                   (lambda () (tcp-connect "127.0.0.1" owned-port)))
        (close-input-port reader-in)
        (close-output-port reader-out)
        ;; A boundary exception must clean the job and preserve this exact place.
        (place-channel-put worker
                           (Place-Start 2 uri #f root))
        (check-match (receive worker) (Place-Terminal (Place-Failure (? string?))))
        (place-channel-put worker (Place-Cancel 1))
        (place-channel-put worker
                           (Place-Start 3 uri
                                        "#lang racket/base\n(define value 42)\n" root))
        (check-match (receive worker) (Place-Terminal (Place-Check-Syntax-Result #t _)))
        (place-channel-put resyntax-worker (Place-Prepare 'resyntax))
        (check-equal? (receive resyntax-worker) (Place-Ready))
        (place-channel-put resyntax-worker
                           (Place-Start 4 uri "#lang racket/base\n" root))
        (check-match (receive resyntax-worker) (Place-Terminal (Place-Resyntax-Result (? list?))))
        (when (resyntax-available?)
          ;; The message comes from real Resyntax expansion, after its child
          ;; resources exist. Cancellation must preserve the initialized stack.
          (define blocking-source
            (~a "#lang racket/base\n"
                "(require (for-syntax racket/base racket/tcp))\n"
                "(begin-for-syntax\n"
                (blocking-handshake-source port 'resyntax-start)
                ")\n"))
          (place-channel-put resyntax-worker (Place-Start 20 uri blocking-source root))
          (check-not-false (sync/timeout 60 listener))
          (define-values (in out) (tcp-accept listener))
          (match-define (Resources-Started 'resyntax-start child-port) (read in))
          (place-channel-put resyntax-worker (Place-Cancel 20))
          (check-equal? (receive resyntax-worker) (Place-Terminal (Place-Cancelled)))
          (check-not-false (sync/timeout 5 in))
          (check-true (eof-object? (read in)))
          (check-exn exn:fail:network? (lambda () (tcp-connect "127.0.0.1" child-port)))
          (close-input-port in)
          (close-output-port out)
          (place-channel-put resyntax-worker
                             (Place-Start 21 uri
                                          "#lang racket\n(or 1 (or 2 3))" root))
          (define result (receive resyntax-worker))
          (check-match result (Place-Terminal (Place-Resyntax-Result (list _))))
          (match-define (Place-Terminal (Place-Resyntax-Result (list finding))) result)
          (check-equal? (hash-ref finding 'new-text) "(or 1 2 3)"))
        ;; Either completion or cancellation may win; each job has one terminal.
        ;; A subsequent job response also detects a duplicate terminal.
        (for ([id (in-range 5 9)])
          (place-channel-put worker
                             (Place-Start id uri
                                          "#lang racket/base\n" root))
          (place-channel-put worker (Place-Cancel id))
          (define terminal (receive worker))
          (check-true
            (match terminal
              [(Place-Terminal (or (Place-Check-Syntax-Result #t _) (Place-Cancelled))) #t]
              [_ #f])))
        (check-false (sync/timeout 0 (place-dead-evt worker))))
      (lambda ()
        (place-kill worker)
        (place-wait worker)
        (place-kill resyntax-worker)
        (place-wait resyntax-worker)
        (tcp-close listener)
        (delete-directory/files root)))))
