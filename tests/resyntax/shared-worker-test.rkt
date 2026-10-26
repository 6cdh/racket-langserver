#lang racket/base

;; Interrupted real Check Syntax and installed Resyntax must clean job
;; resources and reuse the same place. An availability skip does not establish
;; Resyntax integration.

(require rackunit
         racket/file
         racket/format
         racket/match
         racket/place
         racket/port
         racket/runtime-path
         racket/tcp
         "../../doclib/external/resyntax.rkt")

(define-runtime-path worker-path "../../lsp/analysis-place.rkt")

(define (receive worker)
  (or (sync/timeout 60 worker) (error 'test "worker response timed out")))

;; Source shared by the reader and expansion programs: announce that child
;; resources exist, then block until the job custodian shuts them down.
(define (blocking-handshake-source port started-tag)
  (~a "  (define owned (tcp-listen 0 4 #t \"127.0.0.1\"))\n"
      "  (define-values (h p r rp) (tcp-addresses owned #t))\n"
      "  (define-values (i o) (tcp-connect \"127.0.0.1\" " port "))\n"
      "  (thread (lambda () (sync never-evt)))\n"
      "  (write (list '" started-tag " p) o) (newline o) (flush-output o)\n"
      "  (sync never-evt)"))

(module+ test
  (test-case "real worker cleans cancelled readers and reuses both analysis stacks"
    (define root (make-temporary-file "shared-worker~a" 'directory))
    (define listener (tcp-listen 0 4 #t "127.0.0.1"))
    (define-values (_host port _remote _remote-port) (tcp-addresses listener #t))
    (define worker (dynamic-place worker-path 'run))
    (dynamic-wind
      void
      (lambda ()
        (place-channel-put worker '(prepare check-syntax))
        ;; A stale cancel during first initialization must not own its lifetime.
        (place-channel-put worker '(cancel obsolete))
        (check-equal? (receive worker) '(ready check-syntax))
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
                           (list 'start 1 'check-syntax uri
                                 (~a "#reader (file " (~s (path->string reader)) ")\n") root))
        ;; This message is emitted inside read-syntax after child resources exist.
        (check-not-false (sync/timeout 60 listener))
        (define-values (reader-in reader-out) (tcp-accept listener))
        (match-define (list 'reader-start owned-port) (read reader-in))
        (place-channel-put worker '(cancel 1))
        (check-equal? (receive worker) '(terminal 1 (cancelled)))
        (check-not-false (sync/timeout 5 reader-in))
        (check-true (eof-object? (read reader-in)))
        (check-exn exn:fail:network?
                   (lambda () (tcp-connect "127.0.0.1" owned-port)))
        (close-input-port reader-in)
        (close-output-port reader-out)
        ;; A boundary exception must clean the job and preserve this exact place.
        (place-channel-put worker
                           (list 'start 2 'check-syntax uri #f root))
        (check-match (receive worker) (list 'terminal 2 (list 'failure (? string?))))
        (place-channel-put worker '(cancel 1))
        (place-channel-put worker
                           (list 'start 3 'check-syntax uri
                                 "#lang racket/base\n(define value 42)\n" root))
        (check-match (receive worker) (list 'terminal 3 (list 'result #t _)))
        (place-channel-put worker '(prepare resyntax))
        (check-equal? (receive worker) '(ready resyntax))
        (place-channel-put worker
                           (list 'start 4 'resyntax uri "#lang racket/base\n" root))
        (check-match (receive worker) (list 'terminal 4 (list 'result (? list?))))
        (when (resyntax-available?)
          ;; The message comes from real Resyntax expansion, after its child
          ;; resources exist. Cancellation must preserve the initialized stack.
          (define blocking-source
            (~a "#lang racket/base\n"
                "(require (for-syntax racket/base racket/tcp))\n"
                "(begin-for-syntax\n"
                (blocking-handshake-source port 'resyntax-start)
                ")\n"))
          (place-channel-put worker (list 'start 20 'resyntax uri blocking-source root))
          (check-not-false (sync/timeout 60 listener))
          (define-values (in out) (tcp-accept listener))
          (match-define (list 'resyntax-start child-port) (read in))
          (place-channel-put worker '(cancel 20))
          (check-equal? (receive worker) '(terminal 20 (cancelled)))
          (check-not-false (sync/timeout 5 in))
          (check-true (eof-object? (read in)))
          (check-exn exn:fail:network? (lambda () (tcp-connect "127.0.0.1" child-port)))
          (close-input-port in)
          (close-output-port out)
          (place-channel-put worker
                             (list 'start 21 'resyntax uri
                                   "#lang racket\n(or 1 (or 2 3))" root))
          (define result (receive worker))
          (check-match result (list 'terminal 21 (list 'result (list _))))
          (match-define (list 'terminal 21 (list 'result (list finding))) result)
          (check-equal? (hash-ref finding 'new-text) "(or 1 2 3)"))
        ;; Either completion or cancellation may win; each ID has one terminal.
        ;; The subsequent ready response also detects a duplicate terminal.
        (for ([id (in-range 5 9)])
          (place-channel-put worker
                             (list 'start id 'check-syntax uri
                                   "#lang racket/base\n" root))
          (place-channel-put worker (list 'cancel id))
          (define terminal (receive worker))
          (check-true
            (match terminal
              [(list 'terminal received-id (or (list 'result #t _) '(cancelled)))
               (equal? received-id id)]
              [_ #f]))
          (place-channel-put worker '(prepare check-syntax))
          (check-equal? (receive worker) '(ready check-syntax)))
        (check-false (sync/timeout 0 (place-dead-evt worker))))
      (lambda ()
        (place-kill worker)
        (place-wait worker)
        (tcp-close listener)
        (delete-directory/files root)))))
