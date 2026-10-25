#lang racket/base

;; Local contract of the stdio place-start guard: it pauses a running reader,
;; keeps the caller's custodian, preserves values, resumes after a raise, and
;; leaves an already-suspended reader suspended. These tests do not reproduce
;; the Racket CS scheduler hang, and they do not exercise `call-as-atomic` or
;; cancellation from another thread during suspend.

(require rackunit
         "../../common/place-start-guard.rkt")

(module+ test
  (define (with-reader proc)
    (define reader (thread (lambda () (sync never-evt))))
    (dynamic-wind void
                  (lambda () (proc reader (make-reader-place-start-guard reader)))
                  (lambda () (kill-thread reader))))

  (test-case "startup pauses input, keeps the caller's custodian, and preserves return values"
    (with-reader
      (lambda (reader guard)
        (define job (make-custodian))
        (parameterize ([current-custodian job])
          (check-equal?
            (call-with-values
              (lambda ()
                (guard (lambda ()
                         (check-false (thread-running? reader))
                         (check-eq? (current-custodian) job)
                         (values 'place 'stdin 'stdout 'stderr))))
              list)
            '(place stdin stdout stderr)))
        (check-true (thread-running? reader))
        (custodian-shutdown-all job))))

  (test-case "a failed startup resumes input and propagates the original exception"
    (with-reader
      (lambda (reader guard)
        (define reason (exn:fail "startup failed" (current-continuation-marks)))
        (check-exn (lambda (e) (eq? reason e)) (lambda () (guard (lambda () (raise reason)))))
        (check-true (thread-running? reader)))))

  (test-case "startup preserves an existing reader suspension"
    (with-reader
      (lambda (reader guard)
        (thread-suspend reader)
        (guard void)
        (check-false (thread-running? reader))
        (thread-resume reader)))))
