#lang racket/base

;; Pause the LSP stdin reader while this process creates a worker place.
;; Workaround for a Racket CS hang when place creation overlaps input
;; readiness. `main-loop` installs it; the session contract is in
;; `lsp/analysis-lifecycle.md`.
(require ffi/unsafe/atomic)

(provide make-reader-place-start-guard)

;; Construct this only when `(current-custodian)` solely manages `reader`
;; (stdio: the main thread that reads stdin). The returned procedure runs on
;; analysis threads and returns whatever `thunk` returns, including multiple
;; values.
;;
;; `thread-suspend` raises unless the current custodian solely manages the
;; thread, so suspend and resume switch to that captured reader custodian.
;; `thunk` keeps the analysis thread's current custodian, so the new place
;; stays in the generation rather than moving onto the reader's custodian.
;;
;; (-> thread? (-> (-> any) any))
(define (make-reader-place-start-guard reader)
  (define reader-custodian (current-custodian))
  (lambda (thunk)
    ;; Atomic: no other Racket thread can interrupt this create call while the
    ;; reader is suspended. `dynamic-wind` still resumes the reader if `thunk`
    ;; raises. A reader that is not running is left unchanged.
    (call-as-atomic
      (lambda ()
        (define running? (thread-running? reader))
        (dynamic-wind
          (lambda ()
            (when running?
              (parameterize ([current-custodian reader-custodian])
                (thread-suspend reader))))
          thunk
          (lambda ()
            (when running?
              (parameterize ([current-custodian reader-custodian])
                (thread-resume reader)))))))))
