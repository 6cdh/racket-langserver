#lang racket/base

;; Grouped module-level imports, and failures that escape without `#:fail`.

(require rackunit
         "../../common/dynamic-import.rkt")

(module+ test
  (dynamic-imports ('racket/list take drop)
                   ('racket/string string-join))

  (test-case "grouped imports define module-level bindings"
    (check-equal? (take '(a b c) 2) '(a b))
    (check-equal? (drop '(a b c) 1) '(b c))
    (check-equal? (string-join '("a" "b")) "a b"))

  (test-case "imports without a fail thunk propagate failures"
    (check-exn #rx"missing-dynamic-import-binding"
               (lambda ()
                 (dynamic-imports ('racket/base missing-dynamic-import-binding))
                 (void)))))
