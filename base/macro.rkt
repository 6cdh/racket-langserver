#lang racket/base

(provide ~>
         loop-forever)

(require syntax/parse/define)

(define-syntax-parse-rule (~> x:id flows ...)
  (let* ([x flows] ...)
    x))

(define-syntax-parse-rule (loop-forever body ...)
  (let loop ()
    body ...
    (loop)))
