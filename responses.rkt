#lang racket/base
(require json
         racket/contract/base)

(define not-given (gensym 'not-given))

;; Constructor for a response object representing success.
(define (success-response id result)
  (hasheq 'jsonrpc "2.0"
          'id id
          'result result))

;; Constructor for a response object representing failure.
(define (error-response id code message [data not-given])
  (define err (hasheq 'code code
                      'message message))
  (define err* (if (eq? data not-given)
                   err
                   (hash-set err 'data data)))
  (hasheq 'jsonrpc "2.0"
          'id id
          'error err*))

(provide
 (contract-out
  [success-response
   ((or/c number? string?) jsexpr? . -> . jsexpr?)]
  [error-response
   (->* ((or/c number? string? (json-null)) number? string?)
        (any/c)
        jsexpr?)]))