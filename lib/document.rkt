#lang typed/racket

(provide (struct-out Document)
         make-document)

(struct Document
  ([uri : String]
   [buffer : String]
   [analysis : (Option Any)]))

(: make-document (String String -> Document))
(define (make-document uri buffer)
  (Document uri buffer #f))

