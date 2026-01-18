#lang typed/racket/base

(provide (struct-out Document)
         make-document)

(require "textbuffer.rkt")

(struct Document
  ([uri : String]
   [buffer : TextBuffer]
   [analysis : (Option Any)]))

(: make-document (String String -> Document))
(define (make-document uri text)
  (Document uri (make-textbuffer text) #f))

