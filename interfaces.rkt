#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/class
         racket/contract/base
         racket/match
         "json-util.rkt")

(provide WorkspaceEdit
         TextEdit
         CodeAction
         Diagnostic
         Pos
         Range
         abs-pos->Pos)

(define-json-expander WorkspaceEdit
  [changes any/c])
(define-json-expander TextEdit
  [range any/c]
  [newText string?])
(define-json-expander CodeAction
  [title string?]
  [kind string?]
  [diagnostics any/c]
  [isPreferred boolean?]
  [edit any/c])

(define-json-expander Diagnostic
  [range any/c]
  [severity (or/c 1 2 3 4)]
  [source string?]
  [message string?])

(define-match-expander Pos
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hash-table ['line (? exact-nonnegative-integer? l)]
                     ['character (? exact-nonnegative-integer? c)]))]))
  (λ (stx)
    (syntax-parse stx
      [(_ #:line l #:char c)
       (syntax/loc stx
         (hasheq 'line l
                 'character c))])))

(define-json-expander Range
  [start any/c]
  [end any/c])

(define (abs-pos->Pos editor pos)
  (match-define (list line char) (send editor pos->line/char pos))
  (Pos #:line line #:char char))

