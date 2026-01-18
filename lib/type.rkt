#lang typed/racket/base

(provide (struct-out IPosition)
         (struct-out IRange))

(struct IPosition
  ([line : Index]
   [character : Index]))

(struct IRange
  ([start : IPosition]
   [end : IPosition]))
