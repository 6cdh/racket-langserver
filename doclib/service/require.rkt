#lang racket/base

(require "interface.rkt"
         "interval-map-snapshot.rkt"
         racket/class
         data/interval-map
         drracket/check-syntax)

(provide require%)

(define require%
  (class base-service%
    (super-new)
    (define requires (make-interval-map))

    (define/override (serialize) (interval-map->snapshot requires))

    (define/override (deserialize! data)
      (set! requires (snapshot->interval-map data)))

    (define/override (get)
      requires)

    (define/override (reset)
      (set! requires (make-interval-map)))

    (define/override (expand start end)
      (interval-map-expand! requires start end))

    (define/override (contract start end)
      (interval-map-contract! requires start end))

    (define/override (syncheck:add-require-open-menu _text start finish file)
      (interval-map-set! requires start finish file))))

