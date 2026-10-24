#lang racket/base

(require data/interval-map
         racket/dict
         racket/match)

(provide interval-map->snapshot snapshot->interval-map)

;; Encode each eq? value once. Adjacent intervals can merge on identity, so
;; independently decoding each entry changes subsequent edit behavior.
;; IDs belong only to this map's snapshot.
(define (interval-map->snapshot intervals [encode values])
  (define ids (make-hasheq))
  (define encoded '())
  (define entries
    (for/list ([(range value) (in-dict intervals)])
      (define id
        (hash-ref! ids value
                   (lambda ()
                     (define id (hash-count ids))
                     (set! encoded (cons (encode value) encoded))
                     id)))
      (list (car range) (cdr range) id)))
  (list entries (reverse encoded)))

(define (snapshot->interval-map snapshot [decode values])
  (match-define (list entries encoded) snapshot)
  (define decoded (list->vector (map decode encoded)))
  (define intervals (make-interval-map))
  (for ([entry (in-list entries)])
    (match-define (list start end id) entry)
    (interval-map-set! intervals start end (vector-ref decoded id)))
  intervals)
