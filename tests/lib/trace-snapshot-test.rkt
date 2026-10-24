#lang racket/base

(require rackunit
         drracket/check-syntax
         racket/class
         racket/dict
         racket/place
         racket/runtime-path
         data/interval-map
         "../../doclib/service/completion.rkt"
         "../../doclib/service/require.rkt"
         "../../doclib/service/interval-map-snapshot.rkt"
         "../../doclib/internal-types.rkt")

(define-runtime-path worker-path "trace-place-worker.rkt")

(module+ test
  (test-case "completed completion candidates restore and survive edits"
    (define original (new completion%))
    (send original walk-stx (ExpandResult #'unused #'(define-values (λ-name) 1) '()))
    (define restored (new completion%))
    (check-true (place-message-allowed? (send original serialize)))
    (send restored deserialize! (send original serialize))
    (check-not-false (memq 'λ-name (send restored get)))
    (for ([service (in-list (list original restored))])
      (send service expand 0 2)
      (send service contract 1 2))
    (check-equal? (send restored get) (send original get)))

  (test-case "require intervals restore and shift after edits"
    (define original (new require%))
    (send original syncheck:add-require-open-menu #f 3 8 (string->path "dep.rkt"))
    (define restored (new require%))
    (check-true (place-message-allowed? (send original serialize)))
    (send restored deserialize! (send original serialize))
    (check-equal? (interval-map-ref (send restored get) 4) (string->path "dep.rkt"))
    (for ([service (in-list (list original restored))])
      (send service expand 0 2)
      (send service contract 6 7))
    (check-equal? (dict->list (send restored get)) (dict->list (send original get))))

  (test-case "interval snapshots preserve shared identities across a deleted gap"
    (define intervals (make-interval-map))
    (define shared (list "shared"))
    (interval-map-set! intervals 0 2 shared)
    (interval-map-set! intervals 4 6 shared)
    ;; Reconstructing lists deliberately prevents accidental reuse by the decoder.
    (define restored
      (snapshot->interval-map (interval-map->snapshot intervals) (lambda (v) (map values v))))
    (check-eq? (interval-map-ref restored 0) (interval-map-ref restored 4))
    (interval-map-contract! intervals 2 4)
    (interval-map-contract! restored 2 4)
    (check-equal? (dict->list restored) (dict->list intervals)))

  (test-case "real place preserves shared and equal-but-distinct interval values"
    (define intervals (make-interval-map))
    (define shared (list (string-copy "same")))
    (define distinct (list (string-copy "same")))
    (check-equal? shared distinct)
    (check-not-eq? shared distinct)
    (interval-map-set! intervals 0 2 shared)
    (interval-map-set! intervals 4 6 shared)
    (interval-map-set! intervals 8 10 distinct)
    (define echo (dynamic-place worker-path 'echo))
    (dynamic-wind
      void
      (lambda ()
        (place-channel-put echo (interval-map->snapshot intervals))
        (define data (sync/timeout 10 echo))
        (check-not-false data)
        (define restored (snapshot->interval-map data))
        (check-eq? (interval-map-ref restored 0) (interval-map-ref restored 4))
        (check-not-eq? (interval-map-ref restored 0) (interval-map-ref restored 8))
        (for ([map (in-list (list intervals restored))])
          (interval-map-contract! map 6 8)
          (interval-map-contract! map 2 4))
        (check-equal? (length (dict->list restored)) 2)
        (check-equal? (dict->list restored) (dict->list intervals)))
      (lambda () (place-kill echo)))))
