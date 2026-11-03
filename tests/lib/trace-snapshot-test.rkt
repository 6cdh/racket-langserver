#lang racket/base

(require rackunit
         drracket/check-syntax
         racket/class
         racket/place
         racket/runtime-path
         "../../common/interval-map.rkt"
         "../../common/source.rkt"
         "../../doclib/service/completion.rkt"
         "../../doclib/service/declaration.rkt"
         "../../doclib/service/interface.rkt"
         "../../doclib/service/require.rkt"
         "../../doclib/internal-types.rkt"
         "../../doclib/editor.rkt")

(define-runtime-path worker-path "trace-place-worker.rkt")

(module+ test
  (test-case "a service without serialization methods fails explicitly"
    (define service (new base-service%))
    (check-exn #rx"does not implement serialization" (lambda () (send service serialize)))
    (check-exn #rx"does not implement deserialization"
               (lambda () (send service deserialize! #f))))

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
    (check-equal? (interval-map->list (send restored get)) (interval-map->list (send original get))))

  (test-case "declaration snapshot restores graph relations and survives edits"
    (define source (path->source (string->path "/tmp/fake.rkt")))
    (define doc-text (new lsp-editor%))
    (send doc-text insert "#lang racket/base\n(define x 1)\n(+ x x)\n" 0)
    (define original (new declaration% [source source] [doc-text doc-text]))
    (send original syncheck:add-definition-target/phase-level+space #f 27 28 'x '() 0)
    (send original syncheck:add-arrow/name-dup #f 27 28 #f 35 36 #t 0 #f #f)
    (send original syncheck:add-arrow/name-dup #f 27 28 #f 37 38 #t 0 #f #f)
    (send original walk-stx #f)

    (define serialized (send original serialize))
    (check-true (place-message-allowed? serialized))

    (define restored (new declaration% [source source] [doc-text doc-text]))
    (send restored deserialize! serialized)

    (check-equal? (send restored definition-at 27) (send original definition-at 27))
    (check-equal? (send restored uses-at 27) (send original uses-at 27))
    (check-equal? (send restored occurrence-at 35) (send original occurrence-at 35))

    (for ([decl (in-list (list original restored))])
      (send decl expand 0 5)
      (send decl contract 2 4))
    (check-equal? (send restored definition-at 30) (send original definition-at 30))
    (check-equal? (send restored uses-at 30) (send original uses-at 30)))

  (test-case "interval snapshots preserve shared identities across a deleted gap"
    (define shared (list "shared"))
    (define intervals (make-interval-map))
    (interval-map-set! intervals 0 2 shared)
    (interval-map-set! intervals 4 6 shared)
    ;; Reconstructing lists deliberately prevents accidental reuse by the decoder.
    (define restored
      (snapshot->interval-map (interval-map->snapshot intervals) (lambda (v) (map values v))))
    (check-eq? (interval-map-ref restored 0) (interval-map-ref restored 4))
    (interval-map-contract! intervals 2 4)
    (interval-map-contract! restored 2 4)
    (check-equal? (interval-map->list restored)
                  (interval-map->list intervals)))

  (test-case "real place preserves shared and equal-but-distinct interval values"
    (define shared (list (string-copy "same")))
    (define distinct (list (string-copy "same")))
    (check-equal? shared distinct)
    (check-not-eq? shared distinct)
    (define intervals (make-interval-map))
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
        (for ([im (in-list (list intervals restored))])
          (interval-map-contract! im 6 8)
          (interval-map-contract! im 2 4))
        (check-equal? (length (interval-map->list restored)) 2)
        (check-equal? (interval-map->list restored)
                      (interval-map->list intervals)))
      (lambda () (place-kill echo)))))
