#lang racket/base

;; A real place with a test-controlled protocol. Assignment, preparation, and
;; cancellation receipts are barriers; tests decide when cleanup is acknowledged.
(require racket/match racket/place "../../lsp/analysis-place.rkt")
(provide run-peer run-unready-peer)

(define (run-peer channel)
  (define control (place-channel-get channel))
  (define-values (local remote) (place-channel))
  (thread (lambda () (run remote)))
  (define pending #f)
  (define manual-prepare? #f)
  (let loop ()
    (match (sync (handle-evt channel (lambda (v) (list 'wire v)))
                 (handle-evt control (lambda (v) (list 'command v))))
      [(list 'wire (list 'prepare kind))
       (place-channel-put local (list 'prepare kind))
       (define ready (place-channel-get local))
       (if manual-prepare?
           (begin (set! pending ready) (place-channel-put control (list 'preparing kind)))
           (place-channel-put channel ready))]
      [(list 'wire (and request (list 'start _ _ _ _ _)))
       (set! pending request)
       (place-channel-put control request)]
      [(list 'wire (and request (list 'cancel _)))
       (place-channel-put control request)]
      [(list 'command 'manual-prepare) (set! manual-prepare? #t)
       (place-channel-put control 'configured)]
      [(list 'command 'ready) (place-channel-put channel pending)]
      [(list 'command 'cancelled)
       (match-define (list 'start id _ _ _ _) pending)
       (place-channel-put channel (list 'terminal id '(cancelled)))]
      [(list 'command 'finish)
       (match-define (list 'start id kind _ _ _) pending)
       (place-channel-put channel
                          (list 'terminal id
                                (if (eq? kind 'resyntax)
                                    '(result ()) '(result #t #hasheq()))))]
      [(list 'command (list 'reply payload))
       (match-define (list 'start id _ _ _ _) pending)
       (place-channel-put channel (list 'terminal id payload))]
      [(list 'command 'wrong-id)
       (match-define (list 'start id _ _ _ _) pending)
       (place-channel-put channel (list 'terminal (add1 id) '(cancelled)))]
      [(list 'command 'bad-id)
       (place-channel-put channel '(terminal invalid-id (cancelled)))]
      [(list 'command 'wrong-kind)
       (match-define (list 'start id _ _ _ _) pending)
       (place-channel-put channel (list 'terminal id '(result ())))]
      [(list 'command 'failure)
       (match-define (list 'start id _ _ _ _) pending)
       (place-channel-put channel (list 'terminal id '(failure "job failure")))]
      [(list 'command 'run)
       (place-channel-put local pending)
       (place-channel-put channel (place-channel-get local))
       (place-channel-put control 'cleaned)]
      [(list 'command 'die) (exit 1)]
      [(list 'command 'hang) (sync never-evt)])
    (loop)))

;; This peer exposes initialization receipt without loading an analysis stack.
;; Withholding readiness makes the parent's startup watchdog observable.
(define (run-unready-peer channel)
  (define control (place-channel-get channel))
  (let loop ()
    (match (place-channel-get channel)
      [(list 'prepare kind) (place-channel-put control (list 'preparing kind))])
    (loop)))
