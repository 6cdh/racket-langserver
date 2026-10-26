#lang racket/base

;; A real place with a test-controlled protocol. Assignment, preparation, and
;; cancellation receipts are barriers; tests decide when cleanup is acknowledged.
(require "../../lsp/analysis-place-protocol.rkt"
         racket/contract racket/match racket/place "../../lsp/analysis-place.rkt")
(provide run-peer run-unready-peer
         (struct-out Peer-Preparing) (struct-out Peer-Reply))

;; Control-channel receipts, not worker wire messages.
(struct Peer-Preparing (kind) #:prefab)

(struct Peer-Reply (outcome) #:prefab)

(struct/contract Peer-Wire
  ([message any/c])
  #:transparent)

(struct/contract Peer-Command
  ([message (or/c symbol? Peer-Reply?)])
  #:transparent)

(define (run-peer channel)
  (define control (place-channel-get channel))
  (define-values (local remote) (place-channel))
  (thread (lambda () (run remote)))
  (define pending #f)
  (define manual-prepare? #f)
  (let loop ()
    (match (sync (handle-evt channel Peer-Wire)
                 (handle-evt control Peer-Command))
      [(Peer-Wire (Place-Prepare kind))
       (place-channel-put local (Place-Prepare kind))
       (define ready (place-channel-get local))
       (if manual-prepare?
           (begin (set! pending ready) (place-channel-put control (Peer-Preparing kind)))
           (place-channel-put channel ready))]
      [(Peer-Wire (and request (struct* Place-Start ())))
       (set! pending request)
       (place-channel-put control request)]
      [(Peer-Wire (and request (Place-Cancel _)))
       (place-channel-put control request)]
      [(Peer-Command 'manual-prepare) (set! manual-prepare? #t)
       (place-channel-put control 'configured)]
      [(Peer-Command 'ready) (place-channel-put channel pending)]
      [(Peer-Command 'cancelled)
       (match-define (struct* Place-Start ([id id])) pending)
       (place-channel-put channel (Place-Terminal id (Place-Cancelled)))]
      [(Peer-Command 'finish)
       (match-define (struct* Place-Start ([id id] [kind kind])) pending)
       (place-channel-put channel
                          (Place-Terminal id
                                          (if (eq? kind 'resyntax)
                                              (Place-Resyntax-Result '())
                                              (Place-Check-Syntax-Result #t #hasheq()))))]
      [(Peer-Command (Peer-Reply payload))
       (match-define (struct* Place-Start ([id id])) pending)
       (place-channel-put channel (Place-Terminal id payload))]
      [(Peer-Command 'wrong-id)
       (match-define (struct* Place-Start ([id id])) pending)
       (place-channel-put channel (Place-Terminal (add1 id) (Place-Cancelled)))]
      [(Peer-Command 'bad-id)
       (place-channel-put channel (Place-Terminal 'invalid-id (Place-Cancelled)))]
      [(Peer-Command 'wrong-kind)
       (match-define (struct* Place-Start ([id id])) pending)
       (place-channel-put channel (Place-Terminal id (Place-Resyntax-Result '())))]
      [(Peer-Command 'failure)
       (match-define (struct* Place-Start ([id id])) pending)
       (place-channel-put channel (Place-Terminal id (Place-Failure "job failure")))]
      [(Peer-Command 'run)
       (place-channel-put local pending)
       (place-channel-put channel (place-channel-get local))
       (place-channel-put control 'cleaned)]
      [(Peer-Command 'die) (exit 1)]
      [(Peer-Command 'hang) (sync never-evt)])
    (loop)))

;; This peer exposes initialization receipt without loading an analysis stack.
;; Withholding readiness makes the parent's startup watchdog observable.
(define (run-unready-peer channel)
  (define control (place-channel-get channel))
  (let loop ()
    (match (place-channel-get channel)
      [(Place-Prepare kind) (place-channel-put control (Peer-Preparing kind))])
    (loop)))
