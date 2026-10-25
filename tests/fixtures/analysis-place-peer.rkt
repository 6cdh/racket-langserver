#lang racket/base

;; A controllable real-place peer around the production worker loop. Messages
;; acknowledge receipt of each assignment before the test releases execution.
(require racket/match racket/place "../../lsp/analysis-place.rkt")
(provide run-peer)

(define (run-peer channel)
  (define control (place-channel-get channel))
  (define-values (to-worker from-worker) (place-channel))
  (define parent (current-custodian))
  (define service-custodian (make-custodian))
  (parameterize ([current-custodian service-custodian])
    (thread (lambda () (run from-worker))))
  (let loop ()
    (define request (place-channel-get channel))
    (place-channel-put control request)
    (define command (place-channel-get control))
    (case command
      [(die) (exit 1)]
      [(hang) (sync never-evt)]
      [else
       (place-channel-put to-worker request)
       (define reply (place-channel-get to-worker))
       ;; Job custodians have been shut down before the reply reaches here.
       (place-channel-put control
                          (length (custodian-managed-list service-custodian parent)))
       (match command
         ['wrong-id
          (match-define (list job payload) reply)
          (place-channel-put channel (list (add1 job) payload))]
         ['bad-snapshot
          (match-define (list job _) reply)
          (place-channel-put channel (list job (list 'result #t #hasheq())))]
         [_ (place-channel-put channel reply)])
       (when (eq? command 'reply-and-die) (exit 0))
       (loop)])))
