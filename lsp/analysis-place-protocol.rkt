#lang racket/base

;; Parent/worker wire for analysis places. Prefab structs keep their types
;; across place channels. struct/contract does not support #:prefab.
;;
;; kind: `'check-syntax` or `'resyntax`.
;; id: Unique assignment identity, not a slot index. The worker ignores a
;;   Place-Cancel for another job.
;;   A worker sends at most one terminal per job, after cleanup; the pool keeps its
;;   slot reserved until that terminal or worker retirement. A late cancel
;;   after a terminal therefore cannot produce another reply for that job.
;;   Initialization has no job id, so a cancel cannot abort it.
;; Place-Prepare / Place-Ready: load `kind` under a persistent worker lifetime.
;; Place-Start / Place-Terminal: one job. The terminal is sent only after job
;;   cleanup. `outcome` is a result, Place-Failure, or Place-Cancelled.
;; Place-Init-Failure: adapter load failed; the worker is not reusable.
;; Place-Failure / Place-Cancelled: job ended after cleanup; the worker stays
;;   reusable.
(provide (struct-out Place-Prepare)
         (struct-out Place-Start)
         (struct-out Place-Cancel)
         (struct-out Place-Ready)
         (struct-out Place-Terminal)
         (struct-out Place-Init-Failure)
         (struct-out Place-Failure)
         (struct-out Place-Cancelled)
         (struct-out Place-Check-Syntax-Result)
         (struct-out Place-Resyntax-Result))

(struct Place-Prepare (kind) #:prefab)

;; `directory` is the job working directory.
(struct Place-Start (id uri text directory) #:prefab)

(struct Place-Cancel (id) #:prefab)

(struct Place-Ready () #:prefab)

(struct Place-Terminal (outcome) #:prefab)

(struct Place-Init-Failure (detail) #:prefab)

(struct Place-Failure (detail) #:prefab)

(struct Place-Cancelled () #:prefab)

;; `trace` is the serialized document trace.
(struct Place-Check-Syntax-Result (success? trace) #:prefab)

;; `findings` is a JSON-convertible list.
(struct Place-Resyntax-Result (findings) #:prefab)
