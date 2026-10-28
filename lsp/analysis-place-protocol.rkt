#lang racket/base

;; Parent/worker wire for analysis places. Prefab structs keep their types
;; across place channels. struct/contract does not support #:prefab; receivers
;; validate fields and assignment identity at the boundary. Contracted prefab
;; accessors would reject malformed fields before parent recovery, so exports
;; stay uncontracted.
;;
;; kind: `'check-syntax` or `'resyntax`.
;; id: Unique assignment identity, not a slot index. The worker ignores a
;;   Place-Cancel for another job; the pool treats a mismatched Place-Terminal
;;   as a protocol failure and retires the worker.
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
(struct Place-Start (id kind uri text directory) #:prefab)

(struct Place-Cancel (id) #:prefab)

(struct Place-Ready (kind) #:prefab)

(struct Place-Terminal (id outcome) #:prefab)

(struct Place-Init-Failure (detail) #:prefab)

(struct Place-Failure (detail) #:prefab)

(struct Place-Cancelled () #:prefab)

;; `trace` is the serialized document trace.
(struct Place-Check-Syntax-Result (success? trace) #:prefab)

;; `findings` is a JSON-convertible list.
(struct Place-Resyntax-Result (findings) #:prefab)
