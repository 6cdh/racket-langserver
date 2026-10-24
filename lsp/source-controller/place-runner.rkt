#lang racket/base

;; Session-owned reusable places, with no pending queue. The scheduler
;; reserves a slot before launch. A bridge supervises startup through restoration,
;; and retains the reservation until cancelled places have terminated.
(require racket/async-channel
         racket/match
         racket/place
         racket/port
         racket/runtime-path
         "../../common/time.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt")

(provide Place-Runner? Place-Task? make-place-runner place-runner-available?
         start-place-task! cancel-place-task! stop-place-runner!)

(define-runtime-path worker-path "../analysis-place.rkt")

(define (spawn-worker)
  ;; The file-stream stdout sink and closed stdin pipe need no pump threads.
  ;; Supplied stdout/stderr ports make both returned port values #f. The parent
  ;; stderr port may still need a pump, which place-wait joins at retirement.
  (define sink
    (open-output-file (if (eq? (system-type) 'windows) "NUL" "/dev/null") #:exists 'append))
  (define-values (worker input _stdout _stderr)
    (dynamic-place* worker-path 'run #:out sink #:err (current-error-port)))
  (close-output-port input)
  worker)

;; Test factories still create real places. They can establish startup barriers
;; or substitute a wire peer without transporting an analysis closure.
(define current-place-factory (make-parameter spawn-worker))
(define current-analysis-restorer (make-parameter restore-analysis))
(module+ test-support (provide current-place-factory current-analysis-restorer))

;; One physical place. id is its creating job's ID and stays fixed while later
;; jobs advance Place-Runner's counter. A replacement records its own creating
;; job's ID, so a reply from a retired generation cannot match the replacement.
;;
;; custodian: Owns the place and stdout sink, including interrupted startup
;;   before the place handle is stored.
;; worker: #f until startup returns its place handle.
(struct Generation (id custodian [worker #:mutable]))
(struct Place-Task (cancel))
;; Each slot stays reserved through restoration or observed retirement, so both
;; physical places and parent-side work remain bounded by the configured capacity.
(struct Worker-Slot ([generation #:mutable] [task #:mutable]))
;; owner-custodian owns both places and their cleanup bridges.
;; serial is the increasing job counter; new generations use their first job ID.
(struct Place-Runner (lock owner-custodian slots [serial #:mutable] [stopped? #:mutable]))

;; (->* () (#:slots exact-positive-integer?) Place-Runner?)
(define (make-place-runner #:slots [slots 1])
  (unless (exact-positive-integer? slots)
    (raise-argument-error 'make-place-runner "exact-positive-integer?" slots))
  (Place-Runner (make-semaphore 1) (current-custodian)
                (for/list ([_ (in-range slots)]) (Worker-Slot #f #f)) 0 #f))

(define (locked runner proc)
  (call-with-semaphore (Place-Runner-lock runner) proc))

;; Cleanup must survive disposal of the consumer's custodian. Places and
;; bridges share the runner's owning lifetime; each job has a narrower boundary.
(define (runner-thread runner thunk)
  (parameterize ([current-custodian (Place-Runner-owner-custodian runner)]) (thread thunk)))

;; (-> Place-Runner? boolean?)
(define (place-runner-available? runner)
  (locked runner (lambda () (and (not (Place-Runner-stopped? runner))
                                 (for/or ([slot (in-list (Place-Runner-slots runner))])
                                   (not (Worker-Slot-task slot)))))))

;; (-> Place-Task? void?)
(define (cancel-place-task! task)
  ;; This handle can only signal its own bridge, even after the place is reused.
  (semaphore-post (Place-Task-cancel task)))

(define (retire-generation! runner generation)
  ;; Startup can be interrupted between place creation and recording its handle.
  ;; Stop the startup thread first, then collect its custodian's places too.
  (define workers
    (filter place? (custodian-managed-list (Generation-custodian generation)
                                           (Place-Runner-owner-custodian runner))))
  (for ([worker (in-list workers)]) (place-kill worker))
  ;; Let any remaining port pumps finish before shutting down their custodian.
  (for ([worker (in-list workers)]) (place-wait worker))
  (custodian-shutdown-all (Generation-custodian generation)))

;; (-> Place-Runner? void?) — initiates cleanup without waiting for termination.
(define (stop-place-runner! runner)
  (define idle
    (locked runner
            (lambda ()
              (set-Place-Runner-stopped?! runner #t)
              (for/fold ([idle '()]) ([slot (in-list (Place-Runner-slots runner))])
                (define task (Worker-Slot-task slot))
                (define generation (Worker-Slot-generation slot))
                (cond
                  [task
                   (cancel-place-task! task)
                   idle]
                  [generation
                   (set-Worker-Slot-generation! slot #f)
                   (cons generation idle)]
                  [else idle])))))
  (for ([generation (in-list idle)])
    (runner-thread runner (lambda () (retire-generation! runner generation))))
  (void))

(define (failure reason)
  (Analysis-Outcome 'failed
                    (if (exn? reason) (exn-message reason)
                        "analysis execution raised a non-exception value")))

(define (receive-reply worker)
  (sync worker
        (handle-evt (place-dead-evt worker)
                    (lambda (_)
                      ;; A final reply and death may both be ready. Replies are
                      ;; lists, so a #f poll means no queued reply.
                      (or (sync/timeout 0 worker)
                          (error 'place-runner "worker exited"))))))

(define (execute-job generation job snapshot place-reusable?)
  (unless (Generation-worker generation)
    (define worker
      (parameterize ([current-custodian (Generation-custodian generation)])
        ((current-place-factory))))
    (set-Generation-worker! generation worker))
  (define worker (Generation-worker generation))
  (place-channel-put worker
                     (list (Generation-id generation) job (Source-Snapshot-uri snapshot)
                           (Source-Snapshot-text snapshot) (current-directory)))
  (match (receive-reply worker)
    [(list (== (Generation-id generation)) (== job) (list 'result (? boolean? succeeded?) data))
     ;; Job resources are gone. Restoration failure or cancellation still
     ;; reuses this place.
     (set-box! place-reusable? #t)
     ((current-analysis-restorer) snapshot succeeded? data)]
    [(list (== (Generation-id generation)) (== job) (list 'failure (? string? detail)))
     ;; Cleanup ran, but the exception may have touched place-wide state outside
     ;; the job custodian. Fail the attempt and retire this place.
     (Analysis-Outcome 'failed detail)]
    [_ (error 'place-runner "invalid worker reply")]))

;; (-> Place-Runner? Source-Snapshot? async-channel? real? procedure? Place-Task?)
;; seconds is nonnegative.
;; Requires an available runner. done runs on the bridge thread; its returned
;; envelope is sent to events after cleanup. The owner alone accepts the result.
(define (start-place-task! runner snapshot events seconds done)
  (define deadline (+ (current-deadline-milliseconds) (* 1000 seconds)))
  (define task (Place-Task (make-semaphore 0)))
  (define-values (slot generation retired job)
    (locked runner
            (lambda ()
              (define slot
                (for/first ([slot (in-list (Place-Runner-slots runner))]
                            #:unless (Worker-Slot-task slot))
                  slot))
              (when (or (Place-Runner-stopped? runner) (not slot))
                (error 'start-place-task! "runner is unavailable"))
              (set-Place-Runner-serial! runner (add1 (Place-Runner-serial runner)))
              (define job (Place-Runner-serial runner))
              (define previous (Worker-Slot-generation slot))
              (define dead?
                (and previous (Generation-worker previous)
                     (sync/timeout 0 (place-dead-evt (Generation-worker previous)))))
              (when (or (not previous) dead?)
                (set-Worker-Slot-generation!
                  slot (Generation job
                                   (make-custodian (Place-Runner-owner-custodian runner)) #f)))
              (set-Worker-Slot-task! slot task)
              (values slot (Worker-Slot-generation slot) (and dead? previous) job))))
  (runner-thread
    runner
    (lambda ()
      ;; An idle worker may die after its last reply. Observe its retirement
      ;; before creating a replacement for this assignment.
      (when retired (retire-generation! runner retired))
      (define custodian (make-custodian))
      (define results (make-async-channel))
      ;; The job thread sets this before restoration after accepting a normal
      ;; result. Read it only after stopping that thread below. The side channel
      ;; preserves reuse if cancellation or timeout interrupts restoration.
      (define place-reusable? (box #f))
      (define work
        (parameterize ([current-custodian custodian]
                       [current-output-port (open-output-nowhere)])
          (thread
            (lambda ()
              (async-channel-put results
                                 (with-handlers ([(lambda (_) #t) failure])
                                   (execute-job generation job snapshot place-reusable?)))))))
      (define result
        (sync results
              (handle-evt (semaphore-peek-evt (Place-Task-cancel task))
                          (lambda (_) (Analysis-Outcome 'stopped 'cancelled)))
              (handle-evt (deadline-evt deadline)
                          (lambda (_) (Analysis-Outcome 'failed 'analysis-timeout)))
              (handle-evt (thread-dead-evt work)
                          (lambda (_)
                            (or (sync/timeout 0 results)
                                (Analysis-Outcome 'failed 'worker-exited))))))
      (custodian-shutdown-all custodian)
      (define retire?
        (locked runner
                (lambda ()
                  (define retire? (or (not (unbox place-reusable?)) (Place-Runner-stopped? runner)))
                  (unless retire? (set-Worker-Slot-task! slot #f))
                  retire?)))
      ;; Keep the runner lock released across place-wait so shutdown and
      ;; dispatch never wait on termination while acquiring that lock.
      (when retire?
        (retire-generation! runner generation)
        (locked runner
                (lambda ()
                  (set-Worker-Slot-generation! slot #f)
                  (set-Worker-Slot-task! slot #f))))
      ;; Includes cancellation: the stale completion wakes dispatch only after
      ;; physical capacity is free. Acceptance still checks attempt identity.
      (async-channel-put events (done result))))
  task)
