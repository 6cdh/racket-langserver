#lang racket/base

;; One worker pool. The scheduler constructs one for Check Syntax and one for
;; Resyntax; a pool can still run both kinds, which tests do. The manager owns
;; the FIFO, deadlines, and physical reservations. Logical settlement never
;; releases a slot; cleanup or observed retirement does. Task deadlines,
;; normally 90 seconds from submission, include queueing and parent restoration.
;; Expiry cancels the task while preserving a responsive worker. A startup
;; watchdog distinct from the task deadline, also 90 seconds by default, covers
;; native creation plus first stack initialization. Its expiry retires that
;; worker. Cross-file rules are in ../analysis-lifecycle.md.
(require "../analysis-place-protocol.rkt"
         data/queue
         racket/async-channel
         racket/contract
         racket/list
         racket/match
         racket/place
         racket/port
         racket/runtime-path
         "../../common/time.rkt"
         "../../common/json-util.rkt"
         "../../common/interfaces.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt")

(provide Place-Runner? Place-Task? make-place-runner
         submit-place-task! start-place-task! cancel-place-task! stop-place-runner!
         place-runner-shutdown-evt)

(define-runtime-path worker-path "../analysis-place.rkt")

(define (spawn-worker guard)
  ;; The worker custodian owns this port through warm reuse. Retirement closes
  ;; it, including when startup fails before a place handle is recorded.
  (define sink
    (open-output-file (if (eq? (system-type) 'windows) "NUL" "/dev/null") #:exists 'append))
  (define-values (worker input _stdout _stderr)
    (guard (lambda () (dynamic-place* worker-path 'run #:out sink #:err (current-error-port)))))
  (close-output-port input)
  worker)

(module+ test-support
  (provide make-place-runner/configured spawn-worker))

;; Admission boundary for the manager thread.
;;
;; commands: Submit, cancel, and stop messages for the manager.
;; gate: Serializes those commands with stop so none arrive after the manager
;;   may exit.
;; shutdown: Posted after the manager returns.
;; stopped?: Set at stop so later submits are answered here without a manager.
;; manager: Manager thread; shutdown-evt also waits for its death.
(struct/contract Place-Runner
  ([commands async-channel?]
   [gate semaphore?]
   [shutdown semaphore?]
   [(stopped? #:mutable) boolean?]
   [(manager #:mutable) (or/c #f thread?)]))

;; The handle denotes exactly one submission, including after settlement.
;;
;; runner: Admission and cancellation boundary; no physical worker identity
;;   escapes.
;; kind: `'check-syntax` or `'resyntax`, used by the analysis scheduler,
;;   manager, and worker.
;; snapshot: Immutable source input retained in the parent for restoration.
;; deadline: Absolute monotonic milliseconds, captured once at submission.
;; directory: Submission directory, used for worker execution and parent work.
;; events: Caller channel receiving one logical terminal notification, possibly
;;   before physical cleanup finishes on cancellation or expiry.
;; done: Caller envelope constructor wrapping the terminal result; never
;;   installs source results or publishes.
;; work: Optional local test procedure; `#f` selects the fixed place adapter.
;; active?: Cleared at logical settlement; capacity can remain reserved afterward.
(struct/contract Place-Task
  ([runner Place-Runner?]
   [kind (or/c 'check-syntax 'resyntax)]
   [snapshot Source-Snapshot?]
   [deadline real?]
   [directory path?]
   [events async-channel?]
   [done (-> any/c any/c)]
   [work (or/c #f (-> any/c))]
   [(active? #:mutable) boolean?]))

;; Physical place and its initialized stacks. Independent of the current task.
;; A spawning worker already owns a custodian, even before `place` is recorded.
;;
;; custodian: Owns the place from spawn through retirement.
;; place: `#f` until native creation returns.
;; ready: Kinds whose adapters have loaded. A production pool sends one kind;
;;   tests may initialize both on the same place. Not a liveness flag; death
;;   uses `dead?`.
;; dead?: Recorded until retirement. A dead worker never becomes idle.
;; events: Wire and death events, built once when `place` is recorded.
(struct/contract Worker
  ([custodian custodian?]
   [(place #:mutable) (or/c #f place?)]
   [(ready #:mutable) (listof (or/c 'check-syntax 'resyntax))]
   [(dead? #:mutable) boolean?]
   [(events #:mutable) (listof evt?)]))

;; Completion callbacks run only on the manager. Helpers deliver a callback
;; carrying their result; they never mutate pool state or release capacity.
(struct/contract Async-Task
  ([thread thread?]
   [event evt?]))

;; A slot reserves capacity through startup, execution, parent work, and
;; retirement.
;;
;; worker: Physical resource, independent of the task and parent-work custody.
;; task: Current assignment; may already be settled while this slot stays reserved.
;; phase: `empty` and `idle` are assignable; every other phase reserves this
;;   capacity.
;; until: Absolute startup, initialization, or cancellation watchdog
;;   milliseconds; `#f` outside those phases, independent of the task deadline.
;; id: Unique assignment ID retained through cancellation and cleanup.
;; async: Current helper and its manager-thread completion event, or `#f`.
;; work-custodian: Parent restoration or injected work, retained through cleanup.
(struct/contract Slot
  ([(worker #:mutable) (or/c #f Worker?)]
   [(task #:mutable) (or/c #f Place-Task?)]
   [(phase #:mutable) (or/c 'empty 'idle 'spawning 'preparing 'executing 'cancelling
                            'restoring 'injected 'cleaning 'retiring)]
   [(until #:mutable) (or/c #f real?)]
   [(id #:mutable) (or/c #f exact-positive-integer?)]
   [(async #:mutable) (or/c #f Async-Task?)]
   [(work-custodian #:mutable) (or/c #f custodian?)]))

;; Manager-only mutable pool. Helpers report callbacks; they never release slots.
;;
;; tasks: Logically active submissions. A slot may still hold a settled task.
;; owner: Session custodian; retirement and cleanup helpers stay under it.
;; guard / factory: Physical startup captured at construction.
;; cleanup-ms / startup-ms: Watchdog durations captured at construction.
;; cleaner: Parent-work teardown, also captured at construction.
;; restorer: Check Syntax result restoration, fixed for this pool.
;; serial: Assignment IDs, unique across replacements; not a slot index.
(struct/contract Pool
  ([slots (listof Slot?)]
   [queue queue?]
   [tasks (hash/c Place-Task? boolean? #:immutable #f)]
   [owner custodian?]
   [guard (-> (-> any) any)]
   [factory (-> procedure? place?)]
   [cleanup-ms (and/c real? (>=/c 0))]
   [startup-ms (and/c real? (>=/c 0))]
   [cleaner (-> custodian? any)]
   [restorer (-> Source-Snapshot? boolean? any/c any/c)]
   [command-event evt?]
   [(stopped? #:mutable) boolean?]
   [(serial #:mutable) exact-nonnegative-integer?]))

(struct/contract Pool-Submit
  ([task Place-Task?])
  #:transparent)

(struct/contract Pool-Cancel
  ([task Place-Task?])
  #:transparent)

(struct/contract Pool-Stop ()
  #:transparent)

(struct/contract Pool-Worker-Message
  ([slot Slot?] [message any/c])
  #:transparent)

(struct/contract Pool-Worker-Dead
  ([slot Slot?])
  #:transparent)

(struct/contract Pool-Async-Finished
  ([finish (-> any)])
  #:transparent)

(struct/contract Pool-Timer ()
  #:transparent)

(struct/contract Pool-Command
  ([message (or/c Pool-Submit? Pool-Cancel? Pool-Stop?)])
  #:transparent)

(define (make-place-runner #:slots [count 4]
                           #:place-start-guard [guard (lambda (thunk) (thunk))])
  (make-place-runner/configured #:slots count #:place-start-guard guard))

(define (make-place-runner/configured #:slots [count 4]
                                      #:place-start-guard [guard (lambda (thunk) (thunk))]
                                      #:factory [factory spawn-worker]
                                      #:restorer [restorer restore-analysis]
                                      #:cleanup-seconds [cleanup-seconds 1]
                                      #:startup-seconds [startup-seconds 90]
                                      #:cleaner [cleaner custodian-shutdown-all])
  (unless (exact-positive-integer? count)
    (raise-argument-error 'make-place-runner "exact-positive-integer?" count))
  (define runner (Place-Runner (make-async-channel) (make-semaphore 1) (make-semaphore 0) #f #f))
  (define pool
    (Pool (for/list ([_ (in-range count)]) (Slot #f #f 'empty #f #f #f #f))
          (make-queue) (make-hasheq) (current-custodian) guard factory
          (* 1000 cleanup-seconds) (* 1000 startup-seconds) cleaner restorer
          (handle-evt (Place-Runner-commands runner) Pool-Command)
          #f 0))
  (set-Place-Runner-manager!
    runner
    (thread (lambda ()
              (run-pool-manager pool)
              (semaphore-post (Place-Runner-shutdown runner)))))
  runner)

(define (submit-place-task! runner kind snapshot events deadline done #:work [work #f])
  (define task
    (Place-Task runner kind snapshot deadline (current-directory)
                events done work #t))
  (call-with-semaphore
    (Place-Runner-gate runner)
    (lambda ()
      (if (Place-Runner-stopped? runner)
          (deliver-task-outcome! task (Analysis-Outcome 'stopped 'shutdown))
          (async-channel-put (Place-Runner-commands runner) (Pool-Submit task)))))
  task)

;; Direct Check Syntax submission with a relative timeout. The scheduler
;; captures an absolute deadline and chooses the kind's pool instead.
(define (start-place-task! runner snapshot events seconds done)
  (submit-place-task! runner 'check-syntax snapshot events
                      (+ (current-deadline-milliseconds) (* 1000 seconds)) done))

(define (cancel-place-task! task)
  (define runner (Place-Task-runner task))
  (call-with-semaphore
    (Place-Runner-gate runner)
    (lambda ()
      (unless (Place-Runner-stopped? runner)
        (async-channel-put (Place-Runner-commands runner) (Pool-Cancel task))))))

(define (stop-place-runner! runner)
  (call-with-semaphore
    (Place-Runner-gate runner)
    (lambda ()
      (unless (Place-Runner-stopped? runner)
        (set-Place-Runner-stopped?! runner #t)
        (async-channel-put (Place-Runner-commands runner) (Pool-Stop))))))

(define (place-runner-shutdown-evt runner)
  ;; Abnormal manager death must not report successful physical cleanup.
  (replace-evt (semaphore-peek-evt (Place-Runner-shutdown runner))
               (lambda (_) (thread-dead-evt (Place-Runner-manager runner)))))

;; Only malformed worker messages cross this recovery boundary. Manager bugs
;; must escape instead of being reported as one worker's protocol failure.
(struct/contract exn:fail:place-protocol exn:fail ())

(define (raise-protocol-error message)
  (raise (exn:fail:place-protocol (string-append "place-runner: " message)
                                  (current-continuation-marks))))

(define (failure-outcome reason)
  (Analysis-Outcome 'failed (if (exn? reason) (exn-message reason) reason)))

(define (deliver-task-outcome! task outcome)
  (set-Place-Task-active?! task #f)
  (async-channel-put (Place-Task-events task) ((Place-Task-done task) outcome)))

(define (settle-pool-task! pool task outcome)
  (when (Place-Task-active? task)
    (hash-remove! (Pool-tasks pool) task)
    (deliver-task-outcome! task outcome)))

(define (start-slot-helper! pool slot phase thunk completed failed! [custodian (Pool-owner pool)])
  (define results (make-async-channel))
  (define helper
    (parameterize ([current-custodian custodian])
      (thread
        (lambda ()
          (define finish
            (with-handlers ([(lambda (_) #t)
                             (lambda (reason)
                               (define outcome (failure-outcome reason))
                               (lambda () (failed! outcome)))])
              (define value (thunk))
              (lambda () (completed value))))
          (async-channel-put results finish)))))
  (define event
    (choice-evt
      (handle-evt results Pool-Async-Finished)
      (handle-evt (thread-dead-evt helper)
                  (lambda (_)
                    (Pool-Async-Finished
                      (or (async-channel-try-get results)
                          (lambda () (failed! (failure-outcome 'worker-exited)))))))))
  (set-Slot-phase! slot phase)
  (set-Slot-async! slot (Async-Task helper event)))

(define (stop-slot-helper! slot)
  (when (Slot-async slot) (kill-thread (Async-Task-thread (Slot-async slot))))
  (set-Slot-async! slot #f))

;; Clearing a reservation either exposes a live warm worker or starts retirement.
;; A dead worker never passes through idle, even after a valid result is restored.
(define (clear-slot-assignment! pool slot)
  (set-Slot-id! slot #f)
  (set-Slot-task! slot #f)
  (set-Slot-async! slot #f)
  (set-Slot-work-custodian! slot #f)
  (set-Slot-until! slot #f)
  (cond
    [(not (Slot-worker slot)) (set-Slot-phase! slot 'empty)]
    [(Worker-dead? (Slot-worker slot)) (retire-slot-worker! pool slot)]
    [else (set-Slot-phase! slot 'idle)]))

(define (fail-slot! pool slot outcome)
  (when (Slot-task slot) (settle-pool-task! pool (Slot-task slot) outcome))
  (retire-slot-worker! pool slot))

(define (retire-slot-worker! pool slot)
  (unless (eq? (Slot-phase slot) 'retiring)
    (stop-slot-helper! slot)
    (define worker (Slot-worker slot))
    (define work-custody (Slot-work-custodian slot))
    (set-Slot-until! slot #f)
    (start-slot-helper! pool slot 'retiring
                        (lambda ()
                          (when work-custody (custodian-shutdown-all work-custody))
                          (when worker
                            (define custody (Worker-custodian worker))
                            ;; Includes a place created before the startup helper recorded it.
                            (define places
                              (filter place? (custodian-managed-list custody (Pool-owner pool))))
                            (for ([place (in-list places)]) (place-kill place))
                            (for ([place (in-list places)]) (place-wait place))
                            (custodian-shutdown-all custody)))
                        (lambda (_)
                          (set-Slot-worker! slot #f)
                          (clear-slot-assignment! pool slot))
                        (lambda (outcome)
                          (error 'place-runner "worker retirement failed: ~v" outcome)))))

(define (cleanup-parent-work! pool slot [outcome #f])
  (stop-slot-helper! slot)
  (define custody (Slot-work-custodian slot))
  (start-slot-helper! pool slot 'cleaning
                      (lambda () ((Pool-cleaner pool) custody))
                      (lambda (_)
                        ;; Timers can settle during async cleanup. This boundary also
                        ;; rejects success if cleanup completed after the task deadline.
                        (define task (Slot-task slot))
                        (when (and task (Place-Task-active? task))
                          (settle-pool-task! pool task
                                             (if (>= (current-deadline-milliseconds)
                                                     (Place-Task-deadline task))
                                                 (failure-outcome 'analysis-timeout) outcome)))
                        (clear-slot-assignment! pool slot))
                      (lambda (outcome) (fail-slot! pool slot outcome))))

(define (cancel-pool-task! pool task outcome)
  (when (Place-Task-active? task)
    (settle-pool-task! pool task outcome)
    (queue-filter! (Pool-queue pool) (lambda (waiting) (not (eq? waiting task))))
    (define slot
      (for/first ([slot (in-list (Pool-slots pool))] #:when (eq? task (Slot-task slot))) slot))
    (when slot
      (case (Slot-phase slot)
        [(executing)
         (place-channel-put (Worker-place (Slot-worker slot)) (Place-Cancel (Slot-id slot)))
         (set-Slot-phase! slot 'cancelling)
         (set-Slot-until! slot (+ (current-deadline-milliseconds) (Pool-cleanup-ms pool)))]
        [(restoring injected) (cleanup-parent-work! pool slot)]
        ;; Startup and initialization survive cancellation with their original
        ;; watchdog. A cancelled task is never started on the prepared worker.
        [else (void)]))))

(define (start-parent-work! pool slot task thunk phase)
  (define custody (make-custodian (Pool-owner pool)))
  (set-Slot-work-custodian! slot custody)
  (start-slot-helper! pool slot phase
                      (lambda ()
                        (parameterize ([current-directory (Place-Task-directory task)]
                                       [current-output-port (open-output-nowhere)])
                          (thunk)))
                      (lambda (value) (cleanup-parent-work! pool slot value))
                      (lambda (outcome) (cleanup-parent-work! pool slot outcome))
                      custody))

(define (start-worker-job! slot task)
  (set-Slot-phase! slot 'executing)
  (set-Slot-until! slot #f)
  (define input (Place-Task-snapshot task))
  (place-channel-put (Worker-place (Slot-worker slot))
                     (Place-Start (Slot-id slot) (Place-Task-kind task)
                                  (Source-Snapshot-uri input) (Source-Snapshot-text input)
                                  (Place-Task-directory task))))

;; Dispatch arms the startup watchdog. A newly created place uses the remainder
;; of that same budget for its first library initialization.
(define (prepare-worker-for-task! slot task)
  (set-Slot-phase! slot 'preparing)
  (place-channel-put (Worker-place (Slot-worker slot)) (Place-Prepare (Place-Task-kind task))))

(define (register-spawned-worker! slot place)
  (define worker (Slot-worker slot))
  (set-Worker-place! worker place)
  (set-Worker-events!
    worker
    (list (handle-evt place (lambda (reply) (Pool-Worker-Message slot reply)))
          (handle-evt (place-dead-evt place) (lambda (_) (Pool-Worker-Dead slot)))))
  (set-Slot-async! slot #f)
  ;; Initialize even if the first assignment was cancelled.
  (prepare-worker-for-task! slot (Slot-task slot)))

(define (assign-task-to-slot! pool slot task)
  (set-Slot-task! slot task)
  ;; IDs belong to assignments, not slots. Obsolete replies cannot complete a
  ;; later task even when the same physical worker is reused.
  (set-Pool-serial! pool (add1 (Pool-serial pool)))
  (set-Slot-id! slot (Pool-serial pool))
  (cond
    [(Place-Task-work task) (start-parent-work! pool slot task (Place-Task-work task) 'injected)]
    [(and (Slot-worker slot) (memq (Place-Task-kind task) (Worker-ready (Slot-worker slot))))
     (start-worker-job! slot task)]
    [else
     (set-Slot-until! slot (+ (current-deadline-milliseconds) (Pool-startup-ms pool)))
     (cond
       [(Slot-worker slot) (prepare-worker-for-task! slot task)]
       [else
        (define custody (make-custodian (Pool-owner pool)))
        (set-Slot-worker! slot (Worker custody #f '() #f '()))
        (start-slot-helper! pool slot 'spawning
                            (lambda () ((Pool-factory pool) (Pool-guard pool)))
                            (lambda (place) (register-spawned-worker! slot place))
                            (lambda (outcome) (fail-slot! pool slot outcome))
                            custody)])]))

(define (assign-queued-tasks! pool)
  (define slot
    (for/first ([slot (in-list (Pool-slots pool))]
                #:when (memq (Slot-phase slot) '(empty idle)))
      slot))
  (when (and slot (not (queue-empty? (Pool-queue pool))))
    (assign-task-to-slot! pool slot (dequeue! (Pool-queue pool)))
    (assign-queued-tasks! pool)))

(define (process-worker-message! pool slot message)
  (define task (Slot-task slot))
  (match message
    [(Place-Ready kind)
     (unless (and (eq? (Slot-phase slot) 'preparing)
                  task (eq? kind (Place-Task-kind task)))
       (raise-protocol-error "unexpected readiness"))
     (define worker (Slot-worker slot))
     (set-Worker-ready! worker (cons kind (Worker-ready worker)))
     (if (Place-Task-active? task)
         (start-worker-job! slot task)
         (clear-slot-assignment! pool slot))]
    [(Place-Terminal (? exact-positive-integer? id) payload)
     (unless (and task (= id (Slot-id slot))
                  (memq (Slot-phase slot) '(executing cancelling)))
       (raise-protocol-error "unexpected terminal reply"))
     (set-Slot-until! slot #f)
     (match payload
       [(Place-Failure (? string? detail))
        (settle-pool-task! pool task (failure-outcome detail))
        (clear-slot-assignment! pool slot)]
       [(Place-Cancelled)
        (settle-pool-task! pool task (Analysis-Outcome 'stopped 'cancelled))
        (clear-slot-assignment! pool slot)]
       [(Place-Check-Syntax-Result (? boolean? success?) data)
        #:when (eq? (Place-Task-kind task) 'check-syntax)
        (if (Place-Task-active? task)
            (start-parent-work! pool slot task
                                (lambda () ((Pool-restorer pool)
                                            (Place-Task-snapshot task) success? data))
                                'restoring)
            (clear-slot-assignment! pool slot))]
       [(Place-Resyntax-Result (? list? data))
        #:when (eq? (Place-Task-kind task) 'resyntax)
        (if (Place-Task-active? task)
            (start-parent-work! pool slot task
                                (lambda () (map jsexpr->Resyntax-Result data)) 'restoring)
            (clear-slot-assignment! pool slot))]
       [_ (raise-protocol-error "result does not match task kind")])]
    [(Place-Init-Failure (? string? detail))
     (unless (and task (eq? (Slot-phase slot) 'preparing))
       (raise-protocol-error "unexpected initialization failure"))
     (fail-slot! pool slot (failure-outcome detail))]
    [_ (raise-protocol-error "invalid worker reply")]))

(define (handle-worker-message! pool slot reply)
  (with-handlers ([exn:fail:place-protocol?
                   (lambda (reason) (fail-slot! pool slot (failure-outcome reason)))])
    (define task (Slot-task slot))
    (when (and task (Place-Task-active? task)
               (>= (current-deadline-milliseconds) (Place-Task-deadline task)))
      ;; A terminal reply already proves worker cleanup. Settle expiry without
      ;; sending an obsolete cancel; `process-worker-message!` still validates
      ;; the reply.
      (settle-pool-task! pool task (failure-outcome 'analysis-timeout)))
    (process-worker-message! pool slot reply)))

(define (handle-worker-death! pool slot)
  (define worker (Slot-worker slot))
  (set-Worker-dead?! worker #t)
  ;; A queued terminal reply wins over death. A dead place is never reused.
  (define reply (sync/timeout 0 (Worker-place worker)))
  (when reply
    (with-handlers ([exn:fail:place-protocol? void]) (process-worker-message! pool slot reply)))
  ;; Transferred data and parent custody no longer depend on the place. Keep
  ;; the reservation through parent cleanup, then `clear-slot-assignment!`
  ;; retires its worker.
  (unless (memq (Slot-phase slot) '(restoring cleaning))
    (fail-slot! pool slot (failure-outcome 'worker-exited))))

(define (slot-events slot)
  (define worker (Slot-worker slot))
  (append
    ;; Death events remain ready forever. Skip observed death during parent
    ;; work, and all worker events during retirement, so this sync cannot spin.
    (if (and worker (not (Worker-dead? worker)) (not (eq? (Slot-phase slot) 'retiring)))
        (Worker-events worker) '())
    (if (Slot-async slot) (list (Async-Task-event (Slot-async slot))) '())))

(define (enforce-pool-deadlines! pool)
  (define now (current-deadline-milliseconds))
  (for ([task (in-list (hash-keys (Pool-tasks pool)))]
        #:when (>= now (Place-Task-deadline task)))
    (cancel-pool-task! pool task (failure-outcome 'analysis-timeout)))
  (for ([slot (in-list (Pool-slots pool))]
        #:when (and (Slot-until slot) (>= now (Slot-until slot))))
    (fail-slot! pool slot (failure-outcome 'worker-watchdog))))

(define (sync-pool-events pool)
  (define deadlines
    (append (map Place-Task-deadline (hash-keys (Pool-tasks pool)))
            (filter-map Slot-until (Pool-slots pool))))
  (define events
    (append (list (Pool-command-event pool))
            (if (pair? deadlines)
                (list (handle-evt (deadline-evt (apply min deadlines)) (lambda (_) (Pool-Timer))))
                '())
            (append-map slot-events (Pool-slots pool))))
  (apply sync events))

(define (handle-pool-command! pool command)
  (match command
    [(Pool-Submit task)
     (hash-set! (Pool-tasks pool) task #t)
     (enqueue! (Pool-queue pool) task)]
    [(Pool-Cancel task) (cancel-pool-task! pool task (Analysis-Outcome 'stopped 'cancelled))]
    [(Pool-Stop)
     (set-Pool-stopped?! pool #t)
     (for ([task (in-list (hash-keys (Pool-tasks pool)))])
       (settle-pool-task! pool task (Analysis-Outcome 'stopped 'shutdown)))
     (queue-filter! (Pool-queue pool) (lambda (_) #f))
     (for ([slot (in-list (Pool-slots pool))] #:unless (eq? (Slot-phase slot) 'empty))
       (retire-slot-worker! pool slot))]))

(define (run-pool-manager pool)
  (let loop ()
    ;; Timers precede assignment, even when another event won the previous sync.
    (enforce-pool-deadlines! pool)
    (unless (Pool-stopped? pool) (assign-queued-tasks! pool))
    (unless (and (Pool-stopped? pool)
                 (andmap (lambda (slot) (eq? (Slot-phase slot) 'empty)) (Pool-slots pool)))
      (match (sync-pool-events pool)
        [(Pool-Command command) (handle-pool-command! pool command)]
        [(Pool-Worker-Message slot reply) (handle-worker-message! pool slot reply)]
        [(Pool-Worker-Dead slot) (handle-worker-death! pool slot)]
        [(Pool-Async-Finished finish) (finish)]
        [(Pool-Timer) (void)])
      (loop))))
