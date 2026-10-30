#lang racket/base

;; Worker pool coordinating persistent Racket place instances for background analysis.
;;
;; Each runner manages background analysis for one analysis kind ('check-syntax or 'resyntax).
;; The pool manager thread serializes admission, maintains a FIFO queue of pending tasks,
;; schedules work to a configurable number of execution slots, enforces task and watchdog deadlines,
;; and supervises parent-thread result restoration.
;;
;; Logical settlement and physical reservation are decoupled:
;; - Cancellation and deadline expiry notify callers immediately without waiting for
;;   physical place cleanup.
;; - Successful results settle after parent result reconstruction and cleanup finish.
;; - A slot remains reserved through all intermediate phases until cleanup or retirement completes.
;;
;; Deadlines governing execution:
;; 1. Task deadline: Absolute monotonic timestamp supplied per task by the caller, covering
;;    queue wait, place execution, and parent result restoration. Expiry cancels the task
;;    while preserving a healthy worker place for subsequent work.
;; 2. Startup watchdog: Configured timeout (default: 30s) covering native place creation plus
;;    first adapter initialization. Expiry retires the uncommunicative worker place.
;; 3. Cleanup watchdog: Configured timeout (default: 1s) covering cancellation acknowledgment
;;    from the worker place. Expiry retires the unresponsive worker place.
;;
;; Canonical cross-file ownership and lifecycle contracts are defined in ../analysis-lifecycle.md.

(require racket/async-channel
         racket/contract
         racket/list
         racket/match
         racket/place
         racket/port
         racket/runtime-path
         data/queue
         "../../common/interfaces.rkt"
         "../../common/json-util.rkt"
         "../../common/time.rkt"
         "../../workspace/source-store.rkt"
         "../analysis.rkt"
         "../analysis-place-protocol.rkt")

(provide Place-Runner?
         Place-Task?
         (contract-out
           [make-place-runner
            (->* ((or/c 'check-syntax 'resyntax)
                  #:slots exact-positive-integer?)
                 (#:place-start-guard (-> (-> any) any))
                 Place-Runner?)]
           [submit-place-task!
            (->* (Place-Runner?
                   Source-Snapshot?
                   async-channel?
                   real?
                   (-> any/c any/c))
                 (#:work (or/c #f (-> any/c)))
                 Place-Task?)]
           [cancel-place-task!
            (-> Place-Task? void?)]
           [resize-place-runner!
            (-> Place-Runner? exact-positive-integer? void?)]
           [stop-place-runner!
            (-> Place-Runner? void?)]
           [place-runner-shutdown-evt
            (-> Place-Runner? evt?)]))

(define-runtime-path worker-path "../analysis-place.rkt")

;;; Types and records

;; External boundary handle for submitting tasks to the pool manager thread.
;;
;; commands: Channel delivering submit, cancel, and stop commands to the manager loop.
;; gate: Mutex serializing submission and stop so no command enters after shutdown.
;; shutdown: Semaphore posted after the manager releases all slots and exits its loop.
;; stopped?: Mutable flag set at stop; lets subsequent submissions fail fast without waking the manager.
;; manager: The pool manager thread, or #f before startup. Observed by place-runner-shutdown-evt.
(struct/contract Place-Runner
  ([commands async-channel?]
   [gate semaphore?]
   [shutdown semaphore?]
   [(stopped? #:mutable) boolean?]
   [(manager #:mutable) (or/c #f thread?)]))

;; Represents one submitted unit of analysis work.
;;
;; runner: Owning runner instance through which cancellation is routed.
;; snapshot: Immutable source snapshot retained for parent-side result reconstruction.
;; deadline: Absolute monotonic timestamp (milliseconds) by which the task must settle.
;; directory: Working directory captured at submission for module resolution in worker and parent.
;; events: Caller-provided channel receiving the terminal result envelope.
;; done: Caller constructor wrapping the terminal Analysis-Outcome or analysis result.
;; work: Optional injected analyzer run in the parent thread under task custody.
(struct/contract Place-Task
  ([runner Place-Runner?]
   [snapshot Source-Snapshot?]
   [deadline real?]
   [directory path?]
   [events async-channel?]
   [done (-> any/c any/c)]
   [work (or/c #f (-> any/c))]))

;; Physical place process and its initialization state, decoupled from any single task.
;;
;; custodian: Dedicated child custodian governing the place process and its redirected output port.
;; place: Native place handle, or #f while initial spawn is in flight.
;; terminated?: Flag set when place process exit is observed; ensures a dead worker is never reused.
;; events: Cached sync events for incoming place messages and place process termination.
(struct/contract Worker
  ([custodian custodian?]
   [(place #:mutable) (or/c #f place?)]
   [(terminated? #:mutable) boolean?]
   [(events #:mutable) (listof evt?)]))

;; Background helper thread for asynchronous slot operations (spawn, parent work, retirement).
;;
;; thread: Background thread executing the operation under the appropriate custodian.
;; event: Synchronization event delivering the completed callback to the manager loop.
(struct/contract Slot-Helper
  ([thread thread?]
   [event evt?]))

;; Execution slot reserving one unit of pool capacity through all lifecycle phases.
;;
;; worker: Underlying Worker instance, or #f when empty or fully retired.
;; task: Currently assigned Place-Task, retained until cleanup completes even if settled early.
;; phase: Slot lifecycle state. Only 'empty and 'idle are assignable; all others reserve capacity.
;; watchdog-deadline: Absolute monotonic watchdog deadline (milliseconds) for startup,
;;   initialization, or cancellation cleanup; #f when idle, executing, or restoring.
;; assignment-id: Monotonically increasing ID used to correlate start and cancel commands.
;; helper: In-flight Slot-Helper, or #f when no background helper is active.
;; work-custodian: Custodian scoping parent-side restoration or injected work.
(struct/contract Slot
  ([(worker #:mutable) (or/c #f Worker?)]
   [(task #:mutable) (or/c #f Place-Task?)]
   [(phase #:mutable) (or/c 'empty 'idle 'spawning 'preparing 'executing 'cancelling
                            'restoring 'injected 'cleaning 'retiring)]
   [(watchdog-deadline #:mutable) (or/c #f real?)]
   [(assignment-id #:mutable) (or/c #f exact-positive-integer?)]
   [(helper #:mutable) (or/c #f Slot-Helper?)]
   [(work-custodian #:mutable) (or/c #f custodian?)]))

;; Private mutable pool state managed exclusively by the manager thread.
;;
;; kind: Analysis kind ('check-syntax or 'resyntax) used by every worker in this pool.
;; slots: Admission slots followed by excess slots draining after a capacity reduction.
;; slot-limit: Desired admission capacity; excess slots get no new assignments.
;; task-queue: FIFO queue of Place-Task submissions waiting for an assignable slot.
;; active-tasks: Hash set of all active tasks currently tracked by this pool.
;; owner-custodian: Session custodian under which slot custodians and cleanup helpers are rooted.
;; place-start-guard: Startup hook (such as place-start-guard) wrapping native place creation.
;; worker-factory: Procedure spawning new physical worker place instances.
;; cleanup-watchdog-ms: Watchdog duration (milliseconds) allotted for cancellation cleanup.
;; startup-watchdog-ms: Watchdog duration (milliseconds) allotted for place spawn and initialization.
;; parent-cleaner: Procedure invoked to terminate parent work custodians.
;; analysis-restorer: Procedure reconstructing parent-side analysis structures from wire results.
;; command-event: Event wrapped with Pool-Command for incoming external runner commands.
;; stopped?: Set when Pool-Stop is processed to cease dispatch and initiate teardown.
;; assignment-counter: Sequence counter generating unique assignment IDs across worker reuse.
(struct/contract Pool
  ([kind (or/c 'check-syntax 'resyntax)]
   [(slots #:mutable) (listof Slot?)]
   [(slot-limit #:mutable) exact-positive-integer?]
   [task-queue queue?]
   [active-tasks (hash/c Place-Task? boolean? #:immutable #f)]
   [owner-custodian custodian?]
   [place-start-guard (-> (-> any) any)]
   [worker-factory (-> procedure? place?)]
   [cleanup-watchdog-ms (and/c real? (>=/c 0))]
   [startup-watchdog-ms (and/c real? (>=/c 0))]
   [parent-cleaner (-> custodian? any)]
   [analysis-restorer (-> Source-Snapshot? boolean? any/c any/c)]
   [command-event evt?]
   [(stopped? #:mutable) boolean?]
   [(assignment-counter #:mutable) exact-nonnegative-integer?]))

;; Internal event messages dispatched across the manager event loop.
(struct/contract Pool-Submit
  ([task Place-Task?])
  #:transparent)

(struct/contract Pool-Cancel
  ([task Place-Task?])
  #:transparent)

(struct/contract Pool-Resize
  ([slots exact-positive-integer?])
  #:transparent)

(struct/contract Pool-Stop ()
  #:transparent)

(struct/contract Pool-Worker-Message
  ([slot Slot?] [message any/c])
  #:transparent)

(struct/contract Pool-Worker-Dead
  ([slot Slot?])
  #:transparent)

(struct/contract Pool-Helper-Finished
  ([callback (-> any)])
  #:transparent)

(struct/contract Pool-Timer ()
  #:transparent)

(struct/contract Pool-Command
  ([command (or/c Pool-Submit? Pool-Cancel? Pool-Resize? Pool-Stop?)])
  #:transparent)

(define (failure-outcome reason)
  (Analysis-Outcome 'failed (if (exn? reason) (exn-message reason) reason)))

;; Delivers the terminal result envelope to the caller's events channel.
(define (deliver-task-outcome! task outcome)
  (async-channel-put (Place-Task-events task) ((Place-Task-done task) outcome)))

(define (active-task? pool task)
  (hash-has-key? (Pool-active-tasks pool) task))

;; Untracks the task from the pool and immediately notifies the caller if not already settled.
(define (settle-pool-task! pool task outcome)
  (when (active-task? pool task)
    (hash-remove! (Pool-active-tasks pool) task)
    (deliver-task-outcome! task outcome)))

;;; Public constructors and entry points

(define (make-place-runner kind
                           #:slots slot-count
                           #:place-start-guard [guard (lambda (thunk) (thunk))])
  (make-place-runner/configured kind #:slots slot-count #:place-start-guard guard))

(define (make-place-runner/configured kind
                                      #:slots slot-count
                                      #:place-start-guard [guard (lambda (thunk) (thunk))]
                                      #:factory [factory spawn-worker]
                                      #:restorer [restorer restore-analysis]
                                      #:cleanup-seconds [cleanup-seconds 1]
                                      #:startup-seconds [startup-seconds 30]
                                      #:cleaner [cleaner custodian-shutdown-all])
  (define runner (Place-Runner (make-async-channel) (make-semaphore 1) (make-semaphore 0) #f #f))
  (define pool
    (Pool kind
          (for/list ([_ (in-range slot-count)])
            (Slot #f #f 'empty #f #f #f #f))
          slot-count
          (make-queue) (make-hasheq)
          (current-custodian)
          guard factory
          (* 1000 cleanup-seconds) (* 1000 startup-seconds)
          cleaner restorer
          (handle-evt (Place-Runner-commands runner) Pool-Command)
          #f 0))

  (set-Place-Runner-manager!
    runner
    (thread (lambda ()
              (run-pool-manager pool)
              (semaphore-post (Place-Runner-shutdown runner)))))
  runner)

(define (spawn-worker place-start-guard)
  ;; dynamic-place* requires a file-stream port for stdout redirection.
  ;; The worker custodian owns this port through warm reuse; retirement closes it.
  (define sink-path (if (eq? (system-type) 'windows) "NUL" "/dev/null"))
  (define sink (open-output-file sink-path #:exists 'append))
  (define-values (place in-port _stdout _stderr)
    (place-start-guard
      (lambda ()
        (dynamic-place* worker-path 'run #:out sink #:err (current-error-port)))))
  (close-output-port in-port)
  place)

(module+ test-support
  (provide
    (contract-out
      [make-place-runner/configured
       (->* ((or/c 'check-syntax 'resyntax)
             #:slots exact-positive-integer?)
            (#:place-start-guard (-> (-> any) any)
             #:factory (-> procedure? place?)
             #:restorer (-> Source-Snapshot? boolean? any/c any/c)
             #:cleanup-seconds (and/c real? (>=/c 0))
             #:startup-seconds (and/c real? (>=/c 0))
             #:cleaner (-> custodian? any))
            Place-Runner?)]
      [spawn-worker (-> procedure? place?)])))

(define (submit-place-task! runner snapshot events deadline done #:work [work #f])
  (define task
    (Place-Task runner snapshot deadline (current-directory)
                events done work))
  (call-with-semaphore
    (Place-Runner-gate runner)
    (lambda ()
      (if (Place-Runner-stopped? runner)
          (deliver-task-outcome! task (Analysis-Outcome 'stopped 'shutdown))
          (async-channel-put (Place-Runner-commands runner) (Pool-Submit task)))))
  task)

(define (cancel-place-task! task)
  (define runner (Place-Task-runner task))
  (call-with-semaphore
    (Place-Runner-gate runner)
    (lambda ()
      (unless (Place-Runner-stopped? runner)
        (async-channel-put (Place-Runner-commands runner) (Pool-Cancel task))))))

(define (resize-place-runner! runner slots)
  (call-with-semaphore
    (Place-Runner-gate runner)
    (lambda ()
      (unless (Place-Runner-stopped? runner)
        (async-channel-put (Place-Runner-commands runner) (Pool-Resize slots))))))

(define (stop-place-runner! runner)
  (call-with-semaphore
    (Place-Runner-gate runner)
    (lambda ()
      (unless (Place-Runner-stopped? runner)
        (set-Place-Runner-stopped?! runner #t)
        (async-channel-put (Place-Runner-commands runner) (Pool-Stop))))))

(define (place-runner-shutdown-evt runner)
  ;; Delivers readiness once the manager exits after all reservations are cleared.
  (replace-evt (semaphore-peek-evt (Place-Runner-shutdown runner))
               (lambda (_) (thread-dead-evt (Place-Runner-manager runner)))))

;;; Helper threads and slot lifecycle

;; Launches an asynchronous helper thread delivering its completion callback back to the manager.
;; The helper thread runs under the specified custodian (defaulting to the pool session custodian)
;; and delivers either the success value or an error outcome through result-channel.
(define (start-slot-helper! pool slot phase thunk on-success on-failure
                            [custodian (Pool-owner-custodian pool)])
  (define result-channel (make-async-channel))

  (define helper-thread
    (parameterize ([current-custodian custodian])
      (thread
        (lambda ()
          (define finish-action
            (with-handlers ([(lambda (_) #t)
                             (lambda (reason)
                               (define outcome (failure-outcome reason))
                               (lambda () (on-failure outcome)))])
              (define value (thunk))
              (lambda () (on-success value))))
          (async-channel-put result-channel finish-action)))))

  ;; If the helper terminates immediately after putting its completion thunk, thread-dead-evt
  ;; fires concurrently with result-channel. Polling with async-channel-try-get guarantees
  ;; that a completed result is never dropped in favor of a spurious 'worker-exited outcome.
  (define completion-evt
    (choice-evt
      (handle-evt result-channel Pool-Helper-Finished)
      (handle-evt (thread-dead-evt helper-thread)
                  (lambda (_)
                    (Pool-Helper-Finished
                      (or (async-channel-try-get result-channel)
                          (lambda () (on-failure (failure-outcome 'worker-exited)))))))))

  (set-Slot-phase! slot phase)
  (set-Slot-helper! slot (Slot-Helper helper-thread completion-evt)))

(define (stop-slot-helper! slot)
  (define helper (Slot-helper slot))
  (when helper
    (kill-thread (Slot-Helper-thread helper)))
  (set-Slot-helper! slot #f))

;; Resets slot assignment fields. Exposes a warm worker as 'idle, or transitions to 'retiring
;; if the worker place died. A dead worker never transitions through 'idle.
(define (clear-slot-assignment! pool slot)
  (set-Slot-assignment-id! slot #f)
  (set-Slot-task! slot #f)
  (set-Slot-helper! slot #f)
  (set-Slot-work-custodian! slot #f)
  (set-Slot-watchdog-deadline! slot #f)
  (cond
    [(not (Slot-worker slot)) (set-Slot-phase! slot 'empty)]
    [(Worker-terminated? (Slot-worker slot)) (retire-slot-worker! pool slot)]
    [else (set-Slot-phase! slot 'idle)]))

(define (fail-slot! pool slot outcome)
  (when (Slot-task slot)
    (settle-pool-task! pool (Slot-task slot) outcome))
  (retire-slot-worker! pool slot))

;; Tears down worker and parent resources for a slot asynchronously.
;; Shuts down parent work custody, kills managed place instances, and shuts down worker custody.
(define (retire-slot-worker! pool slot)
  (unless (eq? (Slot-phase slot) 'retiring)
    (stop-slot-helper! slot)
    (define worker (Slot-worker slot))
    (define work-custodian (Slot-work-custodian slot))
    (set-Slot-watchdog-deadline! slot #f)
    (start-slot-helper! pool slot 'retiring
                        (lambda ()
                          (when work-custodian
                            (custodian-shutdown-all work-custodian))
                          (when worker
                            (define worker-custodian (Worker-custodian worker))
                            ;; custodian-managed-list finds place handles created before the
                            ;; startup helper recorded them into the Worker struct.
                            (define places
                              (filter place?
                                      (custodian-managed-list worker-custodian
                                                              (Pool-owner-custodian pool))))
                            (for ([place (in-list places)])
                              (place-kill place))
                            (for ([place (in-list places)])
                              (place-wait place))
                            (custodian-shutdown-all worker-custodian)))
                        (lambda (_)
                          (set-Slot-worker! slot #f)
                          (clear-slot-assignment! pool slot))
                        (lambda (outcome)
                          (error 'place-runner "worker retirement failed: ~v" outcome)))))

;; Tears down parent work custody. Validates task deadline upon completion; if the deadline
;; passed while parent cleanup was in flight, settles as 'analysis-timeout instead of success.
(define (cleanup-parent-work! pool slot [outcome #f])
  (stop-slot-helper! slot)
  (define work-custodian (Slot-work-custodian slot))
  (start-slot-helper! pool slot 'cleaning
                      (lambda () ((Pool-parent-cleaner pool) work-custodian))
                      (lambda (_)
                        (define task (Slot-task slot))
                        (when (active-task? pool task)
                          (settle-pool-task! pool task
                                             (if (>= (current-deadline-milliseconds)
                                                     (Place-Task-deadline task))
                                                 (failure-outcome 'analysis-timeout)
                                                 outcome)))
                        (clear-slot-assignment! pool slot))
                      (lambda (clean-failure) (fail-slot! pool slot clean-failure))))

(define (find-slot-by-task pool task)
  (for/first ([slot (in-list (Pool-slots pool))]
              #:when (eq? task (Slot-task slot)))
    slot))

(define (cancel-pool-task! pool task outcome)
  (when (active-task? pool task)
    (settle-pool-task! pool task outcome)
    (queue-filter! (Pool-task-queue pool) (lambda (waiting) (not (eq? waiting task))))
    (define slot (find-slot-by-task pool task))
    (when slot
      (case (Slot-phase slot)
        [(executing)
         (place-channel-put (Worker-place (Slot-worker slot))
                            (Place-Cancel (Slot-assignment-id slot)))
         (set-Slot-phase! slot 'cancelling)
         (set-Slot-watchdog-deadline! slot
                                      (+ (current-deadline-milliseconds)
                                         (Pool-cleanup-watchdog-ms pool)))]
        [(restoring injected)
         (cleanup-parent-work! pool slot)]
        ;; Worker spawn and preparation survive cancellation with their original watchdog.
        ;; A cancelled task is never dispatched to the newly prepared worker.
        [else (void)]))))

(define (start-parent-work! pool slot task thunk phase)
  (define work-custodian (make-custodian (Pool-owner-custodian pool)))
  (set-Slot-work-custodian! slot work-custodian)
  (start-slot-helper! pool slot phase
                      (lambda ()
                        (parameterize ([current-directory (Place-Task-directory task)]
                                       [current-output-port (open-output-nowhere)])
                          (thunk)))
                      (lambda (result) (cleanup-parent-work! pool slot result))
                      (lambda (outcome) (cleanup-parent-work! pool slot outcome))
                      work-custodian))

(define (start-worker-job! slot task)
  (define input (Place-Task-snapshot task))
  (set-Slot-phase! slot 'executing)
  (set-Slot-watchdog-deadline! slot #f)
  (place-channel-put (Worker-place (Slot-worker slot))
                     (Place-Start (Slot-assignment-id slot)
                                  (Source-Snapshot-uri input)
                                  (Source-Snapshot-text input)
                                  (Place-Task-directory task))))

;; Dispatches code loading in the worker place. Newly spawned places use the remainder
;; of the startup watchdog for this first adapter initialization.
(define (prepare-worker-for-task! pool slot)
  (set-Slot-phase! slot 'preparing)
  (place-channel-put (Worker-place (Slot-worker slot))
                     (Place-Prepare (Pool-kind pool))))

(define (register-spawned-worker! pool slot place)
  (define worker (Slot-worker slot))
  (set-Worker-place! worker place)
  (set-Worker-events!
    worker
    (list (handle-evt place (lambda (message) (Pool-Worker-Message slot message)))
          (handle-evt (place-dead-evt place) (lambda (_) (Pool-Worker-Dead slot)))))
  (set-Slot-helper! slot #f)
  ;; Initialize adapter even if the original task was cancelled during spawn.
  (prepare-worker-for-task! pool slot))

(define (spawn-slot-worker! pool slot)
  (define worker-custodian (make-custodian (Pool-owner-custodian pool)))
  (set-Slot-worker! slot (Worker worker-custodian #f #f '()))
  (start-slot-helper! pool slot 'spawning
                      (lambda ()
                        ((Pool-worker-factory pool) (Pool-place-start-guard pool)))
                      (lambda (place)
                        (register-spawned-worker! pool slot place))
                      (lambda (outcome)
                        (fail-slot! pool slot outcome))
                      worker-custodian))

;;; Task dispatch and message processing

(define (assign-task-to-slot! pool slot task)
  (set-Slot-task! slot task)
  ;; A unique ID prevents an obsolete cancel from stopping a later assignment.
  (set-Pool-assignment-counter! pool (add1 (Pool-assignment-counter pool)))
  (set-Slot-assignment-id! slot (Pool-assignment-counter pool))
  (cond
    [(Place-Task-work task)
     (start-parent-work! pool slot task (Place-Task-work task) 'injected)]
    [(Slot-worker slot)
     (start-worker-job! slot task)]
    [else
     (set-Slot-watchdog-deadline! slot
                                  (+ (current-deadline-milliseconds)
                                     (Pool-startup-watchdog-ms pool)))
     (spawn-slot-worker! pool slot)]))

;; Keep busy excess slots observable until their task and cleanup finish. Idle
;; excess workers retire asynchronously; only empty excess slots can be forgotten.
(define (retire-excess-slots! pool)
  (define limit (Pool-slot-limit pool))
  (when (> (length (Pool-slots pool)) limit)
    (set-Pool-slots!
      pool
      (for/list ([slot (in-list (Pool-slots pool))] [index (in-naturals)]
                 #:unless (and (>= index limit) (eq? (Slot-phase slot) 'empty)))
        (when (and (>= index limit) (eq? (Slot-phase slot) 'idle))
          (retire-slot-worker! pool slot))
        slot))))

(define (assign-queued-tasks! pool)
  (unless (queue-empty? (Pool-task-queue pool))
    (for ([slot (in-list (Pool-slots pool))]
          [_ (in-range (Pool-slot-limit pool))]
          #:when (memq (Slot-phase slot) '(empty idle))
          #:break (queue-empty? (Pool-task-queue pool)))
      (assign-task-to-slot! pool slot (dequeue! (Pool-task-queue pool))))))

(define (dispatch-worker-message! pool slot message)
  (define task (Slot-task slot))
  (match message
    [(Place-Ready)
     (if (active-task? pool task)
         (start-worker-job! slot task)
         (clear-slot-assignment! pool slot))]

    [(Place-Terminal payload)
     (set-Slot-watchdog-deadline! slot #f)
     (match payload
       [(Place-Failure detail)
        (settle-pool-task! pool task (failure-outcome detail))
        (clear-slot-assignment! pool slot)]

       [(Place-Cancelled)
        (clear-slot-assignment! pool slot)]

       [(Place-Check-Syntax-Result success? data)
        (if (active-task? pool task)
            (start-parent-work! pool slot task
                                (lambda ()
                                  ((Pool-analysis-restorer pool)
                                   (Place-Task-snapshot task)
                                   success?
                                   data))
                                'restoring)
            (clear-slot-assignment! pool slot))]

       [(Place-Resyntax-Result data)
        (if (active-task? pool task)
            (start-parent-work! pool slot task
                                (lambda ()
                                  (map jsexpr->Resyntax-Result data))
                                'restoring)
            (clear-slot-assignment! pool slot))])]

    [(Place-Init-Failure detail)
     (fail-slot! pool slot (failure-outcome detail))]))

(define (handle-worker-message! pool slot message)
  (define task (Slot-task slot))
  ;; sync may have waited past the deadline or selected a message over its timer.
  (when (and (active-task? pool task)
             (>= (current-deadline-milliseconds) (Place-Task-deadline task)))
    ;; A terminal needs no cancel after cleanup; readiness must not start expired work.
    (settle-pool-task! pool task (failure-outcome 'analysis-timeout)))
  (dispatch-worker-message! pool slot message))

(define (handle-worker-death! pool slot)
  (define worker (Slot-worker slot))
  (set-Worker-terminated?! worker #t)
  ;; A queued terminal reply wins over place death. A dead place is never reused.
  (define message (sync/timeout 0 (Worker-place worker)))
  (when message
    (dispatch-worker-message! pool slot message))
  ;; Data already transferred to the parent no longer depends on the place process.
  ;; Retain the slot reservation through parent cleanup, then clear-slot-assignment!
  ;; retires the worker.
  (unless (memq (Slot-phase slot) '(restoring cleaning))
    (fail-slot! pool slot (failure-outcome 'worker-exited))))

(define (slot-events slot)
  (define worker (Slot-worker slot))
  (define helper (Slot-helper slot))
  ;; Place termination events remain ready forever once triggered. Skip observed
  ;; termination during parent restoration/cleanup, and skip all worker events
  ;; during retirement, so the manager event loop cannot spin on an already-handled event.
  (define worker-evts
    (if (and worker
             (not (Worker-terminated? worker))
             (not (eq? (Slot-phase slot) 'retiring)))
        (Worker-events worker)
        '()))
  (if helper
      (cons (Slot-Helper-event helper) worker-evts)
      worker-evts))

(define (enforce-pool-deadlines! pool)
  (define now (current-deadline-milliseconds))
  ;; hash-keys snapshots keys before iteration because cancel-pool-task! removes
  ;; expired tasks from (Pool-active-tasks pool) in place.
  (for ([task (in-list (hash-keys (Pool-active-tasks pool)))]
        #:when (>= now (Place-Task-deadline task)))
    (cancel-pool-task! pool task (failure-outcome 'analysis-timeout)))
  (for ([slot (in-list (Pool-slots pool))]
        #:when (and (Slot-watchdog-deadline slot) (>= now (Slot-watchdog-deadline slot))))
    (fail-slot! pool slot (failure-outcome 'worker-watchdog))))

;;; Manager event loop

(define (earliest-pool-deadline pool)
  (define min-task-deadline
    (for/fold ([earliest #f])
              ([task (in-hash-keys (Pool-active-tasks pool))])
      (define task-deadline (Place-Task-deadline task))
      (if earliest (min earliest task-deadline) task-deadline)))
  (for/fold ([earliest min-task-deadline])
            ([slot (in-list (Pool-slots pool))])
    (define watchdog-deadline (Slot-watchdog-deadline slot))
    (cond
      [(not watchdog-deadline) earliest]
      [(not earliest) watchdog-deadline]
      [else (min earliest watchdog-deadline)])))

(define (sync-pool-events pool)
  (define earliest (earliest-pool-deadline pool))
  (define timer-events
    (if earliest
        (list (handle-evt (deadline-evt earliest) (lambda (_) (Pool-Timer))))
        '()))
  (apply sync
         (Pool-command-event pool)
         (append timer-events
                 (append-map slot-events (Pool-slots pool)))))

(define (handle-pool-command! pool command)
  (match command
    [(Pool-Submit task)
     (hash-set! (Pool-active-tasks pool) task #t)
     (enqueue! (Pool-task-queue pool) task)]

    [(Pool-Cancel task)
     (cancel-pool-task! pool task (Analysis-Outcome 'stopped 'cancelled))]

    [(Pool-Resize slots)
     (set-Pool-slot-limit! pool slots)
     (set-Pool-slots!
       pool
       (append (Pool-slots pool)
               (for/list ([_ (in-range (length (Pool-slots pool)) slots)])
                 (Slot #f #f 'empty #f #f #f #f))))
     (retire-excess-slots! pool)]

    [(Pool-Stop)
     (set-Pool-stopped?! pool #t)
     (for ([task (in-list (hash-keys (Pool-active-tasks pool)))])
       (settle-pool-task! pool task (Analysis-Outcome 'stopped 'shutdown)))
     ;; Clear queued tasks that were never assigned to slots.
     (queue-filter! (Pool-task-queue pool) (lambda (_) #f))
     (for ([slot (in-list (Pool-slots pool))] #:unless (eq? (Slot-phase slot) 'empty))
       (retire-slot-worker! pool slot))]))

(define (run-pool-manager pool)
  (let loop ()
    ;; Timers precede assignment, even when an external command won the previous sync.
    (enforce-pool-deadlines! pool)
    (unless (Pool-stopped? pool)
      (retire-excess-slots! pool)
      (assign-queued-tasks! pool))

    (unless (and (Pool-stopped? pool)
                 (andmap (lambda (slot) (eq? (Slot-phase slot) 'empty)) (Pool-slots pool)))
      (match (sync-pool-events pool)
        [(Pool-Command command) (handle-pool-command! pool command)]
        [(Pool-Worker-Message slot message) (handle-worker-message! pool slot message)]
        [(Pool-Worker-Dead slot) (handle-worker-death! pool slot)]
        [(Pool-Helper-Finished callback) (callback)]
        [(Pool-Timer) (void)])
      (loop))))
