# Source ownership and analysis lifetime

This file is the contract for analysis across the source controller, its
subsystems, and request handlers. Module comments cover local mechanics. Change
the corresponding code when you change a rule here.

The session consumer owns source authority, analysis attempts, available
results, and publication. Disk readers and analyzers report products; only the
consumer may accept them.

Source identity, source revision, and attempt identity are separate. A revision
is a lifetime token for one source. Retrying the same revision creates a new
attempt.

## Request commands

Request threads use `source-controller.rkt`. Constructing a command event
submits work immediately. Cancelling a wait does not cancel shared work.
Callers must check cancellation before submission and validate the target after
waiting, including when using available facts.

- `source-controller-observe-evt` acquires current source knowledge. For a file
  without an open buffer, each command is an ordered read barrier: it cannot
  reuse a disk read started before the consumer accepted that command.
  Concurrent observations share the next read. Open buffers need no disk read.
- `source-controller-analysis-evt` starts or joins analysis for a snapshot and
  replies with `Analysis-Handle`. `reusable-summary` holds optional reusable
  immutable facts while completion is still pending. `completion-evt` is a
  newly scheduled attempt, or the installed result's completion when a current
  success is reused. A ready outcome takes precedence over reusable facts,
  including failure and stop.
- `source-controller-current-view-evt` asks whether a captured view is still
  current. It does not observe the filesystem.

A source revision certifies the server's accepted observation, not an atomic
filesystem transaction.

## Cross-file queries

Definition and hover observe the target once per request. They use that
snapshot's analysis and perform a final owner view check without rereading
disk.

- An observed target change or invalidated acquisition ends the request with
  RequestFailed. The query does not reacquire or retry. A later query observes
  the target again.
- An external disk change that the server has not observed may leave an
  in-flight query answering from its original snapshot.
- Opening a buffer during a pending disk read may complete acquisition with
  buffer text. Once acquired, that view must remain eligible through the
  query's final check.
- Missing files, Check Syntax errors, and absent bindings supply no feature
  detail. Read or worker failures end the request.
- Changing or closing the requesting document ends it with ContentModified
  (-32801), including pending exact inlay-hint and semantic-token requests.
- Deadlines and shutdown end the request without cancelling shared analysis.

## Source identity

Binding lookup uses a complete lexically normalized native path, submodule
list, phase+space, and identifier. File URI construction is in
`common/source.rkt`.

- Dot segments collapse without filesystem access, symlink resolution, or
  general case folding.
- There is no query restriction on submodules or phase+space: matching Check
  Syntax jump and definition-target callbacks suffice, including after
  upstream fixes.
- Missing or inconsistent upstream binding information gives no detail. The
  query does not fall back to a different submodule or phase.
- Target observation uses `path->source`. Incoming file URIs establish the
  same identity at entry and retain their original URI for outputs, including
  disk snapshots.
- Analysis jump paths complete relative to the load directory before
  normalization. Other relative-path callers use their current directory.
- Path-only outputs use standard `net/url` encoding of the normalized complete
  path. Decoding and normalizing those outputs recovers the source identity;
  original URI text need not match.
- Virtual identities retain the full URI, including query and fragment.

## Owner operations

The `owner` submodule is for the session consumer only. The command queue
accepts named operations and worker completions. It has no arbitrary callback
command.

- Open registers the buffer, restores matching retained analysis, and
  schedules fresh analysis.
- Edit takes the registered buffer selected by the incoming URI, owns the
  write lock, invalidates eligibility before the supplied document mutation,
  and schedules the successor after releasing the lock. The mutation callback
  updates text and client version and must not acquire the same buffer lock.
- Close handles retention, cancellation, and removal of publications.
- `source-controller-refresh-analysis!` joins active work or starts another
  attempt after completion.
- `source-controller-ensure-analysis!` also reuses a current completed success.

## Ownership

`source-controller.rkt` owns command submission, reply handling, and consumer
dispatch. `source-controller/source-owner.rkt` owns the private controller
record, construction, source transitions, acquisition policy, revision
validation, result routing, and shutdown ordering. The public interface cannot
access record fields or subsystem handles. The owner never imports the public
interface. Lifecycle and acceptance policy stay together in the owner so
revision, locking, cancellation, and trace-transfer invariants remain local.

Subsystems own private records and collections. None imports or receives the
controller.

The controller receives its publication workspace through `#:workspace`. The
default is the process's `current-workspace`, also used by existing workspace
requests. Injecting a different publication workspace does not isolate an LSP
session; the request layer must use that same workspace.

## Modules

- `source-controller.rkt`: request-thread commands and consumer dispatch
- `source-controller/source-owner.rkt`: ownership, acceptance, and lifecycle
- `workspace/source-store.rkt`: source authority, revisions, and change signals
- `source-controller/disk-reader.rkt`: ordered per-source disk reads and shared
  waiters
- `source-controller/analysis-scheduler.rkt`: source attempts, acceptance,
  completion, and routing to a pool per kind
- `source-controller/place-runner.rkt`: one pool's FIFO, deadlines, slots,
  restoration, and recovery
- `analysis-place.rkt`: persistent control loop and per-job threads; one place
  can load both task kinds
- `source-controller/supervised-worker.rkt`: independent disk-read supervision
- `source-controller/analysis-publisher.rkt`: installation, diagnostics, and
  Resyntax publication; buffer operations require the controller-held SafeDoc
  write lock
- `source-controller/closed-analysis-cache.rkt`: completed analysis while files
  are closed
- `source-controller/completion-event.rkt`: single-owner first-value broadcast,
  including `#f`

## Acceptance

Worker messages are opaque envelopes. Readers and schedulers check attempt
identity, then invoke a synchronous acceptance procedure on the consumer.
These procedures run inline. They do not wait for workers and do not dispatch
another controller command. They are internal acceptance boundaries, not
request commands and not a callback registry.

- Disk acceptance receives the started view and read result, commits source
  knowledge, cancels analysis when that view changes, and returns the observer
  response plus the successor disk view. The reader then answers current
  waiters and starts any queued read using that successor view.
- Analysis acceptance receives the snapshot, kind, completion, parent
  completion (for Resyntax), and result. The controller validates revision and
  executor products, installs or discards analysis, and translates acceptance
  failures to terminal outcomes. The scheduler removes the attempt and
  resolves its event only after acceptance returns. Successful open-buffer
  analysis may submit Resyntax during acceptance. Its worker may start before
  acceptance returns, but its result can only be accepted by a later owner event.

Source replacement immediately revokes acceptance and cancels affected pool
tasks. Availability lookup and reuse of completed success belong to the
controller, not the scheduler.

`supervised-worker.rkt` suppresses worker stdout and releases resources before
reporting a result. Cancelling work does not kill its supervisor; a late
message is rejected by the reader.

## Place execution

Check Syntax and Resyntax have separate session-owned pools. Each pool has an
ordinary manager thread that owns its FIFO, absolute deadlines, fixed slots,
and recovery. `#:analysis-slots` sets Check Syntax capacity, four by default;
Resyntax has one slot. A controller can have up to five places by default. The
analysis scheduler (`source-controller/analysis-scheduler.rkt`) routes each kind
to its pool, cancels task handles, and accepts outcomes. It does not know worker
identities, availability, or retirement counts. A blocked Resyntax task cannot
reserve Check Syntax capacity, and conversely.

Each slot holds at most one place and stays reserved through native startup,
library initialization, execution, transfer, parent restoration, and cleanup.
With N Check Syntax slots, at most N Check Syntax tasks and one Resyntax task
can execute or restore, with at most N + 1 places. A dead worker stays reserved
through parent cleanup, then retires without becoming idle. Both pools start
places only on submission; constructing a controller creates no places. The
Resyntax pool stays empty until accepted, successful Check Syntax for an open
buffer schedules enabled Resyntax. Idle places stay warm within their own pool.
Shutdown stops both pools.

`#:analyze #f` selects production Check Syntax. `analyze-source` remains in
`analysis.rkt`'s `test-support` submodule for reference collection and the
ordinary-thread benchmark. Injected analyzers and Resyntax procedures use the
same submission, deadline, cancellation, and reservation contract, and run
locally under a task custodian. They do not create a second queue. Disk
readers keep independent supervision.

Each submission captures one absolute monotonic deadline, 90 seconds by
default. That budget includes FIFO waiting, startup, initialization,
execution, serialization, transfer, and restoration. It does not reset when
the task changes phase. Queued work expires even while every slot is occupied.
Same-revision callers share the existing attempt and deadline. The pool checks
activity and deadline before consuming a result. The analysis scheduler checks
the deadline again immediately before acceptance, so a completion delayed in the
consumer queue cannot install after expiry. Installation and publication
remain one short owner operation before shared completion becomes ready.
Resyntax is a separate submission after accepted Check Syntax, with its own
budget.

Worker stdout is suppressed from native startup onward. Pool construction
captures the worker factory, startup guard, watchdog durations, and parent
cleanup procedure. A task captures its restoration procedure at submission.
Changing the factory around submission does not change a pool's workers.

### Place creation and the stdin reader

Racket CS 9.3 can lose a scheduled-thread count when place creation overlaps
input readiness. The runtime then undercounts runnable threads, so the
stdin reader never runs and a stdio session hangs.

`main-loop` wraps `dynamic-place*` in both pools with
`make-reader-place-start-guard` on this process's stdin reader. The guard
suspends that reader for the native create call and resumes it when creation
returns, including when creation raises. Non-stdio controllers keep the
default identity guard. Local constraints are in
`common/place-start-guard.rkt`.

The pause is only around place creation. During the atomic create window,
other Racket threads cannot cancel that startup. Cancellation after
creation is unchanged.

### Cancellation and reservation

Logical settlement and physical capacity release are separate manager events.
Cancellation callers never wait for cleanup. Source changes revoke acceptance
immediately, independently of messages already queued for the controller.

- Queued cancellation or expiry settles the task and removes it from the FIFO.
- Startup or library initialization settles the task, keeps the startup and
  its original watchdog, and never starts the obsolete task. A newly ready
  worker stays available for later work.
- Execution settles the task and sends its unique job ID. The slot stays
  reserved until cleanup acknowledgment.
- Parent restoration stops and cleans that task while preserving the worker.
  The slot stays reserved through parent cleanup.
- Already terminal handles have no effect, including after slot reuse.

### Worker jobs and initialization

The worker control thread stays outside job custody. Each invocation gets a
new job thread and child custodian. Only the control thread sends terminal
success, failure, or cancellation, and only after it has cleaned resources
and dropped job references. Job threads never send those replies. Job death
first polls for a queued outcome; without one, it cleans and reports failure.

Library initialization uses a separate persistent lifetime. Readiness is
reported after the adapter loads. Unhandled initialization errors fail the
assignment and retire the worker.

The Resyntax adapter binds its optional imports when that module loads,
including when Doc imports it for Check Syntax. Import failures log and clear
a cached availability flag; they do not escape to ordinary callers.
`resyntax-available?` reads that flag, and `run-resyntax` returns no findings
when disabled. That covers an absent package and errors or missing exports in
an installed stack. Those handled failures disable Resyntax without retiring
an otherwise responsive worker. Errors while running an available analyzer
still follow the ordinary job-failure path.

### Watchdogs

Cancellation acknowledgment waits one second. Native startup and the first
library initialization share one 90-second watchdog, armed at dispatch and not
renewed when the place is created. A second kind on an existing worker, which
production pools do not send, gets a new 90-second watchdog. These are
operational defaults, not measured bounds. Repeated cancellation does not
restart the startup watchdog or the cleanup-acknowledgment timer.
A stalled worker is killed and observed terminated before its slot becomes
free. Replacement is lazy. Jobs are never retried automatically.

### Shutdown

Shutdown stops admission and dispatch, settles outstanding tasks, stops parent
work, and retires starting, active, and idle workers. The pool's internal
`place-runner-shutdown-evt` becomes ready after the manager exits and every
reservation is released. Submission and stop are serialized at the runner
boundary: no command is admitted behind stop. Later submissions receive a
stopped outcome without keeping a manager thread alive. The controller's
existing shutdown event still signals logical shutdown. Cleanup helpers remain
under the pool's owning session lifetime.

### Replies and reuse

The wire uses the prefab structs in `analysis-place-protocol.rkt` for preparation
and readiness, start, cancellation, and terminal messages. Check Syntax and Resyntax
have separate result structs. Pool and worker event structs stay local to their
threads. Every job command and terminal reply carries a unique assignment ID.
The manager reads messages from the assigned physical worker and validates that
ID, so an obsolete result or cancel cannot affect a later task.

- Reader and expansion errors remain completed Check Syntax products with
  diagnostics.
- A caught job exception with completed cleanup fails its task and keeps the
  worker.
- Parent restoration errors fail their task and keep the cleaned worker.
- Worker death, initialization failure, malformed protocol, or unacknowledged
  cancellation retires the worker. A queued terminal reply is inspected before
  reporting premature death. After valid transfer, parent restoration and
  cleanup may finish independently before retirement; a dead worker is never
  reused.
- Once cancellation or expiry settles a task, later results are discarded.
  Capacity still requires cleanup acknowledgment or observed retirement.

The parent restores the completed trace and derives its product without
repeating Check Syntax. Conversion and fidelity live in `doclib/doc-trace.rkt`
and `tests/lib/trace-place-test.rkt`. The worker's persistent-state audit and
limits are beside its adapters in `analysis-place.rkt`. Fresh namespaces and
job custodians do not roll back arbitrary reader, macro, library-cache, or
native mutations. Interrupted real Check Syntax and installed Resyntax tests
establish specific cleanup and reuse cases, not universal transactional
isolation.

## Available results and fresh completion

`SafeDoc-available` holds a completed product and the snapshot it analyzed.
`SafeDoc-analysis-completion` is the broadcast event for the attempt being
joined: a newly scheduled attempt, or a reused completed success's event.
Available analysis may answer queries while that event is pending. Restoring a
product never completes that event or starts Resyntax.

Only active attempts occupy the analysis scheduler's private attempt table.
Every terminal attempt is removed. An accepted available result retains its
completion event, so repeated acquisitions can reuse success without retaining
the job. Resyntax acceptance compares its parent completion with the installed
result's completion; same-revision retries cannot accept an old attempt's
findings.

Exact editor queries require available analysis under the current source
revision; otherwise they wait for fresh completion. Fast local queries keep
their adjusted-trace fallbacks.

- An `errors` outcome publishes current diagnostics and clears exact
  availability.
- A `failed` outcome or timeout also clears availability. The old trace may
  still support fast fallbacks, but cannot be retained as a completed success.
- Non-success attempts may be retried.

## Close, restore, and retention

Closing an eligible file detaches its trace from the old Doc, moves its
completed product into closed retention, and clears the SafeDoc projections.
Close also cancels pending jobs, signals the old revision, removes the active
workspace contribution, and publishes empty diagnostics. Edits clear
eligibility before adjusting the trace, so an unfinished edit cannot enter
retained storage.

Opening creates a new source revision. Exact text equality permits transferring
a retained trace into the new buffer, installing its contribution, and
publishing its Check Syntax diagnostics to the incoming URI. The restored
available result has no fresh completion in that lifetime. Every open
schedules fresh analysis. Old completion events and Resyntax findings are
never restored.

Closed-target definition and hover requests observe saved text before
acquisition. They may use matching retained facts while fresh analysis runs,
then perform final view validation. Text equality does not establish
dependency freshness: retained dependency information can remain stale until
fresh analysis replaces it. Neither current revisions nor fresh completion
track external dependency changes.

Only closed files occupy retention: at most 64 entries by default,
configurable with `#:closed-analysis-limit`. Results are keyed by source ID.
Insertion and successful text-matching lookups mark an entry most recently
used; misses and text mismatches leave recency unchanged. Insertion beyond
capacity evicts the least recently used result without changing source
authority or live documents. Reopening removes the entry before checking its
text and transferring its trace, so retained storage cannot keep a trace that
a live document starts modifying.

A zero limit disables retention; evicted sources must analyze again. Existing
waiters retain their outcomes. Virtual sources never enter retention. Shutdown
stops work and clears retention.

## Capacity

Keep four Check Syntax slots, one Resyntax slot, and lazy startup. Do not add
prewarming, priority scheduling, or automatic retry. Normal cancellation
preserves the executing place after job cleanup.

The earlier [shared-pool measurements](../bench/shared-place-pool-results.md)
record rapid-edit latency and worker-creation counts with a single shared
pool, including slower samples. Those measurements do not evaluate the
separate pools. They do not establish a general latency improvement. The
archived [place-runner measurements](../bench/place-runner-results.md)
compared one, two, and four slots with an ordinary-thread baseline. Those
samples showed a warm heavy-batch benefit at four places, and higher first-use
and resident-memory cost than ordinary threads. They do not justify a
different default or allocating workers before demand. Controller command
latency in those reports is not full LSP request latency.

## Verification

Production-path tests use the session consumer and named commands or LSP
messages. Tests that must order individual transitions or replay delayed
worker messages own an isolated controller on the test thread and call the
same owner operations. Assertions run on the test thread. Command failures
are values only where a test explicitly exercises that production contract.

- `tests/resyntax/separate-pools-test.rkt`: lazy place creation, independent
  capacity, shutdown, and the Check Syntax success gate.
- `tests/resyntax/place-runner-test.rkt`: pool transitions and fallback, using a
  controllable real place. Barriers are explicit receipts, not sleeps.
- `tests/resyntax/shared-worker-test.rkt`: started real jobs, resource cleanup,
  result/cancel races, and later Check Syntax and Resyntax in the same place.
  An availability skip does not establish Resyntax integration.
- `tests/resyntax/resyntax-loading-test.rkt`: eager imports, cached availability,
  optional-package absence, and logging that disables an installed stack
  whose imports fail.
- `tests/lib/place-start-guard-test.rkt`: local guard behavior. Those tests
  do not reproduce the Racket CS hang.
