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
- `disk-reader.rkt`: ordered per-source disk reads and shared waiters
- `analysis-scheduler.rkt`: shared attempts, FIFO dispatch, capacity, and
  completion
- `place-runner.rkt`: session-owned Check Syntax place pool
- `../analysis-place.rkt`: per-job analysis namespace inside a place
- `supervised-worker.rkt`: disk reads, Resyntax, and injected analyzers
- `analysis-publisher.rkt`: installation, diagnostics, and Resyntax
  publication; buffer operations require the controller-held SafeDoc write lock
- `closed-analysis-cache.rkt`: completed analysis while files are closed
- `completion-event.rkt`: single-owner first-value broadcast, including `#f`

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
  analysis may enqueue Resyntax during acceptance; dispatch occurs after the
  command finishes.

Source replacement cancels affected work. The scheduler also checks queued
snapshot eligibility immediately before launch. Availability lookup and reuse
of completed success belong to the controller, not the scheduler.

`supervised-worker.rkt` suppresses worker stdout and releases resources before
reporting a result. Cancelling work does not kill its supervisor; a late
message is rejected by the reader or scheduler.

## Place execution

Production Check Syntax uses a session-owned pool of reusable places, created
lazily up to `#:analysis-slots` (four by default), with hard cancellation and
supervised restoration. The scheduler is the only FIFO job queue and limits
combined Check Syntax and Resyntax attempts to that capacity. Resyntax uses
its separate executor and shares that bound.

Each pool slot holds at most one place and stays reserved through parent
restoration or observed place retirement. Idle workers cannot cause extra
restoration work to bypass the shared attempt bound.

`#:analyze #f` selects the session's place runner. Ordinary-thread Check Syntax
is no longer a production export: `analyze-source` lives in `analysis.rkt`'s
`test-support` submodule for equivalence checks and the benchmark baseline.
`#:analyze` still accepts a procedure as a deterministic test seam, supervised
with the same attempt deadline. Production Resyntax and disk reads keep
ordinary-thread supervision.

The FIFO scheduler reserves a slot before launch. Its 90-second default
deadline includes startup, collection, serialization, transfer, local document
construction, and restoration. Queue waiting is outside the deadline. Worker
stdout is suppressed from startup, including language loading and extraction.

### Place creation and the stdin reader

Racket CS 9.3 can lose a scheduled-thread count when place creation overlaps
input readiness. The scheduler then undercounts runnable threads, so the
stdin reader never runs and a stdio session hangs.

`main-loop` wraps Check Syntax `dynamic-place*` with
`make-reader-place-start-guard` on this process's stdin reader. The guard
suspends that reader for the native create call and resumes it when creation
returns, including when creation raises. Non-stdio controllers keep the
default identity guard. Local constraints are in
`common/place-start-guard.rkt`.

The pause is only around place creation. During the atomic create window,
other Racket threads cannot cancel that startup. Cancellation after
creation is unchanged.

### Cancellation and reservation

The runner retains its reservation through restoration.

- Cancelling queued work has no worker effect.
- Cancelling running place work signals its bridge. The consumer never waits
  for termination. The bridge kills and observes the place before releasing
  physical capacity and sending its terminal message.
- The scheduler counts cancelled place reservations against the shared bound
  until it consumes that message, including when dispatching Resyntax.
- When only parent restoration remains, cancellation stops that task and
  preserves the cleaned worker.
- A successful assignment returns its place for reuse.
- Logical settlement only removes the attempt and resolves its event.
  Cancellation separately stops its worker.
- Each cancellation handle belongs to one assignment and cannot kill a newer
  job.

Shutdown prevents new dispatch and asynchronously retires both active and idle
places. Bridges and retirement threads share the runner's owning custodian, so
disposing the consumer's custodian cannot interrupt cleanup of its places.

### Replies and reuse

Replies contain a job ID, success plus the completed trace, or normalized
failure text. Job IDs increase across all slots and replacements within the
runner. Each reply is read from its assigned place channel.

- Read and expansion errors remain completed products with diagnostics.
- Unexpected execution, serialization, protocol, and restoration errors fail
  the attempt.
- A worker failure reply retires the place even after job cleanup: the
  exception may have left place-wide state inconsistent.
- A parent restoration failure preserves the worker that returned a normal
  result.
- A reply queued alongside worker death is considered before reporting
  premature exit.
- Cancelled attempts cannot accept late replies. Source revision is checked
  again at acceptance before installation, publication, and shared completion.

The parent restores the completed trace and derives its product without
repeating Check Syntax. The worker loop in `analysis-place.rkt` gives each job
a fresh analysis namespace and custodian and cleans up before replying. Trace
conversion and fidelity live in `doclib/doc-trace.rkt` and
`tests/lib/trace-place-test.rkt`.

## Available results and fresh completion

`SafeDoc-available` holds a completed product and the snapshot it analyzed.
`SafeDoc-analysis-completion` is the broadcast event for the attempt being
joined: a newly scheduled attempt, or a reused completed success's event.
Available analysis may answer queries while that event is pending. Restoring a
product never completes that event or starts Resyntax.

Only queued and running attempts occupy the scheduler's private attempt table.
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

[The benchmark and measured results](../bench/place-runner-results.md) compare
one, two, and four slots with the ordinary-thread test baseline. They cover
cold and warm batches, fresh and warm rapid-edit sessions, controller command
latency, resident memory, and isolated conversion and transport probes. The
report defines their boundaries: controller command latency is not full LSP
request latency, and companion phase samples do not decompose production batch
totals.

Keep the four-slot default and lazy startup. The measured warm heavy batch
benefits from four places, while first use and resident memory cost more than
ordinary threads. These workload-specific samples do not establish a better
universal capacity or justify allocating warm workers before demand. Hard
cancellation still discards the executing place, so repeated cancellation can
lose reuse. This migration does not add prewarming, caching, payload
optimization, or cooperative recovery.

## Verification

Production-path tests use the session consumer and named commands or LSP
messages. Tests that must order individual transitions or replay delayed
worker messages own an isolated controller on the test thread and call the
same owner operations. Assertions run on the test thread; command failures
are values only where a test explicitly exercises that production contract.

Place-pool obligations live in `tests/lsp/place-runner-test.rkt`. Guard local
behavior lives in `tests/lib/place-start-guard-test.rkt`; those tests do not
reproduce the Racket CS hang.
