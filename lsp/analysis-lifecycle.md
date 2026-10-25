# Source ownership and analysis lifetime

The session consumer owns source authority, analysis attempts, available results,
and publication. Disk readers and analyzers run privately and report products;
only the consumer can accept them. Source identity, source revision, and attempt
identity are separate: a revision is a source lifetime token, and retrying the
same revision creates a new attempt.

## Interfaces and responsibilities

`source-controller.rkt` exposes commands for request threads:

- `source-controller-observe-evt` acquires current source knowledge. For a file
  without an open buffer, each command is an ordered read barrier: it cannot
  reuse a disk read started before the consumer accepted that command. Concurrent
  observations share the next read. Open buffers require no disk read.
- `source-controller-analysis-evt` starts or joins analysis for a snapshot and
  replies with `Analysis-Handle`. Its `reusable-summary` field contains optional
  reusable immutable facts while the completion event is still pending. Its
  `completion-evt` is a newly scheduled attempt, or the installed result's
  completion when a current success is reused. A ready outcome takes precedence
  over reusable facts, including failure and stop.
- `source-controller-current-view-evt` asks the consumer whether a captured view
  is still current. It does not observe the filesystem.

Constructing a command event submits work immediately. Cancelling a request's
wait does not cancel shared work. Request callers must check cancellation before
submission and validate the target after waiting, including when using available
facts. A source revision certifies the server's accepted observation, not an
atomic filesystem transaction.

Cross-file definition and hover observe the target once per request. They use
that snapshot's analysis and perform a final owner view check without rereading
disk. An observed target change or invalidated acquisition ends the request with
RequestFailed; the query does not reacquire or retry. A subsequent query observes
the target again. An external disk change that the server has not observed may
therefore leave an in-flight query answering from its original snapshot. Opening
a buffer during a pending disk read may complete acquisition with buffer text;
once acquired, that view must remain eligible through the query's final check.
Missing files, Check Syntax errors, and absent bindings supply no feature detail.
Read/worker failures end the request. Changing or closing the requesting document
ends it with ContentModified (-32801), including pending exact inlay-hint and
semantic-token requests. Deadlines and shutdown end it without cancelling shared analysis.

Binding lookup uses a complete lexically normalized native path, submodule list,
phase+space, and identifier. Dot segments collapse without filesystem access,
symlink resolution, or general case folding.
There is no query restriction on submodules or phase+space: matching Check Syntax
jump and definition-target callbacks suffice, including after upstream fixes.
Missing or inconsistent upstream binding information gives no detail; the query
does not fall back to a different submodule or phase. Target observation uses
`path->source`. Incoming file URIs establish the same identity at entry while
retaining their original URI for outputs, including disk snapshots. Analysis jump
paths complete relative to its load directory before normalization. Other relative
path callers use their current directory. Path-only outputs use standard `net/url`
encoding of the normalized complete path. Decoding and normalizing those outputs
recovers the source identity; original URI text need not match. Virtual identities
retain the full URI, including query and fragment.

The `owner` submodule exports operations for the session consumer only. Open
registers the buffer, restores matching retained analysis, and schedules fresh
analysis. Edit takes the registered buffer selected by the incoming URI, owns
the write lock, invalidates eligibility before the supplied
document mutation, and schedules the successor after releasing the lock. Its
mutation callback updates text and client version and must not acquire the same
buffer lock. Close handles retention, cancellation, and removal of publications.
`source-controller-refresh-analysis!` joins active work or starts another attempt
after completion; `source-controller-ensure-analysis!` also reuses a current
completed success. The command queue accepts named operations and worker
completions; it has no arbitrary callback command.

`source-controller.rkt` owns command submission, reply handling, and consumer
dispatch. `source-controller/source-owner.rkt` owns the private controller
record, construction, source transitions, acquisition policy, revision validation,
result routing, and shutdown ordering. The public interface calls semantic owner
operations and re-exports the supported interfaces; it cannot access record
fields or subsystem handles. The owner never imports the public interface.
Lifecycle and acceptance policy stay together in the owner so their revision,
locking, cancellation, and trace-transfer invariants remain local. Subsystems own
private records and collections; none imports or receives the controller:

- `workspace/source-store.rkt`: source authority, revisions, and change signals.
- `disk-reader.rkt`: `Disk-Reader`, ordered per-source disk reads and waiters.
- `analysis-scheduler.rkt`: `Analysis-Scheduler`, shared queued/running attempts,
  FIFO dispatch, capacity, cancellation, and attempt completion.
- `place-runner.rkt`: session-owned bounded Check Syntax place pool, with lazy
  startup, unique job wire identities, hard cancellation, and supervised
  parent restoration threads. The scheduler is its only queue. The worker loop
  in `../analysis-place.rkt` gives each job a fresh analysis namespace and
  custodian and cleans up before replying. The parent restores the completed
  trace and derives its product without repeating Check Syntax.
- `supervised-worker.rkt`: `Supervised-Worker`, resource ownership, timeout and
  premature-exit detection for disk reads, Resyntax, and injected analyzers.
  It suppresses worker stdout and releases resources before reporting a result.
  Cancelling work does not kill its supervisor; a resulting late message is
  rejected by the reader or scheduler.
- `analysis-publisher.rkt`: `Analysis-Publisher`, document analysis installation,
  workspace contributions, diagnostics, and Resyntax publication. Buffer
  operations require the controller-held SafeDoc write lock.
- `closed-analysis-cache.rkt`: `Closed-Analysis-Cache`, bounded completed analysis
  owned while files are closed.
- `completion-event.rkt`: `Completion-Event`, a single-owner first-value
  broadcast event, including when that value is `#f`.

Worker messages are opaque envelopes. Readers and schedulers check attempt
identity before invoking a synchronous acceptance procedure on the consumer:

- Disk acceptance receives the started view and read result, commits source
  knowledge, cancels analysis when that view changes, and returns the observer
  response plus the successor disk view. The reader then answers current waiters
  and starts any queued read using that successor view.
- Analysis acceptance receives the snapshot, kind, completion, parent completion
  (for Resyntax), and result. The controller validates revision and executor
  products, installs or discards analysis, and translates acceptance failures to
  terminal outcomes. The scheduler removes the attempt and resolves its event
  only after acceptance returns. Successful open-buffer analysis may enqueue
  Resyntax during acceptance; dispatch occurs after the command finishes.

These procedures run inline, do not wait for workers, and do not dispatch another
controller command. They are internal acceptance boundaries, not request commands
or a callback registry. Source replacement cancels affected work. The scheduler
also checks queued snapshot eligibility immediately before launch. Availability
lookup and reuse of completed success belong to the controller, not the scheduler.

The controller receives its publication workspace through `#:workspace`.
The default remains the process's `current-workspace`, also used by existing
workspace requests. Injecting a different publication workspace alone does not
create an isolated LSP session; the request layer must use that same workspace.

## Place execution

Production Check Syntax uses a session-owned pool of reusable places, created
lazily up to `#:analysis-slots` (four by default). The scheduler retains the only
FIFO job queue and limits combined Check Syntax and Resyntax attempts to that
capacity. Each pool slot holds at most one place and stays reserved through
parent restoration or observed place retirement; idle workers cannot cause
additional restoration work to bypass the shared attempt bound. Resyntax uses
its separate executor and shares the scheduler capacity.
`#:analyze #f` selects the session's place runner. The ordinary-thread Check
Syntax implementation is no longer a production export: `analyze-source` lives
in `analysis.rkt`'s `test-support` submodule for direct equivalence checks and
the benchmark baseline. `#:analyze` still accepts a procedure as a deterministic
test seam, supervised with the same attempt deadline. Production Resyntax and
disk reads retain their ordinary-thread supervision.
The FIFO scheduler reserves a slot before launch. Its 90-second default deadline
includes startup, collection, serialization, transfer, local document construction,
and restoration. Queue waiting is outside the deadline. Worker stdout is
suppressed from startup, including language loading and extraction.

The runner retains its reservation through restoration. Cancelling queued work
has no worker effect. Cancelling running place work signals its bridge; the
consumer never waits for termination. The bridge kills and observes the place
before releasing physical capacity and sending its terminal message. The scheduler
counts cancelled place reservations against the shared bound until it consumes
that message, including when dispatching Resyntax. When only
parent restoration remains, cancellation stops that task and preserves the
cleaned worker. A successful assignment returns its place for reuse. Logical
settlement only removes the attempt and resolves its event; cancellation
separately stops its worker. Each cancellation handle belongs to one assignment
and cannot kill a newer job. Shutdown
prevents new dispatch and asynchronously retires both active and idle places.
Bridges and retirement threads share the runner's owning custodian, so disposing
the consumer's custodian cannot interrupt cleanup of its places.

Replies contain a job ID, success plus the completed trace,
or normalized failure text. Job IDs increase across all slots and replacements
within the runner, and each reply is read from its assigned place channel.
Read/expansion errors remain completed products with
diagnostics. Unexpected execution, serialization, protocol, and restoration
errors fail the attempt. A worker failure reply retires the place even after
job cleanup: the exception may have left place-wide state inconsistent. A
parent restoration failure preserves the worker that returned a normal result.
A reply queued alongside worker death is considered before reporting premature
exit. Cancelled attempts cannot accept late replies;
source revision is checked again at acceptance before installation, publication,
and shared completion.

`tests/lsp/place-runner-test.rkt` uses real places and explicit assignment barriers
for reuse, resource isolation, cancellation, timeout, startup failure, worker death,
replacement, restoration failure, and late-reply rejection. Pool coverage checks
concurrent assignments, shared FIFO capacity with Resyntax, cancellation during
restoration, concurrent startup shutdown, and retained URI aliases through the
controller with another place assignment still running. The trace conversion
contract and fidelity coverage remain in `doclib/doc-trace.rkt` and
`tests/lib/trace-place-test.rkt`.

## Measurements and capacity

[The benchmark and measured results](../bench/place-runner-results.md) compare
one, two, and four slots with the ordinary-thread test baseline. They cover
cold and warm batches, fresh and warm rapid-edit sessions, controller command
latency, resident memory, and isolated conversion/transport probes. The report
defines their boundaries; controller command latency is not full LSP request
latency, and companion phase samples do not decompose production batch totals.

Keep the four-slot default and lazy startup. The measured warm heavy batch
benefits from four places, while first use and resident memory cost more than
ordinary threads. These workload-specific samples do not establish a better
universal capacity or justify allocating warm workers before demand. Hard
cancellation still discards the executing place, so repeated cancellation can
lose reuse. No prewarming, caching, payload optimization, or cooperative recovery
is part of this migration.

## Available results and fresh completion

`SafeDoc-available` holds a completed product and the snapshot it analyzed.
`SafeDoc-analysis-completion` is the broadcast event for the attempt being
joined—either a newly scheduled one or a reused completed success's event.
Available analysis may answer queries while that event is pending. Restoring a
product never completes that event or starts Resyntax.

Only queued and running attempts occupy the scheduler's private attempt table. Every terminal
attempt is removed. An accepted available result retains its completion event,
so repeated acquisitions can reuse success without retaining the job. Resyntax
acceptance compares its parent completion with the installed result's completion;
same-revision retries cannot accept an old attempt's findings.

Exact editor queries require available analysis under the current source
revision; otherwise they wait for fresh completion. Fast local queries keep
their adjusted-trace fallbacks. An `errors` outcome publishes current
diagnostics and clears exact availability. A `failed` outcome or timeout also
clears availability. The old trace may still support fast fallbacks, but cannot
be retained as a completed success. Non-success attempts may be retried.

## Close, restore, and retention

Closing an eligible file detaches its trace from the old Doc, moves its completed
product into closed retention, and clears the SafeDoc projections. Close also
cancels pending jobs, signals the old revision, removes the active workspace
contribution, and publishes empty diagnostics. Edits clear eligibility before
adjusting the trace, so an unfinished edit cannot enter retained storage.

Opening creates a new source revision. Exact text equality permits transferring
a retained trace into the new buffer, installing its contribution, and publishing
its Check Syntax diagnostics to the incoming URI. The restored available result
has no fresh completion in that lifetime. Every open schedules fresh analysis;
old completion events and Resyntax findings are never restored.

Closed-target definition and hover requests observe saved text before acquisition.
They may use matching retained facts while fresh analysis runs, then perform final
view validation. Text equality does not establish dependency freshness:
retained dependency information can remain stale until fresh analysis replaces it.
Neither current revisions nor fresh completion track external dependency changes.

Only closed files occupy retention: at most 64 entries by default, configurable
with `#:closed-analysis-limit`. Results are keyed by source ID in a private hash. Insertion
and successful text-matching lookups mark an entry most recently used; misses and
text mismatches leave recency unchanged. Insertion beyond capacity evicts the
least recently used result without changing source authority or live documents.
Reopening removes the entry before checking its text and transferring its trace,
so retained storage cannot keep a trace that a live document starts modifying.
A zero limit disables retention; evicted sources must analyze again. Existing
waiters retain their outcomes.
Virtual sources never enter retention. Shutdown stops work and clears retention.

`tests/lsp/source-controller-test.rkt` covers ordered observation through the
production consumer. `analysis-controller-test.rkt` covers attempt identity,
bounds, acceptance, available facts, cancellation, and URI preservation.
`closed-analysis-test.rkt` covers restoration, dependency changes, and eviction.
`closed-analysis-cache-test.rkt`, `disk-reader-test.rkt`,
`analysis-scheduler-test.rkt`, and `supervised-worker-test.rkt` exercise subsystem
interfaces, ownership, and completion ordering without exposing mutable records.

Integration tests use the production consumer and its named commands or LSP
messages. Tests that must order individual transitions or replay delayed worker
messages own an isolated controller on the test thread and call the same owner
operations directly. Assertions run on the test thread; command failures are
values only where the test explicitly exercises that production contract.
