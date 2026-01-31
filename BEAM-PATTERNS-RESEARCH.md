# BEAM Design Patterns and Best Practices - Research Document

**Researched:** 2026-01-31
**Domain:** BEAM/Erlang/OTP concurrency patterns for Rust implementation
**Confidence:** HIGH (verified with official docs and authoritative sources)

## Executive Summary

This document captures comprehensive research on BEAM (Erlang VM) design patterns and best practices, with focus on patterns that can be implemented in Rust with Tokio. The BEAM's approach to fault-tolerant, concurrent systems has been refined over 30+ years in telecom (Ericsson) where availability requirements exceed 99.999%.

**Core insight:** The BEAM's power comes not from any single feature, but from the combination of:
1. Lightweight isolated processes
2. Asynchronous message passing
3. Supervision hierarchies
4. "Let it crash" philosophy

---

## 1. Core BEAM Concepts

### 1.1 Process Model

**What makes BEAM processes special:**

| Property | BEAM Processes | OS Threads |
|----------|---------------|------------|
| Memory | Isolated heap per process | Shared memory |
| Creation cost | ~2KB, microseconds | ~1MB stack, milliseconds |
| Scheduling | Preemptive, reduction-based | OS-managed |
| GC | Per-process, no stop-the-world | Global GC pauses |
| Communication | Message passing only | Shared state + locks |

**Key characteristics:**

1. **Complete isolation** - No shared memory between processes. A crash in one process cannot corrupt another's state.

2. **Lightweight** - Can spawn millions of processes. Use processes liberally for isolation, not sparingly for performance.

3. **Preemptive scheduling** - Uses "reductions" (roughly ~function calls) instead of time slices. A process executing a tight loop cannot starve others.

4. **Per-process garbage collection** - GC runs only when needed on individual processes. No "stop the world" pauses.

**Rust/Tokio mapping:**
- BEAM process -> Tokio task + owned state
- Isolation achieved via Rust ownership (no shared mutable state)
- Message passing via `mpsc` channels

### 1.2 Message Passing

**Mailbox semantics:**

1. **Asynchronous sends** - `send` (Erlang's `!` operator) never blocks. Messages are copied to receiver's mailbox.

2. **Selective receive** - Can pattern match on messages in mailbox, processing them out of order.

3. **FIFO ordering** - Messages from a single sender arrive in order. No global ordering across senders.

4. **Unbounded mailboxes** - Default mailboxes grow without limit (potential memory issue).

**Selective receive mechanism:**
```erlang
receive
    {priority, Msg} -> handle_priority(Msg);
    {normal, Msg}   -> handle_normal(Msg)
after 5000 ->
    timeout
end
```

If no match found, message goes to "save queue" and next message is tried. When match found, save queue messages return to mailbox.

**Performance warning:** Selective receive with large mailboxes is O(n) per receive. Use reference-based optimization:
```erlang
Ref = make_ref(),
send(Pid, {Ref, request}),
receive
    {Ref, Response} -> Response  % Compiler optimizes: skips messages before Ref creation
after 5000 ->
    timeout
end
```

**Timeout patterns:**
- `after N` - timeout in milliseconds
- `after 0` - check mailbox, return immediately if empty (used for flushing)
- `after infinity` - wait forever (equivalent to no timeout)

**Rust/Tokio mapping:**
- Mailbox -> `mpsc::Receiver<T>`
- Selective receive -> enum variants with `match`
- Timeouts -> `tokio::time::timeout()` or `tokio::select!` with `sleep`

### 1.3 Links and Monitors

**Links (bidirectional):**
- Created with `link(Pid)` or `spawn_link`
- When one linked process dies, all linked processes receive exit signal
- Exit signals cause receiving process to die (unless trapping exits)
- Only one link can exist between any two processes
- **Use case:** Supervisor trees, tight coupling where failure should propagate

**Monitors (unidirectional):**
- Created with `monitor(process, Pid)`
- Returns a reference
- When monitored process dies, monitor receives `{'DOWN', Ref, process, Pid, Reason}` message
- Multiple monitors can exist on same process
- Monitored process is unaware of monitors
- **Use case:** Loose observation, libraries, request-reply patterns

| Feature | Links | Monitors |
|---------|-------|----------|
| Direction | Bidirectional | Unidirectional |
| Stackable | No (one per pair) | Yes (multiple independent) |
| Signal type | Exit signal | DOWN message |
| Effect on monitored | Mutual termination | None |
| Primary use | Supervision, tight coupling | Observation, request-reply |

**Rust/Tokio mapping:**
- Links -> `tokio::select!` with `JoinHandle`
- Monitors -> Watch channels or `JoinHandle` polling
- Exit trapping -> Error handling in message loop

### 1.4 Process Registry and Naming

**Built-in registry:**
- `register(atom, Pid)` - Register process with local name
- `whereis(atom)` - Look up Pid by name
- Only atoms allowed as names
- Local to node

**Gproc (extended registry):**
- Keys: `{Type, Scope, Key}` where:
  - Type: `n` (name), `p` (property), `c` (counter), `a` (aggregate counter)
  - Scope: `l` (local), `g` (global/cluster-wide)
  - Key: any Erlang term
- Properties: non-unique, multiple processes can have same property
- Counters: per-process with automatic aggregation
- Performance: <40 microseconds for local registration

**Rust/Tokio mapping:**
- Registry -> `DashMap<K, ActorHandle>` or similar concurrent map
- Global -> Distributed via network protocol
- Names -> Any hashable type (not just atoms)

---

## 2. OTP Patterns

### 2.1 GenServer (Generic Server)

The most common OTP behavior. Encapsulates:
- State management
- Synchronous and asynchronous request handling
- Standard callbacks for lifecycle

**Message types:**

| Type | Function | Callback | Semantics |
|------|----------|----------|-----------|
| `call` | Synchronous | `handle_call/3` | Blocks caller until reply |
| `cast` | Asynchronous | `handle_cast/2` | Fire-and-forget |
| `info` | Other messages | `handle_info/2` | `send`, monitors, timers |

**Call semantics:**
```erlang
% Client
gen_server:call(Server, Request) -> Response

% Server callback
handle_call(Request, From, State) ->
    {reply, Response, NewState}
```

**Cast semantics:**
```erlang
% Client
gen_server:cast(Server, Message)  % Returns ok immediately

% Server callback
handle_cast(Message, State) ->
    {noreply, NewState}
```

**Best practices:**

1. **Keep callbacks short** - Long operations should spawn separate tasks
2. **Prefer call over cast** - Cast provides no delivery guarantee
3. **Pattern match exhaustively** - Add catch-all clauses to prevent crashes
4. **Use timeouts** - Prevent hanging on unresponsive servers
5. **Don't use GenServer for pure functions** - Anti-pattern that creates bottlenecks

**Anti-pattern example:**
```erlang
% BAD: Calculator as GenServer (bottleneck, unnecessary process)
gen_server:call(Calculator, {add, 1, 2})

% GOOD: Direct function call
Calculator:add(1, 2)
```

**When to use processes:**
> "Use processes only to model runtime properties, such as mutable state, concurrency and failures, never for code organization."

**Rust/Tokio implementation:**
```rust
// Message enum
enum ActorMessage {
    // Call pattern: includes response channel
    GetState { respond_to: oneshot::Sender<State> },

    // Cast pattern: no response
    UpdateState { new_value: i32 },
}

// Actor struct
struct MyActor {
    receiver: mpsc::Receiver<ActorMessage>,
    state: State,
}

impl MyActor {
    async fn run(&mut self) {
        while let Some(msg) = self.receiver.recv().await {
            match msg {
                ActorMessage::GetState { respond_to } => {
                    let _ = respond_to.send(self.state.clone());
                }
                ActorMessage::UpdateState { new_value } => {
                    self.state.value = new_value;
                }
            }
        }
    }
}

// Handle (client interface)
#[derive(Clone)]
struct ActorHandle {
    sender: mpsc::Sender<ActorMessage>,
}

impl ActorHandle {
    // Call (sync request-reply)
    async fn get_state(&self) -> Result<State, Error> {
        let (tx, rx) = oneshot::channel();
        self.sender.send(ActorMessage::GetState { respond_to: tx }).await?;
        rx.await.map_err(|_| Error::ActorDied)
    }

    // Cast (async fire-and-forget)
    async fn update_state(&self, value: i32) -> Result<(), Error> {
        self.sender.send(ActorMessage::UpdateState { new_value: value }).await?;
        Ok(())
    }
}
```

### 2.2 Supervisor

**Purpose:** Start, monitor, and restart child processes according to a strategy.

**Restart strategies:**

| Strategy | Behavior | Use When |
|----------|----------|----------|
| `one_for_one` | Restart only failed child | Children are independent |
| `one_for_all` | Restart all children | Children are mutually dependent |
| `rest_for_one` | Restart failed + all started after it | Children have ordered dependencies |
| `simple_one_for_one` | Dynamic pool of identical children | Worker pools |

**Restart intensity:**
- `MaxRestarts` within `MaxSeconds` before supervisor gives up
- If exceeded, supervisor terminates (escalates to its supervisor)
- Default: 1 restart per 5 seconds

**Child specification:**
```erlang
#{
    id => worker_1,
    start => {Module, start_link, [Args]},
    restart => permanent | temporary | transient,
    shutdown => 5000 | brutal_kill | infinity,
    type => worker | supervisor
}
```

**Restart types:**
- `permanent` - Always restart
- `temporary` - Never restart
- `transient` - Restart only if abnormal termination

**Shutdown strategies:**
- Integer (ms) - Graceful shutdown timeout
- `brutal_kill` - Immediate `kill` signal
- `infinity` - Wait forever (use for supervisors)

**Design principles:**

1. **Critical components near root** - Stable, essential services high in tree
2. **Volatile components as leaves** - Risky, experimental code at edges
3. **Structure encodes recovery** - Tree shape defines failure domains

**Example hierarchy:**
```
Application (root supervisor)
├── Database Pool (one_for_one, permanent)
│   └── Connection Workers
├── Business Logic (one_for_all, permanent)
│   ├── Cache
│   └── Processor
└── External Integrations (one_for_one, transient)
    ├── Payment Gateway
    └── Email Service
```

**Rust/Tokio mapping:**
```rust
enum SupervisorStrategy {
    OneForOne,
    OneForAll,
    RestForOne,
}

struct SupervisorConfig {
    strategy: SupervisorStrategy,
    max_restarts: u32,
    max_seconds: Duration,
}

struct Supervisor {
    children: Vec<ChildSpec>,
    config: SupervisorConfig,
    handles: Vec<JoinHandle<()>>,
}

impl Supervisor {
    async fn run(&mut self) {
        loop {
            tokio::select! {
                // Monitor children, restart according to strategy
            }
        }
    }
}
```

### 2.3 gen_statem (State Machine)

**When to use over GenServer:**
- Logic naturally expressed as state machine
- Need co-located code per state
- Need postponing events (selective receive substitute)
- Need state enter callbacks
- Need multiple timeout types

**Callback modes:**

| Mode | State Type | Callback | Best For |
|------|-----------|----------|----------|
| `state_functions` | Atom only | `StateName/3` | Clear state diagrams |
| `handle_event_function` | Any term | `handle_event/4` | Complex state logic |

**State functions mode:**
```erlang
% Each state is a function
locked(cast, {button, N}, Data) ->
    case analyze(N, Data) of
        ok -> {next_state, open, Data};
        error -> {keep_state, Data}
    end.

open(cast, lock, Data) ->
    {next_state, locked, Data}.
```

**Key features:**

1. **Postpone** - Save event for later state
2. **State enter calls** - Run code on state entry
3. **Multiple timeouts** - State timeout, event timeout, generic timeouts

**Event types:** `call`, `cast`, `info`, `timeout`, `internal`

**Rust/Tokio mapping:**
```rust
enum State {
    Locked { code: Vec<u8> },
    Open { timer: Instant },
}

enum Event {
    Button(u8),
    Lock,
    Timeout,
}

impl StateMachine {
    fn handle(&mut self, event: Event) -> Transition {
        match (&self.state, event) {
            (State::Locked { code }, Event::Button(n)) => {
                // State-specific handling
            }
            (State::Open { .. }, Event::Lock) => {
                Transition::To(State::Locked { code: vec![] })
            }
            _ => Transition::Keep,
        }
    }
}
```

### 2.4 Application Behavior

**Purpose:** Package supervision tree as deployable unit with:
- Startup/shutdown lifecycle
- Dependencies on other applications
- Configuration

**Lifecycle callbacks:**
```erlang
start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}
prep_stop(State) -> NewState  % Optional, called before shutdown
stop(State) -> ok
```

**Application types:**
- `permanent` - Termination crashes entire node
- `transient` - Abnormal termination crashes node
- `temporary` - Termination only logged

**Dependencies:**
```erlang
{applications, [kernel, stdlib, crypto, my_lib]}
```

Applications started in dependency order, stopped in reverse.

**Start phases:** Optional ordered initialization steps for complex startup.

### 2.5 Process Pools

**Poolboy pattern:**
- Fixed pool of pre-spawned workers
- Checkout/checkin or transaction API
- Overflow workers for burst capacity

**Configuration:**
```erlang
[
    {size, 10},          % Permanent workers
    {max_overflow, 20},  % Temporary overflow
    {strategy, lifo}     % lifo or fifo
]
```

**Usage patterns:**

1. **Manual checkout:**
```erlang
Worker = poolboy:checkout(Pool),
try
    gen_server:call(Worker, Request)
after
    poolboy:checkin(Pool, Worker)
end
```

2. **Transaction (safer):**
```erlang
poolboy:transaction(Pool, fun(Worker) ->
    gen_server:call(Worker, Request)
end)
```

**Worker failure handling:**
- Monitor on checkout
- Automatic return on client crash
- Restart dead workers to maintain pool size

**Rust/Tokio mapping:**
- `deadpool` or similar crate
- Or custom with `Semaphore` + `VecDeque<ActorHandle>`

---

## 3. Fault Tolerance Patterns

### 3.1 "Let It Crash" Philosophy

**The misunderstanding:** "Let it crash" does NOT mean:
- Ignore all errors
- Write no error handling code
- Let failures propagate randomly

**What it actually means:**

1. **Code for the happy path** - Don't litter code with defensive checks for rare errors
2. **Crashes are controlled** - Supervisor handles recovery, not scattered try/catch
3. **Crash to known state** - Restart returns process to clean initial state
4. **Bug classification matters:**
   - **Bohrbugs** (repeatable): Restarting won't help, fix the code
   - **Heisenbugs** (transient): Restarting usually works (131/132 production errors)

**Why it works:**

1. **Process isolation** - Crash can't corrupt other processes
2. **Supervision trees** - Recovery strategy encoded in structure
3. **Clean restart** - Returns to known good state
4. **Transient bugs** - Most production errors are heisenbugs that don't recur

**Joe Armstrong's insight:**
> "The reason is simple: memory protection and the fact that the kernel controls the memory space."

**Practical application:**
```
Instead of:
try
    risky_operation()
catch
    handle_all_possible_errors()
end

Do:
% Let it crash, supervisor restarts with clean state
risky_operation()
```

**Rust adaptation:**
- Use `Result<T, E>` for expected errors (invalid input, network timeout)
- Let unexpected errors (panics) propagate to supervisor
- Supervisor restarts actor with fresh state
- Log failures for later analysis

### 3.2 Error Kernel Pattern

**Concept:** Minimal critical core that must never fail, surrounded by "expendable" code.

**Like an OS kernel:**
- Kernel assumed correct, controls resources
- User processes can fail without affecting kernel
- Kernel can clean up failed processes

**In Erlang:**
- Error kernel = root supervisor + critical services
- Keep kernel as small as possible
- All "risky" code at leaves of supervision tree

**Design principles:**

1. **Identify what's critical** - What absolutely cannot fail?
2. **Minimize critical surface** - Less code = fewer bugs
3. **Push risk to edges** - External calls, user input, parsing at leaves
4. **Data flows toward stability** - Unreliable edges write to reliable core

**Example structure:**
```
Error Kernel (must not fail)
├── State Storage (persistent, validated)
└── Core Business Logic (well-tested)

Expendable (can crash and restart)
├── External API Clients
├── User Request Handlers
└── Background Jobs
```

### 3.3 Circuit Breakers

**Purpose:** Prevent cascade failures when calling external services.

**States:**
1. **Closed** - Normal operation, requests pass through
2. **Open** - Failures exceeded threshold, requests immediately rejected
3. **Half-open** - Testing if service recovered

**Fuse library (Erlang):**
```erlang
% Install fuse
fuse:install(my_service, {{standard, 10, 60000}, {reset, 30000}})
% 10 failures in 60 seconds -> open for 30 seconds

% Check before calling
case fuse:ask(my_service, sync) of
    ok -> call_service();
    blown -> {error, service_unavailable}
end

% Record failure
fuse:melt(my_service)
```

**Performance:** 2.1M queries/second, sub-microsecond lookup

**Rust implementation considerations:**
- Use `tokio::sync::RwLock` for state
- Atomic counters for failure tracking
- Separate task for half-open probe

### 3.4 Backpressure and Load Shedding

**The problem:** Unbounded mailboxes -> memory exhaustion

**Backpressure (slow down producers):**
- Use bounded channels
- `gen_server:call` (sync) naturally provides backpressure
- Returns control to caller only when processed

**Load shedding (drop excess work):**
- When queue reaches threshold, reject new requests
- Better to serve some requests well than all requests poorly

**Strategies:**

1. **Bounded mailboxes** (OTP 19+):
```erlang
% Process dies if heap exceeds limit
spawn_opt(fun() -> ... end, [{max_heap_size, 1000000}])
```

2. **External buffer (pobox):**
- Separate process manages queue
- Can drop oldest, newest, or random messages
- Provides batching

3. **Explicit queuing:**
- Check queue length before accepting work
- Return `{error, overloaded}` when full

**Rust/Tokio mapping:**
- Bounded channels: `mpsc::channel(capacity)`
- `try_send` for non-blocking with backpressure signal
- `Semaphore` for admission control
- Explicit queue length checks

### 3.5 Graceful Degradation

**Principle:** Partial service is better than no service.

**Strategies:**

1. **Feature flags** - Disable non-essential features under load
2. **Cached responses** - Serve stale data when backend unavailable
3. **Simplified processing** - Skip expensive steps when overloaded
4. **Priority queues** - Process critical work first

**Testing requirement:**
> "If you haven't load tested your service to the point where it breaks, and far beyond the point where it breaks, you should assume that the service will fail in the least desirable way possible."

---

## 4. Concurrency Patterns

### 4.1 Task Parallelism

**Pattern:** Distribute independent work across many processes.

```erlang
Results = [spawn_link(fun() -> Parent ! {self(), work(Item)} end)
           || Item <- Items],
[receive {Pid, Result} -> Result end || Pid <- Results]
```

**BEAM scheduler handles:**
- Work stealing between cores
- Preemptive scheduling (no starvation)
- Automatic load balancing

**Best practice:** Single Erlang instance across cores (intra-node work stealing) rather than multiple instances (no sharing).

### 4.2 Pipeline Processing

**Pattern:** Chain of processes, each performing one transformation.

```
Input -> Stage1 -> Stage2 -> Stage3 -> Output
```

**Benefits:**
- Clear separation of concerns
- Independent scaling per stage
- Natural backpressure (if using bounded queues)

**Rust/Tokio:**
```rust
// Each stage is an actor
let stage1 = Stage1::new(stage2_handle);
let stage2 = Stage2::new(stage3_handle);
let stage3 = Stage3::new(output_handle);
```

### 4.3 Fan-Out / Fan-In

**Fan-out:** Distribute work to multiple workers
```erlang
[Worker ! {work, Item} || Worker <- Workers, Item <- Items]
```

**Fan-in:** Collect results from multiple workers
```erlang
[receive {result, R} -> R end || _ <- Items]
```

**Rust/Tokio:**
```rust
// Fan-out
let handles: Vec<_> = items.iter()
    .map(|item| tokio::spawn(process(item.clone())))
    .collect();

// Fan-in
let results: Vec<_> = futures::future::join_all(handles).await;
```

### 4.4 Work Stealing

**BEAM's approach:**
- One scheduler per core
- Each scheduler has run queue
- Periodically balance across schedulers
- Migration when one scheduler overloaded

**Key insight:** Work stealing is automatic in BEAM. In Rust, consider:
- `tokio` work-stealing runtime (default)
- Or explicit work-stealing with `crossbeam-deque`

---

## 5. Testing Actors

### 5.1 Property-Based Testing

**PropEr/QuickCheck for Erlang:**
- Generate random inputs
- Check properties hold for all inputs
- Shrink failing cases to minimal example

**Stateful testing:**
- Model expected state transitions
- Generate random command sequences
- Verify actual state matches model

**Example properties:**
```erlang
prop_counter() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            {H, S, Res} = run_commands(?MODULE, Cmds),
            Res =:= ok
        end).
```

**Parallel testing:**
- Run commands in parallel
- Check linearizability
- Find race conditions

### 5.2 Testing Guidelines

**Unit testing actors:**
1. Test message handling in isolation
2. Mock dependencies via injected handles
3. Use deterministic message ordering

**Integration testing:**
```erlang
% Common Test
init_per_testcase(_, Config) ->
    {ok, Pid} = my_actor:start_link(),
    [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
    Pid = ?config(pid, Config),
    gen_server:stop(Pid).
```

**Important:** `init_per_testcase` runs in same process as test. `init_per_group` runs in different process (careful with links/ETS).

### 5.3 Rust Testing Approach

```rust
#[tokio::test]
async fn test_actor_message_handling() {
    let handle = MyActor::spawn();

    // Send message
    handle.update_state(42).await.unwrap();

    // Verify state
    let state = handle.get_state().await.unwrap();
    assert_eq!(state.value, 42);

    // Test shutdown
    drop(handle);
    // Actor task should terminate gracefully
}

#[tokio::test]
async fn test_supervisor_restart() {
    let supervisor = Supervisor::spawn(SupervisorConfig {
        strategy: OneForOne,
        max_restarts: 3,
        max_seconds: Duration::from_secs(5),
    });

    // Cause child to crash
    supervisor.child(0).crash().await;

    // Verify restart
    tokio::time::sleep(Duration::from_millis(100)).await;
    assert!(supervisor.child(0).is_running().await);
}
```

---

## 6. Best Practices Summary

### Process Design Guidelines

1. **One process per concurrent activity** - Not one process per object
2. **Processes for runtime concerns** - State, concurrency, failure isolation
3. **Functions for logic** - Pure computation doesn't need processes
4. **Keep process state minimal** - Large state = expensive crashes
5. **Prefer call over cast** - Get confirmation, enable backpressure

### Supervision Design

1. **Structure encodes recovery** - Tree shape defines failure domains
2. **Critical code near root** - Less likely to be restarted
3. **Risky code at leaves** - Can crash without affecting core
4. **Choose strategy carefully:**
   - `one_for_one` - Independent processes
   - `one_for_all` - Tightly coupled (shared resource)
   - `rest_for_one` - Ordered dependencies

### Message Design

1. **Include response channel in message** - For request-reply
2. **Use references for correlation** - Match responses to requests
3. **Handle unknown messages** - Prevent mailbox pollution crashes
4. **Consider message size** - Large messages are copied

### Error Handling

1. **Let it crash** - For unexpected errors
2. **Handle expected errors** - Invalid input, timeouts, etc.
3. **Return errors, don't throw** - For recoverable conditions
4. **Log and restart** - For transient failures

### Performance

1. **Avoid selective receive on large mailboxes** - O(n) scan
2. **Use bounded channels** - Prevent memory exhaustion
3. **Keep callbacks short** - Long work in separate task
4. **Profile message patterns** - Find bottleneck processes

---

## 7. Rust/Tokio Implementation Checklist

### Actor Implementation

- [ ] Separate actor struct from handle struct
- [ ] Message enum with variants for each operation
- [ ] Request-reply uses `oneshot` channel in message
- [ ] Handle is `Clone` (uses `mpsc::Sender`)
- [ ] Actor loop: `while let Some(msg) = receiver.recv().await`
- [ ] Graceful shutdown when all senders dropped

### Supervisor Implementation

- [ ] Track child handles (`JoinHandle<()>`)
- [ ] Monitor children with `tokio::select!`
- [ ] Implement restart strategy
- [ ] Track restart frequency
- [ ] Escalate if max restarts exceeded

### Error Handling

- [ ] `Result<T, Error>` for expected failures
- [ ] Panics caught at actor boundary
- [ ] Supervisor restarts on panic
- [ ] Errors logged with context

### Testing

- [ ] Unit tests for message handling
- [ ] Integration tests for actor lifecycle
- [ ] Property tests for state machines
- [ ] Load tests for backpressure behavior

---

## Sources

### Primary (HIGH confidence)

- [Erlang OTP Documentation - Supervisor Behaviour](https://www.erlang.org/doc/system/sup_princ.html)
- [Erlang OTP Documentation - gen_statem](https://www.erlang.org/doc/system/statem.html)
- [Erlang OTP Documentation - Applications](https://www.erlang.org/doc/system/applications.html)
- [Elixir GenServer Documentation](https://hexdocs.pm/elixir/GenServer.html)
- [Joe Armstrong's PhD Thesis](https://erlang.org/download/armstrong_thesis_2003.pdf)
- [The Zen of Erlang - Fred Hebert](https://ferd.ca/the-zen-of-erlang.html)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)

### Secondary (MEDIUM confidence)

- [Actors with Tokio - Alice Ryhl](https://ryhl.io/blog/actors-with-tokio/)
- [Error Kernels - Jesper L. Andersen](https://medium.com/@jlouis666/error-kernels-9ad991abd)
- [Handling Overload - Fred Hebert](https://ferd.ca/handling-overload.html)
- [Designing for Scalability with Erlang/OTP (O'Reilly)](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/)
- [Gproc - Extended Process Registry](https://github.com/uwiger/gproc)
- [Poolboy - Worker Pool](https://github.com/devinus/poolboy)
- [Fuse - Circuit Breaker](https://github.com/jlouis/fuse)

### Erlang Solutions Blog Posts

- [The BEAM - Erlang's Virtual Machine](https://www.erlang-solutions.com/blog/the-beam-erlangs-virtual-machine/)
- [BEAM vs JVM](https://www.erlang-solutions.com/blog/beam-jvm-virtual-machines-comparing-and-contrasting/)

---

## Metadata

**Confidence breakdown:**
- Core BEAM concepts: HIGH - Official docs + established sources
- OTP patterns: HIGH - Official docs + books
- Fault tolerance patterns: HIGH - Fred Hebert + established patterns
- Concurrency patterns: MEDIUM - Community patterns, well-documented
- Rust mapping: MEDIUM - Alice Ryhl + community practice

**Research date:** 2026-01-31
**Valid until:** Patterns are stable; Rust ecosystem may evolve
