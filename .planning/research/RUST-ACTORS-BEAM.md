# Research: Rust Actor Systems and BEAM-like Patterns

**Researched:** 2026-01-31
**Domain:** Actor systems, fault tolerance, supervision
**Confidence:** HIGH (multiple verified sources, production examples)

## Executive Summary

Rust has a mature actor ecosystem with several frameworks providing BEAM-inspired patterns. For gleam-factory, **ractor** is the recommended choice due to its Erlang gen_server design, production validation at Meta/WhatsApp, Tokio integration, and supervision trees. However, for simpler use cases, **tokio-graceful-shutdown** provides OTP-like subsystem hierarchies without full actor overhead.

**Primary recommendation:** Use **ractor** for actor-based concurrency with supervision, or **tokio-graceful-shutdown** for simpler hierarchical subsystem management.

## Rust Actor Framework Comparison

### Overview Matrix

| Framework | Runtime | Distribution | Supervision | Production Use | Erlang Fidelity |
|-----------|---------|--------------|-------------|----------------|-----------------|
| **ractor** | Tokio/async-std | Yes (ractor_cluster) | Yes | Meta/WhatsApp | HIGH (gen_server) |
| **Kameo** | Tokio | Yes | Yes | Community | MEDIUM |
| **Bastion** | Custom | Yes (NUMA-aware) | Yes | Community | MEDIUM |
| **Actix** | Actix (Tokio-based) | Via extensions | Limited | actix-web | LOW |
| **Lunatic** | Custom WASM | Yes | Yes | YC-backed startup | HIGH (preemptive) |
| **Xtra** | Multi-runtime | No | No | Community | LOW |

### Detailed Analysis

#### Ractor (Recommended)

**Why it stands out:**
- Explicitly modeled on Erlang's `gen_server`
- Production-proven at Meta/WhatsApp for distributed overload protection
- Native Tokio integration (no separate runtime)
- Built-in supervision trees
- Message types: `cast` (fire-and-forget) and `call` (request-reply) matching Erlang semantics
- Signal system with priority (Kill signals interrupt processing)

**Key features:**
```rust
// Erlang-style actor definition
impl Actor for MyActor {
    type Msg = MyMessage;
    type State = MyState;
    type Arguments = ();

    async fn pre_start(&self, myself: ActorRef<Self::Msg>, _: ()) -> Result<Self::State, ActorProcessingErr> {
        // Initialization
    }

    async fn handle(&self, myself: ActorRef<Self::Msg>, message: Self::Msg, state: &mut Self::State) -> Result<(), ActorProcessingErr> {
        // Message handling
    }
}
```

**Distribution:**
- `ractor_cluster` provides Erlang EPMD-like node discovery
- Magic cookie authentication (following Erlang spec)
- Not production-ready but relatively stable

**Limitations:**
- No preemptive scheduling (Tokio is cooperative)
- Clustering not production-ready yet

#### Kameo

**Strengths:**
- Fault-tolerant with supervision strategies
- Backpressure management via bounded/unbounded channels
- Actor linking for error propagation
- Lifecycle hooks: `on_start`, `on_panic`, `on_stop`

**Best for:** Web servers, data processors, real-time systems

**Weaker than ractor for:** Erlang-style semantics, production validation

#### Bastion

**Strengths:**
- Most complete BEAM-inspired feature set
- NUMA-aware, cache-affine executor
- io_uring support
- One-for-one and all-for-one supervision strategies
- LightProc (Erlang-like process abstraction)

**Limitations:**
- Custom runtime (not pure Tokio)
- Heavier weight than ractor
- Less active development recently

**Best for:** High-performance distributed systems needing NUMA awareness

#### Lunatic (Special Case)

**Unique value:** Actual preemptive scheduling via WASM

**How it works:**
- Compiles Rust to WASM
- Runs on custom Wasmtime-based runtime
- Each process is isolated WASM instance (true memory isolation)
- Can spawn 300k+ tasks/second
- Supervisor-based fault tolerance

**Trade-offs:**
- Requires WASM compilation target
- Different deployment model
- Less Rust ecosystem compatibility

**Best for:** When true BEAM-like isolation is required

#### Actix

**Status:** Mature but limited

**What it provides:**
- Fast message passing
- Good for actix-web integration
- Large community

**What it lacks:**
- Distribution (requires external crates)
- Proper supervision trees
- Not modeled on Erlang semantics

## Tokio-Native Patterns

### Supervision Without Full Actors

For projects that don't need full actor semantics, **tokio-graceful-shutdown** provides OTP-like subsystem trees.

```rust
use tokio_graceful_shutdown::{SubsystemBuilder, SubsystemHandle, Toplevel};

async fn subsystem_a(subsys: SubsystemHandle) -> Result<(), Error> {
    // Start nested subsystems
    subsys.start(SubsystemBuilder::new("child", child_subsystem));

    // React to shutdown
    subsys.on_shutdown_requested().await;
    // Cleanup logic
    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    Toplevel::new(|s| async move {
        s.start(SubsystemBuilder::new("subsystem_a", subsystem_a));
    })
    .catch_signals()
    .handle_shutdown_requests(Duration::from_secs(10))
    .await
}
```

**Benefits:**
- Hierarchical subsystem tree (like OTP application tree)
- Automatic signal handling (SIGINT/SIGTERM)
- Shutdown propagation through tree
- Timeout-based forced shutdown
- No actor overhead

### Graceful Shutdown Patterns

Tokio's recommended approach uses `CancellationToken`:

```rust
use tokio_util::sync::CancellationToken;

let token = CancellationToken::new();

// Clone for each task
let task_token = token.clone();
tokio::spawn(async move {
    loop {
        tokio::select! {
            _ = task_token.cancelled() => {
                // Cleanup
                break;
            }
            _ = do_work() => {}
        }
    }
});

// Trigger shutdown
token.cancel();
```

### Tower Middleware for Resilience

**tower-resilience** provides production-ready patterns:

| Pattern | Purpose | Use Case |
|---------|---------|----------|
| Circuit Breaker | Stop cascading failures | External service calls |
| Bulkhead | Resource isolation | Prevent runaway tasks |
| Retry | Intelligent retry with backoff | Transient failures |
| Rate Limiter | Request throttling | API protection |
| Timeout | Deadline enforcement | Hung operations |
| Fallback | Graceful degradation | Reduced functionality |

**Composition pattern:**
```rust
ServiceBuilder::new()
    .layer(TimeoutLayer::new(Duration::from_secs(10)))
    .layer(CircuitBreakerLayer::new())
    .layer(RetryLayer::new())
    .service(inner_service)
```

## Challenges: BEAM vs Rust

### Fundamental Differences

| BEAM Property | Rust Reality | Mitigation |
|---------------|--------------|------------|
| Preemptive scheduling | Cooperative (Tokio) | Lunatic (WASM), or careful async design |
| Process isolation | Shared memory | Type system, careful ownership |
| Hot code reloading | Not possible | Blue-green deploys, feature flags |
| Immutable by default | Ownership-based | Rust's borrow checker enforces |
| Pattern matching everywhere | `match` keyword | Similar but more verbose |
| Let-it-crash | Panics are bad | Result types, supervision |

### Specific Challenges

**No preemptive scheduling:**
- BEAM can interrupt any process after N reductions
- Tokio requires explicit yield points (`.await`)
- Infinite loops in sync code block the executor
- Mitigation: Keep sync blocks short, use `tokio::task::yield_now()`

**No process isolation:**
- BEAM processes have separate heaps
- Rust tasks share memory
- One panic can (without catch) bring down the runtime
- Mitigation: `catch_unwind`, supervisor restarts, structured concurrency

**No hot code reloading:**
- BEAM supports code upgrade on running system
- Rust binaries are static
- Mitigation: Feature flags, graceful restarts, blue-green deployment

**Error propagation differences:**
- Erlang: linked processes receive exit signals
- Rust: Results propagate, panics are caught at task boundaries
- Mitigation: ractor's supervision, explicit error channels

## Recommendations for gleam-factory

### Current State Analysis

gleam-factory currently uses:
- Synchronous `std::process::Command` for external commands
- `thiserror` for Railway-Oriented Programming
- No async runtime (pure sync code)
- Strong error handling discipline (no unwraps/panics)

### Recommendation 1: Keep It Simple (Preferred)

For a CLI tool like gleam-factory, full actor frameworks are likely overkill.

**Recommended stack:**
1. **tokio** - Add async for parallel stage execution
2. **tokio-graceful-shutdown** - Hierarchical task management
3. **tower** - Middleware patterns for stage execution

**Why:**
- CLI tools don't need actor semantics
- Stages can be modeled as subsystems
- Graceful shutdown handles Ctrl+C properly
- Lower complexity than full actor framework

### Recommendation 2: Actor-Based (If Needed)

If gleam-factory evolves to need:
- Long-running daemon mode
- Distributed task execution
- Complex supervision requirements

**Use ractor because:**
- Tokio-native (no runtime conflict)
- Production-proven at scale (Meta)
- Erlang gen_server semantics
- Supervision trees built-in
- Active development

**Add gradually:**
```toml
[dependencies]
ractor = "0.15"
# Later, if distribution needed:
ractor_cluster = "0.15"
```

### Recommendation 3: Avoid

**Bastion:** Custom runtime, heavier than needed
**Lunatic:** WASM compilation adds deployment complexity
**Actix:** Not Erlang-focused, supervision is limited

## Implementation Patterns for gleam-factory

### Pattern 1: Subsystem-Based Stage Execution

```rust
use tokio_graceful_shutdown::{SubsystemBuilder, SubsystemHandle, Toplevel};

async fn stage_executor(
    subsys: SubsystemHandle,
    stage: Stage,
    task: Task,
) -> Result<(), Error> {
    // Check for shutdown before expensive work
    if subsys.is_shutdown_requested() {
        return Ok(());
    }

    // Execute stage with cancellation support
    tokio::select! {
        result = execute_stage(&stage, &task) => result,
        _ = subsys.on_shutdown_requested() => {
            // Cleanup
            Ok(())
        }
    }
}

async fn pipeline_supervisor(subsys: SubsystemHandle, task: Task) -> Result<(), Error> {
    for stage in task.stages() {
        subsys.start(SubsystemBuilder::new(
            &stage.name,
            |s| stage_executor(s, stage.clone(), task.clone()),
        ));
    }
    subsys.on_shutdown_requested().await;
    Ok(())
}
```

### Pattern 2: Railway-Oriented + Async

```rust
// Keep existing Result<T, Error> pattern
// Add async where beneficial

pub async fn run_command_async(
    cmd: &str,
    args: &[&str],
    cwd: &Path,
) -> Result<CommandResult> {
    let output = tokio::process::Command::new(cmd)
        .args(args)
        .current_dir(cwd)
        .output()
        .await?;

    Ok(parse_output(&output))
}
```

### Pattern 3: Graceful Degradation

```rust
use tower::ServiceBuilder;
use tower_resilience::{CircuitBreakerLayer, RetryLayer, TimeoutLayer};

let stage_service = ServiceBuilder::new()
    .layer(TimeoutLayer::new(Duration::from_secs(300)))  // 5 min max
    .layer(RetryLayer::new()
        .with_max_retries(stage.retries)
        .with_backoff(ExponentialBackoff::default()))
    .layer(CircuitBreakerLayer::new()
        .with_failure_threshold(3)
        .with_recovery_timeout(Duration::from_secs(60)))
    .service(StageExecutor::new(stage));
```

## Open Questions

1. **Daemon mode scope:** Does gleam-factory need long-running processes, or is it purely CLI-driven?

2. **Parallel execution:** Should stages within a pipeline run in parallel, or is sequential sufficient?

3. **Distribution:** Any need for distributed task execution across machines?

4. **State persistence:** Should actor state survive restarts?

## Sources

### Primary (HIGH confidence)
- [Ractor GitHub](https://github.com/slawlor/ractor) - Official repository
- [Ractor Documentation](https://slawlor.github.io/ractor/) - Official docs
- [Tokio Graceful Shutdown](https://tokio.rs/tokio/topics/shutdown) - Official guide
- [tokio-graceful-shutdown crate](https://docs.rs/tokio-graceful-shutdown) - API docs
- [Bastion GitHub](https://github.com/bastion-rs/bastion) - Official repository
- [Kameo GitHub](https://github.com/tqwewe/kameo) - Official repository
- [Lunatic](https://lunatic.solutions/) - Official site

### Secondary (MEDIUM confidence)
- [Comparing Rust Actor Libraries](https://tqwewe.com/blog/comparing-rust-actor-libraries/) - Ari Seyhun's comparison
- [tower-resilience](https://github.com/joshrotenberg/tower-resilience) - Resilience patterns
- [Meta adopts Ractor](https://biggo.com/news/202411031313_meta-adopts-ractor-rust-actor-framework) - Production validation

### Tertiary (LOW confidence)
- [HN Discussion: Ractor](https://news.ycombinator.com/item?id=42030625) - Community perspective
- [Rust Forum Discussions](https://users.rust-lang.org/t/what-is-best-tokio-based-actor-framework-also-support-multi-threading/71509) - Community opinions

## Metadata

**Confidence breakdown:**
- Framework comparison: HIGH - Multiple verified sources, benchmarks available
- BEAM limitations: HIGH - Well-documented fundamental differences
- Recommendations: MEDIUM - Context-dependent, needs validation against gleam-factory roadmap

**Research date:** 2026-01-31
**Valid until:** 90 days (actor ecosystem is stable)
