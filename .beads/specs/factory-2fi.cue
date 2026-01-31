package specs

// Inline schema definition for validation
#ValidBead: {
    id: string
    title: string
    type: "feature" | "bug" | "task" | "epic" | "chore"
    priority: 0 | 1 | 2 | 3 | 4
    effort_estimate: "15min" | "30min" | "1hr" | "2hr" | "4hr"
    labels: [...string]
    ears_requirements: _
    contracts: _
    inversions: _
    acceptance_tests: _
    e2e_tests: _
    implementation_tasks: _
    failure_modes: _
    completion_checklist: _
    context: _
    ai_hints: _
}

"factory-2fi": #ValidBead & {
    // ============================================================================
    // BEAD: factory-2fi - Port Gleam OTP actor patterns to Tokio
    // ============================================================================

    id:              "factory-2fi"
    title:           "OTP: Port Gleam OTP actor patterns to Tokio for concurrent stage execution"
    type:            "feature"
    priority:        3
    effort_estimate: "4hr"
    labels:          ["actors", "otp", "concurrency", "supervision", "P3"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use Tokio as the async runtime for all actor operations",
            "THE SYSTEM SHALL implement OTP-style supervision trees with restart strategies",
            "THE SYSTEM SHALL support one_for_one, one_for_all, and rest_for_one supervisor strategies",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL implement BEAM-style message passing with call (sync) and cast (async) patterns",
        ]

        event_driven: [
            {
                trigger: "WHEN a stage actor is spawned"
                shall:   "THE SYSTEM SHALL return an ActorHandle for sending messages"
            },
            {
                trigger: "WHEN a stage actor receives a RunStage message"
                shall:   "THE SYSTEM SHALL execute the stage and report completion via StageResult message"
            },
            {
                trigger: "WHEN a supervised actor panics or returns error"
                shall:   "THE SYSTEM SHALL notify the supervisor via ActorExited signal"
            },
            {
                trigger: "WHEN the supervisor receives ActorExited with abnormal reason"
                shall:   "THE SYSTEM SHALL apply the configured restart strategy"
            },
            {
                trigger: "WHEN restart intensity exceeds max_restarts within max_seconds"
                shall:   "THE SYSTEM SHALL escalate by terminating the supervisor itself"
            },
            {
                trigger: "WHEN a parent supervisor terminates"
                shall:   "THE SYSTEM SHALL propagate shutdown to all child actors (top-down)"
            },
            {
                trigger: "WHEN SIGINT or SIGTERM is received"
                shall:   "THE SYSTEM SHALL initiate graceful shutdown of the actor tree"
            },
        ]

        state_driven: [
            {
                state: "WHILE the supervisor is running"
                shall: "THE SYSTEM SHALL monitor all child actor handles for termination"
            },
            {
                state: "WHILE an actor is processing a message"
                shall: "THE SYSTEM SHALL not process another message for that actor (single-threaded semantics)"
            },
            {
                state: "WHILE stages are executing concurrently"
                shall: "THE SYSTEM SHALL respect stage dependencies and ordering constraints"
            },
            {
                state: "WHILE the system is in graceful shutdown"
                shall: "THE SYSTEM SHALL allow in-flight messages to complete (drain timeout)"
            },
        ]

        unwanted: [
            {
                condition: "IF an actor message handler blocks with synchronous I/O"
                shall_not: "THE SYSTEM SHALL NOT block the Tokio runtime"
                because:   "Blocking sync I/O in async context causes thread starvation and deadlocks"
            },
            {
                condition: "IF a call message times out"
                shall_not: "THE SYSTEM SHALL NOT leave the caller hanging indefinitely"
                because:   "Unbounded waits cause resource exhaustion and unresponsive systems"
            },
            {
                condition: "IF an actor crashes repeatedly"
                shall_not: "THE SYSTEM SHALL NOT restart infinitely"
                because:   "Restart loops waste resources and indicate a bohrbug requiring code fix"
            },
            {
                condition: "IF shutdown is requested"
                shall_not: "THE SYSTEM SHALL NOT accept new actor spawns"
                because:   "New actors during shutdown extend cleanup time and complicate state"
            },
        ]

        complex: [
            {
                state:   "WHILE executing a pipeline with dependent stages"
                trigger: "WHEN a stage fails"
                shall:   "THE SYSTEM SHALL skip dependent stages and report partial completion"
            },
            {
                state:   "WHILE executing parallel stages"
                trigger: "WHEN one stage fails with critical error"
                shall:   "THE SYSTEM SHALL cancel sibling stages and propagate failure"
            },
        ]
    }

    // ============================================================================
    // SECTION 2: KIRK CONTRACTS
    // ============================================================================

    contracts: {
        preconditions: {
            auth_required: false
            required_inputs: [
                {
                    field:           "Cargo.toml dependencies"
                    type:            "Object"
                    constraints:     "Must include tokio (full features), tokio-util"
                    example_valid:   "tokio = { version = \"1\", features = [\"full\"] }"
                    example_invalid: "tokio = \"1\"  # Missing features"
                },
                {
                    field:           "SupervisorConfig"
                    type:            "SupervisorConfig"
                    constraints:     "strategy must be valid, max_restarts > 0, max_seconds > 0"
                    example_valid:   "SupervisorConfig { strategy: OneForOne, max_restarts: 3, max_seconds: Duration::from_secs(5) }"
                    example_invalid: "SupervisorConfig { strategy: OneForOne, max_restarts: 0, .. }  # Invalid: no restarts"
                },
            ]
            system_state: [
                "Rust toolchain installed (rustc, cargo)",
                "Moon build system configured",
                "factory-core crate exists with Railway-Oriented error handling",
                "factory-8cb (JJ-native workspace) implemented",
            ]
        }

        postconditions: {
            state_changes: [
                "New module: crates/factory-core/src/actor.rs",
                "New module: crates/factory-core/src/supervisor.rs",
                "New module: crates/factory-core/src/stage_actor.rs",
                "New error variants in error.rs for actor operations",
                "lib.rs updated to export actor, supervisor, stage_actor modules",
            ]
            return_guarantees: [
                {
                    field:     "ActorHandle<M>"
                    guarantee: "Clone + Send + Sync; valid while actor is alive"
                },
                {
                    field:     "Supervisor::spawn()"
                    guarantee: "Returns Result<SupervisorHandle, Error>"
                },
                {
                    field:     "ActorHandle::call()"
                    guarantee: "Returns Result<Response, CallError> with configurable timeout"
                },
                {
                    field:     "ActorHandle::cast()"
                    guarantee: "Returns Result<(), SendError> immediately (non-blocking)"
                },
                {
                    field:     "Supervisor::shutdown()"
                    guarantee: "Returns Result<(), ShutdownError> after all children terminate"
                },
            ]
            side_effects: [
                "Tokio tasks spawned for each actor",
                "Cancellation tokens linked in supervision tree",
                "Actor state persisted in memory (not disk) during lifecycle",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Actor state is only mutated inside message handlers",
            "Message passing is the only way to communicate with actors",
            "Actors are single-threaded (one message at a time per actor)",
            "Supervisors restart children according to strategy, never more than max_restarts in max_seconds",
            "Shutdown propagates top-down through supervision tree",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Tokio runtime not initialized when spawning actors"
                prevention:  "Use #[tokio::main] or explicit Runtime::new() before actor operations"
                test_for_it: "test_actor_spawn_requires_runtime"
            },
            {
                failure:     "Actor channels closed unexpectedly during message handling"
                prevention:  "Handle SendError::Disconnected gracefully, log and propagate"
                test_for_it: "test_send_to_dead_actor_returns_error"
            },
            {
                failure:     "Supervisor and children use incompatible cancellation tokens"
                prevention:  "Use CancellationToken::child_token() for child actors"
                test_for_it: "test_shutdown_propagates_to_children"
            },
            {
                failure:     "Existing sync process module conflicts with async actors"
                prevention:  "Use spawn_blocking or tokio::process::Command for external commands"
                test_for_it: "test_stage_actor_runs_external_commands"
            },
        ]

        usability_failures: [
            {
                failure:     "Actor API too verbose for simple use cases"
                prevention:  "Provide ActorBuilder with sensible defaults and spawn_simple() helper"
                test_for_it: "test_simple_actor_spawn"
            },
            {
                failure:     "Unclear which restart strategy to use"
                prevention:  "Document strategies in module-level docs with decision tree"
                test_for_it: "test_strategy_documentation_exists"
            },
            {
                failure:     "Error messages don't indicate actor identity"
                prevention:  "Include actor name/id in all error variants"
                test_for_it: "test_error_messages_include_actor_name"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Actor state corrupted by concurrent message handling"
                prevention:  "Enforce single-threaded per-actor via exclusive receiver ownership"
                test_for_it: "test_actor_state_isolation"
            },
            {
                failure:     "Messages lost during shutdown"
                prevention:  "Implement graceful shutdown with drain timeout before force kill"
                test_for_it: "test_graceful_shutdown_processes_pending"
            },
            {
                failure:     "Restart counter not reset after recovery period"
                prevention:  "Use sliding window for restart counting, reset after max_seconds"
                test_for_it: "test_restart_counter_resets_after_window"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_spawn_actor_returns_handle"
                given: "A valid actor implementation"
                when:  "Actor::spawn() is called"
                then: [
                    "Returns Ok(ActorHandle)",
                    "ActorHandle can send messages via cast()",
                    "ActorHandle can send messages via call() with response",
                ]
                real_input: """
                    struct StageActor { stage: Stage }
                    enum StageMsg {
                        Run { respond_to: oneshot::Sender<StageResult> },
                        Cancel,
                    }

                    let handle = StageActor::spawn(StageActor { stage }).await?;
                    """
                expected_output: """
                    let (tx, rx) = oneshot::channel();
                    handle.call(StageMsg::Run { respond_to: tx }).await?;
                    let result = rx.await?;
                    assert!(result.success);
                    """
            },
            {
                name:  "test_supervisor_starts_children"
                given: "A supervisor configuration with child specs"
                when:  "Supervisor::spawn() is called"
                then: [
                    "All children are spawned in order",
                    "SupervisorHandle returned",
                    "Children are monitored for termination",
                ]
                real_input: """
                    let config = SupervisorConfig {
                        strategy: SupervisorStrategy::OneForOne,
                        max_restarts: 3,
                        max_seconds: Duration::from_secs(5),
                    };
                    let children = vec![
                        ChildSpec::new("stage-1", || StageActor::new(stage1)),
                        ChildSpec::new("stage-2", || StageActor::new(stage2)),
                    ];
                    let supervisor = Supervisor::spawn(config, children).await?;
                    """
                expected_output: """
                    assert_eq!(supervisor.child_count(), 2);
                    assert!(supervisor.child("stage-1").is_some());
                    assert!(supervisor.child("stage-2").is_some());
                    """
            },
            {
                name:  "test_one_for_one_restarts_only_failed"
                given: "A supervisor with one_for_one strategy and two children"
                when:  "One child crashes"
                then: [
                    "Only the crashed child is restarted",
                    "Other child continues running with same state",
                    "Restart count is incremented",
                ]
                real_input: """
                    let supervisor = spawn_test_supervisor(OneForOne, 2).await?;
                    let child1_pid = supervisor.child("child-1").pid();

                    // Cause child-1 to crash
                    supervisor.child("child-1").cast(CrashMsg).await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: """
                    // child-1 restarted (new pid)
                    assert_ne!(supervisor.child("child-1").pid(), child1_pid);
                    // child-2 unchanged
                    assert!(supervisor.child("child-2").is_running());
                    assert_eq!(supervisor.restart_count(), 1);
                    """
            },
            {
                name:  "test_one_for_all_restarts_all"
                given: "A supervisor with one_for_all strategy and three children"
                when:  "One child crashes"
                then: [
                    "All children are terminated",
                    "All children are restarted in order",
                    "Single restart counted (not per-child)",
                ]
                real_input: """
                    let supervisor = spawn_test_supervisor(OneForAll, 3).await?;
                    let pids: Vec<_> = (1..=3)
                        .map(|i| supervisor.child(&format!("child-{i}")).pid())
                        .collect();

                    supervisor.child("child-2").cast(CrashMsg).await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: """
                    // All children restarted
                    for i in 1..=3 {
                        let name = format!("child-{i}");
                        assert_ne!(supervisor.child(&name).pid(), pids[i-1]);
                    }
                    assert_eq!(supervisor.restart_count(), 1);
                    """
            },
            {
                name:  "test_concurrent_stage_execution"
                given: "A pipeline with independent stages"
                when:  "Stages are spawned as actors"
                then: [
                    "Stages execute concurrently",
                    "Results are collected as each completes",
                    "Total time < sum of individual times",
                ]
                real_input: """
                    let stages = vec![
                        Stage::new("lint", Duration::from_millis(100)),
                        Stage::new("format", Duration::from_millis(100)),
                        Stage::new("test", Duration::from_millis(100)),
                    ];
                    let start = Instant::now();
                    let results = execute_stages_parallel(stages).await?;
                    let elapsed = start.elapsed();
                    """
                expected_output: """
                    assert_eq!(results.len(), 3);
                    assert!(elapsed < Duration::from_millis(200));  // Parallel, not 300ms
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_max_restarts_exceeded_escalates"
                given: "A supervisor with max_restarts=2 and a child that always crashes"
                when:  "Child crashes 3 times within max_seconds"
                then: [
                    "Supervisor terminates itself",
                    "Returns Err(Error::MaxRestartsExceeded)",
                    "All children are terminated",
                ]
                real_input: """
                    let config = SupervisorConfig {
                        max_restarts: 2,
                        max_seconds: Duration::from_secs(60),
                        ..Default::default()
                    };
                    let supervisor = Supervisor::spawn(config, vec![crash_on_start_spec]).await?;
                    let result = supervisor.wait().await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::MaxRestartsExceeded {
                        child: "crash-child",
                        restarts: 3,
                        window: Duration::from_secs(60),
                    })
                    """
            },
            {
                name:  "test_call_timeout_returns_error"
                given: "An actor that takes too long to respond"
                when:  "call() times out"
                then: [
                    "Returns Err(Error::CallTimeout)",
                    "Actor continues running (not killed)",
                    "Caller can retry or cancel",
                ]
                real_input: """
                    let handle = SlowActor::spawn().await?;
                    let result = handle.call_with_timeout(
                        SlowRequest,
                        Duration::from_millis(100),
                    ).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::CallTimeout {
                        actor: "slow-actor",
                        timeout: Duration::from_millis(100),
                    })
                    """
            },
            {
                name:  "test_send_to_stopped_actor_returns_error"
                given: "An actor that has been stopped"
                when:  "Message is sent to stopped actor"
                then: [
                    "Returns Err(Error::ActorStopped)",
                    "Does not panic",
                    "Error includes actor name",
                ]
                real_input: """
                    let handle = TestActor::spawn().await?;
                    handle.stop().await?;
                    let result = handle.cast(TestMsg).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::ActorStopped { name: "test-actor" })
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_restart_counter_resets"
                scenario: "Child crashes, recovers, then crashes again after window expires"
                input:    "max_restarts=2, max_seconds=1s; crash at t=0, t=0.5s, t=2s"
                expected: "Third crash allowed (counter reset after 1s window)"
            },
            {
                name:     "test_graceful_shutdown_with_pending_messages"
                scenario: "Shutdown requested while actor has pending messages"
                input:    "Queue 10 messages, immediately request shutdown"
                expected: "Pending messages drained before actor stops (up to drain_timeout)"
            },
            {
                name:     "test_nested_supervisors"
                scenario: "Supervisor A supervises Supervisor B which supervises actors"
                input:    "B's child crashes beyond B's restart limit"
                expected: "B terminates, A applies its strategy to restart B with fresh children"
            },
            {
                name:     "test_concurrent_shutdown_requests"
                scenario: "Multiple shutdown() calls on same supervisor"
                input:    "Call shutdown() from 3 different tasks simultaneously"
                expected: "All calls complete successfully, actors stopped exactly once"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in actor modules"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/actor.rs crates/factory-core/src/supervisor.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public actor APIs return Result"
                test:     "cargo doc --document-private-items shows Result returns for all pub fn"
            },
            {
                name:     "test_invariant_single_threaded_actor"
                verifies: "Messages are processed sequentially per actor"
                test:     "Send 1000 increment messages, final count == 1000 (no race)"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_pipeline_with_actors"
            description: "Complete stage pipeline: spawn -> execute -> supervise -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/actor_pipeline_e2e.rs"
                        content: """
                            use factory_core::actor::{Actor, ActorHandle};
                            use factory_core::supervisor::{Supervisor, SupervisorConfig, SupervisorStrategy};
                            use factory_core::stage_actor::StageActor;
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_full_pipeline_with_actors() -> Result<()> {
                                // Test implemented inline
                                Ok(())
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "moon run factory-core:build",
                ]
            }

            execute: {
                command:    "moon run factory-core:test -- --test actor_pipeline_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_pipeline_with_actors ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/actor.rs"
                        contains: "pub trait Actor"
                    },
                    {
                        path:     "crates/factory-core/src/supervisor.rs"
                        contains: "pub struct Supervisor"
                    },
                    {
                        path:     "crates/factory-core/src/stage_actor.rs"
                        contains: "pub struct StageActor"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/actor_pipeline_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_supervisor_restart_cascade"
                description: "Verify supervisor cascades restarts correctly"
                steps: [
                    {action: "Spawn supervisor with 3 children using one_for_all", verify: "All children running"},
                    {action: "Cause child-2 to crash", verify: "All children receive shutdown signal"},
                    {action: "Wait for restart", verify: "All 3 children restarted in order"},
                    {action: "Verify state reset", verify: "Each child has fresh initial state"},
                ]
            },
            {
                name:        "e2e_graceful_shutdown_under_load"
                description: "Verify clean shutdown while processing messages"
                steps: [
                    {action: "Spawn stage actors for 5 stages", verify: "All actors running"},
                    {action: "Start long-running stage execution", verify: "Stages in progress"},
                    {action: "Send SIGTERM", verify: "Shutdown initiated"},
                    {action: "Wait up to 30s", verify: "All stages complete or cancel cleanly"},
                    {action: "Check exit code", verify: "Clean exit (0) or controlled timeout exit"},
                ]
            },
            {
                name:        "e2e_stage_pipeline_concurrent"
                description: "Verify concurrent stage execution with dependencies"
                steps: [
                    {action: "Define pipeline: lint || format, then test, then build", verify: "Pipeline created"},
                    {action: "Spawn actors for each stage", verify: "4 actors running"},
                    {action: "Start pipeline execution", verify: "lint and format run in parallel"},
                    {action: "Wait for lint+format", verify: "test starts after both complete"},
                    {action: "Wait for test", verify: "build starts after test completes"},
                    {action: "Collect results", verify: "All 4 stages report success"},
                ]
            },
        ]
    }

    // ============================================================================
    // SECTION 6: IMPLEMENTATION TASKS
    // ============================================================================

    implementation_tasks: {
        phase_1_tests_first: [
            {
                task:      "Write test: test_spawn_actor_returns_handle"
                file:      "crates/factory-core/src/actor.rs"
                what:      "Test that Actor::spawn returns valid ActorHandle"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_supervisor_starts_children"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test supervisor spawns all children"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_one_for_one_restarts_only_failed"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test one_for_one strategy restarts only crashed child"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_max_restarts_exceeded_escalates"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test supervisor terminates when restart limit exceeded"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_graceful_shutdown"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test shutdown propagates to all children"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add async dependencies to Cargo.toml"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dependencies]
                    tokio = { version = "1", features = ["full"] }
                    tokio-util = "0.7"
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["workspace dependencies for version management"]
            },
            {
                task: "Create actor.rs module with core traits"
                file: "crates/factory-core/src/actor.rs"
                what: """
                    Define Actor trait, ActorHandle, message patterns.
                    Key types:
                    - Actor trait with handle() method
                    - ActorHandle<M> for sending messages
                    - ActorError for spawn/send failures
                    """
                done_when:     "Module compiles, Actor trait defined"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "mpsc::channel for actor mailbox",
                    "oneshot::channel for call pattern",
                    "CancellationToken for shutdown",
                ]
            },
            {
                task: "Create supervisor.rs module"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    Implement OTP-style supervisor:
                    - SupervisorStrategy enum (OneForOne, OneForAll, RestForOne)
                    - SupervisorConfig with restart limits
                    - ChildSpec for child definitions
                    - Supervisor struct with spawn/shutdown methods
                    """
                done_when:     "Supervisor compiles with all three strategies"
                patterns_to_use: [
                    "tokio::select! for monitoring multiple children",
                    "Sliding window restart counter",
                    "CancellationToken::child_token() for hierarchy",
                ]
            },
            {
                task: "Create stage_actor.rs module"
                file: "crates/factory-core/src/stage_actor.rs"
                what: """
                    Stage-specific actor implementation:
                    - StageActor struct implementing Actor trait
                    - StageMsg enum (Run, Cancel, GetStatus)
                    - Integration with existing stage execution logic
                    """
                done_when:     "StageActor can run stages asynchronously"
                patterns_to_use: [
                    "tokio::process::Command for external commands",
                    "spawn_blocking for sync operations",
                    "Progress reporting via cast messages",
                ]
            },
            {
                task: "Add actor error variants to error.rs"
                file: "crates/factory-core/src/error.rs"
                what: """
                    Add variants:
                    - ActorSpawnFailed { name, reason }
                    - ActorStopped { name }
                    - CallTimeout { actor, timeout }
                    - MaxRestartsExceeded { child, restarts, window }
                    - ShutdownFailed { reason }
                    """
                done_when:     "Error variants compile and are documented"
            },
        ]

        phase_3_integration: [
            {
                task:      "Export modules from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod actor; pub mod supervisor; pub mod stage_actor;"
                done_when: "External crates can import factory_core::actor"
            },
            {
                task:      "Implement concurrent stage execution"
                file:      "crates/factory-core/src/stages/pipeline.rs"
                what:      "Use actors to run independent stages in parallel"
                done_when: "Pipeline executes stages concurrently where possible"
            },
            {
                task:      "Add signal handling for graceful shutdown"
                file:      "crates/factory/src/main.rs"
                what:      "Handle SIGINT/SIGTERM to trigger actor tree shutdown"
                done_when: "Ctrl+C triggers graceful shutdown"
            },
        ]

        phase_4_verification: [
            {
                task:     "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
                commands: ["moon run :ci"]
                expected: "exit code 0"
            },
            {
                task:     "Verify no unwraps"
                done_when: "grep finds no unwrap/expect"
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/actor.rs crates/factory-core/src/supervisor.rs crates/factory-core/src/stage_actor.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification"
                done_when: "Stages run in parallel with supervision"
                commands: [
                    "cargo run -- stage -s test-task --stage lint",
                ]
                expected: "Stage executes with actor system logging"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'async fn in trait'"
                likely_cause: "Missing async_trait or using wrong Rust edition"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "edition = \"2021\" and async-trait dependency if needed"
                    },
                ]
                fix_pattern: "Use Rust 1.75+ for native async trait, or add async-trait crate"
            },
            {
                symptom:      "Runtime error: 'no reactor running'"
                likely_cause: "Tokio runtime not initialized before spawning actors"
                where_to_look: [
                    {
                        file:          "crates/factory/src/main.rs"
                        function:      "main()"
                        what_to_check: "Is #[tokio::main] attribute present?"
                    },
                ]
                fix_pattern: "Add #[tokio::main] or use Runtime::new().block_on()"
            },
            {
                symptom:      "Actor messages never processed"
                likely_cause: "Actor task not spawned or receiver dropped"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actor.rs"
                        function:      "spawn()"
                        what_to_check: "Is tokio::spawn() called? Is receiver moved into task?"
                    },
                ]
                fix_pattern: "Ensure actor loop is spawned with tokio::spawn(async move { ... })"
            },
            {
                symptom:      "Supervisor doesn't restart children"
                likely_cause: "Child exit not detected or strategy not applied"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "run()"
                        what_to_check: "Is select! monitoring JoinHandles? Is restart logic called?"
                    },
                ]
                fix_pattern: "Use tokio::select! with biased polling, check JoinHandle completion"
            },
            {
                symptom:      "Test hangs indefinitely"
                likely_cause: "call() without timeout, actor deadlock, or missing .await"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actor.rs"
                        function:      "call()"
                        what_to_check: "Is there a timeout wrapper? Is oneshot receiver awaited?"
                    },
                ]
                fix_pattern: "Wrap call with tokio::time::timeout, use call_with_timeout API"
            },
            {
                symptom:      "Memory usage grows unbounded"
                likely_cause: "Unbounded mailbox, messages accumulating faster than processed"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actor.rs"
                        function:      "spawn()"
                        what_to_check: "Is mpsc::channel bounded? What capacity?"
                    },
                ]
                fix_pattern: "Use bounded channel with backpressure: mpsc::channel(capacity)"
            },
        ]

        debugging_commands: [
            {
                scenario: "When actor doesn't process messages"
                run:      "RUST_LOG=factory_core::actor=debug cargo test"
                look_for: "Actor spawn logs, message receive logs, task panic messages"
            },
            {
                scenario: "When supervisor doesn't restart"
                run:      "RUST_LOG=factory_core::supervisor=debug cargo test -- --nocapture"
                look_for: "Child exit detection, strategy application, restart attempts"
            },
            {
                scenario: "When shutdown hangs"
                run:      "RUST_LOG=debug cargo test -- --test-threads=1 2>&1 | grep -E 'shutdown|cancel'"
                look_for: "Cancellation propagation, drain timeout, stuck tasks"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_spawn_actor_returns_handle passes",
            "[ ] test_supervisor_starts_children passes",
            "[ ] test_one_for_one_restarts_only_failed passes",
            "[ ] test_one_for_all_restarts_all passes",
            "[ ] test_rest_for_one_restarts_subsequent passes",
            "[ ] test_max_restarts_exceeded_escalates passes",
            "[ ] test_call_timeout_returns_error passes",
            "[ ] test_graceful_shutdown passes",
            "[ ] test_concurrent_stage_execution passes",
            "[ ] E2E pipeline test passing with real stages",
        ]

        code: [
            "[ ] actor.rs module created with Actor trait",
            "[ ] supervisor.rs module created with all three strategies",
            "[ ] stage_actor.rs module created for stage execution",
            "[ ] Error variants added to error.rs",
            "[ ] All public APIs return Result<T, Error>",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] CancellationToken used for shutdown propagation",
            "[ ] Bounded channels used for actor mailboxes",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in actor.rs with OTP pattern explanation",
            "[ ] Module-level docs in supervisor.rs with strategy decision tree",
            "[ ] Example usage in doc comments for spawn(), call(), cast()",
            "[ ] BEAM-PATTERNS-RESEARCH.md referenced for design decisions",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add actor, supervisor, stage_actor"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add actor-related error variants"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - must add tokio, tokio-util"
            },
            {
                path:      "crates/factory-core/src/process.rs"
                relevance: "Existing command execution - adapt for async or wrap with spawn_blocking"
            },
            {
                path:      "crates/factory-core/src/stages/"
                relevance: "Stage definitions - StageActor will execute these"
            },
            {
                path:      "BEAM-PATTERNS-RESEARCH.md"
                relevance: "Comprehensive OTP pattern documentation for implementation reference"
            },
            {
                path:      ".planning/research/RUST-ACTORS-BEAM.md"
                relevance: "Rust actor framework comparison and Tokio pattern recommendations"
            },
        ]

        external_references: [
            "https://ryhl.io/blog/actors-with-tokio/ - Alice Ryhl's canonical actor pattern",
            "https://tokio.rs/tokio/topics/shutdown - Tokio graceful shutdown guide",
            "https://docs.rs/tokio-util/latest/tokio_util/sync/struct.CancellationToken.html - Cancellation API",
            "https://www.erlang.org/doc/system/sup_princ.html - OTP Supervisor Principles",
            "https://ferd.ca/the-zen-of-erlang.html - Let it crash philosophy explained",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Newtype Validation"
                example_location: "crates/factory-core/src/domain.rs:Slug"
                how_to_apply:     "Wrap ActorHandle in newtype if needed for type safety"
            },
            {
                pattern:          "Thiserror for Error Enums"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Add actor variants with #[error] and descriptive messages"
            },
        ]

        dependencies: [
            {
                bead:       "factory-8cb"
                what:       "JJ-native workspace implementation"
                why_needed: "Actors will manage JJ workspaces for isolated stage execution"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Follow Alice Ryhl's actor pattern: separate handle struct from actor struct",
            "Use mpsc::channel for mailbox, oneshot::channel for call pattern",
            "Use CancellationToken for coordinated shutdown (not channel close)",
            "Implement BEAM-style supervision: monitor children, apply strategy, escalate on limit",
            "Use tokio::select! with biased for monitoring multiple children",
            "Add #[must_use] to ActorHandle to prevent accidental drops",
            "Use bounded channels with explicit capacity (e.g., 100 messages)",
            "Implement drain timeout before force shutdown",
            "Log all actor lifecycle events at debug level",
            "Use sliding window for restart counting (reset after max_seconds)",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT block the Tokio runtime with sync I/O",
            "Do NOT use unbounded channels (memory exhaustion risk)",
            "Do NOT use Arc<Mutex<State>> for actor state (defeats purpose)",
            "Do NOT restart actors that panicked with same arguments (likely bohrbug)",
            "Do NOT ignore SendError - it means actor is dead",
            "Do NOT use ractor or other frameworks - implement with raw Tokio for simplicity",
        ]

        code_patterns: [
            {
                name:     "Actor Struct + Handle Pattern"
                use_when: "Defining any actor"
                example:  """
                    /// The actor itself - owns state and receiver
                    struct MyActor {
                        receiver: mpsc::Receiver<MyMsg>,
                        state: MyState,
                        cancel: CancellationToken,
                    }

                    impl MyActor {
                        async fn run(mut self) {
                            loop {
                                tokio::select! {
                                    biased;
                                    _ = self.cancel.cancelled() => break,
                                    msg = self.receiver.recv() => {
                                        match msg {
                                            Some(msg) => self.handle(msg).await,
                                            None => break,  // All senders dropped
                                        }
                                    }
                                }
                            }
                        }
                    }

                    /// The handle - cloneable, sendable
                    #[derive(Clone)]
                    pub struct MyActorHandle {
                        sender: mpsc::Sender<MyMsg>,
                    }
                    """
            },
            {
                name:     "Call Pattern (Request-Reply)"
                use_when: "Need synchronous response from actor"
                example:  """
                    pub enum MyMsg {
                        // Call: includes response channel
                        GetState { respond_to: oneshot::Sender<State> },
                        // Cast: fire-and-forget
                        UpdateState { value: i32 },
                    }

                    impl MyActorHandle {
                        pub async fn get_state(&self) -> Result<State> {
                            let (tx, rx) = oneshot::channel();
                            self.sender.send(MyMsg::GetState { respond_to: tx })
                                .await
                                .map_err(|_| Error::actor_stopped("my-actor"))?;
                            tokio::time::timeout(Duration::from_secs(5), rx)
                                .await
                                .map_err(|_| Error::call_timeout("my-actor", Duration::from_secs(5)))?
                                .map_err(|_| Error::actor_stopped("my-actor"))
                        }
                    }
                    """
            },
            {
                name:     "Supervisor with Restart Strategy"
                use_when: "Managing child actor lifecycle"
                example:  """
                    impl Supervisor {
                        async fn run(mut self) {
                            let mut restart_times: VecDeque<Instant> = VecDeque::new();

                            loop {
                                tokio::select! {
                                    biased;
                                    _ = self.cancel.cancelled() => {
                                        self.shutdown_children().await;
                                        break;
                                    }
                                    (idx, result) = self.wait_for_child_exit() => {
                                        // Prune old restart times
                                        let cutoff = Instant::now() - self.config.max_seconds;
                                        while restart_times.front().map_or(false, |&t| t < cutoff) {
                                            restart_times.pop_front();
                                        }

                                        if restart_times.len() >= self.config.max_restarts as usize {
                                            // Escalate
                                            return Err(Error::max_restarts_exceeded(..));
                                        }

                                        restart_times.push_back(Instant::now());
                                        self.apply_strategy(idx).await?;
                                    }
                                }
                            }
                            Ok(())
                        }
                    }
                    """
            },
            {
                name:     "Graceful Shutdown with Drain"
                use_when: "Stopping actor cleanly"
                example:  """
                    impl Supervisor {
                        async fn shutdown_children(&mut self) {
                            // Signal all children to stop
                            for child in &self.children {
                                child.cancel.cancel();
                            }

                            // Wait for clean exit with timeout
                            let drain_timeout = Duration::from_secs(30);
                            for child in self.children.drain(..) {
                                let _ = tokio::time::timeout(
                                    drain_timeout,
                                    child.handle,
                                ).await;
                            }
                        }
                    }
                    """
            },
        ]
    }
}
