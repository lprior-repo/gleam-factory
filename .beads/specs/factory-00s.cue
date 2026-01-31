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

"factory-00s": #ValidBead & {
    // ============================================================================
    // BEAD: factory-00s - Add ractor actor framework and Tokio async runtime
    // ============================================================================

    id:              "factory-00s"
    title:           "Runtime: Add ractor actor framework and Tokio async runtime"
    type:            "feature"
    priority:        0
    effort_estimate: "4hr"
    labels:          ["runtime", "actors", "foundation", "P0"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use Tokio as the async runtime for all concurrent operations",
            "THE SYSTEM SHALL use ractor for actor-based message passing",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN an actor is spawned"
                shall:   "THE SYSTEM SHALL return an ActorRef handle for message passing"
            },
            {
                trigger: "WHEN a message is sent via cast (fire-and-forget)"
                shall:   "THE SYSTEM SHALL enqueue the message without blocking the sender"
            },
            {
                trigger: "WHEN a message is sent via call (request-reply)"
                shall:   "THE SYSTEM SHALL block until response or timeout"
            },
            {
                trigger: "WHEN an actor panics during message handling"
                shall:   "THE SYSTEM SHALL catch the panic and notify the supervisor"
            },
            {
                trigger: "WHEN all ActorRef handles are dropped"
                shall:   "THE SYSTEM SHALL gracefully shut down the actor"
            },
        ]

        state_driven: [
            {
                state: "WHILE the actor system is running"
                shall: "THE SYSTEM SHALL process messages in FIFO order per actor"
            },
            {
                state: "WHILE an actor is processing a message"
                shall: "THE SYSTEM SHALL not process another message for that actor (single-threaded per actor)"
            },
        ]

        unwanted: [
            {
                condition: "IF an actor message handler contains blocking synchronous I/O"
                shall_not: "THE SYSTEM SHALL NOT allow blocking the Tokio runtime"
                because:   "Blocking sync I/O in async context causes thread starvation and deadlocks"
            },
            {
                condition: "IF a call message times out"
                shall_not: "THE SYSTEM SHALL NOT leave the caller hanging indefinitely"
                because:   "Unbounded waits cause resource exhaustion and unresponsive systems"
            },
            {
                condition: "IF an actor is spawned without a supervisor"
                shall_not: "THE SYSTEM SHALL NOT allow unsupervised actors in production code"
                because:   "Unsupervised actors that crash are lost forever, breaking system reliability"
            },
        ]

        complex: [
            {
                state:   "WHILE the system is shutting down"
                trigger: "WHEN a new actor spawn is requested"
                shall:   "THE SYSTEM SHALL reject the spawn with Error::ShuttingDown"
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
                    constraints:     "Must include tokio, ractor, tokio-util"
                    example_valid:   "tokio = { version = \"1\", features = [\"full\"] }"
                    example_invalid: "tokio = \"1\"  # Missing features"
                },
            ]
            system_state: [
                "Rust toolchain installed (rustc, cargo)",
                "Moon build system configured",
                "factory-core crate exists",
            ]
        }

        postconditions: {
            state_changes: [
                "Cargo.toml updated with tokio, ractor, tokio-util dependencies",
                "New module: crates/factory-core/src/actor.rs",
                "New module: crates/factory-core/src/runtime.rs",
                "lib.rs updated to export actor and runtime modules",
            ]
            return_guarantees: [
                {
                    field:     "ActorRef<T>"
                    guarantee: "Always valid while held; actor exists until all refs dropped"
                },
                {
                    field:     "Actor::spawn()"
                    guarantee: "Returns Result<ActorRef, SpawnError>"
                },
                {
                    field:     "ActorRef::call()"
                    guarantee: "Returns Result<Response, CallError> with timeout"
                },
                {
                    field:     "ActorRef::cast()"
                    guarantee: "Returns Result<(), SendError> immediately"
                },
            ]
            side_effects: [
                "Tokio runtime initialized on first actor spawn",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Actor state is only mutated inside message handlers",
            "Message passing is the only way to communicate with actors",
            "Actors are single-threaded (one message at a time)",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Tokio runtime conflicts with existing sync code"
                prevention:  "Use block_in_place or spawn_blocking for sync operations"
                test_for_it: "test_sync_code_in_async_context"
            },
            {
                failure:     "ractor version incompatibility with other deps"
                prevention:  "Pin ractor version, test with cargo tree"
                test_for_it: "test_cargo_build_succeeds"
            },
            {
                failure:     "Actor message types don't implement required traits"
                prevention:  "All message types derive Debug, Clone where needed"
                test_for_it: "test_message_types_are_send_sync"
            },
        ]

        usability_failures: [
            {
                failure:     "Actor API too complex for simple use cases"
                prevention:  "Provide ActorBuilder with sensible defaults"
                test_for_it: "test_simple_actor_spawn"
            },
            {
                failure:     "Error messages don't indicate root cause"
                prevention:  "Use thiserror with descriptive messages and context"
                test_for_it: "test_error_messages_are_helpful"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Actor state corrupted by concurrent access"
                prevention:  "Enforce single-threaded per-actor via ractor's model"
                test_for_it: "test_actor_state_isolation"
            },
            {
                failure:     "Messages lost during shutdown"
                prevention:  "Implement graceful shutdown with drain timeout"
                test_for_it: "test_graceful_shutdown_processes_pending"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_spawn_actor_returns_ref"
                given: "A valid actor implementation"
                when:  "Actor::spawn() is called"
                then: [
                    "Returns Ok(ActorRef)",
                    "ActorRef can send messages",
                    "Actor processes messages",
                ]
                real_input: """
                    struct Counter { count: i32 }
                    enum CounterMsg { Increment, GetCount(oneshot::Sender<i32>) }

                    let actor_ref = Counter::spawn(Counter { count: 0 }).await?;
                    """
                expected_output: """
                    actor_ref.cast(CounterMsg::Increment).await?;
                    let (tx, rx) = oneshot::channel();
                    actor_ref.call(CounterMsg::GetCount(tx)).await?;
                    assert_eq!(rx.await?, 1);
                    """
            },
            {
                name:  "test_call_returns_response"
                given: "A running actor"
                when:  "call() is invoked with a request"
                then: [
                    "Blocks until response received",
                    "Returns Ok(response) on success",
                    "Response matches expected value",
                ]
                real_input: """
                    let (tx, rx) = oneshot::channel();
                    actor_ref.call(GetState { respond_to: tx }).await?;
                    """
                expected_output: """
                    let state = rx.await?;
                    assert!(state.is_valid());
                    """
            },
            {
                name:  "test_cast_returns_immediately"
                given: "A running actor"
                when:  "cast() is invoked"
                then: [
                    "Returns immediately without blocking",
                    "Message is eventually processed",
                ]
                real_input: """
                    let start = Instant::now();
                    actor_ref.cast(SlowOperation).await?;
                    let elapsed = start.elapsed();
                    """
                expected_output: """
                    assert!(elapsed < Duration::from_millis(10));  // Cast is fast
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_call_timeout_returns_error"
                given: "An actor that takes too long to respond"
                when:  "call() times out"
                then: [
                    "Returns Err(CallError::Timeout)",
                    "Does not hang indefinitely",
                    "Actor continues running",
                ]
                real_input: """
                    let result = tokio::time::timeout(
                        Duration::from_millis(100),
                        actor_ref.call(SlowRequest)
                    ).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Elapsed { .. })
                    """
            },
            {
                name:  "test_send_to_dead_actor_returns_error"
                given: "An actor that has stopped"
                when:  "Message is sent to dead actor"
                then: [
                    "Returns Err(SendError::Disconnected)",
                    "Does not panic",
                ]
                real_input: """
                    drop(actor_ref);  // Actor stops
                    let result = actor_ref.cast(Message).await;
                    """
                expected_output: null
                expected_error: """
                    Err(SendError::Disconnected)
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_many_concurrent_actors"
                scenario: "1000 actors running simultaneously"
                input:    "spawn 1000 counter actors, send messages to all"
                expected: "All actors process messages, no deadlocks"
            },
            {
                name:     "test_nested_actor_spawn"
                scenario: "Actor spawns child actor in message handler"
                input:    "Parent actor receives SpawnChild message"
                expected: "Child actor spawned successfully"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in codebase"
                test:     "grep -r 'unwrap()\\|expect(' crates/ returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public APIs return Result"
                test:     "cargo doc --document-private-items shows Result returns"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_actor_lifecycle"
            description: "Complete actor lifecycle: spawn -> messages -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/actor_e2e.rs"
                        content: """
                            use factory_core::actor::{Actor, ActorRef};
                            use factory_core::error::Result;

                            #[tokio::test]
                            async fn test_full_actor_lifecycle() -> Result<()> {
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
                command:    "moon run factory-core:test -- --test actor_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_actor_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/actor.rs"
                        contains: "pub trait Actor"
                    },
                    {
                        path:     "crates/factory-core/src/runtime.rs"
                        contains: "pub fn runtime()"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/actor_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_supervisor_restart"
                description: "Verify supervisor restarts crashed actors"
                steps: [
                    {action: "Spawn supervisor with child actor", verify: "Child is running"},
                    {action: "Cause child to panic", verify: "Supervisor receives exit signal"},
                    {action: "Wait for restart", verify: "Child is running again"},
                    {action: "Verify restart count", verify: "restart_count == 1"},
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
                task:      "Write test: test_spawn_actor_returns_ref"
                file:      "crates/factory-core/src/actor.rs"
                what:      "Test that Actor::spawn returns valid ActorRef"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_call_returns_response"
                file:      "crates/factory-core/src/actor.rs"
                what:      "Test that call() blocks and returns response"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_cast_returns_immediately"
                file:      "crates/factory-core/src/actor.rs"
                what:      "Test that cast() is non-blocking"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_call_timeout_returns_error"
                file:      "crates/factory-core/src/actor.rs"
                what:      "Test timeout handling"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add dependencies to Cargo.toml"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dependencies]
                    tokio = { version = "1", features = ["full"] }
                    ractor = "0.15"
                    tokio-util = "0.7"
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["workspace dependencies for version management"]
            },
            {
                task: "Create actor.rs module"
                file: "crates/factory-core/src/actor.rs"
                what: """
                    Define Actor trait, ActorRef handle, message types.
                    Wrap ractor with factory-specific error handling.
                    """
                done_when:     "Module compiles"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "Newtype wrappers for ractor types",
                    "From<ractor::Error> for Error",
                ]
            },
            {
                task: "Create runtime.rs module"
                file: "crates/factory-core/src/runtime.rs"
                what: """
                    Tokio runtime initialization and configuration.
                    Global runtime accessor.
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "OnceCell for lazy initialization",
                    "Builder pattern for runtime config",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export modules from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod actor; pub mod runtime;"
                done_when: "External crates can import factory_core::actor"
            },
            {
                task:      "Add actor errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "ActorSpawnFailed, MessageSendFailed, CallTimeout variants"
                done_when: "Error variants compile and are documented"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/actor.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification"
                done_when: "Actor can be spawned from CLI"
                commands: [
                    "cargo run --example actor_demo",
                ]
                expected: "Actor spawned and processes messages"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'ractor' not found"
                likely_cause: "Dependency not added or wrong version"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "ractor dependency exists with correct version"
                    },
                ]
                fix_pattern: "Add ractor = \"0.15\" to dependencies"
            },
            {
                symptom:      "Runtime error: 'no reactor running'"
                likely_cause: "Tokio runtime not initialized before actor spawn"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/runtime.rs"
                        function:      "runtime()"
                        what_to_check: "Is OnceCell initialized before use?"
                    },
                ]
                fix_pattern: "Use #[tokio::main] or explicit Runtime::new()"
            },
            {
                symptom:      "Test hangs indefinitely"
                likely_cause: "call() without timeout, actor never responds"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actor.rs"
                        function:      "call()"
                        what_to_check: "Is there a timeout wrapper?"
                    },
                ]
                fix_pattern: "Wrap call with tokio::time::timeout"
            },
        ]

        debugging_commands: [
            {
                scenario: "When actor doesn't process messages"
                run:      "RUST_LOG=ractor=debug cargo test"
                look_for: "Message queue status, actor state transitions"
            },
            {
                scenario: "When spawn fails"
                run:      "cargo test -- --nocapture 2>&1 | head -50"
                look_for: "Error message from ractor::Actor::spawn"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] All acceptance tests written and passing",
            "[ ] All error path tests written and passing",
            "[ ] E2E pipeline test passing with real data",
            "[ ] No mocks or fake data in any test",
            "[ ] test_spawn_actor_returns_ref passes",
            "[ ] test_call_returns_response passes",
            "[ ] test_cast_returns_immediately passes",
            "[ ] test_call_timeout_returns_error passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All preconditions validated",
            "[ ] All postconditions guaranteed",
            "[ ] tokio dependency added",
            "[ ] ractor dependency added",
            "[ ] actor.rs module created",
            "[ ] runtime.rs module created",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in actor.rs",
            "[ ] Module-level docs in runtime.rs",
            "[ ] Example usage in doc comments",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add actor, runtime"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add actor error variants"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - must add tokio, ractor"
            },
            {
                path:      "BEAM-PATTERNS-RESEARCH.md"
                relevance: "Research on OTP patterns to implement"
            },
            {
                path:      ".planning/research/RUST-ACTORS-BEAM.md"
                relevance: "Ractor-specific patterns and recommendations"
            },
        ]

        external_references: [
            "https://github.com/slawlor/ractor - Official ractor repo",
            "https://docs.rs/ractor - ractor API docs",
            "https://ryhl.io/blog/actors-with-tokio/ - Alice Ryhl's actor pattern",
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
                how_to_apply:     "Wrap ractor types in newtypes with From impls"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use ractor 0.15 - it's production-proven at Meta",
            "Wrap ractor types in factory-specific newtypes",
            "Use thiserror for error conversion From<ractor::Error>",
            "Add #[must_use] to ActorRef",
            "Use tokio::time::timeout for all call operations",
            "Document with examples in doc comments",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT block the Tokio runtime with sync I/O",
            "Do NOT allow unbounded message queues in production",
        ]

        code_patterns: [
            {
                name:     "Actor Message Enum"
                use_when: "Defining messages for an actor"
                example:  """
                    pub enum MyActorMsg {
                        // Cast (fire-and-forget)
                        UpdateState { value: i32 },

                        // Call (request-reply)
                        GetState { respond_to: oneshot::Sender<State> },
                    }
                    """
            },
            {
                name:     "Actor Handle Pattern"
                use_when: "Creating client-facing API for actors"
                example:  """
                    #[derive(Clone)]
                    pub struct MyActorRef {
                        inner: ractor::ActorRef<MyActorMsg>,
                    }

                    impl MyActorRef {
                        pub async fn get_state(&self) -> Result<State> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(MyActorMsg::GetState { respond_to: tx })?;
                            rx.await.map_err(Error::from)
                        }
                    }
                    """
            },
        ]
    }
}
