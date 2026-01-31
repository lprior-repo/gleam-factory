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

"factory-uc2": #ValidBead & {
    // ============================================================================
    // EPIC: factory-uc2 - Build BEAM-like Orchestrator for Factory
    // ============================================================================

    id:              "factory-uc2"
    title:           "EPIC: Build BEAM-like Orchestrator for Factory"
    type:            "epic"
    priority:        0
    effort_estimate: "4hr"  // Epic-level estimate is symbolic; actual work is in child beads
    labels:          ["epic", "architecture", "beam", "otp", "actors", "P0"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================
    // Epic-level requirements define system-wide invariants that ALL child beads
    // must respect. These are architectural constraints, not implementation details.

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL implement a 9-actor supervision hierarchy with BEAM-like fault tolerance",
            "THE SYSTEM SHALL use ractor as the actor framework with Tokio async runtime",
            "THE SYSTEM SHALL implement OTP restart strategies: one_for_one, one_for_all, rest_for_one",
            "THE SYSTEM SHALL provide state machine-based lifecycle for factory loops",
            "THE SYSTEM SHALL implement resource governance with ticket-based permits",
            "THE SYSTEM SHALL enforce token budgets for LLM cost control",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types across all actors",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented! in any actor",
            "THE SYSTEM SHALL maintain isolation: actor state is only mutated inside message handlers",
            "THE SYSTEM SHALL coordinate shutdown across all actors within 30s timeout",
        ]

        event_driven: [
            {
                trigger: "WHEN a supervised actor terminates abnormally"
                shall:   "THE SYSTEM SHALL restart the actor according to configured OTP strategy"
            },
            {
                trigger: "WHEN restart intensity exceeds threshold"
                shall:   "THE SYSTEM SHALL escalate to parent supervisor"
            },
            {
                trigger: "WHEN SIGINT or SIGTERM is received"
                shall:   "THE SYSTEM SHALL broadcast ShutdownRequested and terminate actors in LIFO order"
            },
            {
                trigger: "WHEN a bead is assigned"
                shall:   "THE SYSTEM SHALL spawn a factory loop via the dispatcher"
            },
            {
                trigger: "WHEN a resource permit is requested"
                shall:   "THE SYSTEM SHALL either grant immediately or apply backpressure"
            },
            {
                trigger: "WHEN a factory loop completes a phase"
                shall:   "THE SYSTEM SHALL transition the state machine and emit appropriate signals"
            },
            {
                trigger: "WHEN test failure feedback arrives"
                shall:   "THE SYSTEM SHALL invoke LLM fix loop with bounded retries"
            },
            {
                trigger: "WHEN token budget is exhausted"
                shall:   "THE SYSTEM SHALL fail the current task with BudgetExhausted error"
            },
            {
                trigger: "WHEN a golden master snapshot is requested"
                shall:   "THE SYSTEM SHALL create COW snapshot of current source state"
            },
            {
                trigger: "WHEN a merge is approved"
                shall:   "THE SYSTEM SHALL queue the patch for ordered integration"
            },
        ]

        state_driven: [
            {
                state: "WHILE the orchestrator is running"
                shall: "THE SYSTEM SHALL maintain heartbeat health checks for all actors"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL reject new actor spawns and bead assignments"
            },
            {
                state: "WHILE resource capacity is exhausted"
                shall: "THE SYSTEM SHALL queue new requests and apply backpressure signals"
            },
            {
                state: "WHILE a factory loop is in Implementing phase"
                shall: "THE SYSTEM SHALL allow test/feedback cycles until phase completion"
            },
            {
                state: "WHILE in one_for_all restart sequence"
                shall: "THE SYSTEM SHALL block new work until all children are restarted"
            },
        ]

        unwanted: [
            {
                condition: "IF an actor message handler blocks the async runtime"
                shall_not: "THE SYSTEM SHALL NOT allow synchronous I/O in message handlers"
                because:   "Blocking causes thread starvation and cascading deadlocks"
            },
            {
                condition: "IF an actor is spawned without supervision"
                shall_not: "THE SYSTEM SHALL NOT allow unsupervised actors"
                because:   "Unsupervised crashes are lost forever, breaking self-healing"
            },
            {
                condition: "IF restart intensity is exceeded"
                shall_not: "THE SYSTEM SHALL NOT continue restarting in crash loops"
                because:   "Infinite restart loops mask bugs and waste resources"
            },
            {
                condition: "IF an actor receives messages during shutdown"
                shall_not: "THE SYSTEM SHALL NOT drop messages without draining"
                because:   "Lost messages corrupt state and lose in-flight work"
            },
            {
                condition: "IF resource permits are granted without tracking"
                shall_not: "THE SYSTEM SHALL NOT allow unbounded resource consumption"
                because:   "OOM and thread exhaustion crash the entire system"
            },
            {
                condition: "IF LLM retries exceed iteration limit"
                shall_not: "THE SYSTEM SHALL NOT retry indefinitely"
                because:   "Unbounded LLM calls waste money and indicate unfixable bugs"
            },
        ]

        complex: [
            {
                state:   "WHILE the merge queue is processing"
                trigger: "WHEN a higher-priority patch arrives"
                shall:   "THE SYSTEM SHALL reorder queue while preserving consistency"
            },
            {
                state:   "WHILE multiple factory loops are active"
                trigger: "WHEN workspace conflicts are detected"
                shall:   "THE SYSTEM SHALL serialize conflicting operations via golden master"
            },
            {
                state:   "WHILE shutdown is in progress"
                trigger: "WHEN a second signal is received"
                shall:   "THE SYSTEM SHALL ignore duplicate shutdown requests"
            },
        ]
    }

    // ============================================================================
    // SECTION 2: KIRK CONTRACTS
    // ============================================================================
    // Epic-level contracts define system-wide preconditions, postconditions,
    // and invariants that govern coordination between all child beads.

    contracts: {
        preconditions: {
            auth_required: false
            required_inputs: [
                {
                    field:           "Rust Toolchain"
                    type:            "System Dependency"
                    constraints:     "rustc >= 1.75, cargo, clippy"
                    example_valid:   "rustc 1.75.0 (82e1608df 2023-12-21)"
                    example_invalid: "rustc 1.60.0 (missing async traits)"
                },
                {
                    field:           "Moon Build System"
                    type:            "System Dependency"
                    constraints:     "moon >= 1.20, bazel-remote cache configured"
                    example_valid:   "moon 1.26.0"
                    example_invalid: "raw cargo commands"
                },
                {
                    field:           "JJ (Jujutsu)"
                    type:            "System Dependency"
                    constraints:     "jj >= 0.17 for workspace management"
                    example_valid:   "jj 0.20.0"
                    example_invalid: "git without jj"
                },
                {
                    field:           "Beads Issue Tracker"
                    type:            "Integration"
                    constraints:     ".beads/beads.jsonl exists and is valid"
                    example_valid:   ".beads/beads.jsonl with JSONL entries"
                    example_invalid: "Missing .beads directory"
                },
            ]
            system_state: [
                "factory-00s completed (ractor + Tokio runtime available)",
                "Crate structure: factory-core (library), factory (CLI)",
                "Moon tasks configured for :quick, :test, :build, :ci, :fmt-fix",
            ]
        }

        postconditions: {
            state_changes: [
                "9 supervised actors running under root supervisor",
                "Signal handler installed for SIGINT/SIGTERM",
                "Resource governor tracking mutator/loop/workspace slots",
                "Factory dispatcher ready to spawn loops on bead assignment",
                "Merge queue ready for ordered patch integration",
                "Golden master ready for COW snapshots",
                "Token budget tracking enabled for all LLM calls",
            ]
            return_guarantees: [
                {
                    field:     "Orchestrator startup"
                    guarantee: "Returns Result<OrchestratorHandle, Error> or fails with diagnostics"
                },
                {
                    field:     "Actor spawning"
                    guarantee: "All actors supervised; crashes trigger restart or escalation"
                },
                {
                    field:     "Graceful shutdown"
                    guarantee: "All actors terminated within 30s; cleanup completed"
                },
                {
                    field:     "Resource allocation"
                    guarantee: "Permits granted or backpressure applied; never OOM"
                },
                {
                    field:     "State machines"
                    guarantee: "Valid transitions only; invalid events rejected with errors"
                },
            ]
            side_effects: [
                "JJ workspaces created/destroyed for factory loops",
                "Beads tracker updated with task status",
                "LLM API calls for feedback loops",
                "Filesystem access for golden master snapshots",
            ]
        }

        invariants: [
            // Code quality invariants (ALL actors must respect)
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in any crate",
            "All async code uses Tokio runtime (never block_on in message handlers)",
            "All actors are supervised; no orphan processes",

            // Concurrency invariants
            "Actor state is only mutated inside message handlers",
            "Message passing is the only inter-actor communication",
            "Each actor processes one message at a time (no concurrent mutations)",
            "Shutdown order is deterministic: LIFO for correctness",

            // Resource invariants
            "Total active factory loops <= MAX_CONCURRENT_LOOPS (configurable)",
            "Total JJ workspaces <= MAX_WORKSPACES (configurable)",
            "RAM usage checked before spawning new loops",
            "Token budgets enforced per-task",

            // Reliability invariants
            "Crash restarts bounded by intensity limits",
            "Feedback loops bounded by iteration limits",
            "Timeouts on all blocking operations",
            "Graceful degradation under load (backpressure)",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================
    // Epic-level inversions identify systemic failures that span multiple actors.
    // These are architectural risks, not implementation bugs.

    inversions: {
        integration_failures: [
            {
                failure:     "Actor framework version incompatibility with deps"
                prevention:  "Pin ractor version in workspace Cargo.toml; test with cargo tree"
                test_for_it: "test_cargo_build_succeeds (in factory-00s)"
            },
            {
                failure:     "Tokio runtime not initialized before actor spawn"
                prevention:  "Use #[tokio::main] entry point; no lazy runtime init"
                test_for_it: "test_runtime_initialized_before_actors"
            },
            {
                failure:     "Supervisor hierarchy incorrectly ordered"
                prevention:  "Start actors in dependency order; stop in reverse"
                test_for_it: "test_actor_startup_order"
            },
            {
                failure:     "Signal handler conflicts with external handlers"
                prevention:  "Document that factory owns SIGINT/SIGTERM; use cfg(unix)"
                test_for_it: "test_signal_handler_installation"
            },
            {
                failure:     "JJ workspace conflicts between factory loops"
                prevention:  "Unique workspace names; golden master serialization"
                test_for_it: "test_workspace_isolation"
            },
            {
                failure:     "Beads watcher races with external bead modifications"
                prevention:  "Use file locking or debounced polling"
                test_for_it: "test_beads_watcher_handles_concurrent_writes"
            },
        ]

        usability_failures: [
            {
                failure:     "Error messages don't indicate which actor failed"
                prevention:  "Include actor name/ID in all error contexts"
                test_for_it: "test_error_messages_identify_actor"
            },
            {
                failure:     "Shutdown hangs with no progress indication"
                prevention:  "Log each actor shutdown; use progress callbacks"
                test_for_it: "test_shutdown_logging"
            },
            {
                failure:     "Resource exhaustion with no backpressure feedback"
                prevention:  "Emit ResourceExhausted events; expose queue depth"
                test_for_it: "test_backpressure_feedback"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Actor state corrupted by concurrent access"
                prevention:  "Single-threaded actor model; no Arc<Mutex> inside actors"
                test_for_it: "test_actor_state_isolation"
            },
            {
                failure:     "Messages lost during supervisor restart"
                prevention:  "Bounded mailbox with backpressure; persist critical state"
                test_for_it: "test_message_delivery_during_restart"
            },
            {
                failure:     "Token budget tracking drifts from actual usage"
                prevention:  "Atomic increment/decrement; cross-check with billing API"
                test_for_it: "test_token_budget_accuracy"
            },
            {
                failure:     "Golden master snapshot corrupted mid-write"
                prevention:  "Atomic rename pattern; fsync before commit"
                test_for_it: "test_golden_master_atomicity"
            },
            {
                failure:     "Merge queue reorders patches incorrectly"
                prevention:  "Sequence numbers; invariant: seq(A) < seq(B) => A merges first"
                test_for_it: "test_merge_queue_ordering"
            },
        ]

        architectural_failures: [
            {
                failure:     "Supervision tree too deep, causing restart cascades"
                prevention:  "Flat hierarchy with max 2 levels; isolate failure domains"
                test_for_it: "test_restart_cascade_bounded"
            },
            {
                failure:     "Circular dependencies between actors"
                prevention:  "Unidirectional message flow; signal bus for broadcasts"
                test_for_it: "test_no_circular_dependencies"
            },
            {
                failure:     "Actor starvation under high message volume"
                prevention:  "Fair scheduling; bounded mailboxes; priority channels"
                test_for_it: "test_actor_fairness"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================
    // Epic-level tests verify end-to-end system behavior across all actors.
    // These are integration tests that span the entire orchestrator.

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_full_orchestrator_startup"
                given: "Fresh system with no actors running"
                when:  "Orchestrator is started"
                then: [
                    "Root supervisor spawns all 9 child actors",
                    "Signal handler is registered",
                    "Resource governor is tracking capacity",
                    "Heartbeat is polling all actors",
                    "System is ready to accept bead assignments",
                ]
                real_input: """
                    let orchestrator = Orchestrator::new(config)?;
                    orchestrator.start().await?;
                    """
                expected_output: """
                    assert!(orchestrator.is_ready());
                    assert_eq!(orchestrator.actor_count(), 9);
                    """
            },
            {
                name:  "test_bead_assignment_to_completion"
                given: "Running orchestrator with available capacity"
                when:  "A bead is assigned"
                then: [
                    "Factory dispatcher spawns a factory loop",
                    "State machine transitions: Assigned -> Implementing -> Testing -> Completed",
                    "Resource permits are acquired and released",
                    "Merge queue receives completed patch",
                    "Bead status updated in tracker",
                ]
                real_input: """
                    orchestrator.assign_bead("factory-xyz").await?;
                    """
                expected_output: """
                    let status = orchestrator.bead_status("factory-xyz").await?;
                    assert_eq!(status, BeadStatus::Completed);
                    """
            },
            {
                name:  "test_graceful_shutdown"
                given: "Running orchestrator with active factory loops"
                when:  "SIGINT is received"
                then: [
                    "ShutdownRequested is broadcast",
                    "Factory loops drain pending work",
                    "Actors terminate in LIFO order",
                    "All resources are released",
                    "Process exits with code 0",
                ]
                real_input: """
                    tokio::signal::ctrl_c().await?;
                    """
                expected_output: """
                    // All actors stopped within 30s
                    assert!(orchestrator.is_stopped());
                    """
            },
            {
                name:  "test_actor_crash_and_restart"
                given: "Running orchestrator with healthy actors"
                when:  "An actor terminates abnormally"
                then: [
                    "Supervisor detects exit signal",
                    "Restart strategy is applied",
                    "Actor is respawned with fresh state",
                    "System continues operating",
                    "Restart count is incremented",
                ]
                real_input: """
                    // Inject fault into heartbeat actor
                    orchestrator.inject_fault("heartbeat", Fault::Panic).await?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    assert!(orchestrator.is_actor_running("heartbeat"));
                    """
            },
            {
                name:  "test_resource_backpressure"
                given: "Orchestrator at maximum capacity"
                when:  "New bead assignment is requested"
                then: [
                    "Resource governor returns Err(CapacityExhausted)",
                    "Request is queued",
                    "Backpressure signal is emitted",
                    "When capacity frees, queued request proceeds",
                ]
                real_input: """
                    for i in 0..MAX_LOOPS { orchestrator.assign_bead(format!("bead-{i}")).await?; }
                    let result = orchestrator.assign_bead("bead-overflow").await;
                    """
                expected_output: """
                    assert!(matches!(result, Err(Error::Backpressure { queue_position: _ })));
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_restart_intensity_exceeded"
                given: "An actor that keeps crashing"
                when:  "Restart intensity exceeds max_restarts in max_seconds"
                then: [
                    "Supervisor stops trying to restart",
                    "Supervisor itself terminates",
                    "Parent supervisor is notified",
                    "System degrades gracefully or shuts down",
                ]
                real_input: """
                    for _ in 0..10 { orchestrator.inject_fault("heartbeat", Fault::Panic).await?; }
                    """
                expected_output: null
                expected_error: """
                    Err(Error::RestartIntensityExceeded { actor: "heartbeat", restarts: 10 })
                    """
            },
            {
                name:  "test_token_budget_exhausted"
                given: "Factory loop with limited token budget"
                when:  "LLM calls exceed budget"
                then: [
                    "Token budget actor rejects further requests",
                    "Factory loop transitions to Failed state",
                    "Error is logged with budget details",
                    "Bead is marked as failed in tracker",
                ]
                real_input: """
                    orchestrator.set_token_budget("bead-xyz", 1000).await?;
                    // Loop consumes 5000 tokens...
                    """
                expected_output: null
                expected_error: """
                    Err(Error::BudgetExhausted { used: 5000, limit: 1000 })
                    """
            },
            {
                name:  "test_shutdown_timeout"
                given: "Actor that doesn't respond to shutdown"
                when:  "Graceful shutdown times out (30s)"
                then: [
                    "Forceful termination is triggered",
                    "Remaining actors are killed",
                    "Resources are reclaimed",
                    "Process exits with non-zero code",
                ]
                real_input: """
                    orchestrator.inject_fault("merge_queue", Fault::IgnoreShutdown).await?;
                    orchestrator.shutdown(Duration::from_secs(5)).await?;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::ShutdownTimeout { actor: "merge_queue", elapsed: 5s })
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_concurrent_bead_assignments"
                scenario: "100 beads assigned simultaneously"
                input:    "100 concurrent assign_bead calls"
                expected: "All queued, processed up to capacity, rest backpressured"
            },
            {
                name:     "test_signal_during_startup"
                scenario: "SIGINT arrives before all actors are ready"
                input:    "SIGINT 50ms after orchestrator.start()"
                expected: "Startup aborted, partial actors cleaned up"
            },
            {
                name:     "test_double_shutdown"
                scenario: "Two SIGINT signals in rapid succession"
                input:    "SIGINT, wait 10ms, SIGINT"
                expected: "Second signal ignored, single shutdown proceeds"
            },
            {
                name:     "test_actor_respawn_under_load"
                scenario: "Actor crashes while processing backlogged messages"
                input:    "1000 queued messages, then crash"
                expected: "Messages preserved, processing resumes after restart"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap_across_crates"
                verifies: "No unwrap() or expect() in any factory crate"
                test:     "rg 'unwrap\\(|expect\\(' crates/ --type rust | wc -l == 0"
            },
            {
                name:     "test_invariant_all_actors_supervised"
                verifies: "Every actor has a supervisor (except root)"
                test:     "Static analysis: all Actor::spawn calls inside supervisor start_child"
            },
            {
                name:     "test_postcondition_nine_actors"
                verifies: "Exactly 9 actors running after startup"
                test:     "orchestrator.actor_count() == 9"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================
    // Epic-level E2E tests verify the full pipeline from bead assignment to merge.

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_factory_pipeline"
            description: "Complete pipeline: bead assignment -> implementation -> test -> merge"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory/tests/e2e_orchestrator.rs"
                        content: """
                            use factory::orchestrator::Orchestrator;
                            use factory_core::error::Result;

                            #[tokio::test]
                            async fn test_full_factory_pipeline() -> Result<()> {
                                let orchestrator = Orchestrator::builder()
                                    .max_loops(2)
                                    .shutdown_timeout(Duration::from_secs(10))
                                    .build()
                                    .await?;

                                orchestrator.start().await?;
                                orchestrator.assign_bead("test-bead-001").await?;

                                // Wait for completion
                                let result = orchestrator.wait_for_bead("test-bead-001").await?;
                                assert!(result.is_completed());

                                orchestrator.shutdown(Duration::from_secs(10)).await?;
                                Ok(())
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "moon run factory-core:build",
                    "moon run factory:build",
                ]
            }

            execute: {
                command:    "moon run factory:test -- --test e2e_orchestrator"
                timeout_ms: 120000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_factory_pipeline ... ok",
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
                        path:     "crates/factory/src/orchestrator.rs"
                        contains: "pub struct Orchestrator"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory/tests/e2e_orchestrator.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_multi_bead_parallel"
                description: "Multiple beads processed in parallel up to capacity"
                steps: [
                    {action: "Start orchestrator with max_loops=3", verify: "9 actors running"},
                    {action: "Assign 5 beads simultaneously", verify: "3 running, 2 queued"},
                    {action: "Wait for first completion", verify: "Queued bead starts"},
                    {action: "Shutdown", verify: "All actors terminated cleanly"},
                ]
            },
            {
                name:        "e2e_crash_recovery"
                description: "System recovers from actor crash during bead processing"
                steps: [
                    {action: "Start orchestrator", verify: "All actors healthy"},
                    {action: "Assign bead", verify: "Factory loop started"},
                    {action: "Inject fault into resource governor", verify: "Governor crashes"},
                    {action: "Wait for restart", verify: "Governor restarted, bead continues"},
                    {action: "Complete bead", verify: "Bead merged successfully"},
                ]
            },
            {
                name:        "e2e_sigterm_shutdown"
                description: "SIGTERM triggers orderly shutdown"
                steps: [
                    {action: "Start orchestrator with 2 active beads", verify: "2 factory loops running"},
                    {action: "Send SIGTERM", verify: "ShutdownRequested broadcast"},
                    {action: "Wait 5s", verify: "Loops drain work-in-progress"},
                    {action: "Wait 10s", verify: "All actors terminated, exit code 0"},
                ]
            },
        ]
    }

    // ============================================================================
    // SECTION 6: IMPLEMENTATION TASKS
    // ============================================================================
    // Epic-level tasks define the order of child bead implementation.
    // Each phase represents a coherent set of functionality that can be tested.

    implementation_tasks: {
        phase_1_foundation: [
            {
                task:      "Complete factory-00s: ractor + Tokio runtime"
                bead:      "factory-00s"
                what:      "Add ractor actor framework and Tokio async runtime"
                done_when: "ActorRef available, message passing works"
                blocks:    ["factory-4pz", "factory-1yj", "all other actors"]
            },
        ]

        phase_2_supervision: [
            {
                task:      "Complete factory-4pz: Root supervisor with OTP strategies"
                bead:      "factory-4pz"
                what:      "Implement one_for_one, one_for_all, rest_for_one restart strategies"
                done_when: "Supervisor restarts crashed children correctly"
                blocks:    ["factory-kou", "factory-avd", "factory-kjt", "all child actors"]
            },
            {
                task:      "Complete factory-1yj: Signal handler for graceful shutdown"
                bead:      "factory-1yj"
                what:      "SIGINT/SIGTERM handling, LIFO shutdown order"
                done_when: "Ctrl+C triggers orderly shutdown within 30s"
                blocks:    ["factory-kou"]
            },
        ]

        phase_3_core_actors: [
            {
                task:      "Complete factory-kou: Signal bus pub/sub"
                bead:      "factory-kou"
                what:      "Event distribution actor for system-wide broadcasts"
                done_when: "ShutdownRequested reaches all subscribers"
                blocks:    ["factory-avd", "factory-192", "factory-a85"]
            },
            {
                task:      "Complete factory-avd: Heartbeat actor"
                bead:      "factory-avd"
                what:      "Health polling for all actors"
                done_when: "Unhealthy actors detected and reported"
                blocks:    ["e2e orchestrator"]
            },
            {
                task:      "Complete factory-kjt: Resource governor"
                bead:      "factory-kjt"
                what:      "Slot allocation, RAM monitoring, backpressure"
                done_when: "Permits granted/rejected, capacity tracked"
                blocks:    ["factory-192", "factory-pa8"]
            },
            {
                task:      "Complete factory-3pb: Actor registry"
                bead:      "factory-3pb"
                what:      "Named process lookup"
                done_when: "Actors registered and looked up by name"
                blocks:    ["factory-192", "factory-a85"]
            },
        ]

        phase_4_workspace_actors: [
            {
                task:      "Complete factory-pa8: JJ workspace manager"
                bead:      "factory-pa8"
                what:      "JJ-native workspace lifecycle"
                done_when: "Workspaces created/destroyed per factory loop"
                blocks:    ["factory-192", "factory-52v"]
            },
            {
                task:      "Complete factory-52v: Golden master actor"
                bead:      "factory-52v"
                what:      "COW snapshots for source-of-truth"
                done_when: "Snapshots created atomically, rollback works"
                blocks:    ["factory-86r"]
            },
        ]

        phase_5_orchestration_actors: [
            {
                task:      "Complete factory-192: Factory dispatcher"
                bead:      "factory-192"
                what:      "Spawns factory loops on BeadAssigned"
                done_when: "Loops spawned, tracked, cleaned up"
                blocks:    ["factory-c2s", "factory-s0r"]
            },
            {
                task:      "Complete factory-a85: Beads watcher"
                bead:      "factory-a85"
                what:      "Issue tracker polling"
                done_when: "Bead changes detected and broadcast"
                blocks:    ["e2e pipeline"]
            },
            {
                task:      "Complete factory-86r: Merge queue"
                bead:      "factory-86r"
                what:      "Ordered patch integration"
                done_when: "Patches merged in sequence order"
                blocks:    ["e2e pipeline"]
            },
        ]

        phase_6_state_machines: [
            {
                task:      "Complete factory-c2s: Factory loop state machine"
                bead:      "factory-c2s"
                what:      "Implementing -> Reviewing -> Pushing -> Completed transitions"
                done_when: "Valid transitions work, invalid rejected"
                blocks:    ["factory-s0r", "factory-czj"]
            },
            {
                task:      "Complete factory-czj: Token budget tracking"
                bead:      "factory-czj"
                what:      "LLM cost control per task"
                done_when: "Budgets enforced, exhaustion triggers failure"
                blocks:    ["factory-s0r"]
            },
            {
                task:      "Complete factory-s0r: LLM feedback loop"
                bead:      "factory-s0r"
                what:      "Auto-healing on test failures"
                done_when: "Test failure -> LLM fix -> retry (bounded)"
                blocks:    ["e2e pipeline"]
            },
        ]

        phase_7_verification: [
            {
                task:     "Run moon run :ci across all crates"
                done_when: "All tests pass, no clippy warnings"
                commands: ["moon run :ci"]
                expected: "exit code 0"
            },
            {
                task:     "Verify no unwraps across crates"
                done_when: "rg finds no unwrap/expect in src/"
                commands: ["rg 'unwrap\\(|expect\\(' crates/*/src/"]
                expected: "no output (empty)"
            },
            {
                task:     "Run E2E orchestrator test"
                done_when: "Full pipeline test passes"
                commands: ["moon run factory:test -- --test e2e_orchestrator"]
                expected: "exit code 0"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================
    // Epic-level failure modes span multiple actors and require systemic fixes.

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Orchestrator hangs on startup"
                likely_cause: "Actor spawn deadlock or missing dependency"
                where_to_look: [
                    {
                        file:          "crates/factory/src/orchestrator.rs"
                        what_to_check: "Actor spawn order matches dependency order"
                    },
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "start_child()"
                        what_to_check: "Timeout on child spawn"
                    },
                ]
                fix_pattern: "Add spawn timeouts; verify dependency DAG is acyclic"
            },
            {
                symptom:      "Shutdown never completes"
                likely_cause: "Actor not responding to stop signal"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actor.rs"
                        function:      "handle() message loop"
                        what_to_check: "Is there a select! with shutdown channel?"
                    },
                    {
                        file:          "crates/factory/src/signal_handler.rs"
                        what_to_check: "Is shutdown broadcast reaching all actors?"
                    },
                ]
                fix_pattern: "Ensure all actors check shutdown channel in message loop"
            },
            {
                symptom:      "Restart loop - actor keeps crashing"
                likely_cause: "Bug in actor init or first message handler"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "handle_child_exit()"
                        what_to_check: "Is restart intensity tracking correct?"
                    },
                ]
                fix_pattern: "Check actor pre_start(); add logging before restart"
            },
            {
                symptom:      "Resource exhaustion - OOM or thread starvation"
                likely_cause: "Unbounded spawning or message queues"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/resource_governor.rs"
                        what_to_check: "Are permits being released after use?"
                    },
                    {
                        file:          "crates/factory/src/factory_dispatcher.rs"
                        what_to_check: "Is there a max_loops limit?"
                    },
                ]
                fix_pattern: "Enforce bounded capacity; add permit release in Drop"
            },
            {
                symptom:      "Messages lost during restart"
                likely_cause: "Mailbox cleared on respawn"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "restart_child()"
                        what_to_check: "Is mailbox preserved or drained?"
                    },
                ]
                fix_pattern: "Drain mailbox before respawn; persist critical messages"
            },
            {
                symptom:      "State machine stuck in invalid state"
                likely_cause: "Invalid transition or missed event"
                where_to_look: [
                    {
                        file:          "crates/factory/src/factory_loop.rs"
                        function:      "transition()"
                        what_to_check: "All events handled for each state?"
                    },
                ]
                fix_pattern: "Add exhaustive match; log rejected transitions"
            },
        ]

        debugging_commands: [
            {
                scenario: "When actors are unresponsive"
                run:      "RUST_LOG=ractor=debug,factory=debug moon run factory:run"
                look_for: "Message queue depths, actor state transitions"
            },
            {
                scenario: "When shutdown hangs"
                run:      "timeout 60 moon run factory:run & sleep 5 && kill -TERM $!"
                look_for: "Which actor logs 'shutting down' vs which doesn't"
            },
            {
                scenario: "When restart loops occur"
                run:      "RUST_LOG=factory::supervisor=debug moon run factory:test"
                look_for: "Restart timestamps, error messages before crash"
            },
            {
                scenario: "When resource exhaustion suspected"
                run:      "cargo run -- 2>&1 | grep -i 'permit\\|capacity\\|exhausted'"
                look_for: "Permit grant/release balance"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================
    // Epic-level checklist for overall system readiness.

    completion_checklist: {
        child_beads: [
            "[ ] factory-00s: ractor + Tokio runtime - COMPLETE",
            "[ ] factory-4pz: Root supervisor with OTP strategies - COMPLETE",
            "[ ] factory-1yj: Signal handler for SIGINT/SIGTERM - COMPLETE",
            "[ ] factory-kou: Signal bus pub/sub - COMPLETE",
            "[ ] factory-avd: Heartbeat actor - COMPLETE",
            "[ ] factory-kjt: Resource governor - COMPLETE",
            "[ ] factory-3pb: Actor registry - COMPLETE",
            "[ ] factory-pa8: JJ workspace manager - COMPLETE",
            "[ ] factory-52v: Golden master actor - COMPLETE",
            "[ ] factory-192: Factory dispatcher - COMPLETE",
            "[ ] factory-a85: Beads watcher - COMPLETE",
            "[ ] factory-86r: Merge queue - COMPLETE",
            "[ ] factory-c2s: Factory loop state machine - COMPLETE",
            "[ ] factory-czj: Token budget tracking - COMPLETE",
            "[ ] factory-s0r: LLM feedback loop - COMPLETE",
        ]

        tests: [
            "[ ] All child bead acceptance tests passing",
            "[ ] E2E orchestrator test passing",
            "[ ] Multi-bead parallel test passing",
            "[ ] Crash recovery test passing",
            "[ ] SIGTERM shutdown test passing",
            "[ ] Resource backpressure test passing",
            "[ ] Restart intensity test passing",
            "[ ] Token budget exhaustion test passing",
        ]

        code: [
            "[ ] All actors implement Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls across all crates",
            "[ ] All actors supervised (no orphans)",
            "[ ] All timeouts bounded (no infinite waits)",
            "[ ] All resources governed (no unbounded consumption)",
            "[ ] State machines have valid transition tables",
        ]

        ci: [
            "[ ] moon run :ci passes across all crates",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Architecture diagram in docs/",
            "[ ] Actor message protocol documented",
            "[ ] State machine diagrams documented",
            "[ ] Failure modes and recovery documented",
            "[ ] Example usage in README",
        ]

        integration: [
            "[ ] JJ workspace integration verified",
            "[ ] Beads tracker integration verified",
            "[ ] LLM API integration verified",
            "[ ] Signal handling verified on Linux",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================
    // Epic-level context provides architectural references and research.

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Core library exports all actor and runtime modules"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types for all actor operations"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies: tokio, ractor, tokio-util"
            },
            {
                path:      "crates/factory/src/main.rs"
                relevance: "CLI entry point - must use #[tokio::main]"
            },
            {
                path:      ".beads/beads.jsonl"
                relevance: "Issue tracker data for beads watcher"
            },
        ]

        child_beads: [
            {
                bead:       "factory-00s"
                title:      "Add ractor actor framework and Tokio async runtime"
                relevance:  "Foundation for all actors"
                spec_file:  ".beads/specs/factory-00s.cue"
            },
            {
                bead:       "factory-4pz"
                title:      "Implement root supervisor with OTP restart strategies"
                relevance:  "Supervision tree structure"
                spec_file:  ".beads/specs/factory-4pz.cue"
            },
            {
                bead:       "factory-1yj"
                title:      "Signal handler for SIGINT/SIGTERM graceful shutdown"
                relevance:  "Graceful shutdown coordination"
                spec_file:  ".beads/specs/factory-1yj.cue"
            },
            {
                bead:       "factory-kou"
                title:      "Signal bus pub/sub actor for event distribution"
                relevance:  "System-wide event broadcasting"
            },
            {
                bead:       "factory-avd"
                title:      "Heartbeat actor for health monitoring"
                relevance:  "Actor health polling"
            },
            {
                bead:       "factory-kjt"
                title:      "Resource governor for slot allocation and RAM monitoring"
                relevance:  "Capacity management and backpressure"
            },
            {
                bead:       "factory-3pb"
                title:      "Actor registry for named process lookup"
                relevance:  "Actor discovery by name"
            },
            {
                bead:       "factory-pa8"
                title:      "JJ-native workspace manager actor"
                relevance:  "Workspace lifecycle"
            },
            {
                bead:       "factory-52v"
                title:      "Golden master actor for source-of-truth snapshots"
                relevance:  "COW snapshots for rollback"
            },
            {
                bead:       "factory-192"
                title:      "Factory dispatcher to spawn factory loops"
                relevance:  "Loop spawning on bead assignment"
            },
            {
                bead:       "factory-a85"
                title:      "Beads watcher actor for issue tracker polling"
                relevance:  "External issue detection"
            },
            {
                bead:       "factory-86r"
                title:      "Merge queue for ordered patch integration"
                relevance:  "Patch ordering and merging"
            },
            {
                bead:       "factory-c2s"
                title:      "Factory loop phase state machine"
                relevance:  "Workflow state transitions"
            },
            {
                bead:       "factory-czj"
                title:      "Token budget tracking for LLM cost control"
                relevance:  "Cost enforcement per task"
            },
            {
                bead:       "factory-s0r"
                title:      "LLM feedback loop for auto-healing"
                relevance:  "Test failure recovery"
            },
        ]

        external_references: [
            "https://github.com/slawlor/ractor - Official ractor repo",
            "https://docs.rs/ractor - ractor API docs",
            "https://erlang.org/doc/design_principles/sup_princ.html - OTP Supervisor principles",
            "https://ryhl.io/blog/actors-with-tokio/ - Alice Ryhl's actor pattern",
            "https://ferd.ca/it-s-about-the-guarantees.html - BEAM reliability model",
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
                how_to_apply:     "Wrap external types in newtypes with From impls"
            },
            {
                pattern:          "Supervisor ChildSpec"
                example_location: "crates/factory-core/src/supervisor.rs"
                how_to_apply:     "Define restart policy, shutdown timeout, start function per child"
            },
            {
                pattern:          "Actor Handle Pattern"
                example_location: "crates/factory-core/src/actor.rs"
                how_to_apply:     "Wrap ActorRef in domain-specific handle with typed methods"
            },
        ]

        architecture: {
            supervision_tree: """
                Root Supervisor (one_for_one)
                ├── Signal Handler (permanent)
                ├── Signal Bus (permanent)
                ├── Heartbeat (permanent)
                ├── Resource Governor (permanent)
                ├── Actor Registry (permanent)
                ├── Workspace Manager (permanent)
                ├── Golden Master (permanent)
                ├── Merge Queue (permanent)
                ├── Beads Watcher (permanent)
                └── Factory Dispatcher (permanent)
                    └── Factory Loops (transient, dynamic children)
                """

            message_flow: """
                Beads Watcher --[BeadDetected]--> Signal Bus
                Signal Bus --[BeadAssigned]--> Factory Dispatcher
                Factory Dispatcher --[ResourceRequest]--> Resource Governor
                Resource Governor --[PermitGranted]--> Factory Dispatcher
                Factory Dispatcher --[SpawnLoop]--> Workspace Manager
                Workspace Manager --[WorkspaceCreated]--> Factory Loop
                Factory Loop --[PhaseCompleted]--> Signal Bus
                Factory Loop --[TokenUsed]--> Token Budget
                Factory Loop --[MergeReady]--> Merge Queue
                """
        }
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================
    // Epic-level hints guide AI agents implementing child beads.

    ai_hints: {
        do: [
            "Implement child beads in dependency order (see phase_* in implementation_tasks)",
            "Start with factory-00s - everything else depends on it",
            "Use ractor 0.15 - it's production-proven at Meta",
            "Wrap ractor types in factory-specific newtypes",
            "Use thiserror for error conversion From<ractor::Error>",
            "Add #[must_use] to all ActorRef types",
            "Use tokio::time::timeout for all call operations",
            "Use select! with shutdown channel in all message loops",
            "Document actor message protocols in doc comments",
            "Add tracing spans per actor for debugging",
            "Test supervision with fault injection",
            "Use moon run :quick for fast iteration (6-7ms cached)",
        ]

        do_not: [
            "Do NOT use unwrap() or expect() - use ? and proper error handling",
            "Do NOT use panic!, todo!, or unimplemented! - return errors",
            "Do NOT modify clippy configuration - fix the code instead",
            "Do NOT use raw cargo commands - use moon",
            "Do NOT block the Tokio runtime with sync I/O",
            "Do NOT allow unbounded message queues",
            "Do NOT spawn actors without supervision",
            "Do NOT skip the shutdown channel in message loops",
            "Do NOT hardcode timeouts - make them configurable",
            "Do NOT assume actor order in tests - use synchronization",
        ]

        implementation_order: [
            "1. factory-00s: ractor + Tokio (FIRST - blocks everything)",
            "2. factory-4pz: Supervisor (blocks all actor spawning)",
            "3. factory-1yj: Signal handler (enables graceful shutdown)",
            "4. factory-kou: Signal bus (enables event distribution)",
            "5. factory-avd, factory-kjt, factory-3pb: Core actors (parallel)",
            "6. factory-pa8, factory-52v: Workspace actors (parallel)",
            "7. factory-192, factory-a85, factory-86r: Orchestration actors (parallel)",
            "8. factory-c2s, factory-czj, factory-s0r: State machines (after dispatcher)",
        ]

        code_patterns: [
            {
                name:     "Supervisor with ChildSpecs"
                use_when: "Defining the root supervisor"
                example:  """
                    let specs = vec![
                        ChildSpec::new("signal_handler", spawn_signal_handler)
                            .restart(Restart::Permanent)
                            .shutdown(Duration::from_secs(5)),
                        ChildSpec::new("signal_bus", spawn_signal_bus)
                            .restart(Restart::Permanent)
                            .shutdown(Duration::from_secs(5)),
                        // ... remaining 7 actors
                    ];

                    Supervisor::spawn(SupervisorConfig {
                        strategy: Strategy::OneForOne,
                        max_restarts: 3,
                        max_seconds: Duration::from_secs(5),
                    }, specs).await?
                    """
            },
            {
                name:     "Message Loop with Shutdown"
                use_when: "Implementing any actor's handle() method"
                example:  """
                    async fn handle(&mut self, msg: Self::Msg, ctx: &mut ActorContext<Self>) -> Result<()> {
                        tokio::select! {
                            _ = ctx.shutdown_signal() => {
                                self.drain_pending().await?;
                                return Ok(());
                            }
                            result = self.process(msg) => {
                                result?;
                            }
                        }
                        Ok(())
                    }
                    """
            },
            {
                name:     "Resource Permit Guard"
                use_when: "Acquiring permits from resource governor"
                example:  """
                    let permit = resource_governor
                        .request_permit(PermitType::FactoryLoop)
                        .await?;

                    // Permit released when guard drops
                    let _guard = permit.guard();

                    // Do work...
                    """
            },
            {
                name:     "State Machine Transition"
                use_when: "Implementing factory loop phase transitions"
                example:  """
                    fn transition(&mut self, event: Event) -> Result<()> {
                        let next_state = match (&self.state, event) {
                            (State::Implementing, Event::TestsPassed) => State::Reviewing,
                            (State::Reviewing, Event::Approved) => State::Pushing,
                            (State::Pushing, Event::MergeSuccess) => State::Completed,
                            (state, event) => {
                                return Err(Error::InvalidTransition {
                                    state: state.clone(),
                                    event,
                                });
                            }
                        };
                        self.state = next_state;
                        Ok(())
                    }
                    """
            },
        ]
    }
}
