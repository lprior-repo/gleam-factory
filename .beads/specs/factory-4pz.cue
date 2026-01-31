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

"factory-4pz": #ValidBead & {
    // ============================================================================
    // BEAD: factory-4pz - Implement root supervisor with OTP restart strategies
    // ============================================================================

    id:              "factory-4pz"
    title:           "Runtime: Implement root supervisor with OTP restart strategies"
    type:            "feature"
    priority:        0
    effort_estimate: "4hr"
    labels:          ["runtime", "actors", "supervisor", "otp", "P0"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL implement OTP-style supervision with restart strategies",
            "THE SYSTEM SHALL track child actors via ChildSpec definitions",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL support one_for_one, one_for_all, and rest_for_one strategies",
        ]

        event_driven: [
            {
                trigger: "WHEN a child actor terminates abnormally"
                shall:   "THE SYSTEM SHALL restart the child according to the configured strategy"
            },
            {
                trigger: "WHEN a child actor terminates normally"
                shall:   "THE SYSTEM SHALL NOT restart transient children"
            },
            {
                trigger: "WHEN restart intensity exceeds max_restarts within max_seconds"
                shall:   "THE SYSTEM SHALL terminate the supervisor and escalate to its parent"
            },
            {
                trigger: "WHEN shutdown is requested"
                shall:   "THE SYSTEM SHALL stop all children in reverse start order"
            },
            {
                trigger: "WHEN a child fails to start during supervisor init"
                shall:   "THE SYSTEM SHALL stop already-started children and fail initialization"
            },
            {
                trigger: "WHEN one_for_all strategy is active and a child crashes"
                shall:   "THE SYSTEM SHALL restart ALL children"
            },
            {
                trigger: "WHEN rest_for_one strategy is active and a child crashes"
                shall:   "THE SYSTEM SHALL restart the crashed child AND all children started after it"
            },
        ]

        state_driven: [
            {
                state: "WHILE the supervisor is running"
                shall: "THE SYSTEM SHALL monitor all child actors for exit signals"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL reject new child spawn requests"
            },
            {
                state: "WHILE restart intensity tracking is active"
                shall: "THE SYSTEM SHALL maintain a sliding window of restart timestamps"
            },
        ]

        unwanted: [
            {
                condition: "IF a child's shutdown timeout expires"
                shall_not: "THE SYSTEM SHALL NOT wait indefinitely for unresponsive children"
                because:   "Hanging shutdowns cause cascading delays and resource leaks"
            },
            {
                condition: "IF restart intensity is exceeded"
                shall_not: "THE SYSTEM SHALL NOT continue restarting in a crash loop"
                because:   "Infinite restart loops mask bugs and waste resources"
            },
            {
                condition: "IF a permanent child exits normally"
                shall_not: "THE SYSTEM SHALL NOT leave permanent children unrunning"
                because:   "Permanent children must always be running for system health"
            },
            {
                condition: "IF supervisor state is corrupted"
                shall_not: "THE SYSTEM SHALL NOT continue operating with inconsistent child tracking"
                because:   "Inconsistent state leads to zombie processes or lost children"
            },
        ]

        complex: [
            {
                state:   "WHILE the supervisor is shutting down"
                trigger: "WHEN a child respawn is requested"
                shall:   "THE SYSTEM SHALL reject the spawn with Error::ShuttingDown"
            },
            {
                state:   "WHILE in one_for_all restart"
                trigger: "WHEN another child crashes during restart sequence"
                shall:   "THE SYSTEM SHALL queue the crash and handle after current restart completes"
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
                    field:           "SupervisorConfig"
                    type:            "Struct"
                    constraints:     "Must specify strategy, max_restarts, max_seconds"
                    example_valid:   "SupervisorConfig { strategy: OneForOne, max_restarts: 3, max_seconds: Duration::from_secs(5) }"
                    example_invalid: "SupervisorConfig { strategy: OneForOne, max_restarts: 0, .. } // No restarts allowed"
                },
                {
                    field:           "ChildSpec"
                    type:            "Struct"
                    constraints:     "Must specify id, start function, restart type, shutdown timeout"
                    example_valid:   "ChildSpec { id: \"worker\", start: spawn_worker, restart: Permanent, shutdown: Duration::from_secs(5) }"
                    example_invalid: "ChildSpec { id: \"\", .. } // Empty ID"
                },
            ]
            system_state: [
                "Tokio runtime initialized",
                "ractor framework available (factory-00s completed)",
                "Parent supervisor exists (except for root)",
            ]
        }

        postconditions: {
            state_changes: [
                "Supervisor actor spawned and running",
                "All child actors started in order",
                "Child tracking state initialized",
                "Restart intensity window initialized",
            ]
            return_guarantees: [
                {
                    field:     "SupervisorRef"
                    guarantee: "Always valid while held; can add/remove children dynamically"
                },
                {
                    field:     "Supervisor::spawn()"
                    guarantee: "Returns Result<SupervisorRef, SpawnError>"
                },
                {
                    field:     "SupervisorRef::start_child()"
                    guarantee: "Returns Result<ChildId, StartError>"
                },
                {
                    field:     "SupervisorRef::terminate_child()"
                    guarantee: "Returns Result<(), TerminateError>"
                },
                {
                    field:     "SupervisorRef::restart_child()"
                    guarantee: "Returns Result<(), RestartError>"
                },
                {
                    field:     "SupervisorRef::which_children()"
                    guarantee: "Returns Vec<ChildInfo> snapshot of current children"
                },
            ]
            side_effects: [
                "Child actors spawned as Tokio tasks",
                "Exit signals monitored via ractor supervision",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Children are started in definition order",
            "Children are stopped in reverse definition order",
            "Restart count resets after max_seconds window expires",
            "Child IDs are unique within a supervisor",
            "Supervisor state is consistent after each restart operation",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "ractor supervisor conflicts with custom supervision logic"
                prevention:  "Use ractor's built-in supervision, extend via composition"
                test_for_it: "test_ractor_supervision_integration"
            },
            {
                failure:     "Shutdown deadlock when child holds resources supervisor needs"
                prevention:  "Enforce timeout-based shutdown, force kill after timeout"
                test_for_it: "test_shutdown_with_unresponsive_child"
            },
            {
                failure:     "Race between restart and shutdown requests"
                prevention:  "Use state machine to serialize shutdown/restart operations"
                test_for_it: "test_concurrent_restart_and_shutdown"
            },
        ]

        usability_failures: [
            {
                failure:     "ChildSpec API too complex for simple use cases"
                prevention:  "Provide ChildSpec::worker() and ChildSpec::supervisor() builders"
                test_for_it: "test_simple_child_spec_creation"
            },
            {
                failure:     "Restart reason not captured for debugging"
                prevention:  "Log restart reason, include in ChildInfo"
                test_for_it: "test_restart_reason_available"
            },
            {
                failure:     "Hard to understand why supervisor terminated"
                prevention:  "Include restart history in supervisor exit reason"
                test_for_it: "test_supervisor_exit_includes_history"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Child tracking becomes inconsistent after partial restart failure"
                prevention:  "Use transactional state updates, rollback on failure"
                test_for_it: "test_state_consistency_after_restart_failure"
            },
            {
                failure:     "Restart intensity window slides incorrectly"
                prevention:  "Use VecDeque with timestamp pruning before each restart"
                test_for_it: "test_restart_intensity_window_accuracy"
            },
            {
                failure:     "Zombie children after supervisor crash"
                prevention:  "Use ractor linking so children die with supervisor"
                test_for_it: "test_no_zombies_after_supervisor_crash"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_supervisor_starts_children_in_order"
                given: "A supervisor config with 3 child specs"
                when:  "Supervisor::spawn() is called"
                then: [
                    "Returns Ok(SupervisorRef)",
                    "All 3 children are running",
                    "Children started in definition order",
                ]
                real_input: """
                    let config = SupervisorConfig::new()
                        .strategy(Strategy::OneForOne)
                        .max_restarts(3)
                        .max_seconds(Duration::from_secs(5));

                    let children = vec![
                        ChildSpec::worker("worker-1", || WorkerActor::spawn()),
                        ChildSpec::worker("worker-2", || WorkerActor::spawn()),
                        ChildSpec::worker("worker-3", || WorkerActor::spawn()),
                    ];

                    let supervisor = FactorySupervisor::spawn(config, children).await?;
                    """
                expected_output: """
                    let children = supervisor.which_children().await?;
                    assert_eq!(children.len(), 3);
                    assert!(children.iter().all(|c| c.status == ChildStatus::Running));
                    assert_eq!(children[0].id, "worker-1");
                    assert_eq!(children[1].id, "worker-2");
                    assert_eq!(children[2].id, "worker-3");
                    """
            },
            {
                name:  "test_one_for_one_restarts_only_crashed_child"
                given: "A supervisor with one_for_one strategy and 3 children"
                when:  "Child 2 crashes"
                then: [
                    "Only child 2 is restarted",
                    "Children 1 and 3 continue running (same ActorRef)",
                    "Restart count incremented",
                ]
                real_input: """
                    let supervisor = spawn_supervisor_with_3_workers(Strategy::OneForOne).await?;
                    let children_before = supervisor.which_children().await?;

                    // Crash child 2
                    supervisor.child("worker-2")?.crash().await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: """
                    let children_after = supervisor.which_children().await?;

                    // Child 1 unchanged
                    assert_eq!(children_before[0].actor_id, children_after[0].actor_id);

                    // Child 2 restarted (new actor ID)
                    assert_ne!(children_before[1].actor_id, children_after[1].actor_id);
                    assert_eq!(children_after[1].restart_count, 1);

                    // Child 3 unchanged
                    assert_eq!(children_before[2].actor_id, children_after[2].actor_id);
                    """
            },
            {
                name:  "test_one_for_all_restarts_all_children"
                given: "A supervisor with one_for_all strategy and 3 children"
                when:  "Any child crashes"
                then: [
                    "All children are restarted",
                    "All children get new ActorRefs",
                ]
                real_input: """
                    let supervisor = spawn_supervisor_with_3_workers(Strategy::OneForAll).await?;
                    let children_before = supervisor.which_children().await?;

                    // Crash child 2
                    supervisor.child("worker-2")?.crash().await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: """
                    let children_after = supervisor.which_children().await?;

                    // All children restarted
                    assert_ne!(children_before[0].actor_id, children_after[0].actor_id);
                    assert_ne!(children_before[1].actor_id, children_after[1].actor_id);
                    assert_ne!(children_before[2].actor_id, children_after[2].actor_id);
                    """
            },
            {
                name:  "test_rest_for_one_restarts_crashed_and_subsequent"
                given: "A supervisor with rest_for_one strategy and 3 children"
                when:  "Child 2 crashes"
                then: [
                    "Child 1 continues running",
                    "Children 2 and 3 are restarted",
                ]
                real_input: """
                    let supervisor = spawn_supervisor_with_3_workers(Strategy::RestForOne).await?;
                    let children_before = supervisor.which_children().await?;

                    // Crash child 2
                    supervisor.child("worker-2")?.crash().await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: """
                    let children_after = supervisor.which_children().await?;

                    // Child 1 unchanged
                    assert_eq!(children_before[0].actor_id, children_after[0].actor_id);

                    // Children 2 and 3 restarted
                    assert_ne!(children_before[1].actor_id, children_after[1].actor_id);
                    assert_ne!(children_before[2].actor_id, children_after[2].actor_id);
                    """
            },
            {
                name:  "test_graceful_shutdown_stops_in_reverse_order"
                given: "A running supervisor with 3 children"
                when:  "Shutdown is requested"
                then: [
                    "Children stopped in reverse start order",
                    "All children terminated",
                    "Supervisor exits cleanly",
                ]
                real_input: """
                    let (shutdown_order_tx, shutdown_order_rx) = mpsc::channel(10);
                    let supervisor = spawn_supervisor_with_tracked_shutdown(shutdown_order_tx).await?;

                    supervisor.shutdown().await?;
                    """
                expected_output: """
                    let shutdown_order: Vec<String> = shutdown_order_rx.try_iter().collect();
                    assert_eq!(shutdown_order, vec!["worker-3", "worker-2", "worker-1"]);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_restart_intensity_exceeded_terminates_supervisor"
                given: "A supervisor with max_restarts=2, max_seconds=5"
                when:  "3 crashes occur within 5 seconds"
                then: [
                    "Supervisor terminates after 3rd crash",
                    "Exit reason includes restart intensity exceeded",
                    "All children terminated",
                ]
                real_input: """
                    let config = SupervisorConfig::new()
                        .strategy(Strategy::OneForOne)
                        .max_restarts(2)
                        .max_seconds(Duration::from_secs(5));

                    let supervisor = FactorySupervisor::spawn(config, children).await?;

                    // Crash 3 times rapidly
                    supervisor.child("worker")?.crash().await?;
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    supervisor.child("worker")?.crash().await?;
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    supervisor.child("worker")?.crash().await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: null
                expected_error: """
                    // Supervisor should have terminated
                    assert!(!supervisor.is_running().await);

                    let exit_reason = supervisor.exit_reason().await?;
                    assert!(matches!(exit_reason, ExitReason::RestartIntensityExceeded { .. }));
                    """
            },
            {
                name:  "test_child_start_failure_rolls_back"
                given: "A supervisor with 3 children where child 2 fails to start"
                when:  "Supervisor::spawn() is called"
                then: [
                    "Returns Err(StartError::ChildStartFailed)",
                    "Child 1 is stopped (was started before failure)",
                    "Child 3 is never started",
                ]
                real_input: """
                    let children = vec![
                        ChildSpec::worker("worker-1", || WorkerActor::spawn()),
                        ChildSpec::worker("worker-2", || async { Err(Error::StartFailed) }),
                        ChildSpec::worker("worker-3", || WorkerActor::spawn()),
                    ];

                    let result = FactorySupervisor::spawn(config, children).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::ChildStartFailed { child_id, .. }) if child_id == "worker-2"));
                    """
            },
            {
                name:  "test_shutdown_timeout_forces_kill"
                given: "A child that ignores shutdown signals"
                when:  "Shutdown is requested with 100ms timeout"
                then: [
                    "Child is force-killed after timeout",
                    "Shutdown completes within reasonable time",
                ]
                real_input: """
                    let children = vec![
                        ChildSpec::worker("unresponsive", || UnresponsiveActor::spawn())
                            .shutdown(Duration::from_millis(100)),
                    ];

                    let supervisor = FactorySupervisor::spawn(config, children).await?;
                    let start = Instant::now();
                    supervisor.shutdown().await?;
                    """
                expected_output: """
                    let elapsed = start.elapsed();
                    assert!(elapsed < Duration::from_millis(200)); // Reasonable buffer
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_restart_window_slides_correctly"
                scenario: "3 crashes at t=0, t=4s, t=6s with max_restarts=2, max_seconds=5"
                input:    "First two crashes within window, third crash after first expires"
                expected: "Supervisor survives because window slides"
            },
            {
                name:     "test_concurrent_child_crashes"
                scenario: "Multiple children crash simultaneously with one_for_all"
                input:    "Children 1 and 3 crash at the same time"
                expected: "Single restart cycle, not multiple"
            },
            {
                name:     "test_transient_child_normal_exit"
                scenario: "Transient child exits normally (Ok return)"
                input:    "Child completes its work successfully"
                expected: "No restart, child removed from tracking"
            },
            {
                name:     "test_permanent_child_normal_exit"
                scenario: "Permanent child exits normally (Ok return)"
                input:    "Permanent child returns Ok"
                expected: "Child is restarted (permanent must always run)"
            },
            {
                name:     "test_dynamic_child_addition"
                scenario: "Add child to running supervisor"
                input:    "supervisor.start_child(new_spec).await"
                expected: "New child started and tracked"
            },
            {
                name:     "test_dynamic_child_removal"
                scenario: "Remove child from running supervisor"
                input:    "supervisor.terminate_child(\"worker-2\").await"
                expected: "Child stopped and removed from tracking"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in supervisor module"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/supervisor.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public supervisor APIs return Result"
                test:     "cargo doc shows Result returns for spawn, start_child, terminate_child"
            },
            {
                name:     "test_invariant_unique_child_ids"
                verifies: "Child IDs are unique within supervisor"
                test:     "Adding duplicate child ID returns Err(DuplicateChildId)"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_supervisor_lifecycle"
            description: "Complete supervisor lifecycle: spawn -> child management -> crashes -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/supervisor_e2e.rs"
                        content: """
                            use factory_core::supervisor::{FactorySupervisor, SupervisorConfig, ChildSpec, Strategy};
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_full_supervisor_lifecycle() -> Result<()> {
                                // Spawn supervisor with 3 workers
                                let config = SupervisorConfig::new()
                                    .strategy(Strategy::OneForOne)
                                    .max_restarts(5)
                                    .max_seconds(Duration::from_secs(10));

                                let children = vec![
                                    ChildSpec::worker("worker-1", || TestWorker::spawn()),
                                    ChildSpec::worker("worker-2", || TestWorker::spawn()),
                                    ChildSpec::worker("worker-3", || TestWorker::spawn()),
                                ];

                                let supervisor = FactorySupervisor::spawn(config, children).await?;

                                // Verify all children running
                                assert_eq!(supervisor.which_children().await?.len(), 3);

                                // Crash a child and verify restart
                                supervisor.child("worker-2")?.crash().await?;
                                tokio::time::sleep(Duration::from_millis(100)).await;
                                assert!(supervisor.child("worker-2")?.is_running().await);

                                // Graceful shutdown
                                supervisor.shutdown().await?;
                                assert!(!supervisor.is_running().await);

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
                command:    "moon run factory-core:test -- --test supervisor_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_supervisor_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/supervisor.rs"
                        contains: "pub struct FactorySupervisor"
                    },
                    {
                        path:     "crates/factory-core/src/supervisor.rs"
                        contains: "pub enum Strategy"
                    },
                    {
                        path:     "crates/factory-core/src/supervisor.rs"
                        contains: "pub struct ChildSpec"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/supervisor_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_nested_supervisor_tree"
                description: "Verify multi-level supervisor hierarchy works"
                steps: [
                    {action: "Spawn root supervisor with 2 child supervisors", verify: "All supervisors running"},
                    {action: "Each child supervisor has 3 workers", verify: "6 workers total"},
                    {action: "Crash a worker in child supervisor 2", verify: "Only that worker restarts"},
                    {action: "Crash child supervisor 2", verify: "Root restarts child supervisor 2"},
                    {action: "Shutdown root", verify: "All supervisors and workers stopped"},
                ]
            },
            {
                name:        "e2e_restart_intensity_escalation"
                description: "Verify restart intensity escalates up the tree"
                steps: [
                    {action: "Create nested supervisor tree", verify: "Tree established"},
                    {action: "Cause child supervisor to exceed restart intensity", verify: "Child supervisor terminates"},
                    {action: "Verify parent restarts child supervisor", verify: "Child supervisor restarted"},
                    {action: "Cause repeated failures", verify: "Eventually escalates to root"},
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
                task:      "Write test: test_supervisor_starts_children_in_order"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test that supervisor starts children in definition order"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_one_for_one_restarts_only_crashed_child"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test that one_for_one only restarts the crashed child"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_one_for_all_restarts_all_children"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test that one_for_all restarts all children on any crash"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_rest_for_one_restarts_crashed_and_subsequent"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test that rest_for_one restarts crashed child and those started after it"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_restart_intensity_exceeded_terminates_supervisor"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test that exceeding max_restarts in max_seconds terminates supervisor"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_graceful_shutdown_stops_in_reverse_order"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Test that shutdown stops children in reverse start order"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define Strategy enum"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    pub enum Strategy {
                        OneForOne,
                        OneForAll,
                        RestForOne,
                    }
                    """
                done_when:     "Enum compiles"
                patterns_to_use: ["Exhaustive enum matching"]
            },
            {
                task: "Define RestartType enum"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    pub enum RestartType {
                        Permanent,  // Always restart
                        Transient,  // Restart only on abnormal exit
                        Temporary,  // Never restart
                    }
                    """
                done_when:     "Enum compiles"
                patterns_to_use: ["OTP restart semantics"]
            },
            {
                task: "Define ChildSpec struct"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    Define ChildSpec with id, start function, restart type, shutdown timeout.
                    Implement builder pattern for ergonomic construction.
                    """
                done_when:     "Struct compiles with builder"
                patterns_to_use: [
                    "Builder pattern",
                    "Type-state for required fields",
                    "Fn() -> Future<Output = Result<ActorRef>> for start",
                ]
            },
            {
                task: "Define SupervisorConfig struct"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    Configuration for supervisor: strategy, max_restarts, max_seconds.
                    Implement builder pattern with sensible defaults.
                    """
                done_when:     "Struct compiles with defaults"
                patterns_to_use: [
                    "Builder pattern",
                    "Default trait implementation",
                    "BEAM default: 1 restart per 5 seconds",
                ]
            },
            {
                task: "Implement restart intensity tracking"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    Use VecDeque<Instant> to track restart timestamps.
                    Prune timestamps older than max_seconds before adding new one.
                    Check if len > max_restarts to determine intensity exceeded.
                    """
                done_when:     "Restart tracking works with sliding window"
                patterns_to_use: [
                    "VecDeque for efficient front removal",
                    "Instant for monotonic timestamps",
                    "Prune before check for accuracy",
                ]
            },
            {
                task: "Implement FactorySupervisor actor"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    Actor that manages child lifecycles.
                    Handles child exit signals, applies restart strategy.
                    Tracks restart intensity, escalates when exceeded.
                    """
                done_when:     "Basic supervisor running with one_for_one"
                patterns_to_use: [
                    "ractor::Actor trait implementation",
                    "ractor supervision (link children)",
                    "State machine for shutdown handling",
                ]
            },
            {
                task: "Implement shutdown logic"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    Stop children in reverse start order.
                    Respect shutdown timeout per child.
                    Force kill after timeout.
                    """
                done_when:     "Graceful shutdown with timeout works"
                patterns_to_use: [
                    "tokio::time::timeout for per-child shutdown",
                    "Reverse iteration over children",
                    "Kill signal as fallback",
                ]
            },
            {
                task: "Implement one_for_all strategy"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    On any child crash: stop all children, restart all children.
                    Maintain start order during restart.
                    """
                done_when:     "one_for_all tests pass"
                patterns_to_use: [
                    "Reuse shutdown logic for stopping",
                    "Reuse start logic for restarting",
                    "Single restart cycle for concurrent crashes",
                ]
            },
            {
                task: "Implement rest_for_one strategy"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    On child crash: stop all children started after crashed one.
                    Restart crashed child and subsequent children in order.
                    """
                done_when:     "rest_for_one tests pass"
                patterns_to_use: [
                    "Track start order via index",
                    "Slice children from crash index",
                    "Restart in original order",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export supervisor module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod supervisor;"
                done_when: "External crates can import factory_core::supervisor"
            },
            {
                task:      "Add supervisor errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "SupervisorSpawnFailed, ChildStartFailed, RestartIntensityExceeded, ShuttingDown variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Implement SupervisorRef handle"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Client-facing API: start_child, terminate_child, restart_child, which_children, shutdown"
                done_when: "All SupervisorRef methods work"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/supervisor.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Run stress test"
                done_when: "Supervisor handles 1000 rapid crashes without deadlock"
                commands: [
                    "cargo test --release -- supervisor_stress_test --ignored",
                ]
                expected: "Test passes within timeout"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Supervisor hangs during shutdown"
                likely_cause: "Child not responding to shutdown signal"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "shutdown_child()"
                        what_to_check: "Is timeout being applied to shutdown?"
                    },
                    {
                        file:          "child actor implementation"
                        function:      "handle_shutdown()"
                        what_to_check: "Does child actor handle Stop signal?"
                    },
                ]
                fix_pattern: "Wrap shutdown with tokio::time::timeout, force kill on timeout"
            },
            {
                symptom:      "Restart intensity not triggering"
                likely_cause: "Window timestamps not being pruned correctly"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "check_restart_intensity()"
                        what_to_check: "Is prune called before adding new timestamp?"
                    },
                ]
                fix_pattern: "Always prune old timestamps before adding new restart timestamp"
            },
            {
                symptom:      "Children not restarting"
                likely_cause: "Exit signal not being received by supervisor"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "pre_start() or handle_supervisor_evt()"
                        what_to_check: "Are children linked to supervisor?"
                    },
                ]
                fix_pattern: "Use ractor::Actor::spawn_linked or monitor children"
            },
            {
                symptom:      "rest_for_one restarts wrong children"
                likely_cause: "Child index tracking incorrect"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "handle_child_exit()"
                        what_to_check: "Is child index computed from original start order?"
                    },
                ]
                fix_pattern: "Use stable child IDs and maintain ordered Vec of children"
            },
            {
                symptom:      "Duplicate child ID accepted"
                likely_cause: "Missing uniqueness check in start_child"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/supervisor.rs"
                        function:      "start_child()"
                        what_to_check: "Is there a check for existing child with same ID?"
                    },
                ]
                fix_pattern: "Check children HashMap/Vec for existing ID before spawning"
            },
        ]

        debugging_commands: [
            {
                scenario: "When children are not restarting"
                run:      "RUST_LOG=ractor=debug,factory_core::supervisor=trace cargo test"
                look_for: "Exit signals received, restart decisions logged"
            },
            {
                scenario: "When restart intensity behaves unexpectedly"
                run:      "Add tracing: tracing::debug!(restarts = ?self.restart_timestamps)"
                look_for: "Timestamp vec contents at each restart"
            },
            {
                scenario: "When shutdown deadlocks"
                run:      "tokio-console or RUST_LOG=tokio=trace"
                look_for: "Blocked tasks waiting on channels"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_supervisor_starts_children_in_order passes",
            "[ ] test_one_for_one_restarts_only_crashed_child passes",
            "[ ] test_one_for_all_restarts_all_children passes",
            "[ ] test_rest_for_one_restarts_crashed_and_subsequent passes",
            "[ ] test_restart_intensity_exceeded_terminates_supervisor passes",
            "[ ] test_graceful_shutdown_stops_in_reverse_order passes",
            "[ ] test_child_start_failure_rolls_back passes",
            "[ ] test_shutdown_timeout_forces_kill passes",
            "[ ] test_transient_child_normal_exit passes",
            "[ ] test_permanent_child_normal_exit passes",
            "[ ] E2E nested supervisor tree test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Strategy enum with OneForOne, OneForAll, RestForOne",
            "[ ] RestartType enum with Permanent, Transient, Temporary",
            "[ ] ChildSpec with builder pattern",
            "[ ] SupervisorConfig with builder pattern",
            "[ ] Restart intensity tracking with sliding window",
            "[ ] FactorySupervisor actor implementation",
            "[ ] SupervisorRef handle implementation",
            "[ ] Graceful shutdown with timeout and force kill",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs explaining OTP supervisor concepts",
            "[ ] Strategy enum variants documented with use cases",
            "[ ] RestartType enum variants documented with behavior",
            "[ ] Example supervisor tree in doc comments",
            "[ ] Restart intensity behavior documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add supervisor"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add supervisor error variants"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - ractor must already be present (factory-00s)"
            },
            {
                path:      "BEAM-PATTERNS-RESEARCH.md"
                relevance: "OTP supervisor patterns: restart strategies, intensity"
            },
            {
                path:      ".planning/research/RUST-ACTORS-BEAM.md"
                relevance: "Ractor-specific patterns for supervision"
            },
            {
                path:      ".beads/specs/factory-00s.cue"
                relevance: "Foundation: actor runtime that supervisor builds on"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-00s"
                description: "Actor framework and Tokio runtime must be in place"
                what_it_provides: "ractor Actor trait, ActorRef, message passing"
            },
        ]

        external_references: [
            "https://www.erlang.org/doc/system/sup_princ.html - Official OTP supervisor docs",
            "https://github.com/slawlor/ractor - Ractor supervision implementation",
            "https://ferd.ca/the-zen-of-erlang.html - Error kernel and supervision philosophy",
            "https://learnyousomeerlang.com/supervisors - Comprehensive supervisor tutorial",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Builder Pattern"
                example_location: "SupervisorConfig, ChildSpec"
                how_to_apply:     "Methods return Self, final build() validates and returns Result"
            },
            {
                pattern:          "Actor Handle Pattern"
                example_location: "Alice Ryhl's blog, ractor ActorRef"
                how_to_apply:     "Separate actor struct from client-facing handle struct"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use ractor's built-in supervision if available, extend if needed",
            "Model restart intensity as VecDeque<Instant> with pruning",
            "Use tokio::select! for monitoring multiple children",
            "Implement shutdown as state machine to handle concurrent requests",
            "Link children to supervisor so exits propagate automatically",
            "Use tracing for observability of supervisor decisions",
            "Test with rapid crashes to verify no race conditions",
            "Follow OTP defaults: 1 restart per 5 seconds",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT block the Tokio runtime with sync I/O",
            "Do NOT create unbounded message queues",
            "Do NOT ignore child exit signals",
            "Do NOT leave zombie children on supervisor crash",
        ]

        code_patterns: [
            {
                name:     "Strategy Enum"
                use_when: "Defining supervisor restart strategy"
                example:  """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    pub enum Strategy {
                        /// Restart only the crashed child
                        OneForOne,
                        /// Restart all children when any crashes
                        OneForAll,
                        /// Restart crashed child and all children started after it
                        RestForOne,
                    }
                    """
            },
            {
                name:     "ChildSpec Builder"
                use_when: "Creating child specifications"
                example:  """
                    pub struct ChildSpec {
                        id: String,
                        start: Box<dyn Fn() -> BoxFuture<'static, Result<ActorCell>> + Send + Sync>,
                        restart: RestartType,
                        shutdown: Duration,
                    }

                    impl ChildSpec {
                        pub fn worker<F, Fut>(id: impl Into<String>, start: F) -> Self
                        where
                            F: Fn() -> Fut + Send + Sync + 'static,
                            Fut: Future<Output = Result<ActorCell>> + Send + 'static,
                        {
                            Self {
                                id: id.into(),
                                start: Box::new(move || Box::pin(start())),
                                restart: RestartType::Permanent,
                                shutdown: Duration::from_secs(5),
                            }
                        }

                        pub fn restart(mut self, restart: RestartType) -> Self {
                            self.restart = restart;
                            self
                        }

                        pub fn shutdown(mut self, timeout: Duration) -> Self {
                            self.shutdown = timeout;
                            self
                        }
                    }
                    """
            },
            {
                name:     "Restart Intensity Tracking"
                use_when: "Checking if restart limit exceeded"
                example:  """
                    struct RestartTracker {
                        timestamps: VecDeque<Instant>,
                        max_restarts: u32,
                        max_seconds: Duration,
                    }

                    impl RestartTracker {
                        fn record_restart(&mut self) -> bool {
                            let now = Instant::now();
                            let cutoff = now - self.max_seconds;

                            // Prune old timestamps
                            while let Some(front) = self.timestamps.front() {
                                if *front < cutoff {
                                    self.timestamps.pop_front();
                                } else {
                                    break;
                                }
                            }

                            // Add new timestamp
                            self.timestamps.push_back(now);

                            // Check if intensity exceeded
                            self.timestamps.len() as u32 > self.max_restarts
                        }
                    }
                    """
            },
            {
                name:     "Supervisor Actor Message Handling"
                use_when: "Processing supervisor events"
                example:  """
                    impl Actor for FactorySupervisor {
                        type Msg = SupervisorMsg;
                        type State = SupervisorState;
                        type Arguments = (SupervisorConfig, Vec<ChildSpec>);

                        async fn handle_supervisor_evt(
                            &self,
                            myself: ActorRef<Self::Msg>,
                            message: SupervisorEvt,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            match message {
                                SupervisorEvt::ActorTerminated(actor_cell) => {
                                    self.handle_child_exit(myself, actor_cell, state).await
                                }
                                SupervisorEvt::ActorPanicked(actor_cell, _) => {
                                    self.handle_child_exit(myself, actor_cell, state).await
                                }
                                _ => Ok(()),
                            }
                        }
                    }
                    """
            },
        ]
    }
}
