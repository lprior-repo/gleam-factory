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

"factory-c2s": #ValidBead & {
    // ============================================================================
    // BEAD: factory-c2s - Implement factory loop phase state machine
    // ============================================================================

    id:              "factory-c2s"
    title:           "Runtime: Implement factory loop phase state machine"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["runtime", "actors", "state-machine", "factory-loop", "P1"]

    // ============================================================================
    // STATE MACHINE DEFINITION
    // ============================================================================
    //
    // Phases (States):
    //   - Implementing: Running implementation/test cycles
    //   - Reviewing: Awaiting review completion
    //   - Pushing: Attempting to push changes
    //   - Completed: Terminal success state
    //   - Failed: Terminal failure state
    //
    // Events (Triggers):
    //   - TestPassed: All tests passed successfully
    //   - TestFailed: One or more tests failed
    //   - PushSuccess: Changes pushed successfully
    //   - PushConflict: Push failed due to conflict
    //   - RebaseSuccess: Rebase completed successfully
    //   - RebaseConflict: Rebase failed with conflicts
    //   - MaxIterationsReached: Implementation iteration limit exceeded
    //   - BudgetExhausted: Token/time budget exhausted
    //
    // Transition Table:
    //   +-----------------+---------------------+------------------+------------------+
    //   | Current State   | Event               | Next State       | Action           |
    //   +-----------------+---------------------+------------------+------------------+
    //   | Implementing    | TestPassed          | Reviewing        | request_review   |
    //   | Implementing    | TestFailed          | Implementing     | fix_tests        |
    //   | Implementing    | MaxIterationsReached| Failed           | report_failure   |
    //   | Implementing    | BudgetExhausted     | Failed           | report_failure   |
    //   +-----------------+---------------------+------------------+------------------+
    //   | Reviewing       | TestPassed          | Pushing          | attempt_push     |
    //   | Reviewing       | TestFailed          | Implementing     | fix_review_issues|
    //   | Reviewing       | BudgetExhausted     | Failed           | report_failure   |
    //   +-----------------+---------------------+------------------+------------------+
    //   | Pushing         | PushSuccess         | Completed        | report_success   |
    //   | Pushing         | PushConflict        | Pushing          | attempt_rebase   |
    //   | Pushing         | RebaseSuccess       | Pushing          | retry_push       |
    //   | Pushing         | RebaseConflict      | Implementing     | fix_conflicts    |
    //   | Pushing         | BudgetExhausted     | Failed           | report_failure   |
    //   +-----------------+---------------------+------------------+------------------+
    //   | Completed       | *                   | Completed        | no-op (terminal) |
    //   | Failed          | *                   | Failed           | no-op (terminal) |
    //   +-----------------+---------------------+------------------+------------------+
    //
    // ============================================================================

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL implement FactoryLoop as a ractor actor with explicit phase state",
            "THE SYSTEM SHALL use an enum-based state machine with exhaustive match handling",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL define exactly 5 phases: Implementing, Reviewing, Pushing, Completed, Failed",
            "THE SYSTEM SHALL define exactly 8 events: TestPassed, TestFailed, PushSuccess, PushConflict, RebaseSuccess, RebaseConflict, MaxIterationsReached, BudgetExhausted",
            "THE SYSTEM SHALL enforce valid state transitions via type system where possible",
        ]

        event_driven: [
            {
                trigger: "WHEN TestPassed event is received in Implementing phase"
                shall:   "THE SYSTEM SHALL transition to Reviewing phase and request review"
            },
            {
                trigger: "WHEN TestFailed event is received in Implementing phase"
                shall:   "THE SYSTEM SHALL remain in Implementing phase and increment iteration counter"
            },
            {
                trigger: "WHEN MaxIterationsReached event is received"
                shall:   "THE SYSTEM SHALL transition to Failed phase with iteration limit error"
            },
            {
                trigger: "WHEN BudgetExhausted event is received in any non-terminal phase"
                shall:   "THE SYSTEM SHALL transition to Failed phase with budget exhausted error"
            },
            {
                trigger: "WHEN PushSuccess event is received in Pushing phase"
                shall:   "THE SYSTEM SHALL transition to Completed phase and signal success"
            },
            {
                trigger: "WHEN PushConflict event is received in Pushing phase"
                shall:   "THE SYSTEM SHALL attempt rebase and remain in Pushing phase"
            },
            {
                trigger: "WHEN RebaseConflict event is received in Pushing phase"
                shall:   "THE SYSTEM SHALL transition to Implementing phase to resolve conflicts"
            },
            {
                trigger: "WHEN any event is received in Completed or Failed phase"
                shall:   "THE SYSTEM SHALL ignore the event (terminal states are absorbing)"
            },
            {
                trigger: "WHEN FactoryLoop actor is spawned"
                shall:   "THE SYSTEM SHALL initialize in Implementing phase with zero iterations"
            },
        ]

        state_driven: [
            {
                state: "WHILE the FactoryLoop is in Implementing phase"
                shall: "THE SYSTEM SHALL track iteration count and check against max_iterations"
            },
            {
                state: "WHILE the FactoryLoop is in Reviewing phase"
                shall: "THE SYSTEM SHALL await review feedback before proceeding"
            },
            {
                state: "WHILE the FactoryLoop is in Pushing phase"
                shall: "THE SYSTEM SHALL retry push with exponential backoff on transient failures"
            },
            {
                state: "WHILE the FactoryLoop is in any phase"
                shall: "THE SYSTEM SHALL check budget constraints before processing events"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL complete current operation then exit gracefully"
            },
        ]

        unwanted: [
            {
                condition: "IF an invalid transition is attempted"
                shall_not: "THE SYSTEM SHALL NOT allow invalid state transitions"
                because:   "Invalid transitions indicate logic errors and corrupt state machine integrity"
            },
            {
                condition: "IF the actor is in a terminal state"
                shall_not: "THE SYSTEM SHALL NOT accept new work events"
                because:   "Terminal states are final; accepting events violates state machine semantics"
            },
            {
                condition: "IF budget is exhausted"
                shall_not: "THE SYSTEM SHALL NOT continue processing implementation cycles"
                because:   "Unbounded resource consumption leads to system instability"
            },
            {
                condition: "IF push fails repeatedly"
                shall_not: "THE SYSTEM SHALL NOT retry indefinitely without backoff"
                because:   "Infinite retry loops cause resource exhaustion and downstream pressure"
            },
            {
                condition: "IF iteration limit is reached"
                shall_not: "THE SYSTEM SHALL NOT continue attempting fixes"
                because:   "Endless fix loops indicate fundamental issues requiring human intervention"
            },
        ]

        complex: [
            {
                state:   "WHILE the FactoryLoop is in Pushing phase"
                trigger: "WHEN PushConflict occurs followed by RebaseSuccess"
                shall:   "THE SYSTEM SHALL retry push with fresh commit"
            },
            {
                state:   "WHILE the FactoryLoop is in Implementing phase"
                trigger: "WHEN TestFailed occurs and iteration_count equals max_iterations"
                shall:   "THE SYSTEM SHALL emit MaxIterationsReached and transition to Failed"
            },
            {
                state:   "WHILE the FactoryLoop is awaiting signal bus notification"
                trigger: "WHEN shutdown signal is received"
                shall:   "THE SYSTEM SHALL complete current atomic operation then shutdown"
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
                    field:           "FactoryLoopConfig"
                    type:            "Struct"
                    constraints:     "Must specify max_iterations, budget, retry_policy"
                    example_valid:   "FactoryLoopConfig { max_iterations: 10, budget: Budget::tokens(100_000), retry_policy: RetryPolicy::exponential(3) }"
                    example_invalid: "FactoryLoopConfig { max_iterations: 0, .. } // Zero iterations makes no sense"
                },
                {
                    field:           "TaskContext"
                    type:            "Struct"
                    constraints:     "Must include bead_id, workspace, signal_bus_ref, governor_ref"
                    example_valid:   "TaskContext { bead_id: \"factory-123\", workspace: jj_workspace, signal_bus, governor }"
                    example_invalid: "TaskContext { bead_id: \"\", .. } // Empty bead ID"
                },
                {
                    field:           "Phase (initial)"
                    type:            "Enum"
                    constraints:     "Must start in Implementing phase"
                    example_valid:   "Phase::Implementing { iteration: 0 }"
                    example_invalid: "Phase::Completed // Cannot start in terminal state"
                },
            ]
            system_state: [
                "Signal bus actor running (factory-kou completed)",
                "Resource governor actor running (factory-kjt completed)",
                "Tokio runtime initialized (factory-00s completed)",
                "Parent supervisor exists for lifecycle management",
            ]
        }

        postconditions: {
            state_changes: [
                "FactoryLoop actor spawned and registered with supervisor",
                "Phase initialized to Implementing with iteration 0",
                "Signal bus subscription established",
                "Governor budget tracking initiated",
            ]
            return_guarantees: [
                {
                    field:     "FactoryLoopRef"
                    guarantee: "Always valid while held; can send events and query phase"
                },
                {
                    field:     "FactoryLoop::spawn()"
                    guarantee: "Returns Result<FactoryLoopRef, SpawnError>"
                },
                {
                    field:     "FactoryLoopRef::send_event()"
                    guarantee: "Returns Result<(), SendError> with phase validation"
                },
                {
                    field:     "FactoryLoopRef::current_phase()"
                    guarantee: "Returns Result<Phase, CallError> with current state snapshot"
                },
                {
                    field:     "FactoryLoopRef::is_terminal()"
                    guarantee: "Returns bool indicating if in Completed or Failed phase"
                },
            ]
            side_effects: [
                "Events logged to audit trail",
                "Phase transitions emit signals via signal bus",
                "Budget consumption reported to resource governor",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Phase transitions are atomic and consistent",
            "Only valid transitions are possible (enforced by transition function)",
            "Terminal phases (Completed, Failed) are absorbing (no exits)",
            "Iteration count is monotonically increasing in Implementing phase",
            "Budget check occurs before every event processing",
            "Signal bus is notified of every phase transition",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Signal bus not available when FactoryLoop starts"
                prevention:  "Require SignalBusRef in spawn arguments, fail fast if unavailable"
                test_for_it: "test_spawn_fails_without_signal_bus"
            },
            {
                failure:     "Resource governor not responding to budget queries"
                prevention:  "Use timeout on governor calls, emit BudgetExhausted on timeout"
                test_for_it: "test_governor_timeout_exhausts_budget"
            },
            {
                failure:     "Race between shutdown signal and phase transition"
                prevention:  "Use atomic state updates, check shutdown flag before transition"
                test_for_it: "test_shutdown_during_transition"
            },
            {
                failure:     "FactoryLoop outlives its parent supervisor"
                prevention:  "Link to supervisor so crash propagates"
                test_for_it: "test_factory_loop_dies_with_supervisor"
            },
        ]

        usability_failures: [
            {
                failure:     "Difficult to understand why FactoryLoop failed"
                prevention:  "Include full phase history in failure result"
                test_for_it: "test_failure_includes_phase_history"
            },
            {
                failure:     "Cannot query progress without consuming events"
                prevention:  "Provide read-only current_phase() and iteration_count() queries"
                test_for_it: "test_phase_query_is_idempotent"
            },
            {
                failure:     "Event ordering ambiguity in concurrent scenarios"
                prevention:  "Events processed in FIFO order per actor (ractor guarantee)"
                test_for_it: "test_event_ordering_is_fifo"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Phase state becomes inconsistent after partial transition"
                prevention:  "Use single atomic state update in transition function"
                test_for_it: "test_transition_is_atomic"
            },
            {
                failure:     "Iteration counter overflows"
                prevention:  "Use u32 with checked_add, treat overflow as MaxIterationsReached"
                test_for_it: "test_iteration_overflow_is_handled"
            },
            {
                failure:     "Lost events during shutdown"
                prevention:  "Drain message queue before exiting, log any dropped events"
                test_for_it: "test_shutdown_logs_dropped_events"
            },
            {
                failure:     "Phase history grows unbounded"
                prevention:  "Limit history to last N transitions (configurable)"
                test_for_it: "test_phase_history_is_bounded"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_happy_path_implementing_to_completed"
                given: "A FactoryLoop in Implementing phase"
                when:  "TestPassed -> TestPassed -> PushSuccess events received"
                then: [
                    "Transitions: Implementing -> Reviewing -> Pushing -> Completed",
                    "Final phase is Completed",
                    "Phase history shows all transitions",
                ]
                real_input: """
                    let config = FactoryLoopConfig::new()
                        .max_iterations(10)
                        .budget(Budget::tokens(100_000));

                    let factory_loop = FactoryLoop::spawn(config, task_context).await?;
                    assert!(matches!(factory_loop.current_phase().await?, Phase::Implementing { .. }));

                    factory_loop.send_event(Event::TestPassed).await?;
                    assert!(matches!(factory_loop.current_phase().await?, Phase::Reviewing));

                    factory_loop.send_event(Event::TestPassed).await?;  // Review passed
                    assert!(matches!(factory_loop.current_phase().await?, Phase::Pushing));

                    factory_loop.send_event(Event::PushSuccess).await?;
                    """
                expected_output: """
                    assert!(matches!(factory_loop.current_phase().await?, Phase::Completed));
                    assert!(factory_loop.is_terminal().await?);

                    let history = factory_loop.phase_history().await?;
                    assert_eq!(history.len(), 4);  // Implementing, Reviewing, Pushing, Completed
                    """
            },
            {
                name:  "test_test_failure_stays_in_implementing"
                given: "A FactoryLoop in Implementing phase with iteration 0"
                when:  "TestFailed event is received"
                then: [
                    "Remains in Implementing phase",
                    "Iteration count incremented to 1",
                    "Ready to retry implementation",
                ]
                real_input: """
                    let factory_loop = FactoryLoop::spawn(config, task_context).await?;
                    factory_loop.send_event(Event::TestFailed).await?;
                    """
                expected_output: """
                    let phase = factory_loop.current_phase().await?;
                    assert!(matches!(phase, Phase::Implementing { iteration: 1 }));
                    """
            },
            {
                name:  "test_push_conflict_triggers_rebase"
                given: "A FactoryLoop in Pushing phase"
                when:  "PushConflict event is received followed by RebaseSuccess"
                then: [
                    "Remains in Pushing phase",
                    "Rebase action is triggered",
                    "Ready to retry push",
                ]
                real_input: """
                    // Setup: get to Pushing phase
                    let factory_loop = setup_in_pushing_phase().await?;

                    factory_loop.send_event(Event::PushConflict).await?;
                    // Internal: triggers rebase action
                    factory_loop.send_event(Event::RebaseSuccess).await?;
                    """
                expected_output: """
                    assert!(matches!(factory_loop.current_phase().await?, Phase::Pushing));
                    // Push retry will be attempted
                    """
            },
            {
                name:  "test_rebase_conflict_returns_to_implementing"
                given: "A FactoryLoop in Pushing phase after PushConflict"
                when:  "RebaseConflict event is received"
                then: [
                    "Transitions to Implementing phase",
                    "Conflict resolution required",
                    "Iteration count preserved",
                ]
                real_input: """
                    let factory_loop = setup_in_pushing_phase().await?;
                    factory_loop.send_event(Event::PushConflict).await?;
                    factory_loop.send_event(Event::RebaseConflict).await?;
                    """
                expected_output: """
                    assert!(matches!(factory_loop.current_phase().await?, Phase::Implementing { .. }));
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_max_iterations_reached_fails"
                given: "A FactoryLoop with max_iterations=3"
                when:  "TestFailed received 3 times"
                then: [
                    "Transitions to Failed phase",
                    "Reason is MaxIterationsReached",
                    "Is terminal state",
                ]
                real_input: """
                    let config = FactoryLoopConfig::new().max_iterations(3);
                    let factory_loop = FactoryLoop::spawn(config, task_context).await?;

                    factory_loop.send_event(Event::TestFailed).await?;  // iteration 1
                    factory_loop.send_event(Event::TestFailed).await?;  // iteration 2
                    factory_loop.send_event(Event::TestFailed).await?;  // iteration 3 -> max reached
                    """
                expected_output: null
                expected_error: """
                    let phase = factory_loop.current_phase().await?;
                    assert!(matches!(phase, Phase::Failed { reason: FailureReason::MaxIterationsReached }));
                    assert!(factory_loop.is_terminal().await?);
                    """
            },
            {
                name:  "test_budget_exhausted_fails"
                given: "A FactoryLoop with limited budget"
                when:  "Budget is consumed and next event arrives"
                then: [
                    "Transitions to Failed phase",
                    "Reason is BudgetExhausted",
                    "Cannot process further events",
                ]
                real_input: """
                    let config = FactoryLoopConfig::new()
                        .budget(Budget::tokens(100));  // Very small budget

                    let factory_loop = FactoryLoop::spawn(config, task_context).await?;
                    // Consume budget through operations...
                    exhaust_budget(&factory_loop).await?;

                    factory_loop.send_event(Event::TestPassed).await?;
                    """
                expected_output: null
                expected_error: """
                    let phase = factory_loop.current_phase().await?;
                    assert!(matches!(phase, Phase::Failed { reason: FailureReason::BudgetExhausted }));
                    """
            },
            {
                name:  "test_events_ignored_in_terminal_state"
                given: "A FactoryLoop in Completed phase"
                when:  "Any event is sent"
                then: [
                    "Event is ignored",
                    "Phase remains Completed",
                    "No error returned (graceful ignore)",
                ]
                real_input: """
                    let factory_loop = setup_in_completed_phase().await?;

                    // These should all be ignored
                    factory_loop.send_event(Event::TestFailed).await?;
                    factory_loop.send_event(Event::PushConflict).await?;
                    """
                expected_output: """
                    assert!(matches!(factory_loop.current_phase().await?, Phase::Completed));
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_rapid_event_sequence"
                scenario: "10 events sent in rapid succession"
                input:    "Alternating TestFailed and TestPassed events"
                expected: "All events processed in order, final state is consistent"
            },
            {
                name:     "test_concurrent_phase_queries"
                scenario: "Multiple tasks query phase simultaneously"
                input:    "10 concurrent current_phase() calls"
                expected: "All return consistent snapshot (may differ between calls)"
            },
            {
                name:     "test_shutdown_mid_transition"
                scenario: "Shutdown signal arrives during event processing"
                input:    "Send event, then immediate shutdown"
                expected: "Current event completes, then graceful shutdown"
            },
            {
                name:     "test_signal_bus_notification"
                scenario: "Phase transition occurs"
                input:    "TestPassed event in Implementing phase"
                expected: "Signal bus receives PhaseChanged notification"
            },
            {
                name:     "test_governor_budget_check"
                scenario: "Event arrives when budget is exactly at limit"
                input:    "Budget at 0, new event arrives"
                expected: "BudgetExhausted before event processing"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in factory_loop module"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/factory_loop.rs returns empty"
            },
            {
                name:     "test_invariant_result_types"
                verifies: "All public APIs return Result"
                test:     "cargo doc shows Result returns for spawn, send_event, current_phase"
            },
            {
                name:     "test_invariant_exhaustive_match"
                verifies: "All Phase and Event matches are exhaustive"
                test:     "Clippy warns on non-exhaustive matches (must have zero warnings)"
            },
            {
                name:     "test_invariant_terminal_absorbing"
                verifies: "Terminal phases ignore all events"
                test:     "Completed and Failed phases return same phase after any event"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_factory_loop_lifecycle"
            description: "Complete factory loop: spawn -> implement -> review -> push -> complete"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/factory_loop_e2e.rs"
                        content: """
                            use factory_core::factory_loop::{FactoryLoop, FactoryLoopConfig, Phase, Event};
                            use factory_core::signal_bus::SignalBus;
                            use factory_core::governor::ResourceGovernor;
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_full_factory_loop_lifecycle() -> Result<()> {
                                // Setup signal bus and governor
                                let signal_bus = SignalBus::spawn().await?;
                                let governor = ResourceGovernor::spawn(Default::default()).await?;

                                let config = FactoryLoopConfig::new()
                                    .max_iterations(10)
                                    .budget(Budget::tokens(100_000));

                                let task_context = TaskContext {
                                    bead_id: "test-bead".into(),
                                    signal_bus: signal_bus.clone(),
                                    governor: governor.clone(),
                                    ..Default::default()
                                };

                                let factory_loop = FactoryLoop::spawn(config, task_context).await?;

                                // Verify initial state
                                assert!(matches!(
                                    factory_loop.current_phase().await?,
                                    Phase::Implementing { iteration: 0 }
                                ));

                                // Simulate successful implementation
                                factory_loop.send_event(Event::TestPassed).await?;
                                assert!(matches!(factory_loop.current_phase().await?, Phase::Reviewing));

                                // Simulate successful review
                                factory_loop.send_event(Event::TestPassed).await?;
                                assert!(matches!(factory_loop.current_phase().await?, Phase::Pushing));

                                // Simulate successful push
                                factory_loop.send_event(Event::PushSuccess).await?;
                                assert!(matches!(factory_loop.current_phase().await?, Phase::Completed));
                                assert!(factory_loop.is_terminal().await?);

                                // Cleanup
                                signal_bus.shutdown().await?;
                                governor.shutdown().await?;

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
                command:    "moon run factory-core:test -- --test factory_loop_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_factory_loop_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/factory_loop.rs"
                        contains: "pub struct FactoryLoop"
                    },
                    {
                        path:     "crates/factory-core/src/factory_loop.rs"
                        contains: "pub enum Phase"
                    },
                    {
                        path:     "crates/factory-core/src/factory_loop.rs"
                        contains: "pub enum Event"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/factory_loop_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_factory_loop_with_failures"
                description: "Verify factory loop handles test failures and retries"
                steps: [
                    {action: "Spawn FactoryLoop with max_iterations=5", verify: "In Implementing phase"},
                    {action: "Send TestFailed event", verify: "Still Implementing, iteration=1"},
                    {action: "Send TestFailed event", verify: "Still Implementing, iteration=2"},
                    {action: "Send TestPassed event", verify: "Transitions to Reviewing"},
                    {action: "Send TestFailed event", verify: "Back to Implementing for fixes"},
                    {action: "Send TestPassed event", verify: "Reviewing again"},
                    {action: "Send TestPassed event", verify: "Pushing"},
                    {action: "Send PushSuccess event", verify: "Completed"},
                ]
            },
            {
                name:        "e2e_factory_loop_push_conflict_recovery"
                description: "Verify factory loop handles push conflicts with rebase"
                steps: [
                    {action: "Spawn FactoryLoop and get to Pushing phase", verify: "In Pushing phase"},
                    {action: "Send PushConflict event", verify: "Rebase initiated"},
                    {action: "Send RebaseSuccess event", verify: "Still Pushing, retry push"},
                    {action: "Send PushSuccess event", verify: "Completed"},
                ]
            },
            {
                name:        "e2e_factory_loop_rebase_conflict"
                description: "Verify factory loop returns to implementing on rebase conflict"
                steps: [
                    {action: "Spawn FactoryLoop and get to Pushing phase", verify: "In Pushing phase"},
                    {action: "Send PushConflict event", verify: "Rebase initiated"},
                    {action: "Send RebaseConflict event", verify: "Back to Implementing"},
                    {action: "Send TestPassed event", verify: "Reviewing"},
                    {action: "Complete remaining steps", verify: "Eventually Completed"},
                ]
            },
            {
                name:        "e2e_factory_loop_max_iterations"
                description: "Verify factory loop fails on max iterations"
                steps: [
                    {action: "Spawn FactoryLoop with max_iterations=3", verify: "In Implementing phase"},
                    {action: "Send TestFailed 3 times", verify: "Transitions to Failed"},
                    {action: "Verify failure reason", verify: "MaxIterationsReached"},
                    {action: "Send any event", verify: "Still Failed (terminal)"},
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
                task:      "Write test: test_spawn_initializes_implementing_phase"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Test that FactoryLoop spawns in Implementing { iteration: 0 }"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_test_passed_transitions_to_reviewing"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Test that TestPassed in Implementing -> Reviewing"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_test_failed_increments_iteration"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Test that TestFailed increments iteration counter"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_max_iterations_transitions_to_failed"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Test that reaching max iterations -> Failed"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_push_success_completes"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Test that PushSuccess in Pushing -> Completed"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_terminal_states_absorb_events"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Test that Completed and Failed ignore all events"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define Phase enum"
                file: "crates/factory-core/src/factory_loop.rs"
                what: """
                    /// Factory loop phases (state machine states)
                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum Phase {
                        /// Running implementation/test cycles
                        Implementing { iteration: u32 },
                        /// Awaiting review completion
                        Reviewing,
                        /// Attempting to push changes
                        Pushing,
                        /// Terminal success state
                        Completed,
                        /// Terminal failure state
                        Failed { reason: FailureReason },
                    }

                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum FailureReason {
                        MaxIterationsReached,
                        BudgetExhausted,
                        RebaseConflict,
                        Other(String),
                    }
                    """
                done_when:     "Enum compiles with all variants"
                patterns_to_use: ["Exhaustive enum matching", "Data-carrying variants for context"]
            },
            {
                task: "Define Event enum"
                file: "crates/factory-core/src/factory_loop.rs"
                what: """
                    /// Events that trigger phase transitions
                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum Event {
                        /// All tests passed successfully
                        TestPassed,
                        /// One or more tests failed
                        TestFailed,
                        /// Changes pushed successfully
                        PushSuccess,
                        /// Push failed due to conflict
                        PushConflict,
                        /// Rebase completed successfully
                        RebaseSuccess,
                        /// Rebase failed with conflicts
                        RebaseConflict,
                        /// Implementation iteration limit exceeded
                        MaxIterationsReached,
                        /// Token/time budget exhausted
                        BudgetExhausted,
                    }
                    """
                done_when:     "Enum compiles with all 8 variants"
                patterns_to_use: ["Exhaustive enum", "Unit variants for simple events"]
            },
            {
                task: "Implement transition function"
                file: "crates/factory-core/src/factory_loop.rs"
                what: """
                    /// Pure function: given current phase and event, return next phase
                    /// This is the core state machine logic - MUST be exhaustive
                    fn transition(phase: Phase, event: Event, config: &FactoryLoopConfig) -> Phase {
                        match (&phase, event) {
                            // Implementing phase transitions
                            (Phase::Implementing { iteration }, Event::TestPassed) => {
                                Phase::Reviewing
                            }
                            (Phase::Implementing { iteration }, Event::TestFailed) => {
                                let next_iteration = iteration.saturating_add(1);
                                if next_iteration >= config.max_iterations {
                                    Phase::Failed { reason: FailureReason::MaxIterationsReached }
                                } else {
                                    Phase::Implementing { iteration: next_iteration }
                                }
                            }

                            // Reviewing phase transitions
                            (Phase::Reviewing, Event::TestPassed) => Phase::Pushing,
                            (Phase::Reviewing, Event::TestFailed) => {
                                Phase::Implementing { iteration: 0 }  // Reset for review fixes
                            }

                            // Pushing phase transitions
                            (Phase::Pushing, Event::PushSuccess) => Phase::Completed,
                            (Phase::Pushing, Event::PushConflict) => Phase::Pushing,  // Trigger rebase
                            (Phase::Pushing, Event::RebaseSuccess) => Phase::Pushing,  // Retry push
                            (Phase::Pushing, Event::RebaseConflict) => {
                                Phase::Implementing { iteration: 0 }  // Resolve conflicts
                            }

                            // Budget exhaustion from any non-terminal phase
                            (Phase::Implementing { .. } | Phase::Reviewing | Phase::Pushing, Event::BudgetExhausted) => {
                                Phase::Failed { reason: FailureReason::BudgetExhausted }
                            }

                            // Terminal phases absorb all events
                            (Phase::Completed, _) => Phase::Completed,
                            (Phase::Failed { reason }, _) => Phase::Failed { reason: reason.clone() },

                            // Invalid transitions - remain in current phase (logged as warning)
                            _ => phase,
                        }
                    }
                    """
                done_when:     "Transition function compiles and handles all cases"
                patterns_to_use: [
                    "Pure function for testability",
                    "Pattern matching on tuple of (phase, event)",
                    "saturating_add to prevent overflow",
                    "Clone for terminal state preservation",
                ]
            },
            {
                task: "Define FactoryLoopConfig struct"
                file: "crates/factory-core/src/factory_loop.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub struct FactoryLoopConfig {
                        pub max_iterations: u32,
                        pub budget: Budget,
                        pub retry_policy: RetryPolicy,
                        pub history_limit: usize,
                    }

                    impl Default for FactoryLoopConfig {
                        fn default() -> Self {
                            Self {
                                max_iterations: 10,
                                budget: Budget::unlimited(),
                                retry_policy: RetryPolicy::exponential(3),
                                history_limit: 100,
                            }
                        }
                    }

                    impl FactoryLoopConfig {
                        pub fn new() -> Self { Self::default() }
                        pub fn max_iterations(mut self, n: u32) -> Self { self.max_iterations = n; self }
                        pub fn budget(mut self, b: Budget) -> Self { self.budget = b; self }
                        pub fn retry_policy(mut self, p: RetryPolicy) -> Self { self.retry_policy = p; self }
                    }
                    """
                done_when:     "Config compiles with builder pattern"
                patterns_to_use: ["Builder pattern", "Default trait", "Sensible defaults"]
            },
            {
                task: "Implement FactoryLoop actor"
                file: "crates/factory-core/src/factory_loop.rs"
                what: """
                    Implement ractor::Actor for FactoryLoop.
                    State includes current phase, history, config, signal_bus_ref, governor_ref.
                    Handle events via transition function, notify signal bus on transitions.
                    """
                done_when:     "Actor spawns and processes events"
                patterns_to_use: [
                    "ractor::Actor trait implementation",
                    "Separate state struct from actor struct",
                    "Notify signal bus on phase change",
                    "Query governor before processing",
                ]
            },
            {
                task: "Implement FactoryLoopRef handle"
                file: "crates/factory-core/src/factory_loop.rs"
                what: """
                    #[derive(Clone)]
                    pub struct FactoryLoopRef {
                        inner: ActorRef<FactoryLoopMsg>,
                    }

                    impl FactoryLoopRef {
                        pub async fn send_event(&self, event: Event) -> Result<()> {
                            self.inner.cast(FactoryLoopMsg::Event(event))
                                .map_err(Error::from)
                        }

                        pub async fn current_phase(&self) -> Result<Phase> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(FactoryLoopMsg::GetPhase { respond_to: tx })?;
                            rx.await.map_err(Error::from)
                        }

                        pub async fn is_terminal(&self) -> Result<bool> {
                            match self.current_phase().await? {
                                Phase::Completed | Phase::Failed { .. } => Ok(true),
                                _ => Ok(false),
                            }
                        }

                        pub async fn phase_history(&self) -> Result<Vec<PhaseTransition>> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(FactoryLoopMsg::GetHistory { respond_to: tx })?;
                            rx.await.map_err(Error::from)
                        }
                    }
                    """
                done_when:     "Handle provides type-safe API"
                patterns_to_use: [
                    "Actor handle pattern",
                    "oneshot channels for request-reply",
                    "Result return types",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export factory_loop module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod factory_loop;"
                done_when: "External crates can import factory_core::factory_loop"
            },
            {
                task:      "Add factory_loop errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "InvalidTransition, BudgetExhausted, MaxIterationsReached, FactoryLoopSpawnFailed variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Integrate with SignalBus"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Emit PhaseChanged signal on every transition"
                done_when: "Signal bus receives phase change notifications"
            },
            {
                task:      "Integrate with ResourceGovernor"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Check budget before processing each event"
                done_when: "BudgetExhausted emitted when budget depleted"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/factory_loop.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify exhaustive matching"
                done_when: "Clippy passes with no non-exhaustive warnings"
                commands: ["moon run :quick"]
                expected: "No match warnings"
            },
            {
                task:      "Run state machine fuzzer"
                done_when: "1000 random event sequences produce valid states"
                commands: [
                    "cargo test --release -- factory_loop_fuzz --ignored",
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
                symptom:      "FactoryLoop stuck in Implementing phase"
                likely_cause: "TestPassed event not being sent, or event lost"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/factory_loop.rs"
                        function:      "handle()"
                        what_to_check: "Is Event::TestPassed being matched correctly?"
                    },
                    {
                        file:          "calling code"
                        function:      "send_event()"
                        what_to_check: "Is the Result being checked? Event might be failing to send"
                    },
                ]
                fix_pattern: "Add tracing to transition function, verify event reaches actor"
            },
            {
                symptom:      "FactoryLoop panics on event"
                likely_cause: "Non-exhaustive match or unwrap in transition"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/factory_loop.rs"
                        function:      "transition()"
                        what_to_check: "Are all (Phase, Event) combinations handled?"
                    },
                ]
                fix_pattern: "Add catch-all arm that logs warning and stays in current phase"
            },
            {
                symptom:      "Phase history grows unbounded"
                likely_cause: "history_limit not being enforced"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/factory_loop.rs"
                        function:      "record_transition()"
                        what_to_check: "Is VecDeque being truncated after push?"
                    },
                ]
                fix_pattern: "Use VecDeque with pop_front when len > limit"
            },
            {
                symptom:      "Signal bus not receiving phase changes"
                likely_cause: "Signal emission failing silently"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/factory_loop.rs"
                        function:      "apply_transition()"
                        what_to_check: "Is signal bus cast result being logged on error?"
                    },
                ]
                fix_pattern: "Log signal bus errors but don't fail the transition"
            },
            {
                symptom:      "Iteration count jumps unexpectedly"
                likely_cause: "Transition called multiple times per event"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/factory_loop.rs"
                        function:      "handle()"
                        what_to_check: "Is transition called exactly once per event?"
                    },
                ]
                fix_pattern: "Ensure single transition call per event, use tracing to verify"
            },
        ]

        debugging_commands: [
            {
                scenario: "When phase transitions seem wrong"
                run:      "RUST_LOG=factory_core::factory_loop=trace cargo test"
                look_for: "Phase before, event, phase after for each transition"
            },
            {
                scenario: "When events seem to be lost"
                run:      "Add tracing::debug! at actor receive and handle"
                look_for: "Event received vs event handled counts"
            },
            {
                scenario: "When terminal states aren't terminal"
                run:      "cargo test -- test_terminal_states_absorb_events --nocapture"
                look_for: "Phase after each event in terminal state"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_spawn_initializes_implementing_phase passes",
            "[ ] test_test_passed_transitions_to_reviewing passes",
            "[ ] test_test_failed_increments_iteration passes",
            "[ ] test_max_iterations_transitions_to_failed passes",
            "[ ] test_push_success_completes passes",
            "[ ] test_terminal_states_absorb_events passes",
            "[ ] test_push_conflict_triggers_rebase passes",
            "[ ] test_rebase_conflict_returns_to_implementing passes",
            "[ ] test_budget_exhausted_fails passes",
            "[ ] test_signal_bus_notification passes",
            "[ ] E2E factory loop lifecycle test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Phase enum with 5 variants (Implementing, Reviewing, Pushing, Completed, Failed)",
            "[ ] Event enum with 8 variants",
            "[ ] FailureReason enum with context",
            "[ ] Transition function is pure and exhaustive",
            "[ ] FactoryLoopConfig with builder pattern",
            "[ ] FactoryLoop actor implementation",
            "[ ] FactoryLoopRef handle implementation",
            "[ ] Signal bus integration",
            "[ ] Resource governor integration",
            "[ ] Phase history with bounded size",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs explaining state machine",
            "[ ] Phase enum variants documented with semantics",
            "[ ] Event enum variants documented with triggers",
            "[ ] Transition table in doc comments",
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
                relevance: "Module exports - must add factory_loop"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add factory loop error variants"
            },
            {
                path:      "crates/factory-core/src/signal_bus.rs"
                relevance: "Signal bus for phase change notifications (factory-kou)"
            },
            {
                path:      "crates/factory-core/src/governor.rs"
                relevance: "Resource governor for budget management (factory-kjt)"
            },
            {
                path:      "crates/factory-core/src/supervisor.rs"
                relevance: "Supervisor for lifecycle management (factory-4pz)"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-kou"
                description: "Signal bus for pub/sub notifications"
                what_it_provides: "SignalBusRef for emitting phase change signals"
            },
            {
                bead_id:     "factory-kjt"
                description: "Resource governor for budget tracking"
                what_it_provides: "GovernorRef for budget checks and consumption tracking"
            },
            {
                bead_id:     "factory-00s"
                description: "Actor framework and Tokio runtime"
                what_it_provides: "ractor Actor trait, ActorRef, message passing"
            },
            {
                bead_id:     "factory-4pz"
                description: "Root supervisor with restart strategies"
                what_it_provides: "Supervisor for FactoryLoop lifecycle"
            },
        ]

        external_references: [
            "https://www.erlang.org/doc/design_principles/fsm.html - OTP gen_statem/FSM patterns",
            "https://docs.rs/state_machine_future/latest/ - Rust state machine patterns",
            "https://hoverbear.org/blog/rust-state-machine-pattern/ - Typestate pattern in Rust",
            "Original Gleam factory_loop.gleam - Reference implementation for port",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Actor Handle Pattern"
                example_location: "crates/factory-core/src/actor.rs (factory-00s)"
                how_to_apply:     "Separate actor struct from client-facing handle struct"
            },
            {
                pattern:          "Pure Transition Function"
                example_location: "State machine theory"
                how_to_apply:     "transition(state, event) -> state is a pure function for testability"
            },
            {
                pattern:          "Exhaustive Pattern Matching"
                example_location: "Rust compiler + clippy"
                how_to_apply:     "Match on (Phase, Event) tuple, compiler enforces exhaustiveness"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use tuple matching (phase, event) for exhaustive state machine",
            "Keep transition function pure - no side effects, easy to test",
            "Use saturating_add for iteration counter to prevent overflow",
            "Emit signals AFTER successful state update (not before)",
            "Check budget BEFORE processing each event",
            "Clone FailureReason in terminal state transitions to preserve context",
            "Use VecDeque for bounded phase history with efficient pop_front",
            "Add tracing spans for each phase transition",
            "Return current phase on invalid transition (with warning log)",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT allow unbounded phase history growth",
            "Do NOT process events in terminal states (just return same state)",
            "Do NOT mix state mutation with side effects in transition function",
            "Do NOT fail silently on signal bus errors (log them)",
        ]

        code_patterns: [
            {
                name:     "Phase Enum with Data"
                use_when: "Defining state machine phases"
                example:  """
                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum Phase {
                        Implementing { iteration: u32 },
                        Reviewing,
                        Pushing,
                        Completed,
                        Failed { reason: FailureReason },
                    }

                    impl Phase {
                        pub fn is_terminal(&self) -> bool {
                            matches!(self, Phase::Completed | Phase::Failed { .. })
                        }
                    }
                    """
            },
            {
                name:     "Pure Transition Function"
                use_when: "Implementing state machine core logic"
                example:  """
                    /// Pure transition function - no side effects
                    /// Returns (next_phase, optional_action)
                    fn transition(
                        phase: &Phase,
                        event: Event,
                        config: &FactoryLoopConfig,
                    ) -> (Phase, Option<Action>) {
                        match (phase, event) {
                            (Phase::Implementing { iteration }, Event::TestPassed) => {
                                (Phase::Reviewing, Some(Action::RequestReview))
                            }
                            (Phase::Implementing { iteration }, Event::TestFailed) => {
                                let next = iteration.saturating_add(1);
                                if next >= config.max_iterations {
                                    (
                                        Phase::Failed { reason: FailureReason::MaxIterationsReached },
                                        Some(Action::ReportFailure),
                                    )
                                } else {
                                    (Phase::Implementing { iteration: next }, Some(Action::FixTests))
                                }
                            }
                            // ... exhaustive matching
                        }
                    }
                    """
            },
            {
                name:     "Actor Message Handler"
                use_when: "Processing events in ractor actor"
                example:  """
                    impl Actor for FactoryLoop {
                        type Msg = FactoryLoopMsg;
                        type State = FactoryLoopState;
                        type Arguments = (FactoryLoopConfig, TaskContext);

                        async fn handle(
                            &self,
                            _myself: ActorRef<Self::Msg>,
                            message: Self::Msg,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            match message {
                                FactoryLoopMsg::Event(event) => {
                                    // Check budget first
                                    if !state.has_budget() {
                                        let event = Event::BudgetExhausted;
                                    }

                                    // Apply transition
                                    let old_phase = state.phase.clone();
                                    let (new_phase, action) = transition(&state.phase, event, &state.config);

                                    // Update state
                                    state.phase = new_phase.clone();
                                    state.record_transition(old_phase.clone(), new_phase.clone());

                                    // Notify signal bus (best effort)
                                    if old_phase != new_phase {
                                        let _ = state.signal_bus.emit(Signal::PhaseChanged {
                                            from: old_phase,
                                            to: new_phase,
                                        }).await;
                                    }

                                    Ok(())
                                }
                                FactoryLoopMsg::GetPhase { respond_to } => {
                                    let _ = respond_to.send(state.phase.clone());
                                    Ok(())
                                }
                                // ... other messages
                            }
                        }
                    }
                    """
            },
            {
                name:     "Bounded History Tracking"
                use_when: "Recording phase transitions"
                example:  """
                    #[derive(Debug, Clone)]
                    pub struct PhaseTransition {
                        pub from: Phase,
                        pub to: Phase,
                        pub timestamp: Instant,
                    }

                    impl FactoryLoopState {
                        fn record_transition(&mut self, from: Phase, to: Phase) {
                            self.history.push_back(PhaseTransition {
                                from,
                                to,
                                timestamp: Instant::now(),
                            });

                            // Maintain bounded size
                            while self.history.len() > self.config.history_limit {
                                self.history.pop_front();
                            }
                        }
                    }
                    """
            },
        ]
    }
}
