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

"factory-avd": #ValidBead & {
    // ============================================================================
    // BEAD: factory-avd - Implement heartbeat actor for health monitoring
    // ============================================================================

    id:              "factory-avd"
    title:           "Runtime: Implement heartbeat actor for health monitoring"
    type:            "feature"
    priority:        1
    effort_estimate: "2hr"
    labels:          ["runtime", "actors", "monitoring", "heartbeat", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL poll test status at configurable intervals using tokio::time::interval",
            "THE SYSTEM SHALL maintain a binary health state: Green (tests passing) or Red (tests failing)",
            "THE SYSTEM SHALL broadcast TestPassing signal on Red-to-Green transition",
            "THE SYSTEM SHALL broadcast TestFailure signal on Green-to-Red transition",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN a tick interval fires"
                shall:   "THE SYSTEM SHALL spawn a test runner task to check golden master status"
            },
            {
                trigger: "WHEN test runner completes with exit code 0"
                shall:   "THE SYSTEM SHALL transition state to Green if not already Green"
            },
            {
                trigger: "WHEN test runner completes with non-zero exit code"
                shall:   "THE SYSTEM SHALL transition state to Red if not already Red"
            },
            {
                trigger: "WHEN state transitions from Green to Red"
                shall:   "THE SYSTEM SHALL broadcast TestFailure signal to signal bus"
            },
            {
                trigger: "WHEN state transitions from Red to Green"
                shall:   "THE SYSTEM SHALL broadcast TestPassing signal to signal bus"
            },
            {
                trigger: "WHEN shutdown message is received"
                shall:   "THE SYSTEM SHALL cancel pending timer and stop gracefully"
            },
            {
                trigger: "WHEN GetStatus request is received"
                shall:   "THE SYSTEM SHALL respond with current health status"
            },
        ]

        state_driven: [
            {
                state: "WHILE the heartbeat actor is running"
                shall: "THE SYSTEM SHALL schedule the next tick after each test completion"
            },
            {
                state: "WHILE a test is running"
                shall: "THE SYSTEM SHALL not schedule another concurrent test"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL ignore further tick messages"
            },
        ]

        unwanted: [
            {
                condition: "IF test runner hangs indefinitely"
                shall_not: "THE SYSTEM SHALL NOT block the heartbeat actor mailbox"
                because:   "Test execution must be non-blocking to allow shutdown and status queries"
            },
            {
                condition: "IF signal bus is unavailable"
                shall_not: "THE SYSTEM SHALL NOT panic or crash"
                because:   "Signal delivery failure should not affect health monitoring"
            },
            {
                condition: "IF tick interval is extremely short (< 100ms)"
                shall_not: "THE SYSTEM SHALL NOT spawn overlapping test runs"
                because:   "Concurrent test runs waste resources and produce inconsistent results"
            },
            {
                condition: "IF test status unchanged between polls"
                shall_not: "THE SYSTEM SHALL NOT broadcast redundant signals"
                because:   "Only transitions should trigger signals to avoid message flooding"
            },
        ]

        complex: [
            {
                state:   "WHILE a test is running"
                trigger: "WHEN shutdown is requested"
                shall:   "THE SYSTEM SHALL abort the running test and exit cleanly"
            },
            {
                state:   "WHILE shutting down"
                trigger: "WHEN tick fires"
                shall:   "THE SYSTEM SHALL ignore the tick and continue shutdown"
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
                    field:           "HeartbeatConfig"
                    type:            "Struct"
                    constraints:     "Must specify interval_ms, test_cmd, golden_master_path"
                    example_valid:   "HeartbeatConfig { interval_ms: 5000, test_cmd: \"moon run :test\", golden_master_path: PathBuf::from(\".\") }"
                    example_invalid: "HeartbeatConfig { interval_ms: 0, .. } // Zero interval"
                },
                {
                    field:           "SignalBusRef"
                    type:            "ActorRef<SignalBusMessage>"
                    constraints:     "Must be a valid reference to a running signal bus actor"
                    example_valid:   "signal_bus_ref from supervisor"
                    example_invalid: "dropped or terminated ActorRef"
                },
            ]
            system_state: [
                "Tokio runtime initialized",
                "ractor framework available (factory-00s completed)",
                "Signal bus actor running (factory-kou completed)",
                "Root supervisor available (factory-4pz completed)",
            ]
        }

        postconditions: {
            state_changes: [
                "Heartbeat actor spawned and running",
                "Periodic polling scheduled via tokio interval",
                "Initial state set to Red (conservative assumption)",
            ]
            return_guarantees: [
                {
                    field:     "HeartbeatRef"
                    guarantee: "Always valid while held; can query status and shutdown"
                },
                {
                    field:     "Heartbeat::spawn()"
                    guarantee: "Returns Result<HeartbeatRef, SpawnError>"
                },
                {
                    field:     "HeartbeatRef::get_status()"
                    guarantee: "Returns Result<TestStatus, CallError> with timeout"
                },
                {
                    field:     "HeartbeatRef::shutdown()"
                    guarantee: "Returns Result<(), SendError> immediately (cast)"
                },
                {
                    field:     "HeartbeatRef::tick()"
                    guarantee: "Returns Result<(), SendError> for manual trigger"
                },
            ]
            side_effects: [
                "Test command executed in subprocess",
                "TestFailure/TestPassing signals broadcast on transitions",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "State is either Green or Red, never undefined",
            "Only one test runs at a time per heartbeat instance",
            "Signals are only broadcast on state transitions, not on every poll",
            "Shutdown always completes, even if test is running",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Test command not found or fails to execute"
                prevention:  "Validate test_cmd exists at startup, treat spawn failures as Red"
                test_for_it: "test_invalid_test_command_returns_red"
            },
            {
                failure:     "Signal bus dies while heartbeat running"
                prevention:  "Use try_cast for signal delivery, log errors but don't crash"
                test_for_it: "test_continues_without_signal_bus"
            },
            {
                failure:     "tokio::interval drift causes timing issues"
                prevention:  "Use interval_at with MissedTickBehavior::Skip"
                test_for_it: "test_interval_stability_under_load"
            },
        ]

        usability_failures: [
            {
                failure:     "Status query blocks indefinitely"
                prevention:  "Use timeout on all call operations (5s default)"
                test_for_it: "test_get_status_with_timeout"
            },
            {
                failure:     "Manual tick ignored during test run"
                prevention:  "Document that tick() is queued, not executed immediately"
                test_for_it: "test_manual_tick_queued_during_test"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Race between test completion and shutdown"
                prevention:  "Use shutting_down flag checked before state transitions"
                test_for_it: "test_no_signal_after_shutdown_started"
            },
            {
                failure:     "Progress buffer grows unbounded"
                prevention:  "Cap buffer at max_buffer_size, drop oldest entries"
                test_for_it: "test_progress_buffer_bounded"
            },
            {
                failure:     "State inconsistent after rapid transitions"
                prevention:  "Process test results sequentially, one at a time"
                test_for_it: "test_rapid_transitions_consistent"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_heartbeat_starts_with_red_state"
                given: "A valid HeartbeatConfig and signal bus"
                when:  "Heartbeat::spawn() is called"
                then: [
                    "Returns Ok(HeartbeatRef)",
                    "Initial status is Red (conservative)",
                    "First tick is scheduled",
                ]
                real_input: """
                    let config = HeartbeatConfig {
                        interval_ms: 5000,
                        test_cmd: "moon run :test".to_string(),
                        golden_master_path: PathBuf::from("."),
                    };

                    let heartbeat = Heartbeat::spawn(config, signal_bus.clone()).await?;
                    """
                expected_output: """
                    let status = heartbeat.get_status().await?;
                    assert_eq!(status, TestStatus::Red);
                    """
            },
            {
                name:  "test_heartbeat_transitions_to_green_on_passing_tests"
                given: "A heartbeat actor with Red state and passing test command"
                when:  "Tick fires and test passes"
                then: [
                    "State transitions to Green",
                    "TestPassing signal broadcast",
                    "Next tick scheduled",
                ]
                real_input: """
                    let config = HeartbeatConfig {
                        interval_ms: 100,
                        test_cmd: "true".to_string(),  // Always passes
                        golden_master_path: PathBuf::from("."),
                    };

                    let (signal_tx, mut signal_rx) = mpsc::channel(10);
                    let heartbeat = Heartbeat::spawn(config, mock_signal_bus(signal_tx)).await?;

                    // Wait for first tick
                    tokio::time::sleep(Duration::from_millis(200)).await;
                    """
                expected_output: """
                    let status = heartbeat.get_status().await?;
                    assert_eq!(status, TestStatus::Green);

                    // Should have received TestPassing signal
                    let signal = signal_rx.recv().await;
                    assert!(matches!(signal, Some(Signal::TestPassing)));
                    """
            },
            {
                name:  "test_heartbeat_transitions_to_red_on_failing_tests"
                given: "A heartbeat actor with Green state and failing test command"
                when:  "Tick fires and test fails"
                then: [
                    "State transitions to Red",
                    "TestFailure signal broadcast",
                    "Next tick scheduled",
                ]
                real_input: """
                    let config = HeartbeatConfig {
                        interval_ms: 100,
                        test_cmd: "false".to_string(),  // Always fails
                        golden_master_path: PathBuf::from("."),
                    };

                    // First get to Green state, then fail
                    let heartbeat = spawn_heartbeat_in_green_state().await?;
                    heartbeat.set_test_cmd("false").await?;

                    // Wait for tick
                    tokio::time::sleep(Duration::from_millis(200)).await;
                    """
                expected_output: """
                    let status = heartbeat.get_status().await?;
                    assert_eq!(status, TestStatus::Red);

                    // Should have received TestFailure signal
                    let signal = signal_rx.recv().await;
                    assert!(matches!(signal, Some(Signal::TestFailure)));
                    """
            },
            {
                name:  "test_heartbeat_no_signal_on_unchanged_state"
                given: "A heartbeat actor with Green state and passing tests"
                when:  "Multiple ticks fire with tests still passing"
                then: [
                    "State remains Green",
                    "No additional signals broadcast",
                ]
                real_input: """
                    let config = HeartbeatConfig {
                        interval_ms: 100,
                        test_cmd: "true".to_string(),
                        golden_master_path: PathBuf::from("."),
                    };

                    let (signal_tx, mut signal_rx) = mpsc::channel(10);
                    let heartbeat = Heartbeat::spawn(config, mock_signal_bus(signal_tx)).await?;

                    // Wait for multiple ticks
                    tokio::time::sleep(Duration::from_millis(500)).await;
                    """
                expected_output: """
                    // Should only have received ONE TestPassing signal (initial transition)
                    let first = signal_rx.try_recv();
                    assert!(first.is_ok());

                    let second = signal_rx.try_recv();
                    assert!(second.is_err());  // No more signals
                    """
            },
            {
                name:  "test_heartbeat_graceful_shutdown"
                given: "A running heartbeat actor"
                when:  "Shutdown is called"
                then: [
                    "Timer cancelled",
                    "Actor exits cleanly",
                    "No more ticks processed",
                ]
                real_input: """
                    let heartbeat = Heartbeat::spawn(config, signal_bus).await?;
                    assert!(heartbeat.is_running().await);

                    heartbeat.shutdown().await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: """
                    assert!(!heartbeat.is_running().await);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_invalid_test_command_returns_red"
                given: "A heartbeat with non-existent test command"
                when:  "Tick fires"
                then: [
                    "State remains/becomes Red",
                    "Error logged",
                    "Actor continues running",
                ]
                real_input: """
                    let config = HeartbeatConfig {
                        interval_ms: 100,
                        test_cmd: "/nonexistent/command".to_string(),
                        golden_master_path: PathBuf::from("."),
                    };

                    let heartbeat = Heartbeat::spawn(config, signal_bus).await?;
                    tokio::time::sleep(Duration::from_millis(200)).await;
                    """
                expected_output: null
                expected_error: """
                    let status = heartbeat.get_status().await?;
                    assert_eq!(status, TestStatus::Red);
                    assert!(heartbeat.is_running().await);  // Didn't crash
                    """
            },
            {
                name:  "test_get_status_timeout"
                given: "A heartbeat actor that is slow to respond"
                when:  "get_status() is called with short timeout"
                then: [
                    "Returns Err(CallError::Timeout)",
                    "Does not hang indefinitely",
                ]
                real_input: """
                    let result = tokio::time::timeout(
                        Duration::from_millis(100),
                        slow_heartbeat.get_status()
                    ).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(result.is_err());  // Timed out
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_shutdown_during_test_run"
                scenario: "Shutdown requested while test command is running"
                input:    "Long-running test command, shutdown called mid-execution"
                expected: "Test aborted, actor exits cleanly within timeout"
            },
            {
                name:     "test_rapid_state_transitions"
                scenario: "Tests alternate pass/fail rapidly"
                input:    "50ms interval, alternating pass/fail test results"
                expected: "Each transition broadcasts exactly one signal"
            },
            {
                name:     "test_manual_tick_during_test"
                scenario: "Manual tick() called while test is already running"
                input:    "Call tick() while previous test still executing"
                expected: "Tick is queued, not executed concurrently"
            },
            {
                name:     "test_signal_bus_unavailable"
                scenario: "Signal bus actor dies after heartbeat starts"
                input:    "Drop signal bus reference, let heartbeat continue"
                expected: "Heartbeat continues polling, logs signal delivery failures"
            },
            {
                name:     "test_zero_interval_rejected"
                scenario: "Configuration with interval_ms = 0"
                input:    "HeartbeatConfig { interval_ms: 0, .. }"
                expected: "Spawn returns Err(InvalidConfig)"
            },
            {
                name:     "test_progress_buffer_overflow"
                scenario: "More progress chunks than buffer size"
                input:    "Send 1500 progress chunks (buffer max 1000)"
                expected: "Oldest 500 dropped, newest 1000 retained"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in heartbeat module"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/heartbeat.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public heartbeat APIs return Result"
                test:     "cargo doc shows Result returns for spawn, get_status, tick, shutdown"
            },
            {
                name:     "test_invariant_single_concurrent_test"
                verifies: "Only one test runs at a time"
                test:     "Concurrent tick calls don't spawn multiple test processes"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_heartbeat_lifecycle"
            description: "Complete heartbeat lifecycle: spawn -> poll -> transitions -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/heartbeat_e2e.rs"
                        content: """
                            use factory_core::heartbeat::{Heartbeat, HeartbeatConfig, TestStatus};
                            use factory_core::signal_bus::{SignalBus, Signal};
                            use factory_core::error::Result;
                            use std::time::Duration;
                            use std::path::PathBuf;
                            use tokio::sync::mpsc;

                            #[tokio::test]
                            async fn test_full_heartbeat_lifecycle() -> Result<()> {
                                // Start signal bus
                                let signal_bus = SignalBus::spawn().await?;

                                // Configure heartbeat with fast interval
                                let config = HeartbeatConfig {
                                    interval_ms: 100,
                                    test_cmd: "true".to_string(),  // Passes
                                    golden_master_path: PathBuf::from("."),
                                };

                                // Spawn heartbeat
                                let heartbeat = Heartbeat::spawn(config, signal_bus.clone()).await?;

                                // Initial state is Red
                                assert_eq!(heartbeat.get_status().await?, TestStatus::Red);

                                // Wait for transition to Green
                                tokio::time::sleep(Duration::from_millis(200)).await;
                                assert_eq!(heartbeat.get_status().await?, TestStatus::Green);

                                // Graceful shutdown
                                heartbeat.shutdown().await?;
                                tokio::time::sleep(Duration::from_millis(100)).await;
                                assert!(!heartbeat.is_running().await);

                                // Cleanup signal bus
                                signal_bus.shutdown().await?;

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
                command:    "moon run factory-core:test -- --test heartbeat_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_heartbeat_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/heartbeat.rs"
                        contains: "pub struct Heartbeat"
                    },
                    {
                        path:     "crates/factory-core/src/heartbeat.rs"
                        contains: "pub enum TestStatus"
                    },
                    {
                        path:     "crates/factory-core/src/heartbeat.rs"
                        contains: "pub struct HeartbeatConfig"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/heartbeat_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_heartbeat_signal_integration"
                description: "Verify heartbeat signals are received by subscribers"
                steps: [
                    {action: "Spawn signal bus", verify: "Bus running"},
                    {action: "Subscribe test listener to TestPassing/TestFailure", verify: "Subscribed"},
                    {action: "Spawn heartbeat with passing tests", verify: "Heartbeat running"},
                    {action: "Wait for Red->Green transition", verify: "Listener receives TestPassing"},
                    {action: "Change to failing tests", verify: "Listener receives TestFailure"},
                    {action: "Shutdown heartbeat", verify: "Actor stopped cleanly"},
                ]
            },
            {
                name:        "e2e_heartbeat_supervisor_integration"
                description: "Verify heartbeat works under supervisor"
                steps: [
                    {action: "Spawn root supervisor with heartbeat as child", verify: "Supervisor running"},
                    {action: "Verify heartbeat polling", verify: "Status queries work"},
                    {action: "Kill heartbeat process", verify: "Supervisor restarts heartbeat"},
                    {action: "Verify new heartbeat instance polling", verify: "Status queries still work"},
                    {action: "Shutdown supervisor", verify: "All actors stopped"},
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
                task:      "Write test: test_heartbeat_starts_with_red_state"
                file:      "crates/factory-core/src/heartbeat.rs"
                what:      "Test that heartbeat initial state is Red"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_heartbeat_transitions_to_green_on_passing_tests"
                file:      "crates/factory-core/src/heartbeat.rs"
                what:      "Test Red->Green transition broadcasts TestPassing"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_heartbeat_transitions_to_red_on_failing_tests"
                file:      "crates/factory-core/src/heartbeat.rs"
                what:      "Test Green->Red transition broadcasts TestFailure"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_heartbeat_no_signal_on_unchanged_state"
                file:      "crates/factory-core/src/heartbeat.rs"
                what:      "Test that repeated same status doesn't broadcast"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_heartbeat_graceful_shutdown"
                file:      "crates/factory-core/src/heartbeat.rs"
                what:      "Test that shutdown cancels timer and stops cleanly"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define TestStatus enum"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    pub enum TestStatus {
                        Green,
                        Red,
                    }
                    """
                done_when:     "Enum compiles"
                patterns_to_use: ["Simple enum for binary state"]
            },
            {
                task: "Define HeartbeatConfig struct"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub struct HeartbeatConfig {
                        pub interval_ms: u64,
                        pub test_cmd: String,
                        pub golden_master_path: PathBuf,
                    }

                    impl HeartbeatConfig {
                        pub fn validate(&self) -> Result<()> {
                            if self.interval_ms == 0 {
                                return Err(Error::InvalidConfig("interval_ms must be > 0".into()));
                            }
                            Ok(())
                        }
                    }
                    """
                done_when:     "Struct compiles with validation"
                patterns_to_use: ["Validation in separate method"]
            },
            {
                task: "Define HeartbeatMessage enum"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    pub enum HeartbeatMessage {
                        Tick,
                        TestResult(TestStatus),
                        GetStatus { reply_to: RpcReplyPort<TestStatus> },
                        StreamProgress { task_id: String, chunk: String },
                        Shutdown,
                    }
                    """
                done_when:     "Enum compiles"
                patterns_to_use: [
                    "RpcReplyPort for call operations",
                    "Separate message for async test results",
                ]
            },
            {
                task: "Define HeartbeatState struct"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    struct HeartbeatState {
                        config: HeartbeatConfig,
                        last_status: TestStatus,
                        signal_bus: ActorRef<SignalBusMessage>,
                        progress_buffer: VecDeque<(String, String)>,
                        shutting_down: bool,
                        test_runner_handle: Option<JoinHandle<TestStatus>>,
                    }
                    """
                done_when:     "Struct compiles"
                patterns_to_use: [
                    "VecDeque for bounded buffer",
                    "Option<JoinHandle> for async task tracking",
                    "Flag for shutdown coordination",
                ]
            },
            {
                task: "Implement Heartbeat actor"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    Implement ractor::Actor trait for Heartbeat.
                    Handle Tick by spawning test runner task.
                    Handle TestResult by updating state and broadcasting.
                    Handle GetStatus with RPC reply.
                    Handle Shutdown by cancelling timer and stopping.
                    """
                done_when:     "Actor compiles and handles all messages"
                patterns_to_use: [
                    "ractor::Actor trait implementation",
                    "tokio::spawn for non-blocking test execution",
                    "tokio::time::interval for periodic polling",
                    "myself.cast for self-messaging results",
                ]
            },
            {
                task: "Implement run_tests function"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    async fn run_tests(config: &HeartbeatConfig) -> TestStatus {
                        match tokio::process::Command::new("sh")
                            .arg("-c")
                            .arg(&config.test_cmd)
                            .current_dir(&config.golden_master_path)
                            .output()
                            .await
                        {
                            Ok(output) if output.status.success() => TestStatus::Green,
                            _ => TestStatus::Red,
                        }
                    }
                    """
                done_when:     "Test execution works"
                patterns_to_use: [
                    "tokio::process for async command execution",
                    "Match on exit status for Green/Red",
                    "Treat any error as Red (fail-safe)",
                ]
            },
            {
                task: "Implement update_status function"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    Detect state transitions (Green->Red, Red->Green).
                    Broadcast appropriate signal on transition.
                    Log transitions for observability.
                    """
                done_when:     "State transitions work with signal broadcasting"
                patterns_to_use: [
                    "Pattern match on (old, new) tuple",
                    "try_cast for resilient signal delivery",
                    "tracing for logging",
                ]
            },
            {
                task: "Implement interval scheduling"
                file: "crates/factory-core/src/heartbeat.rs"
                what: """
                    Use tokio::time::interval in actor's pre_start or via message loop.
                    Send Tick messages to self at configured interval.
                    Cancel interval on shutdown.
                    """
                done_when:     "Periodic ticking works"
                patterns_to_use: [
                    "tokio::time::interval with MissedTickBehavior::Skip",
                    "myself.cast(HeartbeatMessage::Tick) from interval task",
                    "AbortHandle for cancellation",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export heartbeat module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod heartbeat;"
                done_when: "External crates can import factory_core::heartbeat"
            },
            {
                task:      "Add heartbeat errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "HeartbeatSpawnFailed, InvalidConfig, TestExecutionFailed variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Implement HeartbeatRef handle"
                file:      "crates/factory-core/src/heartbeat.rs"
                what:      "Client-facing API: get_status, tick, stream_progress, shutdown"
                done_when: "All HeartbeatRef methods work"
            },
            {
                task:      "Add ChildSpec for supervisor integration"
                file:      "crates/factory-core/src/heartbeat.rs"
                what:      "Heartbeat::child_spec(config, signal_bus) returns ChildSpec"
                done_when: "Heartbeat can be added as supervisor child"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/heartbeat.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification"
                done_when: "Heartbeat polls and broadcasts correctly"
                commands: [
                    "cargo run --example heartbeat_demo",
                ]
                expected: "Heartbeat transitions visible, signals logged"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Heartbeat not polling (no ticks)"
                likely_cause: "Interval task not spawned or died"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/heartbeat.rs"
                        function:      "pre_start() or post_start()"
                        what_to_check: "Is interval spawned? Is it sending Tick messages?"
                    },
                    {
                        file:          "crates/factory-core/src/heartbeat.rs"
                        function:      "handle()"
                        what_to_check: "Is Tick message being processed?"
                    },
                ]
                fix_pattern: "Ensure interval task is spawned in post_start and sends to myself"
            },
            {
                symptom:      "Signals not being broadcast"
                likely_cause: "Signal bus reference invalid or update_status not calling broadcast"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/heartbeat.rs"
                        function:      "update_status()"
                        what_to_check: "Is broadcast called on transition?"
                    },
                    {
                        file:          "crates/factory-core/src/heartbeat.rs"
                        function:      "update_status()"
                        what_to_check: "Is signal_bus.cast returning Ok?"
                    },
                ]
                fix_pattern: "Add tracing::debug! before and after broadcast calls"
            },
            {
                symptom:      "Test always returns Red"
                likely_cause: "Test command failing or not finding working directory"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/heartbeat.rs"
                        function:      "run_tests()"
                        what_to_check: "Is current_dir set correctly? Does command exist?"
                    },
                ]
                fix_pattern: "Log command output, verify path exists, test command manually"
            },
            {
                symptom:      "Shutdown hangs"
                likely_cause: "Test runner task not being cancelled"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/heartbeat.rs"
                        function:      "handle_shutdown() or post_stop()"
                        what_to_check: "Is test_runner_handle being aborted?"
                    },
                ]
                fix_pattern: "Call handle.abort() on shutdown, await with timeout"
            },
            {
                symptom:      "Concurrent test runs"
                likely_cause: "Not checking if test is already running before spawning"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/heartbeat.rs"
                        function:      "handle_tick()"
                        what_to_check: "Is test_runner_handle.is_some() checked?"
                    },
                ]
                fix_pattern: "Skip tick if test_runner_handle is Some"
            },
        ]

        debugging_commands: [
            {
                scenario: "When ticks are not firing"
                run:      "RUST_LOG=factory_core::heartbeat=trace cargo test"
                look_for: "Tick messages, interval creation"
            },
            {
                scenario: "When signals are not broadcast"
                run:      "RUST_LOG=factory_core::heartbeat=debug,factory_core::signal_bus=debug cargo test"
                look_for: "Transition detection, broadcast calls, signal bus receipts"
            },
            {
                scenario: "When test execution fails"
                run:      "Add tracing::debug!(output = ?output) in run_tests"
                look_for: "Command stdout/stderr, exit code"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_heartbeat_starts_with_red_state passes",
            "[ ] test_heartbeat_transitions_to_green_on_passing_tests passes",
            "[ ] test_heartbeat_transitions_to_red_on_failing_tests passes",
            "[ ] test_heartbeat_no_signal_on_unchanged_state passes",
            "[ ] test_heartbeat_graceful_shutdown passes",
            "[ ] test_invalid_test_command_returns_red passes",
            "[ ] test_shutdown_during_test_run passes",
            "[ ] test_signal_bus_unavailable passes",
            "[ ] E2E heartbeat lifecycle test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] TestStatus enum with Green, Red",
            "[ ] HeartbeatConfig with validation",
            "[ ] HeartbeatMessage enum with all variants",
            "[ ] HeartbeatState with all required fields",
            "[ ] Heartbeat actor implementation",
            "[ ] HeartbeatRef handle implementation",
            "[ ] run_tests async function",
            "[ ] update_status with transition detection",
            "[ ] Interval scheduling with cancellation",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs explaining heartbeat purpose",
            "[ ] TestStatus enum variants documented",
            "[ ] HeartbeatConfig fields documented with examples",
            "[ ] Example usage in doc comments",
            "[ ] Signal transition behavior documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add heartbeat"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add heartbeat error variants"
            },
            {
                path:      "crates/factory-core/src/signal_bus.rs"
                relevance: "Signal bus for broadcasting TestPassing/TestFailure"
            },
            {
                path:      "crates/factory-core/src/supervisor.rs"
                relevance: "Supervisor for managing heartbeat lifecycle"
            },
            {
                path:      "src/heartbeat.gleam"
                relevance: "Original Gleam implementation to port"
            },
            {
                path:      "BEAM-PATTERNS-RESEARCH.md"
                relevance: "OTP patterns including periodic polling"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-00s"
                description: "Actor framework and Tokio runtime must be in place"
                what_it_provides: "ractor Actor trait, ActorRef, message passing"
            },
            {
                bead_id:     "factory-4pz"
                description: "Root supervisor for managing heartbeat lifecycle"
                what_it_provides: "Supervision tree, restart strategies, ChildSpec"
            },
            {
                bead_id:     "factory-kou"
                description: "Signal bus for broadcasting test status signals"
                what_it_provides: "Signal enum with TestPassing/TestFailure, broadcast API"
            },
        ]

        external_references: [
            "https://docs.rs/tokio/latest/tokio/time/fn.interval.html - Tokio interval documentation",
            "https://docs.rs/tokio/latest/tokio/process/struct.Command.html - Async command execution",
            "https://github.com/slawlor/ractor - Ractor actor framework",
            "https://ryhl.io/blog/actors-with-tokio/ - Alice Ryhl's actor patterns",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Actor Handle Pattern"
                example_location: "src/heartbeat.gleam:get_status()"
                how_to_apply:     "HeartbeatRef wraps ActorRef with typed methods"
            },
            {
                pattern:          "Periodic Polling with Tokio"
                example_location: "tokio::time::interval"
                how_to_apply:     "Spawn task with interval, send self Tick messages"
            },
            {
                pattern:          "State Transition Detection"
                example_location: "src/heartbeat.gleam:update_status()"
                how_to_apply:     "Match on (old_status, new_status) tuple for transitions"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Port patterns from heartbeat.gleam faithfully",
            "Use tokio::time::interval with MissedTickBehavior::Skip to prevent drift",
            "Use tokio::process::Command for non-blocking test execution",
            "Use tokio::spawn for test runner to avoid blocking actor mailbox",
            "Send TestResult back to self via myself.cast() after test completes",
            "Match on (old, new) tuple to detect transitions precisely",
            "Use try_cast for signal bus delivery to handle bus failures gracefully",
            "Track test_runner_handle to prevent concurrent test runs",
            "Use AbortHandle to cancel running tests on shutdown",
            "Start in Red state (conservative, fail-safe assumption)",
            "Document signal transition behavior clearly",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT block the actor mailbox with synchronous test execution",
            "Do NOT spawn concurrent test runs",
            "Do NOT broadcast signals when state hasn't changed",
            "Do NOT leave test processes running on shutdown",
            "Do NOT use std::process::Command (use tokio::process)",
        ]

        code_patterns: [
            {
                name:     "TestStatus Enum"
                use_when: "Defining health state"
                example:  """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    pub enum TestStatus {
                        Green,
                        Red,
                    }
                    """
            },
            {
                name:     "Periodic Interval with Self-Messaging"
                use_when: "Setting up periodic polling"
                example:  """
                    async fn post_start(
                        &self,
                        myself: ActorRef<Self::Msg>,
                        state: &mut Self::State,
                    ) -> Result<(), ActorProcessingErr> {
                        let interval_ms = state.config.interval_ms;
                        let myself_clone = myself.clone();

                        let handle = tokio::spawn(async move {
                            let mut interval = tokio::time::interval(
                                Duration::from_millis(interval_ms)
                            );
                            interval.set_missed_tick_behavior(MissedTickBehavior::Skip);

                            loop {
                                interval.tick().await;
                                if myself_clone.cast(HeartbeatMessage::Tick).is_err() {
                                    break;  // Actor stopped
                                }
                            }
                        });

                        state.interval_handle = Some(handle);
                        Ok(())
                    }
                    """
            },
            {
                name:     "Non-blocking Test Execution"
                use_when: "Running tests without blocking mailbox"
                example:  """
                    fn handle_tick(
                        myself: ActorRef<HeartbeatMessage>,
                        state: &mut HeartbeatState,
                    ) -> Result<(), ActorProcessingErr> {
                        // Skip if already running a test
                        if state.test_runner_handle.is_some() {
                            return Ok(());
                        }

                        if state.shutting_down {
                            return Ok(());
                        }

                        let config = state.config.clone();
                        let myself_clone = myself.clone();

                        let handle = tokio::spawn(async move {
                            let status = run_tests(&config).await;
                            let _ = myself_clone.cast(HeartbeatMessage::TestResult(status));
                        });

                        state.test_runner_handle = Some(handle);
                        Ok(())
                    }
                    """
            },
            {
                name:     "State Transition Detection"
                use_when: "Detecting and handling state changes"
                example:  """
                    fn update_status(
                        state: &mut HeartbeatState,
                        new_status: TestStatus,
                    ) -> Option<Signal> {
                        let signal = match (state.last_status, new_status) {
                            (TestStatus::Green, TestStatus::Red) => {
                                tracing::info!("Health transition: Green -> Red");
                                Some(Signal::TestFailure)
                            }
                            (TestStatus::Red, TestStatus::Green) => {
                                tracing::info!("Health transition: Red -> Green");
                                Some(Signal::TestPassing)
                            }
                            _ => None,  // No transition
                        };

                        state.last_status = new_status;
                        signal
                    }
                    """
            },
            {
                name:     "Async Test Execution"
                use_when: "Running test command"
                example:  """
                    async fn run_tests(config: &HeartbeatConfig) -> TestStatus {
                        let result = tokio::process::Command::new("sh")
                            .arg("-c")
                            .arg(&config.test_cmd)
                            .current_dir(&config.golden_master_path)
                            .stdout(std::process::Stdio::null())
                            .stderr(std::process::Stdio::null())
                            .status()
                            .await;

                        match result {
                            Ok(status) if status.success() => TestStatus::Green,
                            Ok(_) => TestStatus::Red,  // Non-zero exit
                            Err(e) => {
                                tracing::warn!(error = %e, "Test execution failed");
                                TestStatus::Red
                            }
                        }
                    }
                    """
            },
            {
                name:     "Graceful Shutdown with Test Cancellation"
                use_when: "Handling shutdown message"
                example:  """
                    fn handle_shutdown(state: &mut HeartbeatState) -> Result<(), ActorProcessingErr> {
                        state.shutting_down = true;

                        // Cancel interval
                        if let Some(handle) = state.interval_handle.take() {
                            handle.abort();
                        }

                        // Cancel running test
                        if let Some(handle) = state.test_runner_handle.take() {
                            handle.abort();
                        }

                        tracing::info!("Heartbeat shutting down");
                        Err(ActorProcessingErr::Stop)
                    }
                    """
            },
        ]
    }
}
