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

"factory-1yj": #ValidBead & {
    // ============================================================================
    // BEAD: factory-1yj - Implement signal handler for SIGINT/SIGTERM graceful shutdown
    // ============================================================================

    id:              "factory-1yj"
    title:           "Signal Handler: Implement SIGINT/SIGTERM graceful shutdown"
    type:            "feature"
    priority:        0
    effort_estimate: "2hr"
    labels:          ["signal-handling", "shutdown", "actors", "P0"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use tokio::signal for async signal handling",
            "THE SYSTEM SHALL catch SIGINT (Ctrl+C) and SIGTERM signals",
            "THE SYSTEM SHALL broadcast ShutdownRequested to signal bus on signal receipt",
            "THE SYSTEM SHALL coordinate graceful shutdown with 30s timeout",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN SIGINT signal is received"
                shall:   "THE SYSTEM SHALL log the signal and broadcast ShutdownRequested"
            },
            {
                trigger: "WHEN SIGTERM signal is received"
                shall:   "THE SYSTEM SHALL log the signal and broadcast ShutdownRequested"
            },
            {
                trigger: "WHEN ShutdownRequested is broadcast"
                shall:   "THE SYSTEM SHALL initiate sequential actor termination"
            },
            {
                trigger: "WHEN an actor fails to stop within its timeout"
                shall:   "THE SYSTEM SHALL log an error and proceed to next actor"
            },
            {
                trigger: "WHEN graceful shutdown times out (30s)"
                shall:   "THE SYSTEM SHALL forcefully terminate remaining actors"
            },
        ]

        state_driven: [
            {
                state: "WHILE waiting for shutdown signal"
                shall: "THE SYSTEM SHALL not block other async tasks"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL reject new actor spawn requests"
            },
            {
                state: "WHILE actors are terminating"
                shall: "THE SYSTEM SHALL terminate in reverse startup order (LIFO)"
            },
        ]

        unwanted: [
            {
                condition: "IF a second signal is received during shutdown"
                shall_not: "THE SYSTEM SHALL NOT restart the shutdown sequence"
                because:   "Double shutdown would corrupt actor state and cause races"
            },
            {
                condition: "IF shutdown has not been requested"
                shall_not: "THE SYSTEM SHALL NOT terminate actors prematurely"
                because:   "Premature termination loses in-flight work"
            },
            {
                condition: "IF the signal handler task panics"
                shall_not: "THE SYSTEM SHALL NOT leave the system in an inconsistent state"
                because:   "Orphaned actors would leak resources indefinitely"
            },
        ]

        complex: [
            {
                state:   "WHILE the system is shutting down"
                trigger: "WHEN an actor receives new messages"
                shall:   "THE SYSTEM SHALL drain pending messages before termination"
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
                    field:           "SignalBusRef"
                    type:            "ActorRef<SignalBusMessage>"
                    constraints:     "Valid reference to running signal bus actor"
                    example_valid:   "signal_bus_ref from supervisor startup"
                    example_invalid: "dropped/dead actor reference"
                },
                {
                    field:           "ShutdownTimeout"
                    type:            "Duration"
                    constraints:     "Positive duration, recommended 30s"
                    example_valid:   "Duration::from_secs(30)"
                    example_invalid: "Duration::ZERO"
                },
            ]
            system_state: [
                "Tokio runtime is running",
                "Signal bus actor is spawned and accessible",
                "Unix signal handlers are available (not Windows)",
            ]
        }

        postconditions: {
            state_changes: [
                "Signal handler task spawned and listening",
                "SIGINT and SIGTERM handlers registered with OS",
                "ShutdownRequested signal broadcast on signal receipt",
                "All supervised actors terminated in LIFO order",
            ]
            return_guarantees: [
                {
                    field:     "SignalHandler::new()"
                    guarantee: "Returns Result<SignalHandler, Error>"
                },
                {
                    field:     "SignalHandler::wait_for_shutdown()"
                    guarantee: "Returns ShutdownSignal variant when signal received"
                },
                {
                    field:     "graceful_shutdown()"
                    guarantee: "Returns Result<ShutdownStats, Error> with timing info"
                },
            ]
            side_effects: [
                "OS signal handlers modified",
                "Signal bus receives ShutdownRequested message",
                "Actors receive shutdown commands",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Signal handler does not block the async runtime",
            "Shutdown sequence is idempotent (safe to call multiple times)",
            "Actor termination order is deterministic (LIFO)",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Signal handler not compatible with Windows"
                prevention:  "Use cfg(unix) and provide no-op stub for Windows"
                test_for_it: "test_signal_handler_creation_platform_specific"
            },
            {
                failure:     "Signal arrives before handler is registered"
                prevention:  "Register signal handler before spawning other actors"
                test_for_it: "test_signal_registration_order"
            },
            {
                failure:     "tokio::signal conflicts with external signal handlers"
                prevention:  "Document that factory owns SIGINT/SIGTERM handlers"
                test_for_it: "test_signal_handler_installation"
            },
        ]

        usability_failures: [
            {
                failure:     "Shutdown takes too long, user sends multiple Ctrl+C"
                prevention:  "Log progress during shutdown, honor timeout"
                test_for_it: "test_shutdown_progress_logging"
            },
            {
                failure:     "No indication which actor is blocking shutdown"
                prevention:  "Log each actor termination with timing"
                test_for_it: "test_shutdown_logs_actor_names"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "In-flight work lost during shutdown"
                prevention:  "Drain actor message queues before termination"
                test_for_it: "test_message_drain_before_shutdown"
            },
            {
                failure:     "State corruption from partial shutdown"
                prevention:  "Use atomic shutdown flag, terminate in order"
                test_for_it: "test_shutdown_atomicity"
            },
            {
                failure:     "Race between signal and normal shutdown"
                prevention:  "Use compare-and-swap for shutdown_requested flag"
                test_for_it: "test_concurrent_shutdown_requests"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_sigint_triggers_shutdown"
                given: "A running signal handler with signal bus"
                when:  "SIGINT is received"
                then: [
                    "ShutdownRequested is broadcast to signal bus",
                    "wait_for_shutdown returns Sigint variant",
                    "All actors receive shutdown notification",
                ]
                real_input: """
                    use tokio::signal::unix::{signal, SignalKind};
                    use std::sync::Arc;
                    use tokio::sync::Notify;

                    let shutdown_notify = Arc::new(Notify::new());
                    let handler = SignalHandler::new(signal_bus_ref.clone())?;

                    // Simulate SIGINT via test helper
                    tokio::spawn(async move {
                        tokio::time::sleep(Duration::from_millis(50)).await;
                        nix::sys::signal::raise(nix::sys::signal::Signal::SIGINT).ok();
                    });

                    let signal = handler.wait_for_shutdown().await?;
                    """
                expected_output: """
                    assert!(matches!(signal, ShutdownSignal::Sigint));
                    // Verify signal bus received ShutdownRequested
                    """
            },
            {
                name:  "test_sigterm_triggers_shutdown"
                given: "A running signal handler with signal bus"
                when:  "SIGTERM is received"
                then: [
                    "ShutdownRequested is broadcast to signal bus",
                    "wait_for_shutdown returns Sigterm variant",
                ]
                real_input: """
                    let handler = SignalHandler::new(signal_bus_ref.clone())?;

                    tokio::spawn(async move {
                        tokio::time::sleep(Duration::from_millis(50)).await;
                        nix::sys::signal::raise(nix::sys::signal::Signal::SIGTERM).ok();
                    });

                    let signal = handler.wait_for_shutdown().await?;
                    """
                expected_output: """
                    assert!(matches!(signal, ShutdownSignal::Sigterm));
                    """
            },
            {
                name:  "test_graceful_shutdown_terminates_actors"
                given: "Running supervisor with multiple actors"
                when:  "graceful_shutdown is called with 30s timeout"
                then: [
                    "Actors terminate in LIFO order",
                    "Each actor termination is logged",
                    "Returns ShutdownStats with timing",
                ]
                real_input: """
                    let supervisor = Supervisor::start(config).await?;
                    let stats = graceful_shutdown(
                        supervisor,
                        Duration::from_secs(30)
                    ).await?;
                    """
                expected_output: """
                    assert!(stats.total_duration < Duration::from_secs(30));
                    assert!(stats.actors_terminated > 0);
                    assert!(stats.actors_forcefully_killed == 0);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_shutdown_timeout_force_kills"
                given: "An actor that ignores shutdown requests"
                when:  "graceful_shutdown times out"
                then: [
                    "Actor is forcefully terminated",
                    "Returns with forcefully_killed count > 0",
                    "Does not hang indefinitely",
                ]
                real_input: """
                    let slow_actor = spawn_slow_actor().await?;
                    let stats = graceful_shutdown(
                        supervisor,
                        Duration::from_millis(100)
                    ).await?;
                    """
                expected_output: null
                expected_error: """
                    // Not an error, but stats reflect forced termination
                    assert!(stats.actors_forcefully_killed >= 1);
                    """
            },
            {
                name:  "test_signal_handler_on_dead_bus"
                given: "Signal bus has been shut down"
                when:  "Signal is received"
                then: [
                    "Returns Error::SignalBusDisconnected",
                    "Does not panic",
                    "Logs error appropriately",
                ]
                real_input: """
                    signal_bus_ref.stop().await?;
                    let handler = SignalHandler::new(signal_bus_ref)?;
                    let result = handler.broadcast_shutdown().await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::SignalBusDisconnected)
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_multiple_signals_during_shutdown"
                scenario: "User presses Ctrl+C multiple times"
                input:    "Send SIGINT, then SIGINT again 100ms later"
                expected: "Second signal is ignored, shutdown continues"
            },
            {
                name:     "test_sigterm_after_sigint"
                scenario: "SIGTERM arrives while processing SIGINT"
                input:    "Send SIGINT, then SIGTERM during shutdown"
                expected: "Shutdown completes, second signal logged but ignored"
            },
            {
                name:     "test_signal_during_actor_spawn"
                scenario: "Signal arrives while actor is being spawned"
                input:    "Start slow actor spawn, send SIGINT mid-spawn"
                expected: "Spawn completes or fails, then shutdown proceeds"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in signal handler code"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/signal_handler.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public APIs return Result"
                test:     "All public functions in signal_handler.rs return Result<T, Error>"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_signal_shutdown_lifecycle"
            description: "Complete signal handling: setup -> signal -> shutdown -> cleanup"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/signal_handler_e2e.rs"
                        content: """
                            use factory_core::signal_handler::{SignalHandler, ShutdownSignal};
                            use factory_core::signal_bus::SignalBus;
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_full_signal_shutdown_lifecycle() -> Result<()> {
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
                command:    "moon run factory-core:test -- --test signal_handler_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_signal_shutdown_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/signal_handler.rs"
                        contains: "pub struct SignalHandler"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/signal_handler_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_ctrl_c_graceful_shutdown"
                description: "Verify Ctrl+C triggers clean supervisor shutdown"
                steps: [
                    {action: "Start supervisor with all actors", verify: "All actors running"},
                    {action: "Send SIGINT to process", verify: "ShutdownRequested broadcast"},
                    {action: "Wait for shutdown", verify: "All actors stopped"},
                    {action: "Check logs", verify: "Shutdown timing logged"},
                ]
            },
            {
                name:        "e2e_systemd_sigterm"
                description: "Verify systemd SIGTERM triggers clean shutdown"
                steps: [
                    {action: "Start supervisor with all actors", verify: "All actors running"},
                    {action: "Send SIGTERM to process", verify: "ShutdownRequested broadcast"},
                    {action: "Wait for shutdown (30s max)", verify: "All actors stopped"},
                    {action: "Verify exit code", verify: "Exit code is 0"},
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
                task:      "Write test: test_sigint_triggers_shutdown"
                file:      "crates/factory-core/src/signal_handler.rs"
                what:      "Test that SIGINT broadcasts ShutdownRequested"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_sigterm_triggers_shutdown"
                file:      "crates/factory-core/src/signal_handler.rs"
                what:      "Test that SIGTERM broadcasts ShutdownRequested"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_graceful_shutdown_terminates_actors"
                file:      "crates/factory-core/src/signal_handler.rs"
                what:      "Test LIFO termination with timing stats"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_shutdown_timeout_force_kills"
                file:      "crates/factory-core/src/signal_handler.rs"
                what:      "Test force termination after timeout"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add nix dependency for signal testing"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dev-dependencies]
                    nix = { version = "0.29", features = ["signal"] }
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["cfg(unix) for platform-specific code"]
            },
            {
                task: "Create ShutdownSignal enum"
                file: "crates/factory-core/src/signal_handler.rs"
                what: """
                    /// Shutdown signal types
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    pub enum ShutdownSignal {
                        Sigint,
                        Sigterm,
                    }

                    impl std::fmt::Display for ShutdownSignal {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            match self {
                                Self::Sigint => write!(f, "SIGINT"),
                                Self::Sigterm => write!(f, "SIGTERM"),
                            }
                        }
                    }
                    """
                done_when:     "Enum compiles with Display impl"
                patterns_to_use: ["derive macros for common traits"]
            },
            {
                task: "Create SignalHandler struct"
                file: "crates/factory-core/src/signal_handler.rs"
                what: """
                    use std::sync::atomic::{AtomicBool, Ordering};
                    use std::sync::Arc;
                    use tokio::signal::unix::{signal, SignalKind};

                    /// Signal handler for graceful shutdown
                    pub struct SignalHandler {
                        signal_bus: ActorRef<SignalBusMessage>,
                        shutdown_requested: Arc<AtomicBool>,
                    }

                    impl SignalHandler {
                        pub fn new(signal_bus: ActorRef<SignalBusMessage>) -> Result<Self, Error> {
                            Ok(Self {
                                signal_bus,
                                shutdown_requested: Arc::new(AtomicBool::new(false)),
                            })
                        }
                    }
                    """
                done_when:     "Struct compiles with constructor"
                patterns_to_use: [
                    "Arc<AtomicBool> for thread-safe shutdown flag",
                    "Result<T, Error> for fallible construction",
                ]
            },
            {
                task: "Implement wait_for_shutdown"
                file: "crates/factory-core/src/signal_handler.rs"
                what: """
                    impl SignalHandler {
                        /// Wait for SIGINT or SIGTERM, broadcast shutdown, return signal type
                        pub async fn wait_for_shutdown(&self) -> Result<ShutdownSignal, Error> {
                            #[cfg(unix)]
                            {
                                let mut sigint = signal(SignalKind::interrupt())
                                    .map_err(|e| Error::SignalSetupFailed(e.to_string()))?;
                                let mut sigterm = signal(SignalKind::terminate())
                                    .map_err(|e| Error::SignalSetupFailed(e.to_string()))?;

                                let signal = tokio::select! {
                                    _ = sigint.recv() => ShutdownSignal::Sigint,
                                    _ = sigterm.recv() => ShutdownSignal::Sigterm,
                                };

                                // Prevent duplicate shutdown
                                if self.shutdown_requested.swap(true, Ordering::SeqCst) {
                                    tracing::warn!("Duplicate shutdown signal ignored");
                                    return Ok(signal);
                                }

                                tracing::info!("Received {}, initiating shutdown", signal);
                                self.broadcast_shutdown().await?;
                                Ok(signal)
                            }

                            #[cfg(not(unix))]
                            {
                                // Windows: use ctrl_c only
                                tokio::signal::ctrl_c()
                                    .await
                                    .map_err(|e| Error::SignalSetupFailed(e.to_string()))?;
                                self.broadcast_shutdown().await?;
                                Ok(ShutdownSignal::Sigint)
                            }
                        }

                        async fn broadcast_shutdown(&self) -> Result<(), Error> {
                            self.signal_bus
                                .cast(SignalBusMessage::Publish(Signal::ShutdownRequested))
                                .map_err(|_| Error::SignalBusDisconnected)
                        }
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "tokio::select! for racing multiple signals",
                    "AtomicBool::swap for idempotent shutdown",
                    "cfg(unix) for platform-specific code",
                    "tracing for structured logging",
                ]
            },
            {
                task: "Implement graceful_shutdown function"
                file: "crates/factory-core/src/signal_handler.rs"
                what: """
                    use std::time::{Duration, Instant};

                    /// Statistics from shutdown process
                    #[derive(Debug, Clone)]
                    pub struct ShutdownStats {
                        pub total_duration: Duration,
                        pub actors_terminated: usize,
                        pub actors_forcefully_killed: usize,
                    }

                    /// Gracefully shutdown supervisor with timeout
                    pub async fn graceful_shutdown(
                        supervisor: &Supervisor,
                        timeout: Duration,
                    ) -> Result<ShutdownStats, Error> {
                        let start = Instant::now();
                        let deadline = start + timeout;
                        let mut terminated = 0usize;
                        let mut force_killed = 0usize;

                        // Terminate in LIFO order (reverse of startup)
                        let actors = supervisor.actors_lifo();

                        for (name, actor_ref) in actors {
                            let remaining = deadline.saturating_duration_since(Instant::now());
                            if remaining.is_zero() {
                                tracing::warn!("Shutdown timeout, force killing {}", name);
                                actor_ref.stop();
                                force_killed += 1;
                                continue;
                            }

                            tracing::info!("Stopping {}", name);
                            match tokio::time::timeout(remaining, actor_ref.stop_gracefully()).await {
                                Ok(Ok(())) => {
                                    tracing::info!("{} stopped", name);
                                    terminated += 1;
                                }
                                Ok(Err(e)) => {
                                    tracing::error!("{} failed to stop: {}", name, e);
                                    force_killed += 1;
                                }
                                Err(_) => {
                                    tracing::warn!("{} timed out, force killing", name);
                                    actor_ref.stop();
                                    force_killed += 1;
                                }
                            }
                        }

                        let stats = ShutdownStats {
                            total_duration: start.elapsed(),
                            actors_terminated: terminated,
                            actors_forcefully_killed: force_killed,
                        };

                        tracing::info!(
                            "Shutdown complete in {:?}: {} terminated, {} force killed",
                            stats.total_duration,
                            stats.actors_terminated,
                            stats.actors_forcefully_killed
                        );

                        Ok(stats)
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "Instant and Duration for timing",
                    "tokio::time::timeout for per-actor timeout",
                    "saturating_duration_since for safe time math",
                    "LIFO termination order",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export signal_handler from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod signal_handler;"
                done_when: "External crates can import factory_core::signal_handler"
            },
            {
                task:      "Add signal handler errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "SignalSetupFailed(String), SignalBusDisconnected variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Integrate with supervisor startup"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Create SignalHandler after signal_bus, store in Started struct"
                done_when: "Supervisor creates signal handler on startup"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/signal_handler.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification"
                done_when: "Ctrl+C triggers graceful shutdown"
                commands: [
                    "cargo run --bin factory -- start &",
                    "sleep 2 && kill -INT $!",
                ]
                expected: "Graceful shutdown logs, exit code 0"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'SignalKind' not found"
                likely_cause: "Missing unix feature or non-unix platform"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_handler.rs"
                        what_to_check: "Is code wrapped in #[cfg(unix)]?"
                    },
                ]
                fix_pattern: "Add #[cfg(unix)] and provide Windows fallback"
            },
            {
                symptom:      "Runtime error: 'signal only works on the current thread'"
                likely_cause: "Signal handler created on non-runtime thread"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_handler.rs"
                        function:      "new()"
                        what_to_check: "Is signal() called inside tokio runtime?"
                    },
                ]
                fix_pattern: "Defer signal() call to wait_for_shutdown, not constructor"
            },
            {
                symptom:      "Shutdown hangs indefinitely"
                likely_cause: "Actor not responding to stop signal"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_handler.rs"
                        function:      "graceful_shutdown()"
                        what_to_check: "Is there a timeout on stop_gracefully?"
                    },
                ]
                fix_pattern: "Wrap stop_gracefully with tokio::time::timeout"
            },
            {
                symptom:      "Second Ctrl+C causes panic or double shutdown"
                likely_cause: "Shutdown flag not checked atomically"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_handler.rs"
                        function:      "wait_for_shutdown()"
                        what_to_check: "Is AtomicBool::swap used correctly?"
                    },
                ]
                fix_pattern: "Use swap() and return early if already true"
            },
        ]

        debugging_commands: [
            {
                scenario: "When signal is not received"
                run:      "strace -e signal cargo run --bin factory"
                look_for: "rt_sigaction calls for SIGINT/SIGTERM"
            },
            {
                scenario: "When shutdown hangs"
                run:      "RUST_LOG=factory_core=debug cargo run"
                look_for: "Which actor is blocking in logs"
            },
            {
                scenario: "When signal handler fails to install"
                run:      "cargo test signal_handler -- --nocapture"
                look_for: "SignalSetupFailed error message"
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
            "[ ] E2E pipeline test passing with real signals",
            "[ ] No mocks or fake data in any test",
            "[ ] test_sigint_triggers_shutdown passes",
            "[ ] test_sigterm_triggers_shutdown passes",
            "[ ] test_graceful_shutdown_terminates_actors passes",
            "[ ] test_shutdown_timeout_force_kills passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All preconditions validated",
            "[ ] All postconditions guaranteed",
            "[ ] ShutdownSignal enum created",
            "[ ] SignalHandler struct created",
            "[ ] wait_for_shutdown implemented",
            "[ ] graceful_shutdown implemented",
            "[ ] Platform-specific code uses cfg(unix)",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in signal_handler.rs",
            "[ ] Example usage in doc comments",
            "[ ] ShutdownStats fields documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add signal_handler"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add signal error variants"
            },
            {
                path:      "crates/factory-core/src/signal_bus.rs"
                relevance: "Signal bus - receives ShutdownRequested"
            },
            {
                path:      "crates/factory-core/src/supervisor.rs"
                relevance: "Supervisor - integrates signal handler"
            },
            {
                path:      "src/signal_handler.gleam"
                relevance: "Gleam reference implementation to port"
            },
            {
                path:      "src/factory_supervisor.gleam"
                relevance: "Gleam supervisor showing shutdown flow"
            },
        ]

        external_references: [
            "https://docs.rs/tokio/latest/tokio/signal/index.html - Tokio signal handling",
            "https://docs.rs/tokio/latest/tokio/signal/unix/fn.signal.html - Unix signals",
            "https://docs.rs/nix/latest/nix/sys/signal/index.html - nix signal testing",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Atomic State Flags"
                example_location: "std::sync::atomic::AtomicBool"
                how_to_apply:     "Use swap() for idempotent state transitions"
            },
            {
                pattern:          "Graceful Shutdown with Timeout"
                example_location: "src/factory_supervisor.gleam:graceful_shutdown"
                how_to_apply:     "Sequential termination in LIFO order with per-actor timeout"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use tokio::signal::unix for SIGINT/SIGTERM on Unix",
            "Use tokio::signal::ctrl_c as Windows fallback",
            "Use AtomicBool::swap for idempotent shutdown flag",
            "Use tokio::select! to race SIGINT and SIGTERM",
            "Use tokio::time::timeout for per-actor shutdown timeout",
            "Log each actor termination with timing info",
            "Return ShutdownStats with termination counts",
            "Use cfg(unix) for platform-specific code",
            "Document 30s default timeout in doc comments",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT block the async runtime with sync I/O",
            "Do NOT ignore the second signal during shutdown",
            "Do NOT assume Unix-only deployment (handle Windows)",
        ]

        code_patterns: [
            {
                name:     "Signal Handler Setup"
                use_when: "Creating platform-specific signal handlers"
                example:  """
                    #[cfg(unix)]
                    async fn setup_signals() -> Result<impl Future<Output = ShutdownSignal>, Error> {
                        use tokio::signal::unix::{signal, SignalKind};

                        let mut sigint = signal(SignalKind::interrupt())
                            .map_err(|e| Error::SignalSetupFailed(e.to_string()))?;
                        let mut sigterm = signal(SignalKind::terminate())
                            .map_err(|e| Error::SignalSetupFailed(e.to_string()))?;

                        Ok(async move {
                            tokio::select! {
                                _ = sigint.recv() => ShutdownSignal::Sigint,
                                _ = sigterm.recv() => ShutdownSignal::Sigterm,
                            }
                        })
                    }
                    """
            },
            {
                name:     "Idempotent Shutdown Flag"
                use_when: "Preventing duplicate shutdown sequences"
                example:  """
                    use std::sync::atomic::{AtomicBool, Ordering};

                    let shutdown_requested = AtomicBool::new(false);

                    // Only proceed if we're the first to request shutdown
                    if shutdown_requested.swap(true, Ordering::SeqCst) {
                        tracing::warn!("Shutdown already in progress, ignoring signal");
                        return Ok(());
                    }

                    // Proceed with shutdown...
                    """
            },
            {
                name:     "LIFO Actor Termination"
                use_when: "Shutting down actors in reverse startup order"
                example:  """
                    pub async fn graceful_shutdown(
                        actors: Vec<(String, ActorRef)>,
                        timeout: Duration,
                    ) -> Result<ShutdownStats, Error> {
                        let start = Instant::now();
                        let mut terminated = 0;
                        let mut force_killed = 0;

                        // Reverse for LIFO order
                        for (name, actor) in actors.into_iter().rev() {
                            let remaining = timeout.saturating_sub(start.elapsed());
                            match tokio::time::timeout(remaining, actor.stop()).await {
                                Ok(Ok(())) => terminated += 1,
                                _ => {
                                    actor.abort();
                                    force_killed += 1;
                                }
                            }
                        }

                        Ok(ShutdownStats {
                            total_duration: start.elapsed(),
                            actors_terminated: terminated,
                            actors_forcefully_killed: force_killed,
                        })
                    }
                    """
            },
        ]
    }
}
