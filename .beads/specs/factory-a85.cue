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

"factory-a85": #ValidBead & {
    // ============================================================================
    // BEAD: factory-a85 - Implement beads watcher actor for issue tracker polling
    // ============================================================================

    id:              "factory-a85"
    title:           "Implement beads watcher actor for issue tracker polling"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["actor", "beads", "polling", "signal-bus", "P2"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL poll the beads database at a configurable interval",
            "THE SYSTEM SHALL emit BeadAssigned events to the signal bus when new work is available",
            "THE SYSTEM SHALL integrate with the bd CLI for bead status queries",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN the poll interval timer fires"
                shall:   "THE SYSTEM SHALL query the beads database for assigned issues"
            },
            {
                trigger: "WHEN a new assigned bead is discovered"
                shall:   "THE SYSTEM SHALL emit a BeadAssigned event to the signal bus"
            },
            {
                trigger: "WHEN a bead transitions from unassigned to assigned"
                shall:   "THE SYSTEM SHALL record the transition timestamp and emit an event"
            },
            {
                trigger: "WHEN the bd CLI returns an error"
                shall:   "THE SYSTEM SHALL log the error and retry on next poll interval"
            },
            {
                trigger: "WHEN the watcher actor receives a Shutdown message"
                shall:   "THE SYSTEM SHALL cancel the poll timer and drain pending events"
            },
            {
                trigger: "WHEN the signal bus is unavailable"
                shall:   "THE SYSTEM SHALL buffer events up to a configurable limit and retry"
            },
        ]

        state_driven: [
            {
                state: "WHILE the watcher is running"
                shall: "THE SYSTEM SHALL maintain a set of known assigned beads to detect changes"
            },
            {
                state: "WHILE the watcher is polling"
                shall: "THE SYSTEM SHALL not start another poll until the current one completes"
            },
            {
                state: "WHILE events are buffered due to signal bus unavailability"
                shall: "THE SYSTEM SHALL attempt to flush buffer on each successful poll"
            },
        ]

        unwanted: [
            {
                condition: "IF the poll interval is set to zero or negative"
                shall_not: "THE SYSTEM SHALL NOT accept invalid poll intervals"
                because:   "Zero or negative intervals would cause spin loops or undefined behavior"
            },
            {
                condition: "IF the bd CLI is not installed or not in PATH"
                shall_not: "THE SYSTEM SHALL NOT crash on bd CLI absence"
                because:   "Missing CLI should result in graceful degradation with clear error messages"
            },
            {
                condition: "IF the beads database file is locked by another process"
                shall_not: "THE SYSTEM SHALL NOT corrupt the beads database"
                because:   "Read-only access should be safe; write conflicts must be avoided"
            },
            {
                condition: "IF events are emitted faster than the signal bus can process"
                shall_not: "THE SYSTEM SHALL NOT cause unbounded memory growth"
                because:   "Buffer limits prevent OOM conditions under backpressure"
            },
        ]

        complex: [
            {
                state:   "WHILE the system is shutting down"
                trigger: "WHEN a poll timer fires"
                shall:   "THE SYSTEM SHALL ignore the timer and not start new polls"
            },
            {
                state:   "WHILE the watcher has buffered events"
                trigger: "WHEN the buffer reaches capacity"
                shall:   "THE SYSTEM SHALL drop oldest events and log a warning"
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
                    field:           "poll_interval"
                    type:            "Duration"
                    constraints:     "Must be >= 1 second and <= 1 hour"
                    example_valid:   "Duration::from_secs(30)"
                    example_invalid: "Duration::ZERO"
                },
                {
                    field:           "signal_bus"
                    type:            "ActorRef<SignalBusMsg>"
                    constraints:     "Must be a valid, running signal bus actor"
                    example_valid:   "signal_bus_ref.clone()"
                    example_invalid: "dropped or stopped actor ref"
                },
                {
                    field:           "beads_path"
                    type:            "Option<PathBuf>"
                    constraints:     "If provided, must be a valid path to .beads directory"
                    example_valid:   "Some(PathBuf::from(\".beads\"))"
                    example_invalid: "Some(PathBuf::from(\"/nonexistent\"))"
                },
            ]
            system_state: [
                "Signal bus actor (factory-kou) is running",
                "bd CLI is installed and accessible in PATH (optional, graceful degradation)",
                ".beads directory exists with valid beads.jsonl (or configured path)",
            ]
        }

        postconditions: {
            state_changes: [
                "BeadsWatcher actor is running and polling",
                "Poll timer is scheduled at configured interval",
                "Known beads set is initialized from initial poll",
            ]
            return_guarantees: [
                {
                    field:     "BeadsWatcherRef"
                    guarantee: "Valid actor handle for sending messages to watcher"
                },
                {
                    field:     "spawn()"
                    guarantee: "Returns Result<BeadsWatcherRef, Error>"
                },
                {
                    field:     "get_status()"
                    guarantee: "Returns Result<WatcherStatus, Error> with current state"
                },
                {
                    field:     "set_poll_interval()"
                    guarantee: "Returns Result<(), Error> and takes effect on next poll"
                },
            ]
            side_effects: [
                "BeadAssigned events emitted to signal bus when new work detected",
                "bd CLI invoked for database queries (subprocess spawned)",
                "Log entries written for poll cycles and detected changes",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Poll interval is always >= 1 second",
            "Event buffer never exceeds configured capacity",
            "Only one poll operation runs at a time per watcher instance",
            "Known beads set is consistent with last successful poll",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Signal bus actor not available at watcher spawn"
                prevention:  "Validate signal bus ref before spawning watcher"
                test_for_it: "test_spawn_fails_without_signal_bus"
            },
            {
                failure:     "bd CLI output format changes break parsing"
                prevention:  "Use structured JSON output (--json flag) and version check"
                test_for_it: "test_bd_cli_json_parsing"
            },
            {
                failure:     "Race condition between poll and shutdown"
                prevention:  "Use AtomicBool for shutdown flag, check before each poll phase"
                test_for_it: "test_shutdown_during_poll"
            },
            {
                failure:     "Beads database locked during poll"
                prevention:  "Use read-only access, implement retry with backoff"
                test_for_it: "test_poll_with_locked_database"
            },
        ]

        usability_failures: [
            {
                failure:     "Poll interval too short causes high CPU usage"
                prevention:  "Enforce minimum 1 second interval, warn if < 10 seconds"
                test_for_it: "test_minimum_poll_interval_enforced"
            },
            {
                failure:     "No visibility into watcher state"
                prevention:  "Expose get_status() returning poll count, last poll time, buffer size"
                test_for_it: "test_get_status_returns_metrics"
            },
            {
                failure:     "Unclear why events aren't being emitted"
                prevention:  "Structured logging with poll results and event emission counts"
                test_for_it: "test_logging_includes_poll_results"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Duplicate BeadAssigned events for same bead"
                prevention:  "Track known beads set, only emit for new assignments"
                test_for_it: "test_no_duplicate_events"
            },
            {
                failure:     "Events lost during signal bus reconnection"
                prevention:  "Buffer events during outage, flush on reconnection"
                test_for_it: "test_event_buffering_during_bus_outage"
            },
            {
                failure:     "Stale bead data due to caching"
                prevention:  "No caching of bead data; fresh query each poll"
                test_for_it: "test_poll_returns_fresh_data"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_spawn_beads_watcher"
                given: "A running signal bus actor"
                when:  "BeadsWatcher::spawn() is called with valid config"
                then: [
                    "Returns Ok(BeadsWatcherRef)",
                    "Watcher starts polling immediately",
                    "Initial poll queries beads database",
                ]
                real_input: """
                    let signal_bus = SignalBus::spawn().await?;
                    let config = BeadsWatcherConfig {
                        poll_interval: Duration::from_secs(30),
                        beads_path: None,  // Use default .beads
                        buffer_capacity: 100,
                    };
                    let watcher = BeadsWatcher::spawn(config, signal_bus.clone()).await?;
                    """
                expected_output: """
                    assert!(watcher.is_running());
                    let status = watcher.get_status().await?;
                    assert!(status.poll_count >= 1);
                    """
            },
            {
                name:  "test_emits_bead_assigned_event"
                given: "A running watcher and a newly assigned bead"
                when:  "The poll detects a new assigned bead"
                then: [
                    "BeadAssigned event is emitted to signal bus",
                    "Event contains bead ID and assignment metadata",
                    "Known beads set is updated",
                ]
                real_input: """
                    // Setup: create a bead and assign it
                    let bead_id = "factory-xyz";
                    bd_update(bead_id, "--status", "in_progress").await?;

                    // Wait for poll to detect
                    tokio::time::sleep(Duration::from_secs(5)).await;
                    """
                expected_output: """
                    let events = signal_bus.get_events::<BeadAssigned>().await?;
                    assert!(events.iter().any(|e| e.bead_id == bead_id));
                    """
            },
            {
                name:  "test_poll_interval_configurable"
                given: "A running watcher"
                when:  "set_poll_interval() is called"
                then: [
                    "New interval takes effect on next poll",
                    "Current poll completes normally",
                ]
                real_input: """
                    let watcher = spawn_test_watcher(Duration::from_secs(60)).await?;
                    watcher.set_poll_interval(Duration::from_secs(10)).await?;
                    """
                expected_output: """
                    let status = watcher.get_status().await?;
                    assert_eq!(status.poll_interval, Duration::from_secs(10));
                    """
            },
            {
                name:  "test_graceful_shutdown"
                given: "A running watcher with pending poll"
                when:  "Shutdown message is sent"
                then: [
                    "Current poll completes or is cancelled",
                    "Buffered events are flushed to signal bus",
                    "Actor terminates cleanly",
                ]
                real_input: """
                    let watcher = spawn_test_watcher(Duration::from_secs(1)).await?;
                    watcher.shutdown().await?;
                    """
                expected_output: """
                    assert!(!watcher.is_running());
                    // No panic, no resource leaks
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_spawn_fails_with_invalid_interval"
                given: "An invalid poll interval (zero or negative)"
                when:  "BeadsWatcher::spawn() is called"
                then: [
                    "Returns Err(Error::InvalidPollInterval)",
                    "No actor is spawned",
                ]
                real_input: """
                    let config = BeadsWatcherConfig {
                        poll_interval: Duration::ZERO,
                        ..Default::default()
                    };
                    let result = BeadsWatcher::spawn(config, signal_bus).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InvalidPollInterval { interval: Duration::ZERO })
                    """
            },
            {
                name:  "test_poll_handles_bd_cli_error"
                given: "bd CLI returns an error (e.g., corrupt database)"
                when:  "Poll attempts to query beads"
                then: [
                    "Error is logged but watcher continues running",
                    "Next poll is scheduled normally",
                    "No event is emitted for failed poll",
                ]
                real_input: """
                    // Corrupt the beads database
                    std::fs::write(".beads/beads.jsonl", "invalid json")?;
                    // Wait for poll
                    tokio::time::sleep(poll_interval * 2).await;
                    """
                expected_output: """
                    let status = watcher.get_status().await?;
                    assert!(status.last_error.is_some());
                    assert!(watcher.is_running());  // Still running
                    """
                expected_error: null
            },
            {
                name:  "test_handles_signal_bus_disconnect"
                given: "Signal bus becomes unavailable"
                when:  "Watcher tries to emit an event"
                then: [
                    "Event is buffered",
                    "Watcher continues polling",
                    "Buffered events are flushed when bus reconnects",
                ]
                real_input: """
                    signal_bus.shutdown().await?;  // Disconnect bus
                    // Assign a bead to trigger event
                    bd_update("test-bead", "--status", "in_progress").await?;
                    tokio::time::sleep(poll_interval * 2).await;
                    """
                expected_output: """
                    let status = watcher.get_status().await?;
                    assert!(status.buffered_events > 0);
                    """
                expected_error: null
            },
        ]

        edge_cases: [
            {
                name:     "test_no_events_for_already_known_beads"
                scenario: "Bead was assigned before watcher started"
                input:    "Start watcher with pre-existing assigned beads"
                expected: "Initial poll populates known set, no events emitted"
            },
            {
                name:     "test_handles_empty_beads_database"
                scenario: "No beads in database"
                input:    "Start watcher with empty .beads/beads.jsonl"
                expected: "Watcher runs normally, no events emitted, no errors"
            },
            {
                name:     "test_handles_high_volume_assignments"
                scenario: "100 beads assigned simultaneously"
                input:    "Bulk assign 100 beads between polls"
                expected: "All 100 BeadAssigned events emitted, no duplicates"
            },
            {
                name:     "test_bead_unassignment_not_tracked"
                scenario: "Bead transitions from assigned to unassigned"
                input:    "Update bead status from in_progress to open"
                expected: "No event emitted (watcher only tracks assignments)"
            },
            {
                name:     "test_buffer_overflow_drops_oldest"
                scenario: "Event buffer reaches capacity"
                input:    "Fill buffer, then detect new assignment"
                expected: "Oldest event dropped, newest added, warning logged"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in beads_watcher.rs"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/actors/beads_watcher.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public APIs return Result"
                test:     "cargo doc shows Result returns for spawn, get_status, set_poll_interval"
            },
            {
                name:     "test_invariant_poll_interval_minimum"
                verifies: "Poll interval is always >= 1 second"
                test:     "Any interval < 1 second results in Error::InvalidPollInterval"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_beads_watcher_e2e"
            description: "Complete watcher lifecycle: spawn -> poll -> emit -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/beads_watcher_e2e.rs"
                        content: """
                            use factory_core::actors::{BeadsWatcher, BeadsWatcherConfig, SignalBus};
                            use factory_core::signals::BeadAssigned;
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_beads_watcher_e2e() -> Result<()> {
                                // Setup signal bus
                                let signal_bus = SignalBus::spawn().await?;

                                // Setup watcher with short poll interval for testing
                                let config = BeadsWatcherConfig {
                                    poll_interval: Duration::from_secs(1),
                                    beads_path: Some(".beads".into()),
                                    buffer_capacity: 10,
                                };
                                let watcher = BeadsWatcher::spawn(config, signal_bus.clone()).await?;

                                // Verify initial poll completed
                                tokio::time::sleep(Duration::from_millis(1500)).await;
                                let status = watcher.get_status().await?;
                                assert!(status.poll_count >= 1, "Expected at least one poll");

                                // Shutdown gracefully
                                watcher.shutdown().await?;
                                assert!(!watcher.is_running());

                                Ok(())
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "moon run factory-core:build",
                    "mkdir -p .beads",
                    "touch .beads/beads.jsonl",
                ]
            }

            execute: {
                command:    "moon run factory-core:test -- --test beads_watcher_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_beads_watcher_e2e ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/actors/beads_watcher.rs"
                        contains: "pub struct BeadsWatcher"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/beads_watcher_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_watcher_detects_new_assignment"
                description: "Verify watcher emits event when bead is assigned"
                steps: [
                    {action: "Start signal bus and watcher", verify: "Both actors running"},
                    {action: "Subscribe to BeadAssigned events on signal bus", verify: "Subscription active"},
                    {action: "Assign a bead via bd CLI", verify: "Bead status is in_progress"},
                    {action: "Wait for poll interval", verify: "Poll completes"},
                    {action: "Check signal bus for event", verify: "BeadAssigned event received with correct bead ID"},
                ]
            },
            {
                name:        "e2e_watcher_survives_bd_failure"
                description: "Verify watcher continues after bd CLI failure"
                steps: [
                    {action: "Start watcher with valid config", verify: "Watcher running"},
                    {action: "Corrupt beads database", verify: "File is invalid JSON"},
                    {action: "Wait for poll to fail", verify: "Error logged, watcher still running"},
                    {action: "Fix beads database", verify: "File is valid again"},
                    {action: "Wait for next poll", verify: "Poll succeeds, watcher recovers"},
                ]
            },
            {
                name:        "e2e_watcher_integration_with_factory"
                description: "Verify watcher integrates with factory task workflow"
                steps: [
                    {action: "Start factory runtime with watcher enabled", verify: "All actors running"},
                    {action: "Create a task via factory CLI", verify: "Task created, bead assigned"},
                    {action: "Verify watcher detected assignment", verify: "BeadAssigned event in signal bus"},
                    {action: "Verify task workflow triggered", verify: "Workspace created for assigned bead"},
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
                task:      "Write test: test_spawn_beads_watcher"
                file:      "crates/factory-core/src/actors/beads_watcher.rs"
                what:      "Test that BeadsWatcher::spawn returns valid ref and starts polling"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_emits_bead_assigned_event"
                file:      "crates/factory-core/src/actors/beads_watcher.rs"
                what:      "Test that watcher emits events to signal bus on new assignment"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_poll_interval_configurable"
                file:      "crates/factory-core/src/actors/beads_watcher.rs"
                what:      "Test that poll interval can be changed at runtime"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_spawn_fails_with_invalid_interval"
                file:      "crates/factory-core/src/actors/beads_watcher.rs"
                what:      "Test that zero/negative intervals are rejected"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_graceful_shutdown"
                file:      "crates/factory-core/src/actors/beads_watcher.rs"
                what:      "Test clean shutdown with pending operations"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define BeadsWatcherConfig struct"
                file: "crates/factory-core/src/actors/beads_watcher.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub struct BeadsWatcherConfig {
                        pub poll_interval: Duration,
                        pub beads_path: Option<PathBuf>,
                        pub buffer_capacity: usize,
                    }

                    impl Default for BeadsWatcherConfig {
                        fn default() -> Self {
                            Self {
                                poll_interval: Duration::from_secs(30),
                                beads_path: None,
                                buffer_capacity: 100,
                            }
                        }
                    }
                    """
                done_when:     "Struct compiles with validation"
                patterns_to_use: ["Builder pattern for config", "Default impl"]
            },
            {
                task: "Define BeadsWatcher message types"
                file: "crates/factory-core/src/actors/beads_watcher.rs"
                what: """
                    pub enum BeadsWatcherMsg {
                        // Internal: timer fired
                        Poll,
                        // Public: get current status
                        GetStatus { respond_to: oneshot::Sender<WatcherStatus> },
                        // Public: update poll interval
                        SetPollInterval { interval: Duration },
                        // Public: graceful shutdown
                        Shutdown,
                    }
                    """
                done_when:     "Message enum compiles"
                patterns_to_use: ["Typed message enum", "oneshot for request-reply"]
            },
            {
                task: "Implement BeadsWatcher actor"
                file: "crates/factory-core/src/actors/beads_watcher.rs"
                what: """
                    Implement Actor trait for BeadsWatcher:
                    - pre_start: validate config, schedule initial poll
                    - handle: process Poll, GetStatus, SetPollInterval, Shutdown
                    - post_stop: cancel timer, flush buffer
                    """
                done_when:     "Actor handles all message types"
                patterns_to_use: [
                    "ractor::Actor trait implementation",
                    "tokio::time::interval for poll scheduling",
                    "HashSet for known beads tracking",
                ]
            },
            {
                task: "Implement bd CLI integration"
                file: "crates/factory-core/src/actors/beads_watcher.rs"
                what: """
                    async fn query_assigned_beads(beads_path: &Path) -> Result<Vec<Bead>> {
                        let output = Command::new("bd")
                            .args(["list", "--status", "in_progress", "--json"])
                            .current_dir(beads_path)
                            .output()
                            .await
                            .map_err(Error::BdCliExecution)?;

                        if !output.status.success() {
                            return Err(Error::BdCliFailed {
                                stderr: String::from_utf8_lossy(&output.stderr).into(),
                            });
                        }

                        serde_json::from_slice(&output.stdout)
                            .map_err(Error::BdCliParsing)
                    }
                    """
                done_when:     "bd CLI is invoked and output parsed"
                patterns_to_use: [
                    "tokio::process::Command for async subprocess",
                    "serde_json for parsing",
                    "Result<T, Error> for all operations",
                ]
            },
            {
                task: "Implement signal bus event emission"
                file: "crates/factory-core/src/actors/beads_watcher.rs"
                what: """
                    async fn emit_bead_assigned(
                        &mut self,
                        bead: &Bead,
                    ) -> Result<()> {
                        let event = BeadAssigned {
                            bead_id: bead.id.clone(),
                            title: bead.title.clone(),
                            assigned_at: Utc::now(),
                        };

                        match self.signal_bus.cast(SignalBusMsg::Emit(Box::new(event))) {
                            Ok(()) => Ok(()),
                            Err(_) => {
                                // Buffer event for retry
                                self.buffer_event(event)?;
                                Ok(())
                            }
                        }
                    }
                    """
                done_when:     "Events emitted to signal bus with buffering"
                patterns_to_use: [
                    "VecDeque for event buffer",
                    "Signal bus cast for fire-and-forget",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Add BeadAssigned signal type"
                file:      "crates/factory-core/src/signals.rs"
                what:      "Define BeadAssigned event struct with bead_id, title, assigned_at"
                done_when: "Signal type compiles and is exported"
            },
            {
                task:      "Export BeadsWatcher from actors module"
                file:      "crates/factory-core/src/actors/mod.rs"
                what:      "pub mod beads_watcher; pub use beads_watcher::*;"
                done_when: "External crates can import BeadsWatcher"
            },
            {
                task:      "Add watcher error variants"
                file:      "crates/factory-core/src/error.rs"
                what:      "InvalidPollInterval, BdCliExecution, BdCliFailed, BdCliParsing, BufferOverflow"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Wire up watcher to runtime"
                file:      "crates/factory-core/src/runtime.rs"
                what:      "Add BeadsWatcher to runtime supervisor, configure from env/config"
                done_when: "Watcher starts when runtime starts"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
                commands:  ["moon run :ci"]
                expected:  "exit code 0"
            },
            {
                task:      "Verify no unwraps"
                done_when: "grep finds no unwrap/expect"
                commands:  ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/actors/beads_watcher.rs"]
                expected:  "no output (empty)"
            },
            {
                task:      "Manual integration test"
                done_when: "Watcher detects bead assignment in real scenario"
                commands: [
                    "cargo run -- --with-watcher &",
                    "bd update test-bead --status in_progress",
                    "# Verify event logged",
                ]
                expected: "BeadAssigned event logged for test-bead"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Watcher not emitting events despite assignments"
                likely_cause: "Signal bus ref is stale or disconnected"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/beads_watcher.rs"
                        function:      "emit_bead_assigned()"
                        what_to_check: "Is signal_bus.cast() returning Ok?"
                    },
                    {
                        file:          "crates/factory-core/src/actors/beads_watcher.rs"
                        function:      "handle()"
                        what_to_check: "Is buffer being drained on poll?"
                    },
                ]
                fix_pattern: "Check signal bus health, implement reconnection logic"
            },
            {
                symptom:      "bd CLI errors on every poll"
                likely_cause: "bd not in PATH or wrong working directory"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/beads_watcher.rs"
                        function:      "query_assigned_beads()"
                        what_to_check: "Is beads_path correct? Is bd installed?"
                    },
                ]
                fix_pattern: "Use which::which to check bd availability, validate beads_path"
            },
            {
                symptom:      "High CPU usage from watcher"
                likely_cause: "Poll interval too short or spin loop in timer"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/beads_watcher.rs"
                        function:      "schedule_poll()"
                        what_to_check: "Is interval >= 1 second? Is timer awaited correctly?"
                    },
                ]
                fix_pattern: "Enforce minimum interval, use tokio::time::sleep not busy wait"
            },
            {
                symptom:      "Duplicate BeadAssigned events"
                likely_cause: "Known beads set not updated correctly"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/beads_watcher.rs"
                        function:      "handle_poll()"
                        what_to_check: "Is known_beads updated after detecting new assignment?"
                    },
                ]
                fix_pattern: "Insert bead_id into known_beads immediately after emitting event"
            },
            {
                symptom:      "Memory growth over time"
                likely_cause: "Event buffer not draining, unbounded growth"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/beads_watcher.rs"
                        function:      "buffer_event()"
                        what_to_check: "Is buffer_capacity enforced? Are old events dropped?"
                    },
                ]
                fix_pattern: "Enforce capacity with VecDeque::pop_front when at limit"
            },
        ]

        debugging_commands: [
            {
                scenario: "When events aren't being emitted"
                run:      "RUST_LOG=factory_core::actors::beads_watcher=debug cargo run"
                look_for: "Poll results, event emission attempts, buffer state"
            },
            {
                scenario: "When bd CLI fails"
                run:      "bd list --status in_progress --json"
                look_for: "Valid JSON output or error message"
            },
            {
                scenario: "When watcher appears stuck"
                run:      "RUST_LOG=ractor=debug,factory_core=debug cargo run"
                look_for: "Actor message processing, timer scheduling"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_spawn_beads_watcher passes",
            "[ ] test_emits_bead_assigned_event passes",
            "[ ] test_poll_interval_configurable passes",
            "[ ] test_spawn_fails_with_invalid_interval passes",
            "[ ] test_graceful_shutdown passes",
            "[ ] test_poll_handles_bd_cli_error passes",
            "[ ] test_handles_signal_bus_disconnect passes",
            "[ ] test_no_duplicate_events passes",
            "[ ] E2E pipeline test passing with real beads database",
            "[ ] No mocks for bd CLI in integration tests",
        ]

        code: [
            "[ ] BeadsWatcherConfig struct defined with validation",
            "[ ] BeadsWatcherMsg enum with all message types",
            "[ ] BeadsWatcher implements ractor::Actor",
            "[ ] bd CLI integration with JSON parsing",
            "[ ] Signal bus event emission with buffering",
            "[ ] Known beads tracking with HashSet",
            "[ ] Poll timer with configurable interval",
            "[ ] Graceful shutdown handling",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All preconditions validated",
            "[ ] All postconditions guaranteed",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in beads_watcher.rs",
            "[ ] Doc comments on BeadsWatcher, BeadsWatcherConfig, BeadsWatcherMsg",
            "[ ] Example usage in doc comments",
            "[ ] Error variant documentation",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/actors/mod.rs"
                relevance: "Module exports - must add beads_watcher"
            },
            {
                path:      "crates/factory-core/src/actors/signal_bus.rs"
                relevance: "Signal bus actor - dependency for event emission"
            },
            {
                path:      "crates/factory-core/src/signals.rs"
                relevance: "Signal types - must add BeadAssigned"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add watcher error variants"
            },
            {
                path:      "crates/factory-core/src/runtime.rs"
                relevance: "Runtime supervisor - must integrate watcher"
            },
            {
                path:      ".beads/beads.jsonl"
                relevance: "Beads database - data source for polling"
            },
        ]

        dependencies: [
            {
                bead_id:    "factory-kou"
                title:      "Signal bus actor"
                relevance:  "Required for event emission"
                must_exist: true
            },
        ]

        external_references: [
            "https://github.com/slawlor/ractor - Actor framework",
            "https://docs.rs/tokio/latest/tokio/time/fn.interval.html - Poll timer",
            "https://docs.rs/tokio/latest/tokio/process/struct.Command.html - Async subprocess",
        ]

        codebase_patterns: [
            {
                pattern:          "Actor message handling"
                example_location: "crates/factory-core/src/actors/signal_bus.rs"
                how_to_apply:     "Match on message enum, handle each variant, return Ok(())"
            },
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Subprocess execution"
                example_location: "crates/factory-core/src/jj.rs"
                how_to_apply:     "Use tokio::process::Command, check exit status, parse output"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use ractor for actor implementation - consistent with factory-kou",
            "Use tokio::time::interval for poll scheduling",
            "Use tokio::process::Command for async bd CLI execution",
            "Use HashSet<String> for known beads tracking",
            "Use VecDeque for event buffer with FIFO semantics",
            "Implement graceful shutdown with drain timeout",
            "Add structured logging with tracing",
            "Validate poll interval in config constructor",
            "Use oneshot channels for request-reply messages",
            "Emit events immediately on detection, don't batch",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT block the Tokio runtime with sync bd CLI calls",
            "Do NOT allow poll interval < 1 second",
            "Do NOT cache bead data between polls",
            "Do NOT emit events for already-known assignments",
            "Do NOT use unbounded buffers for events",
            "Do NOT ignore bd CLI errors silently",
        ]

        code_patterns: [
            {
                name:     "Poll Timer Pattern"
                use_when: "Scheduling periodic polls"
                example:  """
                    async fn schedule_poll(&self, myself: ActorRef<BeadsWatcherMsg>) {
                        let interval = self.config.poll_interval;
                        tokio::spawn(async move {
                            tokio::time::sleep(interval).await;
                            let _ = myself.cast(BeadsWatcherMsg::Poll);
                        });
                    }
                    """
            },
            {
                name:     "BD CLI Query Pattern"
                use_when: "Querying beads database via CLI"
                example:  """
                    async fn query_assigned_beads(&self) -> Result<Vec<String>> {
                        let output = Command::new("bd")
                            .args(["list", "--status", "in_progress", "--json"])
                            .current_dir(&self.beads_path)
                            .output()
                            .await
                            .map_err(|e| Error::BdCliExecution { source: e })?;

                        if !output.status.success() {
                            return Err(Error::BdCliFailed {
                                stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
                            });
                        }

                        let beads: Vec<Bead> = serde_json::from_slice(&output.stdout)
                            .map_err(|e| Error::BdCliParsing { source: e })?;

                        Ok(beads.into_iter().map(|b| b.id).collect())
                    }
                    """
            },
            {
                name:     "Change Detection Pattern"
                use_when: "Detecting new assignments"
                example:  """
                    fn detect_new_assignments(&mut self, current: &[String]) -> Vec<String> {
                        let current_set: HashSet<_> = current.iter().collect();
                        let new_beads: Vec<String> = current_set
                            .difference(&self.known_beads)
                            .map(|&id| id.clone())
                            .collect();

                        // Update known set
                        self.known_beads = current_set.into_iter().cloned().collect();

                        new_beads
                    }
                    """
            },
            {
                name:     "Event Buffering Pattern"
                use_when: "Buffering events during signal bus outage"
                example:  """
                    fn buffer_event(&mut self, event: BeadAssigned) -> Result<()> {
                        if self.event_buffer.len() >= self.config.buffer_capacity {
                            let dropped = self.event_buffer.pop_front();
                            tracing::warn!(
                                dropped_bead_id = ?dropped.map(|e| e.bead_id),
                                "Event buffer full, dropping oldest event"
                            );
                        }
                        self.event_buffer.push_back(event);
                        Ok(())
                    }
                    """
            },
        ]
    }
}
