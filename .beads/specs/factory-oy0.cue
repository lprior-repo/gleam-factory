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

"factory-oy0": #ValidBead & {
    // ============================================================================
    // BEAD: factory-oy0 - Implement structured logging with tracing spans per actor
    // ============================================================================

    id:              "factory-oy0"
    title:           "Observability: Implement structured logging with tracing spans per actor"
    type:            "task"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["observability", "logging", "tracing", "actors", "P2"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use the tracing crate for all structured logging",
            "THE SYSTEM SHALL create a span for each actor lifecycle (spawn to termination)",
            "THE SYSTEM SHALL propagate span context across async message handlers",
            "THE SYSTEM SHALL output JSON-formatted logs in production mode",
            "THE SYSTEM SHALL output human-readable logs in development mode",
            "THE SYSTEM SHALL never use println!, eprintln!, or dbg! for logging",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN an actor is spawned"
                shall:   "THE SYSTEM SHALL create a span with actor.id, actor.type, and parent supervisor fields"
            },
            {
                trigger: "WHEN an actor receives a message"
                shall:   "THE SYSTEM SHALL log message.type and message.id within the actor's span"
            },
            {
                trigger: "WHEN an actor's state transitions"
                shall:   "THE SYSTEM SHALL log from_state, to_state, and trigger within the actor's span"
            },
            {
                trigger: "WHEN an actor terminates"
                shall:   "THE SYSTEM SHALL log exit_reason, lifetime_ms, and messages_processed"
            },
            {
                trigger: "WHEN an error occurs during message handling"
                shall:   "THE SYSTEM SHALL log error with span context, including actor.id and message.type"
            },
            {
                trigger: "WHEN a supervisor restarts a child"
                shall:   "THE SYSTEM SHALL correlate restart logs with both supervisor and child spans"
            },
        ]

        state_driven: [
            {
                state: "WHILE an actor is processing a message"
                shall: "THE SYSTEM SHALL maintain an active span for the message processing"
            },
            {
                state: "WHILE the tracing subscriber is active"
                shall: "THE SYSTEM SHALL buffer logs if the output is temporarily unavailable"
            },
            {
                state: "WHILE in production mode"
                shall: "THE SYSTEM SHALL filter logs at INFO level by default"
            },
            {
                state: "WHILE in development mode"
                shall: "THE SYSTEM SHALL show DEBUG level logs with pretty formatting"
            },
        ]

        unwanted: [
            {
                condition: "IF a span is created without required fields"
                shall_not: "THE SYSTEM SHALL NOT allow unidentifiable spans in production"
                because:   "Spans without actor.id or type are useless for debugging"
            },
            {
                condition: "IF logging causes blocking I/O"
                shall_not: "THE SYSTEM SHALL NOT block the Tokio runtime with synchronous log writes"
                because:   "Blocking I/O causes thread starvation and actor message delays"
            },
            {
                condition: "IF log volume exceeds output capacity"
                shall_not: "THE SYSTEM SHALL NOT drop logs silently without warning"
                because:   "Silent log loss hides critical errors during incidents"
            },
            {
                condition: "IF sensitive data is present in message payloads"
                shall_not: "THE SYSTEM SHALL NOT log full message contents without redaction"
                because:   "Leaking secrets/PII in logs violates security requirements"
            },
        ]

        complex: [
            {
                state:   "WHILE an actor is handling a call (request-reply)"
                trigger: "WHEN the response is sent"
                shall:   "THE SYSTEM SHALL record latency_ms in the span and close it"
            },
            {
                state:   "WHILE a supervisor is performing one_for_all restart"
                trigger: "WHEN multiple children are being restarted"
                shall:   "THE SYSTEM SHALL correlate all restart events under a single restart_cycle span"
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
                    field:           "TracingConfig"
                    type:            "Struct"
                    constraints:     "Must specify format (json/pretty), default_level, and optional file output"
                    example_valid:   "TracingConfig { format: Format::Json, level: Level::INFO, file: None }"
                    example_invalid: "TracingConfig { format: Format::Json, level: Level::TRACE, .. } // TRACE in prod is too verbose"
                },
                {
                    field:           "ActorSpanFields"
                    type:            "Struct"
                    constraints:     "Must include actor_id, actor_type; optional: parent_id, supervisor_id"
                    example_valid:   "ActorSpanFields { actor_id: \"worker-1\", actor_type: \"TaskWorker\", supervisor_id: Some(\"root\") }"
                    example_invalid: "ActorSpanFields { actor_id: \"\", .. } // Empty actor ID"
                },
            ]
            system_state: [
                "Tokio runtime initialized (factory-00s completed)",
                "Supervisor framework available (factory-4pz completed)",
                "tracing crate added to dependencies",
                "tracing-subscriber crate added to dependencies",
            ]
        }

        postconditions: {
            state_changes: [
                "Global tracing subscriber installed",
                "All actors instrumented with spans",
                "Log output configured (stdout/file)",
                "Span context propagated through actor hierarchy",
            ]
            return_guarantees: [
                {
                    field:     "init_tracing()"
                    guarantee: "Returns Result<TracingGuard, InitError> with guard that flushes on drop"
                },
                {
                    field:     "ActorSpan::new()"
                    guarantee: "Returns entered Span that auto-closes when dropped"
                },
                {
                    field:     "ActorSpan::record_message()"
                    guarantee: "Records message fields in current span, returns immediately"
                },
                {
                    field:     "ActorSpan::record_error()"
                    guarantee: "Records error with backtrace context in current span"
                },
            ]
            side_effects: [
                "Logs written to configured output (stdout/file)",
                "Span data collected by subscriber",
                "Metrics may be derived from span timings",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Every actor has exactly one active span during its lifecycle",
            "Span parent-child relationships match supervisor-actor relationships",
            "All spans have actor_id and actor_type fields",
            "JSON output is valid JSON (one object per line)",
            "Log levels follow: ERROR > WARN > INFO > DEBUG > TRACE",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Multiple tracing subscribers conflict"
                prevention:  "Use tracing::subscriber::set_global_default only once at startup"
                test_for_it: "test_single_subscriber_initialization"
            },
            {
                failure:     "Span context lost across await points"
                prevention:  "Use #[instrument] macro or explicit span.enter() guards"
                test_for_it: "test_span_context_across_await"
            },
            {
                failure:     "tracing-subscriber feature flags missing"
                prevention:  "Enable json, env-filter features in Cargo.toml"
                test_for_it: "test_json_output_format"
            },
            {
                failure:     "ractor actor handlers not instrumented"
                prevention:  "Wrap Actor trait methods with #[instrument(skip(self, state))]"
                test_for_it: "test_actor_message_spans"
            },
        ]

        usability_failures: [
            {
                failure:     "Logs too verbose in production"
                prevention:  "Default to INFO level, allow RUST_LOG override"
                test_for_it: "test_default_log_level_info"
            },
            {
                failure:     "JSON logs hard to read during development"
                prevention:  "Auto-detect TTY and use pretty format for terminals"
                test_for_it: "test_pretty_format_for_tty"
            },
            {
                failure:     "Cannot correlate logs across actors"
                prevention:  "Include correlation_id field, propagate through message chains"
                test_for_it: "test_correlation_across_actors"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Span timing incorrect due to async gaps"
                prevention:  "Use tracing's built-in async span tracking, not manual timing"
                test_for_it: "test_span_timing_accuracy"
            },
            {
                failure:     "Logs dropped under high volume"
                prevention:  "Use non-blocking writer with bounded buffer, log warning when buffer full"
                test_for_it: "test_high_volume_logging"
            },
            {
                failure:     "Sensitive data leaked in logs"
                prevention:  "Implement Skip/Redact traits for sensitive fields, use #[instrument(skip(secret))]"
                test_for_it: "test_sensitive_data_redacted"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_actor_lifecycle_span"
                given: "An actor that processes messages and terminates"
                when:  "Actor goes through full lifecycle"
                then: [
                    "Span created at spawn with actor.id and actor.type",
                    "Message events logged within span",
                    "Span closed at termination with lifetime_ms",
                ]
                real_input: """
                    let (logs, _guard) = capture_logs();

                    let actor = TestActor::spawn("worker-1").await?;
                    actor.cast(TestMsg::Ping).await?;
                    actor.stop().await?;

                    let entries = logs.into_entries();
                    """
                expected_output: """
                    // Verify span fields
                    let spawn_entry = entries.find(|e| e.message == "actor.spawned");
                    assert_eq!(spawn_entry.fields["actor.id"], "worker-1");
                    assert_eq!(spawn_entry.fields["actor.type"], "TestActor");

                    // Verify message logged in span
                    let msg_entry = entries.find(|e| e.message == "message.received");
                    assert_eq!(msg_entry.fields["message.type"], "Ping");
                    assert_eq!(msg_entry.span["actor.id"], "worker-1");

                    // Verify termination
                    let stop_entry = entries.find(|e| e.message == "actor.stopped");
                    assert!(stop_entry.fields["lifetime_ms"].as_u64() > 0);
                    """
            },
            {
                name:  "test_supervisor_child_span_correlation"
                given: "A supervisor with child actors"
                when:  "Supervisor spawns children"
                then: [
                    "Child spans have parent_id linking to supervisor",
                    "Supervisor span contains children.count field",
                    "Spans form proper hierarchy",
                ]
                real_input: """
                    let (logs, _guard) = capture_logs();

                    let supervisor = FactorySupervisor::spawn(config, vec![
                        ChildSpec::worker("child-1", || TestWorker::spawn()),
                        ChildSpec::worker("child-2", || TestWorker::spawn()),
                    ]).await?;

                    let entries = logs.into_entries();
                    """
                expected_output: """
                    let sup_entry = entries.find(|e| e.fields["actor.id"] == "supervisor");
                    let child1_entry = entries.find(|e| e.fields["actor.id"] == "child-1");
                    let child2_entry = entries.find(|e| e.fields["actor.id"] == "child-2");

                    // Verify hierarchy
                    assert_eq!(child1_entry.fields["supervisor.id"], "supervisor");
                    assert_eq!(child2_entry.fields["supervisor.id"], "supervisor");
                    """
            },
            {
                name:  "test_json_output_format"
                given: "Tracing configured for JSON output"
                when:  "Log events are emitted"
                then: [
                    "Each line is valid JSON",
                    "Contains timestamp, level, message, fields",
                    "Span context included in each event",
                ]
                real_input: """
                    let config = TracingConfig::new()
                        .format(Format::Json)
                        .level(Level::DEBUG);

                    let (output, _guard) = init_tracing_with_capture(config)?;

                    tracing::info!(actor.id = "test", "test message");

                    let line = output.take_line();
                    """
                expected_output: """
                    let json: serde_json::Value = serde_json::from_str(&line)?;
                    assert!(json["timestamp"].is_string());
                    assert_eq!(json["level"], "INFO");
                    assert_eq!(json["message"], "test message");
                    assert_eq!(json["actor.id"], "test");
                    """
            },
            {
                name:  "test_state_transition_logging"
                given: "An actor with explicit state machine"
                when:  "Actor transitions between states"
                then: [
                    "State transition logged with from_state and to_state",
                    "Trigger that caused transition logged",
                    "Transition recorded within actor span",
                ]
                real_input: """
                    let (logs, _guard) = capture_logs();

                    let actor = StatefulActor::spawn("state-actor").await?;
                    actor.cast(StateMsg::Start).await?;  // Idle -> Running
                    actor.cast(StateMsg::Pause).await?;  // Running -> Paused

                    let entries = logs.into_entries();
                    """
                expected_output: """
                    let transitions: Vec<_> = entries
                        .filter(|e| e.message == "state.transition")
                        .collect();

                    assert_eq!(transitions[0].fields["from"], "Idle");
                    assert_eq!(transitions[0].fields["to"], "Running");
                    assert_eq!(transitions[0].fields["trigger"], "Start");

                    assert_eq!(transitions[1].fields["from"], "Running");
                    assert_eq!(transitions[1].fields["to"], "Paused");
                    """
            },
            {
                name:  "test_error_logging_with_context"
                given: "An actor that encounters an error"
                when:  "Error occurs during message handling"
                then: [
                    "Error logged at ERROR level",
                    "Error message and type captured",
                    "Actor span context preserved",
                    "Message that caused error identified",
                ]
                real_input: """
                    let (logs, _guard) = capture_logs();

                    let actor = TestActor::spawn("error-actor").await?;
                    let _ = actor.call(TestMsg::FailingOperation).await;

                    let entries = logs.into_entries();
                    """
                expected_output: """
                    let error_entry = entries.find(|e| e.level == "ERROR");
                    assert!(error_entry.message.contains("operation failed"));
                    assert_eq!(error_entry.span["actor.id"], "error-actor");
                    assert_eq!(error_entry.fields["message.type"], "FailingOperation");
                    assert!(error_entry.fields["error.type"].is_string());
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_duplicate_subscriber_returns_error"
                given: "Tracing subscriber already initialized"
                when:  "init_tracing() is called again"
                then: [
                    "Returns Err(Error::TracingAlreadyInitialized)",
                    "Original subscriber continues working",
                ]
                real_input: """
                    let _guard1 = init_tracing(TracingConfig::default())?;
                    let result = init_tracing(TracingConfig::default());
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::TracingAlreadyInitialized)));
                    """
            },
            {
                name:  "test_invalid_filter_directive"
                given: "Invalid RUST_LOG filter string"
                when:  "Tracing is initialized with bad filter"
                then: [
                    "Returns Err(Error::InvalidFilterDirective)",
                    "Error message includes the invalid filter",
                ]
                real_input: """
                    std::env::set_var("RUST_LOG", "invalid[[[filter");
                    let result = init_tracing(TracingConfig::default());
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::InvalidFilterDirective { .. })));
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_high_frequency_logging"
                scenario: "1000 messages processed rapidly"
                input:    "Actor receives 1000 messages in 100ms"
                expected: "All messages logged, no drops, reasonable memory usage"
            },
            {
                name:     "test_deeply_nested_supervision"
                scenario: "5-level deep supervisor tree"
                input:    "Root -> Sup1 -> Sup2 -> Sup3 -> Worker"
                expected: "Span hierarchy preserved through all levels"
            },
            {
                name:     "test_concurrent_actors_logging"
                scenario: "100 actors logging simultaneously"
                input:    "Spawn 100 actors, each logs 10 events"
                expected: "All events captured, no interleaving corruption"
            },
            {
                name:     "test_large_field_values"
                scenario: "Log field with very long string"
                input:    "Field value is 10KB string"
                expected: "Value truncated with '...[truncated]' marker"
            },
            {
                name:     "test_unicode_in_fields"
                scenario: "Log field contains emoji and CJK characters"
                input:    "actor.id contains rocket emoji and Chinese characters"
                expected: "JSON properly escapes unicode, pretty format displays correctly"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in tracing module"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/tracing.rs returns empty"
            },
            {
                name:     "test_invariant_no_println"
                verifies: "No println! or eprintln! in codebase"
                test:     "grep -r 'println!\\|eprintln!' crates/ returns empty (except tests)"
            },
            {
                name:     "test_postcondition_json_validity"
                verifies: "All JSON log lines are valid JSON"
                test:     "Parse each line with serde_json, all succeed"
            },
            {
                name:     "test_invariant_required_span_fields"
                verifies: "All actor spans have actor.id and actor.type"
                test:     "Filter spans by target, verify fields present"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_tracing_pipeline"
            description: "Complete tracing lifecycle: init -> actor spans -> shutdown with flush"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/tracing_e2e.rs"
                        content: """
                            use factory_core::tracing::{init_tracing, TracingConfig, Format};
                            use factory_core::actor::{Actor, TestActor};
                            use factory_core::supervisor::{FactorySupervisor, SupervisorConfig, ChildSpec};
                            use factory_core::error::Result;
                            use std::time::Duration;
                            use tempfile::NamedTempFile;

                            #[tokio::test]
                            async fn test_full_tracing_pipeline() -> Result<()> {
                                // Create temp file for log output
                                let log_file = NamedTempFile::new()?;

                                // Initialize tracing with JSON output to file
                                let config = TracingConfig::new()
                                    .format(Format::Json)
                                    .level(tracing::Level::DEBUG)
                                    .file(log_file.path());

                                let guard = init_tracing(config)?;

                                // Spawn supervisor with workers
                                let sup_config = SupervisorConfig::default();
                                let supervisor = FactorySupervisor::spawn(sup_config, vec![
                                    ChildSpec::worker("worker-1", || TestActor::spawn()),
                                    ChildSpec::worker("worker-2", || TestActor::spawn()),
                                ]).await?;

                                // Send messages
                                supervisor.child("worker-1")?.cast(TestMsg::Ping).await?;
                                supervisor.child("worker-2")?.cast(TestMsg::Ping).await?;

                                // Shutdown and flush
                                supervisor.shutdown().await?;
                                drop(guard); // Flush logs

                                // Verify log contents
                                let log_contents = std::fs::read_to_string(log_file.path())?;
                                let lines: Vec<&str> = log_contents.lines().collect();

                                // Each line should be valid JSON
                                for line in &lines {
                                    serde_json::from_str::<serde_json::Value>(line)?;
                                }

                                // Should have spawn, message, and stop events
                                assert!(lines.iter().any(|l| l.contains("actor.spawned")));
                                assert!(lines.iter().any(|l| l.contains("message.received")));
                                assert!(lines.iter().any(|l| l.contains("actor.stopped")));

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
                command:    "moon run factory-core:test -- --test tracing_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_tracing_pipeline ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/tracing.rs"
                        contains: "pub fn init_tracing"
                    },
                    {
                        path:     "crates/factory-core/src/tracing.rs"
                        contains: "pub struct TracingConfig"
                    },
                    {
                        path:     "crates/factory-core/src/tracing.rs"
                        contains: "pub enum Format"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/tracing_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_log_correlation_across_restart"
                description: "Verify logs correlate when supervisor restarts crashed child"
                steps: [
                    {action: "Initialize tracing with JSON output", verify: "Subscriber active"},
                    {action: "Spawn supervisor with child", verify: "Both have correlated spans"},
                    {action: "Crash the child actor", verify: "Error logged with actor span"},
                    {action: "Wait for restart", verify: "New child span links to supervisor"},
                    {action: "Verify correlation_id links old and new child", verify: "restart_cycle span present"},
                ]
            },
            {
                name:        "e2e_production_json_to_file"
                description: "Verify production-style JSON logging to file"
                steps: [
                    {action: "Configure JSON format with file output", verify: "File created"},
                    {action: "Run typical actor workload", verify: "Events written"},
                    {action: "Parse file with jq", verify: "All lines valid JSON"},
                    {action: "Query for specific actor", verify: "jq filter works"},
                    {action: "Verify no sensitive data logged", verify: "No secrets in output"},
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
                task:      "Write test: test_actor_lifecycle_span"
                file:      "crates/factory-core/src/tracing.rs"
                what:      "Test that actor spawn/process/stop creates proper span hierarchy"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_json_output_format"
                file:      "crates/factory-core/src/tracing.rs"
                what:      "Test that JSON output is valid and contains required fields"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_supervisor_child_span_correlation"
                file:      "crates/factory-core/src/tracing.rs"
                what:      "Test that child spans link to supervisor via parent_id"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_error_logging_with_context"
                file:      "crates/factory-core/src/tracing.rs"
                what:      "Test that errors are logged with full span context"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_state_transition_logging"
                file:      "crates/factory-core/src/tracing.rs"
                what:      "Test that state transitions are logged with from/to/trigger"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add tracing dependencies to Cargo.toml"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dependencies]
                    tracing = "0.1"
                    tracing-subscriber = { version = "0.3", features = ["json", "env-filter"] }
                    tracing-appender = "0.2"
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["workspace dependencies for version management"]
            },
            {
                task: "Define TracingConfig struct"
                file: "crates/factory-core/src/tracing.rs"
                what: """
                    Configuration for tracing: format (json/pretty), default level,
                    optional file output, env filter support.
                    Implement builder pattern with sensible defaults.
                    """
                done_when:     "Struct compiles with defaults"
                patterns_to_use: [
                    "Builder pattern",
                    "Default trait implementation",
                    "Level::INFO default for production",
                ]
            },
            {
                task: "Define Format enum"
                file: "crates/factory-core/src/tracing.rs"
                what: """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
                    pub enum Format {
                        #[default]
                        Json,
                        Pretty,
                        Compact,
                    }
                    """
                done_when:     "Enum compiles"
                patterns_to_use: ["Auto-detect TTY for Pretty default"]
            },
            {
                task: "Implement init_tracing function"
                file: "crates/factory-core/src/tracing.rs"
                what: """
                    Initialize global tracing subscriber based on config.
                    Return guard that flushes on drop.
                    Handle duplicate initialization gracefully.
                    """
                done_when:     "Basic subscriber initialization works"
                patterns_to_use: [
                    "tracing_subscriber::fmt::layer()",
                    "tracing_subscriber::EnvFilter",
                    "tracing::subscriber::set_global_default",
                    "OnceCell for single initialization",
                ]
            },
            {
                task: "Implement ActorSpan helper"
                file: "crates/factory-core/src/tracing.rs"
                what: """
                    Helper struct for creating and managing actor lifecycle spans.
                    Ensures required fields (actor.id, actor.type) are always present.
                    Provides methods for recording messages, errors, state transitions.
                    """
                done_when:     "ActorSpan can be used in actor implementations"
                patterns_to_use: [
                    "tracing::span! macro",
                    "span.record() for dynamic fields",
                    "span.enter() for scope guards",
                ]
            },
            {
                task: "Implement log capture for testing"
                file: "crates/factory-core/src/tracing.rs"
                what: """
                    Test utility to capture log output for assertions.
                    Returns structured log entries that can be queried.
                    """
                done_when:     "Tests can verify log output"
                patterns_to_use: [
                    "tracing_subscriber::fmt::MakeWriter",
                    "Arc<Mutex<Vec<u8>>> for capture buffer",
                    "cfg(test) for test-only code",
                ]
            },
            {
                task: "Add #[instrument] to actor message handlers"
                file: "crates/factory-core/src/actor.rs"
                what: """
                    Instrument Actor trait methods with tracing spans.
                    Skip self and state parameters.
                    Record message type as span field.
                    """
                done_when:     "Actor message handling creates spans"
                patterns_to_use: [
                    "#[instrument(skip(self, state), fields(actor.id = %self.id))]",
                    "tracing::Instrument trait for async",
                ]
            },
            {
                task: "Add span correlation to supervisor"
                file: "crates/factory-core/src/supervisor.rs"
                what: """
                    Create supervisor span, pass to children as parent.
                    Create restart_cycle span for one_for_all restarts.
                    Log restart events with correlation fields.
                    """
                done_when:     "Supervisor and child spans are correlated"
                patterns_to_use: [
                    "Span::current() for parent",
                    "span.follows_from() for restart correlation",
                    "span.record() for dynamic child count",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export tracing module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod tracing;"
                done_when: "External crates can import factory_core::tracing"
            },
            {
                task:      "Add tracing errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "TracingAlreadyInitialized, InvalidFilterDirective, LogWriteFailed variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Update CLI to initialize tracing"
                file:      "crates/factory/src/main.rs"
                what:      "Call init_tracing at startup with CLI-configured format/level"
                done_when: "CLI outputs structured logs"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/tracing.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify no println"
                done_when: "grep finds no println/eprintln in non-test code"
                commands: ["rg 'println!|eprintln!' crates/factory-core/src/ --glob '!*_test.rs'"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification"
                done_when: "CLI outputs JSON logs that can be piped to jq"
                commands: [
                    "factory list 2>&1 | jq .",
                ]
                expected: "Valid JSON output"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "No logs appearing"
                likely_cause: "Tracing subscriber not initialized or wrong level"
                where_to_look: [
                    {
                        file:          "crates/factory/src/main.rs"
                        function:      "main()"
                        what_to_check: "Is init_tracing() called before any other operations?"
                    },
                    {
                        file:          "RUST_LOG environment"
                        what_to_check: "Is RUST_LOG set to filter out the logs?"
                    },
                ]
                fix_pattern: "Ensure init_tracing() is called first in main(), check RUST_LOG=debug"
            },
            {
                symptom:      "Span context lost in async handlers"
                likely_cause: "Missing .instrument() or #[instrument] on async functions"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actor.rs"
                        function:      "async message handlers"
                        what_to_check: "Are async functions instrumented?"
                    },
                ]
                fix_pattern: "Use #[instrument] macro or .instrument(span) on async blocks"
            },
            {
                symptom:      "JSON logs are malformed"
                likely_cause: "Multiple subscribers or wrong formatter"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/tracing.rs"
                        function:      "init_tracing()"
                        what_to_check: "Is only one subscriber initialized? Is json() formatter used?"
                    },
                ]
                fix_pattern: "Use OnceCell to ensure single initialization, use .json() layer"
            },
            {
                symptom:      "Logs missing actor.id field"
                likely_cause: "ActorSpan not used or span created without required fields"
                where_to_look: [
                    {
                        file:          "Actor implementations"
                        function:      "pre_start() or spawn()"
                        what_to_check: "Is ActorSpan::new() used with actor_id?"
                    },
                ]
                fix_pattern: "Always use ActorSpan::new() which enforces required fields"
            },
            {
                symptom:      "Log file not created"
                likely_cause: "Missing directory or permissions"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/tracing.rs"
                        function:      "init_tracing()"
                        what_to_check: "Is parent directory created? Are permissions checked?"
                    },
                ]
                fix_pattern: "Create parent directories before opening file, return Error on failure"
            },
            {
                symptom:      "Tests fail with 'subscriber already initialized'"
                likely_cause: "Tests share global subscriber state"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/tracing.rs"
                        function:      "capture_logs() or test setup"
                        what_to_check: "Are tests using thread-local or test-specific subscribers?"
                    },
                ]
                fix_pattern: "Use tracing::subscriber::with_default() for test-local subscribers"
            },
        ]

        debugging_commands: [
            {
                scenario: "When logs are not appearing"
                run:      "RUST_LOG=trace factory list 2>&1 | head -20"
                look_for: "Any output, subscriber initialization messages"
            },
            {
                scenario: "When JSON is malformed"
                run:      "factory list 2>&1 | jq . 2>&1 | head -5"
                look_for: "jq parse errors indicate line and position"
            },
            {
                scenario: "When spans are not correlated"
                run:      "factory list 2>&1 | jq 'select(.span.actor_id)'"
                look_for: "span.parent and span.actor_id fields"
            },
            {
                scenario: "When file logging fails"
                run:      "strace -e openat,write factory list 2>&1 | grep -E 'log|trace'"
                look_for: "File open failures, permission denied"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_actor_lifecycle_span passes",
            "[ ] test_supervisor_child_span_correlation passes",
            "[ ] test_json_output_format passes",
            "[ ] test_state_transition_logging passes",
            "[ ] test_error_logging_with_context passes",
            "[ ] test_duplicate_subscriber_returns_error passes",
            "[ ] test_high_frequency_logging passes",
            "[ ] test_concurrent_actors_logging passes",
            "[ ] E2E full tracing pipeline test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Zero println! or eprintln! calls (use tracing macros)",
            "[ ] TracingConfig struct with builder pattern",
            "[ ] Format enum with Json, Pretty, Compact",
            "[ ] init_tracing() returns TracingGuard",
            "[ ] ActorSpan helper enforces required fields",
            "[ ] All actor handlers instrumented with spans",
            "[ ] Supervisor-child span correlation implemented",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs explaining structured logging approach",
            "[ ] TracingConfig builder methods documented",
            "[ ] Format variants documented with use cases",
            "[ ] Example JSON log output in doc comments",
            "[ ] RUST_LOG filter syntax documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add tracing"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add tracing error variants"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - must add tracing-subscriber with features"
            },
            {
                path:      "crates/factory-core/src/actor.rs"
                relevance: "Actor trait - must add #[instrument] to handlers"
            },
            {
                path:      "crates/factory-core/src/supervisor.rs"
                relevance: "Supervisor - must add span correlation for children"
            },
            {
                path:      "crates/factory/src/main.rs"
                relevance: "CLI entry point - must call init_tracing()"
            },
            {
                path:      ".beads/specs/factory-4pz.cue"
                relevance: "Supervisor spec - tracing must integrate with supervisor patterns"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-00s"
                description: "Actor framework must be in place for instrumentation"
                what_it_provides: "Actor trait, ActorRef, message handling"
            },
            {
                bead_id:     "factory-4pz"
                description: "Supervisor framework for span correlation"
                what_it_provides: "FactorySupervisor, child management, restart handling"
            },
        ]

        external_references: [
            "https://docs.rs/tracing - Official tracing documentation",
            "https://docs.rs/tracing-subscriber - Subscriber configuration",
            "https://tokio.rs/tokio/topics/tracing - Tracing with Tokio",
            "https://www.lpalmieri.com/posts/2020-09-27-zero-to-production-4-are-we-observable-yet/ - Rust observability",
            "https://opentelemetry.io/docs/concepts/signals/traces/ - Span concepts",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, log errors at call site"
            },
            {
                pattern:          "Builder Pattern"
                example_location: "TracingConfig, existing builders in codebase"
                how_to_apply:     "Methods return Self, final build() validates"
            },
            {
                pattern:          "Instrument Macro"
                example_location: "tracing crate docs"
                how_to_apply:     "#[instrument(skip(self, state), fields(actor.id))] on async fns"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use tracing, NOT log crate - tracing has spans and structured fields",
            "Use tracing-subscriber with json and env-filter features",
            "Use #[instrument] macro for automatic span creation",
            "Use span.record() for fields known only at runtime",
            "Use tracing::Instrument trait for async blocks",
            "Default to INFO level in production, DEBUG in development",
            "Include actor.id and actor.type in EVERY actor span",
            "Use tracing-appender for non-blocking file writes",
            "Use EnvFilter for RUST_LOG support",
            "Implement TracingGuard that flushes on drop",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT use println!, eprintln!, or dbg!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT block Tokio runtime with synchronous log writes",
            "Do NOT log sensitive data (passwords, tokens, PII)",
            "Do NOT use global mutable state for log capture in tests",
            "Do NOT call set_global_default multiple times",
        ]

        code_patterns: [
            {
                name:     "TracingConfig Builder"
                use_when: "Configuring the tracing subscriber"
                example:  """
                    #[derive(Debug, Clone)]
                    pub struct TracingConfig {
                        format: Format,
                        level: Level,
                        file: Option<PathBuf>,
                    }

                    impl Default for TracingConfig {
                        fn default() -> Self {
                            Self {
                                format: if atty::is(atty::Stream::Stderr) {
                                    Format::Pretty
                                } else {
                                    Format::Json
                                },
                                level: Level::INFO,
                                file: None,
                            }
                        }
                    }

                    impl TracingConfig {
                        pub fn new() -> Self {
                            Self::default()
                        }

                        pub fn format(mut self, format: Format) -> Self {
                            self.format = format;
                            self
                        }

                        pub fn level(mut self, level: Level) -> Self {
                            self.level = level;
                            self
                        }

                        pub fn file(mut self, path: impl Into<PathBuf>) -> Self {
                            self.file = Some(path.into());
                            self
                        }
                    }
                    """
            },
            {
                name:     "init_tracing Function"
                use_when: "Initializing the global tracing subscriber"
                example:  """
                    use std::sync::OnceLock;
                    use tracing_subscriber::{fmt, prelude::*, EnvFilter};

                    static TRACING_INIT: OnceLock<()> = OnceLock::new();

                    pub fn init_tracing(config: TracingConfig) -> Result<TracingGuard> {
                        // Check for duplicate initialization
                        if TRACING_INIT.get().is_some() {
                            return Err(Error::TracingAlreadyInitialized);
                        }

                        // Build filter from RUST_LOG or default
                        let filter = EnvFilter::try_from_default_env()
                            .unwrap_or_else(|_| EnvFilter::new(config.level.to_string()));

                        // Build subscriber based on format
                        let subscriber = tracing_subscriber::registry().with(filter);

                        match config.format {
                            Format::Json => {
                                let layer = fmt::layer()
                                    .json()
                                    .with_span_list(true)
                                    .with_current_span(true);
                                subscriber.with(layer).try_init()
                                    .map_err(|e| Error::TracingInitFailed { reason: e.to_string() })?;
                            }
                            Format::Pretty => {
                                let layer = fmt::layer()
                                    .pretty()
                                    .with_target(true);
                                subscriber.with(layer).try_init()
                                    .map_err(|e| Error::TracingInitFailed { reason: e.to_string() })?;
                            }
                            Format::Compact => {
                                let layer = fmt::layer().compact();
                                subscriber.with(layer).try_init()
                                    .map_err(|e| Error::TracingInitFailed { reason: e.to_string() })?;
                            }
                        }

                        TRACING_INIT.set(()).ok();
                        Ok(TracingGuard { _private: () })
                    }
                    """
            },
            {
                name:     "ActorSpan Helper"
                use_when: "Creating spans for actor lifecycles"
                example:  """
                    use tracing::{span, Level, Span};

                    pub struct ActorSpan {
                        span: Span,
                    }

                    impl ActorSpan {
                        pub fn new(actor_id: &str, actor_type: &str) -> Self {
                            let span = span!(
                                Level::INFO,
                                "actor",
                                actor.id = %actor_id,
                                actor.type = %actor_type,
                                messages_processed = tracing::field::Empty,
                                lifetime_ms = tracing::field::Empty,
                            );
                            Self { span }
                        }

                        pub fn with_supervisor(mut self, supervisor_id: &str) -> Self {
                            self.span.record("supervisor.id", supervisor_id);
                            self
                        }

                        pub fn enter(&self) -> tracing::span::EnteredSpan {
                            self.span.clone().entered()
                        }

                        pub fn record_message(&self, msg_type: &str) {
                            tracing::info!(
                                parent: &self.span,
                                message.type = %msg_type,
                                "message.received"
                            );
                        }

                        pub fn record_state_transition(&self, from: &str, to: &str, trigger: &str) {
                            tracing::info!(
                                parent: &self.span,
                                from = %from,
                                to = %to,
                                trigger = %trigger,
                                "state.transition"
                            );
                        }

                        pub fn record_error(&self, error: &Error, msg_type: &str) {
                            tracing::error!(
                                parent: &self.span,
                                error = %error,
                                error.type = %std::any::type_name_of_val(error),
                                message.type = %msg_type,
                                "message.failed"
                            );
                        }

                        pub fn close(self, messages_processed: u64, lifetime_ms: u64) {
                            self.span.record("messages_processed", messages_processed);
                            self.span.record("lifetime_ms", lifetime_ms);
                            tracing::info!(parent: &self.span, "actor.stopped");
                        }
                    }
                    """
            },
            {
                name:     "Instrumented Actor Handler"
                use_when: "Adding tracing to actor message handlers"
                example:  """
                    impl Actor for TaskWorker {
                        type Msg = WorkerMsg;
                        type State = WorkerState;
                        type Arguments = WorkerArgs;

                        async fn pre_start(
                            &self,
                            myself: ActorRef<Self::Msg>,
                            args: Self::Arguments,
                        ) -> Result<Self::State, ActorProcessingErr> {
                            let span = ActorSpan::new(&args.id, "TaskWorker")
                                .with_supervisor(&args.supervisor_id);

                            tracing::info!(parent: span.enter(), "actor.spawned");

                            Ok(WorkerState {
                                span,
                                start_time: Instant::now(),
                                messages_processed: 0,
                            })
                        }

                        #[instrument(skip(self, myself, message, state), fields(message.type))]
                        async fn handle(
                            &self,
                            myself: ActorRef<Self::Msg>,
                            message: Self::Msg,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            let _guard = state.span.enter();

                            // Record message type in span
                            Span::current().record("message.type", std::any::type_name_of_val(&message));
                            state.span.record_message(std::any::type_name_of_val(&message));

                            state.messages_processed += 1;

                            match message {
                                WorkerMsg::Process(task) => {
                                    self.process_task(task, state).await?;
                                }
                                WorkerMsg::GetStatus { respond_to } => {
                                    let status = self.get_status(state);
                                    respond_to.send(status).ok();
                                }
                            }

                            Ok(())
                        }

                        async fn post_stop(
                            &self,
                            _myself: ActorRef<Self::Msg>,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            let lifetime_ms = state.start_time.elapsed().as_millis() as u64;
                            state.span.clone().close(state.messages_processed, lifetime_ms);
                            Ok(())
                        }
                    }
                    """
            },
            {
                name:     "Test Log Capture"
                use_when: "Writing tests that assert on log output"
                example:  """
                    #[cfg(test)]
                    mod tests {
                        use std::sync::{Arc, Mutex};
                        use tracing_subscriber::fmt::MakeWriter;

                        #[derive(Clone, Default)]
                        struct TestWriter {
                            buffer: Arc<Mutex<Vec<u8>>>,
                        }

                        impl TestWriter {
                            fn new() -> Self {
                                Self::default()
                            }

                            fn into_string(self) -> String {
                                let buffer = self.buffer.lock().unwrap_or_else(|e| e.into_inner());
                                String::from_utf8_lossy(&buffer).to_string()
                            }
                        }

                        impl<'a> MakeWriter<'a> for TestWriter {
                            type Writer = TestWriterGuard;

                            fn make_writer(&'a self) -> Self::Writer {
                                TestWriterGuard {
                                    buffer: Arc::clone(&self.buffer),
                                }
                            }
                        }

                        struct TestWriterGuard {
                            buffer: Arc<Mutex<Vec<u8>>>,
                        }

                        impl std::io::Write for TestWriterGuard {
                            fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                                let mut buffer = self.buffer.lock().unwrap_or_else(|e| e.into_inner());
                                buffer.extend_from_slice(buf);
                                Ok(buf.len())
                            }

                            fn flush(&mut self) -> std::io::Result<()> {
                                Ok(())
                            }
                        }

                        fn capture_logs() -> (TestWriter, impl Drop) {
                            let writer = TestWriter::new();
                            let subscriber = tracing_subscriber::fmt()
                                .json()
                                .with_writer(writer.clone())
                                .with_max_level(tracing::Level::TRACE)
                                .finish();

                            let guard = tracing::subscriber::set_default(subscriber);
                            (writer, guard)
                        }

                        #[tokio::test]
                        async fn test_actor_span_logged() {
                            let (writer, _guard) = capture_logs();

                            // ... test actor operations ...

                            let logs = writer.into_string();
                            assert!(logs.contains("actor.spawned"));
                            assert!(logs.contains("\"actor.id\":\"test-worker\""));
                        }
                    }
                    """
            },
        ]
    }
}
