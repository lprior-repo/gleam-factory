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

"factory-j5q": #ValidBead & {
    // ============================================================================
    // BEAD: factory-j5q - Fix silent P2 default on malformed priority
    // ============================================================================

    id:              "factory-j5q"
    title:           "Fix silent P2 default on malformed priority in persistence.rs:155"
    type:            "bug"
    priority:        2
    effort_estimate: "30min"
    labels:          ["bug", "data-integrity", "error-handling", "persistence"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL propagate all parsing errors via Result<T, Error> types",
            "THE SYSTEM SHALL never silently coerce invalid data to default values",
            "THE SYSTEM SHALL surface data corruption to callers for explicit handling",
            "THE SYSTEM SHALL never use unwrap_or_default() on parse operations that can fail",
        ]

        event_driven: [
            {
                trigger: "WHEN a task record with malformed priority is loaded"
                shall:   "THE SYSTEM SHALL return Err(Error::InvalidPriority) with the malformed value"
            },
            {
                trigger: "WHEN Priority::parse returns Err"
                shall:   "THE SYSTEM SHALL propagate the error to record_to_task caller"
            },
            {
                trigger: "WHEN loading legacy records without priority field"
                shall:   "THE SYSTEM SHALL use the serde default (P2) which is intentional behavior"
            },
            {
                trigger: "WHEN an invalid priority string is encountered"
                shall:   "THE SYSTEM SHALL include the invalid value in the error message"
            },
        ]

        state_driven: [
            {
                state: "WHILE parsing a TaskRecord"
                shall: "THE SYSTEM SHALL validate all fields before constructing a Task"
            },
            {
                state: "WHILE loading tasks from .factory/tasks.json"
                shall: "THE SYSTEM SHALL fail fast on data corruption rather than silently degrade"
            },
        ]

        unwanted: [
            {
                condition: "IF priority string is malformed (not P1, P2, or P3)"
                shall_not: "THE SYSTEM SHALL NOT silently default to P2"
                because:   "Silent defaults mask data corruption, making bugs invisible until they cause downstream failures"
            },
            {
                condition: "IF record_to_task encounters parse errors"
                shall_not: "THE SYSTEM SHALL NOT use unwrap_or_default() to hide failures"
                because:   "Hiding parse failures violates Railway-Oriented Programming principles"
            },
            {
                condition: "IF .factory/tasks.json contains corrupted data"
                shall_not: "THE SYSTEM SHALL NOT partially load data with invalid entries"
                because:   "Partial loads cause inconsistent state and hard-to-debug issues"
            },
        ]

        complex: [
            {
                state:   "WHILE list_all_tasks iterates over records"
                trigger: "WHEN any record fails to parse"
                shall:   "THE SYSTEM SHALL fail the entire operation with the first error encountered"
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
                    field:           "record.priority"
                    type:            "String"
                    constraints:     "Must be 'P1', 'P2', or 'P3' (case-insensitive)"
                    example_valid:   "\"P1\", \"p2\", \"P3\""
                    example_invalid: "\"P4\", \"high\", \"\", \"priority-1\""
                },
            ]
            system_state: [
                "persistence.rs exists with record_to_task function",
                "Priority::parse returns Result<Priority, Error>",
                "Error::InvalidPriority variant exists in error.rs",
            ]
        }

        postconditions: {
            state_changes: [
                "record_to_task now uses ? operator instead of unwrap_or_default()",
                "Invalid priority strings cause record_to_task to return Err",
                "All callers of record_to_task handle the new error case",
            ]
            return_guarantees: [
                {
                    field:     "record_to_task()"
                    guarantee: "Returns Err(Error::InvalidPriority) for malformed priority"
                },
                {
                    field:     "load_task_record()"
                    guarantee: "Propagates InvalidPriority error to caller"
                },
                {
                    field:     "list_all_tasks()"
                    guarantee: "Fails with first InvalidPriority error encountered"
                },
            ]
            side_effects: [
                "Existing corrupted tasks.json files will now cause load failures",
            ]
        }

        invariants: [
            "All parsed Task objects have valid Priority values",
            "No unwrap_or_default() on fallible parse operations",
            "All errors from Priority::parse are propagated",
            "Error messages include the invalid input value for debugging",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Existing tasks.json files with corrupted priority fail to load"
                prevention:  "Migration script or manual fix for corrupted data"
                test_for_it: "test_corrupted_priority_returns_error"
            },
            {
                failure:     "list_all_tasks callers don't handle new error case"
                prevention:  "Review all callers, update error handling"
                test_for_it: "test_list_all_tasks_propagates_priority_error"
            },
            {
                failure:     "CLI shows cryptic error for corrupted data"
                prevention:  "Error message includes slug and invalid value"
                test_for_it: "test_error_message_is_actionable"
            },
        ]

        usability_failures: [
            {
                failure:     "User doesn't understand why task load failed"
                prevention:  "Error message format: 'invalid priority \"XYZ\" in task \"slug\"'"
                test_for_it: "test_error_message_includes_context"
            },
            {
                failure:     "No way to recover from corrupted tasks.json"
                prevention:  "Document manual edit procedure, provide validate command"
                test_for_it: "test_manual_fix_procedure"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Corrupted priority silently becomes P2"
                prevention:  "Remove unwrap_or_default(), use ? operator"
                test_for_it: "test_no_silent_default_on_malformed_priority"
            },
            {
                failure:     "Tasks with wrong priority get wrong scheduling"
                prevention:  "Strict validation surfaces corruption immediately"
                test_for_it: "test_priority_round_trip_preserves_value"
            },
            {
                failure:     "Partial load leaves system in inconsistent state"
                prevention:  "All-or-nothing loading via collect::<Result<Vec<_>>>()"
                test_for_it: "test_list_fails_on_any_invalid_record"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_valid_priority_parses_correctly"
                given: "A TaskRecord with valid priority 'P1'"
                when:  "record_to_task is called"
                then: [
                    "Returns Ok(Task)",
                    "Task.priority equals Priority::P1",
                ]
                real_input: """
                    let record = TaskRecord {
                        slug: "test-task".to_string(),
                        language: "rust".to_string(),
                        status: "created".to_string(),
                        priority: "P1".to_string(),
                        created_at: "2024-01-01T00:00:00Z".to_string(),
                        updated_at: "2024-01-01T00:00:00Z".to_string(),
                        stages: vec![],
                        worktree_path: "/tmp/test".to_string(),
                        current_stage: String::new(),
                        current_error: String::new(),
                    };
                    """
                expected_output: """
                    let task = record_to_task(&record)?;
                    assert_eq!(task.priority, Priority::P1);
                    """
            },
            {
                name:  "test_case_insensitive_priority"
                given: "A TaskRecord with lowercase priority 'p3'"
                when:  "record_to_task is called"
                then: [
                    "Returns Ok(Task)",
                    "Task.priority equals Priority::P3",
                ]
                real_input: """
                    let record = TaskRecord {
                        priority: "p3".to_string(),
                        // ... other fields ...
                    };
                    """
                expected_output: """
                    let task = record_to_task(&record)?;
                    assert_eq!(task.priority, Priority::P3);
                    """
            },
            {
                name:  "test_default_priority_for_missing_field"
                given: "A TaskRecord deserialized without priority field (legacy)"
                when:  "serde default is applied"
                then: [
                    "priority field defaults to 'P2'",
                    "This is intentional for backwards compatibility",
                ]
                real_input: """
                    let json = r#"{"slug":"test","language":"rust","status":"created"}"#;
                    let record: TaskRecord = serde_json::from_str(json)?;
                    """
                expected_output: """
                    assert_eq!(record.priority, "P2");
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_invalid_priority_returns_error"
                given: "A TaskRecord with invalid priority 'P4'"
                when:  "record_to_task is called"
                then: [
                    "Returns Err(Error::InvalidPriority)",
                    "Error contains the invalid value 'P4'",
                ]
                real_input: """
                    let record = TaskRecord {
                        priority: "P4".to_string(),
                        // ... other fields ...
                    };
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InvalidPriority { value: "P4".to_string() })
                    """
            },
            {
                name:  "test_malformed_priority_string_returns_error"
                given: "A TaskRecord with priority 'high'"
                when:  "record_to_task is called"
                then: [
                    "Returns Err(Error::InvalidPriority)",
                    "Error message is human-readable",
                ]
                real_input: """
                    let record = TaskRecord {
                        priority: "high".to_string(),
                        // ... other fields ...
                    };
                    let result = record_to_task(&record);
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::InvalidPriority { value }) if value == "HIGH"));
                    """
            },
            {
                name:  "test_empty_priority_returns_error"
                given: "A TaskRecord with empty priority string"
                when:  "record_to_task is called"
                then: [
                    "Returns Err(Error::InvalidPriority)",
                    "Does not silently default to P2",
                ]
                real_input: """
                    let record = TaskRecord {
                        priority: "".to_string(),
                        // ... other fields ...
                    };
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InvalidPriority { value: "".to_string() })
                    """
            },
            {
                name:  "test_list_all_tasks_fails_on_invalid_priority"
                given: "tasks.json contains one task with invalid priority"
                when:  "list_all_tasks is called"
                then: [
                    "Returns Err with InvalidPriority",
                    "Does not return partial results",
                ]
                real_input: """
                    // tasks.json contains:
                    // [{"slug":"good","priority":"P1",...},{"slug":"bad","priority":"invalid",...}]
                    let result = list_all_tasks(&repo_root);
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::InvalidPriority { .. })));
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_priority_with_whitespace"
                scenario: "Priority string has leading/trailing whitespace"
                input:    "priority = \" P1 \""
                expected: "Should fail - whitespace not trimmed, returns InvalidPriority"
            },
            {
                name:     "test_priority_with_numbers_only"
                scenario: "Priority string is just '1' instead of 'P1'"
                input:    "priority = \"1\""
                expected: "Should fail - returns InvalidPriority"
            },
            {
                name:     "test_unicode_priority"
                scenario: "Priority contains unicode characters"
                input:    "priority = \"P\u00B9\" (P with superscript 1)"
                expected: "Should fail - returns InvalidPriority"
            },
        ]

        contract_tests: [
            {
                name:     "test_no_unwrap_or_default_on_priority"
                verifies: "No unwrap_or_default() on Priority::parse"
                test:     "grep -n 'unwrap_or_default' persistence.rs returns empty"
            },
            {
                name:     "test_error_propagation_uses_question_mark"
                verifies: "Priority::parse uses ? operator"
                test:     "Code review: line 155 uses ? not unwrap_or_default"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_corrupted_tasks_json_fails_gracefully"
            description: "Load tasks.json with corrupted priority, verify error"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/priority_validation_e2e.rs"
                        content: """
                            use factory_core::persistence::{record_to_task, TaskRecord};
                            use factory_core::error::Error;

                            #[test]
                            fn test_malformed_priority_returns_error() {
                                let record = TaskRecord {
                                    slug: "test-task".to_string(),
                                    language: "rust".to_string(),
                                    status: "created".to_string(),
                                    priority: "INVALID".to_string(),
                                    created_at: "2024-01-01T00:00:00Z".to_string(),
                                    updated_at: "2024-01-01T00:00:00Z".to_string(),
                                    stages: vec![],
                                    worktree_path: "/tmp/test".to_string(),
                                    current_stage: String::new(),
                                    current_error: String::new(),
                                };

                                let result = record_to_task(&record);
                                assert!(matches!(result, Err(Error::InvalidPriority { .. })));
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "moon run factory-core:build",
                ]
            }

            execute: {
                command:    "moon run factory-core:test -- --test priority_validation_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_malformed_priority_returns_error ... ok",
                ]
                files_created: []
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/priority_validation_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_cli_shows_error_for_corrupted_task"
                description: "CLI shows helpful error when loading corrupted task"
                steps: [
                    {action: "Create tasks.json with invalid priority", verify: "File exists"},
                    {action: "Run factory list", verify: "Returns non-zero exit code"},
                    {action: "Check stderr", verify: "Contains 'invalid priority' message"},
                    {action: "Check stderr", verify: "Contains the invalid value"},
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
                task:      "Write test: test_invalid_priority_returns_error"
                file:      "crates/factory-core/src/persistence.rs"
                what:      "Test that malformed priority causes record_to_task to fail"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_valid_priority_round_trip"
                file:      "crates/factory-core/src/persistence.rs"
                what:      "Test that valid priorities P1/P2/P3 parse correctly"
                done_when: "Test exists and PASSES (sanity check)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Remove unwrap_or_default() on line 155"
                file: "crates/factory-core/src/persistence.rs"
                what: """
                    Change:
                      let priority = Priority::parse(&record.priority).unwrap_or_default();
                    To:
                      let priority = Priority::parse(&record.priority)?;
                    """
                done_when:     "No unwrap_or_default on Priority::parse"
                patterns_to_use: [
                    "? operator for error propagation",
                    "Railway-Oriented Programming",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Verify all callers handle new error"
                file:      "crates/factory-core/src/persistence.rs"
                what:      "load_task_record and list_all_tasks already use ? and collect"
                done_when: "No changes needed - error propagates automatically"
            },
            {
                task:      "Verify CLI error handling"
                file:      "crates/factory/src/main.rs"
                what:      "CLI should display InvalidPriority errors nicely"
                done_when: "Error message is user-friendly"
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
                task:     "Verify no unwrap_or_default on parse operations"
                done_when: "grep finds no unwrap_or_default in persistence.rs"
                commands: ["rg 'unwrap_or_default' crates/factory-core/src/persistence.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual test with corrupted tasks.json"
                done_when: "factory list shows clear error message"
                commands: [
                    "echo '[{\"slug\":\"test\",\"language\":\"rust\",\"status\":\"created\",\"priority\":\"INVALID\"}]' > /tmp/test-factory/tasks.json",
                    "factory list",
                ]
                expected: "Error message mentioning 'invalid priority'"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Test fails: 'expected Err, got Ok'"
                likely_cause: "unwrap_or_default() still in place"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/persistence.rs"
                        function:      "record_to_task"
                        what_to_check: "Line 155: is it using ? or unwrap_or_default()?"
                    },
                ]
                fix_pattern: "Replace .unwrap_or_default() with ?"
            },
            {
                symptom:      "Compilation error: 'the ? operator can only be used in a function that returns Result'"
                likely_cause: "record_to_task return type changed or missing"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/persistence.rs"
                        function:      "record_to_task"
                        what_to_check: "Return type is Result<Task>"
                    },
                ]
                fix_pattern: "Ensure function signature is: pub fn record_to_task(record: &TaskRecord) -> Result<Task>"
            },
            {
                symptom:      "Existing tests fail after change"
                likely_cause: "Test creates record with invalid priority"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/persistence.rs"
                        function:      "test_task_record_roundtrip"
                        what_to_check: "Does test record have valid priority?"
                    },
                ]
                fix_pattern: "Update test to use valid priority (P1, P2, or P3)"
            },
        ]

        debugging_commands: [
            {
                scenario: "When test fails unexpectedly"
                run:      "cargo test --package factory-core -- --nocapture 2>&1 | head -100"
                look_for: "Error variant, panic message, assertion failure"
            },
            {
                scenario: "When checking for unwrap_or_default usage"
                run:      "rg 'unwrap_or_default' crates/factory-core/src/"
                look_for: "Any remaining usages of unwrap_or_default"
            },
            {
                scenario: "When verifying error message format"
                run:      "cargo test test_invalid_priority -- --nocapture"
                look_for: "Error message includes invalid value"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_invalid_priority_returns_error passes",
            "[ ] test_valid_priority_round_trip passes",
            "[ ] test_malformed_priority_string_returns_error passes",
            "[ ] test_list_all_tasks_fails_on_invalid_priority passes",
            "[ ] Existing test_task_record_roundtrip still passes",
            "[ ] No mocks - all tests use real TaskRecord structs",
        ]

        code: [
            "[ ] Line 155: unwrap_or_default() replaced with ?",
            "[ ] No other unwrap_or_default() on parse operations",
            "[ ] Error propagation uses ? operator throughout",
            "[ ] Error::InvalidPriority includes the invalid value",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
        ]

        documentation: [
            "[ ] record_to_task doc comment mentions it can fail",
            "[ ] Error variants documented with examples",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/persistence.rs"
                relevance: "Contains the bug on line 155 - unwrap_or_default()"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Contains Priority::parse which returns Result"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Contains Error::InvalidPriority variant"
            },
            {
                path:      "crates/factory/src/main.rs"
                relevance: "CLI - needs to handle new error gracefully"
            },
        ]

        external_references: [
            "https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html",
            "https://fsharpforfunandprofit.com/rop/ - Railway-Oriented Programming",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs:Language::parse"
                how_to_apply:     "Return Result<T, Error>, use ? operator, never unwrap_or_default"
            },
            {
                pattern:          "Error Context"
                example_location: "crates/factory-core/src/error.rs:InvalidPriority"
                how_to_apply:     "Include the invalid value in error for debugging"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use ? operator to propagate Priority::parse errors",
            "Keep the existing Error::InvalidPriority variant",
            "Ensure error message includes the malformed value",
            "Test with real TaskRecord structs, not mocks",
            "Verify existing tests still pass after the change",
            "Run moon run :quick frequently during development",
        ]

        do_not: [
            "Do NOT use unwrap_or_default() on any parse operation",
            "Do NOT add new default behavior for invalid priorities",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT silently convert invalid data to defaults",
            "Do NOT catch and swallow the error",
        ]

        code_patterns: [
            {
                name:     "Error Propagation with ?"
                use_when: "Replacing unwrap_or_default() with proper error handling"
                example:  """
                    // BEFORE (bug):
                    let priority = Priority::parse(&record.priority).unwrap_or_default();

                    // AFTER (fix):
                    let priority = Priority::parse(&record.priority)?;
                    """
            },
            {
                name:     "Testing Error Cases"
                use_when: "Writing tests for invalid priority handling"
                example:  """
                    #[test]
                    fn test_invalid_priority_returns_error() {
                        let record = TaskRecord {
                            slug: "test".to_string(),
                            language: "rust".to_string(),
                            status: "created".to_string(),
                            priority: "INVALID".to_string(),
                            // ... other fields
                        };

                        let result = record_to_task(&record);
                        assert!(matches!(
                            result,
                            Err(Error::InvalidPriority { value }) if value == "INVALID"
                        ));
                    }
                    """
            },
            {
                name:     "Distinguishing Serde Default vs Parse Error"
                use_when: "Understanding the difference between missing field and invalid field"
                example:  """
                    // Missing field in JSON -> serde uses #[serde(default)] -> "P2" string
                    // This is INTENTIONAL for backwards compatibility
                    #[serde(default = "default_priority")]
                    pub priority: String,

                    // Invalid value in priority field -> Priority::parse returns Err
                    // This should NOT be silently converted to P2
                    let priority = Priority::parse(&record.priority)?; // Propagate error!
                    """
            },
        ]
    }
}
