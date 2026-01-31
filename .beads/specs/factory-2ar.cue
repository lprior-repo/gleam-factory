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

"factory-2ar": #ValidBead & {
    // ============================================================================
    // BEAD: factory-2ar - Add Comprehensive Tests
    // ============================================================================

    id:              "factory-2ar"
    title:           "Add comprehensive tests - currently 85% of logic untested"
    type:            "task"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["testing", "quality", "proptest", "coverage", "P2"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL achieve minimum 80% code coverage across all modules",
            "THE SYSTEM SHALL use proptest for property-based testing of all domain types",
            "THE SYSTEM SHALL include golden master tests for all I/O operations",
            "THE SYSTEM SHALL test all error paths with explicit assertions",
            "THE SYSTEM SHALL never use unwrap() or expect() in test code - use assert! patterns",
        ]

        event_driven: [
            {
                trigger: "WHEN a Slug is constructed with arbitrary input"
                shall:   "THE SYSTEM SHALL validate and accept/reject deterministically per spec"
            },
            {
                trigger: "WHEN persistence save_task_record is called"
                shall:   "THE SYSTEM SHALL write valid JSON that can be loaded back identically"
            },
            {
                trigger: "WHEN worktree create_worktree is called"
                shall:   "THE SYSTEM SHALL create isolated workspace or return descriptive error"
            },
            {
                trigger: "WHEN stage execution fails"
                shall:   "THE SYSTEM SHALL return Error with language, stage, and reason fields"
            },
            {
                trigger: "WHEN audit log_event is called"
                shall:   "THE SYSTEM SHALL append JSONL entry that can be parsed back"
            },
        ]

        state_driven: [
            {
                state: "WHILE tests are running"
                shall: "THE SYSTEM SHALL use tempdir for all file operations (no real filesystem pollution)"
            },
            {
                state: "WHILE property tests execute"
                shall: "THE SYSTEM SHALL shrink failing cases to minimal reproducible examples"
            },
        ]

        unwanted: [
            {
                condition: "IF a test modifies global state"
                shall_not: "THE SYSTEM SHALL NOT allow tests to interfere with each other"
                because:   "Flaky tests waste developer time and erode trust in CI"
            },
            {
                condition: "IF a test requires network or external services"
                shall_not: "THE SYSTEM SHALL NOT make real network calls in unit tests"
                because:   "External dependencies make tests slow, flaky, and non-deterministic"
            },
            {
                condition: "IF coverage drops below 80%"
                shall_not: "THE SYSTEM SHALL NOT allow merging without explicit coverage waiver"
                because:   "Untested code is liability; regressions hide in dark corners"
            },
        ]

        complex: [
            {
                state:   "WHILE running integration tests"
                trigger: "WHEN git/jj commands would be executed"
                shall:   "THE SYSTEM SHALL use mock process execution or isolated test repos"
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
                    field:           "proptest dependency"
                    type:            "Cargo.toml entry"
                    constraints:     "proptest = \"1.4\" in dev-dependencies"
                    example_valid:   "[dev-dependencies]\nproptest = \"1.4\""
                    example_invalid: "proptest in regular dependencies"
                },
                {
                    field:           "test-case dependency"
                    type:            "Cargo.toml entry"
                    constraints:     "test-case for parameterized tests"
                    example_valid:   "test-case = \"3.3\""
                    example_invalid: "missing test-case"
                },
            ]
            system_state: [
                "Dependencies factory-ac1, factory-duf, factory-goi, factory-4pz completed",
                "Moon build system configured",
                "factory-core crate compiles without errors",
                "tempfile crate available for test isolation",
            ]
        }

        postconditions: {
            state_changes: [
                "Cargo.toml updated with proptest, test-case dev-dependencies",
                "New test modules in each source file",
                "Coverage report generated showing >80%",
                "All property tests pass with 256+ cases",
            ]
            return_guarantees: [
                {
                    field:     "moon run :test"
                    guarantee: "Returns exit code 0 with all tests passing"
                },
                {
                    field:     "cargo tarpaulin"
                    guarantee: "Reports >80% line coverage"
                },
                {
                    field:     "proptest regressions"
                    guarantee: "Zero regression files in proptest-regressions/"
                },
            ]
            side_effects: [
                "Test fixtures created in tests/fixtures/ directory",
                "Golden master files created in tests/golden/",
            ]
        }

        invariants: [
            "All tests are deterministic (same input = same output)",
            "No test depends on execution order",
            "All file I/O uses tempdir or controlled fixtures",
            "Property tests use reproducible seeds",
            "No unwrap() or expect() in any test code",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "proptest version conflicts with other deps"
                prevention:  "Pin proptest = \"1.4\", test with cargo tree"
                test_for_it: "test_cargo_build_with_dev_deps"
            },
            {
                failure:     "Tests fail due to missing git repo context"
                prevention:  "Initialize temp git repos in test setup"
                test_for_it: "test_repo_detection_in_temp_dir"
            },
            {
                failure:     "Golden master tests fail on different platforms"
                prevention:  "Normalize line endings, use platform-agnostic paths"
                test_for_it: "test_golden_master_cross_platform"
            },
        ]

        usability_failures: [
            {
                failure:     "Property tests too slow for rapid iteration"
                prevention:  "Use PROPTEST_CASES=16 for dev, 256 for CI"
                test_for_it: "verify proptest config respects env var"
            },
            {
                failure:     "Test failures don't indicate root cause"
                prevention:  "Use descriptive assertion messages with context"
                test_for_it: "test_error_messages_include_context"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Persistence roundtrip loses data"
                prevention:  "Property test: task == record_to_task(task_to_record(task))"
                test_for_it: "test_task_roundtrip_property"
            },
            {
                failure:     "Audit log entries malformed after write"
                prevention:  "Golden master test: write -> read -> compare"
                test_for_it: "test_audit_log_golden_master"
            },
            {
                failure:     "Worktree paths contain invalid characters"
                prevention:  "Property test slug generation with unicode edge cases"
                test_for_it: "test_slug_property_ascii_only"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_slug_property_valid_inputs"
                given: "Arbitrary strings matching [a-z0-9_-]{1,50}"
                when:  "Slug::new() is called"
                then: [
                    "Returns Ok(Slug)",
                    "Slug.as_str() matches input",
                    "Roundtrip through serde preserves value",
                ]
                real_input: """
                    proptest! {
                        #[test]
                        fn slug_valid_roundtrip(
                            s in "[a-z0-9][a-z0-9_-]{0,49}"
                        ) {
                            let slug = Slug::new(&s)?;
                            prop_assert_eq!(slug.as_str(), s);
                            let json = serde_json::to_string(&slug)?;
                            let restored: Slug = serde_json::from_str(&json)?;
                            prop_assert_eq!(slug, restored);
                        }
                    }
                    """
                expected_output: """
                    All 256 test cases pass
                    """
            },
            {
                name:  "test_persistence_roundtrip_property"
                given: "Arbitrary valid Task objects"
                when:  "task_to_record and record_to_task called"
                then: [
                    "Task fields preserved exactly",
                    "Status enum variants preserved",
                    "Priority preserved",
                ]
                real_input: """
                    proptest! {
                        #[test]
                        fn task_roundtrip(
                            slug in "[a-z][a-z0-9_-]{0,19}",
                            lang in prop_oneof![
                                Just(Language::Rust),
                                Just(Language::Go),
                                Just(Language::Gleam),
                            ],
                        ) {
                            let task = Task::new(
                                Slug::new(&slug)?,
                                lang,
                                PathBuf::from("/tmp/test"),
                            );
                            let record = task_to_record(&task);
                            let restored = record_to_task(&record)?;
                            prop_assert_eq!(task.slug.as_str(), restored.slug.as_str());
                            prop_assert_eq!(task.language, restored.language);
                        }
                    }
                    """
                expected_output: """
                    All test cases pass, zero shrink failures
                    """
            },
            {
                name:  "test_audit_log_write_read_golden"
                given: "Known audit entry with specific fields"
                when:  "log_event writes, read_audit_log reads"
                then: [
                    "Entry fields match golden master",
                    "JSONL format preserved",
                    "Timestamp format valid ISO8601",
                ]
                real_input: """
                    #[test]
                    fn audit_golden_master() -> Result<()> {
                        let temp = tempfile::tempdir()?;
                        let repo_root = temp.path();

                        log_event(
                            repo_root,
                            AuditEventType::TaskCreated,
                            "test-slug",
                            "Task created",
                            &[("language", "rust"), ("branch", "feat/test-slug")],
                        )?;

                        let log = read_audit_log(repo_root, "test-slug")?;
                        assert_eq!(log.entries.len(), 1);
                        assert_eq!(log.entries[0].task_slug, "test-slug");
                        assert_eq!(log.entries[0].event_type, AuditEventType::TaskCreated);
                        Ok(())
                    }
                    """
                expected_output: """
                    test audit_golden_master ... ok
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_slug_property_invalid_inputs"
                given: "Strings with uppercase, spaces, or length > 50"
                when:  "Slug::new() is called"
                then: [
                    "Returns Err(Error::InvalidSlug)",
                    "Error message indicates specific violation",
                ]
                real_input: """
                    proptest! {
                        #[test]
                        fn slug_rejects_uppercase(s in "[A-Z][a-zA-Z0-9_-]{0,49}") {
                            let result = Slug::new(&s);
                            prop_assert!(result.is_err());
                        }

                        #[test]
                        fn slug_rejects_too_long(s in "[a-z0-9_-]{51,100}") {
                            let result = Slug::new(&s);
                            prop_assert!(result.is_err());
                        }

                        #[test]
                        fn slug_rejects_spaces(s in "[a-z]+ +[a-z]+") {
                            let result = Slug::new(&s);
                            prop_assert!(result.is_err());
                        }
                    }
                    """
                expected_output: null
                expected_error: """
                    Error::InvalidSlug with descriptive reason
                    """
            },
            {
                name:  "test_persistence_load_missing_task"
                given: "Empty or non-existent tasks.json"
                when:  "load_task_record called with unknown slug"
                then: [
                    "Returns Err(Error::TaskNotFound)",
                    "Does not panic",
                ]
                real_input: """
                    #[test]
                    fn load_missing_task() -> Result<()> {
                        let temp = tempfile::tempdir()?;
                        let result = load_task_record("nonexistent", temp.path());
                        assert!(matches!(result, Err(Error::TaskNotFound { .. })));
                        Ok(())
                    }
                    """
                expected_output: null
                expected_error: """
                    Err(Error::TaskNotFound { slug: "nonexistent" })
                    """
            },
            {
                name:  "test_worktree_create_duplicate"
                given: "Worktree already exists for slug"
                when:  "create_worktree called with same slug"
                then: [
                    "Returns Err(Error::WorktreeExists)",
                    "Does not corrupt existing worktree",
                ]
                real_input: """
                    #[test]
                    fn worktree_duplicate_rejected() -> Result<()> {
                        // This test requires mock or integration setup
                        // See integration_tests module
                        Ok(())
                    }
                    """
                expected_output: null
                expected_error: """
                    Err(Error::WorktreeExists { slug: "..." })
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_slug_boundary_lengths"
                scenario: "Slugs at exactly 1 char and 50 chars"
                input:    "\"a\" and \"a\".repeat(50)"
                expected: "Both accepted, 51 chars rejected"
            },
            {
                name:     "test_empty_tasks_json"
                scenario: "tasks.json exists but is empty"
                input:    "Empty file at .factory/tasks.json"
                expected: "list_all_tasks returns empty Vec, no error"
            },
            {
                name:     "test_malformed_json_recovery"
                scenario: "tasks.json contains invalid JSON"
                input:    "Truncated or corrupted JSON"
                expected: "Error::JsonParseFailed with reason"
            },
            {
                name:     "test_concurrent_audit_writes"
                scenario: "Multiple log_event calls in rapid succession"
                input:    "10 concurrent writes to same log"
                expected: "All entries present, no data loss"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap_in_tests"
                verifies: "Test code itself uses safe patterns"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src --include='*.rs' | grep -v '#\\[cfg(test)\\]' returns empty"
            },
            {
                name:     "test_coverage_threshold"
                verifies: "80% minimum coverage"
                test:     "cargo tarpaulin --out Xml && coverage > 80%"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_test_suite_execution"
            description: "Complete test suite runs with all property and golden tests"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/property_tests.rs"
                        content: """
                            //! Property-based tests for factory-core
                            use proptest::prelude::*;
                            use factory_core::domain::*;
                            use factory_core::persistence::*;
                            use factory_core::error::Result;

                            proptest! {
                                #[test]
                                fn slug_roundtrip(s in "[a-z][a-z0-9_-]{0,19}") {
                                    let slug = Slug::new(&s).unwrap();
                                    prop_assert_eq!(slug.as_str(), s);
                                }
                            }
                            """
                    },
                    {
                        path: "crates/factory-core/tests/golden_tests.rs"
                        content: """
                            //! Golden master tests for I/O operations
                            use factory_core::audit::*;
                            use factory_core::persistence::*;
                            use factory_core::error::Result;

                            #[test]
                            fn audit_log_format() -> Result<()> {
                                let temp = tempfile::tempdir()?;
                                // Test implementation
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
                command:    "moon run factory-core:test"
                timeout_ms: 120000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test result: ok",
                    "0 failed",
                ]
                files_created: []
            }

            cleanup: {
                commands: []
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_property_test_shrinking"
                description: "Verify proptest shrinks failing cases"
                steps: [
                    {action: "Create intentionally failing property test", verify: "Test compiles"},
                    {action: "Run test with PROPTEST_CASES=1000", verify: "Failure detected"},
                    {action: "Check shrunk output", verify: "Minimal counterexample shown"},
                ]
            },
            {
                name:        "e2e_coverage_report"
                description: "Generate and verify coverage meets threshold"
                steps: [
                    {action: "Run cargo tarpaulin", verify: "Report generated"},
                    {action: "Parse coverage percentage", verify: "Coverage >= 80%"},
                    {action: "Check uncovered lines", verify: "Only error paths uncovered"},
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
                task:      "Add proptest and test-case to dev-dependencies"
                file:      "crates/factory-core/Cargo.toml"
                what:      "proptest = \"1.4\", test-case = \"3.3\""
                done_when: "cargo check --tests succeeds"
            },
            {
                task:      "Write property tests for Slug validation"
                file:      "crates/factory-core/src/domain.rs"
                what:      "proptest! tests for valid/invalid slug patterns"
                done_when: "Tests exist and pass"
            },
            {
                task:      "Write property tests for GitHash validation"
                file:      "crates/factory-core/src/domain.rs"
                what:      "proptest! for 40-char hex strings"
                done_when: "Tests exist and pass"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add persistence roundtrip property tests"
                file: "crates/factory-core/src/persistence.rs"
                what: """
                    proptest! {
                        #[test]
                        fn task_roundtrip_preserves_data(
                            slug in "[a-z][a-z0-9_-]{0,19}",
                            lang in prop_oneof![Just(Language::Rust), Just(Language::Go)],
                            priority in prop_oneof![Just(Priority::P1), Just(Priority::P2), Just(Priority::P3)],
                        ) {
                            let temp = tempfile::tempdir()?;
                            let task = Task::new(Slug::new(&slug)?, lang, PathBuf::from("/tmp"))
                                .with_priority(priority);

                            save_task_record(&task, temp.path())?;
                            let loaded = load_task_record(&slug, temp.path())?;

                            prop_assert_eq!(task.slug.as_str(), loaded.slug.as_str());
                            prop_assert_eq!(task.language, loaded.language);
                            prop_assert_eq!(task.priority, loaded.priority);
                        }
                    }
                    """
                done_when:     "Property test passes 256 cases"
                patterns_to_use: ["tempfile for isolation", "prop_assert_eq for comparisons"]
            },
            {
                task: "Add audit log golden master tests"
                file: "crates/factory-core/src/audit.rs"
                what: """
                    #[test]
                    fn audit_entry_serialization_golden() -> Result<()> {
                        let entry = create_entry(
                            AuditEventType::TaskCreated,
                            "test-task",
                            "Task created",
                            &[("language", "rust")],
                        );

                        let json = serde_json::to_string(&entry)?;
                        assert!(json.contains("\"event_type\":\"task_created\""));
                        assert!(json.contains("\"task_slug\":\"test-task\""));
                        assert!(json.contains("\"language\":\"rust\""));
                        Ok(())
                    }
                    """
                done_when:     "Golden master test passes"
                patterns_to_use: ["serde_json for serialization checks"]
            },
            {
                task: "Add worktree tests with mocked commands"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    #[test]
                    fn workspaces_base_path_correct() {
                        let path = workspaces_base(Path::new("/repo"));
                        assert_eq!(path, PathBuf::from("/repo/.factory-workspaces"));
                    }

                    #[test]
                    fn generate_unique_id_is_numeric() {
                        let id = generate_unique_id();
                        assert!(id.chars().all(|c| c.is_ascii_digit()));
                        assert!(!id.is_empty());
                        assert!(id.len() <= 8);
                    }
                    """
                done_when:     "Worktree unit tests pass"
                patterns_to_use: ["Unit test pure functions first", "Integration test I/O later"]
            },
            {
                task: "Add stage execution error path tests"
                file: "crates/factory-core/src/stages/mod.rs"
                what: """
                    #[cfg(test)]
                    mod tests {
                        use super::*;

                        #[test]
                        fn unknown_stage_returns_error() {
                            let result = execute_stage(
                                "nonexistent",
                                Language::Rust,
                                Path::new("/tmp"),
                            );
                            assert!(matches!(result, Err(Error::UnknownStage { .. })));
                        }

                        #[test]
                        fn stage_preview_returns_valid_data() {
                            let stages = vec![
                                Stage::new("implement", "Code compiles", 5),
                            ];
                            let previews = execute_stages_dry_run(&stages, Language::Rust);
                            assert_eq!(previews.len(), 1);
                            assert_eq!(previews[0].name, "implement");
                            assert!(!previews[0].command.is_empty());
                        }
                    }
                    """
                done_when:     "Stage tests pass"
                patterns_to_use: ["matches! for error variant checking"]
            },
            {
                task: "Add process module tests"
                file: "crates/factory-core/src/process.rs"
                what: """
                    #[test]
                    fn command_result_success_check() {
                        let result = CommandResult {
                            stdout: "output".to_string(),
                            stderr: String::new(),
                            exit_code: 0,
                        };
                        assert!(result.is_success());
                        assert!(result.check_success().is_ok());
                    }

                    #[test]
                    fn command_result_failure_check() {
                        let result = CommandResult {
                            stdout: String::new(),
                            stderr: "error".to_string(),
                            exit_code: 1,
                        };
                        assert!(!result.is_success());
                        assert!(result.check_success().is_err());
                    }

                    #[test]
                    fn file_operations_with_tempdir() -> Result<()> {
                        let temp = tempfile::tempdir()?;
                        let file_path = temp.path().join("test.txt");

                        write_text_file(&file_path, "hello")?;
                        let content = read_text_file(&file_path)?;
                        assert_eq!(content, "hello");
                        Ok(())
                    }
                    """
                done_when:     "Process tests pass"
                patterns_to_use: ["tempfile for file operation tests"]
            },
            {
                task: "Add repo module tests"
                file: "crates/factory-core/src/repo.rs"
                what: """
                    #[test]
                    fn language_detection_priority() {
                        // Test detection priority: gleam > go > rust > python > js
                        let temp = tempfile::tempdir().unwrap();

                        // Create gleam.toml
                        std::fs::write(temp.path().join("gleam.toml"), "").unwrap();
                        std::fs::write(temp.path().join("Cargo.toml"), "").unwrap();

                        let lang = detect_language(temp.path()).unwrap();
                        assert_eq!(lang, Language::Gleam);  // Gleam takes priority
                    }
                    """
                done_when:     "Repo detection tests pass"
                patterns_to_use: ["tempdir for mock repo structures"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Create integration test module"
                file:      "crates/factory-core/tests/integration.rs"
                what:      "Full workflow tests with temp git repos"
                done_when: "Integration tests pass with real git commands"
            },
            {
                task:      "Add coverage CI step"
                file:      "moon.yml or CI config"
                what:      "Run cargo tarpaulin, fail if < 80%"
                done_when: "CI enforces coverage threshold"
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
                task:     "Verify coverage threshold"
                done_when: "Coverage >= 80%"
                commands: ["cargo tarpaulin --out Xml --skip-clean"]
                expected: "Line coverage: 80%+"
            },
            {
                task:     "Verify property test count"
                done_when: "Property tests run 256+ cases each"
                commands: ["PROPTEST_CASES=256 cargo test"]
                expected: "All proptest! tests run 256 cases"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "proptest! macro not found"
                likely_cause: "Missing proptest in dev-dependencies or import"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "proptest = \"1.4\" in [dev-dependencies]"
                    },
                    {
                        file:          "test file"
                        what_to_check: "use proptest::prelude::*;"
                    },
                ]
                fix_pattern: "Add proptest dependency and import prelude"
            },
            {
                symptom:      "Property test runs forever"
                likely_cause: "Regex pattern generates huge strings"
                where_to_look: [
                    {
                        file:          "proptest! block"
                        function:      "in clause"
                        what_to_check: "Is regex bounded? Use {0,N} not +"
                    },
                ]
                fix_pattern: "Bound regex: \"[a-z]{1,50}\" not \"[a-z]+\""
            },
            {
                symptom:      "tempdir cleaned up before test finishes"
                likely_cause: "tempdir dropped early due to scope"
                where_to_look: [
                    {
                        file:          "test function"
                        what_to_check: "Is tempdir binding alive for duration of test?"
                    },
                ]
                fix_pattern: "Store tempdir in named binding, not inline"
            },
            {
                symptom:      "Coverage much lower than expected"
                likely_cause: "Tests exist but don't exercise code paths"
                where_to_look: [
                    {
                        file:          "tarpaulin output"
                        what_to_check: "Which lines are uncovered?"
                    },
                ]
                fix_pattern: "Add tests for uncovered error paths and branches"
            },
            {
                symptom:      "Test passes locally, fails in CI"
                likely_cause: "Time-dependent or environment-dependent logic"
                where_to_look: [
                    {
                        file:          "failing test"
                        what_to_check: "Does test use timestamps, random values, or env vars?"
                    },
                ]
                fix_pattern: "Mock time, use deterministic values, explicit env setup"
            },
        ]

        debugging_commands: [
            {
                scenario: "When proptest fails with shrunk value"
                run:      "PROPTEST_VERBOSE=1 cargo test"
                look_for: "Shrunk counterexample, original failing input"
            },
            {
                scenario: "When coverage is low"
                run:      "cargo tarpaulin --out Html && open tarpaulin-report.html"
                look_for: "Red lines indicating uncovered code"
            },
            {
                scenario: "When golden master fails"
                run:      "cargo test -- --nocapture 2>&1 | diff - expected_output"
                look_for: "Actual vs expected output differences"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] Property tests for Slug::new() - valid and invalid inputs",
            "[ ] Property tests for GitHash::new() - valid and invalid inputs",
            "[ ] Property tests for Language::parse() - all variants",
            "[ ] Property tests for Priority::parse() - all variants",
            "[ ] Property tests for TaskStatus serialization roundtrip",
            "[ ] Property tests for persistence task roundtrip",
            "[ ] Golden master tests for audit log JSONL format",
            "[ ] Golden master tests for tasks.json format",
            "[ ] Error path tests for all Error variants",
            "[ ] Unit tests for worktree path generation",
            "[ ] Unit tests for stage execution routing",
            "[ ] Unit tests for process command helpers",
            "[ ] Integration tests for repo detection",
        ]

        code: [
            "[ ] proptest = \"1.4\" in dev-dependencies",
            "[ ] test-case = \"3.3\" in dev-dependencies",
            "[ ] All tests use tempfile for isolation",
            "[ ] No unwrap() or expect() in test code",
            "[ ] All property tests bounded (no unbounded regex)",
            "[ ] Tests are deterministic (no time/random deps)",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] moon run :test passes",
            "[ ] cargo tarpaulin reports >= 80% coverage",
            "[ ] No clippy warnings",
            "[ ] No proptest regressions",
        ]

        documentation: [
            "[ ] Test module docs explain testing strategy",
            "[ ] Property test docs explain invariants being tested",
            "[ ] Golden master docs explain expected formats",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Primary target - Slug, GitHash, Language, Priority, TaskStatus, Stage"
            },
            {
                path:      "crates/factory-core/src/persistence.rs"
                relevance: "Needs roundtrip tests for save/load operations"
            },
            {
                path:      "crates/factory-core/src/worktree.rs"
                relevance: "Needs tests for path generation, currently 2 tests only"
            },
            {
                path:      "crates/factory-core/src/audit.rs"
                relevance: "Needs golden master tests for JSONL format"
            },
            {
                path:      "crates/factory-core/src/process.rs"
                relevance: "Needs tests for command execution helpers"
            },
            {
                path:      "crates/factory-core/src/repo.rs"
                relevance: "Needs tests for repo detection logic"
            },
            {
                path:      "crates/factory-core/src/stages/mod.rs"
                relevance: "Needs tests for stage routing and execution"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error constructors should be tested"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Add proptest, test-case to dev-dependencies"
            },
        ]

        external_references: [
            "https://proptest-rs.github.io/proptest/intro.html - proptest documentation",
            "https://docs.rs/proptest/latest/proptest/ - API reference",
            "https://docs.rs/test-case/latest/test_case/ - test-case macros",
            "https://blog.logrocket.com/property-based-testing-in-rust-with-proptest/ - Tutorial",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "All tests return Result<()>, use ? operator"
            },
            {
                pattern:          "Property-Based Testing"
                example_location: "To be created in this bead"
                how_to_apply:     "proptest! macro with bounded generators"
            },
            {
                pattern:          "Golden Master Testing"
                example_location: "To be created in this bead"
                how_to_apply:     "Serialize, compare with expected, fail on diff"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use proptest 1.4 - stable and well-documented",
            "Bound all regex generators: \"[a-z]{1,50}\" not \"[a-z]+\"",
            "Use tempfile::tempdir() for all file I/O tests",
            "Return Result<()> from tests, use ? operator",
            "Use prop_assert! and prop_assert_eq! in property tests",
            "Name tests descriptively: test_<module>_<behavior>_<condition>",
            "Use test-case for parameterized unit tests",
            "Mock external commands where possible, integration test otherwise",
        ]

        do_not: [
            "Do NOT use unwrap() or expect() even in tests",
            "Do NOT use panic!, todo!, or unimplemented! in tests",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT use unbounded regex in proptest generators",
            "Do NOT rely on test execution order",
            "Do NOT make real network calls",
            "Do NOT use system temp directories directly (use tempfile crate)",
        ]

        code_patterns: [
            {
                name:     "Property Test with Bounded Generator"
                use_when: "Testing validation functions"
                example:  """
                    use proptest::prelude::*;

                    proptest! {
                        #[test]
                        fn slug_valid_accepted(s in "[a-z][a-z0-9_-]{0,49}") {
                            let result = Slug::new(&s);
                            prop_assert!(result.is_ok());
                            prop_assert_eq!(result.as_ref().map(|s| s.as_str()), Ok(s.as_str()));
                        }
                    }
                    """
            },
            {
                name:     "Property Test with Custom Strategy"
                use_when: "Testing with complex types"
                example:  """
                    fn arb_language() -> impl Strategy<Value = Language> {
                        prop_oneof![
                            Just(Language::Rust),
                            Just(Language::Go),
                            Just(Language::Gleam),
                            Just(Language::Python),
                            Just(Language::Javascript),
                        ]
                    }

                    proptest! {
                        #[test]
                        fn language_roundtrip(lang in arb_language()) {
                            let s = lang.as_str();
                            let parsed = Language::parse(s);
                            prop_assert_eq!(parsed.ok(), Some(lang));
                        }
                    }
                    """
            },
            {
                name:     "Golden Master Test"
                use_when: "Testing serialization formats"
                example:  """
                    #[test]
                    fn task_record_json_format() -> Result<()> {
                        let task = Task::new(
                            Slug::new("test-task")?,
                            Language::Rust,
                            PathBuf::from("/tmp/test"),
                        );
                        let record = task_to_record(&task);
                        let json = serde_json::to_string_pretty(&record)?;

                        // Golden assertions
                        assert!(json.contains("\"slug\": \"test-task\""));
                        assert!(json.contains("\"language\": \"rust\""));
                        assert!(json.contains("\"status\": \"created\""));
                        Ok(())
                    }
                    """
            },
            {
                name:     "Tempdir Isolated Test"
                use_when: "Testing file I/O"
                example:  """
                    #[test]
                    fn persistence_save_load() -> Result<()> {
                        let temp = tempfile::tempdir()?;
                        let repo_root = temp.path();

                        let task = Task::new(
                            Slug::new("my-task")?,
                            Language::Rust,
                            PathBuf::from("/tmp"),
                        );

                        save_task_record(&task, repo_root)?;
                        let loaded = load_task_record("my-task", repo_root)?;

                        assert_eq!(task.slug.as_str(), loaded.slug.as_str());
                        Ok(())
                    }
                    """
            },
            {
                name:     "Error Path Test"
                use_when: "Testing error conditions"
                example:  """
                    #[test]
                    fn slug_empty_rejected() {
                        let result = Slug::new("");
                        assert!(matches!(
                            result,
                            Err(Error::InvalidSlug { reason }) if reason.contains("empty")
                        ));
                    }

                    #[test]
                    fn slug_uppercase_rejected() {
                        let result = Slug::new("MyTask");
                        assert!(matches!(
                            result,
                            Err(Error::InvalidSlug { reason }) if reason.contains("invalid characters")
                        ));
                    }
                    """
            },
        ]
    }
}
