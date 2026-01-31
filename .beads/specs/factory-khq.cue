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

"factory-khq": #ValidBead & {
    // ============================================================================
    // BEAD: factory-khq - Refactor stage handlers into declarative StageTable
    // ============================================================================

    id:              "factory-khq"
    title:           "Refactor 665 lines of duplicated stage handlers into declarative table"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["refactor", "DRY", "stages", "builder-pattern", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL define stage handlers using a declarative StageTable structure",
            "THE SYSTEM SHALL eliminate duplicate match statement routers across language handlers",
            "THE SYSTEM SHALL use a builder pattern for registering language-specific commands",
            "THE SYSTEM SHALL maintain the same 9 stages: implement, unit-test, coverage, lint, static, integration, security, review, accept",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN execute_stage() is called with a language and stage name"
                shall:   "THE SYSTEM SHALL look up the stage command from the StageTable and execute it"
            },
            {
                trigger: "WHEN a new language is added"
                shall:   "THE SYSTEM SHALL require only adding entries to the StageTable, not new functions"
            },
            {
                trigger: "WHEN get_preview() is called for a stage"
                shall:   "THE SYSTEM SHALL return the command string and timeout from the StageTable entry"
            },
            {
                trigger: "WHEN a stage has custom execution logic (e.g., review's grep inversion)"
                shall:   "THE SYSTEM SHALL support custom executor functions in the StageTable entry"
            },
            {
                trigger: "WHEN the accept stage runs"
                shall:   "THE SYSTEM SHALL compose multiple stages (implement + unit-test + lint) from table references"
            },
        ]

        state_driven: [
            {
                state: "WHILE the StageTable is being built"
                shall: "THE SYSTEM SHALL validate that all 9 stages are defined for each language"
            },
            {
                state: "WHILE a stage executes"
                shall: "THE SYSTEM SHALL use the same error reporting format as before ({language}: {stage} failed - {reason})"
            },
        ]

        unwanted: [
            {
                condition: "IF a developer adds a new stage"
                shall_not: "THE SYSTEM SHALL NOT require changes to 5 language files"
                because:   "Scattered changes are error-prone and violate DRY"
            },
            {
                condition: "IF stage execution logic is identical across languages"
                shall_not: "THE SYSTEM SHALL NOT duplicate the execution code in each language module"
                because:   "665 lines of duplication is unmaintainable"
            },
            {
                condition: "IF a language lacks a tool for a stage (e.g., Python has no native audit)"
                shall_not: "THE SYSTEM SHALL NOT panic or return cryptic errors"
                because:   "Clear error messages enable debugging"
            },
        ]

        complex: [
            {
                state:   "WHILE building the StageTable"
                trigger: "WHEN a language omits a required stage"
                shall:   "THE SYSTEM SHALL return a compile-time or early-runtime error indicating the missing stage"
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
                    field:           "Language enum"
                    type:            "enum"
                    constraints:     "Must be one of: Go, Gleam, Rust, Python, Javascript"
                    example_valid:   "Language::Rust"
                    example_invalid: "Language::Haskell"
                },
                {
                    field:           "stage_name"
                    type:            "str"
                    constraints:     "Must be one of 9 valid stage names"
                    example_valid:   "\"implement\""
                    example_invalid: "\"compile\""
                },
            ]
            system_state: [
                "factory-00s (actor framework) is implemented",
                "Current 5 language handlers exist in crates/factory-core/src/stages/",
                "Error::StageFailed variant exists in error.rs",
                "process::run_command() helper is available",
            ]
        }

        postconditions: {
            state_changes: [
                "New file: crates/factory-core/src/stages/table.rs (~80 lines)",
                "Modified: crates/factory-core/src/stages/mod.rs to use StageTable",
                "Deleted: rust.rs, gleam.rs, go.rs, python.rs, javascript.rs (or reduced to minimal re-exports)",
                "Total stages/ directory reduced from ~665 lines to ~150 lines",
            ]
            return_guarantees: [
                {
                    field:     "StageTable::execute()"
                    guarantee: "Returns Result<(), Error> with same error types as before"
                },
                {
                    field:     "StageTable::preview()"
                    guarantee: "Returns (String, u64) tuple with command and timeout"
                },
                {
                    field:     "StageTableBuilder::build()"
                    guarantee: "Returns Result<StageTable, Error> validating completeness"
                },
            ]
            side_effects: [
                "All existing tests continue to pass",
                "Stage execution behavior is identical to before",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Each language defines exactly 9 stages",
            "Stage names are validated at table construction time",
            "Error messages preserve the format: {language}: {stage} failed - {reason}",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Existing tests break due to different error messages"
                prevention:  "Preserve exact error format using Error::stage_failed()"
                test_for_it: "test_error_message_format_preserved"
            },
            {
                failure:     "Stage execution order changes"
                prevention:  "Use the same standard_pipeline() order from domain.rs"
                test_for_it: "test_stage_execution_order"
            },
            {
                failure:     "Custom executors (review stage) lose their inverted logic"
                prevention:  "Support Executor::Custom(fn) in StageEntry"
                test_for_it: "test_review_stage_inverts_grep_result"
            },
        ]

        usability_failures: [
            {
                failure:     "Adding new language requires understanding complex generics"
                prevention:  "Provide StageTableBuilder with simple .stage() method"
                test_for_it: "test_builder_api_is_simple"
            },
            {
                failure:     "Table lookup is slower than direct function dispatch"
                prevention:  "Use HashMap or match expression on Language enum"
                test_for_it: "test_lookup_performance"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Language X missing stage Y not caught until runtime"
                prevention:  "Builder::build() validates all 9 stages present"
                test_for_it: "test_missing_stage_detected_at_build"
            },
            {
                failure:     "Timeout values differ from original implementation"
                prevention:  "Copy exact timeout values from existing preview functions"
                test_for_it: "test_timeout_values_match_original"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_execute_rust_implement_stage"
                given: "A StageTable with Rust language configured"
                when:  "execute_stage(\"implement\", Language::Rust, cwd) is called"
                then: [
                    "Runs 'cargo build' command",
                    "Returns Ok(()) on success",
                    "Returns Err(StageFailed) on compile error",
                ]
                real_input: """
                    let table = StageTable::new();
                    let cwd = Path::new("/tmp/rust-project");
                    let result = table.execute("implement", Language::Rust, cwd);
                    """
                expected_output: """
                    assert!(result.is_ok());
                    // Or on failure:
                    assert!(matches!(result, Err(Error::StageFailed { language, stage, .. })
                        if language == "Rust" && stage == "implement"));
                    """
            },
            {
                name:  "test_get_preview_returns_command_and_timeout"
                given: "A StageTable with all languages configured"
                when:  "preview(\"lint\", Language::Python) is called"
                then: [
                    "Returns (\"ruff check .\", 5000)",
                    "Command string matches original get_python_preview()",
                    "Timeout value matches original",
                ]
                real_input: """
                    let table = StageTable::new();
                    let (cmd, timeout) = table.preview("lint", Language::Python);
                    """
                expected_output: """
                    assert_eq!(cmd, "ruff check .");
                    assert_eq!(timeout, 5000);
                    """
            },
            {
                name:  "test_builder_creates_complete_table"
                given: "A StageTableBuilder with all stages registered"
                when:  "build() is called"
                then: [
                    "Returns Ok(StageTable)",
                    "Table contains all 9 stages for all 5 languages",
                    "Each stage is executable",
                ]
                real_input: """
                    let table = StageTableBuilder::new()
                        .language(Language::Rust)
                            .stage("implement", "cargo", &["build"], 30000)
                            .stage("unit-test", "cargo", &["test"], 60000)
                            // ... remaining 7 stages
                        .language(Language::Python)
                            // ... 9 stages
                        .build()?;
                    """
                expected_output: """
                    assert!(table.is_ok());
                    let table = table?;
                    assert!(table.has_stage("implement", Language::Rust));
                    assert!(table.has_stage("accept", Language::Python));
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_unknown_stage_returns_error"
                given: "A complete StageTable"
                when:  "execute(\"compile\", Language::Rust, cwd) is called with invalid stage"
                then: [
                    "Returns Err(Error::UnknownStage)",
                    "Error message contains stage name",
                ]
                real_input: """
                    let table = StageTable::new();
                    let result = table.execute("compile", Language::Rust, Path::new("."));
                    """
                expected_output: null
                expected_error: """
                    Err(Error::UnknownStage { name: "compile".into() })
                    """
            },
            {
                name:  "test_builder_fails_on_missing_stage"
                given: "A StageTableBuilder with Rust missing 'coverage' stage"
                when:  "build() is called"
                then: [
                    "Returns Err with descriptive message",
                    "Message indicates which language and stage is missing",
                ]
                real_input: """
                    let table = StageTableBuilder::new()
                        .language(Language::Rust)
                            .stage("implement", "cargo", &["build"], 30000)
                            .stage("unit-test", "cargo", &["test"], 60000)
                            // Missing coverage, lint, static, etc.
                        .build();
                    """
                expected_output: null
                expected_error: """
                    Err(Error::IncompleteStageTable {
                        language: "Rust",
                        missing_stages: vec!["coverage", "lint", "static", ...]
                    })
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_review_stage_inverts_grep_result"
                scenario: "Review stage should PASS when grep finds NO matches"
                input:    "Execute review stage on clean codebase"
                expected: "Returns Ok(()) when grep exits with code 1 (no matches)"
            },
            {
                name:     "test_accept_stage_composes_three_stages"
                scenario: "Accept stage runs implement + unit-test + lint"
                input:    "Execute accept stage"
                expected: "All three stages run in sequence, stops on first failure"
            },
            {
                name:     "test_go_lint_checks_stdout_not_exit_code"
                scenario: "Go's gofmt returns 0 but outputs unformatted files"
                input:    "Run lint on Go project with formatting issues"
                expected: "Returns error listing unformatted files"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap_in_table"
                verifies: "No unwrap() or expect() in table.rs"
                test:     "grep -c 'unwrap()\\|expect(' crates/factory-core/src/stages/table.rs returns 0"
            },
            {
                name:     "test_line_count_under_100"
                verifies: "table.rs is under 100 lines"
                test:     "wc -l crates/factory-core/src/stages/table.rs < 100"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_stage_table_lifecycle"
            description: "Build table, execute stages, verify behavior matches original"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/stage_table_e2e.rs"
                        content: """
                            use factory_core::stages::{execute_stage, execute_stages_dry_run};
                            use factory_core::domain::{Language, standard_pipeline};
                            use factory_core::error::Result;
                            use std::path::Path;
                            use tempfile::TempDir;

                            #[test]
                            fn test_all_languages_all_stages_have_previews() -> Result<()> {
                                let pipeline = standard_pipeline();
                                for lang in [Language::Rust, Language::Go, Language::Gleam,
                                             Language::Python, Language::Javascript] {
                                    let previews = execute_stages_dry_run(&pipeline, lang);
                                    assert_eq!(previews.len(), 9, "Expected 9 stages for {}", lang);
                                    for preview in previews {
                                        assert!(!preview.command.is_empty(),
                                            "Empty command for {} {}", lang, preview.name);
                                        assert!(preview.estimated_duration > 0,
                                            "Zero timeout for {} {}", lang, preview.name);
                                    }
                                }
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
                command:    "moon run factory-core:test -- --test stage_table_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_all_languages_all_stages_have_previews ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/stages/table.rs"
                        contains: "pub struct StageTable"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/stage_table_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_stage_execution_parity"
                description: "Verify new implementation matches old behavior exactly"
                steps: [
                    {action: "Run cargo build via old execute_rust_stage()", verify: "Success"},
                    {action: "Run cargo build via new table.execute()", verify: "Same result"},
                    {action: "Compare error messages on failure", verify: "Identical format"},
                    {action: "Compare timeout values for all 45 stage/language combinations", verify: "All match"},
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
                task:      "Write test: test_stage_table_has_all_stages"
                file:      "crates/factory-core/src/stages/table.rs"
                what:      "Test that StageTable contains all 9 stages for all 5 languages"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_builder_validates_completeness"
                file:      "crates/factory-core/src/stages/table.rs"
                what:      "Test that builder fails if stages are missing"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_execute_matches_original"
                file:      "crates/factory-core/src/stages/table.rs"
                what:      "Test that table execution produces same results as original functions"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Create StageEntry struct"
                file: "crates/factory-core/src/stages/table.rs"
                what: """
                    Define the entry that holds stage configuration:

                    /// Single stage configuration for a language.
                    #[derive(Debug, Clone)]
                    pub struct StageEntry {
                        /// Command to run (e.g., "cargo", "npm", "gleam")
                        pub command: &'static str,
                        /// Arguments to pass (e.g., ["build"], ["test", "-v"])
                        pub args: &'static [&'static str],
                        /// Timeout in milliseconds
                        pub timeout_ms: u64,
                        /// Error message on failure
                        pub failure_msg: &'static str,
                        /// Optional custom executor for special logic
                        pub executor: Option<Executor>,
                    }

                    /// Executor variants for special stage logic.
                    #[derive(Debug, Clone, Copy)]
                    pub enum Executor {
                        /// Standard: run command, check exit code
                        Standard,
                        /// Inverted: success on exit code 1 (e.g., grep finding no TODOs)
                        Inverted,
                        /// Check stdout is empty (e.g., gofmt -l)
                        StdoutEmpty,
                        /// Composite: run multiple stages in sequence
                        Composite(&'static [&'static str]),
                    }
                    """
                done_when:     "Struct compiles"
                patterns_to_use: [
                    "Static str slices for zero-copy",
                    "Option<Executor> with None defaulting to Standard",
                ]
            },
            {
                task: "Create StageTable struct"
                file: "crates/factory-core/src/stages/table.rs"
                what: """
                    Define the lookup table:

                    use std::collections::HashMap;
                    use crate::domain::Language;

                    /// Declarative table of all stage configurations.
                    pub struct StageTable {
                        entries: HashMap<(Language, &'static str), StageEntry>,
                    }

                    impl StageTable {
                        /// Create the default table with all languages configured.
                        pub fn new() -> Self { ... }

                        /// Execute a stage for a language.
                        pub fn execute(&self, stage: &str, lang: Language, cwd: &Path) -> Result<()> { ... }

                        /// Get preview (command, timeout) for a stage.
                        pub fn preview(&self, stage: &str, lang: Language) -> (String, u64) { ... }
                    }
                    """
                done_when:     "StageTable compiles and tests pass"
                patterns_to_use: [
                    "HashMap<(Language, &'static str), StageEntry>",
                    "Result<T, Error> for all fallible operations",
                ]
            },
            {
                task: "Implement stage entries for Rust"
                file: "crates/factory-core/src/stages/table.rs"
                what: """
                    Define all 9 Rust stages:

                    fn rust_stages() -> Vec<(&'static str, StageEntry)> {
                        vec![
                            ("implement", StageEntry {
                                command: "cargo",
                                args: &["build"],
                                timeout_ms: 30000,
                                failure_msg: "Code does not compile",
                                executor: None,
                            }),
                            ("unit-test", StageEntry {
                                command: "cargo",
                                args: &["test"],
                                timeout_ms: 60000,
                                failure_msg: "Tests failed",
                                executor: None,
                            }),
                            // ... 7 more stages
                            ("review", StageEntry {
                                command: "grep",
                                args: &["-r", "TODO\\|FIXME\\|XXX\\|HACK", "--include=*.rs", "."],
                                timeout_ms: 2000,
                                failure_msg: "TODO/FIXME/XXX/HACK markers found",
                                executor: Some(Executor::Inverted),
                            }),
                            ("accept", StageEntry {
                                command: "",
                                args: &[],
                                timeout_ms: 120000,
                                failure_msg: "Acceptance gates failed",
                                executor: Some(Executor::Composite(&["implement", "unit-test", "lint"])),
                            }),
                        ]
                    }
                    """
                done_when:     "All 9 Rust stages defined and working"
                patterns_to_use: [
                    "Static str for zero allocation",
                    "Executor::Inverted for review stage",
                    "Executor::Composite for accept stage",
                ]
            },
            {
                task: "Implement stage entries for all other languages"
                file: "crates/factory-core/src/stages/table.rs"
                what: """
                    Add functions: gleam_stages(), go_stages(), python_stages(), javascript_stages()
                    Each returns Vec<(&'static str, StageEntry)> with all 9 stages.

                    Key differences to preserve:
                    - Go lint uses StdoutEmpty executor (gofmt -l checks stdout)
                    - Gleam coverage uses find command
                    - JavaScript uses npm run for everything
                    """
                done_when:     "All 45 stage entries defined (9 stages x 5 languages)"
                patterns_to_use: [
                    "Match exact commands from original files",
                    "Match exact timeout values from original files",
                ]
            },
            {
                task: "Implement execute() with executor dispatch"
                file: "crates/factory-core/src/stages/table.rs"
                what: """
                    impl StageTable {
                        pub fn execute(&self, stage: &str, lang: Language, cwd: &Path) -> Result<()> {
                            let entry = self.entries.get(&(lang, stage))
                                .ok_or_else(|| Error::UnknownStage { name: stage.into() })?;

                            match entry.executor.unwrap_or(Executor::Standard) {
                                Executor::Standard => self.execute_standard(entry, lang, stage, cwd),
                                Executor::Inverted => self.execute_inverted(entry, lang, stage, cwd),
                                Executor::StdoutEmpty => self.execute_stdout_empty(entry, lang, stage, cwd),
                                Executor::Composite(stages) => self.execute_composite(stages, lang, cwd),
                            }
                        }
                    }
                    """
                done_when:     "All executor types work correctly"
                patterns_to_use: [
                    "? operator for error propagation",
                    "Error::stage_failed() for consistent error format",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Update mod.rs to use StageTable"
                file:      "crates/factory-core/src/stages/mod.rs"
                what:      "Replace language dispatch with single StageTable::execute() call"
                done_when: "execute_stage() uses StageTable internally"
            },
            {
                task:      "Remove old language handler files"
                file:      "crates/factory-core/src/stages/"
                what:      "Delete rust.rs, gleam.rs, go.rs, python.rs, javascript.rs"
                done_when: "Only mod.rs and table.rs remain in stages/"
            },
            {
                task:      "Update lib.rs exports if needed"
                file:      "crates/factory-core/src/lib.rs"
                what:      "Ensure stages module still exports execute_stage, execute_stages_dry_run"
                done_when: "External API unchanged"
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
                task:     "Verify line count reduction"
                done_when: "stages/ directory under 200 lines total"
                commands: ["wc -l crates/factory-core/src/stages/*.rs | tail -1"]
                expected: "Under 200 total"
            },
            {
                task:     "Verify no unwraps"
                done_when: "grep finds no unwrap/expect in stages/"
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/stages/"]
                expected: "no output (empty)"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'StageEntry' not found"
                likely_cause: "Module not properly exported from table.rs"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/stages/mod.rs"
                        what_to_check: "mod table; is present"
                    },
                    {
                        file:          "crates/factory-core/src/stages/table.rs"
                        what_to_check: "pub struct StageEntry is public"
                    },
                ]
                fix_pattern: "Add pub mod table; to mod.rs"
            },
            {
                symptom:      "Test fails: error message format different"
                likely_cause: "Not using Error::stage_failed() helper"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/stages/table.rs"
                        function:      "execute_standard()"
                        what_to_check: "Uses Error::stage_failed(lang.display_name(), stage, msg)"
                    },
                ]
                fix_pattern: "Use Error::stage_failed() with lang.display_name()"
            },
            {
                symptom:      "Review stage always passes"
                likely_cause: "Executor::Inverted not implemented correctly"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/stages/table.rs"
                        function:      "execute_inverted()"
                        what_to_check: "Returns Ok on exit code 1, Err on exit code 0"
                    },
                ]
                fix_pattern: """
                    fn execute_inverted(&self, entry: &StageEntry, ...) -> Result<()> {
                        let result = run_command(entry.command, entry.args, cwd)?;
                        match result.exit_code {
                            0 => Err(Error::stage_failed(lang, stage, entry.failure_msg)),
                            1 => Ok(()),
                            code => Err(Error::stage_failed(lang, stage, format!("grep failed: {}", code))),
                        }
                    }
                    """
            },
            {
                symptom:      "Go lint passes when it should fail"
                likely_cause: "Using exit code instead of stdout check"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/stages/table.rs"
                        function:      "execute_stdout_empty()"
                        what_to_check: "Checks result.stdout.trim().is_empty()"
                    },
                ]
                fix_pattern: """
                    fn execute_stdout_empty(&self, entry: &StageEntry, ...) -> Result<()> {
                        let result = run_command(entry.command, entry.args, cwd)?;
                        if result.stdout.trim().is_empty() {
                            Ok(())
                        } else {
                            Err(Error::stage_failed(lang, stage,
                                format!("Unformatted files:\\n{}", result.stdout)))
                        }
                    }
                    """
            },
        ]

        debugging_commands: [
            {
                scenario: "When stage lookup fails"
                run:      "RUST_LOG=debug cargo test test_execute"
                look_for: "HashMap key (Language, stage_name) mismatch"
            },
            {
                scenario: "When timeout values are wrong"
                run:      "cargo test test_preview -- --nocapture"
                look_for: "Compare timeout_ms values against original get_*_preview() functions"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_stage_table_has_all_stages passes",
            "[ ] test_builder_validates_completeness passes",
            "[ ] test_execute_matches_original passes",
            "[ ] test_review_stage_inverts_grep_result passes",
            "[ ] test_accept_stage_composes passes",
            "[ ] test_go_lint_checks_stdout passes",
            "[ ] All existing stage tests still pass",
        ]

        code: [
            "[ ] StageEntry struct defined with all fields",
            "[ ] Executor enum with Standard, Inverted, StdoutEmpty, Composite",
            "[ ] StageTable with HashMap-based lookup",
            "[ ] All 45 stage entries defined (9 x 5)",
            "[ ] execute() dispatches to correct executor",
            "[ ] preview() returns correct command and timeout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Old language files deleted or minimized",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] stages/ directory under 200 lines total",
        ]

        documentation: [
            "[ ] Module-level docs in table.rs",
            "[ ] Doc comments on StageEntry, StageTable, Executor",
            "[ ] Example usage in doc comments",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/stages/mod.rs"
                relevance: "Current router - will be simplified to use StageTable"
            },
            {
                path:      "crates/factory-core/src/stages/rust.rs"
                relevance: "Canonical example of duplication pattern - 134 lines to replace"
            },
            {
                path:      "crates/factory-core/src/stages/gleam.rs"
                relevance: "Has coverage stage using find command - needs StageEntry support"
            },
            {
                path:      "crates/factory-core/src/stages/go.rs"
                relevance: "Has lint stage checking stdout - needs Executor::StdoutEmpty"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error::stage_failed() helper - must preserve format"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Language enum and standard_pipeline() - used for validation"
            },
            {
                path:      "crates/factory-core/src/process.rs"
                relevance: "run_command() helper - reuse for execution"
            },
        ]

        external_references: [
            "https://refactoring.guru/design-patterns/builder - Builder pattern reference",
            "https://doc.rust-lang.org/std/collections/struct.HashMap.html - HashMap for lookups",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Static Configuration Tables"
                example_location: "crates/factory-core/src/domain.rs:standard_pipeline()"
                how_to_apply:     "Define stages as static data, iterate for execution"
            },
            {
                pattern:          "Display trait for enums"
                example_location: "crates/factory-core/src/domain.rs:Language"
                how_to_apply:     "Use display_name() for error messages"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use &'static str for command/args to avoid allocation",
            "Use HashMap<(Language, &'static str), StageEntry> for O(1) lookup",
            "Copy EXACT timeout values from original get_*_preview() functions",
            "Preserve error message format using Error::stage_failed()",
            "Support Executor variants for special cases (inverted grep, stdout check)",
            "Implement Composite executor for accept stage",
            "Validate all 9 stages present at table construction",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT change error message format - tests may depend on it",
            "Do NOT change timeout values - they were carefully tuned",
            "Do NOT use String where &'static str works",
        ]

        code_patterns: [
            {
                name:     "StageEntry Definition"
                use_when: "Defining a single stage's configuration"
                example:  """
                    StageEntry {
                        command: "cargo",
                        args: &["clippy", "--all-targets"],
                        timeout_ms: 45000,
                        failure_msg: "Clippy failed",
                        executor: None, // Uses Standard executor
                    }
                    """
            },
            {
                name:     "Inverted Executor for Review Stage"
                use_when: "Stage should pass when command finds nothing"
                example:  """
                    ("review", StageEntry {
                        command: "grep",
                        args: &["-r", "TODO\\|FIXME", "--include=*.rs", "."],
                        timeout_ms: 2000,
                        failure_msg: "TODO/FIXME markers found",
                        executor: Some(Executor::Inverted),
                    })

                    // In execute_inverted:
                    match result.exit_code {
                        0 => Err(Error::stage_failed(lang, stage, entry.failure_msg)),
                        1 => Ok(()), // grep found nothing - success!
                        _ => Err(Error::stage_failed(lang, stage, "grep error")),
                    }
                    """
            },
            {
                name:     "Composite Executor for Accept Stage"
                use_when: "Stage composes multiple other stages"
                example:  """
                    ("accept", StageEntry {
                        command: "", // Not used
                        args: &[],
                        timeout_ms: 120000,
                        failure_msg: "Acceptance failed",
                        executor: Some(Executor::Composite(&["implement", "unit-test", "lint"])),
                    })

                    // In execute_composite:
                    fn execute_composite(&self, stages: &[&str], lang: Language, cwd: &Path) -> Result<()> {
                        for stage in stages {
                            self.execute(stage, lang, cwd)?;
                        }
                        Ok(())
                    }
                    """
            },
            {
                name:     "StdoutEmpty Executor for Go Lint"
                use_when: "Stage passes when stdout is empty"
                example:  """
                    ("lint", StageEntry {
                        command: "gofmt",
                        args: &["-l", "."],
                        timeout_ms: 5000,
                        failure_msg: "Unformatted files",
                        executor: Some(Executor::StdoutEmpty),
                    })

                    // In execute_stdout_empty:
                    if result.stdout.trim().is_empty() {
                        Ok(())
                    } else {
                        Err(Error::stage_failed(lang, stage,
                            format!("Unformatted files:\\n{}", result.stdout)))
                    }
                    """
            },
            {
                name:     "BEFORE: Duplicated Pattern (665 lines)"
                use_when: "Understanding what we're replacing"
                example:  """
                    // rust.rs - 134 lines
                    pub fn execute_rust_stage(stage_name: &str, cwd: &Path) -> Result<()> {
                        match stage_name {
                            "implement" => rust_implement(cwd),
                            "unit-test" => rust_unit_test(cwd),
                            // ... 9 branches
                        }
                    }
                    fn rust_implement(cwd: &Path) -> Result<()> { ... }
                    fn rust_unit_test(cwd: &Path) -> Result<()> { ... }
                    // ... 9 functions

                    // gleam.rs - 151 lines (same pattern)
                    // go.rs - 147 lines (same pattern)
                    // python.rs - 114 lines (same pattern)
                    // javascript.rs - 124 lines (same pattern)
                    """
            },
            {
                name:     "AFTER: Declarative Table (~80 lines)"
                use_when: "Understanding the target design"
                example:  """
                    // table.rs - ~80 lines total
                    pub struct StageTable {
                        entries: HashMap<(Language, &'static str), StageEntry>,
                    }

                    impl StageTable {
                        pub fn new() -> Self {
                            let mut entries = HashMap::new();
                            for (stage, entry) in rust_stages() {
                                entries.insert((Language::Rust, stage), entry);
                            }
                            // ... other languages
                            Self { entries }
                        }
                    }

                    fn rust_stages() -> Vec<(&'static str, StageEntry)> {
                        vec![
                            ("implement", StageEntry { command: "cargo", args: &["build"], ... }),
                            ("unit-test", StageEntry { command: "cargo", args: &["test"], ... }),
                            // ... 9 entries, ~2 lines each
                        ]
                    }
                    """
            },
        ]
    }
}
