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

"factory-ac1": #ValidBead & {
    // ============================================================================
    // BEAD: factory-ac1 - Fix silent language detection fallback
    // ============================================================================

    id:              "factory-ac1"
    title:           "Bug: Fix silent language detection fallback that runs wrong pipeline"
    type:            "bug"
    priority:        1
    effort_estimate: "1hr"
    labels:          ["bug", "correctness", "error-handling", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL propagate language detection errors instead of silently defaulting",
            "THE SYSTEM SHALL provide clear error messages when language cannot be detected",
            "THE SYSTEM SHALL never run a pipeline for the wrong language",
            "THE SYSTEM SHALL use Result<T, Error> for all fallible language detection operations",
        ]

        event_driven: [
            {
                trigger: "WHEN get_worktree is called for an existing worktree"
                shall:   "THE SYSTEM SHALL detect the language or return Error::LanguageDetectionFailed"
            },
            {
                trigger: "WHEN language detection fails in get_worktree"
                shall:   "THE SYSTEM SHALL return a descriptive error with the worktree path"
            },
            {
                trigger: "WHEN a user runs a stage on a worktree with unknown language"
                shall:   "THE SYSTEM SHALL fail fast with actionable guidance"
            },
        ]

        state_driven: [
            {
                state: "WHILE a worktree exists without recognized language markers"
                shall: "THE SYSTEM SHALL report LanguageDetectionFailed when retrieving worktree info"
            },
        ]

        unwanted: [
            {
                condition: "IF language detection fails"
                shall_not: "THE SYSTEM SHALL NOT silently default to Go or any other language"
                because:   "Running the wrong pipeline wastes time and produces confusing errors"
            },
            {
                condition: "IF worktree path has no recognizable language files"
                shall_not: "THE SYSTEM SHALL NOT proceed with pipeline execution"
                because:   "Wrong pipeline tools will fail with cryptic errors"
            },
            {
                condition: "IF detect_language returns Err"
                shall_not: "THE SYSTEM SHALL NOT use unwrap_or to hide the error"
                because:   "Silent fallbacks violate Railway-Oriented Programming principles"
            },
        ]

        complex: [
            {
                state:   "WHILE retrieving worktree information"
                trigger: "WHEN language detection fails"
                shall:   "THE SYSTEM SHALL include the worktree path in the error context"
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
                    field:           "worktree_path"
                    type:            "PathBuf"
                    constraints:     "Must be an existing directory"
                    example_valid:   "/repo/.factory-workspaces/my-task-12345"
                    example_invalid: "/nonexistent/path"
                },
                {
                    field:           "slug"
                    type:            "String"
                    constraints:     "Must match an existing worktree symlink"
                    example_valid:   "my-task"
                    example_invalid: "nonexistent-task"
                },
            ]
            system_state: [
                "Worktree symlink exists in .factory/ directory",
                "Worktree directory is accessible",
            ]
        }

        postconditions: {
            state_changes: [
                "No silent language fallback occurs",
                "Error is propagated to caller with full context",
            ]
            return_guarantees: [
                {
                    field:     "get_worktree()"
                    guarantee: "Returns Result<Worktree, Error> - never silently assumes language"
                },
                {
                    field:     "Worktree.language"
                    guarantee: "Always correctly detected from project files, never defaulted"
                },
                {
                    field:     "Error::LanguageDetectionFailed"
                    guarantee: "Contains worktree path for debugging"
                },
            ]
            side_effects: []
        }

        invariants: [
            "get_worktree never uses unwrap_or for language detection",
            "Language field always matches actual project type",
            "Error messages include path context for debugging",
            "No silent fallbacks anywhere in worktree module",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Existing callers expect get_worktree to always succeed for existing worktrees"
                prevention:  "Callers must handle LanguageDetectionFailed error variant"
                test_for_it: "test_get_worktree_fails_on_unknown_language"
            },
            {
                failure:     "CLI commands fail with confusing error when language undetected"
                prevention:  "CLI layer translates error to user-friendly message with guidance"
                test_for_it: "test_stage_command_shows_helpful_error_for_unknown_language"
            },
        ]

        usability_failures: [
            {
                failure:     "Error message doesn't tell user how to fix the problem"
                prevention:  "Include list of supported file markers in error message"
                test_for_it: "test_error_message_lists_supported_markers"
            },
            {
                failure:     "User doesn't know which directory failed detection"
                prevention:  "Include worktree path in error context"
                test_for_it: "test_error_includes_path"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Task stored with wrong language, corrupts future operations"
                prevention:  "Never store task if language detection fails"
                test_for_it: "test_task_creation_fails_on_unknown_language"
            },
            {
                failure:     "Worktree struct contains incorrect language field"
                prevention:  "Propagate error instead of defaulting"
                test_for_it: "test_worktree_language_matches_detection"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_get_worktree_detects_rust_language"
                given: "A worktree with Cargo.toml"
                when:  "get_worktree() is called"
                then: [
                    "Returns Ok(Worktree)",
                    "Worktree.language == Language::Rust",
                ]
                real_input: """
                    // Setup: create worktree with Cargo.toml
                    let slug = "rust-task";
                    let worktree_path = repo_root.join(".factory-workspaces/rust-task-12345");
                    std::fs::create_dir_all(&worktree_path)?;
                    std::fs::write(worktree_path.join("Cargo.toml"), "[package]\\nname = \\"test\\"")?;
                    // Create symlink
                    let factory_dir = repo_root.join(".factory");
                    std::fs::create_dir_all(&factory_dir)?;
                    std::os::unix::fs::symlink(&worktree_path, factory_dir.join(slug))?;
                    """
                expected_output: """
                    let result = get_worktree(slug, &repo_root);
                    assert!(result.is_ok());
                    let wt = result.unwrap();
                    assert_eq!(wt.language, Language::Rust);
                    """
            },
            {
                name:  "test_get_worktree_detects_gleam_language"
                given: "A worktree with gleam.toml"
                when:  "get_worktree() is called"
                then: [
                    "Returns Ok(Worktree)",
                    "Worktree.language == Language::Gleam",
                ]
                real_input: """
                    // Setup: create worktree with gleam.toml
                    let worktree_path = repo_root.join(".factory-workspaces/gleam-task-12345");
                    std::fs::create_dir_all(&worktree_path)?;
                    std::fs::write(worktree_path.join("gleam.toml"), "name = \\"test\\"")?;
                    """
                expected_output: """
                    let result = get_worktree("gleam-task", &repo_root);
                    assert!(result.is_ok());
                    assert_eq!(result.unwrap().language, Language::Gleam);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_get_worktree_fails_on_unknown_language"
                given: "A worktree with no recognized language files"
                when:  "get_worktree() is called"
                then: [
                    "Returns Err(Error::LanguageDetectionFailed)",
                    "Error message mentions the worktree path",
                    "Error message lists supported file markers",
                ]
                real_input: """
                    // Setup: create worktree with no language markers
                    let worktree_path = repo_root.join(".factory-workspaces/empty-task-12345");
                    std::fs::create_dir_all(&worktree_path)?;
                    // Only create a README, no language-specific files
                    std::fs::write(worktree_path.join("README.md"), "# Empty")?;
                    // Create symlink
                    let factory_dir = repo_root.join(".factory");
                    std::fs::create_dir_all(&factory_dir)?;
                    std::os::unix::fs::symlink(&worktree_path, factory_dir.join("empty-task"))?;
                    """
                expected_output: null
                expected_error: """
                    let result = get_worktree("empty-task", &repo_root);
                    assert!(result.is_err());
                    let err = result.unwrap_err();
                    assert!(matches!(err, Error::LanguageDetectionFailed));
                    let msg = err.to_string();
                    assert!(msg.contains("gleam.toml") || msg.contains("Cargo.toml"));
                    """
            },
            {
                name:  "test_get_worktree_does_not_default_to_go"
                given: "A worktree where language detection fails"
                when:  "get_worktree() is called"
                then: [
                    "Returns Err, not Ok with Language::Go",
                    "No silent fallback occurs",
                ]
                real_input: """
                    // Setup: worktree with only .txt files
                    let worktree_path = repo_root.join(".factory-workspaces/text-task-12345");
                    std::fs::create_dir_all(&worktree_path)?;
                    std::fs::write(worktree_path.join("notes.txt"), "some notes")?;
                    """
                expected_output: null
                expected_error: """
                    let result = get_worktree("text-task", &repo_root);
                    // The bug: before fix, this would return Ok with Language::Go
                    // After fix: this must return Err
                    assert!(result.is_err(), "Must not silently default to Go");
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_worktree_with_multiple_language_markers"
                scenario: "Worktree has both Cargo.toml and gleam.toml (monorepo)"
                input:    "Worktree with multiple language files"
                expected: "Returns first detected language per priority order (Gleam > Go > Rust)"
            },
            {
                name:     "test_worktree_path_not_readable"
                scenario: "Worktree directory exists but is not readable"
                input:    "Worktree path with no read permissions"
                expected: "Returns appropriate error, not silent Go fallback"
            },
        ]

        contract_tests: [
            {
                name:     "test_no_unwrap_or_in_get_worktree"
                verifies: "No unwrap_or() or similar fallback patterns"
                test:     "rg 'unwrap_or|unwrap_or_default|unwrap_or_else.*Language' crates/factory-core/src/worktree.rs returns empty"
            },
            {
                name:     "test_get_worktree_returns_result"
                verifies: "get_worktree signature returns Result<Worktree, Error>"
                test:     "Function signature inspection"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_stage_fails_fast_on_unknown_language"
            description: "Verify stage command fails with helpful error when language unknown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/language_detection_e2e.rs"
                        content: """
                            use factory_core::worktree::get_worktree;
                            use factory_core::error::{Error, Result};
                            use std::path::Path;
                            use tempfile::TempDir;

                            #[test]
                            fn test_get_worktree_rejects_unknown_language() -> Result<()> {
                                let temp = TempDir::new()?;
                                let repo_root = temp.path();

                                // Create worktree structure without language markers
                                let wt_path = repo_root.join(".factory-workspaces/test-12345");
                                std::fs::create_dir_all(&wt_path)?;
                                std::fs::write(wt_path.join("README.md"), "# Test")?;

                                // Create symlink
                                let factory_dir = repo_root.join(".factory");
                                std::fs::create_dir_all(&factory_dir)?;
                                std::os::unix::fs::symlink(&wt_path, factory_dir.join("test"))?;

                                // Act
                                let result = get_worktree("test", repo_root);

                                // Assert: must be error, not silent Go fallback
                                assert!(result.is_err());
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
                command:    "moon run factory-core:test -- --test language_detection_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_get_worktree_rejects_unknown_language ... ok",
                ]
                files_created: []
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/language_detection_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_stage_command_unknown_language"
                description: "factory stage fails helpfully when worktree has no language markers"
                steps: [
                    {action: "Create worktree with no language files", verify: "Worktree directory exists"},
                    {action: "Run factory stage -s test --stage implement", verify: "Command fails with error"},
                    {action: "Check error message", verify: "Error mentions language detection and lists supported files"},
                ]
            },
            {
                name:        "e2e_list_worktrees_with_unknown_language"
                description: "factory list handles worktrees with unknown language gracefully"
                steps: [
                    {action: "Create worktree with Cargo.toml", verify: "Worktree exists with Rust"},
                    {action: "Create worktree with no language files", verify: "Worktree exists"},
                    {action: "Run factory list", verify: "Shows error for unknown language worktree, success for Rust"},
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
                task:      "Write test: test_get_worktree_fails_on_unknown_language"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that get_worktree returns Err when language detection fails"
                done_when: "Test exists and FAILS (red phase - proves bug exists)"
            },
            {
                task:      "Write test: test_get_worktree_does_not_default_to_go"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that get_worktree does not silently return Language::Go"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_error_message_is_helpful"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that error message lists supported language markers"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Remove unwrap_or fallback in get_worktree"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    Change line 168 from:
                      let language = detect_language(&worktree_path).unwrap_or(Language::Go);
                    To:
                      let language = detect_language(&worktree_path)?;
                    """
                done_when:     "unwrap_or removed, ? operator used"
                patterns_to_use: ["Railway-Oriented error propagation with ?"]
            },
            {
                task: "Verify error message quality"
                file: "crates/factory-core/src/error.rs"
                what: """
                    Ensure LanguageDetectionFailed error variant has helpful message.
                    Current message already lists supported files, verify it's adequate.
                    """
                done_when:     "Error message contains list of supported file markers"
                patterns_to_use: ["Descriptive error messages with actionable guidance"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Update list_worktrees to handle detection failures"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "list_worktrees calls get_worktree which may now fail; handle gracefully"
                done_when: "list_worktrees either skips failed worktrees with warning or propagates error"
            },
            {
                task:      "Update CLI error handling"
                file:      "crates/factory/src/main.rs"
                what:      "Ensure CLI displays helpful error when language detection fails"
                done_when: "User sees actionable error message"
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
                task:     "Verify no unwrap_or for language"
                done_when: "grep finds no unwrap_or with Language fallback"
                commands: ["rg 'unwrap_or.*Language|Language.*unwrap_or' crates/factory-core/src/worktree.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual test with empty worktree"
                done_when: "factory stage fails with helpful error"
                commands: [
                    "mkdir -p /tmp/test-repo/.factory-workspaces/empty-task",
                    "mkdir -p /tmp/test-repo/.factory",
                    "ln -sf /tmp/test-repo/.factory-workspaces/empty-task /tmp/test-repo/.factory/empty-task",
                    "cd /tmp/test-repo && factory stage -s empty-task --stage implement",
                ]
                expected: "Error message about language detection, not Go pipeline running"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Test test_get_worktree_fails_on_unknown_language passes before fix"
                likely_cause: "Test setup is wrong - language marker file accidentally created"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        function:      "test setup"
                        what_to_check: "Ensure no Cargo.toml, gleam.toml, go.mod, etc. in test worktree"
                    },
                ]
                fix_pattern: "Use tempdir with only README.md or .txt files"
            },
            {
                symptom:      "list_worktrees panics after fix"
                likely_cause: "list_worktrees doesn't handle get_worktree returning Err"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        function:      "list_worktrees()"
                        what_to_check: "Does it handle Err from get_worktree?"
                    },
                ]
                fix_pattern: "Use filter_map to skip failed worktrees or collect Results"
            },
            {
                symptom:      "CLI shows raw error instead of user-friendly message"
                likely_cause: "CLI doesn't format LanguageDetectionFailed specially"
                where_to_look: [
                    {
                        file:          "crates/factory/src/main.rs"
                        function:      "error handling"
                        what_to_check: "Is there special handling for this error?"
                    },
                ]
                fix_pattern: "Match on Error::LanguageDetectionFailed and provide guidance"
            },
        ]

        debugging_commands: [
            {
                scenario: "When unsure if detect_language returns error"
                run:      "cd worktree_path && ls -la"
                look_for: "Presence of gleam.toml, Cargo.toml, go.mod, pyproject.toml, package.json"
            },
            {
                scenario: "When error message unclear"
                run:      "RUST_BACKTRACE=1 factory stage -s task --stage implement"
                look_for: "Stack trace showing where error originated"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_get_worktree_fails_on_unknown_language passes",
            "[ ] test_get_worktree_does_not_default_to_go passes",
            "[ ] test_error_message_is_helpful passes",
            "[ ] test_get_worktree_detects_rust_language passes",
            "[ ] test_get_worktree_detects_gleam_language passes",
            "[ ] All existing worktree tests still pass",
        ]

        code: [
            "[ ] unwrap_or(Language::Go) removed from line 168",
            "[ ] ? operator used for error propagation",
            "[ ] No new unwrap() or expect() introduced",
            "[ ] list_worktrees handles detection failures gracefully",
            "[ ] Error message is helpful and actionable",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] rg 'unwrap_or.*Language' returns empty",
        ]

        documentation: [
            "[ ] get_worktree doc comment updated to note it can fail on language detection",
            "[ ] Error variant documented with recovery guidance",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/worktree.rs"
                relevance: "Contains the bug at line 168: unwrap_or(Language::Go)"
            },
            {
                path:      "crates/factory-core/src/repo.rs"
                relevance: "Contains detect_language() that returns Result<Language>"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Defines Language enum and detect_from_files()"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Defines Error::LanguageDetectionFailed variant"
            },
            {
                path:      "crates/factory/src/main.rs"
                relevance: "CLI entry point - may need error handling update"
            },
        ]

        external_references: [
            "https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html - Result and ? operator",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs:Slug::new"
                how_to_apply:     "Replace unwrap_or with ? operator to propagate errors"
            },
            {
                pattern:          "Descriptive Error Messages"
                example_location: "crates/factory-core/src/error.rs:LanguageDetectionFailed"
                how_to_apply:     "Error already has good message, just propagate it"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Replace unwrap_or(Language::Go) with ? operator",
            "Ensure error propagates up the call stack",
            "Handle potential cascade failures in list_worktrees",
            "Test with worktree directories that have no language markers",
            "Verify error message is helpful (it already lists supported files)",
        ]

        do_not: [
            "Do NOT replace one silent fallback with another",
            "Do NOT use unwrap(), expect(), or any panic-inducing code",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT add new languages without updating Language::detect_from_files",
            "Do NOT ignore the downstream impact on list_worktrees",
        ]

        code_patterns: [
            {
                name:     "Error Propagation with ?"
                use_when: "Converting unwrap_or to proper error handling"
                example:  """
                    // BEFORE (bug):
                    let language = detect_language(&worktree_path).unwrap_or(Language::Go);

                    // AFTER (fix):
                    let language = detect_language(&worktree_path)?;
                    """
            },
            {
                name:     "Graceful Collection with filter_map"
                use_when: "Collecting results where some may fail"
                example:  """
                    // If list_worktrees should skip failed detections:
                    output
                        .stdout
                        .lines()
                        .filter(|line| !line.trim().is_empty())
                        .filter_map(|slug| get_worktree(slug.trim(), repo_root).ok())
                        .collect()
                    """
            },
            {
                name:     "Helpful Error Context"
                use_when: "Adding path context to errors"
                example:  """
                    // If needed, wrap error with context:
                    detect_language(&worktree_path)
                        .map_err(|e| Error::WorktreeLanguageDetectionFailed {
                            slug: slug.to_string(),
                            path: worktree_path.clone(),
                            source: Box::new(e),
                        })?
                    """
            },
        ]
    }
}
