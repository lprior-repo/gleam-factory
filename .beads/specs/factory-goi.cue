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

"factory-goi": #ValidBead & {
    // ============================================================================
    // BEAD: factory-goi - Fix worktree cleanup that ignores all failures
    // ============================================================================

    id:              "factory-goi"
    title:           "Bug: Fix worktree cleanup that ignores all failures and always returns Ok"
    type:            "bug"
    priority:        1
    effort_estimate: "2hr"
    labels:          ["bug", "worktree", "error-handling", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL collect all cleanup errors during remove_worktree()",
            "THE SYSTEM SHALL propagate cleanup failures via Result<T, Error>",
            "THE SYSTEM SHALL never silently discard errors with `let _ = ...`",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN jj workspace forget fails"
                shall:   "THE SYSTEM SHALL record the failure and continue with git cleanup"
            },
            {
                trigger: "WHEN git worktree remove fails"
                shall:   "THE SYSTEM SHALL record the failure and continue with rm -rf cleanup"
            },
            {
                trigger: "WHEN rm -rf fails on worktree directory"
                shall:   "THE SYSTEM SHALL record the failure and continue with symlink removal"
            },
            {
                trigger: "WHEN symlink removal fails"
                shall:   "THE SYSTEM SHALL record the failure and include in final error"
            },
            {
                trigger: "WHEN all cleanup steps succeed"
                shall:   "THE SYSTEM SHALL return Ok(())"
            },
            {
                trigger: "WHEN any cleanup step fails but worktree is fully removed"
                shall:   "THE SYSTEM SHALL log warnings but return Ok(())"
            },
            {
                trigger: "WHEN cleanup fails and orphaned resources remain"
                shall:   "THE SYSTEM SHALL return Err with aggregated failures"
            },
        ]

        state_driven: [
            {
                state: "WHILE cleanup is in progress"
                shall: "THE SYSTEM SHALL track which resources have been cleaned"
            },
            {
                state: "WHILE cleanup is in progress"
                shall: "THE SYSTEM SHALL continue cleanup even after individual failures"
            },
        ]

        unwanted: [
            {
                condition: "IF all cleanup operations fail"
                shall_not: "THE SYSTEM SHALL NOT return Ok(())"
                because:   "Silent failure leaves orphaned worktrees consuming disk space and confusing state"
            },
            {
                condition: "IF symlink still exists after cleanup"
                shall_not: "THE SYSTEM SHALL NOT report success"
                because:   "Users will think worktree is gone but it will reappear in list_worktrees()"
            },
            {
                condition: "IF cleanup fails"
                shall_not: "THE SYSTEM SHALL NOT lose error details"
                because:   "Operators need error context to manually clean up orphaned resources"
            },
        ]

        complex: [
            {
                state:   "WHILE the worktree directory still exists"
                trigger: "WHEN symlink removal succeeds"
                shall:   "THE SYSTEM SHALL still return error about orphaned directory"
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
                    field:           "slug"
                    type:            "String"
                    constraints:     "Valid slug that exists in .factory/"
                    example_valid:   "my-feature"
                    example_invalid: "../escape"
                },
                {
                    field:           "repo_root"
                    type:            "Path"
                    constraints:     "Valid path to repository root"
                    example_valid:   "/home/user/project"
                    example_invalid: "/nonexistent/path"
                },
            ]
            system_state: [
                "Worktree exists (symlink in .factory/ points to valid path)",
                "User has write permissions to .factory-workspaces/",
            ]
        }

        postconditions: {
            state_changes: [
                "On success: symlink removed from .factory/<slug>",
                "On success: worktree directory removed from .factory-workspaces/",
                "On success: jj workspace forgotten OR git worktree removed",
                "On failure: error contains list of failed operations",
                "On failure: error indicates which resources remain orphaned",
            ]
            return_guarantees: [
                {
                    field:     "Result<()>"
                    guarantee: "Ok only if symlink is removed and directory no longer exists"
                },
                {
                    field:     "Error::WorktreeCleanupFailed"
                    guarantee: "Contains all failed operations and remaining orphaned paths"
                },
            ]
            side_effects: [
                "Logging via tracing for each cleanup step attempted",
                "Logging warnings for non-critical failures (e.g., jj fails but git succeeds)",
            ]
        }

        invariants: [
            "No `let _ = ...` patterns that discard Result values",
            "All cleanup operations are attempted regardless of prior failures",
            "Error aggregation preserves context from all failures",
            "Success only returned when critical resources are cleaned",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "jj command not installed but repo uses jj"
                prevention:  "Detect jj presence before attempting jj cleanup"
                test_for_it: "test_cleanup_without_jj_installed"
            },
            {
                failure:     "git worktree remove fails with 'not a worktree'"
                prevention:  "Check if path is a git worktree before calling git worktree remove"
                test_for_it: "test_cleanup_jj_only_worktree"
            },
            {
                failure:     "rm -rf fails due to permissions"
                prevention:  "Check write permissions, include path in error"
                test_for_it: "test_cleanup_permission_denied"
            },
        ]

        usability_failures: [
            {
                failure:     "Error message doesn't show which step failed"
                prevention:  "Include step name in each collected error"
                test_for_it: "test_error_identifies_failed_step"
            },
            {
                failure:     "User can't tell what to clean up manually"
                prevention:  "Error includes paths of orphaned resources"
                test_for_it: "test_error_includes_orphan_paths"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Symlink removed but directory remains (ghost worktree)"
                prevention:  "Verify directory removal before marking success"
                test_for_it: "test_verify_directory_removed"
            },
            {
                failure:     "Directory removed but symlink remains (dangling symlink)"
                prevention:  "Always attempt symlink removal, verify it's gone"
                test_for_it: "test_verify_symlink_removed"
            },
            {
                failure:     "Worktree in list_worktrees() after 'successful' removal"
                prevention:  "Verify worktree not in list after cleanup"
                test_for_it: "test_worktree_not_listed_after_removal"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_remove_worktree_success_jj"
                given: "A worktree created with jj workspace add"
                when:  "remove_worktree() is called"
                then: [
                    "jj workspace forget succeeds",
                    "Directory is removed",
                    "Symlink is removed",
                    "Returns Ok(())",
                ]
                real_input: """
                    let slug = "test-feature";
                    let repo_root = setup_jj_repo_with_worktree(slug)?;
                    """
                expected_output: """
                    let result = remove_worktree(slug, &repo_root);
                    assert!(result.is_ok());
                    assert!(!repo_root.join(".factory").join(slug).exists());
                    """
            },
            {
                name:  "test_remove_worktree_success_git"
                given: "A worktree created with git worktree add"
                when:  "remove_worktree() is called"
                then: [
                    "jj workspace forget fails (expected)",
                    "git worktree remove succeeds",
                    "Symlink is removed",
                    "Returns Ok(())",
                ]
                real_input: """
                    let slug = "test-feature";
                    let repo_root = setup_git_repo_with_worktree(slug)?;
                    """
                expected_output: """
                    let result = remove_worktree(slug, &repo_root);
                    assert!(result.is_ok());
                    assert!(!repo_root.join(".factory").join(slug).exists());
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_remove_worktree_all_fail_returns_error"
                given: "A worktree where all cleanup operations fail"
                when:  "remove_worktree() is called"
                then: [
                    "Returns Err(WorktreeCleanupFailed)",
                    "Error contains all failure reasons",
                    "Error lists orphaned paths",
                ]
                real_input: """
                    let slug = "test-feature";
                    let repo_root = setup_worktree_in_readonly_dir(slug)?;
                    """
                expected_output: null
                expected_error: """
                    Error::WorktreeCleanupFailed {
                        slug: "test-feature",
                        failures: vec![...],
                        orphaned_paths: vec![...],
                    }
                    """
            },
            {
                name:  "test_remove_worktree_symlink_remains_returns_error"
                given: "A worktree where directory removal succeeds but symlink removal fails"
                when:  "remove_worktree() is called"
                then: [
                    "Returns Err(WorktreeCleanupFailed)",
                    "Error indicates symlink still exists",
                ]
                real_input: """
                    let slug = "test-feature";
                    let repo_root = setup_worktree_with_readonly_factory_dir(slug)?;
                    """
                expected_output: null
                expected_error: """
                    Error::WorktreeCleanupFailed {
                        slug: "test-feature",
                        failures: vec!["symlink removal failed: permission denied"],
                        orphaned_paths: vec![PathBuf::from(".factory/test-feature")],
                    }
                    """
            },
            {
                name:  "test_remove_worktree_directory_remains_returns_error"
                given: "A worktree where symlink removal succeeds but directory removal fails"
                when:  "remove_worktree() is called"
                then: [
                    "Returns Err(WorktreeCleanupFailed)",
                    "Error indicates directory still exists",
                ]
                real_input: """
                    let slug = "test-feature";
                    let repo_root = setup_worktree_with_locked_directory(slug)?;
                    """
                expected_output: null
                expected_error: """
                    Error::WorktreeCleanupFailed {
                        slug: "test-feature",
                        failures: vec!["rm -rf failed", "directory still exists"],
                        orphaned_paths: vec![PathBuf::from(".factory-workspaces/test-feature-12345")],
                    }
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_cleanup_partial_success_jj_only"
                scenario: "jj succeeds, git fails, rm succeeds, symlink succeeds"
                input:    "Worktree in jj-only repo where git is not tracking worktree"
                expected: "Returns Ok() because resources are cleaned"
            },
            {
                name:     "test_cleanup_partial_success_rm_only"
                scenario: "jj fails, git fails, rm succeeds, symlink succeeds"
                input:    "Worktree where VCS commands fail but filesystem cleanup works"
                expected: "Returns Ok() because directory and symlink are gone"
            },
            {
                name:     "test_cleanup_worktree_not_found"
                scenario: "Slug doesn't exist in .factory/"
                input:    "remove_worktree('nonexistent', repo_root)"
                expected: "Returns Err(WorktreeNotFound)"
            },
        ]

        contract_tests: [
            {
                name:     "test_no_let_underscore_result"
                verifies: "No `let _ = ...` discarding Result types"
                test:     "grep -n 'let _ = run_command' crates/factory-core/src/worktree.rs returns empty"
            },
            {
                name:     "test_error_aggregation"
                verifies: "Errors are collected not discarded"
                test:     "Code review confirms Vec<String> or similar aggregation"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_remove_worktree_e2e"
            description: "Complete worktree lifecycle: create -> verify -> remove -> verify gone"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/worktree_cleanup_e2e.rs"
                        content: """
                            use factory_core::worktree::{create_worktree, remove_worktree, get_worktree};
                            use factory_core::domain::Language;
                            use factory_core::error::Result;
                            use tempfile::tempdir;
                            use std::process::Command;

                            fn setup_git_repo() -> Result<tempfile::TempDir> {
                                let dir = tempdir()?;
                                Command::new("git")
                                    .args(["init"])
                                    .current_dir(dir.path())
                                    .status()?;
                                Command::new("git")
                                    .args(["commit", "--allow-empty", "-m", "init"])
                                    .current_dir(dir.path())
                                    .status()?;
                                Ok(dir)
                            }

                            #[test]
                            fn test_worktree_cleanup_removes_all_resources() -> Result<()> {
                                let repo = setup_git_repo()?;
                                let slug = "test-cleanup";

                                // Create worktree
                                let wt = create_worktree(slug, Language::Rust, repo.path())?;
                                assert!(wt.path.exists());
                                assert!(repo.path().join(".factory").join(slug).exists());

                                // Remove worktree
                                remove_worktree(slug, repo.path())?;

                                // Verify cleanup
                                assert!(!wt.path.exists(), "Directory should be removed");
                                assert!(!repo.path().join(".factory").join(slug).exists(), "Symlink should be removed");
                                assert!(get_worktree(slug, repo.path()).is_err(), "Worktree should not be found");

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
                command:    "moon run factory-core:test -- --test worktree_cleanup_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_worktree_cleanup_removes_all_resources ... ok",
                ]
                files_created: []
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/worktree_cleanup_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_cleanup_failure_shows_orphans"
                description: "Verify failure error includes orphaned resource paths"
                steps: [
                    {action: "Create worktree", verify: "Worktree exists"},
                    {action: "Make directory read-only", verify: "chmod 555 succeeds"},
                    {action: "Attempt remove_worktree", verify: "Returns error"},
                    {action: "Check error contents", verify: "Contains orphaned_paths field"},
                    {action: "Restore permissions", verify: "chmod 755 succeeds"},
                    {action: "Retry remove_worktree", verify: "Succeeds now"},
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
                task:      "Write test: test_remove_worktree_all_fail_returns_error"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that all failures result in Err, not Ok"
                done_when: "Test exists and FAILS with current code (red phase)"
            },
            {
                task:      "Write test: test_remove_worktree_symlink_remains_returns_error"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that orphaned symlink causes error"
                done_when: "Test exists and FAILS with current code (red phase)"
            },
            {
                task:      "Write test: test_cleanup_partial_success_rm_only"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that rm+symlink success is Ok even if jj/git fail"
                done_when: "Test exists and FAILS with current code (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add WorktreeCleanupFailed error variant"
                file: "crates/factory-core/src/error.rs"
                what: """
                    #[error("worktree cleanup failed for '{slug}': {failures:?}")]
                    WorktreeCleanupFailed {
                        slug: String,
                        failures: Vec<String>,
                        orphaned_paths: Vec<PathBuf>,
                    }
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["thiserror derive", "Vec for aggregation"]
            },
            {
                task: "Create CleanupResult struct for tracking"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    /// Tracks cleanup operation results.
                    struct CleanupResult {
                        jj_success: bool,
                        git_success: bool,
                        rm_success: bool,
                        symlink_success: bool,
                        failures: Vec<String>,
                    }

                    impl CleanupResult {
                        fn new() -> Self { ... }
                        fn record_jj(&mut self, result: Result<CommandOutput>) { ... }
                        fn record_git(&mut self, result: Result<CommandOutput>) { ... }
                        fn record_rm(&mut self, result: Result<CommandOutput>) { ... }
                        fn record_symlink(&mut self, result: Result<CommandOutput>) { ... }
                        fn is_success(&self) -> bool {
                            // Success if symlink gone AND (directory gone via any method)
                            self.symlink_success && (self.jj_success || self.git_success || self.rm_success)
                        }
                    }
                    """
                done_when:     "Module compiles"
                patterns_to_use: ["Builder pattern for result tracking"]
            },
            {
                task: "Refactor remove_worktree to collect errors"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    pub fn remove_worktree(slug: &str, repo_root: &Path) -> Result<()> {
                        tracing::info!(slug, "Removing worktree");

                        let wt = get_worktree(slug, repo_root)?;
                        let mut cleanup = CleanupResult::new();

                        // Try jj workspace forget
                        cleanup.record_jj(try_jj_forget(slug, repo_root));

                        // Try git worktree remove
                        cleanup.record_git(try_git_remove(&wt.path, repo_root));

                        // Try rm -rf as fallback
                        if !cleanup.directory_removed() {
                            cleanup.record_rm(try_rm_rf(&wt.path, repo_root));
                        }

                        // Remove symlink
                        let symlink_path = repo_root.join(FACTORY_DIR).join(slug);
                        cleanup.record_symlink(try_rm_symlink(&symlink_path, repo_root));

                        // Verify and return
                        cleanup.into_result(slug, &wt.path, &symlink_path)
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "Result<T, Error> for all operations",
                    "Error aggregation into Vec<String>",
                    "Verification after all cleanup attempts",
                ]
            },
            {
                task: "Add helper functions for cleanup operations"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    fn try_jj_forget(slug: &str, repo_root: &Path) -> Result<CommandOutput> {
                        let repo_str = repo_root.to_string_lossy();
                        run_command(
                            "jj",
                            &["-R", &repo_str, "workspace", "forget", &format!("{slug}-*")],
                            repo_root,
                        )
                    }

                    fn try_git_remove(worktree_path: &Path, repo_root: &Path) -> Result<CommandOutput> {
                        let repo_str = repo_root.to_string_lossy();
                        run_command(
                            "git",
                            &["-C", &repo_str, "worktree", "remove", &worktree_path.to_string_lossy(), "--force"],
                            repo_root,
                        )
                    }

                    fn try_rm_rf(path: &Path, cwd: &Path) -> Result<CommandOutput> {
                        run_command("rm", &["-rf", &path.to_string_lossy()], cwd)
                    }

                    fn try_rm_symlink(path: &Path, cwd: &Path) -> Result<CommandOutput> {
                        run_command("rm", &["-f", &path.to_string_lossy()], cwd)
                    }
                    """
                done_when:     "Functions compile and are used by remove_worktree"
                patterns_to_use: ["Small focused functions", "Return Result not ()"]
            },
            {
                task: "Implement CleanupResult::into_result"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    impl CleanupResult {
                        fn into_result(
                            self,
                            slug: &str,
                            worktree_path: &Path,
                            symlink_path: &Path,
                        ) -> Result<()> {
                            let mut orphaned_paths = Vec::new();

                            // Check what's still there
                            if worktree_path.exists() {
                                orphaned_paths.push(worktree_path.to_path_buf());
                            }
                            if symlink_path.exists() || symlink_path.symlink_metadata().is_ok() {
                                orphaned_paths.push(symlink_path.to_path_buf());
                            }

                            if orphaned_paths.is_empty() {
                                tracing::info!(slug, ?worktree_path, "Worktree removed");
                                Ok(())
                            } else {
                                Err(Error::WorktreeCleanupFailed {
                                    slug: slug.to_string(),
                                    failures: self.failures,
                                    orphaned_paths,
                                })
                            }
                        }
                    }
                    """
                done_when:     "Verification logic returns error when resources remain"
                patterns_to_use: ["Filesystem verification", "Error aggregation"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Update error.rs with WorktreeCleanupFailed"
                file:      "crates/factory-core/src/error.rs"
                what:      "Add the new error variant with proper fields"
                done_when: "Error variant exists and formats correctly"
            },
            {
                task:      "Add Error::worktree_cleanup_failed helper"
                file:      "crates/factory-core/src/error.rs"
                what:      "Convenience constructor for the error"
                done_when: "Helper function exists"
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
                task:     "Verify no let _ = patterns"
                done_when: "grep finds no let _ = run_command"
                commands: ["rg 'let _ = run_command' crates/factory-core/src/worktree.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Manual verification - success case"
                done_when: "Worktree creates and removes cleanly"
                commands: [
                    "factory new -s test-cleanup",
                    "factory remove -s test-cleanup",
                    "ls .factory/test-cleanup",  // Should fail (not found)
                ]
                expected: "Worktree fully removed, no orphans"
            },
            {
                task:     "Manual verification - failure case"
                done_when: "Error shows what failed and what remains"
                commands: [
                    "factory new -s test-cleanup",
                    "chmod 555 .factory-workspaces/test-cleanup-*",
                    "factory remove -s test-cleanup",  // Should fail with details
                ]
                expected: "Error message lists failures and orphaned paths"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "remove_worktree returns Ok but worktree still in list"
                likely_cause: "Verification not checking symlink existence"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        function:      "CleanupResult::into_result"
                        what_to_check: "Is symlink_path.exists() OR symlink_metadata().is_ok() checked?"
                    },
                ]
                fix_pattern: "Check both exists() and symlink_metadata() for dangling symlinks"
            },
            {
                symptom:      "remove_worktree errors don't show which step failed"
                likely_cause: "CleanupResult not recording step names with failures"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        function:      "CleanupResult::record_*"
                        what_to_check: "Are step names included in failure strings?"
                    },
                ]
                fix_pattern: "Format failures as 'step_name: error_message'"
            },
            {
                symptom:      "Error doesn't show orphaned paths"
                likely_cause: "Verification not adding paths to orphaned_paths vec"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        function:      "CleanupResult::into_result"
                        what_to_check: "Are remaining paths pushed to orphaned_paths?"
                    },
                ]
                fix_pattern: "Check each resource and add to vec if still exists"
            },
            {
                symptom:      "Cleanup succeeds but directory actually remains"
                likely_cause: "Checking wrong path or using relative path"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        function:      "CleanupResult::into_result"
                        what_to_check: "Is worktree_path the actual full path?"
                    },
                ]
                fix_pattern: "Use wt.path from get_worktree which is the resolved path"
            },
        ]

        debugging_commands: [
            {
                scenario: "When cleanup seems to succeed but files remain"
                run:      "ls -la .factory/ .factory-workspaces/"
                look_for: "Symlinks, directories that should be gone"
            },
            {
                scenario: "When error message is unclear"
                run:      "RUST_LOG=factory_core::worktree=debug factory remove -s slug"
                look_for: "Tracing output showing each cleanup step"
            },
            {
                scenario: "When jj/git commands fail unexpectedly"
                run:      "jj workspace list && git worktree list"
                look_for: "Whether workspace/worktree is actually registered"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_remove_worktree_all_fail_returns_error passes",
            "[ ] test_remove_worktree_symlink_remains_returns_error passes",
            "[ ] test_remove_worktree_directory_remains_returns_error passes",
            "[ ] test_cleanup_partial_success_rm_only passes",
            "[ ] test_worktree_cleanup_removes_all_resources e2e passes",
            "[ ] No mocks - tests use real filesystem/git",
        ]

        code: [
            "[ ] WorktreeCleanupFailed error variant added to error.rs",
            "[ ] CleanupResult struct tracks all operation results",
            "[ ] All `let _ = run_command` patterns removed",
            "[ ] Errors are collected not discarded",
            "[ ] Final verification checks filesystem state",
            "[ ] orphaned_paths included in error",
            "[ ] Zero unwrap() or expect() calls",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] rg 'let _ = run_command' returns empty",
        ]

        documentation: [
            "[ ] CleanupResult struct documented",
            "[ ] remove_worktree function docs updated",
            "[ ] WorktreeCleanupFailed error documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/worktree.rs"
                relevance: "THE FILE TO FIX - remove_worktree at lines 178-216"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Add WorktreeCleanupFailed error variant"
            },
            {
                path:      "crates/factory-core/src/process.rs"
                relevance: "run_command function used for cleanup operations"
            },
            {
                path:      "crates/factory/src/commands/remove.rs"
                relevance: "CLI that calls remove_worktree - may need error handling update"
            },
        ]

        external_references: [
            "https://github.com/martinvonz/jj/blob/main/docs/working-copy.md - jj workspace docs",
            "https://git-scm.com/docs/git-worktree - git worktree docs",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Collect errors into Vec, return aggregated error at end"
            },
            {
                pattern:          "Result-returning helpers"
                example_location: "crates/factory-core/src/worktree.rs:run_command_checked"
                how_to_apply:     "Each try_* helper returns Result, caller collects results"
            },
        ]

        buggy_code_analysis: {
            file:      "crates/factory-core/src/worktree.rs"
            lines:     "186-211"
            problem:   "Four `let _ = run_command(...)` patterns silently discard all errors"
            evidence: [
                "Line 186: `let _ = run_command(\"jj\", ...)` - discards jj failure",
                "Line 193: `let _ = run_command(\"git\", ...)` - discards git failure",
                "Line 207: `let _ = run_command(\"rm\", ...)` - discards rm failure",
                "Line 211: `let _ = run_command(\"rm\", ...)` - discards symlink rm failure",
                "Line 215: Always returns Ok(()) regardless of what failed",
            ]
            consequence: "If all four operations fail, worktree directory and symlink remain but function reports success"
        }
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Collect ALL errors before deciding success/failure",
            "Verify filesystem state after all cleanup attempts",
            "Include operation name in each error for debugging",
            "Check symlink with both exists() and symlink_metadata()",
            "Continue cleanup after individual failures (best effort)",
            "Return Ok only when critical resources are verified gone",
            "Log warnings for non-critical failures (jj vs git redundancy)",
        ]

        do_not: [
            "Do NOT use `let _ = ...` to discard Result values",
            "Do NOT return Ok(()) without verifying cleanup succeeded",
            "Do NOT stop cleanup on first failure",
            "Do NOT unwrap() or expect()",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
        ]

        code_patterns: [
            {
                name:     "Error Aggregation Pattern"
                use_when: "Multiple operations that can each fail"
                example:  """
                    struct CleanupResult {
                        failures: Vec<String>,
                    }

                    impl CleanupResult {
                        fn record(&mut self, step: &str, result: Result<CommandOutput>) {
                            match result {
                                Ok(output) if output.is_success() => {}
                                Ok(output) => {
                                    self.failures.push(format!("{step}: {}", output.stderr));
                                }
                                Err(e) => {
                                    self.failures.push(format!("{step}: {e}"));
                                }
                            }
                        }
                    }
                    """
            },
            {
                name:     "Verification After Cleanup"
                use_when: "Need to confirm resources are actually removed"
                example:  """
                    fn verify_removed(path: &Path) -> bool {
                        // exists() returns false for dangling symlinks
                        // symlink_metadata() succeeds for dangling symlinks
                        !path.exists() && path.symlink_metadata().is_err()
                    }
                    """
            },
            {
                name:     "Best Effort With Aggregated Error"
                use_when: "Multiple cleanup operations, continue on failure"
                example:  """
                    let mut cleanup = CleanupResult::new();

                    // Try all operations, don't short-circuit
                    cleanup.record("jj forget", try_jj_forget(slug, repo_root));
                    cleanup.record("git remove", try_git_remove(&wt.path, repo_root));
                    cleanup.record("rm directory", try_rm_rf(&wt.path, repo_root));
                    cleanup.record("rm symlink", try_rm_symlink(&symlink_path, repo_root));

                    // Only fail if resources remain
                    if !verify_removed(&wt.path) || !verify_removed(&symlink_path) {
                        return Err(Error::WorktreeCleanupFailed { ... });
                    }
                    Ok(())
                    """
            },
        ]

        the_bug_explained: """
            CURRENT CODE (lines 186-211):
            ```rust
            let _ = run_command("jj", &["workspace", "forget", ...], repo_root);
            let _ = run_command("git", &["worktree", "remove", ...], repo_root);
            let _ = run_command("rm", &["-rf", ...], repo_root);
            let _ = run_command("rm", &["-f", ...], repo_root);
            Ok(())  // Always returns Ok!
            ```

            PROBLEM: `let _ = expr` explicitly discards the Result. This is a
            Rust idiom to silence "unused Result" warnings, but here it means
            ALL cleanup failures are ignored.

            SCENARIO: User runs `factory remove -s my-task`
            - jj fails (not installed)
            - git fails (wasn't a git worktree)
            - rm -rf fails (permission denied)
            - rm symlink fails (permission denied)
            - Function returns Ok(())
            - User thinks cleanup succeeded
            - Worktree directory and symlink still exist
            - `factory list` still shows the task
            - Disk space wasted, state inconsistent

            FIX: Collect all errors, verify filesystem state, return error
            with details if anything remains.
            """
    }
}
