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

"factory-8cb": #ValidBead & {
    // ============================================================================
    // BEAD: factory-8cb - JJ-native workspace management (remove Git fallback)
    // ============================================================================

    id:              "factory-8cb"
    title:           "Remove Git fallback, implement JJ-native workspace management"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["jj", "workspace", "vcs", "P1", "breaking-change"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use JJ (jujutsu) exclusively for all version control operations",
            "THE SYSTEM SHALL use OperationId for undo/rollback capabilities",
            "THE SYSTEM SHALL use ChangeId as the primary commit identifier instead of Git SHA",
            "THE SYSTEM SHALL never fall back to Git commands for workspace management",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN a new task is created"
                shall:   "THE SYSTEM SHALL create an isolated JJ workspace via 'jj workspace add'"
            },
            {
                trigger: "WHEN workspace creation is requested"
                shall:   "THE SYSTEM SHALL capture the OperationId before the operation for rollback"
            },
            {
                trigger: "WHEN a task's workspace is removed"
                shall:   "THE SYSTEM SHALL use 'jj workspace forget' to clean up"
            },
            {
                trigger: "WHEN a user requests undo"
                shall:   "THE SYSTEM SHALL restore to the captured OperationId via 'jj op restore'"
            },
            {
                trigger: "WHEN a change is committed"
                shall:   "THE SYSTEM SHALL return and store the ChangeId for tracking"
            },
            {
                trigger: "WHEN listing workspaces"
                shall:   "THE SYSTEM SHALL use 'jj workspace list' for accurate state"
            },
        ]

        state_driven: [
            {
                state: "WHILE a workspace exists"
                shall: "THE SYSTEM SHALL track workspace name, path, and associated ChangeId"
            },
            {
                state: "WHILE the repository is in use"
                shall: "THE SYSTEM SHALL maintain a history of OperationIds for rollback chain"
            },
            {
                state: "WHILE a task is in progress"
                shall: "THE SYSTEM SHALL associate exactly one JJ workspace with each task"
            },
        ]

        unwanted: [
            {
                condition: "IF JJ is not installed or not available"
                shall_not: "THE SYSTEM SHALL NOT attempt Git fallback"
                because:   "JJ-only simplifies the codebase and ensures consistent behavior"
            },
            {
                condition: "IF 'git worktree' commands exist in codebase"
                shall_not: "THE SYSTEM SHALL NOT contain any Git worktree code paths"
                because:   "Git worktree code is dead code that increases maintenance burden"
            },
            {
                condition: "IF an operation fails"
                shall_not: "THE SYSTEM SHALL NOT leave partial state (orphaned workspaces, broken symlinks)"
                because:   "Partial state causes confusion and requires manual cleanup"
            },
            {
                condition: "IF OperationId is not captured before mutation"
                shall_not: "THE SYSTEM SHALL NOT perform workspace mutations"
                because:   "Missing OperationId prevents undo, violating recovery requirements"
            },
        ]

        complex: [
            {
                state:   "WHILE another operation is in progress"
                trigger: "WHEN a concurrent workspace operation is requested"
                shall:   "THE SYSTEM SHALL serialize operations or return Error::ConcurrentOperation"
            },
            {
                state:   "WHILE workspace cleanup is running"
                trigger: "WHEN a new task is created for the same slug"
                shall:   "THE SYSTEM SHALL wait for cleanup or return Error::WorkspaceCleanupInProgress"
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
                    field:           "JJ binary"
                    type:            "Executable"
                    constraints:     "jj must be in PATH and executable"
                    example_valid:   "which jj returns /usr/bin/jj"
                    example_invalid: "which jj returns empty"
                },
                {
                    field:           "JJ repository"
                    type:            "Directory"
                    constraints:     "Current directory must be inside a JJ repository"
                    example_valid:   ".jj directory exists at repo root"
                    example_invalid: "Only .git directory exists (pure Git repo)"
                },
            ]
            system_state: [
                "JJ installed and available in PATH",
                "Repository initialized with JJ (jj init or jj git init)",
                "Moon build system configured",
                "factory-core crate exists with process module",
            ]
        }

        postconditions: {
            state_changes: [
                "All Git worktree code removed from worktree.rs",
                "New module: crates/factory-core/src/jj.rs for JJ-native types",
                "Updated error.rs with JJ-specific error variants",
                "OperationId and ChangeId newtypes in domain.rs",
                "Workspace struct updated to use ChangeId instead of branch",
            ]
            return_guarantees: [
                {
                    field:     "Workspace::create()"
                    guarantee: "Returns Result<(Workspace, OperationId), Error>"
                },
                {
                    field:     "Workspace::remove()"
                    guarantee: "Returns Result<OperationId, Error> with cleanup operation ID"
                },
                {
                    field:     "OperationId"
                    guarantee: "Opaque validated type, exactly 40 hex characters"
                },
                {
                    field:     "ChangeId"
                    guarantee: "Opaque validated type, exactly 40 hex characters (reversed for display)"
                },
                {
                    field:     "jj::undo()"
                    guarantee: "Returns Result<(), Error> restoring to given OperationId"
                },
            ]
            side_effects: [
                "JJ workspace created on disk via 'jj workspace add'",
                "Symlink created in .factory/ directory pointing to workspace",
                "JJ operation log updated with new operations",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Zero Git fallback code paths remain",
            "Every workspace mutation captures OperationId before execution",
            "ChangeId and OperationId are validated newtypes (not raw strings)",
            "Symlinks in .factory/ always resolve to existing workspaces or are cleaned up",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "JJ not installed on target system"
                prevention:  "Check jj availability at startup, return clear error with install instructions"
                test_for_it: "test_jj_not_installed_returns_clear_error"
            },
            {
                failure:     "Repository is Git-only (no .jj directory)"
                prevention:  "Detect repo type at startup, return Error::JjRequired with migration hint"
                test_for_it: "test_git_only_repo_rejected"
            },
            {
                failure:     "JJ CLI output format changes between versions"
                prevention:  "Parse JSON output (--output json) instead of text, pin minimum JJ version"
                test_for_it: "test_jj_output_parsing_handles_version_differences"
            },
            {
                failure:     "Concurrent JJ operations corrupt repository"
                prevention:  "JJ handles this internally with operation log, but serialize at app level"
                test_for_it: "test_concurrent_workspace_operations"
            },
        ]

        usability_failures: [
            {
                failure:     "Users unfamiliar with JJ concepts (ChangeId vs commit hash)"
                prevention:  "Provide clear documentation, show both ChangeId and short form in output"
                test_for_it: "test_output_shows_human_readable_change_ids"
            },
            {
                failure:     "Undo operation restores too much (unexpected state)"
                prevention:  "Capture fine-grained OperationIds, show operation description before undo"
                test_for_it: "test_undo_operation_is_targeted"
            },
            {
                failure:     "Error messages reference removed Git concepts"
                prevention:  "Update all error messages to use JJ terminology"
                test_for_it: "test_error_messages_use_jj_terminology"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Workspace created but OperationId not captured (can't undo)"
                prevention:  "Capture OperationId BEFORE operation, atomic create-and-capture"
                test_for_it: "test_operation_id_always_captured"
            },
            {
                failure:     "Orphaned workspaces after failed cleanup"
                prevention:  "Use jj workspace list to verify cleanup, retry on failure"
                test_for_it: "test_cleanup_is_idempotent"
            },
            {
                failure:     "Symlinks point to non-existent workspaces"
                prevention:  "Validate symlinks on list, auto-cleanup stale links"
                test_for_it: "test_stale_symlinks_cleaned_on_list"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_create_workspace_returns_operation_id"
                given: "A JJ repository with factory initialized"
                when:  "create_workspace(slug, language) is called"
                then: [
                    "Returns Ok((Workspace, OperationId))",
                    "Workspace path exists on disk",
                    "jj workspace list includes the new workspace",
                    "Symlink in .factory/ points to workspace",
                ]
                real_input: """
                    let slug = Slug::new("my-feature")?;
                    let repo_root = detect_repo_root()?;
                    let (workspace, op_id) = jj::create_workspace(&slug, Language::Rust, &repo_root)?;
                    """
                expected_output: """
                    assert!(workspace.path.exists());
                    assert!(workspace.change_id.as_str().len() == 40);
                    assert!(op_id.as_str().len() == 40);
                    """
            },
            {
                name:  "test_remove_workspace_cleans_up"
                given: "An existing workspace for a task"
                when:  "remove_workspace(slug) is called"
                then: [
                    "Returns Ok(OperationId)",
                    "Workspace path no longer exists",
                    "jj workspace list excludes the workspace",
                    "Symlink in .factory/ is removed",
                ]
                real_input: """
                    let slug = Slug::new("my-feature")?;
                    let op_id = jj::remove_workspace(&slug, &repo_root)?;
                    """
                expected_output: """
                    assert!(!workspace.path.exists());
                    assert!(op_id.as_str().len() == 40);
                    """
            },
            {
                name:  "test_undo_restores_previous_state"
                given: "A captured OperationId from before an operation"
                when:  "jj::undo(op_id) is called"
                then: [
                    "Returns Ok(())",
                    "Repository state matches the captured operation",
                    "jj op log shows restore operation",
                ]
                real_input: """
                    let before_op = jj::current_operation_id(&repo_root)?;
                    let (workspace, _) = jj::create_workspace(&slug, Language::Rust, &repo_root)?;
                    jj::undo(&before_op, &repo_root)?;
                    """
                expected_output: """
                    let workspaces = jj::list_workspaces(&repo_root)?;
                    assert!(!workspaces.iter().any(|w| w.slug == slug));
                    """
            },
            {
                name:  "test_list_workspaces_uses_jj_workspace_list"
                given: "Multiple workspaces exist"
                when:  "list_workspaces() is called"
                then: [
                    "Returns accurate list from JJ",
                    "Each workspace has valid ChangeId",
                    "Stale symlinks are cleaned up",
                ]
                real_input: """
                    let workspaces = jj::list_workspaces(&repo_root)?;
                    """
                expected_output: """
                    for ws in &workspaces {
                        assert!(ws.path.exists());
                        assert!(ws.change_id.as_str().len() == 40);
                    }
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_jj_not_available_returns_error"
                given: "JJ is not installed"
                when:  "Any JJ operation is attempted"
                then: [
                    "Returns Err(Error::JjNotInstalled)",
                    "Error message includes installation instructions",
                    "No fallback to Git",
                ]
                real_input: """
                    // Simulate by temporarily modifying PATH
                    let result = jj::create_workspace(&slug, Language::Rust, &repo_root);
                    """
                expected_output: null
                expected_error: """
                    Err(Error::JjNotInstalled { hint: "Install with: cargo install jj-cli" })
                    """
            },
            {
                name:  "test_git_only_repo_rejected"
                given: "A Git repository without JJ"
                when:  "create_workspace is called"
                then: [
                    "Returns Err(Error::JjRequired)",
                    "Error message suggests jj git init",
                    "No Git worktree fallback",
                ]
                real_input: """
                    // In a pure Git repo without .jj
                    let result = jj::create_workspace(&slug, Language::Rust, &git_only_repo);
                    """
                expected_output: null
                expected_error: """
                    Err(Error::JjRequired { hint: "Initialize with: jj git init" })
                    """
            },
            {
                name:  "test_workspace_already_exists_error"
                given: "A workspace with the same slug exists"
                when:  "create_workspace is called with same slug"
                then: [
                    "Returns Err(Error::WorkspaceExists)",
                    "Existing workspace is not modified",
                ]
                real_input: """
                    let _ = jj::create_workspace(&slug, Language::Rust, &repo_root)?;
                    let result = jj::create_workspace(&slug, Language::Go, &repo_root);
                    """
                expected_output: null
                expected_error: """
                    Err(Error::WorkspaceExists { slug: "my-feature".to_string() })
                    """
            },
            {
                name:  "test_invalid_operation_id_rejected"
                given: "An invalid or non-existent OperationId"
                when:  "jj::undo(invalid_op_id) is called"
                then: [
                    "Returns Err(Error::InvalidOperationId)",
                    "Repository state is unchanged",
                ]
                real_input: """
                    let invalid = OperationId::new("0000000000000000000000000000000000000000")?;
                    let result = jj::undo(&invalid, &repo_root);
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InvalidOperationId { op_id: "0000000000...".to_string() })
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_workspace_name_collision_with_id_suffix"
                scenario: "Two workspaces created rapidly for same slug"
                input:    "create workspace 'task' twice in quick succession"
                expected: "Both get unique names via timestamp/nanos suffix"
            },
            {
                name:     "test_long_slug_workspace_name"
                scenario: "Slug at maximum length (50 chars)"
                input:    "slug = 'a'.repeat(50)"
                expected: "Workspace created with truncated or hashed name if needed"
            },
            {
                name:     "test_concurrent_operations_serialized"
                scenario: "Multiple threads try to create workspaces"
                input:    "spawn 10 threads, each creating a workspace"
                expected: "All succeed without corruption, proper OperationIds"
            },
            {
                name:     "test_undo_after_multiple_operations"
                scenario: "Several operations, then undo to specific point"
                input:    "create 3 workspaces, capture op_id after first, undo to that point"
                expected: "Only first workspace remains"
            },
        ]

        contract_tests: [
            {
                name:     "test_no_git_worktree_code"
                verifies: "All Git worktree code removed"
                test:     "rg 'git.*worktree|worktree.*add' crates/ returns empty"
            },
            {
                name:     "test_no_git_fallback_logic"
                verifies: "No Git fallback patterns remain"
                test:     "rg 'git.*fallback|fall.*back.*git' crates/ returns empty"
            },
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in codebase"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/jj.rs returns empty"
            },
            {
                name:     "test_all_operations_return_operation_id"
                verifies: "Mutating operations return OperationId"
                test:     "All create/remove/modify functions signature includes OperationId"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_jj_workspace_lifecycle"
            description: "Complete workspace lifecycle: create -> use -> remove -> undo"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/jj_e2e.rs"
                        content: """
                            use factory_core::jj::{self, OperationId, ChangeId};
                            use factory_core::domain::{Slug, Language};
                            use factory_core::error::Result;

                            #[test]
                            fn test_full_jj_workspace_lifecycle() -> Result<()> {
                                // Test implemented inline
                                Ok(())
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "which jj",
                    "jj root",
                    "moon run factory-core:build",
                ]
            }

            execute: {
                command:    "moon run factory-core:test -- --test jj_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_jj_workspace_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/jj.rs"
                        contains: "pub fn create_workspace"
                    },
                    {
                        path:     "crates/factory-core/src/jj.rs"
                        contains: "pub struct OperationId"
                    },
                    {
                        path:     "crates/factory-core/src/jj.rs"
                        contains: "pub struct ChangeId"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "jj workspace forget --all-but-default",
                    "rm -f .factory/*",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_create_task_with_jj_workspace"
                description: "Full factory new -s slug flow with JJ"
                steps: [
                    {action: "Run factory new -s my-task", verify: "Workspace created"},
                    {action: "Check jj workspace list", verify: "my-task-* appears"},
                    {action: "Check .factory/my-task symlink", verify: "Points to workspace"},
                    {action: "Make changes in workspace", verify: "jj status shows changes"},
                    {action: "Run factory approve -s my-task", verify: "ChangeId recorded"},
                ]
            },
            {
                name:        "e2e_undo_workspace_creation"
                description: "Create workspace then undo"
                steps: [
                    {action: "Capture current OperationId", verify: "op_id stored"},
                    {action: "Create workspace for task", verify: "Workspace exists"},
                    {action: "Run jj::undo(op_id)", verify: "Returns Ok(())"},
                    {action: "Check workspace", verify: "Workspace no longer exists"},
                    {action: "Check symlink", verify: "Symlink cleaned up"},
                ]
            },
            {
                name:        "e2e_recovery_from_failed_operation"
                description: "Partial failure during workspace creation"
                steps: [
                    {action: "Capture OperationId before", verify: "op_id stored"},
                    {action: "Simulate partial failure (disk full etc)", verify: "Error returned"},
                    {action: "Run jj::undo(op_id)", verify: "Clean state restored"},
                    {action: "Retry operation", verify: "Succeeds normally"},
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
                task:      "Write test: test_create_workspace_returns_operation_id"
                file:      "crates/factory-core/src/jj.rs"
                what:      "Test that workspace creation returns (Workspace, OperationId)"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_remove_workspace_cleans_up"
                file:      "crates/factory-core/src/jj.rs"
                what:      "Test that removal returns OperationId and cleans everything"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_undo_restores_previous_state"
                file:      "crates/factory-core/src/jj.rs"
                what:      "Test that undo via OperationId works"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_jj_not_available_returns_error"
                file:      "crates/factory-core/src/jj.rs"
                what:      "Test clear error when JJ not installed"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_no_git_worktree_code"
                file:      "crates/factory-core/tests/regression.rs"
                what:      "Test that no Git worktree code remains"
                done_when: "Test exists and FAILS while Git code present"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add OperationId and ChangeId newtypes to domain.rs"
                file: "crates/factory-core/src/domain.rs"
                what: """
                    /// JJ OperationId - 40 hex chars, used for undo.
                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
                    #[serde(try_from = "String", into = "String")]
                    pub struct OperationId(String);

                    impl OperationId {
                        pub fn new(s: impl Into<String>) -> Result<Self> {
                            let s = s.into();
                            if s.len() != 40 || !s.chars().all(|c| c.is_ascii_hexdigit()) {
                                return Err(Error::InvalidOperationId { op_id: s });
                            }
                            Ok(Self(s.to_lowercase()))
                        }

                        #[must_use]
                        pub fn as_str(&self) -> &str { &self.0 }
                    }

                    /// JJ ChangeId - 40 hex chars, identifies a change.
                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
                    #[serde(try_from = "String", into = "String")]
                    pub struct ChangeId(String);

                    impl ChangeId {
                        pub fn new(s: impl Into<String>) -> Result<Self> {
                            let s = s.into();
                            if s.len() != 40 || !s.chars().all(|c| c.is_ascii_hexdigit()) {
                                return Err(Error::InvalidChangeId { change_id: s });
                            }
                            Ok(Self(s.to_lowercase()))
                        }

                        #[must_use]
                        pub fn as_str(&self) -> &str { &self.0 }

                        /// Display short form (first 12 chars, reversed for JJ convention).
                        #[must_use]
                        pub fn short(&self) -> &str { &self.0[..12] }
                    }
                    """
                done_when:     "Types compile with TryFrom/Into/Display impls"
                patterns_to_use: ["Newtype validation pattern from Slug/GitHash"]
            },
            {
                task: "Add JJ-specific error variants to error.rs"
                file: "crates/factory-core/src/error.rs"
                what: """
                    // JJ-specific errors
                    #[error("JJ not installed (install with: cargo install jj-cli)")]
                    JjNotInstalled,

                    #[error("JJ repository required (initialize with: jj git init)")]
                    JjRequired,

                    #[error("invalid operation ID: {op_id}")]
                    InvalidOperationId { op_id: String },

                    #[error("invalid change ID: {change_id}")]
                    InvalidChangeId { change_id: String },

                    #[error("JJ operation failed: {reason}")]
                    JjOperationFailed { reason: String },

                    #[error("workspace cleanup in progress for: {slug}")]
                    WorkspaceCleanupInProgress { slug: String },

                    #[error("concurrent operation detected, retry")]
                    ConcurrentOperation,
                    """
                done_when:     "Error variants compile and are documented"
                patterns_to_use: ["thiserror derive pattern"]
            },
            {
                task: "Create jj.rs module with JJ CLI integration"
                file: "crates/factory-core/src/jj.rs"
                what: """
                    //! JJ (Jujutsu) native workspace management.
                    //!
                    //! Provides JJ-native APIs for workspace isolation with OperationId-based undo.

                    use std::path::{Path, PathBuf};
                    use crate::{
                        domain::{ChangeId, Language, OperationId, Slug},
                        error::{Error, Result},
                        process::run_command,
                    };

                    /// Workspace information with JJ-native types.
                    #[derive(Debug, Clone)]
                    pub struct Workspace {
                        pub slug: String,
                        pub path: PathBuf,
                        pub name: String,
                        pub change_id: ChangeId,
                        pub language: Language,
                    }

                    /// Check if JJ is available.
                    pub fn check_jj_available() -> Result<()> {
                        run_command("jj", &["--version"], Path::new("."))
                            .map_err(|_| Error::JjNotInstalled)?
                            .check_success()
                            .map_err(|_| Error::JjNotInstalled)?;
                        Ok(())
                    }

                    /// Check if we're in a JJ repository.
                    pub fn check_jj_repo(repo_root: &Path) -> Result<()> {
                        let jj_dir = repo_root.join(".jj");
                        if !jj_dir.exists() {
                            return Err(Error::JjRequired);
                        }
                        Ok(())
                    }

                    /// Get the current operation ID (for undo).
                    pub fn current_operation_id(repo_root: &Path) -> Result<OperationId> {
                        let output = run_command(
                            "jj", &["op", "log", "-n1", "--no-graph", "-T", "self.id()"],
                            repo_root
                        )?;
                        output.check_success().map_err(|e| Error::JjOperationFailed {
                            reason: e.to_string()
                        })?;
                        OperationId::new(output.stdout.trim())
                    }

                    /// Create a workspace, returning workspace info and operation ID.
                    pub fn create_workspace(
                        slug: &Slug,
                        language: Language,
                        repo_root: &Path,
                    ) -> Result<(Workspace, OperationId)> {
                        check_jj_available()?;
                        check_jj_repo(repo_root)?;
                        check_slug_not_exists(slug.as_str(), repo_root)?;

                        let before_op = current_operation_id(repo_root)?;

                        let workspaces_base = repo_root.join(".factory-workspaces");
                        let unique_id = generate_unique_id();
                        let workspace_name = format!("{}-{}", slug.as_str(), unique_id);
                        let workspace_path = workspaces_base.join(&workspace_name);

                        // Create base directory
                        std::fs::create_dir_all(&workspaces_base)?;

                        // Create JJ workspace
                        let output = run_command(
                            "jj",
                            &["workspace", "add", "--name", &workspace_name,
                              &workspace_path.to_string_lossy()],
                            repo_root,
                        )?;
                        output.check_success().map_err(|_| Error::WorktreeCreationFailed {
                            reason: "jj workspace add failed".into()
                        })?;

                        // Get the change ID for the new workspace
                        let change_id = get_workspace_change_id(&workspace_path)?;

                        // Create symlink
                        let factory_dir = repo_root.join(".factory");
                        std::fs::create_dir_all(&factory_dir)?;
                        let symlink_path = factory_dir.join(slug.as_str());
                        std::os::unix::fs::symlink(&workspace_path, &symlink_path)?;

                        let workspace = Workspace {
                            slug: slug.as_str().to_string(),
                            path: workspace_path,
                            name: workspace_name,
                            change_id,
                            language,
                        };

                        Ok((workspace, before_op))
                    }

                    /// Undo to a previous operation state.
                    pub fn undo(op_id: &OperationId, repo_root: &Path) -> Result<()> {
                        let output = run_command(
                            "jj",
                            &["op", "restore", op_id.as_str()],
                            repo_root,
                        )?;
                        output.check_success().map_err(|_| Error::InvalidOperationId {
                            op_id: op_id.as_str().to_string()
                        })?;
                        Ok(())
                    }
                    """
                done_when:     "Module compiles and exports public API"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "run_command from process module",
                    "Capture OperationId BEFORE mutation",
                ]
            },
            {
                task: "Remove Git fallback from worktree.rs"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    Remove all code paths that call:
                    - git worktree add
                    - git worktree remove
                    - git branch create fallback

                    Update create_jj_workspace to return Error instead of falling back.
                    """
                done_when:     "rg 'git.*worktree' crates/ returns empty"
                patterns_to_use: ["Return Error::JjRequired instead of fallback"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export jj module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod jj;"
                done_when: "External crates can import factory_core::jj"
            },
            {
                task:      "Update Task struct to use ChangeId"
                file:      "crates/factory-core/src/domain.rs"
                what:      "Replace branch: String with change_id: ChangeId in Task"
                done_when: "Task uses JJ-native identifier"
            },
            {
                task:      "Update CLI to use jj module"
                file:      "crates/factory/src/cli.rs"
                what:      "Import jj module, update factory new command"
                done_when: "factory new creates JJ workspace via new module"
            },
            {
                task:      "Add workspace undo subcommand"
                file:      "crates/factory/src/cli.rs"
                what:      "factory undo -s <slug> restores to pre-creation state"
                done_when: "Undo command works with stored OperationId"
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
                task:     "Verify no Git worktree code"
                done_when: "rg finds no git worktree references"
                commands: ["rg 'git.*worktree|worktree.*add.*-b' crates/"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify no unwraps"
                done_when: "rg finds no unwrap/expect"
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/jj.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification with JJ repo"
                done_when: "Full lifecycle works"
                commands: [
                    "factory new -s test-jj",
                    "jj workspace list",
                    "ls -la .factory/test-jj",
                    "factory show -s test-jj",
                ]
                expected: "Workspace created with JJ, symlink valid, show displays ChangeId"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Error: JJ not installed"
                likely_cause: "JJ binary not in PATH"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/jj.rs"
                        function:      "check_jj_available()"
                        what_to_check: "Is the PATH correct? Is jj executable?"
                    },
                ]
                fix_pattern: "Install JJ: cargo install jj-cli"
            },
            {
                symptom:      "Error: JJ repository required"
                likely_cause: "Repository not initialized with JJ"
                where_to_look: [
                    {
                        file:          "Repository root"
                        what_to_check: "Does .jj directory exist?"
                    },
                ]
                fix_pattern: "Initialize: jj git init (for existing git repo) or jj init"
            },
            {
                symptom:      "Error: invalid operation ID"
                likely_cause: "OperationId doesn't exist or was garbage collected"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/jj.rs"
                        function:      "undo()"
                        what_to_check: "Is the OperationId still in jj op log?"
                    },
                ]
                fix_pattern: "Check jj op log, use a more recent OperationId"
            },
            {
                symptom:      "Workspace created but undo fails"
                likely_cause: "OperationId not captured before mutation"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/jj.rs"
                        function:      "create_workspace()"
                        what_to_check: "Is current_operation_id called BEFORE jj workspace add?"
                    },
                ]
                fix_pattern: "Ensure OperationId captured before any mutating operation"
            },
            {
                symptom:      "Compilation error: 'git worktree' reference"
                likely_cause: "Git fallback code not fully removed"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        what_to_check: "Are there any remaining git worktree calls?"
                    },
                ]
                fix_pattern: "Remove all git worktree code paths"
            },
            {
                symptom:      "Test fails with 'JJ not installed' in CI"
                likely_cause: "CI environment doesn't have JJ"
                where_to_look: [
                    {
                        file:          ".github/workflows/*.yml or .moon/tasks.yml"
                        what_to_check: "Is JJ installed in CI setup?"
                    },
                ]
                fix_pattern: "Add 'cargo install jj-cli' to CI setup"
            },
        ]

        debugging_commands: [
            {
                scenario: "When workspace creation fails"
                run:      "jj workspace list && jj op log -n5"
                look_for: "Existing workspaces, recent operations"
            },
            {
                scenario: "When undo doesn't work as expected"
                run:      "jj op log --no-graph -T 'self.id() ++ \" \" ++ description ++ \"\\n\"'"
                look_for: "Operation descriptions, find correct OperationId"
            },
            {
                scenario: "When symlinks are stale"
                run:      "ls -la .factory/ && jj workspace list"
                look_for: "Symlinks pointing to non-existent paths"
            },
            {
                scenario: "When JJ commands fail silently"
                run:      "JJ_LOG=debug jj workspace add test /tmp/test 2>&1"
                look_for: "Detailed error messages from JJ"
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
            "[ ] E2E lifecycle test passing",
            "[ ] test_create_workspace_returns_operation_id passes",
            "[ ] test_remove_workspace_cleans_up passes",
            "[ ] test_undo_restores_previous_state passes",
            "[ ] test_jj_not_available_returns_error passes",
            "[ ] test_no_git_worktree_code passes (regression)",
        ]

        code: [
            "[ ] OperationId newtype implemented in domain.rs",
            "[ ] ChangeId newtype implemented in domain.rs",
            "[ ] JJ-specific errors added to error.rs",
            "[ ] jj.rs module created with full API",
            "[ ] All Git worktree code removed from worktree.rs",
            "[ ] Task struct uses ChangeId instead of branch",
            "[ ] CLI updated to use jj module",
            "[ ] Zero unwrap() or expect() calls in new code",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] JJ installed in CI environment",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in jj.rs",
            "[ ] Doc comments on OperationId and ChangeId",
            "[ ] Example usage in doc comments",
            "[ ] Updated CLAUDE.md if JJ requirements added",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/worktree.rs"
                relevance: "Current implementation with Git fallback - to be refactored"
            },
            {
                path:      "crates/factory-core/src/repo.rs"
                relevance: "Repository detection - may need JJ detection added"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Add OperationId and ChangeId newtypes here"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Add JJ-specific error variants"
            },
            {
                path:      "crates/factory-core/src/process.rs"
                relevance: "Use run_command for JJ CLI calls"
            },
            {
                path:      "crates/factory/src/cli.rs"
                relevance: "Update CLI to use new jj module"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-goi"
                title:       "Fix worktree cleanup race condition"
                why_depends: "Must fix cleanup issues before redesigning workspace management"
            },
        ]

        external_references: [
            "https://github.com/martinvonz/jj - JJ repository",
            "https://martinvonz.github.io/jj/latest/ - JJ documentation",
            "https://martinvonz.github.io/jj/latest/working-copy/ - JJ workspace concepts",
            "https://martinvonz.github.io/jj/latest/operation-log/ - OperationId and undo",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Newtype Validation"
                example_location: "crates/factory-core/src/domain.rs:Slug, GitHash"
                how_to_apply:     "OperationId and ChangeId follow same pattern"
            },
            {
                pattern:          "Process Execution"
                example_location: "crates/factory-core/src/process.rs:run_command"
                how_to_apply:     "Use run_command for all JJ CLI invocations"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use 'jj workspace add --name NAME PATH' for workspace creation",
            "Use 'jj workspace forget NAME' for cleanup",
            "Use 'jj op log -n1 --no-graph -T self.id()' to get OperationId",
            "Use 'jj op restore OP_ID' to undo operations",
            "Capture OperationId BEFORE any mutating operation",
            "Parse JSON output where available: --output json flag",
            "Follow existing newtype pattern for OperationId/ChangeId",
            "Test with real JJ repository in integration tests",
            "Add JJ to CI setup before running tests",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT fall back to Git for any operation",
            "Do NOT leave Git worktree code in codebase",
            "Do NOT hardcode OperationId/ChangeId lengths (use 40 constant)",
            "Do NOT assume JJ is installed - check first",
            "Do NOT mutate repo state without capturing OperationId first",
        ]

        code_patterns: [
            {
                name:     "OperationId Capture Pattern"
                use_when: "Before any mutating JJ operation"
                example:  """
                    pub fn create_workspace(...) -> Result<(Workspace, OperationId)> {
                        // ALWAYS capture before mutation
                        let before_op = current_operation_id(repo_root)?;

                        // Now perform mutation
                        run_command("jj", &["workspace", "add", ...], repo_root)?;

                        // Return the BEFORE operation ID for undo
                        Ok((workspace, before_op))
                    }
                    """
            },
            {
                name:     "JJ Command Execution"
                use_when: "Running any JJ CLI command"
                example:  """
                    fn run_jj(args: &[&str], repo_root: &Path) -> Result<String> {
                        let output = run_command("jj", args, repo_root)?;
                        output.check_success().map_err(|_| Error::JjOperationFailed {
                            reason: format!("jj {} failed: {}", args.join(" "), output.stderr)
                        })?;
                        Ok(output.stdout)
                    }
                    """
            },
            {
                name:     "JJ Availability Check"
                use_when: "At start of any public JJ function"
                example:  """
                    pub fn create_workspace(...) -> Result<...> {
                        check_jj_available()?;  // Fail fast with clear error
                        check_jj_repo(repo_root)?;  // Ensure .jj exists
                        // ... rest of function
                    }
                    """
            },
            {
                name:     "Workspace Cleanup"
                use_when: "Removing a workspace"
                example:  """
                    pub fn remove_workspace(slug: &Slug, repo_root: &Path) -> Result<OperationId> {
                        let before_op = current_operation_id(repo_root)?;

                        let workspace_name = find_workspace_name(slug, repo_root)?;

                        // Forget workspace in JJ
                        run_command("jj", &["workspace", "forget", &workspace_name], repo_root)?
                            .check_success()?;

                        // Remove symlink
                        let symlink = repo_root.join(".factory").join(slug.as_str());
                        if symlink.exists() {
                            std::fs::remove_file(&symlink)?;
                        }

                        Ok(before_op)
                    }
                    """
            },
        ]

        jj_specific_notes: [
            "JJ uses ChangeId (content-addressable) not Git commits",
            "OperationId tracks repo mutations, enabling surgical undo",
            "Workspaces are lightweight - create freely",
            "jj workspace list shows all workspaces",
            "jj op log shows operation history",
            "jj op restore OP_ID is surgical undo (not like git reset)",
            "JJ colocated repos have both .git and .jj",
        ]
    }
}
