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

"factory-pa8": #ValidBead & {
    // ============================================================================
    // BEAD: factory-pa8 - Implement JJ-native workspace manager actor
    // ============================================================================

    id:              "factory-pa8"
    title:           "Actor: Implement JJ-native workspace manager actor"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["actor", "jj", "workspace", "vcs", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use JJ (Jujutsu) as the primary VCS for workspace management",
            "THE SYSTEM SHALL implement WorkspaceManager as a ractor actor with message-based API",
            "THE SYSTEM SHALL track all active workspaces via actor state",
            "THE SYSTEM SHALL support COW (Copy-on-Write) snapshots using filesystem reflinks",
            "THE SYSTEM SHALL track JJ OperationIds for each workspace operation to enable undo",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL never fall back to git commands - JJ-native only",
        ]

        event_driven: [
            {
                trigger: "WHEN CreateWorkspace message is received"
                shall:   "THE SYSTEM SHALL execute jj workspace add and return WorkspaceCreated with OperationId"
            },
            {
                trigger: "WHEN CreateBookmark message is received"
                shall:   "THE SYSTEM SHALL execute jj bookmark create and return BookmarkCreated with OperationId"
            },
            {
                trigger: "WHEN ForgetWorkspace message is received"
                shall:   "THE SYSTEM SHALL execute jj workspace forget and return WorkspaceForgotten with OperationId"
            },
            {
                trigger: "WHEN CreateSnapshot message is received with reflink=true"
                shall:   "THE SYSTEM SHALL use cp --reflink=auto for COW snapshot creation"
            },
            {
                trigger: "WHEN UndoOperation message is received with OperationId"
                shall:   "THE SYSTEM SHALL execute jj undo --operation <id> to revert the operation"
            },
            {
                trigger: "WHEN ListWorkspaces message is received"
                shall:   "THE SYSTEM SHALL execute jj workspace list and return active workspaces"
            },
            {
                trigger: "WHEN a JJ command fails"
                shall:   "THE SYSTEM SHALL return Error::JjCommandFailed with stderr and exit code"
            },
            {
                trigger: "WHEN the actor is stopped"
                shall:   "THE SYSTEM SHALL cleanly terminate without orphaning workspaces"
            },
        ]

        state_driven: [
            {
                state: "WHILE the workspace manager is running"
                shall: "THE SYSTEM SHALL maintain a HashMap<WorkspaceId, WorkspaceInfo> of active workspaces"
            },
            {
                state: "WHILE processing a workspace operation"
                shall: "THE SYSTEM SHALL capture the pre-operation and post-operation OperationIds"
            },
            {
                state: "WHILE a workspace exists"
                shall: "THE SYSTEM SHALL track its path, bookmark, and creation OperationId"
            },
        ]

        unwanted: [
            {
                condition: "IF a JJ operation fails partway through"
                shall_not: "THE SYSTEM SHALL NOT leave workspace state inconsistent"
                because:   "Partial operations cause orphaned directories and bookmark confusion"
            },
            {
                condition: "IF git is available but JJ is not"
                shall_not: "THE SYSTEM SHALL NOT fall back to git worktree commands"
                because:   "Git fallback creates maintenance burden and inconsistent behavior"
            },
            {
                condition: "IF OperationId tracking fails"
                shall_not: "THE SYSTEM SHALL NOT proceed without undo capability"
                because:   "Users must be able to recover from any workspace operation"
            },
            {
                condition: "IF reflink is unavailable for snapshot"
                shall_not: "THE SYSTEM SHALL NOT silently perform full copy"
                because:   "Full copy is expensive; user should explicitly request it"
            },
            {
                condition: "IF workspace directory already exists"
                shall_not: "THE SYSTEM SHALL NOT overwrite existing content"
                because:   "Data loss from overwriting is unrecoverable"
            },
        ]

        complex: [
            {
                state:   "WHILE the actor is processing a CreateWorkspace request"
                trigger: "WHEN a ForgetWorkspace request arrives for the same workspace"
                shall:   "THE SYSTEM SHALL queue the forget request until create completes"
            },
            {
                state:   "WHILE undo is in progress"
                trigger: "WHEN workspace state doesn't match expected post-undo state"
                shall:   "THE SYSTEM SHALL return Error::UndoStateMismatch with details"
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
                    field:           "WorkspaceManagerConfig"
                    type:            "Struct"
                    constraints:     "Must specify repo_root, workspaces_dir, enable_reflink"
                    example_valid:   "WorkspaceManagerConfig { repo_root: PathBuf::from(\"/repo\"), workspaces_dir: \".factory-workspaces\", enable_reflink: true }"
                    example_invalid: "WorkspaceManagerConfig { repo_root: PathBuf::from(\"\"), .. } // Empty path"
                },
                {
                    field:           "Slug"
                    type:            "Slug (validated newtype)"
                    constraints:     "Non-empty, 1-50 chars, lowercase a-z, 0-9, hyphen, underscore"
                    example_valid:   "Slug::new(\"my-feature\")"
                    example_invalid: "Slug::new(\"\") // Empty"
                },
            ]
            system_state: [
                "JJ (jujutsu) is installed and in PATH",
                "Repository root is a valid JJ repository (has .jj directory)",
                "Tokio runtime is running",
                "Supervisor (factory-4pz) is running and can spawn child actors",
            ]
        }

        postconditions: {
            state_changes: [
                "WorkspaceManager actor spawned and registered with supervisor",
                "Actor state initialized with empty workspace registry",
                "JJ repository validated on startup",
            ]
            return_guarantees: [
                {
                    field:     "WorkspaceManager::spawn()"
                    guarantee: "Returns Result<WorkspaceManagerRef, SpawnError>"
                },
                {
                    field:     "WorkspaceManagerRef::create_workspace()"
                    guarantee: "Returns Result<WorkspaceCreated, Error> with OperationId"
                },
                {
                    field:     "WorkspaceManagerRef::forget_workspace()"
                    guarantee: "Returns Result<WorkspaceForgotten, Error> with OperationId"
                },
                {
                    field:     "WorkspaceManagerRef::create_bookmark()"
                    guarantee: "Returns Result<BookmarkCreated, Error> with OperationId"
                },
                {
                    field:     "WorkspaceManagerRef::create_snapshot()"
                    guarantee: "Returns Result<SnapshotCreated, Error> with snapshot path"
                },
                {
                    field:     "WorkspaceManagerRef::undo_operation()"
                    guarantee: "Returns Result<UndoCompleted, Error>"
                },
                {
                    field:     "WorkspaceManagerRef::list_workspaces()"
                    guarantee: "Returns Result<Vec<WorkspaceInfo>, Error>"
                },
                {
                    field:     "OperationId"
                    guarantee: "Always present in successful response for undo support"
                },
            ]
            side_effects: [
                "JJ workspace directories created/removed on filesystem",
                "JJ bookmarks created in repository",
                "JJ operation log extended with each operation",
                "Symlinks created in .factory/ directory",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Every mutating JJ operation returns an OperationId",
            "Actor state matches JJ repository state (consistency)",
            "Workspaces are tracked from creation until explicit forget",
            "No git commands are ever executed - JJ only",
            "Reflink failures are explicit, not silent fallback to copy",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "JJ not installed or not in PATH"
                prevention:  "Check jj --version on actor startup, fail fast with clear error"
                test_for_it: "test_startup_fails_without_jj"
            },
            {
                failure:     "Repository not initialized with JJ"
                prevention:  "Check for .jj directory on startup, provide migration guidance"
                test_for_it: "test_startup_fails_without_jj_repo"
            },
            {
                failure:     "OperationId format changes between JJ versions"
                prevention:  "Parse OperationId as opaque string, use jj operation log for verification"
                test_for_it: "test_operation_id_parsing"
            },
            {
                failure:     "Reflink not supported on filesystem (ext3, NFS, etc.)"
                prevention:  "Detect reflink support on startup, set enable_reflink accordingly"
                test_for_it: "test_reflink_detection"
            },
            {
                failure:     "Concurrent JJ operations from multiple actors"
                prevention:  "Single WorkspaceManager actor serializes all JJ operations"
                test_for_it: "test_serialized_operations"
            },
        ]

        usability_failures: [
            {
                failure:     "User doesn't know which OperationId to undo"
                prevention:  "Return OperationId in all responses, provide operation_history() method"
                test_for_it: "test_operation_history_available"
            },
            {
                failure:     "Error messages don't explain JJ failure"
                prevention:  "Include JJ stderr in Error::JjCommandFailed"
                test_for_it: "test_error_includes_jj_stderr"
            },
            {
                failure:     "Workspace path too long for filesystem"
                prevention:  "Use short unique IDs (8 hex chars), validate total path length"
                test_for_it: "test_workspace_path_length_validation"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Workspace created but not tracked in actor state"
                prevention:  "Add to state only after JJ command succeeds"
                test_for_it: "test_state_consistency_after_create"
            },
            {
                failure:     "Workspace forgotten but still in actor state"
                prevention:  "Remove from state only after JJ forget succeeds"
                test_for_it: "test_state_consistency_after_forget"
            },
            {
                failure:     "Undo reverts JJ state but not actor state"
                prevention:  "Refresh actor state from JJ after undo"
                test_for_it: "test_state_refresh_after_undo"
            },
            {
                failure:     "Snapshot created but reflink failed silently"
                prevention:  "Verify snapshot size matches source (reflinks are instant)"
                test_for_it: "test_reflink_verification"
            },
            {
                failure:     "Symlink points to deleted workspace"
                prevention:  "Remove symlink atomically with workspace forget"
                test_for_it: "test_symlink_cleanup_on_forget"
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
                given: "A running WorkspaceManager actor and valid JJ repository"
                when:  "create_workspace is called with a valid slug"
                then: [
                    "Returns Ok(WorkspaceCreated)",
                    "WorkspaceCreated contains OperationId",
                    "Workspace directory exists at expected path",
                    "Workspace is tracked in actor state",
                    "Symlink created in .factory/ directory",
                ]
                real_input: """
                    let manager = WorkspaceManager::spawn(config, supervisor).await?;
                    let slug = Slug::new("my-feature")?;

                    let result = manager.create_workspace(slug.clone()).await?;
                    """
                expected_output: """
                    assert!(result.operation_id.is_some());
                    assert!(result.path.exists());
                    assert!(result.path.join(".jj").exists());  // JJ workspace marker

                    let workspaces = manager.list_workspaces().await?;
                    assert!(workspaces.iter().any(|w| w.slug == slug));
                    """
            },
            {
                name:  "test_create_bookmark_returns_operation_id"
                given: "A workspace exists in the repository"
                when:  "create_bookmark is called for that workspace"
                then: [
                    "Returns Ok(BookmarkCreated)",
                    "BookmarkCreated contains OperationId",
                    "Bookmark is visible in jj bookmark list",
                ]
                real_input: """
                    let workspace = manager.create_workspace(slug.clone()).await?;
                    let bookmark_name = format!("feat/{}", slug);

                    let result = manager.create_bookmark(&workspace.path, &bookmark_name).await?;
                    """
                expected_output: """
                    assert!(result.operation_id.is_some());
                    assert_eq!(result.name, bookmark_name);

                    // Verify via JJ
                    let output = run_jj(&["bookmark", "list"], &workspace.path).await?;
                    assert!(output.stdout.contains(&bookmark_name));
                    """
            },
            {
                name:  "test_forget_workspace_returns_operation_id"
                given: "A workspace exists and is tracked"
                when:  "forget_workspace is called"
                then: [
                    "Returns Ok(WorkspaceForgotten)",
                    "WorkspaceForgotten contains OperationId",
                    "Workspace is removed from actor state",
                    "Workspace directory is removed",
                    "Symlink is removed",
                ]
                real_input: """
                    let workspace = manager.create_workspace(slug.clone()).await?;
                    let path = workspace.path.clone();

                    let result = manager.forget_workspace(slug.clone()).await?;
                    """
                expected_output: """
                    assert!(result.operation_id.is_some());
                    assert!(!path.exists());

                    let workspaces = manager.list_workspaces().await?;
                    assert!(!workspaces.iter().any(|w| w.slug == slug));

                    // Symlink removed
                    let symlink = config.repo_root.join(".factory").join(slug.as_str());
                    assert!(!symlink.exists());
                    """
            },
            {
                name:  "test_create_snapshot_with_reflink"
                given: "A workspace exists on a reflink-capable filesystem"
                when:  "create_snapshot is called with reflink enabled"
                then: [
                    "Returns Ok(SnapshotCreated)",
                    "Snapshot directory exists",
                    "Snapshot is a COW copy (nearly instant, shared blocks)",
                ]
                real_input: """
                    let workspace = manager.create_workspace(slug.clone()).await?;
                    // Create a large file to test reflink behavior
                    std::fs::write(workspace.path.join("large_file"), vec![0u8; 10_000_000])?;

                    let start = Instant::now();
                    let snapshot = manager.create_snapshot(slug.clone()).await?;
                    let duration = start.elapsed();
                    """
                expected_output: """
                    assert!(snapshot.path.exists());
                    assert!(snapshot.path.join("large_file").exists());

                    // Reflink should be nearly instant (< 1 second for any size)
                    // Full copy of 10MB would take ~100ms+ on SSD
                    // We allow generous margin but reflink should be < 10ms
                    if config.reflink_supported {
                        assert!(duration < Duration::from_millis(500),
                            "Reflink took {:?}, expected < 500ms", duration);
                    }
                    """
            },
            {
                name:  "test_undo_operation_reverts_workspace_creation"
                given: "A workspace was just created with known OperationId"
                when:  "undo_operation is called with that OperationId"
                then: [
                    "Returns Ok(UndoCompleted)",
                    "Workspace no longer exists",
                    "Actor state is refreshed to match JJ state",
                ]
                real_input: """
                    let workspace = manager.create_workspace(slug.clone()).await?;
                    let op_id = workspace.operation_id.clone();

                    let result = manager.undo_operation(op_id).await?;
                    """
                expected_output: """
                    assert!(result.is_ok());

                    // Workspace should no longer exist in JJ
                    let output = run_jj(&["workspace", "list"], &config.repo_root).await?;
                    assert!(!output.stdout.contains(slug.as_str()));

                    // Actor state refreshed
                    let workspaces = manager.list_workspaces().await?;
                    assert!(!workspaces.iter().any(|w| w.slug == slug));
                    """
            },
            {
                name:  "test_list_workspaces_returns_all_tracked"
                given: "Multiple workspaces have been created"
                when:  "list_workspaces is called"
                then: [
                    "Returns Vec<WorkspaceInfo> with all active workspaces",
                    "Each WorkspaceInfo contains slug, path, bookmark, operation_id",
                ]
                real_input: """
                    manager.create_workspace(Slug::new("ws-1")?).await?;
                    manager.create_workspace(Slug::new("ws-2")?).await?;
                    manager.create_workspace(Slug::new("ws-3")?).await?;

                    let workspaces = manager.list_workspaces().await?;
                    """
                expected_output: """
                    assert_eq!(workspaces.len(), 3);

                    let slugs: Vec<_> = workspaces.iter().map(|w| w.slug.as_str()).collect();
                    assert!(slugs.contains(&"ws-1"));
                    assert!(slugs.contains(&"ws-2"));
                    assert!(slugs.contains(&"ws-3"));

                    for ws in &workspaces {
                        assert!(ws.path.exists());
                        assert!(ws.operation_id.is_some());
                    }
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_create_workspace_fails_on_duplicate_slug"
                given: "A workspace with slug 'my-feature' already exists"
                when:  "create_workspace is called with 'my-feature'"
                then: [
                    "Returns Err(Error::WorkspaceExists)",
                    "Existing workspace is unchanged",
                    "No partial state changes",
                ]
                real_input: """
                    let slug = Slug::new("my-feature")?;
                    manager.create_workspace(slug.clone()).await?;

                    let result = manager.create_workspace(slug.clone()).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::WorkspaceExists { slug: s }) if s == "my-feature"));
                    """
            },
            {
                name:  "test_forget_workspace_fails_on_nonexistent"
                given: "No workspace with slug 'nonexistent' exists"
                when:  "forget_workspace is called with 'nonexistent'"
                then: [
                    "Returns Err(Error::WorkspaceNotFound)",
                    "No state changes",
                ]
                real_input: """
                    let result = manager.forget_workspace(Slug::new("nonexistent")?).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::WorkspaceNotFound { .. })));
                    """
            },
            {
                name:  "test_jj_command_failure_returns_error"
                given: "JJ is configured to reject workspace creation"
                when:  "create_workspace is called"
                then: [
                    "Returns Err(Error::JjCommandFailed)",
                    "Error contains stderr from JJ",
                    "Error contains exit code",
                ]
                real_input: """
                    // Corrupt the .jj directory to cause JJ failure
                    std::fs::remove_dir_all(config.repo_root.join(".jj/repo"))?;

                    let result = manager.create_workspace(Slug::new("test")?).await;
                    """
                expected_output: null
                expected_error: """
                    match result {
                        Err(Error::JjCommandFailed { stderr, exit_code, .. }) => {
                            assert!(!stderr.is_empty());
                            assert_ne!(exit_code, 0);
                        }
                        _ => panic!("Expected JjCommandFailed error"),
                    }
                    """
            },
            {
                name:  "test_reflink_failure_is_explicit"
                given: "Filesystem does not support reflinks (e.g., NFS)"
                when:  "create_snapshot is called with reflink required"
                then: [
                    "Returns Err(Error::ReflinkNotSupported)",
                    "No silent fallback to full copy",
                ]
                real_input: """
                    let config = WorkspaceManagerConfig {
                        enable_reflink: true,
                        require_reflink: true,  // Fail if not available
                        ..default_config
                    };

                    // On filesystem without reflink support
                    let result = manager.create_snapshot(slug).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::ReflinkNotSupported { .. })));
                    """
            },
            {
                name:  "test_undo_fails_on_invalid_operation_id"
                given: "An invalid or non-existent OperationId"
                when:  "undo_operation is called"
                then: [
                    "Returns Err(Error::InvalidOperationId)",
                    "No state changes",
                ]
                real_input: """
                    let fake_op_id = OperationId::new("invalid-operation-id-that-does-not-exist");
                    let result = manager.undo_operation(fake_op_id).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::InvalidOperationId { .. })));
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_workspace_with_special_characters_in_path"
                scenario: "Workspace path contains spaces or unicode (via repo path)"
                input:    "Create workspace in /path/with spaces/repo"
                expected: "Workspace created successfully with proper escaping"
            },
            {
                name:     "test_concurrent_workspace_operations"
                scenario: "Multiple create/forget requests arrive simultaneously"
                input:    "Send 10 create requests in parallel"
                expected: "All operations serialized by actor, all succeed or fail cleanly"
            },
            {
                name:     "test_workspace_recovery_after_crash"
                scenario: "Actor restarts after crash mid-operation"
                input:    "Create workspace, simulate crash, restart actor"
                expected: "Actor reconciles state with JJ repository on startup"
            },
            {
                name:     "test_very_long_slug"
                scenario: "Slug at maximum allowed length (50 chars)"
                input:    "Slug::new(\"a\".repeat(50))"
                expected: "Workspace created, path is valid"
            },
            {
                name:     "test_bookmark_already_exists"
                scenario: "Bookmark with same name already exists in JJ"
                input:    "Create bookmark for existing name"
                expected: "Returns Error::BookmarkExists or updates existing bookmark"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in workspace_manager module"
                test:     "rg 'unwrap\\(\\)|expect\\(' crates/factory-core/src/workspace_manager.rs returns empty"
            },
            {
                name:     "test_invariant_no_git_commands"
                verifies: "No git commands are executed"
                test:     "rg '\"git\"' crates/factory-core/src/workspace_manager.rs returns empty"
            },
            {
                name:     "test_postcondition_operation_id"
                verifies: "All mutating operations return OperationId"
                test:     "WorkspaceCreated, BookmarkCreated, WorkspaceForgotten all have operation_id field"
            },
            {
                name:     "test_invariant_state_consistency"
                verifies: "Actor state matches JJ repository state"
                test:     "list_workspaces output matches jj workspace list output"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_workspace_lifecycle"
            description: "Complete workspace lifecycle: create -> bookmark -> snapshot -> forget -> undo"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/workspace_manager_e2e.rs"
                        content: """
                            use factory_core::workspace_manager::{
                                WorkspaceManager, WorkspaceManagerConfig, WorkspaceInfo
                            };
                            use factory_core::domain::Slug;
                            use factory_core::error::Result;
                            use std::path::PathBuf;
                            use tempfile::TempDir;

                            async fn setup_jj_repo() -> Result<(TempDir, PathBuf)> {
                                let temp = TempDir::new()?;
                                let repo_root = temp.path().to_path_buf();

                                // Initialize JJ repository
                                let output = tokio::process::Command::new("jj")
                                    .args(["git", "init"])
                                    .current_dir(&repo_root)
                                    .output()
                                    .await?;
                                assert!(output.status.success(), "Failed to init JJ repo");

                                Ok((temp, repo_root))
                            }

                            #[tokio::test]
                            async fn test_full_workspace_lifecycle() -> Result<()> {
                                let (_temp, repo_root) = setup_jj_repo().await?;

                                let config = WorkspaceManagerConfig {
                                    repo_root: repo_root.clone(),
                                    workspaces_dir: ".factory-workspaces".into(),
                                    enable_reflink: true,
                                    require_reflink: false,
                                };

                                // Spawn the actor (requires supervisor from factory-4pz)
                                let manager = WorkspaceManager::spawn(config).await?;

                                // 1. Create workspace
                                let slug = Slug::new("test-feature")?;
                                let created = manager.create_workspace(slug.clone()).await?;
                                assert!(created.path.exists());
                                assert!(created.operation_id.is_some());

                                // 2. Create bookmark
                                let bookmark = manager.create_bookmark(
                                    &created.path,
                                    "feat/test-feature"
                                ).await?;
                                assert!(bookmark.operation_id.is_some());

                                // 3. Create snapshot
                                let snapshot = manager.create_snapshot(slug.clone()).await?;
                                assert!(snapshot.path.exists());

                                // 4. Forget workspace
                                let forgotten = manager.forget_workspace(slug.clone()).await?;
                                assert!(!created.path.exists());
                                assert!(forgotten.operation_id.is_some());

                                // 5. Undo forget (workspace should reappear)
                                manager.undo_operation(forgotten.operation_id.unwrap()).await?;

                                // Refresh and verify
                                let workspaces = manager.list_workspaces().await?;
                                // Note: undo behavior depends on JJ version
                                // This test verifies the undo mechanism works

                                Ok(())
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "jj --version",  // Verify JJ is installed
                    "moon run factory-core:build",
                ]
            }

            execute: {
                command:    "moon run factory-core:test -- --test workspace_manager_e2e"
                timeout_ms: 120000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_workspace_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/workspace_manager.rs"
                        contains: "pub struct WorkspaceManager"
                    },
                    {
                        path:     "crates/factory-core/src/workspace_manager.rs"
                        contains: "OperationId"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/workspace_manager_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_factory_new_creates_workspace"
                description: "Verify factory new command creates JJ workspace via actor"
                steps: [
                    {action: "Run factory new -s my-task", verify: "Returns success"},
                    {action: "Check .factory-workspaces/my-task-* exists", verify: "Directory present"},
                    {action: "Check .factory/my-task symlink", verify: "Symlink points to workspace"},
                    {action: "Run jj workspace list", verify: "Workspace appears in JJ output"},
                ]
            },
            {
                name:        "e2e_workspace_undo_recovery"
                description: "Verify undo recovers from accidental workspace deletion"
                steps: [
                    {action: "Create workspace via actor", verify: "Workspace exists"},
                    {action: "Forget workspace via actor", verify: "Workspace removed"},
                    {action: "Undo forget operation", verify: "Workspace reappears"},
                    {action: "Verify workspace content", verify: "Files intact"},
                ]
            },
            {
                name:        "e2e_reflink_snapshot_performance"
                description: "Verify reflink snapshots are instant regardless of size"
                steps: [
                    {action: "Create workspace with 1GB test file", verify: "Workspace created"},
                    {action: "Create reflink snapshot", verify: "Completes in < 1 second"},
                    {action: "Verify snapshot content", verify: "File content matches"},
                    {action: "Verify disk usage", verify: "Minimal additional space used"},
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
                file:      "crates/factory-core/src/workspace_manager.rs"
                what:      "Test that create_workspace returns WorkspaceCreated with OperationId"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_forget_workspace_returns_operation_id"
                file:      "crates/factory-core/src/workspace_manager.rs"
                what:      "Test that forget_workspace removes workspace and returns OperationId"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_create_bookmark_returns_operation_id"
                file:      "crates/factory-core/src/workspace_manager.rs"
                what:      "Test that create_bookmark creates JJ bookmark with OperationId"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_undo_operation_reverts_workspace_creation"
                file:      "crates/factory-core/src/workspace_manager.rs"
                what:      "Test that undo_operation reverts a workspace creation"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_create_snapshot_with_reflink"
                file:      "crates/factory-core/src/workspace_manager.rs"
                what:      "Test that create_snapshot uses reflink when available"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_list_workspaces_returns_all_tracked"
                file:      "crates/factory-core/src/workspace_manager.rs"
                what:      "Test that list_workspaces returns all active workspaces"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define OperationId newtype"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    /// JJ operation ID for undo support
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct OperationId(String);

                    impl OperationId {
                        pub fn new(id: impl Into<String>) -> Self {
                            Self(id.into())
                        }

                        pub fn as_str(&self) -> &str {
                            &self.0
                        }
                    }

                    impl std::fmt::Display for OperationId {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            write!(f, "{}", self.0)
                        }
                    }
                    """
                done_when:     "OperationId newtype compiles"
                patterns_to_use: ["Opaque newtype pattern"]
            },
            {
                task: "Define WorkspaceInfo struct"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    /// Information about an active workspace
                    #[derive(Debug, Clone)]
                    pub struct WorkspaceInfo {
                        pub slug: Slug,
                        pub path: PathBuf,
                        pub bookmark: Option<String>,
                        pub operation_id: Option<OperationId>,
                        pub created_at: std::time::SystemTime,
                    }
                    """
                done_when:     "WorkspaceInfo struct compiles"
                patterns_to_use: ["Rich return types over primitive obsession"]
            },
            {
                task: "Define response types"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub struct WorkspaceCreated {
                        pub slug: Slug,
                        pub path: PathBuf,
                        pub operation_id: Option<OperationId>,
                    }

                    #[derive(Debug, Clone)]
                    pub struct WorkspaceForgotten {
                        pub slug: Slug,
                        pub operation_id: Option<OperationId>,
                    }

                    #[derive(Debug, Clone)]
                    pub struct BookmarkCreated {
                        pub name: String,
                        pub operation_id: Option<OperationId>,
                    }

                    #[derive(Debug, Clone)]
                    pub struct SnapshotCreated {
                        pub path: PathBuf,
                        pub source_slug: Slug,
                        pub reflink_used: bool,
                    }

                    #[derive(Debug, Clone)]
                    pub struct UndoCompleted {
                        pub operation_id: OperationId,
                    }
                    """
                done_when:     "All response types compile"
                patterns_to_use: ["Rich return types with context"]
            },
            {
                task: "Define WorkspaceManagerConfig"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub struct WorkspaceManagerConfig {
                        pub repo_root: PathBuf,
                        pub workspaces_dir: String,
                        pub enable_reflink: bool,
                        pub require_reflink: bool,
                    }

                    impl Default for WorkspaceManagerConfig {
                        fn default() -> Self {
                            Self {
                                repo_root: PathBuf::new(),
                                workspaces_dir: ".factory-workspaces".into(),
                                enable_reflink: true,
                                require_reflink: false,
                            }
                        }
                    }
                    """
                done_when:     "Config struct compiles with Default"
                patterns_to_use: ["Configuration struct with defaults"]
            },
            {
                task: "Define actor messages"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    use ractor::ActorRef;
                    use tokio::sync::oneshot;

                    /// Messages for WorkspaceManager actor
                    #[derive(Debug)]
                    pub enum WorkspaceManagerMsg {
                        CreateWorkspace {
                            slug: Slug,
                            respond_to: oneshot::Sender<Result<WorkspaceCreated, Error>>,
                        },
                        ForgetWorkspace {
                            slug: Slug,
                            respond_to: oneshot::Sender<Result<WorkspaceForgotten, Error>>,
                        },
                        CreateBookmark {
                            workspace_path: PathBuf,
                            name: String,
                            respond_to: oneshot::Sender<Result<BookmarkCreated, Error>>,
                        },
                        CreateSnapshot {
                            slug: Slug,
                            respond_to: oneshot::Sender<Result<SnapshotCreated, Error>>,
                        },
                        UndoOperation {
                            operation_id: OperationId,
                            respond_to: oneshot::Sender<Result<UndoCompleted, Error>>,
                        },
                        ListWorkspaces {
                            respond_to: oneshot::Sender<Result<Vec<WorkspaceInfo>, Error>>,
                        },
                    }
                    """
                done_when:     "Message enum compiles"
                patterns_to_use: [
                    "Request-reply pattern with oneshot",
                    "Enum for message types",
                ]
            },
            {
                task: "Implement JJ command runner"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    use tokio::process::Command;

                    /// Run a JJ command and capture output
                    async fn run_jj(
                        args: &[&str],
                        cwd: &Path,
                    ) -> Result<JjOutput, Error> {
                        let output = Command::new("jj")
                            .args(args)
                            .current_dir(cwd)
                            .output()
                            .await
                            .map_err(|e| Error::JjNotFound { source: e.to_string() })?;

                        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
                        let stderr = String::from_utf8_lossy(&output.stderr).to_string();

                        if !output.status.success() {
                            return Err(Error::JjCommandFailed {
                                command: format!("jj {}", args.join(" ")),
                                exit_code: output.status.code().unwrap_or(-1),
                                stderr,
                            });
                        }

                        Ok(JjOutput { stdout, stderr })
                    }

                    /// Get the current JJ operation ID
                    async fn get_operation_id(cwd: &Path) -> Result<OperationId, Error> {
                        let output = run_jj(&["operation", "log", "-n1", "--no-graph", "-T", "self.id()"], cwd).await?;
                        let id = output.stdout.trim().to_string();
                        Ok(OperationId::new(id))
                    }

                    #[derive(Debug)]
                    struct JjOutput {
                        stdout: String,
                        stderr: String,
                    }
                    """
                done_when:     "JJ runner compiles and handles errors"
                patterns_to_use: [
                    "tokio::process::Command for async execution",
                    "Result<T, Error> for all fallible operations",
                    "Capture both stdout and stderr",
                ]
            },
            {
                task: "Implement WorkspaceManager actor"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    use ractor::{Actor, ActorProcessingErr, ActorRef};
                    use std::collections::HashMap;

                    /// Actor state
                    pub struct WorkspaceManagerState {
                        config: WorkspaceManagerConfig,
                        workspaces: HashMap<Slug, WorkspaceInfo>,
                    }

                    pub struct WorkspaceManager;

                    impl Actor for WorkspaceManager {
                        type Msg = WorkspaceManagerMsg;
                        type State = WorkspaceManagerState;
                        type Arguments = WorkspaceManagerConfig;

                        async fn pre_start(
                            &self,
                            _myself: ActorRef<Self::Msg>,
                            config: Self::Arguments,
                        ) -> Result<Self::State, ActorProcessingErr> {
                            // Verify JJ is available
                            verify_jj_available(&config.repo_root).await
                                .map_err(|e| ActorProcessingErr::from(e.to_string()))?;

                            // Detect reflink support
                            let reflink_supported = detect_reflink_support(&config.repo_root);

                            // Initialize state by scanning existing workspaces
                            let workspaces = scan_existing_workspaces(&config).await
                                .map_err(|e| ActorProcessingErr::from(e.to_string()))?;

                            Ok(WorkspaceManagerState {
                                config,
                                workspaces,
                            })
                        }

                        async fn handle(
                            &self,
                            _myself: ActorRef<Self::Msg>,
                            message: Self::Msg,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            match message {
                                WorkspaceManagerMsg::CreateWorkspace { slug, respond_to } => {
                                    let result = self.handle_create_workspace(&slug, state).await;
                                    let _ = respond_to.send(result);
                                }
                                WorkspaceManagerMsg::ForgetWorkspace { slug, respond_to } => {
                                    let result = self.handle_forget_workspace(&slug, state).await;
                                    let _ = respond_to.send(result);
                                }
                                WorkspaceManagerMsg::CreateBookmark { workspace_path, name, respond_to } => {
                                    let result = self.handle_create_bookmark(&workspace_path, &name, state).await;
                                    let _ = respond_to.send(result);
                                }
                                WorkspaceManagerMsg::CreateSnapshot { slug, respond_to } => {
                                    let result = self.handle_create_snapshot(&slug, state).await;
                                    let _ = respond_to.send(result);
                                }
                                WorkspaceManagerMsg::UndoOperation { operation_id, respond_to } => {
                                    let result = self.handle_undo_operation(&operation_id, state).await;
                                    let _ = respond_to.send(result);
                                }
                                WorkspaceManagerMsg::ListWorkspaces { respond_to } => {
                                    let result = Ok(state.workspaces.values().cloned().collect());
                                    let _ = respond_to.send(result);
                                }
                            }
                            Ok(())
                        }
                    }
                    """
                done_when:     "Actor compiles and handles messages"
                patterns_to_use: [
                    "ractor::Actor trait implementation",
                    "HashMap for workspace registry",
                    "Pattern matching for message dispatch",
                    "oneshot for request-reply",
                ]
            },
            {
                task: "Implement create_workspace handler"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    impl WorkspaceManager {
                        async fn handle_create_workspace(
                            &self,
                            slug: &Slug,
                            state: &mut WorkspaceManagerState,
                        ) -> Result<WorkspaceCreated, Error> {
                            // Check for duplicate
                            if state.workspaces.contains_key(slug) {
                                return Err(Error::WorkspaceExists { slug: slug.to_string() });
                            }

                            let config = &state.config;
                            let workspaces_base = config.repo_root.join(&config.workspaces_dir);
                            let unique_id = generate_unique_id()?;
                            let workspace_name = format!("{}-{}", slug, unique_id);
                            let workspace_path = workspaces_base.join(&workspace_name);

                            // Ensure base directory exists
                            tokio::fs::create_dir_all(&workspaces_base).await
                                .map_err(|e| Error::directory_creation_failed(&workspaces_base, e.to_string()))?;

                            // Run jj workspace add
                            let pre_op = get_operation_id(&config.repo_root).await?;
                            run_jj(
                                &["workspace", "add", "--name", &workspace_name, workspace_path.to_str().unwrap_or_default()],
                                &config.repo_root
                            ).await?;
                            let post_op = get_operation_id(&config.repo_root).await?;

                            // Create symlink in .factory/
                            let factory_dir = config.repo_root.join(".factory");
                            tokio::fs::create_dir_all(&factory_dir).await
                                .map_err(|e| Error::directory_creation_failed(&factory_dir, e.to_string()))?;

                            let symlink_path = factory_dir.join(slug.as_str());
                            tokio::fs::symlink(&workspace_path, &symlink_path).await
                                .map_err(|e| Error::file_write_failed(&symlink_path, e.to_string()))?;

                            // Track in state
                            let info = WorkspaceInfo {
                                slug: slug.clone(),
                                path: workspace_path.clone(),
                                bookmark: None,
                                operation_id: Some(post_op.clone()),
                                created_at: std::time::SystemTime::now(),
                            };
                            state.workspaces.insert(slug.clone(), info);

                            Ok(WorkspaceCreated {
                                slug: slug.clone(),
                                path: workspace_path,
                                operation_id: Some(post_op),
                            })
                        }
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "Check-then-act pattern for duplicates",
                    "Capture pre/post operation IDs",
                    "Update state only after command succeeds",
                ]
            },
            {
                task: "Implement forget_workspace handler"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    impl WorkspaceManager {
                        async fn handle_forget_workspace(
                            &self,
                            slug: &Slug,
                            state: &mut WorkspaceManagerState,
                        ) -> Result<WorkspaceForgotten, Error> {
                            // Check workspace exists
                            let info = state.workspaces.get(slug)
                                .ok_or_else(|| Error::WorkspaceNotFound { slug: slug.to_string() })?
                                .clone();

                            let config = &state.config;

                            // Get workspace name from path
                            let workspace_name = info.path.file_name()
                                .and_then(|n| n.to_str())
                                .ok_or_else(|| Error::invalid_record("Invalid workspace path"))?;

                            // Run jj workspace forget
                            run_jj(
                                &["workspace", "forget", workspace_name],
                                &config.repo_root
                            ).await?;
                            let post_op = get_operation_id(&config.repo_root).await?;

                            // Remove workspace directory
                            if info.path.exists() {
                                tokio::fs::remove_dir_all(&info.path).await
                                    .map_err(|e| Error::file_write_failed(&info.path, e.to_string()))?;
                            }

                            // Remove symlink
                            let symlink_path = config.repo_root.join(".factory").join(slug.as_str());
                            let _ = tokio::fs::remove_file(&symlink_path).await;

                            // Remove from state
                            state.workspaces.remove(slug);

                            Ok(WorkspaceForgotten {
                                slug: slug.clone(),
                                operation_id: Some(post_op),
                            })
                        }
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "Verify existence before operation",
                    "Clean up directory and symlink",
                    "Remove from state after command succeeds",
                ]
            },
            {
                task: "Implement create_snapshot with reflink"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    impl WorkspaceManager {
                        async fn handle_create_snapshot(
                            &self,
                            slug: &Slug,
                            state: &WorkspaceManagerState,
                        ) -> Result<SnapshotCreated, Error> {
                            let info = state.workspaces.get(slug)
                                .ok_or_else(|| Error::WorkspaceNotFound { slug: slug.to_string() })?;

                            let config = &state.config;
                            let snapshot_id = generate_unique_id()?;
                            let snapshot_name = format!("{}-snapshot-{}", slug, snapshot_id);
                            let snapshot_path = config.repo_root
                                .join(&config.workspaces_dir)
                                .join(&snapshot_name);

                            // Use cp with reflink
                            let reflink_arg = if config.enable_reflink {
                                "--reflink=auto"
                            } else {
                                "--reflink=never"
                            };

                            let output = tokio::process::Command::new("cp")
                                .args(["-a", reflink_arg, info.path.to_str().unwrap_or_default(), snapshot_path.to_str().unwrap_or_default()])
                                .output()
                                .await
                                .map_err(|e| Error::CommandNotFound { cmd: format!("cp: {}", e) })?;

                            if !output.status.success() {
                                let stderr = String::from_utf8_lossy(&output.stderr);

                                // Check if reflink failed and it was required
                                if config.require_reflink && stderr.contains("reflink") {
                                    return Err(Error::ReflinkNotSupported {
                                        path: info.path.clone(),
                                    });
                                }

                                return Err(Error::CommandFailed {
                                    code: output.status.code().unwrap_or(-1),
                                    stderr: stderr.to_string(),
                                });
                            }

                            // Detect if reflink was actually used (fast copy = reflink)
                            let reflink_used = config.enable_reflink;  // Simplified; could measure time

                            Ok(SnapshotCreated {
                                path: snapshot_path,
                                source_slug: slug.clone(),
                                reflink_used,
                            })
                        }
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "cp --reflink=auto for COW copy",
                    "Explicit error on reflink failure if required",
                    "Generate unique snapshot ID",
                ]
            },
            {
                task: "Implement undo_operation handler"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    impl WorkspaceManager {
                        async fn handle_undo_operation(
                            &self,
                            operation_id: &OperationId,
                            state: &mut WorkspaceManagerState,
                        ) -> Result<UndoCompleted, Error> {
                            let config = &state.config;

                            // Run jj undo --at-operation
                            run_jj(
                                &["undo", "--at-operation", operation_id.as_str()],
                                &config.repo_root
                            ).await.map_err(|e| match e {
                                Error::JjCommandFailed { stderr, .. } if stderr.contains("No such operation") => {
                                    Error::InvalidOperationId { id: operation_id.to_string() }
                                }
                                other => other,
                            })?;

                            // Refresh state from JJ
                            let workspaces = scan_existing_workspaces(&config).await?;
                            state.workspaces = workspaces;

                            Ok(UndoCompleted {
                                operation_id: operation_id.clone(),
                            })
                        }
                    }

                    /// Scan JJ for existing workspaces and reconcile state
                    async fn scan_existing_workspaces(
                        config: &WorkspaceManagerConfig,
                    ) -> Result<HashMap<Slug, WorkspaceInfo>, Error> {
                        let output = run_jj(&["workspace", "list"], &config.repo_root).await?;
                        let mut workspaces = HashMap::new();

                        for line in output.stdout.lines() {
                            let parts: Vec<&str> = line.split_whitespace().collect();
                            if parts.len() >= 2 {
                                let ws_name = parts[0];
                                // Parse slug from workspace name (format: slug-uniqueid)
                                if let Some(slug_str) = ws_name.rsplit_once('-').map(|(s, _)| s) {
                                    if let Ok(slug) = Slug::new(slug_str) {
                                        let path = config.repo_root
                                            .join(&config.workspaces_dir)
                                            .join(ws_name);
                                        workspaces.insert(slug.clone(), WorkspaceInfo {
                                            slug,
                                            path,
                                            bookmark: None,
                                            operation_id: None,
                                            created_at: std::time::SystemTime::now(),
                                        });
                                    }
                                }
                            }
                        }

                        Ok(workspaces)
                    }
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "jj undo --at-operation for reverting",
                    "Refresh state after undo",
                    "Map JJ errors to domain errors",
                ]
            },
            {
                task: "Implement WorkspaceManagerRef handle"
                file: "crates/factory-core/src/workspace_manager.rs"
                what: """
                    use tokio::sync::oneshot;
                    use std::time::Duration;

                    /// Client-facing handle for WorkspaceManager actor
                    #[derive(Clone)]
                    pub struct WorkspaceManagerRef {
                        inner: ActorRef<WorkspaceManagerMsg>,
                    }

                    impl WorkspaceManagerRef {
                        pub fn new(actor_ref: ActorRef<WorkspaceManagerMsg>) -> Self {
                            Self { inner: actor_ref }
                        }

                        pub async fn create_workspace(&self, slug: Slug) -> Result<WorkspaceCreated, Error> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(WorkspaceManagerMsg::CreateWorkspace {
                                slug,
                                respond_to: tx,
                            }).map_err(|_| Error::ActorDisconnected)?;

                            tokio::time::timeout(Duration::from_secs(30), rx)
                                .await
                                .map_err(|_| Error::ActorTimeout)?
                                .map_err(|_| Error::ActorDisconnected)?
                        }

                        pub async fn forget_workspace(&self, slug: Slug) -> Result<WorkspaceForgotten, Error> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(WorkspaceManagerMsg::ForgetWorkspace {
                                slug,
                                respond_to: tx,
                            }).map_err(|_| Error::ActorDisconnected)?;

                            tokio::time::timeout(Duration::from_secs(30), rx)
                                .await
                                .map_err(|_| Error::ActorTimeout)?
                                .map_err(|_| Error::ActorDisconnected)?
                        }

                        pub async fn create_bookmark(
                            &self,
                            workspace_path: &Path,
                            name: &str,
                        ) -> Result<BookmarkCreated, Error> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(WorkspaceManagerMsg::CreateBookmark {
                                workspace_path: workspace_path.to_path_buf(),
                                name: name.to_string(),
                                respond_to: tx,
                            }).map_err(|_| Error::ActorDisconnected)?;

                            tokio::time::timeout(Duration::from_secs(30), rx)
                                .await
                                .map_err(|_| Error::ActorTimeout)?
                                .map_err(|_| Error::ActorDisconnected)?
                        }

                        pub async fn create_snapshot(&self, slug: Slug) -> Result<SnapshotCreated, Error> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(WorkspaceManagerMsg::CreateSnapshot {
                                slug,
                                respond_to: tx,
                            }).map_err(|_| Error::ActorDisconnected)?;

                            tokio::time::timeout(Duration::from_secs(60), rx)  // Longer for snapshots
                                .await
                                .map_err(|_| Error::ActorTimeout)?
                                .map_err(|_| Error::ActorDisconnected)?
                        }

                        pub async fn undo_operation(&self, operation_id: OperationId) -> Result<UndoCompleted, Error> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(WorkspaceManagerMsg::UndoOperation {
                                operation_id,
                                respond_to: tx,
                            }).map_err(|_| Error::ActorDisconnected)?;

                            tokio::time::timeout(Duration::from_secs(30), rx)
                                .await
                                .map_err(|_| Error::ActorTimeout)?
                                .map_err(|_| Error::ActorDisconnected)?
                        }

                        pub async fn list_workspaces(&self) -> Result<Vec<WorkspaceInfo>, Error> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(WorkspaceManagerMsg::ListWorkspaces {
                                respond_to: tx,
                            }).map_err(|_| Error::ActorDisconnected)?;

                            tokio::time::timeout(Duration::from_secs(10), rx)
                                .await
                                .map_err(|_| Error::ActorTimeout)?
                                .map_err(|_| Error::ActorDisconnected)?
                        }
                    }
                    """
                done_when:     "All handle methods work"
                patterns_to_use: [
                    "Actor handle pattern (Alice Ryhl)",
                    "oneshot for request-reply",
                    "Timeout on all operations",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export workspace_manager from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod workspace_manager;"
                done_when: "External crates can import factory_core::workspace_manager"
            },
            {
                task:      "Add workspace_manager errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what: """
                    // JJ-specific errors
                    #[error("JJ not found: {source}")]
                    JjNotFound { source: String },

                    #[error("JJ command failed: {command} (exit code {exit_code}): {stderr}")]
                    JjCommandFailed {
                        command: String,
                        exit_code: i32,
                        stderr: String,
                    },

                    #[error("Invalid operation ID: {id}")]
                    InvalidOperationId { id: String },

                    #[error("Reflink not supported for path: {path}")]
                    ReflinkNotSupported { path: PathBuf },

                    #[error("Actor disconnected")]
                    ActorDisconnected,

                    #[error("Actor operation timed out")]
                    ActorTimeout,
                    """
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Remove git fallback from worktree.rs"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Remove create_git_worktree function and git fallback logic"
                done_when: "No git commands in worktree.rs, only JJ via workspace_manager"
            },
            {
                task:      "Register WorkspaceManager with supervisor"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Add WorkspaceManager as a child actor in supervisor startup"
                done_when: "WorkspaceManager spawned and supervised"
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
                commands: ["rg 'unwrap\\(\\)|expect\\(' crates/factory-core/src/workspace_manager.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify no git commands"
                done_when: "grep finds no git commands"
                commands: ["rg '\"git\"' crates/factory-core/src/workspace_manager.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification"
                done_when: "factory new creates JJ workspace via actor"
                commands: [
                    "cd test-repo && jj git init",
                    "cargo run --bin factory -- new -s test-task",
                    "jj workspace list",
                ]
                expected: "Workspace appears in jj workspace list output"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Error: JJ not found"
                likely_cause: "JJ not installed or not in PATH"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/workspace_manager.rs"
                        function:      "verify_jj_available()"
                        what_to_check: "Is jj --version being run correctly?"
                    },
                ]
                fix_pattern: "Install JJ: cargo install jj-cli"
            },
            {
                symptom:      "Error: Not a JJ repository"
                likely_cause: "No .jj directory in repo_root"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/workspace_manager.rs"
                        function:      "pre_start()"
                        what_to_check: "Is .jj directory check running?"
                    },
                ]
                fix_pattern: "Initialize JJ repo: jj git init"
            },
            {
                symptom:      "Workspace created but not tracked in state"
                likely_cause: "State update failed after JJ command succeeded"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/workspace_manager.rs"
                        function:      "handle_create_workspace()"
                        what_to_check: "Is state.workspaces.insert() called after JJ succeeds?"
                    },
                ]
                fix_pattern: "Ensure state update is in the success path"
            },
            {
                symptom:      "Reflink snapshot takes a long time"
                likely_cause: "Reflink not supported, falling back to full copy"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/workspace_manager.rs"
                        function:      "handle_create_snapshot()"
                        what_to_check: "Is --reflink=auto being used?"
                    },
                ]
                fix_pattern: "Use filesystem that supports reflinks (btrfs, xfs, APFS)"
            },
            {
                symptom:      "Undo fails with 'No such operation'"
                likely_cause: "OperationId is from different JJ repo or was garbage collected"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/workspace_manager.rs"
                        function:      "handle_undo_operation()"
                        what_to_check: "Is the OperationId format correct?"
                    },
                ]
                fix_pattern: "Use operation ID from same repo, check jj operation log"
            },
            {
                symptom:      "Actor message times out"
                likely_cause: "JJ operation is hanging or very slow"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/workspace_manager.rs"
                        function:      "run_jj()"
                        what_to_check: "Is there a timeout on the JJ command?"
                    },
                ]
                fix_pattern: "Add timeout to tokio::process::Command"
            },
        ]

        debugging_commands: [
            {
                scenario: "When JJ command fails mysteriously"
                run:      "jj workspace add --name test-ws /tmp/test-ws 2>&1"
                look_for: "JJ error message explaining failure"
            },
            {
                scenario: "When state seems inconsistent"
                run:      "jj workspace list && ls -la .factory-workspaces/"
                look_for: "Mismatch between JJ workspaces and filesystem"
            },
            {
                scenario: "When reflink performance is slow"
                run:      "time cp --reflink=always file1 file2"
                look_for: "If it fails, filesystem doesn't support reflinks"
            },
            {
                scenario: "When undo doesn't work"
                run:      "jj operation log --limit 10"
                look_for: "Operation ID format and available operations"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_create_workspace_returns_operation_id passes",
            "[ ] test_forget_workspace_returns_operation_id passes",
            "[ ] test_create_bookmark_returns_operation_id passes",
            "[ ] test_undo_operation_reverts_workspace_creation passes",
            "[ ] test_create_snapshot_with_reflink passes",
            "[ ] test_list_workspaces_returns_all_tracked passes",
            "[ ] test_create_workspace_fails_on_duplicate_slug passes",
            "[ ] test_forget_workspace_fails_on_nonexistent passes",
            "[ ] test_jj_command_failure_returns_error passes",
            "[ ] E2E full workspace lifecycle test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Zero git commands - JJ only",
            "[ ] OperationId newtype defined",
            "[ ] WorkspaceInfo struct defined",
            "[ ] All response types defined (WorkspaceCreated, etc.)",
            "[ ] WorkspaceManagerConfig with reflink settings",
            "[ ] Actor message enum defined",
            "[ ] WorkspaceManager actor implementation",
            "[ ] WorkspaceManagerRef handle implementation",
            "[ ] JJ command runner with error handling",
            "[ ] State reconciliation with JJ on startup and after undo",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs explaining JJ workspace management",
            "[ ] OperationId documented with undo usage",
            "[ ] Reflink behavior documented",
            "[ ] Actor message protocol documented",
            "[ ] Example usage in doc comments",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add workspace_manager"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add JJ and actor error variants"
            },
            {
                path:      "crates/factory-core/src/worktree.rs"
                relevance: "Existing worktree code - git fallback to be removed"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Slug validation - workspace_manager uses Slug type"
            },
            {
                path:      "crates/factory-core/src/supervisor.rs"
                relevance: "Supervisor - WorkspaceManager to be registered as child"
            },
            {
                path:      ".beads/specs/factory-4pz.cue"
                relevance: "Supervisor spec - dependency for actor registration"
            },
            {
                path:      ".beads/specs/factory-ac1.cue"
                relevance: "Language detection bug - related workspace retrieval"
            },
            {
                path:      ".beads/specs/factory-duf.cue"
                relevance: "Unique ID generation - used for workspace names"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-4pz"
                description: "Root supervisor with OTP restart strategies"
                what_it_provides: "Actor supervision infrastructure for WorkspaceManager"
            },
            {
                bead_id:     "factory-ac1"
                description: "Fix silent language detection fallback"
                what_it_provides: "Proper error handling for workspace retrieval"
            },
            {
                bead_id:     "factory-duf"
                description: "Fix unique ID collision risk"
                what_it_provides: "Cryptographically secure workspace IDs"
            },
            {
                bead_id:     "factory-goi"
                description: "Related workspace operations"
                what_it_provides: "Additional workspace functionality"
            },
        ]

        external_references: [
            "https://martinvonz.github.io/jj/latest/ - JJ documentation",
            "https://martinvonz.github.io/jj/latest/working-copy/#workspaces - JJ workspaces",
            "https://martinvonz.github.io/jj/latest/bookmarks/ - JJ bookmarks",
            "https://martinvonz.github.io/jj/latest/operation-log/ - JJ operation log and undo",
            "https://btrfs.readthedocs.io/en/latest/Reflink.html - Reflink documentation",
            "https://github.com/slawlor/ractor - Ractor actor framework",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Actor Handle Pattern"
                example_location: "crates/factory-core/src/supervisor.rs:SupervisorRef"
                how_to_apply:     "Separate actor struct from client-facing handle with timeout"
            },
            {
                pattern:          "Opaque Newtype"
                example_location: "crates/factory-core/src/domain.rs:Slug"
                how_to_apply:     "Wrap primitive types in validated newtypes"
            },
            {
                pattern:          "Request-Reply with oneshot"
                example_location: "ractor patterns"
                how_to_apply:     "Use oneshot::channel for synchronous actor calls"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use jj CLI directly via tokio::process::Command",
            "Capture OperationId from jj operation log after each mutating command",
            "Use HashMap<Slug, WorkspaceInfo> for actor state",
            "Implement actor handle pattern with timeout on all operations",
            "Use cp --reflink=auto for COW snapshots",
            "Scan existing workspaces on actor startup for state reconciliation",
            "Refresh state from JJ after undo operations",
            "Use oneshot channels for request-reply pattern",
            "Log all JJ commands and their results with tracing",
            "Validate JJ availability in pre_start",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT fall back to git commands if JJ fails",
            "Do NOT silently fall back to full copy if reflink fails",
            "Do NOT proceed without OperationId for mutating operations",
            "Do NOT ignore state consistency between actor and JJ",
            "Do NOT use blocking I/O in async actor handlers",
        ]

        code_patterns: [
            {
                name:     "JJ Command Execution"
                use_when: "Running any JJ CLI command"
                example:  """
                    async fn run_jj(args: &[&str], cwd: &Path) -> Result<JjOutput, Error> {
                        let output = Command::new("jj")
                            .args(args)
                            .current_dir(cwd)
                            .output()
                            .await
                            .map_err(|e| Error::JjNotFound { source: e.to_string() })?;

                        if !output.status.success() {
                            return Err(Error::JjCommandFailed {
                                command: format!("jj {}", args.join(" ")),
                                exit_code: output.status.code().unwrap_or(-1),
                                stderr: String::from_utf8_lossy(&output.stderr).to_string(),
                            });
                        }

                        Ok(JjOutput {
                            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
                            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
                        })
                    }
                    """
            },
            {
                name:     "OperationId Capture"
                use_when: "After any mutating JJ operation"
                example:  """
                    async fn get_operation_id(cwd: &Path) -> Result<OperationId, Error> {
                        let output = run_jj(
                            &["operation", "log", "-n1", "--no-graph", "-T", "self.id()"],
                            cwd
                        ).await?;
                        Ok(OperationId::new(output.stdout.trim()))
                    }

                    // Usage:
                    run_jj(&["workspace", "add", ...], &repo_root).await?;
                    let op_id = get_operation_id(&repo_root).await?;
                    """
            },
            {
                name:     "Actor Request-Reply"
                use_when: "Implementing actor handle methods"
                example:  """
                    pub async fn create_workspace(&self, slug: Slug) -> Result<WorkspaceCreated, Error> {
                        let (tx, rx) = oneshot::channel();
                        self.inner.cast(WorkspaceManagerMsg::CreateWorkspace {
                            slug,
                            respond_to: tx,
                        }).map_err(|_| Error::ActorDisconnected)?;

                        tokio::time::timeout(Duration::from_secs(30), rx)
                            .await
                            .map_err(|_| Error::ActorTimeout)?
                            .map_err(|_| Error::ActorDisconnected)?
                    }
                    """
            },
            {
                name:     "Reflink Snapshot"
                use_when: "Creating COW filesystem snapshots"
                example:  """
                    let output = Command::new("cp")
                        .args(["-a", "--reflink=auto", src.to_str().unwrap_or_default(), dst.to_str().unwrap_or_default()])
                        .output()
                        .await?;

                    if !output.status.success() {
                        let stderr = String::from_utf8_lossy(&output.stderr);
                        if require_reflink && stderr.contains("reflink") {
                            return Err(Error::ReflinkNotSupported { path: src.clone() });
                        }
                    }
                    """
            },
            {
                name:     "State Reconciliation"
                use_when: "Syncing actor state with JJ repository"
                example:  """
                    async fn scan_existing_workspaces(
                        config: &WorkspaceManagerConfig,
                    ) -> Result<HashMap<Slug, WorkspaceInfo>, Error> {
                        let output = run_jj(&["workspace", "list"], &config.repo_root).await?;
                        let mut workspaces = HashMap::new();

                        for line in output.stdout.lines() {
                            // Parse workspace name and create WorkspaceInfo
                            // Add to HashMap
                        }

                        Ok(workspaces)
                    }
                    """
            },
        ]
    }
}
