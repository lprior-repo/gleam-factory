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

"factory-52v": #ValidBead & {
    // ============================================================================
    // BEAD: factory-52v - Implement golden master actor for source-of-truth snapshots
    // ============================================================================

    id:              "factory-52v"
    title:           "Runtime: Implement golden master actor for source-of-truth snapshots"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["runtime", "actors", "golden-master", "snapshots", "cow", "P2"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL maintain a pristine copy of source files for comparison",
            "THE SYSTEM SHALL support Copy-on-Write (COW) snapshots via reflinks",
            "THE SYSTEM SHALL provide diff generation between golden master and workspace",
            "THE SYSTEM SHALL support rollback to any previous snapshot",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN a new workspace is created"
                shall:   "THE SYSTEM SHALL create a golden master snapshot of the source"
            },
            {
                trigger: "WHEN a snapshot is requested"
                shall:   "THE SYSTEM SHALL create a COW snapshot using reflinks where available"
            },
            {
                trigger: "WHEN a diff is requested"
                shall:   "THE SYSTEM SHALL compare current workspace against golden master"
            },
            {
                trigger: "WHEN a rollback is requested"
                shall:   "THE SYSTEM SHALL restore files from the specified snapshot"
            },
            {
                trigger: "WHEN the workspace manager signals workspace creation"
                shall:   "THE SYSTEM SHALL capture initial state as golden master"
            },
            {
                trigger: "WHEN a file changes in the workspace"
                shall:   "THE SYSTEM SHALL track the change for diff generation"
            },
            {
                trigger: "WHEN garbage collection is triggered"
                shall:   "THE SYSTEM SHALL clean up orphaned snapshots older than retention period"
            },
        ]

        state_driven: [
            {
                state: "WHILE the golden master is valid"
                shall: "THE SYSTEM SHALL provide consistent diffs against workspace"
            },
            {
                state: "WHILE a snapshot operation is in progress"
                shall: "THE SYSTEM SHALL prevent concurrent snapshot operations on the same target"
            },
            {
                state: "WHILE rollback is in progress"
                shall: "THE SYSTEM SHALL maintain atomicity - complete or abort entirely"
            },
        ]

        unwanted: [
            {
                condition: "IF the source directory does not exist"
                shall_not: "THE SYSTEM SHALL NOT create a snapshot with missing data"
                because:   "Missing source data results in incomplete golden master"
            },
            {
                condition: "IF disk space is insufficient for snapshot"
                shall_not: "THE SYSTEM SHALL NOT leave partial snapshots on disk"
                because:   "Partial snapshots corrupt comparison logic and waste space"
            },
            {
                condition: "IF reflink fails"
                shall_not: "THE SYSTEM SHALL NOT silently fall back to full copy"
                because:   "Silent fallback causes unexpected disk usage; explicit fallback required"
            },
            {
                condition: "IF rollback fails midway"
                shall_not: "THE SYSTEM SHALL NOT leave workspace in inconsistent state"
                because:   "Partial rollback creates unrecoverable state"
            },
            {
                condition: "IF concurrent snapshots are requested"
                shall_not: "THE SYSTEM SHALL NOT allow data races on snapshot storage"
                because:   "Concurrent writes cause corruption"
            },
        ]

        complex: [
            {
                state:   "WHILE the actor is shutting down"
                trigger: "WHEN a snapshot request arrives"
                shall:   "THE SYSTEM SHALL reject with Error::ShuttingDown"
            },
            {
                state:   "WHILE a rollback is in progress"
                trigger: "WHEN a new snapshot is requested"
                shall:   "THE SYSTEM SHALL queue the snapshot until rollback completes"
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
                    field:           "source_path"
                    type:            "PathBuf"
                    constraints:     "Must exist and be a directory"
                    example_valid:   "PathBuf::from(\"/workspace/my-task\")"
                    example_invalid: "PathBuf::from(\"/nonexistent/path\")"
                },
                {
                    field:           "snapshot_id"
                    type:            "SnapshotId"
                    constraints:     "Valid UUID or timestamp-based identifier"
                    example_valid:   "SnapshotId::new(\"20260131-143052-abc123\")"
                    example_invalid: "SnapshotId::new(\"\")  // Empty ID"
                },
                {
                    field:           "GoldenMasterConfig"
                    type:            "Struct"
                    constraints:     "Must specify storage_path, retention_days, use_reflinks"
                    example_valid:   "GoldenMasterConfig { storage_path: PathBuf::from(\".factory/snapshots\"), retention_days: 30, use_reflinks: true }"
                    example_invalid: "GoldenMasterConfig { storage_path: PathBuf::from(\"\"), .. }"
                },
            ]
            system_state: [
                "Tokio runtime initialized",
                "ractor framework available (factory-00s completed)",
                "WorkspaceManager actor running (factory-pa8 completed)",
                "Filesystem supports reflinks (btrfs/xfs/APFS) or fallback enabled",
            ]
        }

        postconditions: {
            state_changes: [
                "GoldenMaster actor spawned and running",
                "Snapshot storage directory initialized",
                "Initial golden master created from source",
                "Snapshot metadata persisted to disk",
            ]
            return_guarantees: [
                {
                    field:     "GoldenMasterRef"
                    guarantee: "Always valid while held; actor exists until all refs dropped"
                },
                {
                    field:     "GoldenMaster::spawn()"
                    guarantee: "Returns Result<GoldenMasterRef, SpawnError>"
                },
                {
                    field:     "GoldenMasterRef::create_snapshot()"
                    guarantee: "Returns Result<SnapshotId, SnapshotError>"
                },
                {
                    field:     "GoldenMasterRef::diff()"
                    guarantee: "Returns Result<DiffResult, DiffError>"
                },
                {
                    field:     "GoldenMasterRef::rollback()"
                    guarantee: "Returns Result<(), RollbackError>"
                },
                {
                    field:     "GoldenMasterRef::list_snapshots()"
                    guarantee: "Returns Result<Vec<SnapshotInfo>, Error>"
                },
            ]
            side_effects: [
                "Snapshots stored in .factory/snapshots/<workspace>/<snapshot_id>/",
                "Snapshot metadata written to .factory/snapshots/<workspace>/metadata.json",
                "Reflinks created where filesystem supports them",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Golden master is never modified - only workspace changes",
            "Snapshot IDs are globally unique within a workspace",
            "Rollback is atomic - either fully succeeds or fully reverts",
            "Diff results are deterministic given same inputs",
            "Concurrent operations are serialized per workspace",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "WorkspaceManager not available when GoldenMaster starts"
                prevention:  "Supervisor ensures WorkspaceManager starts before GoldenMaster"
                test_for_it: "test_golden_master_waits_for_workspace_manager"
            },
            {
                failure:     "Filesystem doesn't support reflinks"
                prevention:  "Detect filesystem type at startup, use fallback copy if needed"
                test_for_it: "test_fallback_to_full_copy"
            },
            {
                failure:     "Snapshot storage path conflicts with existing files"
                prevention:  "Use unique subdirectory per workspace with validation"
                test_for_it: "test_storage_path_isolation"
            },
            {
                failure:     "Message types don't implement required traits"
                prevention:  "All message types derive Debug, Clone where needed"
                test_for_it: "test_message_types_are_send_sync"
            },
        ]

        usability_failures: [
            {
                failure:     "Diff output too verbose for large changes"
                prevention:  "Provide summary mode with expandable details"
                test_for_it: "test_diff_summary_mode"
            },
            {
                failure:     "Snapshot names not human-readable"
                prevention:  "Include timestamp and optional label in snapshot ID"
                test_for_it: "test_snapshot_id_format"
            },
            {
                failure:     "Rollback destination unclear"
                prevention:  "Require explicit confirmation with preview"
                test_for_it: "test_rollback_preview"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Snapshot corrupted during disk write"
                prevention:  "Use atomic rename after complete write, verify checksum"
                test_for_it: "test_snapshot_atomic_write"
            },
            {
                failure:     "Golden master modified after capture"
                prevention:  "Use immutable storage or copy, verify on read"
                test_for_it: "test_golden_master_immutability"
            },
            {
                failure:     "Rollback interrupted leaves partial state"
                prevention:  "Write to temp location, atomic swap on complete"
                test_for_it: "test_rollback_atomicity"
            },
            {
                failure:     "Concurrent snapshot operations race"
                prevention:  "Serialize operations per workspace via actor mailbox"
                test_for_it: "test_concurrent_snapshot_serialization"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_create_golden_master_snapshot"
                given: "A valid workspace path with source files"
                when:  "GoldenMaster::create_snapshot() is called"
                then: [
                    "Returns Ok(SnapshotId)",
                    "Snapshot files exist in storage path",
                    "Metadata JSON is written",
                    "Files match original source",
                ]
                real_input: """
                    use factory_core::golden_master::{GoldenMaster, GoldenMasterConfig};

                    let config = GoldenMasterConfig {
                        storage_path: PathBuf::from(".factory/snapshots"),
                        retention_days: 30,
                        use_reflinks: true,
                    };
                    let actor = GoldenMaster::spawn(config, workspace_mgr.clone()).await?;
                    let snapshot_id = actor.create_snapshot(&workspace_path).await?;
                    """
                expected_output: """
                    assert!(snapshot_id.as_str().len() > 0);
                    assert!(storage_path.join(snapshot_id.as_str()).exists());
                    """
            },
            {
                name:  "test_diff_against_golden_master"
                given: "A golden master snapshot and modified workspace"
                when:  "GoldenMaster::diff() is called"
                then: [
                    "Returns Ok(DiffResult)",
                    "DiffResult contains added files",
                    "DiffResult contains modified files",
                    "DiffResult contains deleted files",
                ]
                real_input: """
                    // Modify a file in workspace
                    fs::write(workspace_path.join("src/main.rs"), "modified content")?;

                    let diff = actor.diff(&workspace_path, &snapshot_id).await?;
                    """
                expected_output: """
                    assert!(!diff.modified.is_empty());
                    assert!(diff.modified.iter().any(|f| f.path.ends_with("main.rs")));
                    """
            },
            {
                name:  "test_rollback_to_snapshot"
                given: "A golden master snapshot and modified workspace"
                when:  "GoldenMaster::rollback() is called"
                then: [
                    "Returns Ok(())",
                    "Workspace matches snapshot exactly",
                    "All modifications are reverted",
                ]
                real_input: """
                    // Modify files
                    fs::write(workspace_path.join("src/main.rs"), "modified")?;
                    fs::remove_file(workspace_path.join("README.md"))?;

                    // Rollback
                    actor.rollback(&workspace_path, &snapshot_id).await?;
                    """
                expected_output: """
                    let diff = actor.diff(&workspace_path, &snapshot_id).await?;
                    assert!(diff.is_empty(), "Workspace should match snapshot after rollback");
                    """
            },
            {
                name:  "test_list_snapshots"
                given: "Multiple snapshots exist for a workspace"
                when:  "GoldenMaster::list_snapshots() is called"
                then: [
                    "Returns Ok(Vec<SnapshotInfo>)",
                    "List contains all snapshots",
                    "Snapshots are ordered by timestamp",
                ]
                real_input: """
                    let id1 = actor.create_snapshot(&workspace_path).await?;
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    let id2 = actor.create_snapshot(&workspace_path).await?;

                    let snapshots = actor.list_snapshots(&workspace_path).await?;
                    """
                expected_output: """
                    assert_eq!(snapshots.len(), 2);
                    assert!(snapshots[0].created_at < snapshots[1].created_at);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_snapshot_missing_source_returns_error"
                given: "A non-existent source path"
                when:  "GoldenMaster::create_snapshot() is called"
                then: [
                    "Returns Err(SnapshotError::SourceNotFound)",
                    "No partial snapshot created",
                ]
                real_input: """
                    let result = actor.create_snapshot(&PathBuf::from("/nonexistent")).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::SourceNotFound { path: PathBuf::from("/nonexistent") })
                    """
            },
            {
                name:  "test_rollback_unknown_snapshot_returns_error"
                given: "A snapshot ID that doesn't exist"
                when:  "GoldenMaster::rollback() is called"
                then: [
                    "Returns Err(RollbackError::SnapshotNotFound)",
                    "Workspace remains unchanged",
                ]
                real_input: """
                    let bad_id = SnapshotId::new("nonexistent-snapshot");
                    let result = actor.rollback(&workspace_path, &bad_id).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::SnapshotNotFound { id: "nonexistent-snapshot" })
                    """
            },
            {
                name:  "test_diff_corrupted_snapshot_returns_error"
                given: "A snapshot with corrupted metadata"
                when:  "GoldenMaster::diff() is called"
                then: [
                    "Returns Err(DiffError::CorruptedSnapshot)",
                    "Error includes snapshot ID for debugging",
                ]
                real_input: """
                    // Corrupt the metadata file
                    fs::write(storage_path.join(&snapshot_id).join("metadata.json"), "invalid json")?;
                    let result = actor.diff(&workspace_path, &snapshot_id).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::CorruptedSnapshot { id: snapshot_id.clone(), reason: "invalid JSON" })
                    """
            },
            {
                name:  "test_insufficient_disk_space_returns_error"
                given: "Disk space is insufficient for snapshot"
                when:  "GoldenMaster::create_snapshot() is called"
                then: [
                    "Returns Err(SnapshotError::InsufficientSpace)",
                    "No partial files left behind",
                ]
                real_input: """
                    // Mock insufficient disk space scenario
                    let result = actor.create_snapshot(&huge_workspace_path).await;
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InsufficientSpace { required: 1024000, available: 512 })
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_empty_workspace_snapshot"
                scenario: "Workspace contains no files"
                input:    "create snapshot of empty directory"
                expected: "Ok(SnapshotId) with empty file list in metadata"
            },
            {
                name:     "test_large_binary_files"
                scenario: "Workspace contains large binary files (>100MB)"
                input:    "create snapshot with binary files"
                expected: "Snapshot created with binary files intact, uses reflinks"
            },
            {
                name:     "test_symlinks_in_workspace"
                scenario: "Workspace contains symbolic links"
                input:    "create snapshot with symlinks"
                expected: "Symlinks preserved as symlinks, not dereferenced"
            },
            {
                name:     "test_concurrent_snapshot_requests"
                scenario: "Multiple snapshot requests arrive simultaneously"
                input:    "send 10 snapshot requests in parallel"
                expected: "All snapshots created sequentially, no corruption"
            },
            {
                name:     "test_snapshot_during_active_writes"
                scenario: "Files being written while snapshot is created"
                input:    "concurrent file writes during snapshot"
                expected: "Snapshot captures consistent state or returns retry error"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in golden_master.rs"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/golden_master.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public APIs return Result"
                test:     "All pub fn signatures in golden_master.rs return Result<T, Error>"
            },
            {
                name:     "test_invariant_immutable_golden_master"
                verifies: "Golden master files are never modified"
                test:     "No write operations on files under golden_master storage path after creation"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_golden_master_lifecycle"
            description: "Complete golden master lifecycle: create -> modify -> diff -> rollback"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/golden_master_e2e.rs"
                        content: """
                            use factory_core::golden_master::{GoldenMaster, GoldenMasterConfig};
                            use factory_core::error::Result;
                            use std::path::PathBuf;
                            use tempfile::TempDir;

                            #[tokio::test]
                            async fn test_full_golden_master_lifecycle() -> Result<()> {
                                // Setup temp directory
                                let temp = TempDir::new()?;
                                let workspace = temp.path().join("workspace");
                                std::fs::create_dir_all(&workspace)?;
                                std::fs::write(workspace.join("main.rs"), "fn main() {}")?;

                                // Create golden master actor
                                let config = GoldenMasterConfig::new(temp.path().join("snapshots"));
                                let actor = GoldenMaster::spawn(config).await?;

                                // Create initial snapshot
                                let snapshot_id = actor.create_snapshot(&workspace).await?;

                                // Modify workspace
                                std::fs::write(workspace.join("main.rs"), "fn main() { println!(\"hello\"); }")?;

                                // Generate diff
                                let diff = actor.diff(&workspace, &snapshot_id).await?;
                                assert!(!diff.is_empty());

                                // Rollback
                                actor.rollback(&workspace, &snapshot_id).await?;

                                // Verify rollback
                                let content = std::fs::read_to_string(workspace.join("main.rs"))?;
                                assert_eq!(content, "fn main() {}");

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
                command:    "moon run factory-core:test -- --test golden_master_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_golden_master_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/golden_master.rs"
                        contains: "pub struct GoldenMaster"
                    },
                    {
                        path:     "crates/factory-core/src/golden_master.rs"
                        contains: "pub async fn create_snapshot"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/golden_master_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_workspace_integration"
                description: "Verify GoldenMaster integrates with WorkspaceManager"
                steps: [
                    {action: "WorkspaceManager creates workspace", verify: "Workspace exists"},
                    {action: "GoldenMaster receives workspace created event", verify: "Initial snapshot created"},
                    {action: "User modifies files in workspace", verify: "Changes detected"},
                    {action: "User requests diff", verify: "Accurate diff returned"},
                    {action: "User requests rollback", verify: "Workspace restored to initial state"},
                ]
            },
            {
                name:        "e2e_cow_reflink_performance"
                description: "Verify COW snapshots use reflinks on supported filesystems"
                steps: [
                    {action: "Detect filesystem type", verify: "btrfs/xfs/APFS detected"},
                    {action: "Create snapshot of 1GB workspace", verify: "Snapshot < 10MB on disk"},
                    {action: "Verify files are identical", verify: "Content matches via checksum"},
                    {action: "Modify workspace file", verify: "Only modified file uses disk space"},
                ]
            },
            {
                name:        "e2e_garbage_collection"
                description: "Verify old snapshots are cleaned up"
                steps: [
                    {action: "Create multiple snapshots", verify: "All snapshots exist"},
                    {action: "Set retention to 1 day", verify: "Config updated"},
                    {action: "Advance time by 2 days (mock)", verify: "Old snapshots marked for cleanup"},
                    {action: "Run garbage collection", verify: "Old snapshots removed, recent retained"},
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
                task:      "Write test: test_create_golden_master_snapshot"
                file:      "crates/factory-core/src/golden_master.rs"
                what:      "Test that create_snapshot creates valid snapshot"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_diff_against_golden_master"
                file:      "crates/factory-core/src/golden_master.rs"
                what:      "Test that diff returns accurate file differences"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_rollback_to_snapshot"
                file:      "crates/factory-core/src/golden_master.rs"
                what:      "Test that rollback restores workspace to snapshot state"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_cow_reflink_creation"
                file:      "crates/factory-core/src/golden_master.rs"
                what:      "Test that COW reflinks are used when available"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define GoldenMaster types and messages"
                file: "crates/factory-core/src/golden_master.rs"
                what: """
                    pub struct GoldenMasterConfig {
                        pub storage_path: PathBuf,
                        pub retention_days: u32,
                        pub use_reflinks: bool,
                    }

                    #[derive(Debug, Clone)]
                    pub struct SnapshotId(String);

                    #[derive(Debug, Clone)]
                    pub struct SnapshotInfo {
                        pub id: SnapshotId,
                        pub created_at: DateTime<Utc>,
                        pub file_count: usize,
                        pub total_bytes: u64,
                    }

                    #[derive(Debug, Clone)]
                    pub struct DiffResult {
                        pub added: Vec<FileDiff>,
                        pub modified: Vec<FileDiff>,
                        pub deleted: Vec<FileDiff>,
                    }

                    pub enum GoldenMasterMsg {
                        CreateSnapshot { source: PathBuf, respond_to: RpcReplyPort<Result<SnapshotId>> },
                        Diff { workspace: PathBuf, snapshot_id: SnapshotId, respond_to: RpcReplyPort<Result<DiffResult>> },
                        Rollback { workspace: PathBuf, snapshot_id: SnapshotId, respond_to: RpcReplyPort<Result<()>> },
                        ListSnapshots { workspace: PathBuf, respond_to: RpcReplyPort<Result<Vec<SnapshotInfo>>> },
                        GarbageCollect { respond_to: RpcReplyPort<Result<u32>> },
                    }
                    """
                done_when:     "Types compile"
                patterns_to_use: ["Newtype pattern for SnapshotId", "RpcReplyPort for call messages"]
            },
            {
                task: "Implement GoldenMaster actor"
                file: "crates/factory-core/src/golden_master.rs"
                what: """
                    pub struct GoldenMaster {
                        config: GoldenMasterConfig,
                        snapshots: HashMap<PathBuf, Vec<SnapshotInfo>>,
                    }

                    impl Actor for GoldenMaster {
                        type Msg = GoldenMasterMsg;
                        type State = ();
                        type Arguments = GoldenMasterConfig;

                        async fn pre_start(
                            &self,
                            myself: ActorRef<Self::Msg>,
                            config: Self::Arguments,
                        ) -> Result<Self::State, ActorProcessingErr> {
                            // Initialize storage directory
                            Ok(())
                        }

                        async fn handle(
                            &self,
                            myself: ActorRef<Self::Msg>,
                            msg: Self::Msg,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            match msg {
                                GoldenMasterMsg::CreateSnapshot { source, respond_to } => {
                                    let result = self.create_snapshot_impl(&source).await;
                                    respond_to.send(result);
                                }
                                // ... other handlers
                            }
                            Ok(())
                        }
                    }
                    """
                done_when:     "Actor compiles and message handlers defined"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "ractor Actor trait implementation",
                    "Railway-oriented error handling",
                ]
            },
            {
                task: "Implement COW reflink snapshot"
                file: "crates/factory-core/src/golden_master.rs"
                what: """
                    impl GoldenMaster {
                        async fn create_snapshot_impl(&self, source: &Path) -> Result<SnapshotId> {
                            // Validate source exists
                            if !source.exists() {
                                return Err(Error::SourceNotFound { path: source.to_path_buf() });
                            }

                            // Generate snapshot ID
                            let snapshot_id = SnapshotId::generate();
                            let snapshot_path = self.config.storage_path.join(snapshot_id.as_str());

                            // Create snapshot using reflinks or fallback
                            self.copy_with_reflinks(source, &snapshot_path).await?;

                            // Write metadata
                            self.write_metadata(&snapshot_path, source).await?;

                            Ok(snapshot_id)
                        }

                        async fn copy_with_reflinks(&self, source: &Path, dest: &Path) -> Result<()> {
                            if self.config.use_reflinks {
                                // Try cp --reflink=auto
                                let result = process::run_command(
                                    "cp",
                                    &["-a", "--reflink=auto", &source.to_string_lossy(), &dest.to_string_lossy()],
                                    source.parent().unwrap_or(source),
                                )?;
                                if result.is_success() {
                                    return Ok(());
                                }
                            }
                            // Fallback to regular copy
                            self.copy_recursive(source, dest).await
                        }
                    }
                    """
                done_when:     "Snapshot creation with reflinks works"
                patterns_to_use: [
                    "cp --reflink=auto for COW",
                    "Fallback to regular copy",
                    "Atomic directory operations",
                ]
            },
            {
                task: "Implement diff generation"
                file: "crates/factory-core/src/golden_master.rs"
                what: """
                    impl GoldenMaster {
                        async fn diff_impl(&self, workspace: &Path, snapshot_id: &SnapshotId) -> Result<DiffResult> {
                            let snapshot_path = self.config.storage_path.join(snapshot_id.as_str());

                            if !snapshot_path.exists() {
                                return Err(Error::SnapshotNotFound { id: snapshot_id.as_str().to_string() });
                            }

                            let mut result = DiffResult::default();

                            // Walk both trees and compare
                            for entry in walkdir::WalkDir::new(&snapshot_path) {
                                let entry = entry.map_err(|e| Error::DiffFailed { reason: e.to_string() })?;
                                let rel_path = entry.path().strip_prefix(&snapshot_path)?;
                                let workspace_file = workspace.join(rel_path);

                                if !workspace_file.exists() {
                                    result.deleted.push(FileDiff::deleted(rel_path));
                                } else if self.files_differ(&entry.path(), &workspace_file)? {
                                    result.modified.push(FileDiff::modified(rel_path));
                                }
                            }

                            // Check for added files
                            for entry in walkdir::WalkDir::new(workspace) {
                                let entry = entry.map_err(|e| Error::DiffFailed { reason: e.to_string() })?;
                                let rel_path = entry.path().strip_prefix(workspace)?;
                                let snapshot_file = snapshot_path.join(rel_path);

                                if !snapshot_file.exists() {
                                    result.added.push(FileDiff::added(rel_path));
                                }
                            }

                            Ok(result)
                        }
                    }
                    """
                done_when:     "Diff generation works correctly"
                patterns_to_use: [
                    "walkdir for tree traversal",
                    "Strip prefix for relative paths",
                    "Content hashing for comparison",
                ]
            },
            {
                task: "Implement rollback"
                file: "crates/factory-core/src/golden_master.rs"
                what: """
                    impl GoldenMaster {
                        async fn rollback_impl(&self, workspace: &Path, snapshot_id: &SnapshotId) -> Result<()> {
                            let snapshot_path = self.config.storage_path.join(snapshot_id.as_str());

                            if !snapshot_path.exists() {
                                return Err(Error::SnapshotNotFound { id: snapshot_id.as_str().to_string() });
                            }

                            // Create temp directory for atomic swap
                            let temp_workspace = workspace.with_extension("rollback-temp");

                            // Copy snapshot to temp
                            self.copy_with_reflinks(&snapshot_path, &temp_workspace).await?;

                            // Atomic rename: backup current, swap in restored
                            let backup = workspace.with_extension("rollback-backup");
                            std::fs::rename(workspace, &backup)
                                .map_err(|e| Error::RollbackFailed { reason: e.to_string() })?;

                            std::fs::rename(&temp_workspace, workspace)
                                .map_err(|e| {
                                    // Restore backup on failure
                                    let _ = std::fs::rename(&backup, workspace);
                                    Error::RollbackFailed { reason: e.to_string() }
                                })?;

                            // Clean up backup
                            let _ = std::fs::remove_dir_all(&backup);

                            Ok(())
                        }
                    }
                    """
                done_when:     "Rollback is atomic and correct"
                patterns_to_use: [
                    "Atomic rename pattern",
                    "Backup before destructive operation",
                    "Cleanup on success, restore on failure",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod golden_master;"
                done_when: "External crates can import factory_core::golden_master"
            },
            {
                task:      "Add golden master errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "SourceNotFound, SnapshotNotFound, SnapshotFailed, DiffFailed, RollbackFailed, CorruptedSnapshot variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Integrate with WorkspaceManager"
                file:      "crates/factory-core/src/golden_master.rs"
                what:      "Subscribe to workspace creation events, auto-create initial snapshot"
                done_when: "Golden master auto-captures on workspace creation"
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
                done_when: "rg finds no unwrap/expect"
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/golden_master.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Test reflink creation"
                done_when: "Reflinks used on supported filesystems"
                commands: [
                    "cargo run --example golden_master_demo",
                ]
                expected: "Snapshot created with minimal disk usage"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Snapshot creation fails with 'operation not supported'"
                likely_cause: "Filesystem doesn't support reflinks"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/golden_master.rs"
                        function:      "copy_with_reflinks()"
                        what_to_check: "Is fallback to regular copy working?"
                    },
                ]
                fix_pattern: "Check cp --reflink=auto result, fallback to cp -a"
            },
            {
                symptom:      "Diff shows all files as modified"
                likely_cause: "Metadata (permissions/timestamps) differ"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/golden_master.rs"
                        function:      "files_differ()"
                        what_to_check: "Is comparison by content only, not metadata?"
                    },
                ]
                fix_pattern: "Compare files by content hash (sha256), not stat"
            },
            {
                symptom:      "Rollback leaves workspace empty"
                likely_cause: "Atomic rename failed, backup was deleted"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/golden_master.rs"
                        function:      "rollback_impl()"
                        what_to_check: "Is backup restored on rename failure?"
                    },
                ]
                fix_pattern: "Only delete backup AFTER successful rename to workspace"
            },
            {
                symptom:      "Memory exhaustion on large workspaces"
                likely_cause: "Loading entire file content into memory for diff"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/golden_master.rs"
                        function:      "files_differ()"
                        what_to_check: "Is streaming hash used instead of full read?"
                    },
                ]
                fix_pattern: "Use streaming sha256 with buffer, don't read entire file"
            },
            {
                symptom:      "Snapshot metadata JSON parse error"
                likely_cause: "Concurrent write corrupted metadata"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/golden_master.rs"
                        function:      "write_metadata()"
                        what_to_check: "Is atomic write (temp + rename) used?"
                    },
                ]
                fix_pattern: "Write to temp file, atomic rename to metadata.json"
            },
        ]

        debugging_commands: [
            {
                scenario: "When snapshot creation fails"
                run:      "RUST_LOG=factory_core::golden_master=debug cargo test"
                look_for: "Error message from cp command, filesystem type"
            },
            {
                scenario: "When reflinks not working"
                run:      "stat -f . | grep -i type"
                look_for: "Filesystem type (btrfs, xfs, apfs support reflinks)"
            },
            {
                scenario: "When diff is incorrect"
                run:      "diff -r snapshot_path workspace_path"
                look_for: "System diff output to compare with ours"
            },
            {
                scenario: "When rollback fails"
                run:      "ls -la workspace.rollback-backup workspace.rollback-temp"
                look_for: "Temp/backup directories left behind"
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
            "[ ] E2E pipeline test passing with real data",
            "[ ] No mocks or fake data in any test",
            "[ ] test_create_golden_master_snapshot passes",
            "[ ] test_diff_against_golden_master passes",
            "[ ] test_rollback_to_snapshot passes",
            "[ ] test_cow_reflink_creation passes",
            "[ ] test_concurrent_snapshot_serialization passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All preconditions validated",
            "[ ] All postconditions guaranteed",
            "[ ] GoldenMaster actor created",
            "[ ] SnapshotId type created",
            "[ ] DiffResult type created",
            "[ ] COW reflink support implemented",
            "[ ] Atomic rollback implemented",
            "[ ] Garbage collection implemented",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in golden_master.rs",
            "[ ] Example usage in doc comments",
            "[ ] Panic-free guarantee documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add golden_master"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add snapshot/diff/rollback variants"
            },
            {
                path:      "crates/factory-core/src/worktree.rs"
                relevance: "Existing workspace management patterns"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - may need walkdir, sha2"
            },
        ]

        dependencies: [
            {
                bead_id:   "factory-pa8"
                title:     "Implement JJ-native workspace manager actor"
                why:       "GoldenMaster subscribes to workspace creation events from WorkspaceManager"
            },
        ]

        external_references: [
            "https://man7.org/linux/man-pages/man2/copy_file_range.2.html - Reflink syscall",
            "https://btrfs.readthedocs.io/en/latest/Reflink.html - btrfs reflink documentation",
            "https://docs.rs/walkdir - Directory traversal crate",
            "https://docs.rs/sha2 - SHA-256 hashing for file comparison",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Newtype Validation"
                example_location: "crates/factory-core/src/domain.rs:Slug"
                how_to_apply:     "Wrap SnapshotId in newtype with validation"
            },
            {
                pattern:          "Atomic File Operations"
                example_location: "crates/factory-core/src/persistence.rs"
                how_to_apply:     "Write to temp, atomic rename for consistency"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use cp --reflink=auto for COW snapshots on Linux",
            "Use cp -c for COW snapshots on macOS (APFS)",
            "Implement atomic rollback with backup + rename pattern",
            "Use walkdir crate for recursive directory traversal",
            "Use sha2 crate for content comparison (not file metadata)",
            "Serialize concurrent operations via actor mailbox",
            "Add #[must_use] to SnapshotId and DiffResult",
            "Use tempfile crate for atomic write operations",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT read entire large files into memory (stream)",
            "Do NOT compare files by metadata (timestamps/permissions)",
            "Do NOT leave partial snapshots on disk after errors",
            "Do NOT allow concurrent snapshot writes to same target",
        ]

        code_patterns: [
            {
                name:     "SnapshotId Newtype"
                use_when: "Creating or referencing snapshots"
                example:  """
                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
                    pub struct SnapshotId(String);

                    impl SnapshotId {
                        pub fn generate() -> Self {
                            let now = chrono::Utc::now();
                            let id = format!("{}-{}", now.format("%Y%m%d-%H%M%S"), nanoid::nanoid!(8));
                            Self(id)
                        }

                        pub fn new(id: impl Into<String>) -> Result<Self> {
                            let id = id.into();
                            if id.is_empty() {
                                return Err(Error::InvalidSnapshotId { reason: "empty" });
                            }
                            Ok(Self(id))
                        }

                        pub fn as_str(&self) -> &str {
                            &self.0
                        }
                    }
                    """
            },
            {
                name:     "Atomic File Copy"
                use_when: "Writing metadata or performing rollback"
                example:  """
                    async fn atomic_write(path: &Path, content: &[u8]) -> Result<()> {
                        let temp_path = path.with_extension("tmp");

                        // Write to temp file
                        tokio::fs::write(&temp_path, content)
                            .await
                            .map_err(|e| Error::file_write_failed(&temp_path, e.to_string()))?;

                        // Atomic rename
                        tokio::fs::rename(&temp_path, path)
                            .await
                            .map_err(|e| Error::file_write_failed(path, e.to_string()))?;

                        Ok(())
                    }
                    """
            },
            {
                name:     "Streaming File Hash"
                use_when: "Comparing files for diff"
                example:  """
                    use sha2::{Sha256, Digest};
                    use tokio::io::AsyncReadExt;

                    async fn file_hash(path: &Path) -> Result<[u8; 32]> {
                        let mut file = tokio::fs::File::open(path).await?;
                        let mut hasher = Sha256::new();
                        let mut buffer = [0u8; 8192];

                        loop {
                            let n = file.read(&mut buffer).await?;
                            if n == 0 { break; }
                            hasher.update(&buffer[..n]);
                        }

                        Ok(hasher.finalize().into())
                    }

                    fn files_differ(path1: &Path, path2: &Path) -> Result<bool> {
                        let hash1 = file_hash(path1).await?;
                        let hash2 = file_hash(path2).await?;
                        Ok(hash1 != hash2)
                    }
                    """
            },
            {
                name:     "GoldenMaster Actor Handle"
                use_when: "Creating client-facing API"
                example:  """
                    #[derive(Clone)]
                    pub struct GoldenMasterRef {
                        inner: ActorRef<GoldenMasterMsg>,
                    }

                    impl GoldenMasterRef {
                        pub async fn create_snapshot(&self, source: &Path) -> Result<SnapshotId> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(GoldenMasterMsg::CreateSnapshot {
                                source: source.to_path_buf(),
                                respond_to: tx.into(),
                            })?;
                            rx.await.map_err(|_| Error::ActorMailboxClosed)?
                        }

                        pub async fn diff(&self, workspace: &Path, snapshot_id: &SnapshotId) -> Result<DiffResult> {
                            let (tx, rx) = oneshot::channel();
                            self.inner.cast(GoldenMasterMsg::Diff {
                                workspace: workspace.to_path_buf(),
                                snapshot_id: snapshot_id.clone(),
                                respond_to: tx.into(),
                            })?;
                            rx.await.map_err(|_| Error::ActorMailboxClosed)?
                        }
                    }
                    """
            },
        ]
    }
}
