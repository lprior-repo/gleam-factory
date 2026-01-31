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

"factory-86r": #ValidBead & {
    // ============================================================================
    // BEAD: factory-86r - Implement merge queue for ordered patch integration
    // ============================================================================

    id:              "factory-86r"
    title:           "Merge Queue: Implement ordered patch integration with conflict detection"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["actors", "queue", "merge", "conflict-detection", "P2"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL maintain an ordered buffer of patches awaiting integration",
            "THE SYSTEM SHALL process patches in FIFO order (first proposed, first merged)",
            "THE SYSTEM SHALL correlate test results with the correct patch hash",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL broadcast signals via SignalBus for all patch state transitions",
        ]

        event_driven: [
            {
                trigger: "WHEN a patch is proposed via HandlePatchProposed"
                shall:   "THE SYSTEM SHALL add the patch to the queue if not already absorbing"
            },
            {
                trigger: "WHEN a patch is proposed while already absorbing"
                shall:   "THE SYSTEM SHALL reject the patch and log the rejection"
            },
            {
                trigger: "WHEN PatchTestResult arrives with passed=true for current patch"
                shall:   "THE SYSTEM SHALL broadcast PatchAccepted and clear absorbing state"
            },
            {
                trigger: "WHEN PatchTestResult arrives with passed=false for current patch"
                shall:   "THE SYSTEM SHALL broadcast PatchRejected and clear absorbing state"
            },
            {
                trigger: "WHEN PatchTestResult arrives for a non-current patch"
                shall:   "THE SYSTEM SHALL ignore the result (stale correlation)"
            },
            {
                trigger: "WHEN Shutdown message is received"
                shall:   "THE SYSTEM SHALL gracefully stop processing and release resources"
            },
        ]

        state_driven: [
            {
                state: "WHILE absorbing is true"
                shall: "THE SYSTEM SHALL reject new patch proposals until current patch completes"
            },
            {
                state: "WHILE absorbing is false"
                shall: "THE SYSTEM SHALL accept the next patch proposal"
            },
            {
                state: "WHILE a patch is being processed"
                shall: "THE SYSTEM SHALL maintain correlation between patch hash and test results"
            },
        ]

        unwanted: [
            {
                condition: "IF multiple patches arrive simultaneously"
                shall_not: "THE SYSTEM SHALL NOT process them in parallel"
                because:   "Serial merge execution prevents merge conflicts and race conditions"
            },
            {
                condition: "IF a test result arrives for an unknown patch hash"
                shall_not: "THE SYSTEM SHALL NOT apply the result to the current patch"
                because:   "Test result correlation must be exact to prevent incorrect merges"
            },
            {
                condition: "IF the actor panics during message handling"
                shall_not: "THE SYSTEM SHALL NOT leave the queue in an inconsistent state"
                because:   "State corruption breaks the merge pipeline"
            },
            {
                condition: "IF absorbing state is corrupted"
                shall_not: "THE SYSTEM SHALL NOT allow concurrent patch processing"
                because:   "Would cause merge conflicts and data loss"
            },
        ]

        complex: [
            {
                state:   "WHILE the system is shutting down"
                trigger: "WHEN a new patch is proposed"
                shall:   "THE SYSTEM SHALL reject with Error::ShuttingDown"
            },
            {
                state:   "WHILE absorbing a patch"
                trigger: "WHEN a conflicting patch is detected"
                shall:   "THE SYSTEM SHALL reject current patch and broadcast PatchRejected"
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
                    field:           "signal_bus"
                    type:            "ActorRef<SignalBusMessage>"
                    constraints:     "Must be a valid, running SignalBus actor reference"
                    example_valid:   "ActorRef from SignalBus::spawn()"
                    example_invalid: "Dropped or stopped actor reference"
                },
                {
                    field:           "patch_hash"
                    type:            "String"
                    constraints:     "Must be a valid 40-char hex git commit hash"
                    example_valid:   "a1b2c3d4e5f6789012345678901234567890abcd"
                    example_invalid: "short-hash"
                },
            ]
            system_state: [
                "SignalBus actor is running and accessible",
                "factory-c2s (FactoryLoop) dependency is available",
                "ractor actor framework is initialized",
                "Tokio runtime is active",
            ]
        }

        postconditions: {
            state_changes: [
                "MergeQueue actor spawned and registered",
                "Signal subscriptions established for patch events",
                "Absorbing state correctly tracks current patch",
                "PatchAccepted/PatchRejected signals broadcast on completion",
            ]
            return_guarantees: [
                {
                    field:     "MergeQueue::spawn()"
                    guarantee: "Returns Result<ActorRef<MergeQueueMessage>, SpawnError>"
                },
                {
                    field:     "is_absorbing()"
                    guarantee: "Returns bool indicating if queue is processing a patch"
                },
                {
                    field:     "get_current_patch()"
                    guarantee: "Returns Option<PatchHash> of currently absorbing patch"
                },
                {
                    field:     "propose_patch()"
                    guarantee: "Returns Result<(), ProposeError> - fails if already absorbing"
                },
                {
                    field:     "report_test_result()"
                    guarantee: "Returns Result<(), Error> - correlates result with patch"
                },
            ]
            side_effects: [
                "SignalBus receives PatchProposed when absorption starts",
                "SignalBus receives PatchAccepted when tests pass",
                "SignalBus receives PatchRejected when tests fail",
                "Logging captures all state transitions",
            ]
        }

        invariants: [
            "At most one patch can be absorbing at any time",
            "Patch hash in test result must match current_patch_hash for acceptance",
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Actor state is only mutated inside message handlers",
            "absorbing=true implies current_patch_hash.is_some()",
            "absorbing=false implies current_patch_hash.is_none()",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "SignalBus not available when MergeQueue starts"
                prevention:  "Require SignalBus ActorRef in constructor, fail fast on invalid ref"
                test_for_it: "test_spawn_fails_without_signal_bus"
            },
            {
                failure:     "Test results arrive after queue shutdown"
                prevention:  "Check actor liveness before processing, ignore stale results"
                test_for_it: "test_ignores_results_after_shutdown"
            },
            {
                failure:     "Race between patch proposal and test result"
                prevention:  "Single-threaded actor model prevents races; hash correlation validates"
                test_for_it: "test_concurrent_proposal_and_result"
            },
            {
                failure:     "FactoryLoop sends patches faster than queue can process"
                prevention:  "Absorbing state gate rejects overlapping patches; FactoryLoop must wait"
                test_for_it: "test_rejects_while_absorbing"
            },
        ]

        usability_failures: [
            {
                failure:     "Caller cannot tell if patch was accepted or rejected"
                prevention:  "Return explicit Result<Accepted|Rejected, Error> from propose_patch"
                test_for_it: "test_propose_returns_clear_result"
            },
            {
                failure:     "No visibility into queue state for debugging"
                prevention:  "Provide GetState query that returns full MergeQueueState snapshot"
                test_for_it: "test_get_state_returns_snapshot"
            },
            {
                failure:     "Error messages don't indicate why patch was rejected"
                prevention:  "Include patch hash and reason in PatchRejected signal"
                test_for_it: "test_rejection_includes_reason"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Test result applied to wrong patch due to hash mismatch"
                prevention:  "Strict hash comparison; ignore results for non-current patches"
                test_for_it: "test_ignores_stale_test_results"
            },
            {
                failure:     "absorbing state desyncs from current_patch_hash"
                prevention:  "Always update both atomically in single state transition"
                test_for_it: "test_absorbing_state_consistency"
            },
            {
                failure:     "Queue processes patches out of order"
                prevention:  "Single-patch-at-a-time model guarantees FIFO for proposal acceptance"
                test_for_it: "test_fifo_ordering"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_propose_patch_starts_absorbing"
                given: "MergeQueue is idle (not absorbing)"
                when:  "propose_patch() is called with valid hash"
                then: [
                    "is_absorbing() returns true",
                    "get_current_patch() returns the proposed hash",
                    "SignalBus receives PatchProposed signal",
                ]
                real_input: """
                    let queue = MergeQueue::spawn(signal_bus.clone()).await?;
                    let hash = PatchHash::new("a1b2c3d4e5f6789012345678901234567890abcd")?;
                    queue.cast(MergeQueueMessage::HandlePatchProposed { hash: hash.clone() })?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    assert!(queue.call(MergeQueueMessage::GetAbsorbing).await?);
                    assert_eq!(queue.call(MergeQueueMessage::GetCurrentPatch).await?, Some(hash));
                    """
            },
            {
                name:  "test_patch_accepted_on_test_pass"
                given: "MergeQueue is absorbing a patch"
                when:  "PatchTestResult with passed=true arrives for current patch"
                then: [
                    "is_absorbing() returns false",
                    "get_current_patch() returns None",
                    "SignalBus receives PatchAccepted signal with hash and timestamp",
                ]
                real_input: """
                    queue.cast(MergeQueueMessage::HandlePatchProposed { hash: hash.clone() })?;
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    queue.cast(MergeQueueMessage::PatchTestResult { hash: hash.clone(), passed: true })?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    assert!(!queue.call(MergeQueueMessage::GetAbsorbing).await?);
                    // Verify PatchAccepted signal was broadcast
                    """
            },
            {
                name:  "test_patch_rejected_on_test_fail"
                given: "MergeQueue is absorbing a patch"
                when:  "PatchTestResult with passed=false arrives for current patch"
                then: [
                    "is_absorbing() returns false",
                    "get_current_patch() returns None",
                    "SignalBus receives PatchRejected signal with reason",
                ]
                real_input: """
                    queue.cast(MergeQueueMessage::HandlePatchProposed { hash: hash.clone() })?;
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    queue.cast(MergeQueueMessage::PatchTestResult { hash: hash.clone(), passed: false })?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    assert!(!queue.call(MergeQueueMessage::GetAbsorbing).await?);
                    // Verify PatchRejected signal with "Tests failed" reason
                    """
            },
            {
                name:  "test_graceful_shutdown"
                given: "MergeQueue is running"
                when:  "Shutdown message is sent"
                then: [
                    "Actor stops processing",
                    "No resource leaks",
                    "Shutdown logged",
                ]
                real_input: """
                    queue.cast(MergeQueueMessage::Shutdown)?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    assert!(queue.cast(MergeQueueMessage::GetAbsorbing).is_err());  // Actor stopped
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_rejects_patch_while_absorbing"
                given: "MergeQueue is currently absorbing a patch"
                when:  "Another patch proposal arrives"
                then: [
                    "New patch is rejected",
                    "Original patch continues absorbing",
                    "Rejection is logged",
                ]
                real_input: """
                    let hash1 = PatchHash::new("a1b2c3d4e5f6789012345678901234567890abcd")?;
                    let hash2 = PatchHash::new("b2c3d4e5f6789012345678901234567890abcde")?;
                    queue.cast(MergeQueueMessage::HandlePatchProposed { hash: hash1.clone() })?;
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    queue.cast(MergeQueueMessage::HandlePatchProposed { hash: hash2.clone() })?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    assert_eq!(queue.call(MergeQueueMessage::GetCurrentPatch).await?, Some(hash1));
                    """
                expected_error: null
            },
            {
                name:  "test_ignores_stale_test_results"
                given: "MergeQueue is absorbing patch A"
                when:  "Test result arrives for different patch B"
                then: [
                    "Result is ignored",
                    "Patch A continues absorbing",
                    "No signals broadcast",
                ]
                real_input: """
                    let hash_a = PatchHash::new("a1b2c3d4e5f6789012345678901234567890abcd")?;
                    let hash_b = PatchHash::new("b2c3d4e5f6789012345678901234567890abcde")?;
                    queue.cast(MergeQueueMessage::HandlePatchProposed { hash: hash_a.clone() })?;
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    queue.cast(MergeQueueMessage::PatchTestResult { hash: hash_b, passed: true })?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    assert!(queue.call(MergeQueueMessage::GetAbsorbing).await?);
                    assert_eq!(queue.call(MergeQueueMessage::GetCurrentPatch).await?, Some(hash_a));
                    """
                expected_error: null
            },
            {
                name:  "test_ignores_result_when_not_absorbing"
                given: "MergeQueue is idle (not absorbing)"
                when:  "Test result arrives"
                then: [
                    "Result is ignored",
                    "Queue remains idle",
                    "No signals broadcast",
                ]
                real_input: """
                    let hash = PatchHash::new("a1b2c3d4e5f6789012345678901234567890abcd")?;
                    queue.cast(MergeQueueMessage::PatchTestResult { hash, passed: true })?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(10)).await;
                    assert!(!queue.call(MergeQueueMessage::GetAbsorbing).await?);
                    """
                expected_error: null
            },
        ]

        edge_cases: [
            {
                name:     "test_rapid_accept_reject_cycle"
                scenario: "Multiple patches proposed and resolved in quick succession"
                input:    "Propose patch A, pass tests, propose patch B, fail tests, propose patch C"
                expected: "Each patch processed correctly in sequence, correct signals for each"
            },
            {
                name:     "test_empty_hash_rejected"
                scenario: "Patch proposed with empty hash string"
                input:    "HandlePatchProposed with empty string"
                expected: "Validation error returned, queue remains idle"
            },
            {
                name:     "test_query_during_transition"
                scenario: "GetAbsorbing query arrives during state transition"
                input:    "Concurrent GetAbsorbing and PatchTestResult"
                expected: "Consistent state returned (actor serializes messages)"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in merge_queue.rs"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/merge_queue.rs returns empty"
            },
            {
                name:     "test_invariant_absorbing_consistency"
                verifies: "absorbing=true iff current_patch_hash.is_some()"
                test:     "Property-based test with arbitrary state transitions"
            },
            {
                name:     "test_postcondition_signals_broadcast"
                verifies: "All state transitions broadcast appropriate signals"
                test:     "Mock SignalBus receives expected signals for each transition"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_merge_queue_lifecycle"
            description: "Complete merge queue lifecycle: spawn -> propose -> test -> accept/reject -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/merge_queue_e2e.rs"
                        content: """
                            use factory_core::merge_queue::{MergeQueue, MergeQueueMessage, PatchHash};
                            use factory_core::signal_bus::SignalBus;
                            use factory_core::error::Result;

                            #[tokio::test]
                            async fn test_full_merge_queue_lifecycle() -> Result<()> {
                                // 1. Start SignalBus
                                let signal_bus = SignalBus::spawn().await?;

                                // 2. Start MergeQueue
                                let queue = MergeQueue::spawn(signal_bus.clone()).await?;

                                // 3. Propose a patch
                                let hash = PatchHash::new("a1b2c3d4e5f6789012345678901234567890abcd")?;
                                queue.cast(MergeQueueMessage::HandlePatchProposed { hash: hash.clone() })?;
                                tokio::time::sleep(std::time::Duration::from_millis(10)).await;

                                // 4. Verify absorbing
                                assert!(queue.call(MergeQueueMessage::GetAbsorbing).await?);

                                // 5. Report test passed
                                queue.cast(MergeQueueMessage::PatchTestResult { hash, passed: true })?;
                                tokio::time::sleep(std::time::Duration::from_millis(10)).await;

                                // 6. Verify accepted
                                assert!(!queue.call(MergeQueueMessage::GetAbsorbing).await?);

                                // 7. Shutdown
                                queue.cast(MergeQueueMessage::Shutdown)?;

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
                command:    "moon run factory-core:test -- --test merge_queue_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_merge_queue_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/merge_queue.rs"
                        contains: "pub struct MergeQueue"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/merge_queue_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_integration_with_factory_loop"
                description: "Verify MergeQueue integrates with FactoryLoop for patch submission"
                steps: [
                    {action: "Start SignalBus", verify: "SignalBus running"},
                    {action: "Start MergeQueue with SignalBus ref", verify: "MergeQueue spawned"},
                    {action: "FactoryLoop proposes patch", verify: "MergeQueue absorbing"},
                    {action: "Test runner reports success", verify: "PatchAccepted broadcast"},
                    {action: "FactoryLoop receives PatchAccepted", verify: "Loop advances to next phase"},
                ]
            },
            {
                name:        "e2e_conflict_detection_flow"
                description: "Verify conflict detection rejects overlapping patches"
                steps: [
                    {action: "FactoryLoop A proposes patch", verify: "Absorbing started"},
                    {action: "FactoryLoop B proposes patch", verify: "Patch rejected (already absorbing)"},
                    {action: "Patch A tests pass", verify: "PatchAccepted for A"},
                    {action: "FactoryLoop B retries", verify: "Patch B now absorbing"},
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
                task:      "Write test: test_spawn_merge_queue"
                file:      "crates/factory-core/src/merge_queue.rs"
                what:      "Test that MergeQueue::spawn returns valid ActorRef"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_propose_patch_starts_absorbing"
                file:      "crates/factory-core/src/merge_queue.rs"
                what:      "Test that proposing a patch sets absorbing=true"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_rejects_while_absorbing"
                file:      "crates/factory-core/src/merge_queue.rs"
                what:      "Test that second patch is rejected while absorbing"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_patch_accepted_clears_absorbing"
                file:      "crates/factory-core/src/merge_queue.rs"
                what:      "Test that passing test result clears absorbing state"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_ignores_stale_results"
                file:      "crates/factory-core/src/merge_queue.rs"
                what:      "Test that wrong hash results are ignored"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define MergeQueueState struct"
                file: "crates/factory-core/src/merge_queue.rs"
                what: """
                    struct MergeQueueState {
                        absorbing: bool,
                        signal_bus: ActorRef<SignalBusMessage>,
                        current_patch_hash: Option<PatchHash>,
                    }
                    """
                done_when:     "Struct compiles"
                patterns_to_use: ["Validated PatchHash newtype"]
            },
            {
                task: "Define MergeQueueMessage enum"
                file: "crates/factory-core/src/merge_queue.rs"
                what: """
                    pub enum MergeQueueMessage {
                        Shutdown,
                        GetAbsorbing { respond_to: RpcReplyPort<bool> },
                        GetCurrentPatch { respond_to: RpcReplyPort<Option<PatchHash>> },
                        HandlePatchProposed { hash: PatchHash },
                        PatchTestResult { hash: PatchHash, passed: bool },
                    }
                    """
                done_when:     "Enum compiles with all message types"
                patterns_to_use: ["ractor RpcReplyPort for call/response"]
            },
            {
                task: "Implement MergeQueue actor"
                file: "crates/factory-core/src/merge_queue.rs"
                what: """
                    Implement ractor::Actor trait for MergeQueue.
                    Handle each message variant with Railway-Oriented error handling.
                    Broadcast signals via SignalBus on state transitions.
                    """
                done_when:     "Actor handles all message types"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "match on message variants",
                    "signal_bus.cast() for broadcasts",
                ]
            },
            {
                task: "Implement PatchHash validated newtype"
                file: "crates/factory-core/src/merge_queue.rs"
                what: """
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct PatchHash(String);

                    impl PatchHash {
                        pub fn new(s: impl Into<String>) -> Result<Self> {
                            let s = s.into();
                            if s.len() != 40 || !s.chars().all(|c| c.is_ascii_hexdigit()) {
                                return Err(Error::InvalidPatchHash { reason: "..." });
                            }
                            Ok(Self(s.to_lowercase()))
                        }
                    }
                    """
                done_when:     "PatchHash validates correctly"
                patterns_to_use: ["Opaque newtype pattern from domain.rs"]
            },
            {
                task: "Add convenience functions"
                file: "crates/factory-core/src/merge_queue.rs"
                what: """
                    pub async fn is_absorbing(queue: &ActorRef<MergeQueueMessage>) -> Result<bool>
                    pub async fn get_current_patch(queue: &ActorRef<MergeQueueMessage>) -> Result<Option<PatchHash>>
                    pub fn propose_patch(queue: &ActorRef<MergeQueueMessage>, hash: PatchHash) -> Result<()>
                    pub fn report_test_result(queue: &ActorRef<MergeQueueMessage>, hash: PatchHash, passed: bool) -> Result<()>
                    pub fn shutdown(queue: &ActorRef<MergeQueueMessage>) -> Result<()>
                    """
                done_when:     "All convenience functions compile"
                patterns_to_use: ["Async for call, sync for cast"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export merge_queue from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod merge_queue;"
                done_when: "External crates can import factory_core::merge_queue"
            },
            {
                task:      "Add merge queue errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "MergeQueueSpawnFailed, InvalidPatchHash, AlreadyAbsorbing variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Add MergeQueue signals to signal_bus"
                file:      "crates/factory-core/src/signal_bus.rs"
                what:      "PatchProposed, PatchAccepted(hash, timestamp), PatchRejected(reason) signals"
                done_when: "Signal variants added and broadcast works"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/merge_queue.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Property-based test for state consistency"
                done_when: "proptest verifies absorbing/current_patch invariant"
                commands: [
                    "moon run factory-core:test -- --test merge_queue_proptest",
                ]
                expected: "All property tests pass"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "MergeQueue::spawn returns Error"
                likely_cause: "SignalBus ActorRef is invalid or actor is not running"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/merge_queue.rs"
                        function:      "spawn()"
                        what_to_check: "Is signal_bus.cast() returning Ok?"
                    },
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "spawn()"
                        what_to_check: "Was SignalBus started before MergeQueue?"
                    },
                ]
                fix_pattern: "Ensure SignalBus is spawned first and ActorRef is passed correctly"
            },
            {
                symptom:      "Patch proposal silently dropped"
                likely_cause: "Queue is already absorbing; rejection not visible to caller"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/merge_queue.rs"
                        function:      "handle_message() for HandlePatchProposed"
                        what_to_check: "Is absorbing check working correctly?"
                    },
                ]
                fix_pattern: "Check is_absorbing() before proposing; use call() instead of cast() for feedback"
            },
            {
                symptom:      "Test results never processed"
                likely_cause: "Hash mismatch between proposed and result"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/merge_queue.rs"
                        function:      "handle_message() for PatchTestResult"
                        what_to_check: "Is hash comparison case-sensitive? Using eq correctly?"
                    },
                ]
                fix_pattern: "Ensure both hashes are normalized (lowercase) via PatchHash::new()"
            },
            {
                symptom:      "Signals not received by subscribers"
                likely_cause: "SignalBus subscription not established"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "subscribe()"
                        what_to_check: "Is subscriber added to correct signal type?"
                    },
                ]
                fix_pattern: "Verify subscription before first patch proposal"
            },
        ]

        debugging_commands: [
            {
                scenario: "When patches are not processed"
                run:      "RUST_LOG=factory_core::merge_queue=debug cargo test"
                look_for: "Message handling logs, state transitions"
            },
            {
                scenario: "When signals not broadcast"
                run:      "RUST_LOG=factory_core::signal_bus=debug cargo test"
                look_for: "Broadcast calls, subscriber notifications"
            },
            {
                scenario: "When spawn fails"
                run:      "RUST_LOG=ractor=debug cargo test -- --nocapture 2>&1 | head -100"
                look_for: "Actor spawn errors, supervisor failures"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_spawn_merge_queue passes",
            "[ ] test_propose_patch_starts_absorbing passes",
            "[ ] test_rejects_while_absorbing passes",
            "[ ] test_patch_accepted_clears_absorbing passes",
            "[ ] test_patch_rejected_clears_absorbing passes",
            "[ ] test_ignores_stale_results passes",
            "[ ] test_graceful_shutdown passes",
            "[ ] test_query_absorbing_state passes",
            "[ ] test_query_current_patch passes",
            "[ ] E2E test with SignalBus integration passes",
        ]

        code: [
            "[ ] MergeQueueState struct defined",
            "[ ] MergeQueueMessage enum with all variants",
            "[ ] PatchHash validated newtype implemented",
            "[ ] ractor::Actor trait implemented for MergeQueue",
            "[ ] All public APIs return Result<T, Error>",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Signal broadcasts for PatchProposed/Accepted/Rejected",
            "[ ] Convenience functions (is_absorbing, propose_patch, etc.)",
            "[ ] Logging for all state transitions",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level doc comment in merge_queue.rs",
            "[ ] Doc comments on MergeQueueMessage variants",
            "[ ] Doc comments on public functions",
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
                relevance: "Module exports - must add merge_queue"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add MergeQueue error variants"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Pattern reference for validated newtypes (Slug, GitHash)"
            },
            {
                path:      "src/merge_queue.gleam"
                relevance: "Original Gleam implementation to port"
            },
            {
                path:      "src/signal_bus.gleam"
                relevance: "SignalBus patterns for signal types"
            },
            {
                path:      "src/factory_loop.gleam"
                relevance: "FactoryLoop integration patterns"
            },
            {
                path:      "BEAM-PATTERNS-RESEARCH.md"
                relevance: "Actor patterns and OTP principles"
            },
        ]

        dependencies: [
            {
                id:          "factory-c2s"
                name:        "Factory Loop"
                relevance:   "FactoryLoop proposes patches to MergeQueue"
                integration: "FactoryLoop holds MergeQueue ActorRef, calls propose_patch()"
            },
            {
                id:          "factory-kou"
                name:        "Signal Bus"
                relevance:   "MergeQueue broadcasts patch events via SignalBus"
                integration: "MergeQueue holds SignalBus ActorRef, broadcasts signals"
            },
        ]

        external_references: [
            "https://github.com/slawlor/ractor - Official ractor repo",
            "https://docs.rs/ractor - ractor API docs",
            "https://hexdocs.pm/gleam_otp/gleam/otp/actor.html - Gleam OTP actor reference",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Opaque Validated Newtype"
                example_location: "crates/factory-core/src/domain.rs:Slug"
                how_to_apply:     "PatchHash follows same pattern as GitHash"
            },
            {
                pattern:          "Actor Message Enum"
                example_location: "src/merge_queue.gleam:MergeQueueMessage"
                how_to_apply:     "Port Gleam variants to Rust enum with ractor RpcReplyPort"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use ractor 0.15+ for actor framework",
            "Port directly from merge_queue.gleam structure",
            "Use PatchHash validated newtype (like GitHash in domain.rs)",
            "Use RpcReplyPort for GetAbsorbing/GetCurrentPatch calls",
            "Use cast() for fire-and-forget (HandlePatchProposed, Shutdown)",
            "Add #[must_use] to ActorRef return values",
            "Use tokio::time::timeout for call operations",
            "Log all state transitions with tracing",
            "Test hash correlation thoroughly (stale results are common bugs)",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT allow absorbing=true with current_patch_hash=None",
            "Do NOT process test results for wrong patch hash",
            "Do NOT allow parallel patch processing",
        ]

        code_patterns: [
            {
                name:     "MergeQueue Actor State"
                use_when: "Defining the actor's internal state"
                example:  """
                    struct MergeQueueState {
                        absorbing: bool,
                        signal_bus: ActorRef<SignalBusMessage>,
                        current_patch_hash: Option<PatchHash>,
                    }

                    impl Default for MergeQueueState {
                        fn default() -> Self {
                            Self {
                                absorbing: false,
                                signal_bus: /* requires initialization */,
                                current_patch_hash: None,
                            }
                        }
                    }
                    """
            },
            {
                name:     "Message Handler Pattern"
                use_when: "Implementing handle() for actor messages"
                example:  """
                    async fn handle(
                        &self,
                        myself: ActorRef<MergeQueueMessage>,
                        message: MergeQueueMessage,
                        state: &mut MergeQueueState,
                    ) -> Result<(), ActorProcessingErr> {
                        match message {
                            MergeQueueMessage::HandlePatchProposed { hash } => {
                                if state.absorbing {
                                    tracing::info!(?hash, "Patch rejected: already absorbing");
                                    return Ok(());
                                }
                                state.absorbing = true;
                                state.current_patch_hash = Some(hash.clone());
                                state.signal_bus.cast(SignalBusMessage::Publish(Signal::PatchProposed))?;
                                tracing::info!(?hash, "Patch proposed, absorbing started");
                                Ok(())
                            }
                            MergeQueueMessage::PatchTestResult { hash, passed } => {
                                match (&state.current_patch_hash, passed) {
                                    (Some(current), true) if current == &hash => {
                                        handle_patch_accepted(state, &hash).await
                                    }
                                    (Some(current), false) if current == &hash => {
                                        handle_patch_rejected(state, &hash).await
                                    }
                                    _ => {
                                        tracing::debug!(?hash, "Ignoring stale test result");
                                        Ok(())
                                    }
                                }
                            }
                            // ... other variants
                        }
                    }
                    """
            },
            {
                name:     "Patch Accepted Handler"
                use_when: "Processing successful test result"
                example:  """
                    async fn handle_patch_accepted(
                        state: &mut MergeQueueState,
                        hash: &PatchHash,
                    ) -> Result<(), ActorProcessingErr> {
                        tracing::info!(?hash, "Patch accepted");
                        let accepted = PatchAccepted {
                            hash: hash.clone(),
                            merged_at: SystemTime::now()
                                .duration_since(UNIX_EPOCH)
                                .map(|d| d.as_millis() as u64)
                                .unwrap_or(0),
                        };
                        state.signal_bus.cast(SignalBusMessage::Publish(
                            Signal::PatchAccepted(accepted)
                        ))?;
                        state.absorbing = false;
                        state.current_patch_hash = None;
                        Ok(())
                    }
                    """
            },
            {
                name:     "Convenience Query Function"
                use_when: "Providing ergonomic API for callers"
                example:  """
                    pub async fn is_absorbing(
                        queue: &ActorRef<MergeQueueMessage>,
                    ) -> Result<bool> {
                        let result = queue
                            .call(
                                |reply| MergeQueueMessage::GetAbsorbing { respond_to: reply },
                                Some(Duration::from_secs(5)),
                            )
                            .await
                            .map_err(|e| Error::MergeQueueQueryFailed {
                                reason: e.to_string(),
                            })?;
                        Ok(result)
                    }
                    """
            },
        ]
    }
}
