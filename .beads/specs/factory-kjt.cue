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

"factory-kjt": #ValidBead & {
    // ============================================================================
    // BEAD: factory-kjt - Implement resource governor for slot allocation and RAM monitoring
    // ============================================================================

    id:              "factory-kjt"
    title:           "Runtime: Implement resource governor for slot allocation and RAM monitoring"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["runtime", "actors", "resources", "backpressure", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use ticket-based permits for all resource allocation",
            "THE SYSTEM SHALL check /proc/meminfo before granting RAM-sensitive permits",
            "THE SYSTEM SHALL return explicit errors when resources are exhausted (backpressure)",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL track max_mutators, max_loops, max_workspaces, and min_free_ram_mb",
        ]

        event_driven: [
            {
                trigger: "WHEN AcquireMutator message is received"
                shall:   "THE SYSTEM SHALL return MutatorTicket if slot available and RAM sufficient"
            },
            {
                trigger: "WHEN AcquireLoop message is received"
                shall:   "THE SYSTEM SHALL return LoopTicket if under max_loops limit"
            },
            {
                trigger: "WHEN AcquireWorkspace message is received"
                shall:   "THE SYSTEM SHALL return WorkspaceTicket if under max_workspaces limit"
            },
            {
                trigger: "WHEN a ticket is dropped"
                shall:   "THE SYSTEM SHALL automatically release the resource slot"
            },
            {
                trigger: "WHEN free RAM falls below min_free_ram_mb"
                shall:   "THE SYSTEM SHALL reject new allocations with Error::InsufficientMemory"
            },
            {
                trigger: "WHEN QueryCapacity message is received"
                shall:   "THE SYSTEM SHALL return current usage and available slots"
            },
        ]

        state_driven: [
            {
                state: "WHILE mutator slots are at max_mutators"
                shall: "THE SYSTEM SHALL reject new AcquireMutator requests with Error::AtCapacity"
            },
            {
                state: "WHILE the governor is tracking active tickets"
                shall: "THE SYSTEM SHALL maintain accurate counts via Arc-based RAII guards"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL reject all new acquire requests"
            },
        ]

        unwanted: [
            {
                condition: "IF a ticket is leaked without being dropped"
                shall_not: "THE SYSTEM SHALL NOT allow resource leaks"
                because:   "Leaked tickets cause permanent capacity reduction"
            },
            {
                condition: "IF memory check fails due to /proc/meminfo read error"
                shall_not: "THE SYSTEM SHALL NOT panic or block"
                because:   "I/O errors must be handled gracefully with fallback"
            },
            {
                condition: "IF multiple acquire requests arrive simultaneously"
                shall_not: "THE SYSTEM SHALL NOT grant more tickets than max capacity"
                because:   "Race conditions could cause resource exhaustion"
            },
            {
                condition: "IF the governor actor crashes"
                shall_not: "THE SYSTEM SHALL NOT leave dangling tickets"
                because:   "Orphaned tickets cause permanent capacity loss"
            },
        ]

        complex: [
            {
                state:   "WHILE a mutator ticket is held"
                trigger: "WHEN the holder panics"
                shall:   "THE SYSTEM SHALL release the slot via Drop impl on the ticket"
            },
            {
                state:   "WHILE memory is being checked"
                trigger: "WHEN concurrent acquire requests arrive"
                shall:   "THE SYSTEM SHALL serialize memory checks to prevent TOCTOU races"
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
                    field:           "ResourceGovernorConfig"
                    type:            "Struct"
                    constraints:     "Must specify max_mutators, max_loops, max_workspaces, min_free_ram_mb"
                    example_valid:   "ResourceGovernorConfig { max_mutators: 4, max_loops: 8, max_workspaces: 16, min_free_ram_mb: 512 }"
                    example_invalid: "ResourceGovernorConfig { max_mutators: 0, .. } // Zero capacity"
                },
                {
                    field:           "/proc/meminfo"
                    type:            "File"
                    constraints:     "Must be readable on Linux systems; fallback on other platforms"
                    example_valid:   "MemFree: 8192000 kB"
                    example_invalid: "File not found (non-Linux)"
                },
            ]
            system_state: [
                "Tokio runtime initialized (factory-00s)",
                "Supervisor exists to manage governor lifecycle (factory-4pz)",
                "ractor framework available for actor messaging",
            ]
        }

        postconditions: {
            state_changes: [
                "ResourceGovernor actor spawned and registered",
                "Capacity counters initialized to zero",
                "Memory monitoring initialized",
            ]
            return_guarantees: [
                {
                    field:     "ResourceGovernorRef"
                    guarantee: "Handle for sending acquire/release messages"
                },
                {
                    field:     "acquire_mutator()"
                    guarantee: "Returns Result<MutatorTicket, Error> - Ok if slot available and RAM sufficient"
                },
                {
                    field:     "acquire_loop()"
                    guarantee: "Returns Result<LoopTicket, Error> - Ok if under max_loops"
                },
                {
                    field:     "acquire_workspace()"
                    guarantee: "Returns Result<WorkspaceTicket, Error> - Ok if under max_workspaces"
                },
                {
                    field:     "query_capacity()"
                    guarantee: "Returns CapacitySnapshot with current usage and limits"
                },
                {
                    field:     "MutatorTicket"
                    guarantee: "RAII guard that releases slot on drop"
                },
            ]
            side_effects: [
                "Counter incremented on successful acquire",
                "Counter decremented on ticket drop",
                "/proc/meminfo read on RAM-sensitive operations",
            ]
        }

        invariants: [
            "active_mutators <= max_mutators at all times",
            "active_loops <= max_loops at all times",
            "active_workspaces <= max_workspaces at all times",
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Tickets cannot be cloned (enforce single ownership)",
            "Ticket drop always decrements counter exactly once",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "/proc/meminfo not available on macOS/Windows"
                prevention:  "Use cfg! to provide platform-specific implementations; fallback to unlimited on non-Linux"
                test_for_it: "test_meminfo_fallback_on_non_linux"
            },
            {
                failure:     "Ticket dropped after governor actor stops"
                prevention:  "Use Arc<AtomicUsize> for counters, not actor messages"
                test_for_it: "test_ticket_drop_after_governor_stop"
            },
            {
                failure:     "Race between acquire and ticket drop"
                prevention:  "Use AtomicUsize with compare_exchange for counter operations"
                test_for_it: "test_concurrent_acquire_release"
            },
        ]

        usability_failures: [
            {
                failure:     "Unclear why acquire failed"
                prevention:  "Return specific error variants: AtCapacity, InsufficientMemory, ShuttingDown"
                test_for_it: "test_error_messages_are_descriptive"
            },
            {
                failure:     "Hard to debug resource leaks"
                prevention:  "Add tracing spans to ticket lifecycle"
                test_for_it: "test_ticket_lifecycle_logging"
            },
            {
                failure:     "No visibility into current capacity"
                prevention:  "Expose query_capacity() for monitoring"
                test_for_it: "test_capacity_query"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Counter underflow on double-drop"
                prevention:  "Use saturating_sub and validate in debug mode"
                test_for_it: "test_no_counter_underflow"
            },
            {
                failure:     "Memory check returns stale data"
                prevention:  "Read /proc/meminfo fresh on each check (no caching)"
                test_for_it: "test_meminfo_not_cached"
            },
            {
                failure:     "Tickets created without incrementing counter"
                prevention:  "Only create tickets through governor, never directly"
                test_for_it: "test_tickets_only_from_governor"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_acquire_mutator_returns_ticket"
                given: "A resource governor with max_mutators=4 and 0 active"
                when:  "acquire_mutator() is called"
                then: [
                    "Returns Ok(MutatorTicket)",
                    "active_mutators increments to 1",
                    "Ticket contains valid guard",
                ]
                real_input: """
                    let config = ResourceGovernorConfig::new()
                        .max_mutators(4)
                        .max_loops(8)
                        .max_workspaces(16)
                        .min_free_ram_mb(512);

                    let governor = ResourceGovernor::spawn(config).await?;
                    let ticket = governor.acquire_mutator().await?;
                    """
                expected_output: """
                    assert!(ticket.is_valid());
                    let capacity = governor.query_capacity().await?;
                    assert_eq!(capacity.active_mutators, 1);
                    assert_eq!(capacity.available_mutators, 3);
                    """
            },
            {
                name:  "test_ticket_drop_releases_slot"
                given: "A resource governor with 1 active mutator"
                when:  "The MutatorTicket is dropped"
                then: [
                    "active_mutators decrements to 0",
                    "Slot becomes available for new acquire",
                ]
                real_input: """
                    let governor = spawn_governor_with_defaults().await?;

                    {
                        let _ticket = governor.acquire_mutator().await?;
                        let capacity = governor.query_capacity().await?;
                        assert_eq!(capacity.active_mutators, 1);
                    } // ticket dropped here
                    """
                expected_output: """
                    // Give Drop a moment to propagate
                    tokio::time::sleep(Duration::from_millis(10)).await;

                    let capacity = governor.query_capacity().await?;
                    assert_eq!(capacity.active_mutators, 0);
                    assert_eq!(capacity.available_mutators, 4);
                    """
            },
            {
                name:  "test_multiple_ticket_types"
                given: "A resource governor with capacity for all resource types"
                when:  "Different ticket types are acquired"
                then: [
                    "Each type tracks independently",
                    "All tickets valid simultaneously",
                ]
                real_input: """
                    let governor = spawn_governor_with_defaults().await?;

                    let mutator_ticket = governor.acquire_mutator().await?;
                    let loop_ticket = governor.acquire_loop().await?;
                    let workspace_ticket = governor.acquire_workspace().await?;
                    """
                expected_output: """
                    let capacity = governor.query_capacity().await?;
                    assert_eq!(capacity.active_mutators, 1);
                    assert_eq!(capacity.active_loops, 1);
                    assert_eq!(capacity.active_workspaces, 1);
                    """
            },
            {
                name:  "test_query_capacity_returns_snapshot"
                given: "A resource governor with known configuration"
                when:  "query_capacity() is called"
                then: [
                    "Returns CapacitySnapshot with all fields",
                    "Shows current usage and limits",
                ]
                real_input: """
                    let config = ResourceGovernorConfig::new()
                        .max_mutators(4)
                        .max_loops(8)
                        .max_workspaces(16)
                        .min_free_ram_mb(512);

                    let governor = ResourceGovernor::spawn(config).await?;
                    let capacity = governor.query_capacity().await?;
                    """
                expected_output: """
                    assert_eq!(capacity.max_mutators, 4);
                    assert_eq!(capacity.max_loops, 8);
                    assert_eq!(capacity.max_workspaces, 16);
                    assert_eq!(capacity.active_mutators, 0);
                    assert_eq!(capacity.active_loops, 0);
                    assert_eq!(capacity.active_workspaces, 0);
                    assert!(capacity.free_ram_mb > 0);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_at_capacity_returns_error"
                given: "A resource governor at max_mutators capacity"
                when:  "acquire_mutator() is called"
                then: [
                    "Returns Err(Error::AtCapacity)",
                    "Error includes resource type",
                    "Existing tickets unaffected",
                ]
                real_input: """
                    let config = ResourceGovernorConfig::new()
                        .max_mutators(2)
                        .max_loops(8)
                        .max_workspaces(16)
                        .min_free_ram_mb(0);  // Disable RAM check for this test

                    let governor = ResourceGovernor::spawn(config).await?;

                    // Fill capacity
                    let _t1 = governor.acquire_mutator().await?;
                    let _t2 = governor.acquire_mutator().await?;

                    // Try to exceed
                    let result = governor.acquire_mutator().await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(
                        result,
                        Err(Error::AtCapacity { resource: ResourceType::Mutator, .. })
                    ));
                    """
            },
            {
                name:  "test_insufficient_memory_returns_error"
                given: "A resource governor with min_free_ram_mb=999999 (unrealistic)"
                when:  "acquire_mutator() is called"
                then: [
                    "Returns Err(Error::InsufficientMemory)",
                    "Error includes required and available RAM",
                ]
                real_input: """
                    let config = ResourceGovernorConfig::new()
                        .max_mutators(4)
                        .min_free_ram_mb(999_999);  // 999 GB - definitely not available

                    let governor = ResourceGovernor::spawn(config).await?;
                    let result = governor.acquire_mutator().await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(
                        result,
                        Err(Error::InsufficientMemory { required_mb: 999_999, available_mb: _ })
                    ));
                    """
            },
            {
                name:  "test_shutting_down_rejects_requests"
                given: "A resource governor that is shutting down"
                when:  "acquire_mutator() is called"
                then: [
                    "Returns Err(Error::ShuttingDown)",
                    "Does not hang or block",
                ]
                real_input: """
                    let governor = spawn_governor_with_defaults().await?;

                    // Initiate shutdown
                    governor.shutdown().await?;

                    // Try to acquire after shutdown
                    let result = governor.acquire_mutator().await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::ShuttingDown)));
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_concurrent_acquire_respects_limits"
                scenario: "100 concurrent acquire requests for max_mutators=4"
                input:    "spawn 100 tasks each calling acquire_mutator()"
                expected: "Exactly 4 succeed, 96 return AtCapacity"
            },
            {
                name:     "test_ticket_drop_during_governor_shutdown"
                scenario: "Ticket dropped while governor is shutting down"
                input:    "Hold ticket, start shutdown, drop ticket"
                expected: "Counter still decrements correctly"
            },
            {
                name:     "test_rapid_acquire_release_cycles"
                scenario: "1000 acquire/release cycles in tight loop"
                input:    "Loop: acquire, immediately drop, repeat"
                expected: "All cycles succeed, no counter drift"
            },
            {
                name:     "test_memory_check_under_load"
                scenario: "Memory fluctuating near threshold"
                input:    "Acquire with min_free_ram_mb near actual free RAM"
                expected: "Consistent behavior (either allow or deny)"
            },
            {
                name:     "test_all_resource_types_at_capacity"
                scenario: "All three resource types at max simultaneously"
                input:    "Fill mutators, loops, and workspaces"
                expected: "Each type independently tracks capacity"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in governor module"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/governor.rs returns empty"
            },
            {
                name:     "test_invariant_counters_never_exceed_max"
                verifies: "active_X <= max_X at all times"
                test:     "Concurrent stress test verifies invariant holds"
            },
            {
                name:     "test_invariant_ticket_not_clone"
                verifies: "Tickets cannot be cloned"
                test:     "Compile error if Clone is derived or impl'd"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_governor_lifecycle"
            description: "Complete governor lifecycle: spawn -> acquire -> release -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/governor_e2e.rs"
                        content: """
                            use factory_core::governor::{ResourceGovernor, ResourceGovernorConfig};
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_full_governor_lifecycle() -> Result<()> {
                                // Spawn governor with realistic config
                                let config = ResourceGovernorConfig::new()
                                    .max_mutators(4)
                                    .max_loops(8)
                                    .max_workspaces(16)
                                    .min_free_ram_mb(256);

                                let governor = ResourceGovernor::spawn(config).await?;

                                // Verify initial state
                                let capacity = governor.query_capacity().await?;
                                assert_eq!(capacity.active_mutators, 0);

                                // Acquire tickets
                                let mutator1 = governor.acquire_mutator().await?;
                                let mutator2 = governor.acquire_mutator().await?;
                                let loop1 = governor.acquire_loop().await?;

                                // Verify usage
                                let capacity = governor.query_capacity().await?;
                                assert_eq!(capacity.active_mutators, 2);
                                assert_eq!(capacity.active_loops, 1);

                                // Drop one mutator
                                drop(mutator1);
                                tokio::time::sleep(Duration::from_millis(10)).await;

                                let capacity = governor.query_capacity().await?;
                                assert_eq!(capacity.active_mutators, 1);

                                // Graceful shutdown
                                drop(mutator2);
                                drop(loop1);
                                governor.shutdown().await?;

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
                command:    "moon run factory-core:test -- --test governor_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_governor_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/governor.rs"
                        contains: "pub struct ResourceGovernor"
                    },
                    {
                        path:     "crates/factory-core/src/governor.rs"
                        contains: "pub struct MutatorTicket"
                    },
                    {
                        path:     "crates/factory-core/src/governor.rs"
                        contains: "pub struct CapacitySnapshot"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/governor_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_backpressure_under_load"
                description: "Verify backpressure correctly throttles under heavy load"
                steps: [
                    {action: "Spawn governor with max_mutators=4", verify: "Governor running"},
                    {action: "Spawn 10 workers each trying to acquire mutator", verify: "Only 4 succeed initially"},
                    {action: "First worker completes and drops ticket", verify: "5th worker now succeeds"},
                    {action: "All workers complete", verify: "All 10 eventually complete"},
                ]
            },
            {
                name:        "e2e_memory_pressure_response"
                description: "Verify governor responds to memory pressure"
                steps: [
                    {action: "Spawn governor with min_free_ram_mb=512", verify: "Governor running"},
                    {action: "Allocate large Vec to consume RAM", verify: "Free RAM decreases"},
                    {action: "Try to acquire mutator", verify: "May fail if RAM insufficient"},
                    {action: "Free the large Vec", verify: "Acquire succeeds again"},
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
                task:      "Write test: test_acquire_mutator_returns_ticket"
                file:      "crates/factory-core/src/governor.rs"
                what:      "Test that acquire_mutator returns valid ticket"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_ticket_drop_releases_slot"
                file:      "crates/factory-core/src/governor.rs"
                what:      "Test that dropping ticket decrements counter"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_at_capacity_returns_error"
                file:      "crates/factory-core/src/governor.rs"
                what:      "Test that acquiring beyond capacity returns error"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_insufficient_memory_returns_error"
                file:      "crates/factory-core/src/governor.rs"
                what:      "Test that low RAM triggers InsufficientMemory error"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_concurrent_acquire_respects_limits"
                file:      "crates/factory-core/src/governor.rs"
                what:      "Test that concurrent acquires never exceed max"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define ResourceType enum"
                file: "crates/factory-core/src/governor.rs"
                what: """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                    pub enum ResourceType {
                        Mutator,
                        Loop,
                        Workspace,
                    }
                    """
                done_when:     "Enum compiles"
                patterns_to_use: ["Exhaustive enum matching"]
            },
            {
                task: "Define ResourceGovernorConfig struct"
                file: "crates/factory-core/src/governor.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub struct ResourceGovernorConfig {
                        pub max_mutators: u32,
                        pub max_loops: u32,
                        pub max_workspaces: u32,
                        pub min_free_ram_mb: u64,
                    }

                    Implement builder pattern with defaults:
                    - max_mutators: 4
                    - max_loops: 8
                    - max_workspaces: 16
                    - min_free_ram_mb: 512
                    """
                done_when:     "Struct compiles with builder"
                patterns_to_use: [
                    "Builder pattern",
                    "Default trait implementation",
                ]
            },
            {
                task: "Define ticket types with RAII guards"
                file: "crates/factory-core/src/governor.rs"
                what: """
                    Define MutatorTicket, LoopTicket, WorkspaceTicket.
                    Each contains Arc<AtomicUsize> to parent counter.
                    Drop impl decrements counter.
                    No Clone, no Copy.
                    """
                done_when:     "Tickets compile with correct Drop behavior"
                patterns_to_use: [
                    "RAII pattern",
                    "Arc<AtomicUsize> for thread-safe counter",
                    "Negative traits (!Clone, !Copy via no-derive)",
                ]
            },
            {
                task: "Implement memory check from /proc/meminfo"
                file: "crates/factory-core/src/governor.rs"
                what: """
                    fn check_free_ram_mb() -> Result<u64>
                    Parse /proc/meminfo for MemAvailable or MemFree.
                    Return MB value.
                    Use cfg! for platform-specific fallback.
                    """
                done_when:     "Memory check works on Linux, fallback on others"
                patterns_to_use: [
                    "cfg!(target_os = \"linux\") for platform check",
                    "Parse MemAvailable first, fallback to MemFree",
                    "Return u64::MAX on non-Linux (no limit)",
                ]
            },
            {
                task: "Define CapacitySnapshot struct"
                file: "crates/factory-core/src/governor.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub struct CapacitySnapshot {
                        pub max_mutators: u32,
                        pub max_loops: u32,
                        pub max_workspaces: u32,
                        pub active_mutators: u32,
                        pub active_loops: u32,
                        pub active_workspaces: u32,
                        pub available_mutators: u32,
                        pub available_loops: u32,
                        pub available_workspaces: u32,
                        pub free_ram_mb: u64,
                        pub min_free_ram_mb: u64,
                    }
                    """
                done_when:     "Struct compiles"
                patterns_to_use: [
                    "Derive Clone for easy copying",
                    "Compute available_X as max_X - active_X",
                ]
            },
            {
                task: "Implement ResourceGovernor actor"
                file: "crates/factory-core/src/governor.rs"
                what: """
                    Actor that manages resource allocation.
                    State: config + Arc<AtomicUsize> for each counter.
                    Messages: AcquireMutator, AcquireLoop, AcquireWorkspace, QueryCapacity.
                    Returns tickets or errors.
                    """
                done_when:     "Actor spawns and handles messages"
                patterns_to_use: [
                    "ractor::Actor trait implementation",
                    "call() for acquire (request-reply)",
                    "Atomic counters for thread-safe tracking",
                ]
            },
            {
                task: "Implement ResourceGovernorRef handle"
                file: "crates/factory-core/src/governor.rs"
                what: """
                    Client-facing API wrapping ActorRef.
                    Methods: acquire_mutator(), acquire_loop(), acquire_workspace(),
                    query_capacity(), shutdown().
                    All return Result<T, Error>.
                    """
                done_when:     "Handle methods work correctly"
                patterns_to_use: [
                    "Alice Ryhl's actor handle pattern",
                    "oneshot channels for request-reply",
                    "tokio::time::timeout for call operations",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export governor module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod governor;"
                done_when: "External crates can import factory_core::governor"
            },
            {
                task:      "Add governor errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "AtCapacity, InsufficientMemory, ShuttingDown, GovernorSpawnFailed variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Wire governor into supervisor child specs"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Add ChildSpec for ResourceGovernor to root supervisor"
                done_when: "Governor starts with supervisor tree"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/governor.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Run concurrent stress test"
                done_when: "100 concurrent acquires with max=4 never exceed limit"
                commands: [
                    "moon run factory-core:test -- governor_stress --ignored",
                ]
                expected: "Test passes, invariants hold"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Capacity never releases"
                likely_cause: "Ticket Drop not being called"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/governor.rs"
                        function:      "impl Drop for MutatorTicket"
                        what_to_check: "Is counter being decremented? Is Arc still valid?"
                    },
                    {
                        file:          "calling code"
                        what_to_check: "Is ticket being moved into spawned task that never completes?"
                    },
                ]
                fix_pattern: "Ensure Drop decrements via Arc<AtomicUsize>::fetch_sub(1, Ordering::SeqCst)"
            },
            {
                symptom:      "InsufficientMemory error on systems with plenty of RAM"
                likely_cause: "/proc/meminfo parsing incorrect"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/governor.rs"
                        function:      "check_free_ram_mb()"
                        what_to_check: "Is MemAvailable being parsed correctly? Are units correct (kB -> MB)?"
                    },
                ]
                fix_pattern: "Parse 'MemAvailable:' line, extract number, divide by 1024 for MB"
            },
            {
                symptom:      "Counter underflow panic in release mode"
                likely_cause: "Double-drop or mismatched acquire/release"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/governor.rs"
                        function:      "impl Drop for MutatorTicket"
                        what_to_check: "Is saturating_sub being used instead of wrapping_sub?"
                    },
                ]
                fix_pattern: "Use fetch_update with checked subtraction, log warning if already zero"
            },
            {
                symptom:      "Acquire hangs indefinitely"
                likely_cause: "Actor message not being handled"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/governor.rs"
                        function:      "handle()"
                        what_to_check: "Are all message variants handled? Is reply being sent?"
                    },
                ]
                fix_pattern: "Ensure oneshot sender is used even in error paths"
            },
            {
                symptom:      "Race condition: more tickets than max_mutators"
                likely_cause: "Check-then-act race in acquire logic"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/governor.rs"
                        function:      "handle AcquireMutator"
                        what_to_check: "Is compare_exchange being used atomically?"
                    },
                ]
                fix_pattern: "Use compare_exchange loop: load, check, increment atomically"
            },
        ]

        debugging_commands: [
            {
                scenario: "When capacity appears stuck"
                run:      "RUST_LOG=factory_core::governor=trace cargo test"
                look_for: "Ticket creation and drop events, counter values"
            },
            {
                scenario: "When memory check fails unexpectedly"
                run:      "cat /proc/meminfo | grep -E 'MemAvailable|MemFree'"
                look_for: "Actual values to compare against config"
            },
            {
                scenario: "When concurrent test fails"
                run:      "RUST_LOG=trace cargo test governor_concurrent -- --nocapture"
                look_for: "Interleaving of acquire/release, final counter values"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_acquire_mutator_returns_ticket passes",
            "[ ] test_ticket_drop_releases_slot passes",
            "[ ] test_at_capacity_returns_error passes",
            "[ ] test_insufficient_memory_returns_error passes",
            "[ ] test_concurrent_acquire_respects_limits passes",
            "[ ] test_query_capacity_returns_snapshot passes",
            "[ ] test_multiple_ticket_types passes",
            "[ ] test_shutting_down_rejects_requests passes",
            "[ ] E2E full governor lifecycle test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] ResourceType enum defined",
            "[ ] ResourceGovernorConfig with builder pattern",
            "[ ] MutatorTicket with RAII Drop",
            "[ ] LoopTicket with RAII Drop",
            "[ ] WorkspaceTicket with RAII Drop",
            "[ ] check_free_ram_mb() with platform fallback",
            "[ ] CapacitySnapshot struct defined",
            "[ ] ResourceGovernor actor implementation",
            "[ ] ResourceGovernorRef handle implementation",
            "[ ] Atomic counters for thread-safe tracking",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs explaining resource governance",
            "[ ] Ticket types documented with Drop semantics",
            "[ ] CapacitySnapshot fields documented",
            "[ ] Example usage in doc comments",
            "[ ] Platform-specific behavior documented (Linux vs others)",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add governor"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add AtCapacity, InsufficientMemory variants"
            },
            {
                path:      "crates/factory-core/src/supervisor.rs"
                relevance: "Supervisor - governor is child of root supervisor"
            },
            {
                path:      "/proc/meminfo"
                relevance: "Linux memory info source for RAM checking"
            },
            {
                path:      ".beads/specs/factory-4pz.cue"
                relevance: "Dependency: supervisor that manages governor lifecycle"
            },
            {
                path:      ".beads/specs/factory-00s.cue"
                relevance: "Foundation: actor runtime that governor uses"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-4pz"
                description: "Root supervisor must exist to manage governor lifecycle"
                what_it_provides: "ChildSpec, supervision, restart handling"
            },
            {
                bead_id:     "factory-00s"
                description: "Actor framework must be in place"
                what_it_provides: "ractor Actor trait, ActorRef, message passing"
            },
        ]

        external_references: [
            "https://www.kernel.org/doc/Documentation/filesystems/proc.txt - /proc/meminfo format",
            "https://github.com/slawlor/ractor - Actor message patterns",
            "https://ryhl.io/blog/actors-with-tokio/ - RAII ticket pattern",
            "https://doc.rust-lang.org/std/sync/atomic/ - Atomic operations for counters",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "RAII Resource Guards"
                example_location: "std::sync::MutexGuard"
                how_to_apply:     "Tickets hold Arc to counter, Drop decrements"
            },
            {
                pattern:          "Actor Handle Pattern"
                example_location: "Alice Ryhl's blog, ractor ActorRef"
                how_to_apply:     "Separate actor struct from client-facing handle struct"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use Arc<AtomicUsize> for counters - survives actor restarts",
            "Use fetch_sub with Ordering::SeqCst in Drop for correctness",
            "Use compare_exchange loop for atomic increment with bounds check",
            "Parse /proc/meminfo fresh on each check - no caching",
            "Use cfg!(target_os = \"linux\") for platform-specific code",
            "Return u64::MAX for free RAM on non-Linux (effectively unlimited)",
            "Add tracing spans to ticket acquire/drop for debugging",
            "Use oneshot channels for request-reply in actor",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT derive Clone or Copy on tickets",
            "Do NOT cache memory info - always read fresh",
            "Do NOT use Mutex for counters - use AtomicUsize",
            "Do NOT block the actor on I/O - use spawn_blocking for /proc/meminfo",
        ]

        code_patterns: [
            {
                name:     "RAII Ticket Guard"
                use_when: "Creating resource tickets that auto-release"
                example:  """
                    pub struct MutatorTicket {
                        counter: Arc<AtomicUsize>,
                        #[allow(dead_code)]
                        _not_send: PhantomData<*const ()>,  // Optional: prevent Send if needed
                    }

                    impl MutatorTicket {
                        fn new(counter: Arc<AtomicUsize>) -> Self {
                            counter.fetch_add(1, Ordering::SeqCst);
                            Self {
                                counter,
                                _not_send: PhantomData,
                            }
                        }

                        pub fn is_valid(&self) -> bool {
                            true  // Ticket validity is implicit by existence
                        }
                    }

                    impl Drop for MutatorTicket {
                        fn drop(&mut self) {
                            let prev = self.counter.fetch_sub(1, Ordering::SeqCst);
                            if prev == 0 {
                                // Log warning - underflow attempted
                                tracing::warn!("MutatorTicket counter underflow prevented");
                            }
                        }
                    }
                    """
            },
            {
                name:     "Atomic Capacity Check"
                use_when: "Checking and incrementing atomically"
                example:  """
                    fn try_acquire(
                        counter: &AtomicUsize,
                        max: usize,
                    ) -> Result<(), Error> {
                        loop {
                            let current = counter.load(Ordering::SeqCst);
                            if current >= max {
                                return Err(Error::AtCapacity {
                                    resource: ResourceType::Mutator,
                                    max: max as u32,
                                    active: current as u32,
                                });
                            }
                            match counter.compare_exchange(
                                current,
                                current + 1,
                                Ordering::SeqCst,
                                Ordering::SeqCst,
                            ) {
                                Ok(_) => return Ok(()),
                                Err(_) => continue,  // Retry on contention
                            }
                        }
                    }
                    """
            },
            {
                name:     "Memory Check from /proc/meminfo"
                use_when: "Reading available RAM on Linux"
                example:  """
                    #[cfg(target_os = "linux")]
                    fn check_free_ram_mb() -> Result<u64> {
                        use std::fs;

                        let content = fs::read_to_string("/proc/meminfo")
                            .map_err(|e| Error::file_read_failed("/proc/meminfo", e.to_string()))?;

                        // Try MemAvailable first (more accurate)
                        for line in content.lines() {
                            if line.starts_with("MemAvailable:") {
                                return parse_meminfo_line(line);
                            }
                        }

                        // Fallback to MemFree
                        for line in content.lines() {
                            if line.starts_with("MemFree:") {
                                return parse_meminfo_line(line);
                            }
                        }

                        Err(Error::file_read_failed(
                            "/proc/meminfo",
                            "neither MemAvailable nor MemFree found",
                        ))
                    }

                    fn parse_meminfo_line(line: &str) -> Result<u64> {
                        // Format: "MemAvailable:    8192000 kB"
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 2 {
                            let kb: u64 = parts[1].parse().map_err(|_| {
                                Error::file_read_failed("/proc/meminfo", "invalid number format")
                            })?;
                            Ok(kb / 1024)  // Convert kB to MB
                        } else {
                            Err(Error::file_read_failed("/proc/meminfo", "invalid line format"))
                        }
                    }

                    #[cfg(not(target_os = "linux"))]
                    fn check_free_ram_mb() -> Result<u64> {
                        // On non-Linux, return unlimited (no RAM check)
                        Ok(u64::MAX)
                    }
                    """
            },
            {
                name:     "Governor Actor Message Enum"
                use_when: "Defining messages for ResourceGovernor"
                example:  """
                    pub enum GovernorMsg {
                        AcquireMutator {
                            respond_to: oneshot::Sender<Result<MutatorTicket>>,
                        },
                        AcquireLoop {
                            respond_to: oneshot::Sender<Result<LoopTicket>>,
                        },
                        AcquireWorkspace {
                            respond_to: oneshot::Sender<Result<WorkspaceTicket>>,
                        },
                        QueryCapacity {
                            respond_to: oneshot::Sender<CapacitySnapshot>,
                        },
                        Shutdown,
                    }
                    """
            },
            {
                name:     "Governor Handle with Timeout"
                use_when: "Creating client-facing API"
                example:  """
                    #[derive(Clone)]
                    pub struct ResourceGovernorRef {
                        actor: ActorRef<GovernorMsg>,
                        mutator_counter: Arc<AtomicUsize>,
                        loop_counter: Arc<AtomicUsize>,
                        workspace_counter: Arc<AtomicUsize>,
                    }

                    impl ResourceGovernorRef {
                        pub async fn acquire_mutator(&self) -> Result<MutatorTicket> {
                            let (tx, rx) = oneshot::channel();
                            self.actor.cast(GovernorMsg::AcquireMutator { respond_to: tx })?;

                            tokio::time::timeout(Duration::from_secs(5), rx)
                                .await
                                .map_err(|_| Error::CommandTimeout { timeout_ms: 5000 })?
                                .map_err(|_| Error::ShuttingDown)?
                        }

                        pub async fn query_capacity(&self) -> Result<CapacitySnapshot> {
                            let (tx, rx) = oneshot::channel();
                            self.actor.cast(GovernorMsg::QueryCapacity { respond_to: tx })?;

                            tokio::time::timeout(Duration::from_secs(5), rx)
                                .await
                                .map_err(|_| Error::CommandTimeout { timeout_ms: 5000 })?
                                .ok_or(Error::ShuttingDown)
                        }
                    }
                    """
            },
        ]
    }
}
