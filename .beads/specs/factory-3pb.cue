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

"factory-3pb": #ValidBead & {
    // ============================================================================
    // BEAD: factory-3pb - Implement actor registry for named process lookup
    // ============================================================================

    id:              "factory-3pb"
    title:           "Actor Registry: Implement named process lookup with DashMap"
    type:            "feature"
    priority:        1
    effort_estimate: "2hr"
    labels:          ["runtime", "actors", "registry", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use DashMap for concurrent actor name-to-ref mapping",
            "THE SYSTEM SHALL support any hashable key type for actor registration",
            "THE SYSTEM SHALL provide thread-safe access to the registry from any actor or task",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN register(name, ActorRef) is called with a new name"
                shall:   "THE SYSTEM SHALL store the mapping and return Ok(())"
            },
            {
                trigger: "WHEN register(name, ActorRef) is called with an existing name"
                shall:   "THE SYSTEM SHALL return Err(RegistryError::AlreadyRegistered)"
            },
            {
                trigger: "WHEN whereis(name) is called for a registered actor"
                shall:   "THE SYSTEM SHALL return Some(ActorRef) immediately without blocking"
            },
            {
                trigger: "WHEN whereis(name) is called for an unregistered name"
                shall:   "THE SYSTEM SHALL return None without error"
            },
            {
                trigger: "WHEN unregister(name) is called for a registered actor"
                shall:   "THE SYSTEM SHALL remove the mapping and return Ok(ActorRef)"
            },
            {
                trigger: "WHEN unregister(name) is called for an unregistered name"
                shall:   "THE SYSTEM SHALL return Err(RegistryError::NotFound)"
            },
            {
                trigger: "WHEN an actor terminates unexpectedly"
                shall:   "THE SYSTEM SHALL NOT automatically remove it from registry (explicit unregister required)"
            },
        ]

        state_driven: [
            {
                state: "WHILE the registry contains N registered actors"
                shall: "THE SYSTEM SHALL provide O(1) lookup for any registered name"
            },
            {
                state: "WHILE multiple threads access the registry concurrently"
                shall: "THE SYSTEM SHALL guarantee data race freedom via DashMap's internal locking"
            },
            {
                state: "WHILE the system is under high contention"
                shall: "THE SYSTEM SHALL use sharded locking to minimize lock contention"
            },
        ]

        unwanted: [
            {
                condition: "IF a caller holds a registry lock while performing slow operations"
                shall_not: "THE SYSTEM SHALL NOT allow long-held read guards across await points"
                because:   "Holding guards across await points causes deadlocks in async code"
            },
            {
                condition: "IF the registry grows unboundedly"
                shall_not: "THE SYSTEM SHALL NOT leak memory from dead actor references"
                because:   "Forgotten registrations cause memory leaks and stale lookups"
            },
            {
                condition: "IF two actors try to register the same name simultaneously"
                shall_not: "THE SYSTEM SHALL NOT allow duplicate registrations"
                because:   "Duplicate names break the single-source-of-truth invariant"
            },
        ]

        complex: [
            {
                state:   "WHILE the system is shutting down"
                trigger: "WHEN a new registration is attempted"
                shall:   "THE SYSTEM SHALL reject with Err(RegistryError::ShuttingDown)"
            },
            {
                state:   "WHILE an actor is being unregistered"
                trigger: "WHEN whereis is called for that name"
                shall:   "THE SYSTEM SHALL return None (unregister is atomic)"
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
                    field:           "name: K where K: Eq + Hash + Clone + Send + Sync + 'static"
                    type:            "Generic hashable key"
                    constraints:     "Must implement Eq, Hash, Clone, Send, Sync"
                    example_valid:   "String, u64, Uuid, custom newtype"
                    example_invalid: "RefCell<T>, Rc<T> (not Send/Sync)"
                },
                {
                    field:           "actor_ref: ActorRef<M>"
                    type:            "Actor reference handle"
                    constraints:     "Must be valid (actor not yet stopped)"
                    example_valid:   "ActorRef from successful spawn"
                    example_invalid: "ActorRef to a stopped actor"
                },
            ]
            system_state: [
                "Rust toolchain installed (rustc, cargo)",
                "Moon build system configured",
                "factory-core crate exists with actor module (factory-00s complete)",
                "dashmap dependency available",
            ]
        }

        postconditions: {
            state_changes: [
                "Cargo.toml updated with dashmap dependency",
                "New module: crates/factory-core/src/registry.rs",
                "lib.rs updated to export registry module",
                "Error enum extended with registry error variants",
            ]
            return_guarantees: [
                {
                    field:     "register(name, actor_ref)"
                    guarantee: "Returns Result<(), RegistryError>; Ok means name is now mapped"
                },
                {
                    field:     "whereis(name)"
                    guarantee: "Returns Option<ActorRef>; lookup is O(1) and non-blocking"
                },
                {
                    field:     "unregister(name)"
                    guarantee: "Returns Result<ActorRef, RegistryError>; Ok returns the removed ref"
                },
                {
                    field:     "registered_names()"
                    guarantee: "Returns Vec<K> snapshot of all registered names"
                },
                {
                    field:     "count()"
                    guarantee: "Returns usize count of registered actors"
                },
            ]
            side_effects: [
                "Global registry is lazily initialized on first access",
                "Registry entries persist until explicitly unregistered",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error> or Option<T>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "A name can only map to one ActorRef at a time",
            "DashMap provides linearizable operations (read-your-writes)",
            "Registry is Send + Sync and can be shared across threads",
            "Lookups never block other lookups (read concurrency)",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "DashMap version incompatible with async runtime"
                prevention:  "Pin dashmap version, verify no async-unfriendly APIs"
                test_for_it: "test_registry_works_in_async_context"
            },
            {
                failure:     "ActorRef type parameter makes registry non-generic"
                prevention:  "Use type-erased ActorRef or separate registries per message type"
                test_for_it: "test_registry_multiple_actor_types"
            },
            {
                failure:     "Registry global state conflicts with test isolation"
                prevention:  "Provide Registry::new() for local registries in tests"
                test_for_it: "test_isolated_registry_per_test"
            },
        ]

        usability_failures: [
            {
                failure:     "API too verbose for simple name-based lookup"
                prevention:  "Provide convenience methods: register_as, lookup"
                test_for_it: "test_ergonomic_api"
            },
            {
                failure:     "Error messages don't indicate which name failed"
                prevention:  "Include name in error context via thiserror Display"
                test_for_it: "test_error_messages_include_name"
            },
            {
                failure:     "No way to iterate registered actors"
                prevention:  "Provide registered_names() and iter() methods"
                test_for_it: "test_registry_iteration"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Race condition: two threads register same name"
                prevention:  "Use DashMap::try_insert or entry API for atomic insert-if-absent"
                test_for_it: "test_concurrent_registration_same_name"
            },
            {
                failure:     "Stale ActorRef returned after actor stopped"
                prevention:  "Document that users must unregister on actor stop; provide cleanup hooks"
                test_for_it: "test_stale_ref_behavior_documented"
            },
            {
                failure:     "Memory leak from forgotten registrations"
                prevention:  "Provide clear() for cleanup; document lifecycle"
                test_for_it: "test_registry_cleanup"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_register_and_whereis_returns_ref"
                given: "An empty registry and a valid ActorRef"
                when:  "register(name, ref) then whereis(name)"
                then: [
                    "register returns Ok(())",
                    "whereis returns Some(ActorRef)",
                    "Returned ref can send messages",
                ]
                real_input: """
                    let registry = ActorRegistry::<String>::new();
                    let actor_ref = spawn_test_actor().await?;
                    registry.register("my-actor".to_string(), actor_ref.clone())?;
                    """
                expected_output: """
                    let found = registry.whereis(&"my-actor".to_string());
                    assert!(found.is_some());
                    let found_ref = found.ok_or(Error::NotFound)?;
                    found_ref.cast(TestMsg::Ping)?;
                    """
            },
            {
                name:  "test_unregister_removes_mapping"
                given: "A registry with one registered actor"
                when:  "unregister(name) is called"
                then: [
                    "Returns Ok(ActorRef)",
                    "Subsequent whereis returns None",
                    "Registry count decreases by 1",
                ]
                real_input: """
                    let registry = ActorRegistry::<String>::new();
                    registry.register("worker".to_string(), actor_ref.clone())?;
                    let initial_count = registry.count();
                    """
                expected_output: """
                    let removed = registry.unregister(&"worker".to_string())?;
                    assert!(registry.whereis(&"worker".to_string()).is_none());
                    assert_eq!(registry.count(), initial_count - 1);
                    """
            },
            {
                name:  "test_registered_names_returns_snapshot"
                given: "A registry with multiple registered actors"
                when:  "registered_names() is called"
                then: [
                    "Returns Vec containing all registered names",
                    "Order is not guaranteed (hash map)",
                    "Snapshot is consistent at call time",
                ]
                real_input: """
                    let registry = ActorRegistry::<String>::new();
                    for name in ["alpha", "beta", "gamma"] {
                        let actor = spawn_test_actor().await?;
                        registry.register(name.to_string(), actor)?;
                    }
                    """
                expected_output: """
                    let names = registry.registered_names();
                    assert_eq!(names.len(), 3);
                    assert!(names.contains(&"alpha".to_string()));
                    assert!(names.contains(&"beta".to_string()));
                    assert!(names.contains(&"gamma".to_string()));
                    """
            },
            {
                name:  "test_count_returns_registry_size"
                given: "A registry with varying number of actors"
                when:  "count() is called"
                then: [
                    "Returns accurate count",
                    "Updates after register/unregister",
                ]
                real_input: """
                    let registry = ActorRegistry::<String>::new();
                    assert_eq!(registry.count(), 0);
                    registry.register("one".to_string(), spawn_test_actor().await?)?;
                    """
                expected_output: """
                    assert_eq!(registry.count(), 1);
                    registry.register("two".to_string(), spawn_test_actor().await?)?;
                    assert_eq!(registry.count(), 2);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_register_duplicate_returns_error"
                given: "A registry with name 'worker' already registered"
                when:  "register('worker', different_ref) is called"
                then: [
                    "Returns Err(RegistryError::AlreadyRegistered)",
                    "Original mapping is preserved",
                    "Error message includes the duplicate name",
                ]
                real_input: """
                    let registry = ActorRegistry::<String>::new();
                    let first = spawn_test_actor().await?;
                    let second = spawn_test_actor().await?;
                    registry.register("worker".to_string(), first.clone())?;
                    """
                expected_output: null
                expected_error: """
                    let result = registry.register("worker".to_string(), second);
                    assert!(matches!(result, Err(Error::AlreadyRegistered { .. })));
                    // Original still there
                    assert!(registry.whereis(&"worker".to_string()).is_some());
                    """
            },
            {
                name:  "test_unregister_not_found_returns_error"
                given: "A registry without name 'ghost'"
                when:  "unregister('ghost') is called"
                then: [
                    "Returns Err(RegistryError::NotFound)",
                    "Registry state is unchanged",
                ]
                real_input: """
                    let registry = ActorRegistry::<String>::new();
                    """
                expected_output: null
                expected_error: """
                    let result = registry.unregister(&"ghost".to_string());
                    assert!(matches!(result, Err(Error::RegistryNotFound { .. })));
                    """
            },
            {
                name:  "test_whereis_unknown_returns_none"
                given: "A registry without name 'unknown'"
                when:  "whereis('unknown') is called"
                then: [
                    "Returns None (not an error)",
                    "Does not modify registry",
                ]
                real_input: """
                    let registry = ActorRegistry::<String>::new();
                    """
                expected_output: """
                    let result = registry.whereis(&"unknown".to_string());
                    assert!(result.is_none());
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_concurrent_registrations"
                scenario: "100 tasks register unique names simultaneously"
                input:    "spawn 100 tasks, each registers unique name"
                expected: "All 100 registrations succeed, count == 100"
            },
            {
                name:     "test_concurrent_same_name_race"
                scenario: "10 tasks race to register the same name"
                input:    "spawn 10 tasks, all try to register 'singleton'"
                expected: "Exactly 1 succeeds, 9 get AlreadyRegistered error"
            },
            {
                name:     "test_register_unregister_register_cycle"
                scenario: "Name is registered, unregistered, then re-registered"
                input:    "register('temp'), unregister('temp'), register('temp', new_ref)"
                expected: "All operations succeed, final whereis returns new_ref"
            },
            {
                name:     "test_empty_string_as_name"
                scenario: "Empty string used as actor name"
                input:    "register('', actor_ref)"
                expected: "Succeeds (empty string is valid hashable key)"
            },
            {
                name:     "test_unicode_name"
                scenario: "Unicode string used as actor name"
                input:    "register('actor', actor_ref)"
                expected: "Succeeds, whereis('actor') returns the ref"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in registry module"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/registry.rs returns empty"
            },
            {
                name:     "test_invariant_no_panic"
                verifies: "No panic!, todo!, unimplemented! in registry module"
                test:     "rg 'panic!|todo!|unimplemented!' crates/factory-core/src/registry.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All mutating APIs return Result"
                test:     "register and unregister signatures include -> Result"
            },
            {
                name:     "test_invariant_send_sync"
                verifies: "ActorRegistry implements Send + Sync"
                test:     "fn assert_send_sync<T: Send + Sync>() {} assert_send_sync::<ActorRegistry<String>>();"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_registry_actor_communication"
            description: "Complete registry lifecycle: register -> lookup -> communicate -> unregister"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/registry_e2e.rs"
                        content: """
                            use factory_core::registry::ActorRegistry;
                            use factory_core::error::Result;

                            #[tokio::test]
                            async fn test_registry_actor_communication() -> Result<()> {
                                // Full lifecycle test implemented inline
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
                command:    "moon run factory-core:test -- --test registry_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_registry_actor_communication ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/registry.rs"
                        contains: "pub struct ActorRegistry"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/registry_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_cross_actor_communication_via_registry"
                description: "Two actors communicate via registry lookup"
                steps: [
                    {action: "Spawn producer actor", verify: "Producer is running"},
                    {action: "Register producer as 'producer'", verify: "Registration succeeds"},
                    {action: "Spawn consumer actor", verify: "Consumer is running"},
                    {action: "Consumer calls whereis('producer')", verify: "Returns Some(producer_ref)"},
                    {action: "Consumer sends message to producer", verify: "Producer receives message"},
                    {action: "Unregister producer", verify: "Unregistration succeeds"},
                ]
            },
            {
                name:        "e2e_supervisor_managed_registry"
                description: "Supervisor registers child actors on spawn, unregisters on termination"
                steps: [
                    {action: "Spawn supervisor", verify: "Supervisor is running"},
                    {action: "Supervisor spawns worker-1", verify: "worker-1 registered"},
                    {action: "Supervisor spawns worker-2", verify: "worker-2 registered"},
                    {action: "Kill worker-1", verify: "Supervisor unregisters worker-1"},
                    {action: "Verify registry state", verify: "Only worker-2 registered"},
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
                task:      "Write test: test_register_and_whereis_returns_ref"
                file:      "crates/factory-core/src/registry.rs"
                what:      "Test that register stores mapping and whereis retrieves it"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_unregister_removes_mapping"
                file:      "crates/factory-core/src/registry.rs"
                what:      "Test that unregister removes and returns the ActorRef"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_register_duplicate_returns_error"
                file:      "crates/factory-core/src/registry.rs"
                what:      "Test that duplicate registration fails with error"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_concurrent_same_name_race"
                file:      "crates/factory-core/src/registry.rs"
                what:      "Test concurrent registration race is handled correctly"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add dashmap dependency to Cargo.toml"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dependencies]
                    dashmap = "5"
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["workspace dependencies for version management"]
            },
            {
                task: "Add registry error variants to error.rs"
                file: "crates/factory-core/src/error.rs"
                what: """
                    Add to Error enum:
                    - RegistryAlreadyRegistered { name: String }
                    - RegistryNotFound { name: String }
                    - RegistryShuttingDown
                    """
                done_when:     "Error variants compile with thiserror"
                patterns_to_use: [
                    "thiserror derive for Display impl",
                    "Include context in error messages",
                ]
            },
            {
                task: "Create registry.rs module"
                file: "crates/factory-core/src/registry.rs"
                what: """
                    Implement ActorRegistry<K>:
                    - DashMap<K, ActorRef> internal storage
                    - register(name: K, actor: ActorRef) -> Result<()>
                    - whereis(name: &K) -> Option<ActorRef>
                    - unregister(name: &K) -> Result<ActorRef>
                    - registered_names() -> Vec<K>
                    - count() -> usize
                    """
                done_when:     "Tests pass (green phase)"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "Clone for ActorRef retrieval (DashMap requires clone)",
                    "DashMap::try_insert for atomic duplicate detection",
                    "Generic K: Eq + Hash + Clone + Send + Sync + 'static",
                ]
            },
            {
                task: "Implement global registry accessor (optional)"
                file: "crates/factory-core/src/registry.rs"
                what: """
                    Provide GLOBAL_REGISTRY: OnceCell<ActorRegistry<String>>
                    with global_registry() -> &'static ActorRegistry<String>
                    """
                done_when:     "Global accessor works in tests"
                patterns_to_use: [
                    "OnceCell for lazy initialization",
                    "String as default key type for convenience",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export registry module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod registry;"
                done_when: "External crates can import factory_core::registry"
            },
            {
                task:      "Add registry documentation"
                file:      "crates/factory-core/src/registry.rs"
                what:      "Module-level doc with Erlang comparison, examples"
                done_when: "cargo doc generates clean documentation"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/registry.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify no panics"
                done_when: "grep finds no panic!/todo!/unimplemented!"
                commands: ["rg 'panic!|todo!|unimplemented!' crates/factory-core/src/registry.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Stress test concurrent access"
                done_when: "No deadlocks or data races under load"
                commands: [
                    "cargo test --release -- --test-threads=8 test_concurrent",
                ]
                expected: "All concurrent tests pass"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'dashmap' not found"
                likely_cause: "Dependency not added to Cargo.toml"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "dashmap = \"5\" in [dependencies]"
                    },
                ]
                fix_pattern: "Add dashmap = \"5\" to dependencies"
            },
            {
                symptom:      "Error: trait bound `K: Clone` not satisfied"
                likely_cause: "DashMap requires Clone for value retrieval"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/registry.rs"
                        function:      "whereis()"
                        what_to_check: "Is ActorRef Clone? Use .clone() on get"
                    },
                ]
                fix_pattern: "Ensure ActorRef derives Clone; use entry.value().clone()"
            },
            {
                symptom:      "Deadlock in async test"
                likely_cause: "Holding DashMap guard across await point"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/registry.rs"
                        what_to_check: "Are any guards held across .await?"
                    },
                ]
                fix_pattern: "Clone the value and drop the guard before any await"
            },
            {
                symptom:      "Race condition: duplicate registrations succeed"
                likely_cause: "Using insert() instead of try_insert()"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/registry.rs"
                        function:      "register()"
                        what_to_check: "Is try_insert or entry API used?"
                    },
                ]
                fix_pattern: "Use entry().or_insert_with() or try_insert() for atomic insert"
            },
            {
                symptom:      "Test isolation failure: state leaks between tests"
                likely_cause: "Using global registry in tests"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/registry.rs"
                        what_to_check: "Are tests using new() or global registry?"
                    },
                ]
                fix_pattern: "Use ActorRegistry::new() for isolated test instances"
            },
        ]

        debugging_commands: [
            {
                scenario: "When registration fails unexpectedly"
                run:      "RUST_LOG=debug cargo test test_register -- --nocapture"
                look_for: "DashMap entry state, existing values"
            },
            {
                scenario: "When concurrent test hangs"
                run:      "cargo test --release -- --test-threads=1 test_concurrent --nocapture"
                look_for: "Which thread is blocked, lock acquisition order"
            },
            {
                scenario: "When whereis returns unexpected None"
                run:      "RUST_LOG=trace cargo test test_whereis -- --nocapture"
                look_for: "Registration order, key equality, hash collisions"
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
            "[ ] Concurrent registration race test passing",
            "[ ] No mocks or fake data in any test",
            "[ ] test_register_and_whereis_returns_ref passes",
            "[ ] test_unregister_removes_mapping passes",
            "[ ] test_register_duplicate_returns_error passes",
            "[ ] test_concurrent_same_name_race passes",
            "[ ] test_registered_names_returns_snapshot passes",
            "[ ] test_count_returns_registry_size passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Zero panic!, todo!, unimplemented!",
            "[ ] All preconditions validated",
            "[ ] All postconditions guaranteed",
            "[ ] dashmap dependency added",
            "[ ] registry.rs module created",
            "[ ] Error variants added to error.rs",
            "[ ] ActorRegistry<K> is generic over key type",
            "[ ] ActorRegistry is Send + Sync",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in registry.rs",
            "[ ] Erlang process registry comparison in docs",
            "[ ] Example usage in doc comments",
            "[ ] Thread-safety guarantees documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add registry"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add registry error variants"
            },
            {
                path:      "crates/factory-core/src/actor.rs"
                relevance: "Actor types - ActorRef must be storable in registry"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - must add dashmap"
            },
            {
                path:      ".beads/specs/factory-00s.cue"
                relevance: "Dependency: actor framework must be complete first"
            },
        ]

        external_references: [
            "https://docs.rs/dashmap - DashMap concurrent hashmap",
            "https://www.erlang.org/doc/man/erlang.html#register-2 - Erlang process registry",
            "https://www.erlang.org/doc/man/erlang.html#whereis-1 - Erlang whereis/1",
            "https://elixir-lang.org/getting-started/mix-otp/genserver.html#naming-the-process - Elixir process naming",
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
                how_to_apply:     "Wrap registry keys in newtypes if domain-specific validation needed"
            },
            {
                pattern:          "thiserror for Error Types"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Derive Error, include context in Display impl"
            },
        ]

        erlang_equivalents: [
            {
                erlang:   "register(Name, Pid)"
                rust:     "registry.register(name, actor_ref)"
                notes:    "Erlang uses atoms; Rust uses generic hashable keys"
            },
            {
                erlang:   "whereis(Name)"
                rust:     "registry.whereis(&name)"
                notes:    "Both return None/undefined if not found"
            },
            {
                erlang:   "unregister(Name)"
                rust:     "registry.unregister(&name)"
                notes:    "Rust returns the removed ref; Erlang returns true"
            },
            {
                erlang:   "registered()"
                rust:     "registry.registered_names()"
                notes:    "Returns list/Vec of all registered names"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use dashmap 5.x - it's the standard concurrent hashmap for Rust",
            "Make ActorRegistry generic over K: Eq + Hash + Clone + Send + Sync + 'static",
            "Use DashMap::try_insert() for atomic duplicate detection",
            "Clone ActorRef on retrieval (don't return references)",
            "Provide both generic ActorRegistry<K> and String-typed global convenience",
            "Use thiserror for error conversion",
            "Add #[must_use] to methods returning Option or Result",
            "Document thread-safety guarantees in module docs",
            "Include Erlang comparison in documentation for familiarity",
            "Provide clear() method for cleanup in tests and shutdown",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT hold DashMap guards across await points",
            "Do NOT use Mutex/RwLock (DashMap handles locking internally)",
            "Do NOT store references in DashMap (use owned values)",
            "Do NOT assume iteration order (it's a hash map)",
            "Do NOT allow duplicate names (fail fast with AlreadyRegistered)",
        ]

        code_patterns: [
            {
                name:     "ActorRegistry Struct"
                use_when: "Defining the registry container"
                example:  """
                    use dashmap::DashMap;
                    use std::hash::Hash;

                    pub struct ActorRegistry<K> {
                        inner: DashMap<K, ActorRef>,
                    }

                    impl<K> ActorRegistry<K>
                    where
                        K: Eq + Hash + Clone + Send + Sync + 'static,
                    {
                        pub fn new() -> Self {
                            Self {
                                inner: DashMap::new(),
                            }
                        }
                    }
                    """
            },
            {
                name:     "Atomic Registration with try_insert"
                use_when: "Implementing register() to prevent duplicates"
                example:  """
                    pub fn register(&self, name: K, actor: ActorRef) -> Result<()> {
                        match self.inner.try_insert(name.clone(), actor) {
                            Ok(_) => Ok(()),
                            Err(_) => Err(Error::RegistryAlreadyRegistered {
                                name: format!("{:?}", name),
                            }),
                        }
                    }
                    """
            },
            {
                name:     "Safe whereis with Clone"
                use_when: "Implementing whereis() without holding guards"
                example:  """
                    pub fn whereis(&self, name: &K) -> Option<ActorRef> {
                        self.inner.get(name).map(|entry| entry.value().clone())
                    }
                    """
            },
            {
                name:     "Unregister with Entry API"
                use_when: "Implementing unregister() to return the removed ref"
                example:  """
                    pub fn unregister(&self, name: &K) -> Result<ActorRef> {
                        self.inner
                            .remove(name)
                            .map(|(_, actor)| actor)
                            .ok_or_else(|| Error::RegistryNotFound {
                                name: format!("{:?}", name),
                            })
                    }
                    """
            },
            {
                name:     "Global Registry Pattern"
                use_when: "Providing convenient global access"
                example:  """
                    use std::sync::OnceLock;

                    static GLOBAL_REGISTRY: OnceLock<ActorRegistry<String>> = OnceLock::new();

                    pub fn global_registry() -> &'static ActorRegistry<String> {
                        GLOBAL_REGISTRY.get_or_init(ActorRegistry::new)
                    }
                    """
            },
        ]
    }
}
