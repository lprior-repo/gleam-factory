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

"factory-kou": #ValidBead & {
    // ============================================================================
    // BEAD: factory-kou - Implement signal bus pub/sub actor for event distribution
    // ============================================================================

    id:              "factory-kou"
    title:           "Runtime: Implement signal bus pub/sub actor for event distribution"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["runtime", "actors", "pubsub", "signals", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL implement a centralized signal bus for system-wide event distribution",
            "THE SYSTEM SHALL support typed signal subscriptions with compile-time safety",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL clean up subscriptions when subscribers die",
            "THE SYSTEM SHALL guarantee at-most-once delivery semantics",
        ]

        event_driven: [
            {
                trigger: "WHEN a subscriber subscribes to a signal type"
                shall:   "THE SYSTEM SHALL register the subscriber and return a SubscriptionHandle"
            },
            {
                trigger: "WHEN a subscriber unsubscribes via SubscriptionHandle::drop()"
                shall:   "THE SYSTEM SHALL remove the subscription from the registry"
            },
            {
                trigger: "WHEN a signal is published to the bus"
                shall:   "THE SYSTEM SHALL broadcast the signal to all subscribers of that signal type"
            },
            {
                trigger: "WHEN a subscriber actor dies"
                shall:   "THE SYSTEM SHALL detect the death and automatically remove all its subscriptions"
            },
            {
                trigger: "WHEN a broadcast to a subscriber fails"
                shall:   "THE SYSTEM SHALL log the failure and continue broadcasting to other subscribers"
            },
            {
                trigger: "WHEN ShutdownRequested signal is published"
                shall:   "THE SYSTEM SHALL broadcast to all subscribers before shutting down the bus"
            },
            {
                trigger: "WHEN the signal bus is asked for subscriber count"
                shall:   "THE SYSTEM SHALL return current count per signal type"
            },
        ]

        state_driven: [
            {
                state: "WHILE the signal bus is running"
                shall: "THE SYSTEM SHALL maintain a registry of all active subscriptions"
            },
            {
                state: "WHILE broadcasting a signal"
                shall: "THE SYSTEM SHALL not block on any single subscriber"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL reject new subscriptions"
            },
        ]

        unwanted: [
            {
                condition: "IF a subscriber's message queue is full"
                shall_not: "THE SYSTEM SHALL NOT block the signal bus waiting for the subscriber"
                because:   "One slow subscriber must not slow down the entire system"
            },
            {
                condition: "IF a subscriber panics during signal handling"
                shall_not: "THE SYSTEM SHALL NOT crash the signal bus"
                because:   "The bus is critical infrastructure; one bad subscriber must not take it down"
            },
            {
                condition: "IF the same subscriber subscribes twice to the same signal"
                shall_not: "THE SYSTEM SHALL NOT create duplicate subscriptions"
                because:   "Duplicate subscriptions cause duplicate message delivery and resource leaks"
            },
            {
                condition: "IF a signal type has no subscribers"
                shall_not: "THE SYSTEM SHALL NOT error when publishing"
                because:   "Publishing to empty subscriber list is valid; signals may be optional"
            },
            {
                condition: "IF a subscription handle is dropped during broadcast"
                shall_not: "THE SYSTEM SHALL NOT corrupt the subscriber registry"
                because:   "Concurrent modification during iteration causes undefined behavior"
            },
        ]

        complex: [
            {
                state:   "WHILE the signal bus is broadcasting"
                trigger: "WHEN a new subscription request arrives"
                shall:   "THE SYSTEM SHALL queue the subscription for processing after broadcast completes"
            },
            {
                state:   "WHILE shutdown is in progress"
                trigger: "WHEN a new signal publish is requested"
                shall:   "THE SYSTEM SHALL reject with Error::ShuttingDown unless signal is ShutdownRequested"
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
                    field:           "Signal type"
                    type:            "Enum variant"
                    constraints:     "Must be one of the defined Signal variants"
                    example_valid:   "Signal::TestFailure { test_name, error }"
                    example_invalid: "CustomSignal { .. } // Undefined signal type"
                },
                {
                    field:           "Subscriber ActorRef"
                    type:            "ActorRef<SignalMessage>"
                    constraints:     "Must implement SignalHandler trait and be alive"
                    example_valid:   "actor_ref.subscribe::<TestFailure>().await"
                    example_invalid: "dead_actor_ref.subscribe() // Actor already terminated"
                },
            ]
            system_state: [
                "Tokio runtime initialized",
                "Root supervisor running (factory-4pz completed)",
                "Actor framework available (factory-00s completed)",
            ]
        }

        postconditions: {
            state_changes: [
                "SignalBus actor spawned under root supervisor",
                "Subscription registry initialized (empty)",
                "Signal type handlers registered",
            ]
            return_guarantees: [
                {
                    field:     "SignalBusRef"
                    guarantee: "Valid handle for publishing and subscribing"
                },
                {
                    field:     "SignalBusRef::subscribe()"
                    guarantee: "Returns Result<SubscriptionHandle, SubscribeError>"
                },
                {
                    field:     "SignalBusRef::publish()"
                    guarantee: "Returns Result<BroadcastStats, PublishError>"
                },
                {
                    field:     "SignalBusRef::unsubscribe()"
                    guarantee: "Returns Result<(), UnsubscribeError>"
                },
                {
                    field:     "SubscriptionHandle::drop()"
                    guarantee: "Automatically unsubscribes; safe to call multiple times"
                },
                {
                    field:     "SignalBusRef::subscriber_count()"
                    guarantee: "Returns HashMap<SignalType, usize> snapshot"
                },
            ]
            side_effects: [
                "Signals broadcast asynchronously to all subscribers",
                "Dead subscribers automatically pruned from registry",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Signal delivery is at-most-once per subscriber",
            "Subscription handles are unique and non-duplicatable",
            "Subscriber registry remains consistent during concurrent operations",
            "Broadcasting never blocks on any single subscriber",
            "Dead subscribers are eventually removed from registry",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Signal enum grows too large and causes code bloat"
                prevention:  "Use enum_dispatch or trait objects with TypeId for extensibility"
                test_for_it: "test_signal_enum_size_reasonable"
            },
            {
                failure:     "Subscriber death detection delayed or missed"
                prevention:  "Use ractor linking/monitoring for immediate death notification"
                test_for_it: "test_subscriber_death_detected_promptly"
            },
            {
                failure:     "Signal bus becomes bottleneck under high load"
                prevention:  "Use sharded subscriber maps and parallel broadcast"
                test_for_it: "test_high_throughput_broadcast"
            },
        ]

        usability_failures: [
            {
                failure:     "Subscription API too complex for simple use cases"
                prevention:  "Provide SignalBusRef::subscribe_fn() for closure-based handlers"
                test_for_it: "test_simple_subscription_api"
            },
            {
                failure:     "Hard to debug which actors are subscribed to what"
                prevention:  "Provide debug_subscriptions() method with formatted output"
                test_for_it: "test_subscription_debug_info"
            },
            {
                failure:     "Signal types not discoverable"
                prevention:  "Group signals in well-documented enum with examples"
                test_for_it: "test_signal_documentation_complete"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Race between unsubscribe and broadcast causes message to dead subscriber"
                prevention:  "Use weak references or verify subscriber alive before send"
                test_for_it: "test_no_send_to_unsubscribed"
            },
            {
                failure:     "Subscription registry corrupted by concurrent modifications"
                prevention:  "Use actor message serialization; all mutations via messages"
                test_for_it: "test_concurrent_subscribe_unsubscribe"
            },
            {
                failure:     "Memory leak from zombie subscriptions"
                prevention:  "Periodic sweep of dead subscribers, link-based cleanup"
                test_for_it: "test_no_subscription_memory_leak"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_subscribe_receives_signals"
                given: "A running signal bus and a subscriber actor"
                when:  "Subscriber subscribes to TestFailure and signal is published"
                then: [
                    "Subscriber receives the TestFailure signal",
                    "Signal contains expected test_name and error fields",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;
                    let subscriber = TestSubscriber::spawn().await?;

                    // Subscribe to TestFailure signals
                    let handle = bus.subscribe::<TestFailure>(subscriber.clone()).await?;

                    // Publish a signal
                    bus.publish(Signal::TestFailure {
                        test_name: "test_example".into(),
                        error: "assertion failed".into(),
                    }).await?;
                    """
                expected_output: """
                    // Give broadcast time to complete
                    tokio::time::sleep(Duration::from_millis(50)).await;

                    let received = subscriber.received_signals().await?;
                    assert_eq!(received.len(), 1);
                    assert!(matches!(
                        &received[0],
                        Signal::TestFailure { test_name, .. } if test_name == "test_example"
                    ));
                    """
            },
            {
                name:  "test_multiple_subscribers_all_receive"
                given: "A signal bus with 3 subscribers for the same signal type"
                when:  "Signal is published"
                then: [
                    "All 3 subscribers receive the signal",
                    "Each receives exactly one copy",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;

                    let sub1 = TestSubscriber::spawn().await?;
                    let sub2 = TestSubscriber::spawn().await?;
                    let sub3 = TestSubscriber::spawn().await?;

                    bus.subscribe::<BeadAssigned>(sub1.clone()).await?;
                    bus.subscribe::<BeadAssigned>(sub2.clone()).await?;
                    bus.subscribe::<BeadAssigned>(sub3.clone()).await?;

                    bus.publish(Signal::BeadAssigned {
                        bead_id: "factory-kou".into(),
                        agent_id: "agent-1".into(),
                    }).await?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(50)).await;

                    assert_eq!(sub1.received_signals().await?.len(), 1);
                    assert_eq!(sub2.received_signals().await?.len(), 1);
                    assert_eq!(sub3.received_signals().await?.len(), 1);
                    """
            },
            {
                name:  "test_unsubscribe_stops_delivery"
                given: "A subscribed actor"
                when:  "Actor unsubscribes and signal is published"
                then: [
                    "Actor does not receive the signal",
                    "Other subscribers still receive it",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;

                    let sub1 = TestSubscriber::spawn().await?;
                    let sub2 = TestSubscriber::spawn().await?;

                    let handle1 = bus.subscribe::<LoopComplete>(sub1.clone()).await?;
                    let _handle2 = bus.subscribe::<LoopComplete>(sub2.clone()).await?;

                    // Unsubscribe sub1
                    drop(handle1);
                    tokio::time::sleep(Duration::from_millis(10)).await;

                    // Publish after unsubscribe
                    bus.publish(Signal::LoopComplete {
                        loop_id: "loop-123".into(),
                        iterations: 5,
                    }).await?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(50)).await;

                    assert_eq!(sub1.received_signals().await?.len(), 0);  // Unsubscribed
                    assert_eq!(sub2.received_signals().await?.len(), 1);  // Still subscribed
                    """
            },
            {
                name:  "test_subscriber_death_auto_cleanup"
                given: "A subscribed actor"
                when:  "Subscriber actor dies"
                then: [
                    "Subscription is automatically removed",
                    "Future publishes do not attempt delivery to dead subscriber",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;
                    let subscriber = TestSubscriber::spawn().await?;

                    let _handle = bus.subscribe::<TestPassing>(subscriber.clone()).await?;

                    // Verify subscription exists
                    let counts = bus.subscriber_count().await?;
                    assert_eq!(counts.get(&SignalType::TestPassing), Some(&1));

                    // Kill the subscriber
                    subscriber.stop().await?;
                    tokio::time::sleep(Duration::from_millis(100)).await;
                    """
                expected_output: """
                    // Subscription should be cleaned up
                    let counts = bus.subscriber_count().await?;
                    assert_eq!(counts.get(&SignalType::TestPassing), Some(&0));
                    """
            },
            {
                name:  "test_all_signal_types_work"
                given: "A running signal bus"
                when:  "Each signal type is published"
                then: [
                    "All 8 signal types can be published and received",
                    "Signal data is preserved correctly",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;
                    let subscriber = TestSubscriber::spawn().await?;

                    // Subscribe to all signal types
                    bus.subscribe_all(subscriber.clone()).await?;

                    // Publish each signal type
                    bus.publish(Signal::TestFailure { test_name: "t1".into(), error: "e1".into() }).await?;
                    bus.publish(Signal::TestPassing { test_name: "t2".into(), duration_ms: 100 }).await?;
                    bus.publish(Signal::BeadAssigned { bead_id: "b1".into(), agent_id: "a1".into() }).await?;
                    bus.publish(Signal::PatchProposed { bead_id: "b2".into(), patch_id: "p1".into() }).await?;
                    bus.publish(Signal::LoopSpawned { loop_id: "l1".into(), bead_id: "b3".into() }).await?;
                    bus.publish(Signal::LoopComplete { loop_id: "l2".into(), iterations: 10 }).await?;
                    bus.publish(Signal::LoopFailed { loop_id: "l3".into(), error: "err".into() }).await?;
                    bus.publish(Signal::ShutdownRequested { reason: "test".into() }).await?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(100)).await;

                    let received = subscriber.received_signals().await?;
                    assert_eq!(received.len(), 8);
                    """
            },
            {
                name:  "test_broadcast_stats_returned"
                given: "A signal bus with 3 subscribers"
                when:  "Signal is published"
                then: [
                    "BroadcastStats shows attempted=3, delivered=3, failed=0",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;

                    for _ in 0..3 {
                        let sub = TestSubscriber::spawn().await?;
                        bus.subscribe::<PatchProposed>(sub).await?;
                    }

                    let stats = bus.publish(Signal::PatchProposed {
                        bead_id: "bead-1".into(),
                        patch_id: "patch-1".into(),
                    }).await?;
                    """
                expected_output: """
                    assert_eq!(stats.attempted, 3);
                    assert_eq!(stats.delivered, 3);
                    assert_eq!(stats.failed, 0);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_subscribe_during_shutdown_rejected"
                given: "A signal bus that is shutting down"
                when:  "New subscription is attempted"
                then: [
                    "Returns Err(SubscribeError::ShuttingDown)",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;

                    // Initiate shutdown
                    bus.shutdown().await?;

                    // Try to subscribe after shutdown
                    let subscriber = TestSubscriber::spawn().await?;
                    let result = bus.subscribe::<TestFailure>(subscriber).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::ShuttingDown)));
                    """
            },
            {
                name:  "test_duplicate_subscription_prevented"
                given: "A subscriber already subscribed to a signal type"
                when:  "Same subscriber subscribes to same signal type again"
                then: [
                    "Returns Err(SubscribeError::AlreadySubscribed)",
                    "Original subscription remains active",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;
                    let subscriber = TestSubscriber::spawn().await?;

                    let _handle1 = bus.subscribe::<TestFailure>(subscriber.clone()).await?;
                    let result = bus.subscribe::<TestFailure>(subscriber.clone()).await;
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::AlreadySubscribed { .. })));
                    """
            },
            {
                name:  "test_publish_to_dead_subscriber_handled"
                given: "A subscriber that dies between subscribe and publish"
                when:  "Signal is published"
                then: [
                    "Broadcast completes without error",
                    "BroadcastStats shows the failure",
                    "Dead subscriber cleaned up",
                ]
                real_input: """
                    let bus = SignalBus::spawn(supervisor.clone()).await?;
                    let subscriber = TestSubscriber::spawn().await?;

                    bus.subscribe::<LoopFailed>(subscriber.clone()).await?;

                    // Kill subscriber before publish
                    subscriber.stop().await?;

                    let stats = bus.publish(Signal::LoopFailed {
                        loop_id: "loop-1".into(),
                        error: "timeout".into(),
                    }).await?;
                    """
                expected_output: """
                    // Broadcast should succeed overall
                    assert_eq!(stats.attempted, 1);
                    assert_eq!(stats.failed, 1);
                    assert_eq!(stats.delivered, 0);
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_publish_no_subscribers_succeeds"
                scenario: "Publishing to a signal type with no subscribers"
                input:    "bus.publish(Signal::TestFailure { .. }).await"
                expected: "Returns Ok(BroadcastStats { attempted: 0, delivered: 0, failed: 0 })"
            },
            {
                name:     "test_concurrent_subscribe_publish"
                scenario: "Subscribing and publishing happening simultaneously"
                input:    "100 concurrent subscribe and publish operations"
                expected: "No panics, no data corruption, all operations complete"
            },
            {
                name:     "test_rapid_subscribe_unsubscribe"
                scenario: "Same actor subscribes and unsubscribes rapidly"
                input:    "1000 subscribe/unsubscribe cycles"
                expected: "Final state is unsubscribed, no memory leak"
            },
            {
                name:     "test_large_signal_payload"
                scenario: "Signal with large payload (1MB string)"
                input:    "Signal::TestFailure with 1MB error message"
                expected: "Signal delivered without truncation"
            },
            {
                name:     "test_many_subscribers_single_type"
                scenario: "1000 subscribers for same signal type"
                input:    "Subscribe 1000 actors, publish one signal"
                expected: "All 1000 receive signal, broadcast completes < 100ms"
            },
            {
                name:     "test_subscriber_to_multiple_types"
                scenario: "Single actor subscribes to 5 different signal types"
                input:    "Actor subscribes to TestFailure, TestPassing, BeadAssigned, etc."
                expected: "Receives all 5 signal types, each exactly once"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in signal bus module"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/signal_bus.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public signal bus APIs return Result"
                test:     "cargo doc shows Result returns for spawn, subscribe, publish, unsubscribe"
            },
            {
                name:     "test_invariant_at_most_once"
                verifies: "Each subscriber receives each signal at most once"
                test:     "Publish same signal 3 times, verify subscriber count matches publish count"
            },
            {
                name:     "test_invariant_no_duplicate_subscriptions"
                verifies: "Same subscriber cannot subscribe twice to same signal"
                test:     "Second subscribe returns AlreadySubscribed error"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_signal_bus_lifecycle"
            description: "Complete signal bus lifecycle: spawn -> subscriptions -> broadcast -> cleanup -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/signal_bus_e2e.rs"
                        content: """
                            use factory_core::signal_bus::{SignalBus, Signal, SignalType};
                            use factory_core::supervisor::FactorySupervisor;
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_full_signal_bus_lifecycle() -> Result<()> {
                                // Spawn supervisor and signal bus
                                let supervisor = FactorySupervisor::spawn_root().await?;
                                let bus = SignalBus::spawn(supervisor.clone()).await?;

                                // Create test subscribers
                                let failure_sub = TestSubscriber::spawn().await?;
                                let passing_sub = TestSubscriber::spawn().await?;
                                let all_sub = TestSubscriber::spawn().await?;

                                // Subscribe to specific signals
                                let _h1 = bus.subscribe::<TestFailure>(failure_sub.clone()).await?;
                                let _h2 = bus.subscribe::<TestPassing>(passing_sub.clone()).await?;
                                bus.subscribe_all(all_sub.clone()).await?;

                                // Publish signals
                                bus.publish(Signal::TestFailure {
                                    test_name: "integration_test".into(),
                                    error: "expected failure".into(),
                                }).await?;

                                bus.publish(Signal::TestPassing {
                                    test_name: "unit_test".into(),
                                    duration_ms: 50,
                                }).await?;

                                // Wait for delivery
                                tokio::time::sleep(Duration::from_millis(100)).await;

                                // Verify delivery
                                assert_eq!(failure_sub.received_signals().await?.len(), 1);
                                assert_eq!(passing_sub.received_signals().await?.len(), 1);
                                assert_eq!(all_sub.received_signals().await?.len(), 2);

                                // Kill a subscriber and verify cleanup
                                failure_sub.stop().await?;
                                tokio::time::sleep(Duration::from_millis(100)).await;

                                let counts = bus.subscriber_count().await?;
                                assert_eq!(counts.get(&SignalType::TestFailure), Some(&0));

                                // Graceful shutdown
                                bus.shutdown().await?;
                                supervisor.shutdown().await?;

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
                command:    "moon run factory-core:test -- --test signal_bus_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_signal_bus_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/signal_bus.rs"
                        contains: "pub struct SignalBus"
                    },
                    {
                        path:     "crates/factory-core/src/signal_bus.rs"
                        contains: "pub enum Signal"
                    },
                    {
                        path:     "crates/factory-core/src/signal_bus.rs"
                        contains: "pub struct SubscriptionHandle"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/signal_bus_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_test_runner_integration"
                description: "Signal bus integrated with test runner for real-time test status"
                steps: [
                    {action: "Spawn signal bus under root supervisor", verify: "Bus running"},
                    {action: "Test runner subscribes to TestFailure and TestPassing", verify: "Subscribed"},
                    {action: "Run test suite that has 2 passing and 1 failing test", verify: "Tests execute"},
                    {action: "Verify test runner received 2 TestPassing and 1 TestFailure signals", verify: "All received"},
                    {action: "Test runner aggregates results for summary", verify: "Summary correct"},
                ]
            },
            {
                name:        "e2e_cascading_shutdown"
                description: "ShutdownRequested signal propagates to all subscribers"
                steps: [
                    {action: "Spawn 10 worker actors subscribed to ShutdownRequested", verify: "All subscribed"},
                    {action: "Publish ShutdownRequested signal", verify: "Signal published"},
                    {action: "Verify all 10 workers received the signal", verify: "All received"},
                    {action: "Verify all 10 workers initiated graceful shutdown", verify: "All shutting down"},
                    {action: "Wait for all workers to terminate", verify: "All terminated"},
                ]
            },
            {
                name:        "e2e_loop_lifecycle_tracking"
                description: "Track loop lifecycle via signals"
                steps: [
                    {action: "Subscribe metrics collector to LoopSpawned, LoopComplete, LoopFailed", verify: "Subscribed"},
                    {action: "Spawn 5 loops, 3 complete successfully, 2 fail", verify: "Loops executed"},
                    {action: "Verify 5 LoopSpawned signals received", verify: "Spawned tracked"},
                    {action: "Verify 3 LoopComplete signals received", verify: "Completes tracked"},
                    {action: "Verify 2 LoopFailed signals received", verify: "Failures tracked"},
                    {action: "Metrics collector reports 60% success rate", verify: "Metrics accurate"},
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
                task:      "Write test: test_subscribe_receives_signals"
                file:      "crates/factory-core/src/signal_bus.rs"
                what:      "Test that subscriber receives published signals of subscribed type"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_multiple_subscribers_all_receive"
                file:      "crates/factory-core/src/signal_bus.rs"
                what:      "Test that all subscribers receive broadcast"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_unsubscribe_stops_delivery"
                file:      "crates/factory-core/src/signal_bus.rs"
                what:      "Test that unsubscribe prevents future delivery"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_subscriber_death_auto_cleanup"
                file:      "crates/factory-core/src/signal_bus.rs"
                what:      "Test that dead subscriber is removed automatically"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_duplicate_subscription_prevented"
                file:      "crates/factory-core/src/signal_bus.rs"
                what:      "Test that double subscription returns error"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_broadcast_stats_returned"
                file:      "crates/factory-core/src/signal_bus.rs"
                what:      "Test that publish returns accurate BroadcastStats"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define Signal enum with all event types"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum Signal {
                        TestFailure { test_name: String, error: String },
                        TestPassing { test_name: String, duration_ms: u64 },
                        BeadAssigned { bead_id: String, agent_id: String },
                        PatchProposed { bead_id: String, patch_id: String },
                        LoopSpawned { loop_id: String, bead_id: String },
                        LoopComplete { loop_id: String, iterations: u32 },
                        LoopFailed { loop_id: String, error: String },
                        ShutdownRequested { reason: String },
                    }
                    """
                done_when:     "Enum compiles with all 8 variants"
                patterns_to_use: [
                    "Named struct variants for clarity",
                    "String types for flexibility (can add newtype wrappers later)",
                ]
            },
            {
                task: "Define SignalType enum for subscription keys"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                    pub enum SignalType {
                        TestFailure,
                        TestPassing,
                        BeadAssigned,
                        PatchProposed,
                        LoopSpawned,
                        LoopComplete,
                        LoopFailed,
                        ShutdownRequested,
                    }

                    impl Signal {
                        pub fn signal_type(&self) -> SignalType {
                            match self {
                                Signal::TestFailure { .. } => SignalType::TestFailure,
                                // ... etc
                            }
                        }
                    }
                    """
                done_when:     "SignalType enum with Hash, signal_type() method works"
                patterns_to_use: [
                    "Discriminant enum for HashMap keys",
                    "Method on Signal to get type",
                ]
            },
            {
                task: "Define SubscriptionHandle struct"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    Define RAII handle that unsubscribes on drop.
                    Contains bus reference, subscription ID, signal type.
                    Implements Drop to send Unsubscribe message to bus.
                    """
                done_when:     "Handle compiles, Drop implemented"
                patterns_to_use: [
                    "RAII for automatic cleanup",
                    "Unique subscription ID (Uuid or u64 counter)",
                    "Clone-safe bus reference",
                ]
            },
            {
                task: "Define BroadcastStats struct"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    #[derive(Debug, Clone, Default)]
                    pub struct BroadcastStats {
                        pub attempted: usize,
                        pub delivered: usize,
                        pub failed: usize,
                    }
                    """
                done_when:     "Struct compiles"
                patterns_to_use: ["Simple data struct"]
            },
            {
                task: "Define subscription registry data structure"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    Type alias or struct for subscription storage:
                    HashMap<SignalType, HashMap<SubscriptionId, SubscriberInfo>>
                    SubscriberInfo: { actor_ref, subscription_id }
                    """
                done_when:     "Registry can store and lookup subscriptions"
                patterns_to_use: [
                    "HashMap for O(1) lookup by signal type",
                    "Nested HashMap for O(1) unsubscribe by ID",
                ]
            },
            {
                task: "Implement SignalBus actor"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    Actor that manages subscription registry.
                    Handles: Subscribe, Unsubscribe, Publish, SubscriberDied messages.
                    Monitors linked subscribers for death detection.
                    """
                done_when:     "Basic bus actor running"
                patterns_to_use: [
                    "ractor::Actor trait implementation",
                    "ractor supervision for death detection",
                    "State machine for shutdown handling",
                ]
            },
            {
                task: "Implement subscription logic"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    Subscribe: Add to registry, link subscriber, return handle.
                    Check for duplicates, reject if shutting down.
                    Monitor subscriber for death via linking.
                    """
                done_when:     "Subscription tests pass"
                patterns_to_use: [
                    "Link subscriber to bus for death detection",
                    "Duplicate check before insert",
                    "Atomically allocate subscription ID",
                ]
            },
            {
                task: "Implement broadcast logic"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    Publish: Look up subscribers by signal type.
                    Send signal to each via cast (non-blocking).
                    Track success/failure counts.
                    Handle send errors gracefully.
                    """
                done_when:     "Broadcast tests pass"
                patterns_to_use: [
                    "Iterate subscribers, cast to each",
                    "Log failures, continue broadcasting",
                    "Non-blocking sends only",
                ]
            },
            {
                task: "Implement subscriber death cleanup"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    Handle supervisor event when linked subscriber dies.
                    Remove all subscriptions for that subscriber.
                    Log cleanup for debugging.
                    """
                done_when:     "Death cleanup tests pass"
                patterns_to_use: [
                    "ractor handle_supervisor_evt for death notifications",
                    "Iterate all signal types to find subscriber",
                    "Remove by subscriber ActorId",
                ]
            },
            {
                task: "Implement SignalBusRef handle"
                file: "crates/factory-core/src/signal_bus.rs"
                what: """
                    Client-facing API: subscribe, unsubscribe, publish, subscriber_count, shutdown.
                    Wraps ActorRef with ergonomic methods.
                    """
                done_when:     "All SignalBusRef methods work"
                patterns_to_use: [
                    "Newtype wrapper around ActorRef",
                    "Typed helper methods for each signal type",
                    "subscribe_all for subscribing to all types",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export signal_bus module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod signal_bus;"
                done_when: "External crates can import factory_core::signal_bus"
            },
            {
                task:      "Add signal bus errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "SubscribeError, PublishError, UnsubscribeError, AlreadySubscribed variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Add signal bus to root supervisor child specs"
                file:      "crates/factory-core/src/supervisor.rs"
                what:      "Register SignalBus as child of root supervisor"
                done_when: "Signal bus starts with supervisor"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/signal_bus.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Run stress test"
                done_when: "1000 concurrent subscribe/publish cycles complete without deadlock"
                commands: [
                    "cargo test --release -- signal_bus_stress_test --ignored",
                ]
                expected: "Test passes within timeout"
            },
            {
                task:      "Run memory leak test"
                done_when: "No subscription memory leak after 10000 subscribe/unsubscribe cycles"
                commands: [
                    "cargo test --release -- signal_bus_memory_test --ignored",
                ]
                expected: "Memory stable, no growth"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Subscriber never receives signals"
                likely_cause: "Subscription not registered or signal type mismatch"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "handle_subscribe()"
                        what_to_check: "Is subscription added to registry for correct SignalType?"
                    },
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "handle_publish()"
                        what_to_check: "Is correct SignalType used for lookup?"
                    },
                ]
                fix_pattern: "Verify SignalType derivation from Signal, check registry contents"
            },
            {
                symptom:      "Signal bus hangs during broadcast"
                likely_cause: "Blocking send to subscriber with full mailbox"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "broadcast_signal()"
                        what_to_check: "Is cast() used (non-blocking) instead of call()?"
                    },
                ]
                fix_pattern: "Use cast for fire-and-forget, never await subscriber response"
            },
            {
                symptom:      "Dead subscribers not cleaned up"
                likely_cause: "Link not established between bus and subscriber"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "handle_subscribe()"
                        what_to_check: "Is subscriber linked/monitored after subscription?"
                    },
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "handle_supervisor_evt()"
                        what_to_check: "Is ActorTerminated event handled?"
                    },
                ]
                fix_pattern: "Link subscriber on subscribe, handle ActorTerminated event"
            },
            {
                symptom:      "SubscriptionHandle drop doesn't unsubscribe"
                likely_cause: "Drop implementation not sending message or message lost"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "impl Drop for SubscriptionHandle"
                        what_to_check: "Is Unsubscribe message sent to bus?"
                    },
                ]
                fix_pattern: "Send Unsubscribe message in Drop, handle potential send failure gracefully"
            },
            {
                symptom:      "Duplicate signals delivered"
                likely_cause: "Duplicate subscription created despite check"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "handle_subscribe()"
                        what_to_check: "Is duplicate check happening atomically?"
                    },
                ]
                fix_pattern: "Check and insert must be atomic; actor serializes all operations"
            },
            {
                symptom:      "Panic during broadcast"
                likely_cause: "Iterator invalidation from concurrent modification"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/signal_bus.rs"
                        function:      "broadcast_signal()"
                        what_to_check: "Are we modifying registry while iterating?"
                    },
                ]
                fix_pattern: "Clone subscriber list before iterating, or collect to Vec first"
            },
        ]

        debugging_commands: [
            {
                scenario: "When signals are not being delivered"
                run:      "RUST_LOG=factory_core::signal_bus=trace cargo test"
                look_for: "Subscribe/Publish messages received, subscriber lookups"
            },
            {
                scenario: "When subscription cleanup fails"
                run:      "Add tracing: tracing::debug!(registry = ?self.subscriptions)"
                look_for: "Registry state before/after operations"
            },
            {
                scenario: "When broadcast seems slow"
                run:      "tokio-console or RUST_LOG=tokio=trace"
                look_for: "Blocked tasks, mailbox backpressure"
            },
            {
                scenario: "When subscriber death not detected"
                run:      "RUST_LOG=ractor=debug"
                look_for: "Link/monitor established, exit signals"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_subscribe_receives_signals passes",
            "[ ] test_multiple_subscribers_all_receive passes",
            "[ ] test_unsubscribe_stops_delivery passes",
            "[ ] test_subscriber_death_auto_cleanup passes",
            "[ ] test_all_signal_types_work passes",
            "[ ] test_broadcast_stats_returned passes",
            "[ ] test_subscribe_during_shutdown_rejected passes",
            "[ ] test_duplicate_subscription_prevented passes",
            "[ ] test_publish_to_dead_subscriber_handled passes",
            "[ ] test_publish_no_subscribers_succeeds passes",
            "[ ] test_concurrent_subscribe_publish passes",
            "[ ] E2E full signal bus lifecycle test passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] Signal enum with all 8 variants",
            "[ ] SignalType enum for subscription keys",
            "[ ] SubscriptionHandle with RAII drop",
            "[ ] BroadcastStats struct",
            "[ ] SignalBus actor implementation",
            "[ ] SignalBusRef handle implementation",
            "[ ] Subscriber death detection via linking",
            "[ ] Duplicate subscription prevention",
            "[ ] Non-blocking broadcast",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs explaining pub/sub pattern",
            "[ ] Signal enum variants documented with use cases",
            "[ ] SubscriptionHandle usage documented",
            "[ ] Example subscription and publish in doc comments",
            "[ ] At-most-once delivery semantics documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add signal_bus"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add signal bus error variants"
            },
            {
                path:      "crates/factory-core/src/actor.rs"
                relevance: "Actor framework - signal bus is an actor"
            },
            {
                path:      "crates/factory-core/src/supervisor.rs"
                relevance: "Root supervisor - signal bus runs under it"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-00s"
                description: "Actor framework must be in place"
                what_it_provides: "ractor Actor trait, ActorRef, message passing, linking"
            },
            {
                bead_id:     "factory-4pz"
                description: "Root supervisor must be in place"
                what_it_provides: "FactorySupervisor, ChildSpec for running signal bus"
            },
        ]

        external_references: [
            "https://www.erlang.org/doc/man/pg.html - Erlang process groups (similar concept)",
            "https://docs.rs/ractor - Ractor actor framework",
            "https://hexdocs.pm/phoenix_pubsub - Phoenix PubSub for design inspiration",
            "https://tokio.rs/tokio/tutorial/channels - Tokio channel patterns",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Actor Handle Pattern"
                example_location: "crates/factory-core/src/supervisor.rs:SupervisorRef"
                how_to_apply:     "Separate SignalBus actor from SignalBusRef client handle"
            },
            {
                pattern:          "RAII Cleanup"
                example_location: "Standard Rust Drop trait"
                how_to_apply:     "SubscriptionHandle sends Unsubscribe on drop"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use ractor linking to detect subscriber death automatically",
            "Use HashMap<SignalType, HashMap<SubscriptionId, Subscriber>> for O(1) ops",
            "Use cast() for broadcast (non-blocking fire-and-forget)",
            "Generate unique SubscriptionId via AtomicU64 counter",
            "Clone subscriber list before iterating during broadcast",
            "Log all subscribe/unsubscribe/publish operations with tracing",
            "Use SubscriptionHandle RAII pattern for automatic cleanup",
            "Test with rapid subscribe/unsubscribe to verify no race conditions",
            "Make Signal and SignalType Clone for easy broadcast",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT use call() for broadcast (causes blocking)",
            "Do NOT modify registry while iterating",
            "Do NOT block on any single subscriber during broadcast",
            "Do NOT forget to link subscribers for death detection",
            "Do NOT allow duplicate subscriptions",
        ]

        code_patterns: [
            {
                name:     "Signal Enum"
                use_when: "Defining the signal types"
                example:  """
                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum Signal {
                        TestFailure { test_name: String, error: String },
                        TestPassing { test_name: String, duration_ms: u64 },
                        BeadAssigned { bead_id: String, agent_id: String },
                        PatchProposed { bead_id: String, patch_id: String },
                        LoopSpawned { loop_id: String, bead_id: String },
                        LoopComplete { loop_id: String, iterations: u32 },
                        LoopFailed { loop_id: String, error: String },
                        ShutdownRequested { reason: String },
                    }
                    """
            },
            {
                name:     "Subscription Registry"
                use_when: "Storing and looking up subscriptions"
                example:  """
                    type SubscriptionId = u64;

                    struct SubscriberInfo {
                        actor_id: ActorId,
                        actor_ref: ActorRef<SignalMessage>,
                        subscription_id: SubscriptionId,
                    }

                    struct SubscriptionRegistry {
                        by_type: HashMap<SignalType, HashMap<SubscriptionId, SubscriberInfo>>,
                        by_actor: HashMap<ActorId, HashSet<(SignalType, SubscriptionId)>>,
                        next_id: AtomicU64,
                    }

                    impl SubscriptionRegistry {
                        fn subscribe(
                            &mut self,
                            signal_type: SignalType,
                            actor_ref: ActorRef<SignalMessage>,
                        ) -> Result<SubscriptionId> {
                            let actor_id = actor_ref.get_id();

                            // Check for duplicate
                            if let Some(actor_subs) = self.by_actor.get(&actor_id) {
                                if actor_subs.iter().any(|(st, _)| *st == signal_type) {
                                    return Err(Error::AlreadySubscribed { actor_id, signal_type });
                                }
                            }

                            let sub_id = self.next_id.fetch_add(1, Ordering::Relaxed);
                            let info = SubscriberInfo { actor_id, actor_ref, subscription_id: sub_id };

                            self.by_type
                                .entry(signal_type)
                                .or_default()
                                .insert(sub_id, info);

                            self.by_actor
                                .entry(actor_id)
                                .or_default()
                                .insert((signal_type, sub_id));

                            Ok(sub_id)
                        }

                        fn remove_by_actor(&mut self, actor_id: ActorId) {
                            if let Some(subs) = self.by_actor.remove(&actor_id) {
                                for (signal_type, sub_id) in subs {
                                    if let Some(type_subs) = self.by_type.get_mut(&signal_type) {
                                        type_subs.remove(&sub_id);
                                    }
                                }
                            }
                        }
                    }
                    """
            },
            {
                name:     "SubscriptionHandle RAII"
                use_when: "Creating handle that auto-unsubscribes on drop"
                example:  """
                    pub struct SubscriptionHandle {
                        bus_ref: SignalBusRef,
                        subscription_id: SubscriptionId,
                        signal_type: SignalType,
                        dropped: AtomicBool,
                    }

                    impl SubscriptionHandle {
                        pub fn new(
                            bus_ref: SignalBusRef,
                            subscription_id: SubscriptionId,
                            signal_type: SignalType,
                        ) -> Self {
                            Self {
                                bus_ref,
                                subscription_id,
                                signal_type,
                                dropped: AtomicBool::new(false),
                            }
                        }

                        pub fn unsubscribe(self) {
                            // Consumed, drop will handle cleanup
                        }
                    }

                    impl Drop for SubscriptionHandle {
                        fn drop(&mut self) {
                            if !self.dropped.swap(true, Ordering::Relaxed) {
                                // Best-effort unsubscribe, ignore errors
                                let _ = self.bus_ref.inner.cast(
                                    SignalBusMsg::Unsubscribe {
                                        subscription_id: self.subscription_id,
                                        signal_type: self.signal_type,
                                    }
                                );
                            }
                        }
                    }
                    """
            },
            {
                name:     "Non-blocking Broadcast"
                use_when: "Publishing signal to all subscribers"
                example:  """
                    fn broadcast_signal(
                        &self,
                        signal: Signal,
                        registry: &SubscriptionRegistry,
                    ) -> BroadcastStats {
                        let signal_type = signal.signal_type();
                        let mut stats = BroadcastStats::default();

                        let subscribers = registry.by_type
                            .get(&signal_type)
                            .map(|subs| subs.values().cloned().collect::<Vec<_>>())
                            .unwrap_or_default();

                        for sub in subscribers {
                            stats.attempted += 1;
                            match sub.actor_ref.cast(SignalMessage::Signal(signal.clone())) {
                                Ok(()) => stats.delivered += 1,
                                Err(e) => {
                                    stats.failed += 1;
                                    tracing::warn!(
                                        subscriber = ?sub.actor_id,
                                        error = ?e,
                                        "Failed to deliver signal"
                                    );
                                }
                            }
                        }

                        stats
                    }
                    """
            },
            {
                name:     "Subscriber Death Handler"
                use_when: "Cleaning up subscriptions when subscriber dies"
                example:  """
                    impl Actor for SignalBus {
                        // ...

                        async fn handle_supervisor_evt(
                            &self,
                            _myself: ActorRef<Self::Msg>,
                            message: SupervisorEvt,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            match message {
                                SupervisorEvt::ActorTerminated(actor_cell)
                                | SupervisorEvt::ActorPanicked(actor_cell, _) => {
                                    let actor_id = actor_cell.get_id();
                                    tracing::info!(
                                        actor_id = ?actor_id,
                                        "Subscriber died, cleaning up subscriptions"
                                    );
                                    state.registry.remove_by_actor(actor_id);
                                }
                                _ => {}
                            }
                            Ok(())
                        }
                    }
                    """
            },
        ]
    }
}
