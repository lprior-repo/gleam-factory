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

"factory-192": #ValidBead & {
    // ============================================================================
    // BEAD: factory-192 - Implement factory dispatcher to spawn factory loops
    // ============================================================================

    id:              "factory-192"
    title:           "Runtime: Implement factory dispatcher to spawn factory loops on BeadAssigned"
    type:            "feature"
    priority:        1
    effort_estimate: "4hr"
    labels:          ["runtime", "actors", "dispatcher", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL implement FactoryDispatcher as a supervised actor under RootSupervisor",
            "THE SYSTEM SHALL subscribe to BeadAssigned events on the SignalBus at startup",
            "THE SYSTEM SHALL track all active FactoryLoop actors in a HashMap<BeadId, ActorRef<FactoryLoop>>",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL emit LoopSpawned, LoopComplete, and LoopFailed signals to SignalBus",
        ]

        event_driven: [
            {
                trigger: "WHEN a BeadAssigned event is received"
                shall:   "THE SYSTEM SHALL spawn a new FactoryLoop actor for the assigned bead"
            },
            {
                trigger: "WHEN a FactoryLoop actor is spawned successfully"
                shall:   "THE SYSTEM SHALL emit LoopSpawned(bead_id) to the SignalBus"
            },
            {
                trigger: "WHEN a FactoryLoop actor completes successfully"
                shall:   "THE SYSTEM SHALL emit LoopComplete(bead_id) and remove from active loops"
            },
            {
                trigger: "WHEN a FactoryLoop actor fails with an error"
                shall:   "THE SYSTEM SHALL emit LoopFailed(bead_id, error) and remove from active loops"
            },
            {
                trigger: "WHEN a FactoryLoop actor crashes unexpectedly"
                shall:   "THE SYSTEM SHALL emit LoopFailed(bead_id, crash_reason) via supervisor callback"
            },
            {
                trigger: "WHEN ShutdownRequested signal is received"
                shall:   "THE SYSTEM SHALL stop all active FactoryLoop actors gracefully"
            },
            {
                trigger: "WHEN a duplicate BeadAssigned event is received for an active loop"
                shall:   "THE SYSTEM SHALL ignore the event and log a warning"
            },
        ]

        state_driven: [
            {
                state: "WHILE the dispatcher is running"
                shall: "THE SYSTEM SHALL maintain accurate count of active loops via active_loops.len()"
            },
            {
                state: "WHILE shutdown is in progress"
                shall: "THE SYSTEM SHALL reject new BeadAssigned events with Error::ShuttingDown"
            },
            {
                state: "WHILE the ResourceGovernor reports at capacity"
                shall: "THE SYSTEM SHALL queue BeadAssigned events until capacity is available"
            },
        ]

        unwanted: [
            {
                condition: "IF a FactoryLoop spawn fails"
                shall_not: "THE SYSTEM SHALL NOT crash the dispatcher"
                because:   "One failed spawn should not take down the entire dispatch system"
            },
            {
                condition: "IF the SignalBus is unavailable"
                shall_not: "THE SYSTEM SHALL NOT silently drop signals"
                because:   "Lost signals cause inconsistent state and orphaned resources"
            },
            {
                condition: "IF an unknown message type is received"
                shall_not: "THE SYSTEM SHALL NOT panic or crash"
                because:   "Unknown messages should be logged and ignored for forward compatibility"
            },
            {
                condition: "IF ResourceGovernor denies a spawn request"
                shall_not: "THE SYSTEM SHALL NOT retry immediately without backoff"
                because:   "Immediate retries cause thundering herd and resource starvation"
            },
        ]

        complex: [
            {
                state:   "WHILE shutdown is in progress"
                trigger: "WHEN a new BeadAssigned event arrives"
                shall:   "THE SYSTEM SHALL return Error::ShuttingDown and not spawn new loops"
            },
            {
                state:   "WHILE ResourceGovernor is at capacity"
                trigger: "WHEN a BeadAssigned event arrives"
                shall:   "THE SYSTEM SHALL queue the event and retry when LoopComplete frees capacity"
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
                    type:            "ActorRef<SignalBus>"
                    constraints:     "Must be a valid, running SignalBus actor reference"
                    example_valid:   "ActorRef from SignalBus::spawn()"
                    example_invalid: "Dropped or terminated ActorRef"
                },
                {
                    field:           "resource_governor"
                    type:            "ActorRef<ResourceGovernor>"
                    constraints:     "Must be a valid, running ResourceGovernor actor reference"
                    example_valid:   "ActorRef from ResourceGovernor::spawn()"
                    example_invalid: "None or null reference"
                },
                {
                    field:           "supervisor"
                    type:            "ActorRef<RootSupervisor>"
                    constraints:     "Dispatcher must be spawned under RootSupervisor"
                    example_valid:   "supervisor.spawn_child(FactoryDispatcher::new(...))"
                    example_invalid: "Orphan spawn without supervisor"
                },
            ]
            system_state: [
                "RootSupervisor is running (factory-4pz)",
                "SignalBus is running and accepting subscriptions (factory-kou)",
                "ResourceGovernor is running and accepting permit requests (factory-kjt)",
                "Tokio runtime is active",
            ]
        }

        postconditions: {
            state_changes: [
                "FactoryDispatcher actor is running under RootSupervisor",
                "Dispatcher is subscribed to BeadAssigned events on SignalBus",
                "Dispatcher is subscribed to ShutdownRequested events on SignalBus",
                "active_loops HashMap is initialized empty",
                "pending_queue VecDeque is initialized empty",
            ]
            return_guarantees: [
                {
                    field:     "FactoryDispatcher::spawn()"
                    guarantee: "Returns Result<ActorRef<FactoryDispatcher>, Error>"
                },
                {
                    field:     "handle_bead_assigned()"
                    guarantee: "Returns Result<(), Error> with LoopSpawned signal on success"
                },
                {
                    field:     "active_loop_count()"
                    guarantee: "Returns usize matching active_loops.len()"
                },
                {
                    field:     "shutdown()"
                    guarantee: "Returns Result<(), Error> after all loops stopped"
                },
            ]
            side_effects: [
                "LoopSpawned signal emitted on successful FactoryLoop spawn",
                "LoopComplete signal emitted when FactoryLoop finishes successfully",
                "LoopFailed signal emitted when FactoryLoop fails or crashes",
                "ResourceGovernor permit released when loop completes",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "active_loops.len() always equals actual running FactoryLoop count",
            "Every spawned FactoryLoop has a corresponding entry in active_loops",
            "Every completed/failed loop is removed from active_loops",
            "pending_queue.len() equals BeadAssigned events waiting for capacity",
            "Dispatcher only spawns loops when ResourceGovernor grants permit",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "SignalBus subscription fails during dispatcher init"
                prevention:  "Retry subscription with exponential backoff, fail init after max retries"
                test_for_it: "test_dispatcher_init_retries_subscription"
            },
            {
                failure:     "ResourceGovernor unavailable when spawn requested"
                prevention:  "Queue the request, subscribe to capacity-available events"
                test_for_it: "test_dispatcher_queues_when_governor_unavailable"
            },
            {
                failure:     "FactoryLoop actor type not compatible with supervisor"
                prevention:  "Implement ractor::Actor trait with proper message types"
                test_for_it: "test_factory_loop_implements_actor_trait"
            },
            {
                failure:     "Message types don't implement required Send + Sync traits"
                prevention:  "Derive Debug, Clone, Send, Sync on all message types"
                test_for_it: "test_message_types_are_send_sync"
            },
        ]

        usability_failures: [
            {
                failure:     "No visibility into dispatcher state for debugging"
                prevention:  "Implement GetStatus message that returns DispatcherStatus struct"
                test_for_it: "test_dispatcher_status_returns_state"
            },
            {
                failure:     "Errors don't indicate which bead caused failure"
                prevention:  "Include bead_id in all error variants: LoopSpawnFailed { bead_id, reason }"
                test_for_it: "test_error_messages_include_bead_id"
            },
            {
                failure:     "No way to cancel a specific running loop"
                prevention:  "Implement CancelLoop(bead_id) message"
                test_for_it: "test_cancel_loop_stops_specific_loop"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "active_loops map gets out of sync with actual running loops"
                prevention:  "Use RAII pattern: always remove on drop via supervisor callback"
                test_for_it: "test_active_loops_sync_on_crash"
            },
            {
                failure:     "Duplicate loop spawned for same bead"
                prevention:  "Check active_loops.contains_key(bead_id) before spawning"
                test_for_it: "test_no_duplicate_loops_for_same_bead"
            },
            {
                failure:     "Pending queue grows unbounded under load"
                prevention:  "Set max_pending_queue_size, reject with Error::QueueFull when exceeded"
                test_for_it: "test_pending_queue_has_max_size"
            },
            {
                failure:     "Signals lost during SignalBus restart"
                prevention:  "Re-subscribe on SignalBus reconnect, use persistent queue"
                test_for_it: "test_resubscribe_after_signal_bus_restart"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_spawn_loop_on_bead_assigned"
                given: "A running FactoryDispatcher subscribed to SignalBus"
                when:  "BeadAssigned(bead_id) event is published"
                then: [
                    "FactoryLoop actor is spawned for bead_id",
                    "LoopSpawned(bead_id) signal is emitted",
                    "active_loops contains bead_id entry",
                ]
                real_input: """
                    use factory_core::actors::{FactoryDispatcher, DispatcherMsg};
                    use factory_core::signals::{Signal, BeadAssigned};

                    let dispatcher = FactoryDispatcher::spawn(signal_bus.clone(), governor.clone()).await?;

                    signal_bus.cast(Signal::BeadAssigned(BeadAssigned {
                        bead_id: "factory-192".into(),
                        priority: 1,
                    }))?;
                    """
                expected_output: """
                    // Wait for signal processing
                    tokio::time::sleep(Duration::from_millis(50)).await;

                    let status = dispatcher.call(DispatcherMsg::GetStatus).await?;
                    assert_eq!(status.active_loop_count, 1);
                    assert!(status.active_beads.contains(&"factory-192".into()));
                    """
            },
            {
                name:  "test_loop_complete_removes_from_active"
                given: "A running FactoryLoop managed by dispatcher"
                when:  "FactoryLoop completes successfully"
                then: [
                    "LoopComplete(bead_id) signal is emitted",
                    "bead_id is removed from active_loops",
                    "active_loop_count decrements by 1",
                ]
                real_input: """
                    // Spawn a loop
                    dispatcher.cast(DispatcherMsg::SpawnLoop { bead_id: "test-bead".into() })?;
                    tokio::time::sleep(Duration::from_millis(50)).await;

                    // Simulate loop completion
                    dispatcher.cast(DispatcherMsg::LoopCompleted { bead_id: "test-bead".into() })?;
                    """
                expected_output: """
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    let status = dispatcher.call(DispatcherMsg::GetStatus).await?;
                    assert_eq!(status.active_loop_count, 0);
                    assert!(!status.active_beads.contains(&"test-bead".into()));
                    """
            },
            {
                name:  "test_graceful_shutdown_stops_all_loops"
                given: "Multiple active FactoryLoops"
                when:  "ShutdownRequested signal is received"
                then: [
                    "All FactoryLoop actors receive stop message",
                    "All loops complete or timeout within shutdown_timeout",
                    "active_loops is empty after shutdown",
                    "Dispatcher terminates cleanly",
                ]
                real_input: """
                    // Spawn multiple loops
                    for i in 0..3 {
                        dispatcher.cast(DispatcherMsg::SpawnLoop {
                            bead_id: format!("bead-{i}").into(),
                        })?;
                    }
                    tokio::time::sleep(Duration::from_millis(100)).await;

                    signal_bus.cast(Signal::ShutdownRequested)?;
                    """
                expected_output: """
                    // Wait for shutdown
                    tokio::time::sleep(Duration::from_millis(500)).await;

                    // Dispatcher should be stopped
                    let result = dispatcher.call(DispatcherMsg::GetStatus).await;
                    assert!(result.is_err()); // Actor stopped
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_spawn_loop_fails_returns_error"
                given: "ResourceGovernor at capacity"
                when:  "BeadAssigned event is received"
                then: [
                    "Loop is not spawned immediately",
                    "Event is queued in pending_queue",
                    "No LoopSpawned signal emitted",
                ]
                real_input: """
                    // Set governor to capacity
                    governor.cast(GovernorMsg::SetMaxLoops(0))?;

                    signal_bus.cast(Signal::BeadAssigned(BeadAssigned {
                        bead_id: "blocked-bead".into(),
                        priority: 1,
                    }))?;
                    """
                expected_output: null
                expected_error: """
                    let status = dispatcher.call(DispatcherMsg::GetStatus).await?;
                    assert_eq!(status.pending_queue_len, 1);
                    assert_eq!(status.active_loop_count, 0);
                    """
            },
            {
                name:  "test_duplicate_bead_assigned_ignored"
                given: "FactoryLoop already running for bead_id"
                when:  "Duplicate BeadAssigned event received"
                then: [
                    "Event is ignored",
                    "No new loop spawned",
                    "Warning logged",
                    "active_loop_count unchanged",
                ]
                real_input: """
                    dispatcher.cast(DispatcherMsg::SpawnLoop { bead_id: "dup-bead".into() })?;
                    tokio::time::sleep(Duration::from_millis(50)).await;

                    let before = dispatcher.call(DispatcherMsg::GetStatus).await?;

                    // Try duplicate
                    signal_bus.cast(Signal::BeadAssigned(BeadAssigned {
                        bead_id: "dup-bead".into(),
                        priority: 1,
                    }))?;
                    """
                expected_output: null
                expected_error: """
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    let after = dispatcher.call(DispatcherMsg::GetStatus).await?;
                    assert_eq!(before.active_loop_count, after.active_loop_count);
                    """
            },
            {
                name:  "test_loop_crash_emits_failed_signal"
                given: "A running FactoryLoop"
                when:  "FactoryLoop crashes unexpectedly"
                then: [
                    "LoopFailed(bead_id, crash_reason) signal emitted",
                    "Loop removed from active_loops",
                    "Dispatcher continues running",
                ]
                real_input: """
                    dispatcher.cast(DispatcherMsg::SpawnLoop { bead_id: "crash-bead".into() })?;
                    tokio::time::sleep(Duration::from_millis(50)).await;

                    // Simulate crash via supervisor callback
                    dispatcher.cast(DispatcherMsg::ChildExited {
                        bead_id: "crash-bead".into(),
                        reason: ExitReason::Killed,
                    })?;
                    """
                expected_output: null
                expected_error: """
                    tokio::time::sleep(Duration::from_millis(50)).await;
                    let status = dispatcher.call(DispatcherMsg::GetStatus).await?;
                    assert_eq!(status.active_loop_count, 0);
                    // Check LoopFailed was emitted via signal bus subscriber
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_high_volume_bead_assigned_events"
                scenario: "100 BeadAssigned events in rapid succession"
                input:    "Fire 100 BeadAssigned events within 100ms"
                expected: "All events processed, no lost events, no deadlocks"
            },
            {
                name:     "test_shutdown_with_pending_queue"
                scenario: "Shutdown requested while events are queued"
                input:    "Queue 10 events, then ShutdownRequested"
                expected: "Pending events cleared, graceful shutdown completes"
            },
            {
                name:     "test_signal_bus_reconnect"
                scenario: "SignalBus restarts while dispatcher running"
                input:    "Kill and restart SignalBus actor"
                expected: "Dispatcher re-subscribes, resumes normal operation"
            },
            {
                name:     "test_zero_capacity_governor"
                scenario: "ResourceGovernor configured with 0 max_loops"
                input:    "Send BeadAssigned event"
                expected: "Event queued, not rejected, waits for capacity"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in dispatcher module"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/actors/dispatcher.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public APIs return Result"
                test:     "All pub fn signatures return Result<T, Error>"
            },
            {
                name:     "test_invariant_active_loops_sync"
                verifies: "active_loops.len() equals actual running loops"
                test:     "After any operation, query supervisor for child count, compare to active_loops.len()"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_dispatcher_lifecycle"
            description: "Complete dispatcher lifecycle: init -> receive events -> spawn loops -> shutdown"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/dispatcher_e2e.rs"
                        content: """
                            use factory_core::actors::{
                                FactoryDispatcher, RootSupervisor, SignalBus, ResourceGovernor
                            };
                            use factory_core::signals::Signal;
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_full_dispatcher_lifecycle() -> Result<()> {
                                // Setup supervisor tree
                                let supervisor = RootSupervisor::spawn().await?;
                                let signal_bus = supervisor.spawn_child::<SignalBus>().await?;
                                let governor = supervisor.spawn_child::<ResourceGovernor>().await?;

                                // Spawn dispatcher
                                let dispatcher = FactoryDispatcher::spawn(
                                    signal_bus.clone(),
                                    governor.clone(),
                                ).await?;

                                // Send BeadAssigned
                                signal_bus.cast(Signal::BeadAssigned(BeadAssigned {
                                    bead_id: "e2e-test-bead".into(),
                                    priority: 1,
                                }))?;

                                tokio::time::sleep(Duration::from_millis(100)).await;

                                // Verify loop spawned
                                let status = dispatcher.call(DispatcherMsg::GetStatus).await?;
                                assert_eq!(status.active_loop_count, 1);

                                // Shutdown
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
                command:    "moon run factory-core:test -- --test dispatcher_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_dispatcher_lifecycle ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/actors/dispatcher.rs"
                        contains: "pub struct FactoryDispatcher"
                    },
                    {
                        path:     "crates/factory-core/src/actors/mod.rs"
                        contains: "pub mod dispatcher"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/dispatcher_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_multi_bead_processing"
                description: "Verify dispatcher handles multiple concurrent beads"
                steps: [
                    {action: "Start supervisor tree with dispatcher", verify: "Dispatcher running"},
                    {action: "Send 5 BeadAssigned events", verify: "5 loops spawned"},
                    {action: "Wait for 2 loops to complete", verify: "active_loops = 3"},
                    {action: "Send 2 more BeadAssigned events", verify: "active_loops = 5"},
                    {action: "Shutdown", verify: "All loops stopped, dispatcher terminated"},
                ]
            },
            {
                name:        "e2e_backpressure_handling"
                description: "Verify dispatcher respects ResourceGovernor limits"
                steps: [
                    {action: "Configure governor with max_loops = 2", verify: "Config applied"},
                    {action: "Send 5 BeadAssigned events", verify: "2 loops spawned, 3 queued"},
                    {action: "Complete 1 loop", verify: "1 queued event processed, active = 2"},
                    {action: "Complete all loops", verify: "All 5 beads processed sequentially"},
                ]
            },
            {
                name:        "e2e_crash_recovery"
                description: "Verify dispatcher handles loop crashes gracefully"
                steps: [
                    {action: "Spawn dispatcher with 3 loops", verify: "3 active loops"},
                    {action: "Force crash on loop 2", verify: "LoopFailed emitted, active = 2"},
                    {action: "Send new BeadAssigned", verify: "New loop spawned, active = 3"},
                    {action: "Verify dispatcher still healthy", verify: "GetStatus succeeds"},
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
                task:      "Write test: test_spawn_loop_on_bead_assigned"
                file:      "crates/factory-core/src/actors/dispatcher.rs"
                what:      "Test that BeadAssigned event spawns FactoryLoop"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_loop_complete_removes_from_active"
                file:      "crates/factory-core/src/actors/dispatcher.rs"
                what:      "Test that LoopComplete removes from active_loops"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_duplicate_bead_assigned_ignored"
                file:      "crates/factory-core/src/actors/dispatcher.rs"
                what:      "Test that duplicate events are ignored"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_graceful_shutdown_stops_all_loops"
                file:      "crates/factory-core/src/actors/dispatcher.rs"
                what:      "Test graceful shutdown behavior"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_pending_queue_on_capacity"
                file:      "crates/factory-core/src/actors/dispatcher.rs"
                what:      "Test events queued when at capacity"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define DispatcherMsg enum"
                file: "crates/factory-core/src/actors/dispatcher.rs"
                what: """
                    #[derive(Debug, Clone)]
                    pub enum DispatcherMsg {
                        BeadAssigned { bead_id: BeadId, priority: u8 },
                        LoopCompleted { bead_id: BeadId },
                        LoopFailed { bead_id: BeadId, reason: String },
                        ChildExited { bead_id: BeadId, reason: ExitReason },
                        ShutdownRequested,
                        GetStatus { respond_to: oneshot::Sender<DispatcherStatus> },
                        CancelLoop { bead_id: BeadId },
                    }
                    """
                done_when:     "Message enum compiles with all variants"
                patterns_to_use: ["Newtype for BeadId", "oneshot for request-reply"]
            },
            {
                task: "Define DispatcherState struct"
                file: "crates/factory-core/src/actors/dispatcher.rs"
                what: """
                    pub struct DispatcherState {
                        signal_bus: ActorRef<SignalBus>,
                        resource_governor: ActorRef<ResourceGovernor>,
                        active_loops: HashMap<BeadId, ActorRef<FactoryLoop>>,
                        pending_queue: VecDeque<PendingBead>,
                        shutting_down: bool,
                    }
                    """
                done_when:     "State struct compiles with all fields"
                patterns_to_use: ["HashMap for O(1) lookup", "VecDeque for FIFO queue"]
            },
            {
                task: "Implement Actor trait for FactoryDispatcher"
                file: "crates/factory-core/src/actors/dispatcher.rs"
                what: """
                    #[async_trait]
                    impl Actor for FactoryDispatcher {
                        type Msg = DispatcherMsg;
                        type State = DispatcherState;
                        type Arguments = DispatcherArgs;

                        async fn pre_start(
                            &self,
                            myself: ActorRef<Self::Msg>,
                            args: Self::Arguments,
                        ) -> Result<Self::State, ActorProcessingErr> {
                            // Subscribe to SignalBus
                            args.signal_bus
                                .cast(SignalBusMsg::Subscribe {
                                    actor: myself.clone(),
                                    events: vec![SignalType::BeadAssigned, SignalType::ShutdownRequested],
                                })
                                .map_err(|e| ActorProcessingErr::from(e.to_string()))?;

                            Ok(DispatcherState {
                                signal_bus: args.signal_bus,
                                resource_governor: args.resource_governor,
                                active_loops: HashMap::new(),
                                pending_queue: VecDeque::new(),
                                shutting_down: false,
                            })
                        }

                        async fn handle(
                            &self,
                            myself: ActorRef<Self::Msg>,
                            message: Self::Msg,
                            state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            match message {
                                DispatcherMsg::BeadAssigned { bead_id, priority } => {
                                    self.handle_bead_assigned(myself, bead_id, priority, state).await
                                }
                                DispatcherMsg::LoopCompleted { bead_id } => {
                                    self.handle_loop_completed(bead_id, state).await
                                }
                                DispatcherMsg::LoopFailed { bead_id, reason } => {
                                    self.handle_loop_failed(bead_id, reason, state).await
                                }
                                DispatcherMsg::ShutdownRequested => {
                                    self.handle_shutdown(state).await
                                }
                                DispatcherMsg::GetStatus { respond_to } => {
                                    let status = DispatcherStatus {
                                        active_loop_count: state.active_loops.len(),
                                        active_beads: state.active_loops.keys().cloned().collect(),
                                        pending_queue_len: state.pending_queue.len(),
                                        shutting_down: state.shutting_down,
                                    };
                                    let _ = respond_to.send(status);
                                    Ok(())
                                }
                                DispatcherMsg::CancelLoop { bead_id } => {
                                    self.handle_cancel_loop(bead_id, state).await
                                }
                                DispatcherMsg::ChildExited { bead_id, reason } => {
                                    self.handle_child_exited(bead_id, reason, state).await
                                }
                            }
                        }
                    }
                    """
                done_when:     "Actor trait implementation compiles"
                patterns_to_use: [
                    "Result<(), ActorProcessingErr> for all handlers",
                    "Match exhaustively on message variants",
                    "Delegate to helper methods for clarity",
                ]
            },
            {
                task: "Implement handle_bead_assigned helper"
                file: "crates/factory-core/src/actors/dispatcher.rs"
                what: """
                    impl FactoryDispatcher {
                        async fn handle_bead_assigned(
                            &self,
                            myself: ActorRef<DispatcherMsg>,
                            bead_id: BeadId,
                            priority: u8,
                            state: &mut DispatcherState,
                        ) -> Result<(), ActorProcessingErr> {
                            // Check if shutting down
                            if state.shutting_down {
                                tracing::warn!(bead_id = %bead_id, "Rejecting BeadAssigned: shutting down");
                                return Ok(());
                            }

                            // Check for duplicate
                            if state.active_loops.contains_key(&bead_id) {
                                tracing::warn!(bead_id = %bead_id, "Ignoring duplicate BeadAssigned");
                                return Ok(());
                            }

                            // Try to acquire permit from ResourceGovernor
                            let permit_result = state.resource_governor
                                .call(GovernorMsg::AcquireLoopSlot)
                                .await
                                .map_err(|e| ActorProcessingErr::from(e.to_string()))?;

                            match permit_result {
                                Ok(permit) => {
                                    self.spawn_loop(myself, bead_id, permit, state).await
                                }
                                Err(GovernorError::AtCapacity) => {
                                    tracing::info!(bead_id = %bead_id, "Queueing: at capacity");
                                    state.pending_queue.push_back(PendingBead { bead_id, priority });
                                    Ok(())
                                }
                                Err(e) => {
                                    Err(ActorProcessingErr::from(format!("Governor error: {e}")))
                                }
                            }
                        }

                        async fn spawn_loop(
                            &self,
                            myself: ActorRef<DispatcherMsg>,
                            bead_id: BeadId,
                            permit: LoopPermit,
                            state: &mut DispatcherState,
                        ) -> Result<(), ActorProcessingErr> {
                            let loop_args = FactoryLoopArgs {
                                bead_id: bead_id.clone(),
                                permit,
                                dispatcher: myself.clone(),
                            };

                            let loop_ref = FactoryLoop::spawn(loop_args)
                                .await
                                .map_err(|e| ActorProcessingErr::from(e.to_string()))?;

                            state.active_loops.insert(bead_id.clone(), loop_ref);

                            // Emit LoopSpawned signal
                            state.signal_bus
                                .cast(SignalBusMsg::Publish(Signal::LoopSpawned { bead_id }))
                                .map_err(|e| ActorProcessingErr::from(e.to_string()))?;

                            Ok(())
                        }
                    }
                    """
                done_when:     "Handler compiles and tests pass (green phase)"
                patterns_to_use: [
                    "Early return for guard conditions",
                    "? operator for error propagation",
                    "tracing for structured logging",
                ]
            },
            {
                task: "Implement handle_loop_completed helper"
                file: "crates/factory-core/src/actors/dispatcher.rs"
                what: """
                    async fn handle_loop_completed(
                        &self,
                        bead_id: BeadId,
                        state: &mut DispatcherState,
                    ) -> Result<(), ActorProcessingErr> {
                        // Remove from active loops
                        if state.active_loops.remove(&bead_id).is_none() {
                            tracing::warn!(bead_id = %bead_id, "LoopCompleted for unknown bead");
                            return Ok(());
                        }

                        // Emit LoopComplete signal
                        state.signal_bus
                            .cast(SignalBusMsg::Publish(Signal::LoopComplete { bead_id }))
                            .map_err(|e| ActorProcessingErr::from(e.to_string()))?;

                        // Process pending queue if any
                        self.try_process_pending(state).await
                    }

                    async fn try_process_pending(
                        &self,
                        state: &mut DispatcherState,
                    ) -> Result<(), ActorProcessingErr> {
                        if let Some(pending) = state.pending_queue.pop_front() {
                            // Recursively try to spawn (will queue again if still at capacity)
                            // This is safe because we just freed a slot
                            tracing::info!(bead_id = %pending.bead_id, "Processing pending bead");
                            // Note: actual implementation would call handle_bead_assigned
                        }
                        Ok(())
                    }
                    """
                done_when:     "Handler compiles and tests pass"
                patterns_to_use: ["Option::is_none() for missing key check"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Export dispatcher from actors module"
                file:      "crates/factory-core/src/actors/mod.rs"
                what:      "pub mod dispatcher; pub use dispatcher::FactoryDispatcher;"
                done_when: "External crates can import factory_core::actors::FactoryDispatcher"
            },
            {
                task:      "Add dispatcher errors to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "LoopSpawnFailed, DispatcherShuttingDown, DuplicateBead, QueueFull variants"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Add dispatcher to RootSupervisor child specs"
                file:      "crates/factory-core/src/actors/supervisor.rs"
                what:      "Include FactoryDispatcher in supervisor's children list"
                done_when: "Dispatcher started automatically when supervisor starts"
            },
            {
                task:      "Wire dispatcher signals to SignalBus"
                file:      "crates/factory-core/src/signals/mod.rs"
                what:      "Add LoopSpawned, LoopComplete, LoopFailed to Signal enum"
                done_when: "Signals can be published and subscribed"
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
                done_when: "rg finds no unwrap/expect in dispatcher module"
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/actors/dispatcher.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify invariants hold"
                done_when: "Property tests pass for active_loops sync invariant"
                commands: ["moon run factory-core:test -- dispatcher_invariant"]
                expected: "All property tests pass"
            },
            {
                task:      "Manual verification"
                done_when: "Dispatcher spawns loops in integration test"
                commands: [
                    "cargo run --example dispatcher_demo",
                ]
                expected: "Dispatcher receives events, spawns loops, emits signals"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'SignalBus' not found"
                likely_cause: "SignalBus actor not implemented yet (factory-kou dependency)"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/signal_bus.rs"
                        what_to_check: "Does file exist? Is SignalBus exported?"
                    },
                ]
                fix_pattern: "Implement factory-kou first, or use mock SignalBus for testing"
            },
            {
                symptom:      "Compilation error: 'ResourceGovernor' not found"
                likely_cause: "ResourceGovernor actor not implemented yet (factory-kjt dependency)"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/governor.rs"
                        what_to_check: "Does file exist? Is ResourceGovernor exported?"
                    },
                ]
                fix_pattern: "Implement factory-kjt first, or use mock ResourceGovernor for testing"
            },
            {
                symptom:      "Runtime error: 'actor not running' when sending message"
                likely_cause: "ActorRef is stale - actor was stopped or crashed"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/dispatcher.rs"
                        function:      "handle_bead_assigned()"
                        what_to_check: "Is signal_bus ActorRef still valid?"
                    },
                ]
                fix_pattern: "Check actor health before sending, handle SendError::Disconnected"
            },
            {
                symptom:      "Test hangs indefinitely"
                likely_cause: "Deadlock: call() waiting for response that never comes"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/dispatcher.rs"
                        function:      "handle() - GetStatus handler"
                        what_to_check: "Is respond_to.send() being called?"
                    },
                ]
                fix_pattern: "Always send response in GetStatus handler, use timeout wrapper"
            },
            {
                symptom:      "active_loops count doesn't match running loops"
                likely_cause: "Loop crashed without notifying dispatcher"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/dispatcher.rs"
                        function:      "post_stop() / supervision callback"
                        what_to_check: "Is ChildExited message being sent on crash?"
                    },
                ]
                fix_pattern: "Implement supervision callback to notify on child exit"
            },
            {
                symptom:      "Pending queue grows without bound"
                likely_cause: "Loops completing but pending not being processed"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/actors/dispatcher.rs"
                        function:      "handle_loop_completed()"
                        what_to_check: "Is try_process_pending() being called?"
                    },
                ]
                fix_pattern: "Call try_process_pending() after every LoopCompleted/LoopFailed"
            },
        ]

        debugging_commands: [
            {
                scenario: "When dispatcher doesn't process messages"
                run:      "RUST_LOG=factory_core::actors::dispatcher=debug cargo test"
                look_for: "Message received/processed logs, state transitions"
            },
            {
                scenario: "When loop spawn fails"
                run:      "RUST_LOG=ractor=debug,factory_core=debug cargo test -- --nocapture 2>&1 | head -100"
                look_for: "Spawn error details, governor permit status"
            },
            {
                scenario: "When signals aren't being emitted"
                run:      "RUST_LOG=factory_core::signals=trace cargo test"
                look_for: "Signal::LoopSpawned publish calls, subscriber notifications"
            },
            {
                scenario: "When shutdown hangs"
                run:      "RUST_LOG=factory_core::actors=debug timeout 10s cargo test -- shutdown"
                look_for: "Loop stop messages sent, loop acknowledgments received"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_spawn_loop_on_bead_assigned passes",
            "[ ] test_loop_complete_removes_from_active passes",
            "[ ] test_duplicate_bead_assigned_ignored passes",
            "[ ] test_graceful_shutdown_stops_all_loops passes",
            "[ ] test_pending_queue_on_capacity passes",
            "[ ] test_loop_crash_emits_failed_signal passes",
            "[ ] E2E test_full_dispatcher_lifecycle passes",
            "[ ] No mocks - all tests use real actors",
        ]

        code: [
            "[ ] FactoryDispatcher struct defined",
            "[ ] DispatcherMsg enum with all variants",
            "[ ] DispatcherState with active_loops HashMap",
            "[ ] Actor trait implementation complete",
            "[ ] handle_bead_assigned spawns loops",
            "[ ] handle_loop_completed removes from active",
            "[ ] handle_shutdown stops all loops",
            "[ ] GetStatus returns accurate state",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All public APIs return Result<T, Error>",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in dispatcher.rs",
            "[ ] Doc comments on FactoryDispatcher struct",
            "[ ] Doc comments on DispatcherMsg variants",
            "[ ] Example usage in doc comments",
            "[ ] EARS requirements documented inline",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/actors/mod.rs"
                relevance: "Module exports - must add dispatcher module"
            },
            {
                path:      "crates/factory-core/src/actors/supervisor.rs"
                relevance: "RootSupervisor - dispatcher runs under this (factory-4pz)"
            },
            {
                path:      "crates/factory-core/src/actors/signal_bus.rs"
                relevance: "SignalBus - dispatcher subscribes to events (factory-kou)"
            },
            {
                path:      "crates/factory-core/src/actors/governor.rs"
                relevance: "ResourceGovernor - dispatcher requests permits (factory-kjt)"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add dispatcher error variants"
            },
            {
                path:      "crates/factory-core/src/signals/mod.rs"
                relevance: "Signal types - LoopSpawned, LoopComplete, LoopFailed"
            },
            {
                path:      ".beads/specs/factory-4pz.cue"
                relevance: "RootSupervisor spec - dispatcher's parent supervisor"
            },
        ]

        external_references: [
            "https://github.com/slawlor/ractor - ractor actor framework",
            "https://docs.rs/ractor - ractor API docs",
            "https://www.erlang.org/doc/design_principles/des_princ.html - OTP design principles",
            "https://hexdocs.pm/elixir/GenServer.html - GenServer patterns (Elixir)",
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
                how_to_apply:     "Create BeadId newtype with validation: BeadId::new(s) -> Result<BeadId>"
            },
            {
                pattern:          "Actor Message Enum"
                example_location: "Factory actor pattern"
                how_to_apply:     "Define exhaustive enum, use respond_to for call patterns"
            },
            {
                pattern:          "Supervisor Child Tracking"
                example_location: "factory-4pz RootSupervisor"
                how_to_apply:     "HashMap<ChildId, ActorRef> for tracking, remove on exit"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Implement factory-4pz (RootSupervisor), factory-kou (SignalBus), factory-kjt (ResourceGovernor) first",
            "Use HashMap<BeadId, ActorRef<FactoryLoop>> for O(1) loop lookup",
            "Use VecDeque<PendingBead> for FIFO pending queue",
            "Emit signals via SignalBus.cast(Publish(signal)) - fire and forget",
            "Use tracing::warn! for duplicate/ignored events",
            "Implement GetStatus with oneshot channel for debugging",
            "Add #[must_use] to DispatcherStatus return",
            "Use tokio::time::timeout for all call() operations",
            "Test with real actors, not mocks",
            "Document state machine transitions in comments",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT spawn loops without checking active_loops first (duplicates)",
            "Do NOT spawn loops without acquiring permit from ResourceGovernor",
            "Do NOT process pending queue in a loop (use recursion or iteration with break)",
            "Do NOT hold locks across await points",
            "Do NOT ignore ChildExited messages (causes active_loops desync)",
        ]

        code_patterns: [
            {
                name:     "Dispatcher State Machine"
                use_when: "Tracking dispatcher lifecycle"
                example:  """
                    pub struct DispatcherState {
                        // Dependencies
                        signal_bus: ActorRef<SignalBus>,
                        resource_governor: ActorRef<ResourceGovernor>,

                        // Active loop tracking
                        active_loops: HashMap<BeadId, ActorRef<FactoryLoop>>,

                        // Backpressure queue
                        pending_queue: VecDeque<PendingBead>,

                        // Lifecycle
                        shutting_down: bool,
                    }

                    #[derive(Debug, Clone)]
                    pub struct PendingBead {
                        pub bead_id: BeadId,
                        pub priority: u8,
                    }
                    """
            },
            {
                name:     "Dispatcher Message Pattern"
                use_when: "Defining dispatcher message types"
                example:  """
                    #[derive(Debug, Clone)]
                    pub enum DispatcherMsg {
                        // From SignalBus
                        BeadAssigned { bead_id: BeadId, priority: u8 },
                        ShutdownRequested,

                        // From FactoryLoop
                        LoopCompleted { bead_id: BeadId },
                        LoopFailed { bead_id: BeadId, reason: String },

                        // From Supervisor
                        ChildExited { bead_id: BeadId, reason: ExitReason },

                        // Query
                        GetStatus { respond_to: oneshot::Sender<DispatcherStatus> },

                        // Control
                        CancelLoop { bead_id: BeadId },
                    }

                    #[derive(Debug, Clone)]
                    pub struct DispatcherStatus {
                        pub active_loop_count: usize,
                        pub active_beads: Vec<BeadId>,
                        pub pending_queue_len: usize,
                        pub shutting_down: bool,
                    }
                    """
            },
            {
                name:     "BeadId Newtype"
                use_when: "Creating validated bead identifier"
                example:  """
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct BeadId(String);

                    impl BeadId {
                        pub fn new(s: impl Into<String>) -> Result<Self, Error> {
                            let s = s.into();
                            // Validate bead ID format: alphanumeric + hyphen
                            if s.is_empty() {
                                return Err(Error::InvalidBeadId { reason: "empty".into() });
                            }
                            if !s.chars().all(|c| c.is_alphanumeric() || c == '-') {
                                return Err(Error::InvalidBeadId {
                                    reason: "invalid characters".into()
                                });
                            }
                            Ok(Self(s))
                        }

                        pub fn as_str(&self) -> &str {
                            &self.0
                        }
                    }

                    impl std::fmt::Display for BeadId {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            write!(f, "{}", self.0)
                        }
                    }
                    """
            },
            {
                name:     "Signal Subscription Pattern"
                use_when: "Subscribing to SignalBus events"
                example:  """
                    async fn pre_start(
                        &self,
                        myself: ActorRef<Self::Msg>,
                        args: Self::Arguments,
                    ) -> Result<Self::State, ActorProcessingErr> {
                        // Subscribe to relevant signals
                        args.signal_bus
                            .cast(SignalBusMsg::Subscribe {
                                subscriber: myself.clone().into(),
                                events: vec![
                                    SignalType::BeadAssigned,
                                    SignalType::ShutdownRequested,
                                ],
                            })
                            .map_err(|e| ActorProcessingErr::from(format!(
                                "Failed to subscribe to SignalBus: {e}"
                            )))?;

                        Ok(DispatcherState::new(args.signal_bus, args.resource_governor))
                    }
                    """
            },
            {
                name:     "Graceful Shutdown Pattern"
                use_when: "Stopping all child actors on shutdown"
                example:  """
                    async fn handle_shutdown(
                        &self,
                        state: &mut DispatcherState,
                    ) -> Result<(), ActorProcessingErr> {
                        state.shutting_down = true;

                        // Clear pending queue (don't process new work)
                        let pending_count = state.pending_queue.len();
                        state.pending_queue.clear();
                        tracing::info!(pending_count, "Cleared pending queue for shutdown");

                        // Stop all active loops
                        let active_count = state.active_loops.len();
                        for (bead_id, loop_ref) in state.active_loops.drain() {
                            tracing::info!(bead_id = %bead_id, "Stopping loop for shutdown");
                            // Use stop() not kill() for graceful shutdown
                            loop_ref.stop(Some("dispatcher shutdown".into()));
                        }

                        tracing::info!(active_count, "Stopped all active loops");
                        Ok(())
                    }
                    """
            },
        ]
    }
}
