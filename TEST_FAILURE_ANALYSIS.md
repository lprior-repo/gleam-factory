TEST FAILURE ANALYSIS - 41 Failing Tests

Generated: 2026-01-18

SUMMARY
41 test failures across 4 test files:
- 16 factory_supervisor_test failures (CaseClause errors)
- 3 graceful_shutdown_test failures (CaseClause errors)
- 7 performance_test failures (Badarith errors)
- 10 pipeline_integration_test failures (CaseClause errors)
- 1 heartbeat_test failure (TestPassing signal not received)
- 1 integration_test failure (phase assertion failure)

FAILURE CATEGORIES

1. CaseClause(Ok) ERRORS - 29 tests
   Files: factory_supervisor_test, graceful_shutdown_test, pipeline_integration_test
   Root cause: Case expression pattern matching failure

   Pattern in code:
   ```
   case factory_supervisor.start_link(config) {
     Ok(_) -> Nil
     Error(_) -> should.fail()
   }
   |> should.equal(Nil)
   ```

   The runtime error "CaseClause(Ok)" suggests the case statement is being evaluated
   but the Ok pattern is not being recognized properly. This typically happens when:
   - start_link returns something other than Result type
   - The type system/pattern matching is not working as expected
   - Gleam runtime has an issue with the case expression

   Affected tests:
   factory_supervisor_test:
   - supervisor_starts_test
   - signal_bus_accessible_test
   - heartbeat_accessible_test
   - multiple_supervisors_test
   - heartbeat_tick_received_test
   - signal_bus_publish_received_test
   - supervisor_children_responsive_test
   - supervisor_startup_error_test
   - accessor_functions_correct_test
   - supervisor_config_flexibility_test
   - start_link_all_actors_success_test
   - supervisor_restarts_failed_child_test
   - supervisor_returns_all_actor_subjects_test
   - graceful_degradation_on_child_failure_test
   - log_system_ready_outputs_message_test
   - system_ready_after_all_services_started_test

   graceful_shutdown_test:
   - graceful_shutdown_test
   - shutdown_idempotent_test
   - shutdown_timeout_test

   pipeline_integration_test:
   - pipeline_supervisor_startup_test
   - pipeline_signal_broadcast_test
   - pipeline_heartbeat_polling_test
   - pipeline_merge_queue_patch_absorption_test
   - pipeline_test_failure_signal_test
   - pipeline_golden_master_preparation_test
   - pipeline_multiple_subscriptions_test
   - pipeline_error_handling_resilience_test
   - pipeline_complete_workflow_test
   - pipeline_shutdown_test

2. Badarith ERRORS - 7 tests
   Files: performance_test
   Root cause: Arithmetic operation on invalid type

   Problem code (line 52-54):
   ```
   let throughput = case duration_ms {
     0 -> 0.0
     d -> 1000.0 /. int_to_float(d)
   }
   ```

   Issue: int_to_float() is declared as external but never properly converts.
   The external binding on line 37-38 is incorrect:
   ```
   @external(erlang, "erlang", "float")
   fn int_to_float(i: Int) -> Float
   ```

   This doesn't work correctly. Need proper float conversion.

   Affected tests:
   - benchmark_implement_stage_test
   - benchmark_unit_test_stage_test
   - benchmark_lint_stage_test
   - benchmark_static_stage_test
   - benchmark_integration_stage_test
   - benchmark_throughput_calculation_test
   - benchmark_memory_measurement_test

3. Signal Bus/Subscription ISSUE - 1 test
   File: heartbeat_test
   Test: transition_from_red_to_green_broadcasts_test_passing_test (line 70)

   Problem: Signal not being received within timeout
   Expected: TestPassing signal on Red→Green transition
   Actual: Timeout in process.receive(passing_sub, 1000)

   Root cause: The heartbeat/signal_bus integration doesn't properly broadcast
   TestPassing signal when transitioning from Red to Green status.

4. Phase Assertion FAILURE - 1 test
   File: integration_test
   Test: factory_loop_phase_transitions_test

   Problem: Expected phase "Pushing" but got "Reviewing"
   This indicates the approval gate logic in factory_loop isn't transitioning
   states correctly (relates to recent fix in commit c54ab1e)

DEPENDENCY ANALYSIS

Fix Order (dependencies first):

1. FIRST: Investigate CaseClause(Ok) errors
   - These are blocking 29 tests across 3 files
   - Pattern matching issue suggests type system problem
   - Check factory_supervisor.start_link return type
   - Verify Result type is properly defined

2. SECOND: Fix Badarith in performance_test
   - 7 tests failing due to float conversion
   - Replace @external(erlang, "erlang", "float") with proper conversion
   - Use: int.to_float(d) if Gleam stdlib supports it

3. THIRD: Debug heartbeat_test signal broadcasting
   - Signal bus not properly routing TestPassing signal
   - Check signal_bus.broadcast vs signal_bus.publish difference
   - Verify subscription filter matching in line 73-74

4. FOURTH: Review integration_test phase transitions
   - Appears related to approval gate implementation
   - Verify factory_loop state machine logic

COMMON PATTERNS

Pattern 1: All factory_supervisor_test failures share identical structure
All tests use:
```
case factory_supervisor.start_link(config) {
  Ok(_) -> [do something]
  Error(_) -> should.fail()
}
|> should.equal(Nil)
```

This uniform failure suggests not a logic bug but a type/runtime issue
with how start_link is being called or how its return value is being matched.

Pattern 2: Golden master path errors in logs
Logs show: "golden_master prepare failed: repo check failed:"
and "Golden master path does not exist: /tmp/1"

This suggests golden_master initialization is working but repo validation
is failing. These errors are logged but shouldn't cause CaseClause panics.

Pattern 3: Performance tests all fail on arithmetic
All benchmark tests fail at throughput calculation line (division operation)
Consistent issue: float conversion function not working

RECOMMENDATIONS

1. Immediate: Add debug logging to factory_supervisor.start_link
   - Print actual return type before test receives it
   - Verify Ok wrapping is correct

2. Immediate: Fix int_to_float in performance_test
   - Use Gleam's standard library float conversion
   - Test with simple: let x = int.to_float(5) /. 2.0

3. Short-term: Add more detailed error messages
   - CaseClause errors are cryptic
   - Pattern match errors should show what was received

4. Review: Signal bus subscription semantics
   - Verify broadcast vs publish behavior
   - Check filter matching on signal type

NEXT STEPS FOR IMPLEMENTATION

1. Run single test with debug output:
   gleam test -- --module factory_supervisor_test

2. Check actual return type of start_link in REPL if possible

3. Simplify performance_test float conversion to test baseline

4. Verify signal_bus routes signals to all subscribers correctly
