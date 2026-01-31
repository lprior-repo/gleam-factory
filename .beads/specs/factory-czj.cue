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

"factory-czj": #ValidBead & {
    // ============================================================================
    // BEAD: factory-czj - Implement token budget tracking for LLM cost control
    // ============================================================================

    id:              "factory-czj"
    title:           "Budget: Implement token budget tracking for LLM cost control"
    type:            "feature"
    priority:        1
    effort_estimate: "2hr"
    labels:          ["budget", "cost-control", "llm", "factory-loop", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL track token usage via TokenBudget type with max_tokens and used_tokens fields",
            "THE SYSTEM SHALL track iteration count via max_iterations and current_iteration fields",
            "THE SYSTEM SHALL provide has_budget() check that returns false when either limit is exceeded",
            "THE SYSTEM SHALL send RecordTokens message to FactoryLoop to record token consumption",
            "THE SYSTEM SHALL trigger FailedPipeline with BudgetExhausted when budget is exceeded",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN RecordTokens message is received by FactoryLoop"
                shall:   "THE SYSTEM SHALL atomically increment used_tokens by the recorded amount"
            },
            {
                trigger: "WHEN used_tokens exceeds max_tokens"
                shall:   "THE SYSTEM SHALL emit BudgetExhausted event to FactoryLoop"
            },
            {
                trigger: "WHEN current_iteration exceeds max_iterations"
                shall:   "THE SYSTEM SHALL emit BudgetExhausted event to FactoryLoop"
            },
            {
                trigger: "WHEN BudgetExhausted event is received"
                shall:   "THE SYSTEM SHALL transition to FailedPipeline state with reason 'budget_exhausted'"
            },
            {
                trigger: "WHEN a new iteration starts"
                shall:   "THE SYSTEM SHALL increment current_iteration and check has_budget()"
            },
            {
                trigger: "WHEN LLM response is received"
                shall:   "THE SYSTEM SHALL extract token count and send RecordTokens message"
            },
        ]

        state_driven: [
            {
                state: "WHILE the factory loop is in Implementing phase"
                shall: "THE SYSTEM SHALL check has_budget() before each LLM call"
            },
            {
                state: "WHILE the factory loop is in Reviewing phase"
                shall: "THE SYSTEM SHALL check has_budget() before each review iteration"
            },
            {
                state: "WHILE budget tracking is active"
                shall: "THE SYSTEM SHALL maintain accurate running totals of tokens and iterations"
            },
        ]

        unwanted: [
            {
                condition: "IF token recording fails"
                shall_not: "THE SYSTEM SHALL NOT lose track of consumed tokens"
                because:   "Lost token tracking leads to budget overruns and unexpected costs"
            },
            {
                condition: "IF budget is exhausted"
                shall_not: "THE SYSTEM SHALL NOT make additional LLM calls"
                because:   "Exceeding budget violates cost control constraints"
            },
            {
                condition: "IF iteration limit is reached"
                shall_not: "THE SYSTEM SHALL NOT start another iteration"
                because:   "Infinite loops waste resources and never converge"
            },
            {
                condition: "IF budget state is corrupted"
                shall_not: "THE SYSTEM SHALL NOT continue with incorrect token counts"
                because:   "Corrupted budget state leads to either premature termination or overruns"
            },
        ]

        complex: [
            {
                state:   "WHILE the system is processing an LLM response"
                trigger: "WHEN budget becomes exhausted mid-response"
                shall:   "THE SYSTEM SHALL complete processing current response then emit BudgetExhausted"
            },
            {
                state:   "WHILE in FailedPipeline due to BudgetExhausted"
                trigger: "WHEN retry is requested"
                shall:   "THE SYSTEM SHALL reject retry with Error::BudgetExhausted"
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
                    field:           "max_tokens"
                    type:            "u64"
                    constraints:     "Positive integer, typically 100_000 to 1_000_000"
                    example_valid:   "500_000"
                    example_invalid: "0 // No budget is invalid"
                },
                {
                    field:           "max_iterations"
                    type:            "u32"
                    constraints:     "Positive integer, typically 3 to 10"
                    example_valid:   "5"
                    example_invalid: "0 // No iterations is invalid"
                },
                {
                    field:           "token_count"
                    type:            "u64"
                    constraints:     "Non-negative integer from LLM response"
                    example_valid:   "1523"
                    example_invalid: "// Cannot be negative"
                },
            ]
            system_state: [
                "FactoryLoop actor is running (factory-c2s completed)",
                "LLM client is configured and ready",
                "Budget has not already been exhausted",
            ]
        }

        postconditions: {
            state_changes: [
                "used_tokens incremented by token_count on RecordTokens",
                "current_iteration incremented on new iteration start",
                "FactoryLoop transitions to FailedPipeline on budget exhaustion",
            ]
            return_guarantees: [
                {
                    field:     "TokenBudget::new()"
                    guarantee: "Returns Result<TokenBudget, Error> with validated inputs"
                },
                {
                    field:     "TokenBudget::has_budget()"
                    guarantee: "Returns bool indicating if both token and iteration limits are within bounds"
                },
                {
                    field:     "TokenBudget::record_tokens()"
                    guarantee: "Returns Result<BudgetStatus, Error> with updated status"
                },
                {
                    field:     "TokenBudget::increment_iteration()"
                    guarantee: "Returns Result<BudgetStatus, Error> with updated status"
                },
                {
                    field:     "TokenBudget::remaining_tokens()"
                    guarantee: "Returns u64 of tokens remaining (saturating subtraction)"
                },
                {
                    field:     "TokenBudget::remaining_iterations()"
                    guarantee: "Returns u32 of iterations remaining (saturating subtraction)"
                },
            ]
            side_effects: [
                "BudgetExhausted event sent to FactoryLoop signal bus when limits exceeded",
                "Budget metrics logged for cost tracking",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "used_tokens is monotonically increasing (never decreases)",
            "current_iteration is monotonically increasing (never decreases)",
            "has_budget() returns false when either limit is exceeded",
            "BudgetStatus accurately reflects current budget state",
            "Token recording is atomic - no partial updates",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Token count not available from LLM response"
                prevention:  "Define TokenUsage trait that all LLM clients must implement"
                test_for_it: "test_token_extraction_from_llm_response"
            },
            {
                failure:     "Race between RecordTokens and has_budget() check"
                prevention:  "Use actor mailbox serialization - all budget ops go through FactoryLoop"
                test_for_it: "test_concurrent_token_recording"
            },
            {
                failure:     "FactoryLoop not handling RecordTokens message"
                prevention:  "Add RecordTokens variant to FactoryLoopMessage enum"
                test_for_it: "test_factory_loop_handles_record_tokens"
            },
        ]

        usability_failures: [
            {
                failure:     "No visibility into remaining budget"
                prevention:  "Provide remaining_tokens() and remaining_iterations() methods"
                test_for_it: "test_budget_visibility_methods"
            },
            {
                failure:     "Budget exhaustion reason unclear"
                prevention:  "BudgetExhausted includes which limit was exceeded and by how much"
                test_for_it: "test_budget_exhaustion_reason_clarity"
            },
            {
                failure:     "Cannot adjust budget mid-run"
                prevention:  "Provide increase_budget() for emergency extensions (with audit)"
                test_for_it: "test_budget_increase"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Token count overflows u64"
                prevention:  "Use saturating_add() for all token arithmetic"
                test_for_it: "test_token_overflow_handling"
            },
            {
                failure:     "Iteration count overflows u32"
                prevention:  "Use saturating_add() for iteration arithmetic"
                test_for_it: "test_iteration_overflow_handling"
            },
            {
                failure:     "Budget state not persisted on crash"
                prevention:  "Log budget checkpoints to audit log for recovery"
                test_for_it: "test_budget_persistence"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_token_budget_creation"
                given: "Valid max_tokens and max_iterations values"
                when:  "TokenBudget::new() is called"
                then: [
                    "Returns Ok(TokenBudget)",
                    "used_tokens is 0",
                    "current_iteration is 0",
                    "has_budget() returns true",
                ]
                real_input: """
                    let budget = TokenBudget::new(500_000, 5)?;
                    """
                expected_output: """
                    assert_eq!(budget.max_tokens(), 500_000);
                    assert_eq!(budget.max_iterations(), 5);
                    assert_eq!(budget.used_tokens(), 0);
                    assert_eq!(budget.current_iteration(), 0);
                    assert!(budget.has_budget());
                    """
            },
            {
                name:  "test_record_tokens_updates_budget"
                given: "A TokenBudget with remaining capacity"
                when:  "record_tokens() is called with a token count"
                then: [
                    "Returns Ok(BudgetStatus::WithinBudget)",
                    "used_tokens is incremented by token_count",
                    "has_budget() still returns true if within limits",
                ]
                real_input: """
                    let mut budget = TokenBudget::new(500_000, 5)?;
                    let status = budget.record_tokens(1500)?;
                    """
                expected_output: """
                    assert!(matches!(status, BudgetStatus::WithinBudget { .. }));
                    assert_eq!(budget.used_tokens(), 1500);
                    assert!(budget.has_budget());
                    """
            },
            {
                name:  "test_increment_iteration_updates_budget"
                given: "A TokenBudget with remaining iterations"
                when:  "increment_iteration() is called"
                then: [
                    "Returns Ok(BudgetStatus::WithinBudget)",
                    "current_iteration is incremented by 1",
                    "has_budget() still returns true if within limits",
                ]
                real_input: """
                    let mut budget = TokenBudget::new(500_000, 5)?;
                    let status = budget.increment_iteration()?;
                    """
                expected_output: """
                    assert!(matches!(status, BudgetStatus::WithinBudget { .. }));
                    assert_eq!(budget.current_iteration(), 1);
                    assert!(budget.has_budget());
                    """
            },
            {
                name:  "test_has_budget_checks_both_limits"
                given: "A TokenBudget with both token and iteration budgets"
                when:  "has_budget() is called"
                then: [
                    "Returns true only if BOTH limits are not exceeded",
                ]
                real_input: """
                    let mut budget = TokenBudget::new(1000, 2)?;

                    // Within both limits
                    assert!(budget.has_budget());

                    // Exceed tokens but not iterations
                    budget.record_tokens(1001)?;
                    assert!(!budget.has_budget());

                    // Reset and exceed iterations but not tokens
                    let mut budget2 = TokenBudget::new(1000, 2)?;
                    budget2.increment_iteration()?;
                    budget2.increment_iteration()?;
                    budget2.increment_iteration()?;  // iteration 3 > max 2
                    """
                expected_output: """
                    assert!(!budget2.has_budget());
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_budget_exhausted_on_token_limit"
                given: "A TokenBudget at or near token limit"
                when:  "record_tokens() exceeds max_tokens"
                then: [
                    "Returns Ok(BudgetStatus::Exhausted { reason: TokensExceeded })",
                    "has_budget() returns false",
                    "Subsequent has_budget() calls return false",
                ]
                real_input: """
                    let mut budget = TokenBudget::new(1000, 5)?;
                    budget.record_tokens(900)?;
                    let status = budget.record_tokens(200)?;  // 1100 > 1000
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(status, BudgetStatus::Exhausted {
                        reason: ExhaustionReason::TokensExceeded { used: 1100, max: 1000 }
                    }));
                    assert!(!budget.has_budget());
                    """
            },
            {
                name:  "test_budget_exhausted_on_iteration_limit"
                given: "A TokenBudget at iteration limit"
                when:  "increment_iteration() exceeds max_iterations"
                then: [
                    "Returns Ok(BudgetStatus::Exhausted { reason: IterationsExceeded })",
                    "has_budget() returns false",
                ]
                real_input: """
                    let mut budget = TokenBudget::new(500_000, 2)?;
                    budget.increment_iteration()?;  // iteration 1
                    budget.increment_iteration()?;  // iteration 2
                    let status = budget.increment_iteration()?;  // iteration 3 > max 2
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(status, BudgetStatus::Exhausted {
                        reason: ExhaustionReason::IterationsExceeded { current: 3, max: 2 }
                    }));
                    assert!(!budget.has_budget());
                    """
            },
            {
                name:  "test_invalid_budget_creation"
                given: "Invalid budget parameters (zero values)"
                when:  "TokenBudget::new() is called"
                then: [
                    "Returns Err(Error::InvalidBudget)",
                    "Does not create a TokenBudget",
                ]
                real_input: """
                    let result = TokenBudget::new(0, 5);
                    """
                expected_output: null
                expected_error: """
                    assert!(matches!(result, Err(Error::InvalidBudget { reason: _ })));
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_budget_exactly_at_limit"
                scenario: "Token usage exactly equals max_tokens"
                input:    "TokenBudget with max_tokens=1000, record exactly 1000 tokens"
                expected: "has_budget() returns true (equal is within budget)"
            },
            {
                name:     "test_zero_token_recording"
                scenario: "LLM returns zero tokens (cached response)"
                input:    "record_tokens(0)"
                expected: "Budget unchanged, returns Ok(BudgetStatus::WithinBudget)"
            },
            {
                name:     "test_large_token_count"
                scenario: "Single LLM call uses most of budget"
                input:    "max_tokens=100_000, record_tokens(99_000)"
                expected: "Remaining shows 1000, has_budget() true, next call may exhaust"
            },
            {
                name:     "test_u64_overflow_protection"
                scenario: "Attempt to overflow u64 with massive token count"
                input:    "record_tokens(u64::MAX)"
                expected: "Saturates at u64::MAX, returns Exhausted"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in budget code"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/budget.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public APIs return Result"
                test:     "All public functions in budget.rs return Result<T, Error>"
            },
            {
                name:     "test_invariant_monotonic_used_tokens"
                verifies: "used_tokens never decreases"
                test:     "No public method decreases used_tokens"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_budget_integration_with_factory_loop"
            description: "Complete budget lifecycle: create -> record -> exhaust -> fail pipeline"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/budget_e2e.rs"
                        content: """
                            use factory_core::budget::{TokenBudget, BudgetStatus, ExhaustionReason};
                            use factory_core::factory_loop::{FactoryLoop, FactoryLoopMessage, FactoryPhase};
                            use factory_core::error::Result;
                            use std::time::Duration;

                            #[tokio::test]
                            async fn test_budget_exhaustion_fails_pipeline() -> Result<()> {
                                // Create factory loop with small budget
                                let budget = TokenBudget::new(1000, 2)?;
                                let loop_ref = FactoryLoop::spawn(budget).await?;

                                // Simulate LLM calls that exhaust budget
                                loop_ref.cast(FactoryLoopMessage::RecordTokens(500)).await?;
                                loop_ref.cast(FactoryLoopMessage::RecordTokens(600)).await?;  // Exceeds

                                // Verify pipeline failed due to budget
                                tokio::time::sleep(Duration::from_millis(100)).await;
                                let state = loop_ref.call(FactoryLoopMessage::GetState).await?;

                                assert!(matches!(state.phase, FactoryPhase::FailedPipeline { .. }));
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
                command:    "moon run factory-core:test -- --test budget_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_budget_exhaustion_fails_pipeline ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/budget.rs"
                        contains: "pub struct TokenBudget"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/budget_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_budget_tracking_across_iterations"
                description: "Verify budget correctly accumulates across multiple LLM iterations"
                steps: [
                    {action: "Create TokenBudget(100_000, 5)", verify: "Budget created, has_budget()=true"},
                    {action: "Start iteration 1, LLM uses 15000 tokens", verify: "used=15000, iteration=1"},
                    {action: "Start iteration 2, LLM uses 20000 tokens", verify: "used=35000, iteration=2"},
                    {action: "Start iteration 3, LLM uses 25000 tokens", verify: "used=60000, iteration=3"},
                    {action: "Query remaining budget", verify: "40000 tokens, 2 iterations remaining"},
                ]
            },
            {
                name:        "e2e_budget_exhaustion_triggers_failure"
                description: "Verify pipeline correctly fails when budget exhausted"
                steps: [
                    {action: "Create factory loop with TokenBudget(10_000, 3)", verify: "Loop running"},
                    {action: "Run iterations until token budget exceeded", verify: "BudgetExhausted emitted"},
                    {action: "Check factory loop state", verify: "Phase is FailedPipeline"},
                    {action: "Verify failure reason", verify: "reason contains 'budget_exhausted'"},
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
                task:      "Write test: test_token_budget_creation"
                file:      "crates/factory-core/src/budget.rs"
                what:      "Test that TokenBudget::new returns valid budget with correct initial state"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_record_tokens_updates_budget"
                file:      "crates/factory-core/src/budget.rs"
                what:      "Test that record_tokens increments used_tokens correctly"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_budget_exhausted_on_token_limit"
                file:      "crates/factory-core/src/budget.rs"
                what:      "Test that exceeding token limit returns Exhausted status"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_budget_exhausted_on_iteration_limit"
                file:      "crates/factory-core/src/budget.rs"
                what:      "Test that exceeding iteration limit returns Exhausted status"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_has_budget_checks_both_limits"
                file:      "crates/factory-core/src/budget.rs"
                what:      "Test that has_budget returns false when either limit exceeded"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Create ExhaustionReason enum"
                file: "crates/factory-core/src/budget.rs"
                what: """
                    /// Reason why budget was exhausted
                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum ExhaustionReason {
                        /// Token limit exceeded
                        TokensExceeded { used: u64, max: u64 },
                        /// Iteration limit exceeded
                        IterationsExceeded { current: u32, max: u32 },
                    }

                    impl std::fmt::Display for ExhaustionReason {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            match self {
                                Self::TokensExceeded { used, max } => {
                                    write!(f, "token limit exceeded: {} used of {} max", used, max)
                                }
                                Self::IterationsExceeded { current, max } => {
                                    write!(f, "iteration limit exceeded: {} of {} max", current, max)
                                }
                            }
                        }
                    }
                    """
                done_when:     "Enum compiles with Display impl"
                patterns_to_use: ["Exhaustive match in Display"]
            },
            {
                task: "Create BudgetStatus enum"
                file: "crates/factory-core/src/budget.rs"
                what: """
                    /// Status of budget after an operation
                    #[derive(Debug, Clone, PartialEq, Eq)]
                    pub enum BudgetStatus {
                        /// Budget is within limits
                        WithinBudget {
                            remaining_tokens: u64,
                            remaining_iterations: u32,
                        },
                        /// Budget has been exhausted
                        Exhausted {
                            reason: ExhaustionReason,
                        },
                    }

                    impl BudgetStatus {
                        /// Check if budget is exhausted
                        #[must_use]
                        pub const fn is_exhausted(&self) -> bool {
                            matches!(self, Self::Exhausted { .. })
                        }
                    }
                    """
                done_when:     "Enum compiles with is_exhausted method"
                patterns_to_use: ["const fn where possible", "#[must_use] on pure functions"]
            },
            {
                task: "Create TokenBudget struct"
                file: "crates/factory-core/src/budget.rs"
                what: """
                    use crate::error::{Error, Result};

                    /// Token budget for LLM cost control
                    ///
                    /// Tracks both token usage and iteration count to prevent
                    /// runaway costs and infinite loops in the feedback loop.
                    #[derive(Debug, Clone)]
                    pub struct TokenBudget {
                        max_tokens: u64,
                        used_tokens: u64,
                        max_iterations: u32,
                        current_iteration: u32,
                    }

                    impl TokenBudget {
                        /// Create a new token budget with validated limits
                        ///
                        /// # Errors
                        /// Returns `Error::InvalidBudget` if max_tokens or max_iterations is zero.
                        pub fn new(max_tokens: u64, max_iterations: u32) -> Result<Self> {
                            if max_tokens == 0 {
                                return Err(Error::InvalidBudget {
                                    reason: "max_tokens must be greater than zero".into(),
                                });
                            }
                            if max_iterations == 0 {
                                return Err(Error::InvalidBudget {
                                    reason: "max_iterations must be greater than zero".into(),
                                });
                            }

                            Ok(Self {
                                max_tokens,
                                used_tokens: 0,
                                max_iterations,
                                current_iteration: 0,
                            })
                        }

                        /// Get maximum allowed tokens
                        #[must_use]
                        pub const fn max_tokens(&self) -> u64 {
                            self.max_tokens
                        }

                        /// Get current used tokens
                        #[must_use]
                        pub const fn used_tokens(&self) -> u64 {
                            self.used_tokens
                        }

                        /// Get maximum allowed iterations
                        #[must_use]
                        pub const fn max_iterations(&self) -> u32 {
                            self.max_iterations
                        }

                        /// Get current iteration count
                        #[must_use]
                        pub const fn current_iteration(&self) -> u32 {
                            self.current_iteration
                        }
                    }
                    """
                done_when:     "Struct compiles with constructor and accessors"
                patterns_to_use: [
                    "const fn for simple getters",
                    "#[must_use] on pure functions",
                    "Validation in constructor",
                ]
            },
            {
                task: "Implement has_budget and remaining methods"
                file: "crates/factory-core/src/budget.rs"
                what: """
                    impl TokenBudget {
                        /// Check if budget has remaining capacity for both tokens and iterations
                        ///
                        /// Returns `true` if:
                        /// - `used_tokens <= max_tokens`
                        /// - `current_iteration <= max_iterations`
                        #[must_use]
                        pub const fn has_budget(&self) -> bool {
                            self.used_tokens <= self.max_tokens
                                && self.current_iteration <= self.max_iterations
                        }

                        /// Get remaining tokens (saturating subtraction)
                        #[must_use]
                        pub const fn remaining_tokens(&self) -> u64 {
                            self.max_tokens.saturating_sub(self.used_tokens)
                        }

                        /// Get remaining iterations (saturating subtraction)
                        #[must_use]
                        pub const fn remaining_iterations(&self) -> u32 {
                            self.max_iterations.saturating_sub(self.current_iteration)
                        }

                        /// Get current budget status
                        #[must_use]
                        pub fn status(&self) -> BudgetStatus {
                            if self.used_tokens > self.max_tokens {
                                BudgetStatus::Exhausted {
                                    reason: ExhaustionReason::TokensExceeded {
                                        used: self.used_tokens,
                                        max: self.max_tokens,
                                    },
                                }
                            } else if self.current_iteration > self.max_iterations {
                                BudgetStatus::Exhausted {
                                    reason: ExhaustionReason::IterationsExceeded {
                                        current: self.current_iteration,
                                        max: self.max_iterations,
                                    },
                                }
                            } else {
                                BudgetStatus::WithinBudget {
                                    remaining_tokens: self.remaining_tokens(),
                                    remaining_iterations: self.remaining_iterations(),
                                }
                            }
                        }
                    }
                    """
                done_when:     "All methods compile, tests pass"
                patterns_to_use: [
                    "saturating_sub for safe arithmetic",
                    "const fn where possible",
                    "Return enum status not bool for exhaustion",
                ]
            },
            {
                task: "Implement record_tokens method"
                file: "crates/factory-core/src/budget.rs"
                what: """
                    impl TokenBudget {
                        /// Record token usage and return updated budget status
                        ///
                        /// Uses saturating addition to prevent overflow.
                        pub fn record_tokens(&mut self, tokens: u64) -> Result<BudgetStatus> {
                            self.used_tokens = self.used_tokens.saturating_add(tokens);

                            tracing::debug!(
                                tokens_recorded = tokens,
                                used_tokens = self.used_tokens,
                                max_tokens = self.max_tokens,
                                remaining = self.remaining_tokens(),
                                "Token usage recorded"
                            );

                            Ok(self.status())
                        }
                    }
                    """
                done_when:     "Test passes (green phase)"
                patterns_to_use: [
                    "saturating_add for overflow protection",
                    "tracing for observability",
                    "Return status for caller to check exhaustion",
                ]
            },
            {
                task: "Implement increment_iteration method"
                file: "crates/factory-core/src/budget.rs"
                what: """
                    impl TokenBudget {
                        /// Increment iteration count and return updated budget status
                        ///
                        /// Uses saturating addition to prevent overflow.
                        pub fn increment_iteration(&mut self) -> Result<BudgetStatus> {
                            self.current_iteration = self.current_iteration.saturating_add(1);

                            tracing::debug!(
                                current_iteration = self.current_iteration,
                                max_iterations = self.max_iterations,
                                remaining = self.remaining_iterations(),
                                "Iteration count incremented"
                            );

                            Ok(self.status())
                        }
                    }
                    """
                done_when:     "Test passes (green phase)"
                patterns_to_use: [
                    "saturating_add for overflow protection",
                    "tracing for observability",
                ]
            },
        ]

        phase_3_integration: [
            {
                task:      "Add InvalidBudget error variant to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "#[error(\"invalid budget: {reason}\")] InvalidBudget { reason: String }"
                done_when: "Error variant compiles and is documented"
            },
            {
                task:      "Add BudgetExhausted error variant to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "#[error(\"budget exhausted: {reason}\")] BudgetExhausted { reason: String }"
                done_when: "Error variant compiles and is documented"
            },
            {
                task:      "Export budget module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod budget;"
                done_when: "External crates can import factory_core::budget"
            },
            {
                task:      "Add RecordTokens message to FactoryLoopMessage"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "RecordTokens(u64) variant in FactoryLoopMessage enum"
                done_when: "FactoryLoop handles RecordTokens and updates budget"
            },
            {
                task:      "Integrate budget check in factory loop state machine"
                file:      "crates/factory-core/src/factory_loop.rs"
                what:      "Check has_budget() before LLM calls, emit BudgetExhausted on exhaustion"
                done_when: "Factory loop transitions to FailedPipeline on budget exhaustion"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/budget.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Verify budget integration"
                done_when: "Budget exhaustion correctly fails pipeline"
                commands: [
                    "cargo test --package factory-core budget",
                ]
                expected: "All budget tests pass"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'InvalidBudget' not found"
                likely_cause: "Error variant not added to error.rs"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/error.rs"
                        what_to_check: "Is InvalidBudget variant defined in Error enum?"
                    },
                ]
                fix_pattern: "Add #[error(\"invalid budget: {reason}\")] InvalidBudget { reason: String }"
            },
            {
                symptom:      "Budget not being checked before LLM calls"
                likely_cause: "Missing has_budget() check in factory loop"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/factory_loop.rs"
                        function:      "handle_message() or equivalent"
                        what_to_check: "Is budget.has_budget() checked before LLM call?"
                    },
                ]
                fix_pattern: "Add if !self.budget.has_budget() { return BudgetExhausted } before LLM"
            },
            {
                symptom:      "Token count always zero"
                likely_cause: "LLM client not extracting usage from response"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/llm_client.rs"
                        function:      "process_response()"
                        what_to_check: "Is token_usage being extracted from LLM response?"
                    },
                ]
                fix_pattern: "Extract usage from response.usage.total_tokens or equivalent"
            },
            {
                symptom:      "RecordTokens message not processed"
                likely_cause: "Missing match arm in FactoryLoopMessage handler"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/factory_loop.rs"
                        function:      "handle()"
                        what_to_check: "Is RecordTokens arm present in message match?"
                    },
                ]
                fix_pattern: "Add FactoryLoopMessage::RecordTokens(n) => self.state.budget.record_tokens(n)"
            },
        ]

        debugging_commands: [
            {
                scenario: "When budget isn't being tracked"
                run:      "RUST_LOG=factory_core::budget=debug cargo test"
                look_for: "Token usage recorded and Iteration count incremented log messages"
            },
            {
                scenario: "When budget exhaustion doesn't fail pipeline"
                run:      "RUST_LOG=factory_core=debug cargo test budget_exhaustion"
                look_for: "BudgetExhausted event, phase transition to FailedPipeline"
            },
            {
                scenario: "When token counts seem wrong"
                run:      "cargo test -- --nocapture test_record_tokens"
                look_for: "used_tokens values in assertions"
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
            "[ ] E2E pipeline test passing with budget exhaustion",
            "[ ] No mocks or fake data in any test",
            "[ ] test_token_budget_creation passes",
            "[ ] test_record_tokens_updates_budget passes",
            "[ ] test_budget_exhausted_on_token_limit passes",
            "[ ] test_budget_exhausted_on_iteration_limit passes",
            "[ ] test_has_budget_checks_both_limits passes",
        ]

        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All preconditions validated",
            "[ ] All postconditions guaranteed",
            "[ ] ExhaustionReason enum created",
            "[ ] BudgetStatus enum created",
            "[ ] TokenBudget struct created with all methods",
            "[ ] has_budget() checks both limits",
            "[ ] Saturating arithmetic for overflow protection",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in budget.rs",
            "[ ] All public types documented",
            "[ ] Example usage in doc comments",
            "[ ] BudgetStatus variants documented",
        ]

        integration: [
            "[ ] InvalidBudget error variant added to error.rs",
            "[ ] BudgetExhausted error variant added to error.rs",
            "[ ] budget module exported from lib.rs",
            "[ ] RecordTokens message added to FactoryLoopMessage",
            "[ ] Budget exhaustion triggers FailedPipeline transition",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add budget"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add budget error variants"
            },
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Domain types pattern - reference for newtype validation"
            },
            {
                path:      "crates/factory-core/src/factory_loop.rs"
                relevance: "Factory loop - integrates budget tracking (dependency)"
            },
        ]

        dependencies: [
            {
                bead_id: "factory-c2s"
                title:   "Implement factory loop phase state machine"
                how_it_relates: "TokenBudget integrates with FactoryLoop state machine"
                what_to_know: "FactoryLoop manages phases (Implementing, Reviewing, Pushing, Completed)"
            },
        ]

        external_references: [
            "https://platform.openai.com/docs/api-reference/chat - OpenAI API token usage",
            "https://docs.anthropic.com/claude/reference/messages - Anthropic API usage tracking",
            "Railway-Oriented Programming - Scott Wlaschin's approach to error handling",
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
                how_to_apply:     "Validate in constructor, make invalid states unrepresentable"
            },
            {
                pattern:          "Saturating Arithmetic"
                example_location: "std::u64::saturating_add"
                how_to_apply:     "Use saturating_add/saturating_sub to prevent overflow"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use saturating_add() and saturating_sub() for all token/iteration arithmetic",
            "Return BudgetStatus enum, not bool, to provide exhaustion details",
            "Use tracing for structured logging of budget operations",
            "Make TokenBudget Clone and Debug for debugging",
            "Use const fn for simple getters that can be computed at compile time",
            "Add #[must_use] to pure functions that return values",
            "Document the exact conditions when has_budget() returns false",
            "Include used vs max values in exhaustion errors for debugging",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT allow budget to be decreased (tokens/iterations only go up)",
            "Do NOT ignore budget exhaustion - always propagate to FactoryLoop",
            "Do NOT use checked_add then unwrap - use saturating_add instead",
        ]

        code_patterns: [
            {
                name:     "Budget Creation with Validation"
                use_when: "Creating TokenBudget with user-provided limits"
                example:  """
                    pub fn new(max_tokens: u64, max_iterations: u32) -> Result<Self> {
                        if max_tokens == 0 {
                            return Err(Error::InvalidBudget {
                                reason: "max_tokens must be greater than zero".into(),
                            });
                        }
                        if max_iterations == 0 {
                            return Err(Error::InvalidBudget {
                                reason: "max_iterations must be greater than zero".into(),
                            });
                        }
                        Ok(Self {
                            max_tokens,
                            used_tokens: 0,
                            max_iterations,
                            current_iteration: 0,
                        })
                    }
                    """
            },
            {
                name:     "Safe Token Recording"
                use_when: "Recording tokens from LLM response"
                example:  """
                    pub fn record_tokens(&mut self, tokens: u64) -> Result<BudgetStatus> {
                        // Saturating add prevents overflow
                        self.used_tokens = self.used_tokens.saturating_add(tokens);

                        tracing::debug!(
                            tokens_recorded = tokens,
                            used_tokens = self.used_tokens,
                            remaining = self.remaining_tokens(),
                            "Token usage recorded"
                        );

                        Ok(self.status())
                    }
                    """
            },
            {
                name:     "Budget Check Before LLM Call"
                use_when: "In factory loop before making LLM request"
                example:  """
                    async fn handle_implementing(&mut self) -> Result<(), Error> {
                        // Check budget before LLM call
                        if !self.budget.has_budget() {
                            let status = self.budget.status();
                            if let BudgetStatus::Exhausted { reason } = status {
                                return Err(Error::BudgetExhausted {
                                    reason: reason.to_string(),
                                });
                            }
                        }

                        // Proceed with LLM call
                        let response = self.llm_client.complete(prompt).await?;

                        // Record token usage
                        let tokens = response.usage.total_tokens;
                        self.budget.record_tokens(tokens)?;

                        Ok(())
                    }
                    """
            },
            {
                name:     "Factory Loop Message Handler for RecordTokens"
                use_when: "Processing RecordTokens message in FactoryLoop actor"
                example:  """
                    impl Actor for FactoryLoop {
                        type Msg = FactoryLoopMessage;

                        async fn handle(
                            &mut self,
                            _myself: ActorRef<Self::Msg>,
                            message: Self::Msg,
                            _state: &mut Self::State,
                        ) -> Result<(), ActorProcessingErr> {
                            match message {
                                FactoryLoopMessage::RecordTokens(tokens) => {
                                    let status = self.state.budget.record_tokens(tokens)
                                        .map_err(|e| ActorProcessingErr::Custom(e.to_string()))?;

                                    if status.is_exhausted() {
                                        self.transition_to_failed("budget_exhausted".into())?;
                                    }
                                }
                                // ... other message handlers
                            }
                            Ok(())
                        }
                    }
                    """
            },
        ]
    }
}
