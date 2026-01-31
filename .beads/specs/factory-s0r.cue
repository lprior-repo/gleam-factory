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

"factory-s0r": #ValidBead & {
    // ============================================================================
    // BEAD: factory-s0r - Implement LLM feedback loop for auto-healing on test failures
    // ============================================================================

    id:              "factory-s0r"
    title:           "Implement LLM feedback loop for auto-healing on test failures"
    type:            "feature"
    priority:        2
    effort_estimate: "4hr"
    labels:          ["llm", "auto-healing", "feedback-loop", "retry", "P2"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL use an LLM client to analyze test failures and generate fixes",
            "THE SYSTEM SHALL apply generated diffs to source code atomically",
            "THE SYSTEM SHALL enforce a maximum iteration limit to prevent infinite loops",
            "THE SYSTEM SHALL enforce a token budget to control LLM costs",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL log all LLM interactions for audit and debugging",
        ]

        event_driven: [
            {
                trigger: "WHEN a test stage fails"
                shall:   "THE SYSTEM SHALL capture test output, diff context, and error messages"
            },
            {
                trigger: "WHEN failure context is captured"
                shall:   "THE SYSTEM SHALL send context to LLM with fix request prompt"
            },
            {
                trigger: "WHEN LLM returns a diff/patch response"
                shall:   "THE SYSTEM SHALL validate and parse the diff format"
            },
            {
                trigger: "WHEN a valid diff is received"
                shall:   "THE SYSTEM SHALL apply the diff to the working copy"
            },
            {
                trigger: "WHEN a diff is applied successfully"
                shall:   "THE SYSTEM SHALL re-run the failed test stage"
            },
            {
                trigger: "WHEN test passes after fix"
                shall:   "THE SYSTEM SHALL return Ok with fix summary and iteration count"
            },
            {
                trigger: "WHEN iteration limit is reached without success"
                shall:   "THE SYSTEM SHALL return Err(IterationLimitExceeded) with last failure"
            },
            {
                trigger: "WHEN token budget is exhausted"
                shall:   "THE SYSTEM SHALL return Err(TokenBudgetExhausted) with usage summary"
            },
        ]

        state_driven: [
            {
                state: "WHILE feedback loop is active"
                shall: "THE SYSTEM SHALL track iteration count, token usage, and applied fixes"
            },
            {
                state: "WHILE applying a diff"
                shall: "THE SYSTEM SHALL maintain a rollback snapshot for recovery"
            },
            {
                state: "WHILE awaiting LLM response"
                shall: "THE SYSTEM SHALL enforce a request timeout"
            },
        ]

        unwanted: [
            {
                condition: "IF the LLM response is not a valid diff"
                shall_not: "THE SYSTEM SHALL NOT apply malformed patches"
                because:   "Malformed patches corrupt source code and break the build"
            },
            {
                condition: "IF iteration limit is set to zero or negative"
                shall_not: "THE SYSTEM SHALL NOT enter the feedback loop"
                because:   "Zero iterations means no retries; negative is invalid"
            },
            {
                condition: "IF token budget is zero"
                shall_not: "THE SYSTEM SHALL NOT send any LLM requests"
                because:   "Zero budget means no LLM usage allowed"
            },
            {
                condition: "IF the diff targets files outside the worktree"
                shall_not: "THE SYSTEM SHALL NOT apply the patch"
                because:   "Path traversal attacks could modify system files"
            },
            {
                condition: "IF the diff would delete critical files (Cargo.toml, go.mod, etc.)"
                shall_not: "THE SYSTEM SHALL NOT apply destructive patches"
                because:   "Deleting manifest files breaks the entire project"
            },
        ]

        complex: [
            {
                state:   "WHILE iteration count < max_iterations"
                trigger: "WHEN token usage exceeds budget"
                shall:   "THE SYSTEM SHALL abort early with Err(TokenBudgetExhausted)"
            },
            {
                state:   "WHILE applying fix"
                trigger: "WHEN diff application fails"
                shall:   "THE SYSTEM SHALL rollback to snapshot and continue to next iteration"
            },
        ]
    }

    // ============================================================================
    // SECTION 2: KIRK CONTRACTS
    // ============================================================================

    contracts: {
        preconditions: {
            auth_required: true  // LLM API key required
            required_inputs: [
                {
                    field:           "FeedbackConfig.max_iterations"
                    type:            "u32"
                    constraints:     "Must be >= 1 and <= 10"
                    example_valid:   "3"
                    example_invalid: "0 or 100"
                },
                {
                    field:           "FeedbackConfig.token_budget"
                    type:            "u64"
                    constraints:     "Must be >= 1000 (minimum for one request)"
                    example_valid:   "50000"
                    example_invalid: "100"
                },
                {
                    field:           "FeedbackConfig.llm_client"
                    type:            "Box<dyn LlmClient>"
                    constraints:     "Must implement LlmClient trait"
                    example_valid:   "AnthropicClient::new(api_key)"
                    example_invalid: "None"
                },
                {
                    field:           "TestFailure.output"
                    type:            "String"
                    constraints:     "Non-empty test output with error context"
                    example_valid:   "test_foo FAILED: assertion failed: expected 42, got 43"
                    example_invalid: "\"\" (empty)"
                },
                {
                    field:           "TestFailure.file_path"
                    type:            "PathBuf"
                    constraints:     "Must exist within worktree"
                    example_valid:   "src/lib.rs"
                    example_invalid: "../../../etc/passwd"
                },
            ]
            system_state: [
                "LLM API key configured (ANTHROPIC_API_KEY or similar)",
                "Worktree exists and is writable",
                "Test stage can be re-executed",
                "factory-c2s (LLM client) dependency available",
                "factory-czj (diff application) dependency available",
            ]
        }

        postconditions: {
            state_changes: [
                "On success: source files modified with applied fixes",
                "On success: audit log contains fix history",
                "On failure: source files unchanged (rollback applied)",
                "Token usage recorded regardless of outcome",
            ]
            return_guarantees: [
                {
                    field:     "FeedbackResult::Success"
                    guarantee: "Contains iteration_count, total_tokens, applied_fixes Vec"
                },
                {
                    field:     "FeedbackResult::IterationLimitExceeded"
                    guarantee: "Contains last_error, iteration_count, attempted_fixes"
                },
                {
                    field:     "FeedbackResult::TokenBudgetExhausted"
                    guarantee: "Contains tokens_used, tokens_remaining, last_iteration"
                },
                {
                    field:     "FeedbackResult::LlmError"
                    guarantee: "Contains underlying LLM client error with context"
                },
            ]
            side_effects: [
                "LLM API requests made (counted against budget)",
                "Audit entries written to .factory/feedback_audit.jsonl",
                "Temporary backup files created and cleaned up",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Token usage never exceeds budget (checked before each request)",
            "Iteration count never exceeds max_iterations",
            "All diffs are validated before application",
            "All file paths are canonicalized and checked against worktree root",
            "Rollback snapshot exists before any modification",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "LLM API returns rate limit error"
                prevention:  "Implement exponential backoff with jitter"
                test_for_it: "test_rate_limit_backoff"
            },
            {
                failure:     "LLM returns code in wrong language"
                prevention:  "Include language context in prompt, validate response"
                test_for_it: "test_wrong_language_rejection"
            },
            {
                failure:     "Diff format incompatible with unified diff parser"
                prevention:  "Prompt for unified diff format, validate structure"
                test_for_it: "test_invalid_diff_format_handling"
            },
            {
                failure:     "Network timeout during LLM request"
                prevention:  "Configure request timeout, retry with backoff"
                test_for_it: "test_network_timeout_recovery"
            },
            {
                failure:     "factory-c2s LLM client not available"
                prevention:  "Check dependency at startup, fail fast with clear error"
                test_for_it: "test_missing_llm_client_dependency"
            },
        ]

        usability_failures: [
            {
                failure:     "Fix makes test pass but breaks other tests"
                prevention:  "Run full test suite after fix, not just failed test"
                test_for_it: "test_regression_detection"
            },
            {
                failure:     "LLM fix is syntactically correct but semantically wrong"
                prevention:  "Include test intent in prompt, validate fix compiles"
                test_for_it: "test_semantic_fix_validation"
            },
            {
                failure:     "User cannot understand what fixes were applied"
                prevention:  "Log each fix with before/after diff and explanation"
                test_for_it: "test_fix_audit_contains_explanation"
            },
            {
                failure:     "Feedback loop takes too long"
                prevention:  "Add overall timeout, show progress indicators"
                test_for_it: "test_overall_timeout"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Partial diff application corrupts file"
                prevention:  "Atomic write: write to temp, rename on success"
                test_for_it: "test_atomic_diff_application"
            },
            {
                failure:     "Rollback fails, leaving corrupted state"
                prevention:  "Copy original to .bak before modification"
                test_for_it: "test_rollback_restores_original"
            },
            {
                failure:     "Concurrent feedback loops conflict"
                prevention:  "Lock worktree during feedback loop execution"
                test_for_it: "test_concurrent_loop_rejection"
            },
            {
                failure:     "Token count drift from actual API usage"
                prevention:  "Use response metadata for actual token count"
                test_for_it: "test_token_count_accuracy"
            },
        ]

        security_failures: [
            {
                failure:     "LLM suggests path traversal in diff"
                prevention:  "Canonicalize paths, reject if outside worktree"
                test_for_it: "test_path_traversal_blocked"
            },
            {
                failure:     "LLM suggests deleting critical files"
                prevention:  "Maintain blocklist of protected files"
                test_for_it: "test_protected_file_deletion_blocked"
            },
            {
                failure:     "Sensitive data leaked to LLM"
                prevention:  "Redact secrets from test output before sending"
                test_for_it: "test_secret_redaction"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_feedback_loop_fixes_simple_failure"
                given: "A test failure with clear error message"
                when:  "FeedbackLoop::run() is called"
                then: [
                    "LLM is called with failure context",
                    "Diff is applied to source file",
                    "Test is re-run and passes",
                    "Returns FeedbackResult::Success with iteration_count=1",
                ]
                real_input: """
                    let failure = TestFailure {
                        stage: Stage::from_str("unit-test")?,
                        output: "assertion failed: expected 42, got 43".to_string(),
                        file_path: PathBuf::from("src/math.rs"),
                        line_number: Some(15),
                    };
                    let config = FeedbackConfig {
                        max_iterations: 3,
                        token_budget: 50_000,
                        llm_client: Box::new(mock_client),
                    };
                    let result = FeedbackLoop::new(config)
                        .run(failure, &worktree_path)
                        .await?;
                    """
                expected_output: """
                    assert!(matches!(result, FeedbackResult::Success { .. }));
                    assert_eq!(result.iteration_count(), 1);
                    assert!(result.applied_fixes().len() >= 1);
                    """
            },
            {
                name:  "test_feedback_loop_succeeds_on_second_attempt"
                given: "A test failure where first fix is insufficient"
                when:  "FeedbackLoop::run() is called"
                then: [
                    "First fix is applied but test still fails",
                    "Second LLM call includes previous fix context",
                    "Second fix resolves the issue",
                    "Returns Success with iteration_count=2",
                ]
                real_input: """
                    let mock_client = MockLlmClient::new()
                        .with_response(0, partial_fix_diff)
                        .with_response(1, complete_fix_diff);
                    let result = FeedbackLoop::new(config)
                        .run(failure, &worktree_path)
                        .await?;
                    """
                expected_output: """
                    assert_eq!(result.iteration_count(), 2);
                    assert_eq!(result.applied_fixes().len(), 2);
                    """
            },
            {
                name:  "test_token_usage_tracked_accurately"
                given: "Multiple LLM interactions"
                when:  "FeedbackLoop completes"
                then: [
                    "Total tokens equals sum of all request tokens",
                    "Token usage returned in result",
                    "Audit log contains per-request token counts",
                ]
                real_input: """
                    let result = FeedbackLoop::new(config)
                        .run(failure, &worktree_path)
                        .await?;
                    """
                expected_output: """
                    let usage = result.token_usage();
                    assert!(usage.total_tokens > 0);
                    assert!(usage.prompt_tokens + usage.completion_tokens == usage.total_tokens);
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_iteration_limit_exceeded"
                given: "A test failure that cannot be fixed within iteration limit"
                when:  "FeedbackLoop exhausts max_iterations"
                then: [
                    "All iterations attempted",
                    "Returns Err(IterationLimitExceeded)",
                    "Source files rolled back to original state",
                ]
                real_input: """
                    let config = FeedbackConfig {
                        max_iterations: 2,
                        token_budget: 100_000,
                        llm_client: Box::new(always_wrong_client),
                    };
                    let result = FeedbackLoop::new(config)
                        .run(failure, &worktree_path)
                        .await;
                    """
                expected_output: null
                expected_error: """
                    Error::IterationLimitExceeded {
                        max_iterations: 2,
                        last_error: "test_foo still failing: expected 42, got 43",
                    }
                    """
            },
            {
                name:  "test_token_budget_exhausted"
                given: "A token budget that runs out mid-loop"
                when:  "Next LLM request would exceed budget"
                then: [
                    "Loop stops before exceeding budget",
                    "Returns Err(TokenBudgetExhausted)",
                    "Actual usage reported",
                ]
                real_input: """
                    let config = FeedbackConfig {
                        max_iterations: 10,
                        token_budget: 5_000,  // Very small budget
                        llm_client: Box::new(verbose_client),
                    };
                    """
                expected_output: null
                expected_error: """
                    Error::TokenBudgetExhausted {
                        budget: 5_000,
                        used: 4_800,
                        requested: 1_500,
                    }
                    """
            },
            {
                name:  "test_invalid_diff_rejected"
                given: "LLM returns malformed diff"
                when:  "Diff parsing is attempted"
                then: [
                    "Diff validation fails",
                    "Error logged, iteration continues",
                    "Fix not applied, retry with clarified prompt",
                ]
                real_input: """
                    let mock_client = MockLlmClient::new()
                        .with_response(0, "this is not a valid diff")
                        .with_response(1, valid_unified_diff);
                    """
                expected_output: """
                    // First attempt fails parsing, second succeeds
                    assert_eq!(result.iteration_count(), 2);
                    assert_eq!(result.parse_failures(), 1);
                    """
            },
            {
                name:  "test_path_traversal_blocked"
                given: "LLM returns diff with path traversal"
                when:  "Diff path validation runs"
                then: [
                    "Path traversal detected",
                    "Diff rejected with SecurityViolation error",
                    "Iteration continues with fresh prompt",
                ]
                real_input: """
                    let malicious_diff = r#"
                    --- a/../../../etc/passwd
                    +++ b/../../../etc/passwd
                    @@ -1,1 +1,2 @@
                     root:x:0:0:root:/root:/bin/bash
                    +hacked:x:0:0:hacked:/root:/bin/bash
                    "#;
                    """
                expected_output: null
                expected_error: """
                    Error::SecurityViolation {
                        reason: "path traversal detected: ../../../etc/passwd",
                    }
                    """
            },
            {
                name:  "test_llm_api_error_propagated"
                given: "LLM API returns error"
                when:  "LLM request fails"
                then: [
                    "Error wrapped with context",
                    "Retry with backoff attempted",
                    "After max retries, error propagated",
                ]
                real_input: """
                    let failing_client = MockLlmClient::new()
                        .with_error(LlmError::RateLimited { retry_after_secs: 60 });
                    """
                expected_output: null
                expected_error: """
                    Error::LlmRequestFailed {
                        attempt: 3,
                        cause: "rate limited, retry after 60s",
                    }
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_empty_test_output"
                scenario: "Test fails with no output"
                input:    "TestFailure { output: \"\".to_string(), .. }"
                expected: "LLM receives minimal context, may fail gracefully"
            },
            {
                name:     "test_very_large_test_output"
                scenario: "Test output exceeds context window"
                input:    "TestFailure { output: large_output, .. } where large_output > 100KB"
                expected: "Output truncated intelligently, key errors preserved"
            },
            {
                name:     "test_binary_file_in_diff"
                scenario: "LLM suggests modifying binary file"
                input:    "Diff targeting .png or .exe file"
                expected: "Binary file modification rejected"
            },
            {
                name:     "test_concurrent_modification"
                scenario: "File modified by another process during loop"
                input:    "File changed between read and write"
                expected: "Conflict detected, iteration aborted with error"
            },
            {
                name:     "test_zero_max_iterations"
                scenario: "Config with max_iterations=0"
                input:    "FeedbackConfig { max_iterations: 0, .. }"
                expected: "Returns immediately with NoIterationsAllowed error"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in feedback module"
                test:     "grep -r 'unwrap()\\|expect(' crates/factory-core/src/feedback.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All public APIs return Result"
                test:     "cargo doc shows Result returns for all pub fn"
            },
            {
                name:     "test_precondition_validation"
                verifies: "Invalid config rejected at construction"
                test:     "FeedbackConfig::new() with invalid params returns Err"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_feedback_loop_e2e"
            description: "Complete feedback loop: failure -> LLM -> fix -> retest -> success"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/feedback_e2e.rs"
                        content: """
                            use factory_core::feedback::{FeedbackConfig, FeedbackLoop, FeedbackResult};
                            use factory_core::llm::MockLlmClient;
                            use factory_core::error::Result;

                            #[tokio::test]
                            async fn test_full_feedback_loop_e2e() -> Result<()> {
                                // Setup mock LLM that returns valid fix
                                let mock = MockLlmClient::with_fix(VALID_DIFF);
                                let config = FeedbackConfig::builder()
                                    .max_iterations(3)
                                    .token_budget(50_000)
                                    .llm_client(Box::new(mock))
                                    .build()?;

                                // Create test failure scenario
                                let failure = create_test_failure()?;
                                let worktree = setup_worktree()?;

                                // Run feedback loop
                                let result = FeedbackLoop::new(config)
                                    .run(failure, &worktree)
                                    .await?;

                                // Verify success
                                assert!(matches!(result, FeedbackResult::Success { .. }));
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
                command:    "moon run factory-core:test -- --test feedback_e2e"
                timeout_ms: 60000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_feedback_loop_e2e ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/feedback.rs"
                        contains: "pub struct FeedbackLoop"
                    },
                    {
                        path:     "crates/factory-core/src/feedback.rs"
                        contains: "pub async fn run"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm -f crates/factory-core/tests/feedback_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_real_test_failure_fix"
                description: "Fix a real failing Rust test with mock LLM"
                steps: [
                    {action: "Create worktree with failing test", verify: "Test fails with clear error"},
                    {action: "Configure feedback loop with mock", verify: "Config validated"},
                    {action: "Run feedback loop", verify: "LLM called with failure context"},
                    {action: "Verify diff applied", verify: "Source file modified"},
                    {action: "Verify test passes", verify: "Exit code 0"},
                    {action: "Verify audit log", verify: "Fix recorded with metadata"},
                ]
            },
            {
                name:        "e2e_multi_file_fix"
                description: "Fix that spans multiple files"
                steps: [
                    {action: "Create failure requiring multi-file fix", verify: "Failure spans types"},
                    {action: "LLM returns multi-file diff", verify: "Diff parsed correctly"},
                    {action: "All files updated atomically", verify: "Either all or none applied"},
                    {action: "Tests pass", verify: "Integration verified"},
                ]
            },
            {
                name:        "e2e_rollback_on_failure"
                description: "Verify clean rollback when fix fails"
                steps: [
                    {action: "Apply fix that breaks compilation", verify: "Compilation fails"},
                    {action: "Rollback triggered", verify: "Original file restored"},
                    {action: "Next iteration starts", verify: "Clean state for retry"},
                    {action: "Verify no corruption", verify: "File hash matches original"},
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
                task:      "Write test: test_feedback_loop_fixes_simple_failure"
                file:      "crates/factory-core/src/feedback.rs"
                what:      "Test that a simple fix is applied and test passes"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_iteration_limit_exceeded"
                file:      "crates/factory-core/src/feedback.rs"
                what:      "Test that iteration limit stops the loop"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_token_budget_exhausted"
                file:      "crates/factory-core/src/feedback.rs"
                what:      "Test that token budget enforcement works"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_invalid_diff_rejected"
                file:      "crates/factory-core/src/feedback.rs"
                what:      "Test that malformed diffs are rejected"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_path_traversal_blocked"
                file:      "crates/factory-core/src/feedback.rs"
                what:      "Test that path traversal attacks are blocked"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Define FeedbackConfig struct"
                file: "crates/factory-core/src/feedback.rs"
                what: """
                    pub struct FeedbackConfig {
                        max_iterations: u32,
                        token_budget: u64,
                        llm_client: Box<dyn LlmClient>,
                        request_timeout: Duration,
                        protected_files: Vec<String>,
                    }
                    """
                done_when:     "Struct compiles with builder pattern"
                patterns_to_use: ["Builder pattern", "Validation in build()"]
            },
            {
                task: "Define TestFailure and FeedbackResult types"
                file: "crates/factory-core/src/feedback.rs"
                what: """
                    pub struct TestFailure {
                        pub stage: Stage,
                        pub output: String,
                        pub file_path: PathBuf,
                        pub line_number: Option<u32>,
                    }

                    pub enum FeedbackResult {
                        Success { iteration_count: u32, total_tokens: u64, applied_fixes: Vec<AppliedFix> },
                        IterationLimitExceeded { max_iterations: u32, last_error: String },
                        TokenBudgetExhausted { budget: u64, used: u64, requested: u64 },
                    }
                    """
                done_when:     "Types compile and are documented"
                patterns_to_use: ["Sum types for results", "Descriptive error data"]
            },
            {
                task: "Implement LlmClient trait"
                file: "crates/factory-core/src/llm.rs"
                what: """
                    #[async_trait]
                    pub trait LlmClient: Send + Sync {
                        async fn request_fix(
                            &self,
                            context: &FailureContext,
                        ) -> Result<LlmResponse>;

                        fn estimate_tokens(&self, context: &FailureContext) -> u64;
                    }
                    """
                done_when:     "Trait compiles, MockLlmClient implements it"
                patterns_to_use: ["async_trait for async methods", "Send + Sync for thread safety"]
            },
            {
                task: "Implement DiffParser"
                file: "crates/factory-core/src/diff.rs"
                what: """
                    Parse unified diff format, validate paths, apply atomically.
                    Uses factory-czj dependency for diff application.
                    """
                done_when:     "Can parse and apply simple unified diffs"
                patterns_to_use: ["Nom or pest for parsing", "Atomic file writes"]
            },
            {
                task: "Implement FeedbackLoop::run()"
                file: "crates/factory-core/src/feedback.rs"
                what: """
                    pub async fn run(
                        &self,
                        failure: TestFailure,
                        worktree_path: &Path,
                    ) -> Result<FeedbackResult> {
                        let mut state = LoopState::new(&self.config);

                        while state.can_continue() {
                            let context = self.build_context(&failure, &state)?;

                            // Check token budget before request
                            let estimated = self.config.llm_client.estimate_tokens(&context);
                            if !state.can_afford(estimated) {
                                return Ok(FeedbackResult::TokenBudgetExhausted { .. });
                            }

                            // Request fix from LLM
                            let response = self.config.llm_client
                                .request_fix(&context)
                                .await?;
                            state.record_tokens(response.usage);

                            // Parse and validate diff
                            let diff = match DiffParser::parse(&response.content) {
                                Ok(d) => d,
                                Err(e) => {
                                    state.record_parse_failure(e);
                                    continue;
                                }
                            };

                            // Validate paths
                            self.validate_diff_paths(&diff, worktree_path)?;

                            // Create backup and apply
                            let backup = self.backup_files(&diff, worktree_path)?;
                            if let Err(e) = self.apply_diff(&diff, worktree_path) {
                                self.restore_backup(&backup)?;
                                state.record_apply_failure(e);
                                continue;
                            }

                            // Re-run test
                            if self.run_test(&failure.stage, worktree_path)? {
                                return Ok(FeedbackResult::Success {
                                    iteration_count: state.iteration,
                                    total_tokens: state.total_tokens,
                                    applied_fixes: state.fixes,
                                });
                            }

                            state.increment();
                        }

                        Ok(FeedbackResult::IterationLimitExceeded { .. })
                    }
                    """
                done_when:     "Core loop compiles and passes basic tests"
                patterns_to_use: [
                    "State machine for loop control",
                    "Early return for budget exhaustion",
                    "Continue for recoverable errors",
                ]
            },
            {
                task: "Implement path validation"
                file: "crates/factory-core/src/feedback.rs"
                what: """
                    fn validate_diff_paths(&self, diff: &Diff, worktree: &Path) -> Result<()> {
                        for file in diff.affected_files() {
                            let canonical = worktree.join(&file).canonicalize()
                                .map_err(|_| Error::InvalidPath { path: file.clone() })?;

                            if !canonical.starts_with(worktree) {
                                return Err(Error::SecurityViolation {
                                    reason: format!("path traversal detected: {}", file),
                                });
                            }

                            if self.config.protected_files.contains(&file) {
                                return Err(Error::SecurityViolation {
                                    reason: format!("protected file: {}", file),
                                });
                            }
                        }
                        Ok(())
                    }
                    """
                done_when:     "Path traversal and protected file tests pass"
                patterns_to_use: ["Path canonicalization", "Explicit blocklist"]
            },
            {
                task: "Implement backup/restore"
                file: "crates/factory-core/src/feedback.rs"
                what: """
                    Backup: Copy affected files to temp dir before modification.
                    Restore: Copy backup files back on failure.
                    Cleanup: Remove temp dir on success.
                    """
                done_when:     "Rollback test passes"
                patterns_to_use: ["tempfile crate for temp dirs", "RAII for cleanup"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Add feedback error variants to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "IterationLimitExceeded, TokenBudgetExhausted, DiffParseFailed, SecurityViolation, LlmRequestFailed"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Export feedback module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod feedback; pub mod llm; pub mod diff;"
                done_when: "External crates can import factory_core::feedback"
            },
            {
                task:      "Add audit logging for feedback loop"
                file:      "crates/factory-core/src/audit.rs"
                what:      "Log iteration start, LLM request, diff applied, test result, final outcome"
                done_when: "Audit entries written during feedback loop"
            },
            {
                task:      "Wire up to stage execution"
                file:      "crates/factory-core/src/stages/mod.rs"
                what:      "Option to enable feedback loop on stage failure"
                done_when: "factory stage --auto-fix triggers feedback loop"
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
                commands: ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/feedback.rs crates/factory-core/src/llm.rs crates/factory-core/src/diff.rs"]
                expected: "no output (empty)"
            },
            {
                task:      "Manual verification with mock LLM"
                done_when: "Feedback loop runs end-to-end with mock"
                commands: [
                    "cargo run -- stage -s test-task --stage unit-test --auto-fix",
                ]
                expected: "Feedback loop activates on failure, applies fix, test passes"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'LlmClient' trait not found"
                likely_cause: "factory-c2s dependency not added or llm module not exported"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "factory-c2s or llm client dependency exists"
                    },
                    {
                        file:          "crates/factory-core/src/lib.rs"
                        what_to_check: "pub mod llm; is present"
                    },
                ]
                fix_pattern: "Add dependency and export module"
            },
            {
                symptom:      "Compilation error: 'DiffParser' not found"
                likely_cause: "factory-czj dependency not linked or diff module missing"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "factory-czj or diff parsing dependency exists"
                    },
                    {
                        file:          "crates/factory-core/src/lib.rs"
                        what_to_check: "pub mod diff; is present"
                    },
                ]
                fix_pattern: "Add dependency and export module"
            },
            {
                symptom:      "Runtime error: 'token budget exceeded' on first request"
                likely_cause: "Token estimation is wrong or budget too low"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/llm.rs"
                        function:      "estimate_tokens()"
                        what_to_check: "Is estimation accurate? Does it account for system prompt?"
                    },
                    {
                        file:          "crates/factory-core/src/feedback.rs"
                        function:      "run()"
                        what_to_check: "Is budget check before or after request?"
                    },
                ]
                fix_pattern: "Fix token estimation or increase minimum budget"
            },
            {
                symptom:      "Diff application silently fails"
                likely_cause: "Diff line numbers don't match file state"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/diff.rs"
                        function:      "apply()"
                        what_to_check: "Are context lines verified before patching?"
                    },
                ]
                fix_pattern: "Add context line verification, fail loudly on mismatch"
            },
            {
                symptom:      "Test passes but wrong file modified"
                likely_cause: "Path resolution error in diff parsing"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/diff.rs"
                        function:      "parse_file_path()"
                        what_to_check: "Is a/ or b/ prefix stripped correctly?"
                    },
                ]
                fix_pattern: "Fix path prefix handling in unified diff parser"
            },
            {
                symptom:      "Feedback loop hangs indefinitely"
                likely_cause: "LLM request timeout not configured or test re-run hangs"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/llm.rs"
                        function:      "request_fix()"
                        what_to_check: "Is there a timeout on the HTTP request?"
                    },
                    {
                        file:          "crates/factory-core/src/feedback.rs"
                        function:      "run_test()"
                        what_to_check: "Is there a timeout on test execution?"
                    },
                ]
                fix_pattern: "Add tokio::time::timeout wrapper around async operations"
            },
        ]

        debugging_commands: [
            {
                scenario: "When LLM returns unexpected format"
                run:      "RUST_LOG=factory_core::feedback=debug,factory_core::llm=debug cargo test"
                look_for: "Raw LLM response, parse errors"
            },
            {
                scenario: "When diff doesn't apply"
                run:      "RUST_LOG=factory_core::diff=trace cargo test"
                look_for: "Line numbers, context matching, file state"
            },
            {
                scenario: "When token budget seems wrong"
                run:      "RUST_LOG=factory_core::llm=debug cargo test -- --nocapture"
                look_for: "Estimated vs actual tokens per request"
            },
            {
                scenario: "When rollback fails"
                run:      "ls -la /tmp/factory-backup-*"
                look_for: "Backup files existence and contents"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_feedback_loop_fixes_simple_failure passes",
            "[ ] test_feedback_loop_succeeds_on_second_attempt passes",
            "[ ] test_iteration_limit_exceeded passes",
            "[ ] test_token_budget_exhausted passes",
            "[ ] test_invalid_diff_rejected passes",
            "[ ] test_path_traversal_blocked passes",
            "[ ] test_rollback_restores_original passes",
            "[ ] test_atomic_diff_application passes",
            "[ ] test_token_usage_tracked_accurately passes",
            "[ ] E2E pipeline test passing with mock LLM",
        ]

        code: [
            "[ ] FeedbackConfig struct with builder pattern",
            "[ ] TestFailure and FeedbackResult types defined",
            "[ ] LlmClient trait with async request_fix()",
            "[ ] MockLlmClient for testing",
            "[ ] DiffParser with unified diff support",
            "[ ] FeedbackLoop::run() implements full loop",
            "[ ] Path validation with traversal detection",
            "[ ] Protected files blocklist",
            "[ ] Backup and restore functionality",
            "[ ] Token budget enforcement",
            "[ ] Iteration limit enforcement",
            "[ ] All public APIs return Result<T, Error>",
            "[ ] Zero unwrap() or expect() calls",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in feedback.rs",
            "[ ] Module-level docs in llm.rs",
            "[ ] Module-level docs in diff.rs",
            "[ ] Example usage in doc comments",
            "[ ] FeedbackConfig builder documented",
        ]

        security: [
            "[ ] Path traversal test exists and passes",
            "[ ] Protected files test exists and passes",
            "[ ] Secret redaction implemented (if applicable)",
            "[ ] No sensitive data in audit logs",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - must add feedback, llm, diff"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add feedback error variants"
            },
            {
                path:      "crates/factory-core/src/stages/mod.rs"
                relevance: "Stage execution - integration point for auto-fix"
            },
            {
                path:      "crates/factory-core/src/process.rs"
                relevance: "Command execution - used for running tests"
            },
            {
                path:      "crates/factory-core/src/audit.rs"
                relevance: "Audit logging - must log feedback loop events"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - add async-trait, tokio, reqwest"
            },
        ]

        dependencies: [
            {
                bead_id:     "factory-c2s"
                title:       "LLM client integration"
                provides:    "LlmClient trait, API wrappers"
                blocks_this: true
            },
            {
                bead_id:     "factory-czj"
                title:       "Diff application"
                provides:    "DiffParser, patch application logic"
                blocks_this: true
            },
        ]

        external_references: [
            "https://docs.rs/async-trait - async methods in traits",
            "https://docs.rs/reqwest - HTTP client for LLM APIs",
            "https://www.gnu.org/software/diffutils/manual/html_node/Detailed-Unified.html - unified diff format",
            "https://docs.anthropic.com/claude/reference/messages_post - Anthropic API",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then"
            },
            {
                pattern:          "Builder Pattern for Config"
                example_location: "crates/factory-core/src/domain.rs:TaskBuilder"
                how_to_apply:     "FeedbackConfigBuilder with validation in build()"
            },
            {
                pattern:          "State Machine for Complex Flow"
                example_location: "crates/factory-core/src/stages/mod.rs"
                how_to_apply:     "LoopState struct tracking iteration, tokens, fixes"
            },
        ]

        gleam_port_notes: """
            Original Gleam implementation in feedback_loop.gleam used:
            - `use` for monadic error handling (map to ? operator in Rust)
            - `case` for pattern matching (map to match in Rust)
            - Immutable state updates (map to mutable struct in Rust loop)
            - OTP GenServer for retry loop (map to simple while loop in Rust)

            Key differences to handle:
            - Rust async/await vs Gleam's synchronous model
            - Rust's ownership for backup/restore vs Gleam's immutable copies
            - Rust trait objects vs Gleam behaviours for LLM client
            """
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use async-trait for the LlmClient trait",
            "Use reqwest with timeout for HTTP requests",
            "Use tempfile crate for backup directory",
            "Use tokio::time::timeout around all async operations",
            "Log all LLM requests and responses for debugging",
            "Include file content snippet in LLM prompt for context",
            "Return structured FeedbackResult enum, not just Result",
            "Track parse failures separately from apply failures",
            "Verify context lines in diff before applying",
            "Use canonicalize() for all path security checks",
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT send sensitive data (API keys, secrets) to LLM",
            "Do NOT apply diffs to files outside worktree",
            "Do NOT exceed token budget under any circumstances",
            "Do NOT retry infinitely on rate limits",
            "Do NOT trust LLM output - always validate",
            "Do NOT leave partial modifications on failure",
        ]

        code_patterns: [
            {
                name:     "Token Budget Check"
                use_when: "Before each LLM request"
                example:  """
                    let estimated = self.llm_client.estimate_tokens(&context);
                    if state.tokens_used + estimated > self.config.token_budget {
                        return Ok(FeedbackResult::TokenBudgetExhausted {
                            budget: self.config.token_budget,
                            used: state.tokens_used,
                            requested: estimated,
                        });
                    }
                    """
            },
            {
                name:     "Atomic File Modification"
                use_when: "Applying diff to file"
                example:  """
                    // Write to temp file first
                    let temp_path = path.with_extension("tmp");
                    std::fs::write(&temp_path, &new_content)
                        .map_err(|e| Error::file_write_failed(&temp_path, e))?;

                    // Atomic rename
                    std::fs::rename(&temp_path, &path)
                        .map_err(|e| Error::file_write_failed(&path, e))?;
                    """
            },
            {
                name:     "Path Security Validation"
                use_when: "Processing paths from LLM diff"
                example:  """
                    fn validate_path(path: &str, worktree: &Path) -> Result<PathBuf> {
                        let full_path = worktree.join(path);
                        let canonical = full_path.canonicalize()
                            .map_err(|_| Error::InvalidPath { path: path.to_string() })?;

                        if !canonical.starts_with(worktree) {
                            return Err(Error::SecurityViolation {
                                reason: format!("path traversal: {}", path),
                            });
                        }
                        Ok(canonical)
                    }
                    """
            },
            {
                name:     "Retry with Exponential Backoff"
                use_when: "LLM request fails with retryable error"
                example:  """
                    async fn request_with_retry<F, T>(
                        f: F,
                        max_retries: u32,
                    ) -> Result<T>
                    where
                        F: Fn() -> futures::future::BoxFuture<'static, Result<T>>,
                    {
                        let mut delay = Duration::from_millis(100);
                        for attempt in 0..max_retries {
                            match f().await {
                                Ok(result) => return Ok(result),
                                Err(e) if e.is_retryable() => {
                                    tokio::time::sleep(delay).await;
                                    delay *= 2;
                                }
                                Err(e) => return Err(e),
                            }
                        }
                        Err(Error::MaxRetriesExceeded { attempts: max_retries })
                    }
                    """
            },
            {
                name:     "LLM Prompt Construction"
                use_when: "Building context for fix request"
                example:  """
                    fn build_prompt(failure: &TestFailure, history: &[AppliedFix]) -> String {
                        let mut prompt = String::new();
                        prompt.push_str("Fix the following test failure.\n\n");
                        prompt.push_str(&format!("Test stage: {}\n", failure.stage));
                        prompt.push_str(&format!("File: {}\n", failure.file_path.display()));
                        if let Some(line) = failure.line_number {
                            prompt.push_str(&format!("Line: {}\n", line));
                        }
                        prompt.push_str(&format!("\nTest output:\n```\n{}\n```\n", failure.output));

                        if !history.is_empty() {
                            prompt.push_str("\nPrevious fix attempts that did not resolve the issue:\n");
                            for fix in history {
                                prompt.push_str(&format!("- {}\n", fix.description));
                            }
                        }

                        prompt.push_str("\nProvide a unified diff that fixes the issue.\n");
                        prompt
                    }
                    """
            },
        ]
    }
}
