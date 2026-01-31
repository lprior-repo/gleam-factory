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

"factory-e7n": #ValidBead & {
    // ============================================================================
    // BEAD: factory-e7n - Add functional programming utilities
    // ============================================================================

    id:              "factory-e7n"
    title:           "FP Utilities: Railway combinators, validated types, pure/impure separation"
    type:            "feature"
    priority:        3
    effort_estimate: "4hr"
    labels:          ["fp", "types", "patterns", "foundation", "P3"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL provide Railway-Oriented Programming combinators for Result chaining",
            "THE SYSTEM SHALL provide validated opaque types that make illegal states unrepresentable",
            "THE SYSTEM SHALL separate pure functions from I/O operations at the module level",
            "THE SYSTEM SHALL provide builder patterns for complex type construction",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
        ]

        event_driven: [
            {
                trigger: "WHEN a validated type constructor is called with invalid input"
                shall:   "THE SYSTEM SHALL return Err with descriptive validation error"
            },
            {
                trigger: "WHEN a Railway combinator encounters an error"
                shall:   "THE SYSTEM SHALL short-circuit and propagate the error unchanged"
            },
            {
                trigger: "WHEN a builder is finalized with missing required fields"
                shall:   "THE SYSTEM SHALL return Err describing which fields are missing"
            },
            {
                trigger: "WHEN and_then is chained on Ok value"
                shall:   "THE SYSTEM SHALL apply the function and return its Result"
            },
            {
                trigger: "WHEN map_err is applied to Err value"
                shall:   "THE SYSTEM SHALL transform the error and return new Err"
            },
        ]

        state_driven: [
            {
                state: "WHILE a validated type instance exists"
                shall: "THE SYSTEM SHALL guarantee the inner value satisfies all validation invariants"
            },
            {
                state: "WHILE a builder is accumulating values"
                shall: "THE SYSTEM SHALL not perform validation until build() is called"
            },
        ]

        unwanted: [
            {
                condition: "IF a pure function needs to perform I/O"
                shall_not: "THE SYSTEM SHALL NOT allow I/O in pure modules"
                because:   "Mixing I/O with pure logic breaks testability and composability"
            },
            {
                condition: "IF a validated type needs to expose its inner value"
                shall_not: "THE SYSTEM SHALL NOT provide mutable access to inner data"
                because:   "Mutable access could invalidate the type's invariants"
            },
            {
                condition: "IF multiple validations are needed"
                shall_not: "THE SYSTEM SHALL NOT stop at first error when all errors can be collected"
                because:   "Users need to see all validation failures at once, not one at a time"
            },
        ]

        complex: [
            {
                state:   "WHILE building a complex type with multiple validations"
                trigger: "WHEN build() is called"
                shall:   "THE SYSTEM SHALL run all validations and collect all errors before returning"
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
                    field:           "WorkspaceName"
                    type:            "String"
                    constraints:     "1-64 chars, alphanumeric with hyphens, no leading/trailing hyphen"
                    example_valid:   "my-workspace-1"
                    example_invalid: "-invalid-"
                },
                {
                    field:           "OperationId"
                    type:            "String"
                    constraints:     "UUID v4 format (36 chars with hyphens)"
                    example_valid:   "550e8400-e29b-41d4-a716-446655440000"
                    example_invalid: "not-a-uuid"
                },
                {
                    field:           "ChangeId"
                    type:            "String"
                    constraints:     "JJ change ID format (16+ lowercase alphanumeric)"
                    example_valid:   "zzzzzzzzzzzzzzzz"
                    example_invalid: "UPPERCASE"
                },
            ]
            system_state: [
                "Rust toolchain installed (rustc, cargo)",
                "Moon build system configured",
                "factory-core crate exists with domain.rs and error.rs",
            ]
        }

        postconditions: {
            state_changes: [
                "New module: crates/factory-core/src/fp.rs (Railway combinators)",
                "New validated types in domain.rs: WorkspaceName, OperationId, ChangeId",
                "New module: crates/factory-core/src/pure.rs (pure functions)",
                "New module: crates/factory-core/src/builders.rs (builder patterns)",
                "lib.rs updated to export new modules",
            ]
            return_guarantees: [
                {
                    field:     "WorkspaceName::new()"
                    guarantee: "Returns Result<WorkspaceName, Error> - valid if Ok"
                },
                {
                    field:     "OperationId::new()"
                    guarantee: "Returns Result<OperationId, Error> - valid UUID if Ok"
                },
                {
                    field:     "ChangeId::new()"
                    guarantee: "Returns Result<ChangeId, Error> - valid JJ change ID if Ok"
                },
                {
                    field:     "ResultExt::and_then_async()"
                    guarantee: "Chains async operations in Railway style"
                },
                {
                    field:     "Builder::build()"
                    guarantee: "Returns Result with all validation errors collected"
                },
            ]
            side_effects: [
                "None - all new code is pure types and functions",
            ]
        }

        invariants: [
            "All validated types guarantee their constraints if constructed successfully",
            "Pure module contains zero I/O operations (no std::fs, no std::net, no async)",
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "Builder pattern collects all validation errors, not just first",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "New validated types conflict with existing Slug/GitHash patterns"
                prevention:  "Follow exact same pattern as Slug: new(), as_str(), TryFrom, Display"
                test_for_it: "test_validated_types_follow_slug_pattern"
            },
            {
                failure:     "Railway combinators conflict with std Result methods"
                prevention:  "Use extension trait pattern, don't shadow std methods"
                test_for_it: "test_railway_combinators_extend_result"
            },
            {
                failure:     "Pure module accidentally imports I/O"
                prevention:  "Lint for std::fs, std::net, tokio:: imports in pure.rs"
                test_for_it: "test_pure_module_has_no_io_imports"
            },
        ]

        usability_failures: [
            {
                failure:     "Validation error messages are unclear"
                prevention:  "Include field name, constraint violated, example valid input"
                test_for_it: "test_validation_errors_are_descriptive"
            },
            {
                failure:     "Builder API is verbose for simple cases"
                prevention:  "Provide convenience constructors for common patterns"
                test_for_it: "test_builder_has_convenience_methods"
            },
            {
                failure:     "Railway chains are hard to debug"
                prevention:  "Add tap() combinator for logging without changing flow"
                test_for_it: "test_tap_combinator_for_debugging"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "Validated type inner value modified via unsafe/raw pointer"
                prevention:  "No pub fields, no &mut access, only immutable as_str()"
                test_for_it: "test_validated_types_are_immutable"
            },
            {
                failure:     "Builder allows duplicate field setting without error"
                prevention:  "Track set fields, warn or error on double-set"
                test_for_it: "test_builder_detects_duplicate_fields"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_workspace_name_valid"
                given: "A valid workspace name string"
                when:  "WorkspaceName::new() is called"
                then: [
                    "Returns Ok(WorkspaceName)",
                    "as_str() returns the original value",
                    "Display formats correctly",
                ]
                real_input: """
                    let name = WorkspaceName::new("my-workspace-1")?;
                    """
                expected_output: """
                    assert_eq!(name.as_str(), "my-workspace-1");
                    assert_eq!(format!("{name}"), "my-workspace-1");
                    """
            },
            {
                name:  "test_railway_and_then_chain"
                given: "A chain of fallible operations"
                when:  "and_then is used to compose them"
                then: [
                    "All operations execute in sequence",
                    "Final Ok value is returned",
                    "No intermediate unwraps needed",
                ]
                real_input: """
                    fn parse_slug(s: &str) -> Result<Slug> { Slug::new(s) }
                    fn validate_length(slug: &Slug) -> Result<&Slug> {
                        if slug.as_str().len() > 5 { Ok(slug) }
                        else { Err(Error::invalid_slug("too short")) }
                    }

                    let result = parse_slug("my-task")
                        .and_then(|s| validate_length(&s).map(|_| s));
                    """
                expected_output: """
                    assert!(result.is_ok());
                    assert_eq!(result?.as_str(), "my-task");
                    """
            },
            {
                name:  "test_builder_collects_all_errors"
                given: "A builder with multiple invalid fields"
                when:  "build() is called"
                then: [
                    "Returns Err with all validation errors",
                    "Each error identifies the specific field",
                    "User sees complete list of fixes needed",
                ]
                real_input: """
                    let result = TaskBuilder::new()
                        .slug("")           // invalid: empty
                        .priority("P99")    // invalid: not P1-P3
                        .build();
                    """
                expected_output: """
                    let err = result.unwrap_err();
                    let msg = err.to_string();
                    assert!(msg.contains("slug"));
                    assert!(msg.contains("priority"));
                    """
            },
            {
                name:  "test_operation_id_uuid_format"
                given: "A valid UUID v4 string"
                when:  "OperationId::new() is called"
                then: [
                    "Returns Ok(OperationId)",
                    "Preserves lowercase format",
                    "Validates hyphen positions",
                ]
                real_input: """
                    let id = OperationId::new("550e8400-e29b-41d4-a716-446655440000")?;
                    """
                expected_output: """
                    assert_eq!(id.as_str(), "550e8400-e29b-41d4-a716-446655440000");
                    """
            },
            {
                name:  "test_change_id_jj_format"
                given: "A valid JJ change ID"
                when:  "ChangeId::new() is called"
                then: [
                    "Returns Ok(ChangeId)",
                    "Validates lowercase alphanumeric",
                    "Validates minimum length",
                ]
                real_input: """
                    let id = ChangeId::new("zzzzzzzzzzzzzzzzz")?;
                    """
                expected_output: """
                    assert_eq!(id.as_str().len(), 17);
                    assert!(id.as_str().chars().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit()));
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_workspace_name_empty_rejected"
                given: "An empty string"
                when:  "WorkspaceName::new() is called"
                then: [
                    "Returns Err(Error::InvalidWorkspaceName)",
                    "Error message explains the constraint",
                ]
                real_input: """
                    let result = WorkspaceName::new("");
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InvalidWorkspaceName { reason: "workspace name cannot be empty" })
                    """
            },
            {
                name:  "test_workspace_name_leading_hyphen_rejected"
                given: "A workspace name starting with hyphen"
                when:  "WorkspaceName::new() is called"
                then: [
                    "Returns Err with clear message",
                    "Explains no leading/trailing hyphens",
                ]
                real_input: """
                    let result = WorkspaceName::new("-bad-name");
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InvalidWorkspaceName { reason: "cannot start or end with hyphen" })
                    """
            },
            {
                name:  "test_operation_id_invalid_uuid_rejected"
                given: "A string that is not a valid UUID"
                when:  "OperationId::new() is called"
                then: [
                    "Returns Err(Error::InvalidOperationId)",
                    "Provides example of valid format",
                ]
                real_input: """
                    let result = OperationId::new("not-a-uuid");
                    """
                expected_output: null
                expected_error: """
                    Err(Error::InvalidOperationId { reason: "must be UUID v4 format (e.g., 550e8400-e29b-41d4-a716-446655440000)" })
                    """
            },
            {
                name:  "test_railway_short_circuits_on_error"
                given: "A chain where early operation fails"
                when:  "and_then chain executes"
                then: [
                    "Returns first error immediately",
                    "Subsequent operations not executed",
                ]
                real_input: """
                    let mut called = false;
                    let result: Result<i32> = Err(Error::invalid_slug("bad"))
                        .and_then(|x| { called = true; Ok(x + 1) });
                    """
                expected_output: """
                    assert!(result.is_err());
                    assert!(!called);  // Closure never ran
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_workspace_name_max_length"
                scenario: "Workspace name at exactly 64 characters"
                input:    "WorkspaceName::new(\"a\".repeat(64))"
                expected: "Ok(WorkspaceName) - boundary accepted"
            },
            {
                name:     "test_workspace_name_over_max_length"
                scenario: "Workspace name at 65 characters"
                input:    "WorkspaceName::new(\"a\".repeat(65))"
                expected: "Err - over limit rejected"
            },
            {
                name:     "test_change_id_minimum_length"
                scenario: "Change ID at exactly 16 characters"
                input:    "ChangeId::new(\"a\".repeat(16))"
                expected: "Ok(ChangeId) - minimum accepted"
            },
            {
                name:     "test_builder_empty_fields"
                scenario: "Builder with no fields set"
                input:    "TaskBuilder::new().build()"
                expected: "Err listing all required fields"
            },
            {
                name:     "test_tap_does_not_modify_value"
                scenario: "Using tap() for side effect"
                input:    "Ok(42).tap(|x| println!(\"value: {x}\"))"
                expected: "Returns Ok(42) unchanged"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in new modules"
                test:     "rg 'unwrap\\(|expect\\(' crates/factory-core/src/{fp,pure,builders}.rs returns empty"
            },
            {
                name:     "test_invariant_pure_no_io"
                verifies: "Pure module has no I/O imports"
                test:     "rg 'std::fs|std::net|tokio::' crates/factory-core/src/pure.rs returns empty"
            },
            {
                name:     "test_postcondition_result_types"
                verifies: "All constructors return Result"
                test:     "All ::new() functions have Result<Self, Error> return type"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_fp_workflow"
            description: "Complete workflow using Railway combinators and validated types"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/fp_e2e.rs"
                        content: """
                            use factory_core::domain::{WorkspaceName, OperationId, ChangeId};
                            use factory_core::fp::ResultExt;
                            use factory_core::builders::TaskBuilder;
                            use factory_core::error::Result;

                            #[test]
                            fn test_full_fp_workflow() -> Result<()> {
                                // Create validated types
                                let workspace = WorkspaceName::new("test-workspace")?;
                                let op_id = OperationId::generate()?;
                                let change_id = ChangeId::new("zzzzzzzzzzzzzzzzz")?;

                                // Chain operations with Railway pattern
                                let result = Ok(workspace)
                                    .and_then(|ws| validate_workspace(&ws))
                                    .map(|ws| format!("validated: {}", ws));

                                assert!(result.is_ok());
                                Ok(())
                            }

                            fn validate_workspace(ws: &WorkspaceName) -> Result<&WorkspaceName> {
                                if ws.as_str().contains("test") {
                                    Ok(ws)
                                } else {
                                    Err(factory_core::Error::invalid_slug("not a test workspace"))
                                }
                            }
                            """
                    },
                ]
                precondition_commands: [
                    "moon run factory-core:build",
                ]
            }

            execute: {
                command:    "moon run factory-core:test -- --test fp_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_fp_workflow ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/fp.rs"
                        contains: "pub trait ResultExt"
                    },
                    {
                        path:     "crates/factory-core/src/builders.rs"
                        contains: "pub struct TaskBuilder"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/fp_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_validated_type_serialization"
                description: "Verify validated types round-trip through JSON"
                steps: [
                    {action: "Create WorkspaceName from valid string", verify: "Ok returned"},
                    {action: "Serialize to JSON", verify: "Produces valid JSON string"},
                    {action: "Deserialize back", verify: "Produces equal WorkspaceName"},
                    {action: "Deserialize invalid JSON", verify: "Returns validation error"},
                ]
            },
            {
                name:        "e2e_builder_workflow"
                description: "Verify builder pattern for complex construction"
                steps: [
                    {action: "Create TaskBuilder", verify: "Empty builder returned"},
                    {action: "Set valid slug", verify: "Builder accepts value"},
                    {action: "Set valid priority", verify: "Builder accepts value"},
                    {action: "Call build()", verify: "Returns Ok(Task)"},
                    {action: "Access built task fields", verify: "All values correct"},
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
                task:      "Write test: test_workspace_name_valid"
                file:      "crates/factory-core/src/domain.rs"
                what:      "Test WorkspaceName validation with valid input"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_workspace_name_empty_rejected"
                file:      "crates/factory-core/src/domain.rs"
                what:      "Test WorkspaceName rejects empty string"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_operation_id_uuid_format"
                file:      "crates/factory-core/src/domain.rs"
                what:      "Test OperationId validates UUID format"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_change_id_jj_format"
                file:      "crates/factory-core/src/domain.rs"
                what:      "Test ChangeId validates JJ format"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_railway_and_then_chain"
                file:      "crates/factory-core/src/fp.rs"
                what:      "Test and_then chains operations correctly"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_builder_collects_all_errors"
                file:      "crates/factory-core/src/builders.rs"
                what:      "Test builder collects all validation errors"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add WorkspaceName validated type"
                file: "crates/factory-core/src/domain.rs"
                what: """
                    /// Opaque validated workspace name type.
                    /// Constraints: 1-64 chars, alphanumeric + hyphen, no leading/trailing hyphen.
                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
                    #[serde(try_from = "String", into = "String")]
                    pub struct WorkspaceName(String);

                    impl WorkspaceName {
                        pub fn new(s: impl Into<String>) -> Result<Self> { ... }
                        pub fn as_str(&self) -> &str { &self.0 }
                    }
                    """
                done_when:       "Test passes (green phase)"
                patterns_to_use: ["Follow exact Slug pattern", "TryFrom<String>", "Display impl"]
            },
            {
                task: "Add OperationId validated type"
                file: "crates/factory-core/src/domain.rs"
                what: """
                    /// Opaque validated UUID v4 operation identifier.
                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
                    #[serde(try_from = "String", into = "String")]
                    pub struct OperationId(String);

                    impl OperationId {
                        pub fn new(s: impl Into<String>) -> Result<Self> { ... }
                        pub fn generate() -> Result<Self> { ... }  // Uses uuid crate
                        pub fn as_str(&self) -> &str { &self.0 }
                    }
                    """
                done_when:       "Test passes (green phase)"
                patterns_to_use: ["UUID v4 validation regex", "uuid::Uuid::new_v4()"]
            },
            {
                task: "Add ChangeId validated type"
                file: "crates/factory-core/src/domain.rs"
                what: """
                    /// Opaque validated JJ change identifier.
                    /// Constraints: 16+ chars, lowercase alphanumeric only.
                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
                    #[serde(try_from = "String", into = "String")]
                    pub struct ChangeId(String);

                    impl ChangeId {
                        pub fn new(s: impl Into<String>) -> Result<Self> { ... }
                        pub fn as_str(&self) -> &str { &self.0 }
                    }
                    """
                done_when:       "Test passes (green phase)"
                patterns_to_use: ["Lowercase alphanumeric check", "Minimum length validation"]
            },
            {
                task: "Create fp.rs with Railway combinators"
                file: "crates/factory-core/src/fp.rs"
                what: """
                    //! Railway-Oriented Programming combinators.
                    //! Extension trait for Result that adds FP-style chaining.

                    use crate::error::Result;

                    /// Extension trait for Railway-Oriented Programming.
                    pub trait ResultExt<T, E> {
                        /// Tap into the value without consuming it (for logging/debugging).
                        fn tap<F>(self, f: F) -> Self where F: FnOnce(&T);

                        /// Tap into the error without consuming it.
                        fn tap_err<F>(self, f: F) -> Self where F: FnOnce(&E);

                        /// Convert error to a different type with context.
                        fn with_context<F, C>(self, f: F) -> Result<T>
                        where F: FnOnce() -> C, C: std::fmt::Display;
                    }

                    impl<T, E> ResultExt<T, E> for std::result::Result<T, E> {
                        fn tap<F>(self, f: F) -> Self where F: FnOnce(&T) {
                            if let Ok(ref val) = self { f(val); }
                            self
                        }
                        // ... other impls
                    }
                    """
                done_when:       "Tests pass (green phase)"
                patterns_to_use: ["Extension trait pattern", "Generic over E", "Preserve Result semantics"]
            },
            {
                task: "Create builders.rs with TaskBuilder"
                file: "crates/factory-core/src/builders.rs"
                what: """
                    //! Builder patterns for complex type construction.
                    //! Collects all validation errors instead of failing on first.

                    use crate::domain::{Task, Slug, Language, Priority};
                    use crate::error::{Error, Result};

                    #[derive(Default)]
                    pub struct TaskBuilder {
                        slug: Option<String>,
                        language: Option<String>,
                        priority: Option<String>,
                        errors: Vec<String>,
                    }

                    impl TaskBuilder {
                        pub fn new() -> Self { Self::default() }

                        pub fn slug(mut self, s: impl Into<String>) -> Self {
                            self.slug = Some(s.into());
                            self
                        }

                        pub fn build(mut self) -> Result<Task> {
                            // Validate all fields, collect all errors
                            let slug = self.slug.take()
                                .ok_or_else(|| "slug is required".to_string())
                                .and_then(|s| Slug::new(s).map_err(|e| e.to_string()));
                            // ... more validations

                            if !self.errors.is_empty() {
                                return Err(Error::ValidationErrors { errors: self.errors });
                            }
                            // Build Task from validated parts
                        }
                    }
                    """
                done_when:       "Tests pass (green phase)"
                patterns_to_use: ["Collect all errors", "Default::default()", "Fluent API"]
            },
            {
                task: "Create pure.rs module"
                file: "crates/factory-core/src/pure.rs"
                what: """
                    //! Pure functions - zero I/O, fully deterministic.
                    //! This module must NEVER import std::fs, std::net, tokio, or any I/O.

                    use crate::domain::{Stage, Priority};

                    /// Sort stages by retry count (pure, no I/O).
                    pub fn sort_stages_by_retries(stages: &mut [Stage]) {
                        stages.sort_by_key(|s| s.retries);
                    }

                    /// Compare priorities (pure function).
                    pub const fn priority_ord(a: Priority, b: Priority) -> std::cmp::Ordering {
                        // Match on variants...
                    }

                    /// Filter stages matching predicate (pure).
                    pub fn filter_stages<F>(stages: &[Stage], predicate: F) -> Vec<&Stage>
                    where F: Fn(&Stage) -> bool {
                        stages.iter().filter(|s| predicate(s)).collect()
                    }
                    """
                done_when:       "Module compiles with zero I/O imports"
                patterns_to_use: ["const fn where possible", "Generic predicates", "No allocations in hot paths"]
            },
        ]

        phase_3_integration: [
            {
                task:      "Add error variants to error.rs"
                file:      "crates/factory-core/src/error.rs"
                what:      "InvalidWorkspaceName, InvalidOperationId, InvalidChangeId, ValidationErrors"
                done_when: "Error variants compile and are documented"
            },
            {
                task:      "Export modules from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod fp; pub mod pure; pub mod builders; + re-exports"
                done_when: "External crates can import factory_core::fp"
            },
            {
                task:      "Add uuid dependency"
                file:      "crates/factory-core/Cargo.toml"
                what:      "uuid = { version = \"1\", features = [\"v4\"] }"
                done_when: "cargo check succeeds"
            },
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
                commands:  ["moon run :ci"]
                expected:  "exit code 0"
            },
            {
                task:      "Verify no unwraps in new modules"
                done_when: "rg finds no unwrap/expect"
                commands:  ["rg 'unwrap\\(|expect\\(' crates/factory-core/src/{fp,pure,builders}.rs"]
                expected:  "no output (empty)"
            },
            {
                task:      "Verify pure.rs has no I/O"
                done_when: "rg finds no I/O imports"
                commands:  ["rg 'std::fs|std::net|tokio::' crates/factory-core/src/pure.rs"]
                expected:  "no output (empty)"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:       "Compilation error: 'uuid' not found"
                likely_cause:  "uuid dependency not added to Cargo.toml"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "uuid dependency exists with v4 feature"
                    },
                ]
                fix_pattern: "Add uuid = { version = \"1\", features = [\"v4\"] } to dependencies"
            },
            {
                symptom:       "Test fails: 'expected Err but got Ok'"
                likely_cause:  "Validation logic not rejecting invalid input"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/domain.rs"
                        function:      "WorkspaceName::new()"
                        what_to_check: "Are all validation conditions checked?"
                    },
                ]
                fix_pattern: "Add missing validation: length, characters, leading/trailing hyphen"
            },
            {
                symptom:       "Builder returns error for valid input"
                likely_cause:  "Validation too strict or wrong default"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/builders.rs"
                        function:      "TaskBuilder::build()"
                        what_to_check: "Are optional fields handled correctly?"
                    },
                ]
                fix_pattern: "Use Option::unwrap_or_default() for optional fields with defaults"
            },
            {
                symptom:       "Pure module causes clippy warning about unused I/O"
                likely_cause:  "Accidentally imported std::fs in pure.rs"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/pure.rs"
                        what_to_check: "Check all 'use' statements at top of file"
                    },
                ]
                fix_pattern: "Remove I/O imports, move I/O code to impure module"
            },
        ]

        debugging_commands: [
            {
                scenario: "When validation rejects valid input"
                run:      "cargo test -- --nocapture test_workspace_name"
                look_for: "Actual error message vs expected format"
            },
            {
                scenario: "When builder collects wrong errors"
                run:      "RUST_BACKTRACE=1 cargo test test_builder"
                look_for: "Which validation step is failing"
            },
            {
                scenario: "When Railway chain short-circuits unexpectedly"
                run:      "Add .tap(|x| eprintln!(\"value: {x:?}\")) to chain"
                look_for: "Which step produces None/Err"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_workspace_name_valid passes",
            "[ ] test_workspace_name_empty_rejected passes",
            "[ ] test_workspace_name_leading_hyphen_rejected passes",
            "[ ] test_operation_id_uuid_format passes",
            "[ ] test_operation_id_invalid_uuid_rejected passes",
            "[ ] test_change_id_jj_format passes",
            "[ ] test_railway_and_then_chain passes",
            "[ ] test_railway_short_circuits_on_error passes",
            "[ ] test_builder_collects_all_errors passes",
            "[ ] test_tap_does_not_modify_value passes",
        ]

        code: [
            "[ ] WorkspaceName validated type implemented",
            "[ ] OperationId validated type implemented",
            "[ ] ChangeId validated type implemented",
            "[ ] fp.rs with ResultExt trait created",
            "[ ] builders.rs with TaskBuilder created",
            "[ ] pure.rs with zero I/O imports created",
            "[ ] All error variants added to error.rs",
            "[ ] Zero unwrap() or expect() calls",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]

        documentation: [
            "[ ] Module-level docs in fp.rs explaining Railway pattern",
            "[ ] Module-level docs in builders.rs explaining error collection",
            "[ ] Module-level docs in pure.rs explaining I/O prohibition",
            "[ ] Example usage in doc comments for all public types",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/domain.rs"
                relevance: "Existing validated types (Slug, GitHash) - follow same pattern"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - add new validation error variants"
            },
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports - add fp, pure, builders"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - add uuid crate"
            },
        ]

        external_references: [
            "https://fsharpforfunandprofit.com/rop/ - Railway Oriented Programming",
            "https://doc.rust-lang.org/std/result/enum.Result.html - Rust Result type",
            "https://docs.rs/uuid/latest/uuid/ - UUID crate docs",
            "https://rust-unofficial.github.io/patterns/patterns/creational/builder.html - Builder pattern",
        ]

        codebase_patterns: [
            {
                pattern:          "Validated Newtype Pattern"
                example_location: "crates/factory-core/src/domain.rs:Slug"
                how_to_apply:     "new() -> Result, as_str() -> &str, TryFrom<String>, Display"
            },
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs:filter_stages"
                how_to_apply:     "Return Result<T, Error>, use ? operator, chain with and_then/map"
            },
            {
                pattern:          "Error Factory Methods"
                example_location: "crates/factory-core/src/error.rs:Error::invalid_slug"
                how_to_apply:     "Provide constructor methods for common error patterns"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Follow exact pattern of existing Slug type for new validated types",
            "Use extension trait pattern for Railway combinators (don't modify Result)",
            "Collect ALL validation errors in builder, not just first",
            "Mark pure.rs functions as const fn where possible",
            "Use #[must_use] on all validated type constructors",
            "Add serde try_from/into for JSON round-trip support",
            "Use uuid::Uuid::new_v4() for OperationId::generate()",
            "Document with examples showing Railway chains",
        ]

        do_not: [
            "Do NOT use unwrap() or expect() - ever",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT import std::fs, std::net, or tokio in pure.rs",
            "Do NOT expose inner value of validated types via pub field",
            "Do NOT use regex for simple validations (char checks are faster)",
        ]

        code_patterns: [
            {
                name:     "Validated Newtype"
                use_when: "Creating a domain type with constraints"
                example:  """
                    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
                    #[serde(try_from = "String", into = "String")]
                    pub struct WorkspaceName(String);

                    impl WorkspaceName {
                        const MAX_LEN: usize = 64;

                        pub fn new(s: impl Into<String>) -> Result<Self> {
                            let s = s.into();
                            if s.is_empty() {
                                return Err(Error::InvalidWorkspaceName {
                                    reason: "cannot be empty".into()
                                });
                            }
                            if s.len() > Self::MAX_LEN {
                                return Err(Error::InvalidWorkspaceName {
                                    reason: format!("max {} chars", Self::MAX_LEN)
                                });
                            }
                            if s.starts_with('-') || s.ends_with('-') {
                                return Err(Error::InvalidWorkspaceName {
                                    reason: "cannot start/end with hyphen".into()
                                });
                            }
                            if !s.chars().all(|c| c.is_ascii_alphanumeric() || c == '-') {
                                return Err(Error::InvalidWorkspaceName {
                                    reason: "only alphanumeric and hyphen allowed".into()
                                });
                            }
                            Ok(Self(s))
                        }

                        #[must_use]
                        pub fn as_str(&self) -> &str { &self.0 }
                    }

                    impl TryFrom<String> for WorkspaceName {
                        type Error = Error;
                        fn try_from(s: String) -> Result<Self> { Self::new(s) }
                    }

                    impl From<WorkspaceName> for String {
                        fn from(name: WorkspaceName) -> Self { name.0 }
                    }

                    impl std::fmt::Display for WorkspaceName {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            write!(f, "{}", self.0)
                        }
                    }
                    """
            },
            {
                name:     "Railway Extension Trait"
                use_when: "Adding FP combinators to Result"
                example:  """
                    pub trait ResultExt<T, E> {
                        /// Tap into Ok value for side effects (logging).
                        fn tap<F: FnOnce(&T)>(self, f: F) -> Self;

                        /// Tap into Err for side effects.
                        fn tap_err<F: FnOnce(&E)>(self, f: F) -> Self;
                    }

                    impl<T, E> ResultExt<T, E> for std::result::Result<T, E> {
                        fn tap<F: FnOnce(&T)>(self, f: F) -> Self {
                            if let Ok(ref val) = self {
                                f(val);
                            }
                            self
                        }

                        fn tap_err<F: FnOnce(&E)>(self, f: F) -> Self {
                            if let Err(ref err) = self {
                                f(err);
                            }
                            self
                        }
                    }
                    """
            },
            {
                name:     "Error-Collecting Builder"
                use_when: "Building complex types with multiple validations"
                example:  """
                    #[derive(Default)]
                    pub struct TaskBuilder {
                        slug: Option<String>,
                        priority: Option<String>,
                        errors: Vec<String>,
                    }

                    impl TaskBuilder {
                        pub fn new() -> Self { Self::default() }

                        pub fn slug(mut self, s: impl Into<String>) -> Self {
                            self.slug = Some(s.into());
                            self
                        }

                        pub fn priority(mut self, p: impl Into<String>) -> Self {
                            self.priority = Some(p.into());
                            self
                        }

                        pub fn build(mut self) -> Result<Task> {
                            // Validate slug
                            let slug = match self.slug.take() {
                                None => {
                                    self.errors.push("slug is required".into());
                                    None
                                }
                                Some(s) => match Slug::new(&s) {
                                    Ok(slug) => Some(slug),
                                    Err(e) => {
                                        self.errors.push(format!("slug: {e}"));
                                        None
                                    }
                                }
                            };

                            // Validate priority
                            let priority = match self.priority.take() {
                                None => Priority::default(),
                                Some(p) => match Priority::parse(&p) {
                                    Ok(pri) => pri,
                                    Err(e) => {
                                        self.errors.push(format!("priority: {e}"));
                                        Priority::default()
                                    }
                                }
                            };

                            // Return all errors if any
                            if !self.errors.is_empty() {
                                return Err(Error::ValidationErrors {
                                    errors: self.errors
                                });
                            }

                            // Safe to unwrap - we checked for None above
                            let slug = slug.ok_or_else(|| Error::ValidationErrors {
                                errors: vec!["internal error".into()]
                            })?;

                            Ok(Task::new(slug, Language::Rust, PathBuf::new())
                                .with_priority(priority))
                        }
                    }
                    """
            },
        ]
    }
}
