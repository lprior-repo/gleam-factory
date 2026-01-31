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

"factory-duf": #ValidBead & {
    // ============================================================================
    // BEAD: factory-duf - Fix unique ID collision risk from clock fallback
    // ============================================================================

    id:              "factory-duf"
    title:           "Bug: Fix worktree unique ID collision risk from clock fallback"
    type:            "bug"
    priority:        1
    effort_estimate: "1hr"
    labels:          ["bug", "worktree", "security", "P1"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL generate unique worktree IDs using cryptographically secure randomness",
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            "THE SYSTEM SHALL never silently fall back to default values on failure",
        ]

        event_driven: [
            {
                trigger: "WHEN generate_unique_id() is called"
                shall:   "THE SYSTEM SHALL return a cryptographically random ID with sufficient entropy"
            },
            {
                trigger: "WHEN the entropy source fails"
                shall:   "THE SYSTEM SHALL return an explicit error, never a default value"
            },
            {
                trigger: "WHEN a worktree is created"
                shall:   "THE SYSTEM SHALL guarantee the worktree ID is unique across all possible concurrent invocations"
            },
        ]

        state_driven: [
            {
                state: "WHILE the system is operational"
                shall: "THE SYSTEM SHALL never produce duplicate worktree IDs even under clock skew or time rollback"
            },
        ]

        unwanted: [
            {
                condition: "IF the system clock returns an error or invalid time"
                shall_not: "THE SYSTEM SHALL NOT use Duration::default() (zero) as a fallback"
                because:   "Zero duration causes all concurrent worktrees to have the same ID, causing collisions"
            },
            {
                condition: "IF two worktrees are created simultaneously"
                shall_not: "THE SYSTEM SHALL NOT generate identical IDs"
                because:   "Duplicate IDs cause workspace collisions and data corruption"
            },
            {
                condition: "IF the system time is set before UNIX_EPOCH"
                shall_not: "THE SYSTEM SHALL NOT silently continue with a broken ID"
                because:   "Silent failures hide bugs and cause unpredictable behavior"
            },
        ]

        complex: [
            {
                state:   "WHILE running in a containerized environment"
                trigger: "WHEN system clock is unreliable or virtualized"
                shall:   "THE SYSTEM SHALL still generate unique IDs independent of wall clock"
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
                    field:           "None"
                    type:            "Unit"
                    constraints:     "Function takes no arguments"
                    example_valid:   "generate_unique_id()"
                    example_invalid: "N/A"
                },
            ]
            system_state: [
                "Operating system provides /dev/urandom or equivalent",
                "getrandom syscall or rand crate available",
            ]
        }

        postconditions: {
            state_changes: [
                "No state changes - pure function returning random ID",
            ]
            return_guarantees: [
                {
                    field:     "Result<String, Error>"
                    guarantee: "Returns Ok(id) with 64 bits of entropy encoded as hex"
                },
                {
                    field:     "ID format"
                    guarantee: "16 character lowercase hexadecimal string"
                },
                {
                    field:     "Uniqueness"
                    guarantee: "Probability of collision < 1 in 2^64 (negligible)"
                },
            ]
            side_effects: [
                "Consumes entropy from system CSPRNG",
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            "ID generation is independent of system clock",
            "Failed ID generation causes worktree creation to fail explicitly",
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "rand crate not available or fails to initialize"
                prevention:  "Use getrandom directly as it's more reliable, or handle rand errors"
                test_for_it: "test_unique_id_generation_does_not_panic"
            },
            {
                failure:     "WASM or no_std environment without randomness"
                prevention:  "Document platform requirements, fail compilation on unsupported targets"
                test_for_it: "N/A - compile-time check"
            },
        ]

        usability_failures: [
            {
                failure:     "Error message doesn't explain why ID generation failed"
                prevention:  "Include source error in Error::IdGenerationFailed variant"
                test_for_it: "test_error_message_includes_cause"
            },
        ]

        data_integrity_failures: [
            {
                failure:     "ID collision causes worktree overwrite"
                prevention:  "Use 64+ bits of entropy, verify directory doesn't exist before creation"
                test_for_it: "test_no_collisions_in_bulk_generation"
            },
            {
                failure:     "Predictable IDs allow attacker to pre-create directories"
                prevention:  "Use cryptographically secure randomness, not time-based"
                test_for_it: "test_ids_are_unpredictable"
            },
        ]
    }

    // ============================================================================
    // SECTION 4: ATDD ACCEPTANCE TESTS
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_generate_unique_id_returns_valid_hex"
                given: "A functioning system with entropy source"
                when:  "generate_unique_id() is called"
                then: [
                    "Returns Ok(id)",
                    "ID is 16 characters long",
                    "ID contains only lowercase hex characters [0-9a-f]",
                ]
                real_input: """
                    let id = generate_unique_id()?;
                    """
                expected_output: """
                    assert_eq!(id.len(), 16);
                    assert!(id.chars().all(|c| c.is_ascii_hexdigit() && !c.is_uppercase()));
                    """
            },
            {
                name:  "test_generate_unique_id_produces_different_values"
                given: "Multiple calls to generate_unique_id"
                when:  "Called 1000 times in sequence"
                then: [
                    "All IDs are unique",
                    "No duplicates in the set",
                ]
                real_input: """
                    let ids: HashSet<String> = (0..1000)
                        .map(|_| generate_unique_id())
                        .collect::<Result<_, _>>()?;
                    """
                expected_output: """
                    assert_eq!(ids.len(), 1000);  // All unique
                    """
            },
            {
                name:  "test_worktree_creation_uses_unique_id"
                given: "A repository root path"
                when:  "create_worktree is called"
                then: [
                    "Worktree name includes random suffix",
                    "Path includes the unique ID",
                ]
                real_input: """
                    let wt = create_worktree("my-task", Language::Rust, repo_root)?;
                    """
                expected_output: """
                    assert!(wt.path.to_string_lossy().contains("my-task-"));
                    // Suffix is random, not predictable timestamp
                    """
            },
        ]

        error_paths: [
            {
                name:  "test_propagates_error_on_entropy_failure"
                given: "A mocked/failing entropy source"
                when:  "generate_unique_id() cannot get random bytes"
                then: [
                    "Returns Err(Error::IdGenerationFailed)",
                    "Does not panic",
                    "Does not return a default value",
                ]
                real_input: """
                    // With mocked getrandom that fails
                    let result = generate_unique_id();
                    """
                expected_output: null
                expected_error: """
                    Err(Error::IdGenerationFailed { .. })
                    """
            },
        ]

        edge_cases: [
            {
                name:     "test_concurrent_id_generation"
                scenario: "100 threads generating IDs simultaneously"
                input:    "Spawn 100 threads, each generates 100 IDs"
                expected: "All 10,000 IDs are unique"
            },
            {
                name:     "test_id_format_is_stable"
                scenario: "ID format doesn't change unexpectedly"
                input:    "Generate ID and check format"
                expected: "Always 16 lowercase hex chars"
            },
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap_or_default"
                verifies: "No unwrap_or_default() on time operations"
                test:     "grep -r 'unwrap_or_default' crates/factory-core/src/worktree.rs returns empty"
            },
            {
                name:     "test_no_time_based_id"
                verifies: "ID generation doesn't use SystemTime"
                test:     "generate_unique_id function doesn't import or use SystemTime"
            },
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_worktree_id_uniqueness_e2e"
            description: "Verify worktree IDs are unique across rapid sequential creation"

            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/worktree_uniqueness_e2e.rs"
                        content: """
                            use factory_core::worktree::generate_unique_id;
                            use factory_core::error::Result;
                            use std::collections::HashSet;

                            #[test]
                            fn test_bulk_id_uniqueness() -> Result<()> {
                                let ids: HashSet<String> = (0..10_000)
                                    .map(|_| generate_unique_id())
                                    .collect::<Result<_, _>>()?;
                                assert_eq!(ids.len(), 10_000, "All IDs must be unique");
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
                command:    "moon run factory-core:test -- --test worktree_uniqueness_e2e"
                timeout_ms: 30000
            }

            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_bulk_id_uniqueness ... ok",
                ]
                files_created: [
                    {
                        path:     "crates/factory-core/src/worktree.rs"
                        contains: "getrandom"
                    },
                ]
            }

            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/worktree_uniqueness_e2e.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_no_collision_under_load"
                description: "Verify no ID collisions when creating many worktrees rapidly"
                steps: [
                    {action: "Generate 10,000 unique IDs", verify: "All IDs distinct"},
                    {action: "Check ID format", verify: "All are 16-char hex"},
                    {action: "Verify entropy source used", verify: "No SystemTime in generate_unique_id"},
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
                task:      "Write test: test_generate_unique_id_returns_valid_hex"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that generate_unique_id returns 16 lowercase hex chars"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_no_collisions_in_bulk_generation"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that 10,000 IDs are all unique"
                done_when: "Test exists and FAILS (red phase)"
            },
            {
                task:      "Write test: test_generate_unique_id_returns_result"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Test that function returns Result, not raw String"
                done_when: "Test exists and FAILS (red phase)"
            },
        ]

        phase_2_implementation: [
            {
                task: "Add getrandom dependency to Cargo.toml"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dependencies]
                    getrandom = "0.2"
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: ["workspace dependencies for version management"]
            },
            {
                task: "Add IdGenerationFailed error variant"
                file: "crates/factory-core/src/error.rs"
                what: """
                    #[error("Failed to generate unique ID: {source}")]
                    IdGenerationFailed {
                        #[source]
                        source: getrandom::Error,
                    },
                    """
                done_when:     "Error variant compiles"
                patterns_to_use: ["thiserror with #[source] for error chaining"]
            },
            {
                task: "Rewrite generate_unique_id to use getrandom"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    /// Generate a unique ID for worktree names using cryptographic randomness.
                    fn generate_unique_id() -> Result<String> {
                        let mut bytes = [0u8; 8];
                        getrandom::getrandom(&mut bytes)
                            .map_err(|source| Error::IdGenerationFailed { source })?;
                        Ok(hex::encode(bytes))
                    }
                    """
                done_when:     "Function compiles and returns Result<String>"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "? operator for error propagation",
                    "No unwrap, expect, or default fallbacks",
                ]
            },
            {
                task: "Update create_worktree to handle Result"
                file: "crates/factory-core/src/worktree.rs"
                what: """
                    let unique_id = generate_unique_id()?;  // Propagate error
                    """
                done_when:     "create_worktree propagates ID generation errors"
                patterns_to_use: ["Error propagation with ?"]
            },
            {
                task: "Add hex dependency for encoding"
                file: "crates/factory-core/Cargo.toml"
                what: """
                    [dependencies]
                    hex = "0.4"
                    """
                done_when:     "cargo check succeeds"
                patterns_to_use: []
            },
        ]

        phase_3_integration: [
            {
                task:      "Update tests to use new Result-returning function"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Update test_generate_unique_id to handle Result"
                done_when: "All tests compile and pass"
            },
            {
                task:      "Remove SystemTime import from generate_unique_id"
                file:      "crates/factory-core/src/worktree.rs"
                what:      "Remove unused std::time::{SystemTime, UNIX_EPOCH} import"
                done_when: "No time-related imports in generate_unique_id"
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
                task:     "Verify no unwrap_or_default"
                done_when: "grep finds no unwrap_or_default in worktree.rs"
                commands: ["rg 'unwrap_or_default' crates/factory-core/src/worktree.rs"]
                expected: "no output (empty)"
            },
            {
                task:     "Verify no SystemTime in generate_unique_id"
                done_when: "Function doesn't use SystemTime"
                commands: ["rg 'SystemTime' crates/factory-core/src/worktree.rs"]
                expected: "no output (empty)"
            },
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Compilation error: 'getrandom' not found"
                likely_cause: "Dependency not added to Cargo.toml"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "getrandom dependency exists"
                    },
                ]
                fix_pattern: "Add getrandom = \"0.2\" to dependencies"
            },
            {
                symptom:      "Compilation error: 'hex' not found"
                likely_cause: "Dependency not added to Cargo.toml"
                where_to_look: [
                    {
                        file:          "crates/factory-core/Cargo.toml"
                        what_to_check: "hex dependency exists"
                    },
                ]
                fix_pattern: "Add hex = \"0.4\" to dependencies"
            },
            {
                symptom:      "Type mismatch: expected String, found Result"
                likely_cause: "Call sites not updated to handle Result"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/worktree.rs"
                        function:      "create_worktree()"
                        what_to_check: "Is generate_unique_id() called with ?"
                    },
                ]
                fix_pattern: "Add ? after generate_unique_id() call"
            },
            {
                symptom:      "Runtime error on exotic platforms (WASM, embedded)"
                likely_cause: "getrandom doesn't support target platform"
                where_to_look: [
                    {
                        file:          "Cargo.toml"
                        what_to_check: "getrandom features for target platform"
                    },
                ]
                fix_pattern: "Add platform-specific getrandom feature or document limitation"
            },
        ]

        debugging_commands: [
            {
                scenario: "When ID generation fails mysteriously"
                run:      "RUST_BACKTRACE=1 cargo test test_generate_unique_id"
                look_for: "getrandom error source in backtrace"
            },
            {
                scenario: "When checking for duplicate IDs in existing worktrees"
                run:      "ls -la .factory-workspaces/ | awk -F'-' '{print $NF}' | sort | uniq -d"
                look_for: "Any duplicate suffixes indicate collision"
            },
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] test_generate_unique_id_returns_valid_hex passes",
            "[ ] test_no_collisions_in_bulk_generation passes (10,000 unique IDs)",
            "[ ] test_generate_unique_id_returns_result passes",
            "[ ] test_concurrent_id_generation passes (if implemented)",
            "[ ] No mocks or fake data in any test",
        ]

        code: [
            "[ ] generate_unique_id() returns Result<String, Error>",
            "[ ] Uses getrandom for cryptographic randomness",
            "[ ] No SystemTime or UNIX_EPOCH in generate_unique_id",
            "[ ] No unwrap_or_default() anywhere in worktree.rs",
            "[ ] IdGenerationFailed error variant added to error.rs",
            "[ ] create_worktree propagates ID generation errors with ?",
            "[ ] hex crate used for encoding bytes to string",
        ]

        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
        ]

        documentation: [
            "[ ] generate_unique_id doc comment updated",
            "[ ] Error variant documented",
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/worktree.rs"
                relevance: "Contains the buggy generate_unique_id function at line 247-256"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types - must add IdGenerationFailed variant"
            },
            {
                path:      "crates/factory-core/Cargo.toml"
                relevance: "Dependencies - must add getrandom and hex"
            },
        ]

        external_references: [
            "https://docs.rs/getrandom - Official getrandom docs",
            "https://rust-random.github.io/book/ - Rust random number generation",
            "https://doc.rust-lang.org/std/time/struct.SystemTime.html#method.duration_since - Why duration_since can fail",
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator, never use unwrap_or_default"
            },
            {
                pattern:          "Error Variant with Source"
                example_location: "crates/factory-core/src/error.rs"
                how_to_apply:     "Use #[source] attribute for error chaining"
            },
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use getrandom crate - it's the standard for cryptographic randomness in Rust",
            "Use hex crate to encode random bytes as hex string",
            "Return Result<String, Error> from generate_unique_id",
            "Use ? operator to propagate errors to create_worktree",
            "Add IdGenerationFailed error variant with #[source] for the getrandom::Error",
            "Use 8 bytes (64 bits) of entropy - sufficient for uniqueness",
            "Document why time-based IDs are problematic in the function doc comment",
        ]

        do_not: [
            "Do NOT use unwrap(), expect(), or unwrap_or_default()",
            "Do NOT use SystemTime for ID generation",
            "Do NOT silently fall back to default values",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            "Do NOT use rand crate when getrandom suffices (fewer dependencies)",
        ]

        code_patterns: [
            {
                name:     "Cryptographic ID Generation"
                use_when: "Generating unique identifiers"
                example:  """
                    fn generate_unique_id() -> Result<String> {
                        let mut bytes = [0u8; 8];
                        getrandom::getrandom(&mut bytes)
                            .map_err(|source| Error::IdGenerationFailed { source })?;
                        Ok(hex::encode(bytes))
                    }
                    """
            },
            {
                name:     "Error Variant with Source"
                use_when: "Wrapping external errors"
                example:  """
                    #[derive(Debug, thiserror::Error)]
                    pub enum Error {
                        #[error("Failed to generate unique ID: {source}")]
                        IdGenerationFailed {
                            #[source]
                            source: getrandom::Error,
                        },
                    }
                    """
            },
            {
                name:     "Error Propagation in Caller"
                use_when: "Calling fallible functions"
                example:  """
                    pub fn create_worktree(slug: &str, language: Language, repo_root: &Path) -> Result<Worktree> {
                        check_slug_not_exists(slug, repo_root)?;
                        let unique_id = generate_unique_id()?;  // Propagate error
                        let worktree_name = format!("{slug}-{unique_id}");
                        // ...
                    }
                    """
            },
        ]
    }
}
