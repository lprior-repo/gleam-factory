#!/usr/bin/env bash
# generate-bead-spec.sh - Generate a CUE specification for a bead
#
# Usage: ./generate-bead-spec.sh <bead-id>
#
# This script extracts bead info from beads.jsonl and generates a CUE spec template
# following the EARS + KIRK + ATDD methodology.

set -euo pipefail

BEADS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BEADS_FILE="${BEADS_DIR}/beads.jsonl"
SPECS_DIR="${BEADS_DIR}/specs"
TEMPLATE_FILE="${BEADS_DIR}/schemas/bead-template.cue"

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <bead-id>"
    echo "Example: $0 factory-abc"
    exit 1
fi

BEAD_ID="$1"
OUTPUT_FILE="${SPECS_DIR}/${BEAD_ID}.cue"

# Check if spec already exists
if [[ -f "$OUTPUT_FILE" ]]; then
    echo "Error: Spec already exists at $OUTPUT_FILE"
    echo "Delete it first if you want to regenerate."
    exit 1
fi

# Extract bead from beads.jsonl
BEAD_JSON=$(grep "\"id\":\"${BEAD_ID}\"" "$BEADS_FILE" 2>/dev/null || true)

if [[ -z "$BEAD_JSON" ]]; then
    echo "Error: Bead '$BEAD_ID' not found in $BEADS_FILE"
    exit 1
fi

# Parse bead fields using jq
TITLE=$(echo "$BEAD_JSON" | jq -r '.title // "Untitled"')
TYPE=$(echo "$BEAD_JSON" | jq -r '.issue_type // "feature"')
PRIORITY=$(echo "$BEAD_JSON" | jq -r '.priority // 2')
DESCRIPTION=$(echo "$BEAD_JSON" | jq -r '.description // ""' | head -c 500)

# Map issue_type to CUE type
case "$TYPE" in
    feature) CUE_TYPE="feature" ;;
    bug) CUE_TYPE="bug" ;;
    task) CUE_TYPE="task" ;;
    epic) CUE_TYPE="epic" ;;
    *) CUE_TYPE="task" ;;
esac

# Determine effort estimate based on priority
case "$PRIORITY" in
    0) EFFORT="4hr" ;;
    1) EFFORT="2hr" ;;
    2) EFFORT="1hr" ;;
    3) EFFORT="30min" ;;
    *) EFFORT="1hr" ;;
esac

echo "Generating CUE spec for: $BEAD_ID"
echo "  Title: $TITLE"
echo "  Type: $CUE_TYPE"
echo "  Priority: $PRIORITY"
echo "  Output: $OUTPUT_FILE"

# Create specs directory if needed
mkdir -p "$SPECS_DIR"

# Generate the CUE spec
cat > "$OUTPUT_FILE" << CUESPEC
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

"${BEAD_ID}": #ValidBead & {
    // ============================================================================
    // BEAD: ${BEAD_ID} - ${TITLE}
    // ============================================================================

    id:              "${BEAD_ID}"
    title:           "${TITLE}"
    type:            "${CUE_TYPE}"
    priority:        ${PRIORITY}
    effort_estimate: "${EFFORT}"
    labels:          ["P${PRIORITY}"]

    // ============================================================================
    // SECTION 1: EARS REQUIREMENTS
    // ============================================================================

    ears_requirements: {
        ubiquitous: [
            "THE SYSTEM SHALL propagate all errors via Result<T, Error> types",
            "THE SYSTEM SHALL never use unwrap(), expect(), panic!, todo!, or unimplemented!",
            // TODO: Add domain-specific ubiquitous requirements
        ]

        event_driven: [
            {
                trigger: "WHEN <event>"
                shall:   "THE SYSTEM SHALL <action>"
            },
            // TODO: Add event-driven requirements
        ]

        state_driven: [
            {
                state: "WHILE <condition>"
                shall: "THE SYSTEM SHALL <behavior>"
            },
            // TODO: Add state-driven requirements
        ]

        unwanted: [
            {
                condition: "IF <bad situation>"
                shall_not: "THE SYSTEM SHALL NOT <forbidden action>"
                because:   "<rationale>"
            },
            // TODO: Add unwanted behavior requirements
        ]

        complex: [
            {
                state:   "WHILE <condition>"
                trigger: "WHEN <event>"
                shall:   "THE SYSTEM SHALL <action>"
            },
            // TODO: Add complex state+trigger requirements
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
                    field:           "input_field"
                    type:            "Type"
                    constraints:     "Description of valid values"
                    example_valid:   "good_value"
                    example_invalid: "bad_value"
                },
                // TODO: Add required inputs
            ]
            system_state: [
                "Tokio runtime initialized",
                "factory-core crate exists",
                // TODO: Add system state requirements
            ]
        }

        postconditions: {
            state_changes: [
                // TODO: Add state changes
            ]
            return_guarantees: [
                {
                    field:     "return_value"
                    guarantee: "Returns Result<T, Error>"
                },
                // TODO: Add return guarantees
            ]
            side_effects: [
                // TODO: Add side effects
            ]
        }

        invariants: [
            "All public APIs return Result<T, Error>",
            "No unwrap(), expect(), panic!, todo!, unimplemented! in codebase",
            // TODO: Add domain-specific invariants
        ]
    }

    // ============================================================================
    // SECTION 3: INVERSION ANALYSIS
    // ============================================================================

    inversions: {
        integration_failures: [
            {
                failure:     "Integration failure scenario"
                prevention:  "How to prevent it"
                test_for_it: "test_integration_scenario"
            },
            // TODO: Add integration failure scenarios
        ]

        usability_failures: [
            {
                failure:     "Usability problem"
                prevention:  "How to prevent it"
                test_for_it: "test_usability_scenario"
            },
            // TODO: Add usability failure scenarios
        ]

        data_integrity_failures: [
            {
                failure:     "Data corruption scenario"
                prevention:  "How to prevent it"
                test_for_it: "test_data_integrity"
            },
            // TODO: Add data integrity failure scenarios
        ]
    }

    // ============================================================================
    // SECTION 4: ACCEPTANCE TESTS (ATDD)
    // ============================================================================

    acceptance_tests: {
        happy_paths: [
            {
                name:  "test_happy_path_scenario"
                given: "Initial state"
                when:  "Action is performed"
                then: [
                    "Expected outcome 1",
                    "Expected outcome 2",
                ]
                real_input: """
                    // REAL Rust code - not pseudocode
                    let result = function_under_test(input).await?;
                """
                expected_output: """
                    assert!(result.is_ok());
                """
            },
            // TODO: Add happy path tests
        ]

        error_paths: [
            {
                name:  "test_error_scenario"
                given: "Error-inducing state"
                when:  "Action is performed"
                then: [
                    "Returns appropriate error",
                    "Does not panic",
                ]
                real_input: """
                    let result = function_under_test(bad_input).await;
                """
                expected_output: null
                expected_error: "Err(Error::SpecificVariant)"
            },
            // TODO: Add error path tests
        ]

        edge_cases: [
            {
                name:     "test_edge_case"
                scenario: "Edge case description"
                input:    "Edge case input"
                expected: "Expected behavior"
            },
            // TODO: Add edge cases
        ]

        contract_tests: [
            {
                name:     "test_invariant_no_unwrap"
                verifies: "No unwrap() or expect() in module"
                test:     "grep -r 'unwrap()\\|expect(' module.rs returns empty"
            },
            // TODO: Add contract tests
        ]
    }

    // ============================================================================
    // SECTION 5: E2E TESTS
    // ============================================================================

    e2e_tests: {
        pipeline_test: {
            name:        "test_full_lifecycle"
            description: "Complete lifecycle test"
            setup: {
                files_to_create: [
                    {
                        path: "crates/factory-core/tests/e2e_test.rs"
                        content: """
                            use factory_core::error::Result;

                            #[tokio::test]
                            async fn test_full_lifecycle() -> Result<()> {
                                // TODO: Implement E2E test
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
                command:    "moon run factory-core:test -- --test e2e_test"
                timeout_ms: 60000
            }
            verify: {
                exit_code: 0
                stdout_contains: [
                    "test test_full_lifecycle ... ok",
                ]
                files_created: []
            }
            cleanup: {
                commands: [
                    "rm crates/factory-core/tests/e2e_test.rs",
                ]
            }
        }

        e2e_scenarios: [
            {
                name:        "e2e_scenario_name"
                description: "Scenario description"
                steps: [
                    {
                        action: "Step 1 action"
                        verify: "Step 1 verification"
                    },
                    {
                        action: "Step 2 action"
                        verify: "Step 2 verification"
                    },
                ]
            },
            // TODO: Add E2E scenarios
        ]
    }

    // ============================================================================
    // SECTION 6: IMPLEMENTATION TASKS
    // ============================================================================

    implementation_tasks: {
        phase_1_tests_first: [
            {
                task:      "Write test: test_happy_path_scenario"
                file:      "crates/factory-core/src/module.rs"
                what:      "Test description"
                done_when: "Test exists and FAILS (red phase)"
            },
            // TODO: Add test-first tasks
        ]

        phase_2_implementation: [
            {
                task:      "Implement feature"
                file:      "crates/factory-core/src/module.rs"
                what:      "Implementation details"
                done_when: "Tests pass (green phase)"
                patterns_to_use: [
                    "Result<T, Error> for all fallible operations",
                    "? operator for error propagation",
                ]
            },
            // TODO: Add implementation tasks
        ]

        phase_3_integration: [
            {
                task:      "Export module from lib.rs"
                file:      "crates/factory-core/src/lib.rs"
                what:      "pub mod module_name;"
                done_when: "External crates can import"
            },
            // TODO: Add integration tasks
        ]

        phase_4_verification: [
            {
                task:      "Run moon run :ci"
                done_when: "All tests pass, no clippy warnings"
                commands: [
                    "moon run :ci",
                ]
                expected: "exit code 0"
            },
            {
                task:      "Verify no unwraps"
                done_when: "grep finds no unwrap/expect"
                commands: [
                    "rg 'unwrap\\(|expect\\(' crates/factory-core/src/module.rs",
                ]
                expected: "no output (empty)"
            },
            // TODO: Add verification tasks
        ]
    }

    // ============================================================================
    // SECTION 7: FAILURE MODES
    // ============================================================================

    failure_modes: {
        failure_modes: [
            {
                symptom:      "Symptom description"
                likely_cause: "Likely cause"
                where_to_look: [
                    {
                        file:          "crates/factory-core/src/module.rs"
                        function:      "function_name()"
                        what_to_check: "What to look for"
                    },
                ]
                fix_pattern: "How to fix it"
            },
            // TODO: Add failure modes
        ]

        debugging_commands: [
            {
                scenario: "When X happens"
                run:      "RUST_LOG=debug cargo test"
                look_for: "What to look for in output"
            },
            // TODO: Add debugging commands
        ]
    }

    // ============================================================================
    // SECTION 8: COMPLETION CHECKLIST
    // ============================================================================

    completion_checklist: {
        tests: [
            "[ ] All acceptance tests written and passing",
            "[ ] All error path tests written and passing",
            "[ ] E2E pipeline test passing",
            "[ ] No mocks or fake data in any test",
            // TODO: Add specific test checklist items
        ]
        code: [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            "[ ] All preconditions validated",
            "[ ] All postconditions guaranteed",
            // TODO: Add specific code checklist items
        ]
        ci: [
            "[ ] moon run :ci passes",
            "[ ] No clippy warnings",
            "[ ] No compiler warnings",
            "[ ] cargo doc generates without errors",
        ]
        documentation: [
            "[ ] Module-level docs written",
            "[ ] Example usage in doc comments",
            // TODO: Add documentation checklist items
        ]
    }

    // ============================================================================
    // SECTION 9: CONTEXT
    // ============================================================================

    context: {
        related_files: [
            {
                path:      "crates/factory-core/src/lib.rs"
                relevance: "Module exports"
            },
            {
                path:      "crates/factory-core/src/error.rs"
                relevance: "Error types"
            },
            // TODO: Add related files
        ]

        external_references: [
            // TODO: Add external references (docs, tutorials, etc.)
        ]

        codebase_patterns: [
            {
                pattern:          "Railway-Oriented Error Handling"
                example_location: "crates/factory-core/src/domain.rs"
                how_to_apply:     "Return Result<T, Error>, use ? operator"
            },
            // TODO: Add codebase patterns
        ]
    }

    // ============================================================================
    // SECTION 10: AI HINTS
    // ============================================================================

    ai_hints: {
        do: [
            "Use Result<T, Error> for all fallible operations",
            "Use ? operator for error propagation",
            "Document with examples in doc comments",
            "Use tracing for structured logging",
            // TODO: Add specific do hints
        ]

        do_not: [
            "Do NOT use unwrap() or expect()",
            "Do NOT use panic!, todo!, or unimplemented!",
            "Do NOT modify clippy configuration",
            "Do NOT use raw cargo commands (use moon)",
            // TODO: Add specific do not hints
        ]

        code_patterns: [
            {
                name:     "Error Propagation"
                use_when: "Any fallible operation"
                example: """
                    fn operation() -> Result<Output, Error> {
                        let input = get_input()?;
                        let result = process(input)?;
                        Ok(result)
                    }
                """
            },
            // TODO: Add code patterns
        ]
    }
}
CUESPEC

echo ""
echo "Generated: $OUTPUT_FILE"
echo ""
echo "Next steps:"
echo "  1. Edit the file to fill in TODO sections"
echo "  2. Validate with: cue vet $OUTPUT_FILE"
echo "  3. Export to YAML: cue export $OUTPUT_FILE --out yaml"
echo "  4. Update bead description with the exported YAML"
