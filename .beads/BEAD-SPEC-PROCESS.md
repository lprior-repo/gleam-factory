# Bead Specification Process

## Overview

Every bead in the Factory system MUST have a comprehensive CUE specification following the EARS + KIRK + ATDD methodology. This document describes the exact process used to create bead specifications.

## Required Sections (All 10 Mandatory)

Each bead CUE spec MUST include these 10 sections:

### 1. EARS Requirements
EARS (Easy Approach to Requirements Syntax) patterns:

```cue
ears_requirements: {
    // Always-true requirements
    ubiquitous: [
        "THE SYSTEM SHALL ...",
    ]

    // Trigger-based requirements
    event_driven: [
        {
            trigger: "WHEN <event>"
            shall:   "THE SYSTEM SHALL <action>"
        },
    ]

    // State-based requirements
    state_driven: [
        {
            state: "WHILE <condition>"
            shall: "THE SYSTEM SHALL <behavior>"
        },
    ]

    // Negative requirements with justification
    unwanted: [
        {
            condition: "IF <bad situation>"
            shall_not: "THE SYSTEM SHALL NOT <forbidden action>"
            because:   "<rationale>"
        },
    ]

    // Complex state+trigger combinations
    complex: [
        {
            state:   "WHILE <condition>"
            trigger: "WHEN <event>"
            shall:   "THE SYSTEM SHALL <action>"
        },
    ]
}
```

### 2. KIRK Contracts (Design by Contract)

```cue
contracts: {
    preconditions: {
        auth_required: bool
        required_inputs: [
            {
                field:           "FieldName"
                type:            "Type"
                constraints:     "Description of valid values"
                example_valid:   "Good example"
                example_invalid: "Bad example"
            },
        ]
        system_state: [
            "Required system state before operation",
        ]
    }

    postconditions: {
        state_changes: [
            "What changes after operation",
        ]
        return_guarantees: [
            {
                field:     "ReturnValue"
                guarantee: "What is guaranteed about the return"
            },
        ]
        side_effects: [
            "Side effects that occur",
        ]
    }

    invariants: [
        "Conditions that must ALWAYS be true",
    ]
}
```

### 3. Inversions (What Can Go Wrong)

```cue
inversions: {
    integration_failures: [
        {
            failure:     "What could fail during integration"
            prevention:  "How to prevent it"
            test_for_it: "test_name_to_catch_this"
        },
    ]

    usability_failures: [
        {
            failure:     "UX problem"
            prevention:  "How to prevent"
            test_for_it: "test_name"
        },
    ]

    data_integrity_failures: [
        {
            failure:     "Data corruption scenario"
            prevention:  "How to prevent"
            test_for_it: "test_name"
        },
    ]
}
```

### 4. Acceptance Tests (ATDD with Real Code)

```cue
acceptance_tests: {
    happy_paths: [
        {
            name:  "test_descriptive_name"
            given: "Initial state"
            when:  "Action performed"
            then: [
                "Expected outcome 1",
                "Expected outcome 2",
            ]
            real_input: """
                // REAL Rust code, not pseudocode
                let result = function_under_test(input).await?;
            """
            expected_output: """
                assert_eq!(result, expected);
            """
        },
    ]

    error_paths: [
        {
            name:  "test_error_scenario"
            given: "..."
            when:  "..."
            then: [...]
            real_input: "..."
            expected_output: null
            expected_error: "Err(Error::SpecificVariant)"
        },
    ]

    edge_cases: [
        {
            name:     "test_edge_case"
            scenario: "Description of edge case"
            input:    "What to provide"
            expected: "What should happen"
        },
    ]

    contract_tests: [
        {
            name:     "test_invariant_something"
            verifies: "Which contract/invariant this verifies"
            test:     "How to verify it"
        },
    ]
}
```

### 5. E2E Tests

```cue
e2e_tests: {
    pipeline_test: {
        name:        "test_full_lifecycle"
        description: "What this E2E test covers"
        setup: {
            files_to_create: [
                {
                    path:    "path/to/test/file.rs"
                    content: "..."
                },
            ]
            precondition_commands: [
                "moon run project:build",
            ]
        }
        execute: {
            command:    "moon run project:test -- --test name"
            timeout_ms: 30000
        }
        verify: {
            exit_code: 0
            stdout_contains: [
                "expected output",
            ]
            files_created: [
                {
                    path:     "expected/file.rs"
                    contains: "expected content"
                },
            ]
        }
        cleanup: {
            commands: [
                "rm test/files",
            ]
        }
    }

    e2e_scenarios: [
        {
            name:        "e2e_scenario_name"
            description: "What this scenario tests"
            steps: [
                {
                    action: "Step 1"
                    verify: "Expected result"
                },
            ]
        },
    ]
}
```

### 6. Implementation Tasks (TDD Phases)

```cue
implementation_tasks: {
    // RED phase - write failing tests first
    phase_1_tests_first: [
        {
            task:      "Write test: test_name"
            file:      "path/to/file.rs"
            what:      "What the test verifies"
            done_when: "Test exists and FAILS (red phase)"
        },
    ]

    // GREEN phase - implement to pass tests
    phase_2_implementation: [
        {
            task:      "Implement feature"
            file:      "path/to/file.rs"
            what:      "What to implement"
            done_when: "Tests pass (green phase)"
            patterns_to_use: [
                "Pattern name",
            ]
        },
    ]

    // Integration
    phase_3_integration: [
        {
            task:      "Export module"
            file:      "lib.rs"
            what:      "pub mod module_name;"
            done_when: "External crates can import"
        },
    ]

    // Verification
    phase_4_verification: [
        {
            task:      "Run CI"
            done_when: "All tests pass"
            commands: [
                "moon run :ci",
            ]
            expected: "exit code 0"
        },
    ]
}
```

### 7. Failure Modes

```cue
failure_modes: {
    failure_modes: [
        {
            symptom:      "Error message or behavior"
            likely_cause: "What probably went wrong"
            where_to_look: [
                {
                    file:          "path/to/file.rs"
                    function:      "function_name()"
                    what_to_check: "What to look for"
                },
            ]
            fix_pattern: "How to fix it"
        },
    ]

    debugging_commands: [
        {
            scenario: "When X happens"
            run:      "command to debug"
            look_for: "What to find in output"
        },
    ]
}
```

### 8. Completion Checklist

```cue
completion_checklist: {
    tests: [
        "[ ] test_name passes",
    ]
    code: [
        "[ ] Feature implemented",
        "[ ] Zero unwrap() calls",
    ]
    ci: [
        "[ ] moon run :ci passes",
        "[ ] No clippy warnings",
    ]
    documentation: [
        "[ ] Module docs written",
    ]
}
```

### 9. Context

```cue
context: {
    related_files: [
        {
            path:      "path/to/related/file.rs"
            relevance: "Why this file is relevant"
        },
    ]

    dependencies: [
        {
            bead_id:           "factory-xyz"
            description:       "What this bead provides"
            what_it_provides:  "Specific APIs/types needed"
        },
    ]

    external_references: [
        "https://docs.rs/crate - Description",
    ]

    codebase_patterns: [
        {
            pattern:          "Pattern Name"
            example_location: "path/to/example.rs"
            how_to_apply:     "How to use this pattern here"
        },
    ]
}
```

### 10. AI Hints

```cue
ai_hints: {
    do: [
        "Use ractor for actors",
        "Return Result<T, Error> everywhere",
    ]

    do_not: [
        "Do NOT use unwrap() or expect()",
        "Do NOT use panic!, todo!, unimplemented!",
        "Do NOT modify clippy configuration",
        "Do NOT use raw cargo commands (use moon)",
    ]

    code_patterns: [
        {
            name:     "Pattern Name"
            use_when: "When to use this pattern"
            example:  """
                // Real Rust code example
                fn example() -> Result<T, Error> {
                    // ...
                }
            """
        },
    ]
}
```

## Validation

All CUE specs MUST validate against the schema:

```bash
cd /home/lewis/src/brutalist/factory/.beads
cue vet specs/factory-XXX.cue
```

## Export to YAML for Bead Description

To update a bead with its full spec:

```bash
cd /home/lewis/src/brutalist/factory/.beads
cue export specs/factory-XXX.cue --out yaml > /tmp/spec.yaml
# Then update bead description with the YAML content
```

## Key Principles

1. **EARS patterns required** - Every requirement uses "THE SYSTEM SHALL..."
2. **Real code only** - No pseudocode in tests or examples
3. **Railway-Oriented Programming** - All fallible ops return `Result<T, Error>`
4. **Zero panics** - No `unwrap()`, `expect()`, `panic!`, `todo!`, `unimplemented!`
5. **TDD phases** - Tests first (RED), then implement (GREEN), then refactor
6. **Moon only** - Never raw cargo commands
7. **Clippy untouched** - Fix code, not lint rules
