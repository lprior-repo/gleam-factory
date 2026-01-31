// Enhanced Bead Schema - World-Class Ticket Template
// This schema enforces the complete EARS + KIRK + ATDD + E2E structure
// Any bead conforming to this schema is guaranteed to be implementable

package schema

import "strings"

// ============================================================================
// Core Bead Structure
// ============================================================================

#EnhancedBead: {
    // Identity
    id:              string & =~"^factory-[a-z0-9]+$"
    title:           string & =~"^[A-Za-z-]+: .+"  // Must be "Component: Description"
    type:            #IssueType
    priority:        #Priority
    effort_estimate: #EffortEstimate
    labels:          [...string]

    // The 10 Required Sections
    ears_requirements: #EarsRequirements
    contracts:         #KirkContracts
    inversions:        #InversionAnalysis
    acceptance_tests:  #AcceptanceTests
    e2e_tests:         #E2ETests
    implementation_tasks: #ImplementationTasks
    failure_modes:     #FailureModes
    completion_checklist: #CompletionChecklist
    context:           #Context
    ai_hints:          #AIHints
}

#IssueType: "feature" | "bug" | "task" | "epic" | "chore"

#Priority: 0 | 1 | 2 | 3 | 4  // 0=critical, 4=backlog

#EffortEstimate: "15min" | "30min" | "1hr" | "2hr" | "4hr"

// ============================================================================
// Section 1: EARS Requirements
// ============================================================================

#EarsRequirements: {
    // Pattern 1: Universal truths (always true)
    ubiquitous: [...#UbiquitousRequirement]

    // Pattern 2: Trigger-response pairs
    event_driven: [...#EventDrivenRequirement]

    // Pattern 3: Behavior during states
    state_driven?: [...#StateDrivenRequirement]

    // Pattern 4: Conditional on config/roles
    optional?: [...#OptionalRequirement]

    // Pattern 5: Things that must NEVER happen
    unwanted: [...#UnwantedRequirement]  // Required - inversion thinking

    // Pattern 6: State + Event combinations
    complex?: [...#ComplexRequirement]

    // Validation: Must have at least ubiquitous and event_driven
    _valid: len(ubiquitous) > 0 & len(event_driven) > 0 & len(unwanted) > 0
}

#UbiquitousRequirement: string & =~"^THE SYSTEM SHALL .+"

#EventDrivenRequirement: {
    trigger: string & =~"^WHEN .+"
    shall:   string & =~"^THE SYSTEM SHALL .+"
}

#StateDrivenRequirement: {
    state: string & =~"^WHILE .+"
    shall: string & =~"^THE SYSTEM SHALL .+"
}

#OptionalRequirement: {
    condition: string & =~"^WHERE .+"
    shall:     string & =~"^THE SYSTEM SHALL .+"
}

#UnwantedRequirement: {
    condition: string & =~"^IF .+"
    shall_not: string & =~"^THE SYSTEM SHALL NOT .+"
    because:   string & strings.MinRunes(10)  // Must explain why
}

#ComplexRequirement: {
    state:   string & =~"^WHILE .+"
    trigger: string & =~"^WHEN .+"
    shall:   string & =~"^THE SYSTEM SHALL .+"
}

// ============================================================================
// Section 2: KIRK Contracts (Design by Contract)
// ============================================================================

#KirkContracts: {
    preconditions:  #Preconditions
    postconditions: #Postconditions
    invariants:     [...string] & [_, ...]  // At least one invariant required
}

#Preconditions: {
    auth_required:   bool
    required_inputs: [...#InputSpec]
    system_state?:   [...string]
}

#InputSpec: {
    field:           string
    type:            "String" | "Int" | "Bool" | "List" | "Object"
    constraints:     string
    example_valid:   string | number | bool
    example_invalid: string | number | bool
}

#Postconditions: {
    state_changes:      [...string]
    return_guarantees:  [...#ReturnGuarantee]
    side_effects?:      [...string]
}

#ReturnGuarantee: {
    field:     string
    guarantee: string
}

// ============================================================================
// Section 3: Inversion Analysis
// ============================================================================

#InversionAnalysis: {
    security_failures?:       [...#FailureMode]
    usability_failures?:      [...#FailureMode]
    data_integrity_failures?: [...#FailureMode]
    integration_failures?:    [...#FailureMode]

    // Must have at least one category of failures analyzed
    _valid: len(security_failures) > 0 | len(usability_failures) > 0 | len(integration_failures) > 0
}

#FailureMode: {
    failure:     string
    prevention:  string
    test_for_it: string  // Name of test that verifies prevention
}

// ============================================================================
// Section 4: ATDD Acceptance Tests
// ============================================================================

#AcceptanceTests: {
    happy_paths:     [...#AcceptanceTest] & [_, ...]  // At least one required
    error_paths:     [...#AcceptanceTest] & [_, ...]  // At least one required
    edge_cases?:     [...#EdgeCaseTest]
    contract_tests?: [...#ContractTest]
}

#AcceptanceTest: {
    name:  string & =~"^test_.+"
    given: string
    when:  string
    then:  [...string] & [_, ...]  // At least one assertion

    // Real data - no placeholders allowed
    real_input:      string
    expected_output: string | null  // null for error path tests
    expected_error?: string
}

#EdgeCaseTest: {
    name:     string & =~"^test_.+"
    scenario: string
    input:    string
    expected: string
}

#ContractTest: {
    name:     string & =~"^test_(precondition|postcondition|invariant)_.+"
    verifies: string
    test:     string
}

// ============================================================================
// Section 5: End-to-End Tests
// ============================================================================

#E2ETests: {
    pipeline_test: #PipelineTest  // The "soup to nuts" test
    e2e_scenarios?: [...#E2EScenario]
}

#PipelineTest: {
    name:        string & =~"^test_full_.+"
    description: string

    setup: {
        files_to_create?: [...{
            path:    string
            content: string
        }]
        environment?: [...string]
        precondition_commands?: [...string]
    }

    execute: {
        command:     string
        stdin?:      string
        timeout_ms?: number | *10000
    }

    verify: {
        exit_code:           number
        stdout_contains?:    [...string]
        stdout_matches_json?: [...{
            path:        string
            value?:      _
            type?:       string
            pattern?:    string
            min_length?: number
        }]
        files_created?: [...{
            path:      string
            contains?: string
        }]
        files_not_modified?: [...string]
        side_effects?:       [...string]
    }

    cleanup?: {
        commands?:        [...string]
        files_to_delete?: [...string]
    }
}

#E2EScenario: {
    name:        string
    description: string
    steps:       [...{
        action: string
        verify: string
    }]
}

// ============================================================================
// Section 6: Implementation Tasks
// ============================================================================

#ImplementationTasks: {
    // Phase 1: Write failing tests first (TDD Red)
    phase_1_tests_first: [...#Task] & [_, ...]

    // Phase 2: Implement to make tests pass (TDD Green)
    phase_2_implementation: [...#Task] & [_, ...]

    // Phase 3: Wire up to system (Integration)
    phase_3_integration?: [...#Task]

    // Phase 4: Verification
    phase_4_verification: [...#Task] & [_, ...]
}

#Task: {
    task:      string
    file?:     string
    what?:     string
    done_when: string

    patterns_to_use?: [...string]
    commands?: [...string]
    expected?: string
}

// ============================================================================
// Section 7: Failure Modes & Debugging
// ============================================================================

#FailureModes: {
    failure_modes: [...#FailureModeDebug]
    debugging_commands?: [...#DebuggingCommand]
}

#FailureModeDebug: {
    symptom:      string
    likely_cause: string
    where_to_look: [...{
        file:          string
        line_range?:   string
        function?:     string
        what_to_check: string
    }]
    fix_pattern: string
}

#DebuggingCommand: {
    scenario: string
    run:      string
    look_for: string
}

// ============================================================================
// Section 8: Completion Checklist
// ============================================================================

#CompletionChecklist: {
    tests: [...string] & [
            "[ ] All acceptance tests written and passing",
            "[ ] All error path tests written and passing",
            "[ ] E2E pipeline test passing with real data",
            "[ ] No mocks or fake data in any test",
            ...,
    ]

    code: [...string] & [
            "[ ] Implementation uses Result<T, Error> throughout",
            "[ ] Zero unwrap() or expect() calls",
            ...,
    ]

    ci: [...string] & [
            "[ ] moon run :ci passes",
            ...,
    ]

    documentation?: [...string]
}

// ============================================================================
// Section 9: Context & References
// ============================================================================

#Context: {
    related_files: [...{
        path:      string
        relevance: string
    }]

    similar_implementations?: [...string]
    external_references?:     [...string]

    codebase_patterns?: [...{
        pattern:         string
        example_location: string
        how_to_apply:    string
    }]
}

// ============================================================================
// Section 10: AI Implementation Hints
// ============================================================================

#AIHints: {
    do: [...string] & [_, ...]      // At least one "do"
    do_not: [...string] & [_, ...]  // At least one "do not"

    code_patterns?: [...{
        name:     string
        use_when: string
        example:  string
    }]
}

// ============================================================================
// Validation: Quality Gates
// ============================================================================

// A bead is only valid if it passes these quality gates
#ValidBead: #EnhancedBead & {
    // Must have EARS coverage
    ears_requirements: _valid: true

    // Must have inversion analysis
    inversions: _valid: true

    // Must have at least one happy path and one error path test
    acceptance_tests: {
        happy_paths: [_, ...]
        error_paths: [_, ...]
    }

    // Must have pipeline test
    e2e_tests: pipeline_test: name: =~"^test_full_.+"

    // Must have test-first tasks
    implementation_tasks: phase_1_tests_first: [_, ...]
}
