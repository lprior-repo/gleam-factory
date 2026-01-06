// TDD-TCR-REFACTOR AI Communication Contracts
// Compact, validated schemas for AI agent responses

package tcr

// Auditor output - writes one failing test
#AuditorOutput: {
	status: "test_written" | "requirements_complete" | "error"
	test?: {
		name:   string & =~"^[a-z_]+_test$"
		file:   string
		target: string // what behavior is being tested
	}
	reason?: string // brief explanation (max 50 words)
}

// Implementer output - makes test pass
#ImplementerOutput: {
	status: "implemented" | "blocked" | "error"
	changes?: [...{
		file:   string
		action: "add" | "modify"
		lines:  int // approx lines changed
	}]
	reason?: string // max 30 words
}

// Architect output - refactors for quality
#ArchitectOutput: {
	status:      "refactored" | "no_changes" | "error"
	loc_before?: int
	loc_after?:  int
	changes?: [...{
		type:   "delete" | "simplify" | "extract" | "rename"
		target: string
	}]
	smells?: [...{
		title:       string
		description: string
		priority:    0 | 1 | 2 | 3
	}]
}

// Reviewer output - final verification
#ReviewerOutput: {
	status:                    "passed" | "failed"
	acceptance_criteria_met:   bool
	verification: [...{
		criterion: string
		met:       bool
		evidence:  string
	}]
	learnings?: string // max 100 words
}

// Example valid outputs for each role
examples: {
	auditor: #AuditorOutput & {
		status: "test_written"
		test: {
			name:   "workspace_id_from_string_test"
			file:   "test/factory_test.gleam"
			target: "WorkspaceId.from_string creates valid ID"
		}
		reason: "Drives opaque type constructor implementation"
	}

	implementer: #ImplementerOutput & {
		status: "implemented"
		changes: [{
			file:   "src/types.gleam"
			action: "add"
			lines:  5
		}]
		reason: "Added from_string function"
	}

	architect: #ArchitectOutput & {
		status:     "refactored"
		loc_before: 150
		loc_after:  120
		changes: [{
			type:   "delete"
			target: "unused helper function"
		}]
	}

	reviewer: #ReviewerOutput & {
		status:                  "passed"
		acceptance_criteria_met: true
		verification: [{
			criterion: "WorkspaceId wraps string"
			met:       true
			evidence:  "test/factory_test.gleam:1285"
		}]
	}
}
