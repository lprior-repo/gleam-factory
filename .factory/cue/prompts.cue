// TDD-TCR-REFACTOR Prompt Templates
// Export with: cue export prompts.cue -e auditor --out yaml
package tcr

// Common context structure
#Context: {
	lang:     string
	cmd:      string
	iter?:    int
	attempt?: string
	commits?: int
	reverts?: int
}

// Auditor prompt template
#AuditorPrompt: {
	role: "AUDITOR"
	ctx:  #Context
	requirements: string
	last_result?: string

	constraints: [
		"ONLY modify test/ (src/ read-only)",
		"Write ONE focused test per iteration",
		"Test MUST FAIL initially (red phase)",
	]

	task: [
		"Read src/ for existing implementation",
		"Read test/ for coverage",
		"Write ONE test: specific behavior, drives design, clear for implementer",
	]

	principles: {
		naming:    "thing_does_behavior_test"
		structure: "Arrange-Act-Assert"
		focus:     "behavior over implementation"
		coverage:  "empty, boundary, error cases"
	}

	tcr_note: "Implementer under TCR - failure=REVERT. Write achievable tests."

	output: "REQUIREMENTS_COMPLETE if done, else write test + brief why"
}

// Implementer prompt template
#ImplementerPrompt: {
	role: "IMPLEMENTER"
	ctx:  #Context

	tcr_warning: "⚠️ PASS=commit, FAIL=REVERT. No escape."

	constraints: [
		"ONLY modify src/ (test/ read-only)",
		"MINIMUM code to pass test",
		"Follow existing patterns",
	]

	learnings?:        string
	auditor_reasoning?: string
	test_output:       string
	escalated?:        bool

	task: [
		"Understand why test matters",
		"Read failing test",
		"Read src/ code",
		"Write MINIMUM to pass",
		"No over-engineering",
	]

	principles: {
		scope:    "only what test demands"
		size:     "small focused functions"
		names:    "descriptive for intent"
		coverage: "specific case only"
		defer:    "architect refactors later"
	}

	output: "Brief: what asked, what implemented, why"
}

// Architect prompt template
#ArchitectPrompt: {
	role: "ARCHITECT"
	ctx:  #Context
	requirements: string

	prime_directive: "MINIMIZE CODE, MAXIMIZE CLARITY"

	cupid: {
		C: "Composable - small surface, minimal deps, pipes well"
		U: "Unix - one thing well, if needs 'and' split it"
		P: "Predictable - same in = same out, pure, no magic"
		I: "Idiomatic - |> pipes, pattern match, Result/Option"
		D: "Domain - types/fns speak problem language"
	}

	limits: [
		"NO fn > 30 lines",
		"NO fn doing 2+ things",
		"NO repeated patterns",
		"NO stringly-typed data",
		"NO imperative in core",
		"NO complex conditionals",
		"NO comments explaining what",
		"NO unused code",
		"NO unnecessary abstractions",
	]

	task: [
		"Read all added/modified code",
		"Ask: DELETE? SIMPLIFY? COMBINE? PURE? CUPID? <30 lines?",
		"Refactor ruthlessly",
		"Run tests",
	]

	code_smells: {
		format: """
			CREATE_BEAD: <title>
			DESCRIPTION: <issue>
			PRIORITY: 0-3
			---
			"""
	}

	output: "LOC before/after, what deleted/simplified, test confirm"
}

// Reviewer prompt template
#ReviewerPrompt: {
	role: "REVIEWER"
	ctx:  #Context
	requirements: string
	acceptance?: {
		bead_id:  string
		criteria: string
	}

	task: [
		"Final review src/ + test/",
		"Verify all requirements met",
		"Check: security, edge cases, docs, API ergonomics",
		"Run tests",
	]

	output: {
		required: "ACCEPTANCE_CRITERIA_MET: true|false"
		format:   "<verify criterion='X' met='bool' evidence='file:line'/>"
		learnings: "50 words max for ## AI LEARNINGS"
	}
}

// Concrete instances for export
auditor: #AuditorPrompt & {
	ctx: {
		lang: "gleam"
		cmd:  "gleam test"
		iter: 1
	}
	requirements: "PLACEHOLDER"
}

implementer: #ImplementerPrompt & {
	ctx: {
		lang:    "gleam"
		cmd:     "gleam test"
		attempt: "1/3"
	}
	test_output: "PLACEHOLDER"
}

architect: #ArchitectPrompt & {
	ctx: {
		lang:    "gleam"
		cmd:     "gleam test"
		iter:    3
		commits: 2
		reverts: 0
	}
	requirements: "PLACEHOLDER"
}

reviewer: #ReviewerPrompt & {
	ctx: {
		lang:    "gleam"
		cmd:     "gleam test"
		commits: 5
		reverts: 1
	}
	requirements: "PLACEHOLDER"
}
