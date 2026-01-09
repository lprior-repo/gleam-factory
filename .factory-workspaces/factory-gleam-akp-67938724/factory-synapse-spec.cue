// ============================================================================
// FACTORY-SYNAPSE SYSTEM SPECIFICATION
// ============================================================================
// A signal-driven AI coding system combining reactive bug-fixing (Synapse)
// with deliberate TDD-TCR-REFACTOR loops (Factory).
//
// Author: Lewis + Claude
// Version: 1.0.0
// License: MIT
// ============================================================================

package factory_synapse

import "time"

// ============================================================================
// CORE PHILOSOPHY
// ============================================================================

#Philosophy: {
	// The system has two modes that share infrastructure
	paradigms: {
		synapse: {
			model:       "reactive"
			trigger:     "TestFailure signal"
			response:    "spawn N mutators in parallel"
			strategy:    "first valid patch wins, rest die"
			duration:    "seconds"
			use_case:    "bug fixes, regressions, quick patches"
			llm_model:   "small batch (Qwen 1.5B)"
			cost:        "near zero (local batch inference)"
			quality:     "good enough (tests pass)"
		}
		factory: {
			model:       "deliberate"
			trigger:     "BeadAssigned signal"
			response:    "spawn 1 TDD-TCR-REFACTOR loop"
			strategy:    "full verification gauntlet"
			duration:    "minutes"
			use_case:    "features, design work, quality-critical"
			llm_model:   "capable (Qwen 32B) + API reviews"
			cost:        "low local + API for reviews"
			quality:     "production-grade"
		}
	}

	// Key inversions from traditional development
	inversions: {
		control_flow: {
			traditional: "human triggers tests"
			inverted:    "system always running tests, failures broadcast signals"
		}
		branching: {
			traditional: "checkout branch, work, merge"
			inverted:    "reflink snapshot, mutate, first-patch-wins or full-verify"
		}
		agent_model: {
			traditional: "manager assigns work to agents"
			inverted:    "signals broadcast, agents self-select and react"
		}
		failure_handling: {
			traditional: "debug, fix, retry"
			inverted:    "TCR revert, die silently, let others try"
		}
	}

	// Principles from Dave Farley, Rich Hickey, functional design
	principles: [
		"Code is a liability - minimize while maximizing clarity",
		"TCR: Test && Commit || Revert - no broken code survives",
		"TDD: Red-Green-Refactor - tests drive design",
		"Functional core, imperative shell - push IO to edges",
		"CUPID: Composable, Unix philosophy, Predictable, Idiomatic, Domain-based",
		"Immutable by default, explicit state transitions",
		"Parse don't validate - make illegal states unrepresentable",
		"Small PRs, fast feedback loops, continuous integration",
	]
}

// ============================================================================
// SIGNAL BUS - THE CENTRAL NERVOUS SYSTEM
// ============================================================================

#SignalBus: {
	description: "All events flow through the signal bus. Components subscribe and react."
	implementation: "gleam_pubsub or Erlang Registry"
	
	// All signal types in the system
	signals: #Signals
}

#Signals: {
	// Test state signals (from Heartbeat)
	TestFailure: {
		file:         string
		error:        string
		context_hash: #GitHash
		timestamp:    time.Time
	}
	TestPassing: {
		hash:      #GitHash
		timestamp: time.Time
	}

	// Task signals (from Beads)
	BeadAssigned: {
		task_id:      string
		spec:         string
		requirements: string
		priority:     #Priority
		assigned_at:  time.Time
	}

	// Patch signals (from agents)
	PatchProposed: {
		diff:       string
		author_pid: #ProcessId
		workspace:  string
		hash:       #GitHash
	}
	PatchAccepted: {
		hash:       #GitHash
		merged_at:  time.Time
	}
	PatchRejected: {
		reason: string
	}

	// State change signals
	GoldenMasterUpdated: {
		old_hash: #GitHash
		new_hash: #GitHash
	}
	Evolution: {
		new_hash: #GitHash
		cause:    "synapse_patch" | "factory_push" | "external_commit"
	}

	// Loop lifecycle signals
	LoopSpawned: {
		loop_id: string
		task_id: string
		phase:   #FactoryPhase
	}
	LoopComplete: {
		loop_id:      string
		task_id:      string
		commits:      int
		reverts:      int
		duration_ms:  int
	}
	LoopFailed: {
		loop_id: string
		reason:  string
	}

	// Resource signals
	ResourceExhausted: {
		resource: "ram" | "gpu" | "workspaces" | "loops"
		current:  int
		limit:    int
	}
}

#GitHash: =~"^[a-f0-9]{40}$"
#ProcessId: string
#Priority: "critical" | "high" | "medium" | "low"

// ============================================================================
// HEARTBEAT - ALWAYS-RUNNING TEST LOOP
// ============================================================================

#Heartbeat: {
	description: """
		The heartbeat continuously runs tests against the golden master.
		When tests fail, it broadcasts a TestFailure signal.
		When tests pass after being red, it broadcasts TestPassing.
		This is the source of truth for system state.
		"""

	config: {
		golden_master_path: string | *"/data/golden-master"
		interval_ms:        int | *5000
		test_command:       string | *"gleam test"
	}

	state: {
		last_hash:   #GitHash
		last_status: #TestStatus
		last_run:    time.Time
	}

	behavior: {
		on_tick: """
			1. Get current hash of golden master
			2. Run test command
			3. If PASS and was RED: broadcast TestPassing
			4. If FAIL and was GREEN: broadcast TestFailure
			5. If FAIL and different error: broadcast new TestFailure
			6. Schedule next tick
			"""
	}
}

#TestStatus: {
	status: "green" | "red"
	if status == "red" {
		file:  string
		error: string
	}
}

// ============================================================================
// SYNAPSE REACTOR - REACTIVE BUG-FIXING SWARM
// ============================================================================

#SynapseReactor: {
	description: """
		When a TestFailure signal is broadcast, the Synapse reactor spawns
		a swarm of mutator processes. Each mutator:
		1. Creates a reflink workspace (instant, ~0 bytes)
		2. Generates a fix using cheap batch LLM
		3. Applies and tests the fix
		4. If pass: broadcasts PatchProposed
		5. If fail: dies silently
		First valid patch wins. Rest are discarded.
		"""

	config: {
		swarm_size:      int | *32
		model_endpoint:  string | *"http://localhost:8080"
		model_name:      string | *"qwen-coder-1.5b"
		timeout_ms:      int | *30000
		batch_inference: bool | *true
	}

	trigger: #Signals.TestFailure

	behavior: {
		on_test_failure: """
			1. Check resource governor for mutator slots
			2. Spawn swarm_size mutators in parallel
			3. Each mutator runs independently
			4. First PatchProposed that passes absorber verification wins
			5. On Evolution signal, all mutators die (world changed)
			"""
	}
}

#Mutator: {
	description: "A single mutation attempt process"

	state: {
		id:             int
		workspace_path: string
		file:           string
		error:          string
		hash:           #GitHash
		status:         "running" | "succeeded" | "failed" | "killed"
	}

	lifecycle: """
		1. SPAWN: Create reflink workspace from golden master
		2. MUTATE: Call LLM to generate fix for specific file/error
		3. APPLY: Apply generated patch to workspace
		4. VERIFY: Run tests in workspace
		5. PROPOSE: If tests pass, broadcast PatchProposed
		6. DIE: Destroy workspace, exit process
		
		On timeout: DIE
		On Evolution signal: DIE (work is stale)
		On any error: DIE silently (no noise)
		"""

	prompt_template: """
		You are a surgical bug fixer. Your ONLY job is to fix this specific error.

		FILE: {{file}}
		ERROR: {{error}}
		HASH: {{hash}}

		CONSTRAINTS:
		- Make the MINIMUM change to fix the error
		- Do NOT refactor
		- Do NOT add features
		- Do NOT change unrelated code
		- Output ONLY a unified diff

		DIFF:
		"""
}

// ============================================================================
// FACTORY LOOPS - DELIBERATE TDD-TCR-REFACTOR
// ============================================================================

#FactoryLoop: {
	description: """
		A factory loop implements full TDD-TCR-REFACTOR discipline.
		It has 4 roles that execute in sequence:
		1. AUDITOR: Writes ONE failing test
		2. IMPLEMENTER: Makes test pass (TCR enforced)
		3. ARCHITECT: Refactors every N green cycles
		4. REVIEWER: Final polish before push
		"""

	config: {
		max_iterations:       int | *15
		max_impl_attempts:    int | *3
		refactor_interval:    int | *3
		auditor_timeout_ms:   int | *120000
		implementer_timeout_ms: int | *120000
		architect_timeout_ms: int | *180000
		reviewer_timeout_ms:  int | *60000
	}

	trigger: #Signals.BeadAssigned

	state: #FactoryLoopState
	phases: #FactoryPhases
	roles: #FactoryRoles
}

#FactoryLoopState: {
	loop_id:        string
	task_id:        string
	task_spec:      string
	workspace_path: string
	phase:          #FactoryPhase
	iteration:      int
	green_count:    int
	commit_count:   int
	revert_count:   int
	history:        [...#HistoryEntry]
	last_feedback:  string | *""
}

#FactoryPhase: 
	"auditing" |
	"verifying_red" |
	"implementing" |
	"tcr_checking" |
	"verifying" |
	"refactoring" |
	"reviewing" |
	"pushing" |
	"rebasing" |
	"complete" |
	"failed"

#FactoryPhases: {
	auditing: {
		description: "Auditor writes ONE failing test"
		role:        "auditor"
		next_on_success: "verifying_red"
		next_on_complete: "reviewing"
		max_attempts: 3
		filesystem_locks: {
			src:  "locked"
			test: "unlocked"
		}
	}

	verifying_red: {
		description: "Verify the new test actually fails"
		action:      "run_tests"
		next_on_fail: "implementing"  // Good - test fails as expected
		next_on_pass: "auditing"      // Bad - test should fail
	}

	implementing: {
		description: "Implementer makes test pass"
		role:        "implementer"
		next:        "tcr_checking"
		max_attempts: 3
		filesystem_locks: {
			src:  "unlocked"
			test: "locked"
		}
	}

	tcr_checking: {
		description: "TCR: Test && Commit || Revert"
		action:      "run_tests"
		on_pass: {
			action: "commit"
			message: "PASS(tcr): Implementation"
			next:    "verifying"
		}
		on_fail: {
			action: "revert"
			next:   "implementing"  // Try again
		}
	}

	verifying: {
		description: "Full verification gauntlet"
		stages:      #VerificationStages
		on_pass: {
			check: "green_count >= refactor_interval"
			if_true:  "refactoring"
			if_false: "auditing"
		}
		on_fail: {
			action: "revert"
			next:   "implementing"
		}
	}

	refactoring: {
		description: "Architect cleans up code"
		role:        "architect"
		next:        "auditing"
		filesystem_locks: {
			src:  "unlocked"
			test: "unlocked"
		}
		on_tests_fail: "revert and continue to auditing"
	}

	reviewing: {
		description: "Final polish before push"
		role:        "reviewer"
		next:        "pushing"
		model:       "haiku-4.5"  // API
		filesystem_locks: {
			src:  "unlocked"
			test: "unlocked"
		}
	}

	pushing: {
		description: "Push to central hub"
		action:      "jj git push"
		on_success:  "complete"
		on_conflict: "rebasing"
		on_error:    "failed"
	}

	rebasing: {
		description: "Rebase on latest and re-verify"
		actions: [
			"jj git fetch",
			"jj rebase -d main@origin",
		]
		on_success: "verifying"  // Re-run full verification
		on_conflict: "failed"   // Needs human intervention
	}

	complete: {
		description: "Loop finished successfully"
		actions: [
			"broadcast LoopComplete",
			"destroy workspace",
			"release resources",
		]
	}

	failed: {
		description: "Loop failed, needs intervention"
		actions: [
			"broadcast LoopFailed",
			"keep workspace for debugging",
			"release resources",
		]
	}
}

#HistoryEntry: {
	iteration: int
	role:      string
	content:   string
	timestamp: time.Time
}

// ============================================================================
// FACTORY ROLES - THE FOUR ACTORS
// ============================================================================

#FactoryRoles: {
	auditor:     #AuditorRole
	implementer: #ImplementerRole
	architect:   #ArchitectRole
	reviewer:    #ReviewerRole
}

#AuditorRole: {
	description: """
		The auditor writes tests. One test per iteration.
		Tests should be focused, clear, and drive good design.
		The auditor CANNOT touch src/ (filesystem enforced).
		"""

	model:         "qwen-32b" | "local"
	model_endpoint: string | *"http://localhost:8080"
	allowed_tools: ["Read", "Write", "Edit", "Glob", "Grep", "Bash(gleam:*)", "Bash(cargo:*)"]
	filesystem: {
		src:  "read-only"
		test: "read-write"
	}

	system_prompt_addition: """
		You are the AUDITOR. You may ONLY modify files in test/.
		Any attempt to modify src/ will fail.
		Focus on writing ONE failing test that drives good design.
		If all requirements are met, output REQUIREMENTS_COMPLETE.
		"""

	prompt_template: """
		You are the AUDITOR in a TDD-TCR-REFACTOR loop. Your job is to write tests.

		PROJECT: {{language}} project
		TEST COMMAND: {{test_command}}

		REQUIREMENTS:
		{{requirements}}

		ITERATION: {{iteration}}

		YOUR CONSTRAINTS:
		- You can ONLY modify files in test/
		- You CANNOT modify files in src/ (read-only, filesystem enforced)
		- Write ONE focused test at a time
		- The test should FAIL initially (red phase of TDD)

		YOUR TASK:
		1. Read the current src/ code to understand what exists
		2. Read the current tests to understand what's covered
		3. Write EXACTLY ONE test that:
		   - Tests ONE specific behavior or edge case
		   - Drives better code design (not just coverage)
		   - Forces the implementer to write clean, focused code
		   - Follows testing best practices (arrange-act-assert, descriptive names)

		TEST DESIGN PRINCIPLES:
		- Each test should validate ONE thing
		- Test names should describe the behavior being tested
		- Tests should be independent and not rely on each other
		- Prefer testing behavior over implementation details
		- Edge cases matter: empty inputs, boundaries, error conditions

		TCR CONTEXT:
		The implementer is under TCR discipline - if they fail tests, their code is REVERTED.
		Write tests that are:
		- Clear enough that the implementer knows exactly what to build
		- Focused enough that they can be implemented in one small step
		- Strict enough that they catch real bugs

		ARCHITECTURAL NOTE:
		An ARCHITECT will review the code every {{refactor_interval}} iterations.
		Write tests that encourage good architecture, not just passing behavior.

		{{#if last_feedback}}
		LAST RESULT:
		{{last_feedback}}
		{{/if}}

		IMPORTANT: Write ONLY ONE test per iteration.
		If ALL requirements are fully tested and implemented, output EXACTLY:
		REQUIREMENTS_COMPLETE
		"""

	output_parsing: {
		completion_marker: "REQUIREMENTS_COMPLETE"
		on_marker: "transition to reviewing phase"
	}
}

#ImplementerRole: {
	description: """
		The implementer makes tests pass. MINIMUM code only.
		Under TCR discipline: pass = commit, fail = revert.
		The implementer CANNOT touch test/ (filesystem enforced).
		"""

	model:         "qwen-32b" | "local"
	model_endpoint: string | *"http://localhost:8080"
	allowed_tools: ["Read", "Write", "Edit", "Glob", "Grep", "Bash(gleam:*)", "Bash(cargo:*)"]
	filesystem: {
		src:  "read-write"
		test: "read-only"
	}

	system_prompt_addition: """
		You are the IMPLEMENTER under TCR discipline.
		You may ONLY modify files in src/.
		If tests fail, your changes will be REVERTED.
		Write MINIMUM code to pass the test.
		"""

	prompt_template: """
		You are the IMPLEMENTER in a TDD-TCR-REFACTOR loop. Your job is to make tests pass.

		PROJECT: {{language}} project
		TEST COMMAND: {{test_command}}

		⚠️  TCR DISCIPLINE IS ENFORCED ⚠️
		- If tests PASS after your changes → your code is COMMITTED automatically
		- If tests FAIL after your changes → your code is REVERTED automatically
		- There is NO escape. Write code that works or it disappears.

		YOUR CONSTRAINTS:
		- You can ONLY modify files in src/
		- You CANNOT modify files in test/ (read-only, filesystem enforced)
		- Write the MINIMUM code to make tests pass
		- Follow existing patterns in the codebase

		═══════════════════════════════════════════════════════════════════════════════
		                    AUDITOR'S REASONING (why this test exists)
		═══════════════════════════════════════════════════════════════════════════════

		{{auditor_reasoning}}

		═══════════════════════════════════════════════════════════════════════════════

		CURRENT TEST OUTPUT:
		{{test_output}}

		ATTEMPT: {{attempt}} of {{max_attempts}}

		YOUR TASK:
		1. Read the AUDITOR'S REASONING above - understand WHY this test matters
		2. Read the failing test to understand what's expected
		3. Read the current src/ code
		4. Write the MINIMUM code to make ONLY this test pass
		5. Do NOT over-engineer or add untested features

		IMPLEMENTATION PRINCIPLES:
		- Write only what the test demands - nothing more
		- Keep functions small and focused
		- Use descriptive names that explain intent
		- Handle the specific case the test covers
		- The Architect will refactor shortcuts - just make it work first

		REMEMBER: Your code will be REVERTED if tests fail. Be precise. Be minimal. Be correct.

		OUTPUT FORMAT (required):
		1. What the test is asking for (1-2 sentences)
		2. What you implemented (code changes)
		3. Why this approach (1-2 sentences)
		"""
}

#ArchitectRole: {
	description: """
		The architect refactors after every N successful implementations.
		Goal: MINIMIZE code while MAXIMIZING clarity.
		Applies CUPID principles and functional design.
		Can modify both src/ and test/.
		"""

	model:         "qwen-32b" | "local"
	model_endpoint: string | *"http://localhost:8080"
	allowed_tools: ["Read", "Write", "Edit", "Glob", "Grep", "Bash(gleam:*)", "Bash(cargo:*)"]
	filesystem: {
		src:  "read-write"
		test: "read-write"
	}

	system_prompt_addition: """
		You are the ARCHITECT. You may modify both src/ and test/.
		Your job is to REFACTOR the code for quality and maintainability.
		ALL TESTS MUST STILL PASS.
		Focus on CUPID, code minimization, and idiomatic patterns.
		Every line must earn its place.
		"""

	prompt_template: """
		You are the ARCHITECT in a TDD-TCR-REFACTOR loop. This is the REFACTOR phase.

		PROJECT: {{language}} project (functional, immutable, BEAM-based)
		TEST COMMAND: {{test_command}}

		═══════════════════════════════════════════════════════════════════════════════
		              SESSION HISTORY (what happened in previous iterations)
		═══════════════════════════════════════════════════════════════════════════════

		{{session_history}}

		═══════════════════════════════════════════════════════════════════════════════
		                        CODE IS A LIABILITY
		═══════════════════════════════════════════════════════════════════════════════

		Every line of code is:
		- A line that can have bugs
		- A line someone must read and understand
		- A line that must be maintained forever
		- A line that slows down compilation
		- A line that increases cognitive load

		YOUR PRIME DIRECTIVE: MINIMIZE CODE WHILE MAXIMIZING CLARITY

		═══════════════════════════════════════════════════════════════════════════════
		                              CUPID PROPERTIES
		═══════════════════════════════════════════════════════════════════════════════

		C - COMPOSABLE: Small surface area, minimal dependencies
		U - UNIX PHILOSOPHY: Do one thing well
		P - PREDICTABLE: Same input → same output, always
		I - IDIOMATIC: Feels natural for the language
		D - DOMAIN-BASED: Code speaks the language of the problem

		═══════════════════════════════════════════════════════════════════════════════
		                    FUNCTIONAL CORE / IMPERATIVE SHELL
		═══════════════════════════════════════════════════════════════════════════════

		FUNCTIONAL CORE (pure, testable, the heart):
		- All business logic is PURE functions
		- No IO, no side effects, no external calls

		IMPERATIVE SHELL (thin, at the edges):
		- IO operations (file, network, stdin/stdout)
		- As THIN as possible - just plumbing

		═══════════════════════════════════════════════════════════════════════════════
		                           HARD LIMITS (ENFORCED)
		═══════════════════════════════════════════════════════════════════════════════

		□ NO function over 30 lines (if longer, decompose)
		□ NO function doing 2+ things (split it)
		□ NO repeated code patterns (extract or delete)
		□ NO stringly-typed data (create proper types)
		□ NO complex conditionals (pattern match instead)
		□ NO unused code (delete it)

		═══════════════════════════════════════════════════════════════════════════════

		CURRENT STATE:
		- Iteration: {{iteration}}
		- Successful implementations: {{green_count}}
		- TCR Commits: {{commit_count}}
		- TCR Reverts: {{revert_count}}

		ORIGINAL REQUIREMENTS:
		{{requirements}}

		OUTPUT:
		1. Lines of code BEFORE refactoring (count them)
		2. What you DELETED and why
		3. What you SIMPLIFIED and why
		4. Lines of code AFTER refactoring
		5. Net change (should be NEGATIVE or zero)
		6. Test confirmation
		"""
}

#ReviewerRole: {
	description: """
		The reviewer does final polish before push.
		Focuses on production readiness, security, documentation.
		Uses API model (Haiku 4.5) for fresh perspective.
		"""

	model:         "haiku-4.5"
	model_endpoint: "https://api.anthropic.com"
	allowed_tools: ["Read", "Write", "Edit", "Glob", "Grep", "Bash(gleam:*)", "Bash(cargo:*)"]
	filesystem: {
		src:  "read-write"
		test: "read-write"
	}

	system_prompt_addition: """
		You are the REVIEWER doing final review.
		You may modify both src/ and test/.
		All tests MUST pass.
		Focus on production readiness and polish.
		"""

	prompt_template: """
		You are the REVIEWER. The TDD-TCR-REFACTOR loop has completed.

		PROJECT: {{language}} project
		TEST COMMAND: {{test_command}}

		ORIGINAL REQUIREMENTS:
		{{requirements}}

		FINAL STATISTICS:
		- Total iterations: {{iteration}}
		- TCR Commits: {{commit_count}}
		- TCR Reverts: {{revert_count}}

		YOUR TASK:
		1. Final review of all code in src/ and test/
		2. Check that all requirements are met
		3. Look for any remaining issues:
		   - Security concerns
		   - Edge cases not covered by tests
		   - Documentation gaps
		   - API ergonomics
		4. Make final polish improvements
		5. Run tests to verify everything still passes

		CONSTRAINTS:
		- You may modify both src/ and test/
		- All tests MUST still pass after your changes
		- This is the FINAL review - be thorough
		- Focus on production readiness

		OUTPUT:
		1. Summary of what was built
		2. Any final changes made
		3. Any remaining concerns or TODOs
		"""
}

// ============================================================================
// VERIFICATION GAUNTLET
// ============================================================================

#VerificationGauntlet: {
	description: """
		The full verification pipeline that all code must pass.
		Runs after TCR commit in Factory loops.
		Any failure causes TCR revert.
		"""

	stages: #VerificationStages
}

#VerificationStages: {
	// Order matters - early stages are cheaper
	compile: {
		order:       1
		description: "Code compiles without errors"
		command:     "gleam build" | "cargo build"
		on_fail:     "revert"
	}

	unit_test: {
		order:       2
		description: "Unit tests pass"
		command:     "gleam test" | "cargo test --lib"
		on_fail:     "revert"
	}

	integration_test: {
		order:       3
		description: "Integration tests pass"
		command:     "gleam test" | "cargo test --test '*'"
		on_fail:     "revert"
		returns:     "coverage_data"
	}

	coverage: {
		order:       4
		description: "Coverage meets threshold"
		threshold:   80  // percent
		type:        "integration"  // integration > unit
		on_fail:     "revert"
	}

	lint: {
		order:       5
		description: "Code passes linting"
		command:     "gleam format --check" | "cargo clippy -- -D warnings"
		on_fail:     "revert"
	}

	mutation_test: {
		order:       6
		description: "Mutation testing (tests catch mutants)"
		enabled:     false  // TODO: Find Gleam mutation testing tool
		on_fail:     "warn"  // Don't revert, just warn
	}

	llm_judge: {
		order:       7
		description: "LLM reviews code quality"
		model:       "haiku-4.5"
		prompt: """
			Review this code change for quality issues.
			
			CHANGES:
			{{diff}}
			
			Check for:
			- Logic errors
			- Edge cases not handled
			- Security issues
			- Performance problems
			- Readability issues
			
			Output: PASS or FAIL with reasons.
			"""
		on_fail:     "revert"
	}

	cheat_detection: {
		order:       8
		description: "Detect AI cheating patterns"
		patterns: [
			{name: "deleted_tests", pattern: "removed test functions"}
			{name: "hardcoded_values", pattern: "hardcoded expected values in implementation"}
			{name: "ignored_errors", pattern: "@ts-ignore, # type: ignore, etc."}
			{name: "commented_code", pattern: "commented out failing code"}
			{name: "empty_catch", pattern: "empty catch/rescue blocks"}
		]
		on_fail: "revert"
	}

	architectural_review: {
		order:       9
		description: "Architectural review for significant changes"
		model:       "opus-4.5"
		trigger:     "lines_changed > 100 OR new_modules > 0"
		prompt: """
			Review this code for architectural quality.
			Apply Dave Farley's Modern Software Engineering principles.
			Apply CUPID properties.
			
			CHANGES:
			{{diff}}
			
			CURRENT ARCHITECTURE:
			{{module_tree}}
			
			Check for:
			- Separation of concerns
			- Dependency direction (inward)
			- Functional core / imperative shell
			- Testability
			- Coupling and cohesion
			
			Output: PASS or FAIL with architectural concerns.
			"""
		on_fail: "revert"
	}
}

// ============================================================================
// ACP (AGENT CLIENT PROTOCOL) - OPENCODE INTEGRATION
// ============================================================================

#ACP: {
	description: """
		Agent Client Protocol - JSON-RPC 2.0 over stdin/stdout.
		Used to communicate with opencode subprocess.
		Bidirectional: we send requests, opencode can also request from us.
		"""

	transport: {
		type:     "stdio"
		encoding: "nd-json"  // newline-delimited JSON
		protocol: "json-rpc-2.0"
	}

	// Messages we send to opencode
	client_methods: {
		initialize: {
			params: {
				protocolVersion: int
				capabilities:    #ClientCapabilities
			}
			result: {
				protocolVersion:   int
				agentCapabilities: #AgentCapabilities
			}
		}

		"session/new": {
			params: {}
			result: {
				sessionId: string
			}
		}

		"session/prompt": {
			params: {
				sessionId: string
				prompt:    [...#ContentBlock]
			}
			result: {
				stopReason: "end_turn" | "max_tokens" | "cancelled"
			}
		}

		"session/cancel": {
			// Notification, no result
			params: {
				sessionId: string
			}
		}
	}

	// Notifications we receive from opencode
	agent_notifications: {
		"session/update": {
			params: {
				sessionId: string
				update:    #SessionUpdate
			}
		}
	}

	// Requests opencode makes to us
	agent_requests: {
		"fs/read_text_file": {
			params: {
				path: string
			}
			result: {
				content: string
			}
		}

		"fs/write_text_file": {
			params: {
				path:    string
				content: string
			}
			result: {}
		}

		"permission/request": {
			params: {
				tool:   string
				reason: string
			}
			result: {
				granted: bool
			}
		}
	}
}

#ContentBlock: {
	type: "text"
	text: string
} | {
	type: "image"
	data: string  // base64
	mediaType: string
}

#SessionUpdate: {
	type: "agent_message"
	text: string
} | {
	type: "thought"
	text: string
} | {
	type: "tool_call_start"
	callId: string
	tool:   string
} | {
	type: "tool_call_update"
	callId: string
	status: "running" | "completed" | "failed"
}

#ClientCapabilities: {
	filesystem?: {
		read:  bool
		write: bool
	}
	terminal?: {
		create: bool
		kill:   bool
	}
}

#AgentCapabilities: {
	streaming: bool
	tools:     [...string]
}

// ============================================================================
// WORKSPACE MANAGEMENT
// ============================================================================

#WorkspaceManager: {
	description: """
		Manages isolated workspaces for agents.
		Uses reflink (copy-on-write) for instant, zero-cost clones.
		Workspaces are ephemeral - destroyed after use.
		"""

	golden_master: {
		path:        string | *"/data/golden-master"
		description: "The source of truth, always-current, pre-baked"
		contents: {
			deps:     "downloaded and cached"
			build:    "compiled"
			jj:       "initialized, clean state"
		}
		refresh_trigger: ["PatchAccepted", "external_commit", "timer(1h)"]
	}

	workspace_locations: {
		synapse_mutators: "/dev/shm/mutator-*"    // RAM disk, fast
		factory_loops:    "/dev/shm/factory-*"    // RAM disk, fast
	}

	operations: {
		create_reflink: {
			command:  "cp --reflink=always -r {{source}} {{dest}}"
			duration: "<50ms for typical project"
			cost:     "~0 bytes (copy-on-write)"
		}

		destroy: {
			command: "rm -rf {{workspace}}"
		}

		run_tests: {
			command: "cd {{workspace}} && gleam test"
		}

		apply_patch: {
			command: "cd {{workspace}} && patch -p1 < {{patch_file}}"
		}

		get_hash: {
			command: "cd {{workspace}} && jj log -r @ --no-graph -T 'commit_id'"
		}
	}
}

// ============================================================================
// JJ (JUJUTSU) OPERATIONS
// ============================================================================

#JJOperations: {
	description: """
		Jujutsu is used for version control. Simpler than git for our use case.
		Each workspace has its own .jj database (no lock contention).
		"""

	commands: {
		commit: {
			command: "jj commit -m '{{message}}'"
			usage:   "After successful test pass"
		}

		restore: {
			command: "jj restore --from @-"
			usage:   "TCR revert - discard current changes"
		}

		push: {
			command: "jj git push --remote origin"
			usage:   "Push to central hub"
			errors: {
				non_fast_forward: "Conflict - need to rebase"
				other:            "Network or permission error"
			}
		}

		fetch: {
			command: "jj git fetch"
			usage:   "Get latest from hub"
		}

		rebase: {
			command: "jj rebase -d main@origin"
			usage:   "Rebase on latest main"
		}

		diff: {
			command: "jj diff --git"
			usage:   "Get diff for review"
		}
	}

	central_hub: {
		path:        "/data/project-hub"
		type:        "bare repository"
		description: "Central source of truth, receives pushes from loops"
	}
}

// ============================================================================
// MERGE QUEUE (ABSORBER)
// ============================================================================

#MergeQueue: {
	description: """
		The merge queue absorbs valid patches into the golden master.
		For Synapse: first valid patch wins.
		For Factory: optimistic push with rebase-retry.
		"""

	synapse_strategy: {
		name: "first-wins"
		flow: """
			1. Receive PatchProposed signal
			2. If already absorbing: ignore
			3. Apply patch to golden master
			4. Run tests
			5. If pass: commit, push, broadcast PatchAccepted
			6. If fail: revert, ignore
			"""
		concurrency: "single - lock during absorption"
	}

	factory_strategy: {
		name: "optimistic-push-rebase-retry"
		flow: """
			1. Loop completes verification
			2. Attempt push: jj git push
			3. If success: done, broadcast LoopComplete
			4. If conflict: fetch, rebase, re-verify, retry push
			5. If rebase conflict: mark as failed, needs human
			"""
		max_push_attempts: 5
		rebase_backoff_ms: [100, 200, 400, 800, 1600]  // Exponential
	}
}

// ============================================================================
// RESOURCE GOVERNOR
// ============================================================================

#ResourceGovernor: {
	description: """
		Prevents system overload by limiting concurrent resources.
		All spawn requests must go through the governor.
		"""

	limits: {
		max_synapse_mutators: int | *32
		max_factory_loops:    int | *10
		max_total_workspaces: int | *50
		min_free_ram_mb:      int | *8000
		gpu_ticket_count:     int | *4
	}

	state: {
		active_mutators:       int
		active_loops:          int
		active_workspaces:     int
		gpu_tickets_available: int
	}

	operations: {
		request_mutator_slot: {
			checks: [
				"active_mutators < max_synapse_mutators",
				"active_workspaces < max_total_workspaces",
				"free_ram_mb > min_free_ram_mb",
			]
			on_success: "increment counters, return Ok"
			on_fail:    "return Error with reason"
		}

		request_loop_slot: {
			checks: [
				"active_loops < max_factory_loops",
				"active_workspaces < max_total_workspaces",
				"free_ram_mb > min_free_ram_mb",
			]
			on_success: "increment counters, return Ok"
			on_fail:    "return Error with reason"
		}

		request_gpu_ticket: {
			checks: ["gpu_tickets_available > 0"]
			on_success: "decrement available, return Ok"
			on_fail:    "return Error(no tickets)"
		}

		release_mutator_slot: "decrement active_mutators, active_workspaces"
		release_loop_slot:    "decrement active_loops, active_workspaces"
		release_gpu_ticket:   "increment gpu_tickets_available"
	}
}

// ============================================================================
// LLM ROUTER
// ============================================================================

#LLMRouter: {
	description: """
		Routes LLM requests to appropriate endpoints based on role and task.
		Local inference for heavy lifting, API for reviews and quality gates.
		"""

	endpoints: {
		local: {
			url:         "http://localhost:8080"
			models:      ["qwen-32b", "qwen-coder-1.5b"]
			provider:    "llama.cpp"
			cost:        "zero (electricity only)"
			latency:     "~500ms for 32B, ~50ms for 1.5B batch"
		}
		anthropic: {
			url:         "https://api.anthropic.com"
			models:      ["opus-4.5", "haiku-4.5"]
			cost:        "$5/$25 per MTok (opus), $0.25/$1.25 (haiku)"
			latency:     "~1s"
		}
	}

	routing: {
		synapse_mutator: {
			model:    "qwen-coder-1.5b"
			endpoint: "local"
			reason:   "cheap batch inference for swarm"
		}
		factory_auditor: {
			model:    "qwen-32b"
			endpoint: "local"
			reason:   "capable model for test design"
		}
		factory_implementer: {
			model:    "qwen-32b"
			endpoint: "local"
			reason:   "capable model for implementation"
		}
		factory_architect: {
			model:    "qwen-32b"
			endpoint: "local"
			reason:   "capable model for refactoring"
		}
		factory_reviewer: {
			model:    "haiku-4.5"
			endpoint: "anthropic"
			reason:   "fresh perspective, production polish"
		}
		llm_judge: {
			model:    "haiku-4.5"
			endpoint: "anthropic"
			reason:   "quality gate, cheap enough for every check"
		}
		architectural_review: {
			model:    "opus-4.5"
			endpoint: "anthropic"
			reason:   "big decisions need best model"
		}
	}

	gpu_management: {
		batch_size: 32  // For Synapse mutators
		ticket_system: {
			description: "Limit concurrent GPU access"
			tickets:     4
			timeout_ms:  30000
		}
	}
}

// ============================================================================
// BEADS INTEGRATION
// ============================================================================

#BeadsIntegration: {
	description: """
		Beads is a git-backed task tracker for AI agents.
		Tasks live in .beads/ directory as JSON files.
		The system watches for new beads and assigns them to Factory loops.
		"""

	directory: ".beads/"

	task_states: ["open", "in_progress", "done", "failed"]

	task_schema: {
		id:           string
		title:        string
		description:  string
		requirements: string
		priority:     #Priority
		state:        "open" | "in_progress" | "done" | "failed"
		assigned_to?: string
		created_at:   time.Time
		updated_at:   time.Time
	}

	operations: {
		watch: {
			description: "Watch for new open beads"
			action:      "On new open bead, broadcast BeadAssigned signal"
		}
		claim: {
			description: "Mark bead as in_progress"
			action:      "Update state, set assigned_to to loop_id"
		}
		complete: {
			description: "Mark bead as done"
			action:      "Update state, record completion stats"
		}
		fail: {
			description: "Mark bead as failed"
			action:      "Update state, record failure reason"
		}
	}
}

// ============================================================================
// ZELLIJ DASHBOARD
// ============================================================================

#ZellijDashboard: {
	description: """
		Zellij terminal multiplexer provides the monitoring UI.
		BEAM controls Zellij panes via IPC or file watchers.
		Bio-metrics view instead of traditional logs.
		"""

	control_method: "ipc" | "file_watcher"

	layout: {
		pane_signal_bus: {
			position:    "top"
			height:      "30%"
			content:     "scrolling list of signals"
			format:      "[{{timestamp}}] {{signal_type}}: {{summary}}"
		}

		pane_active_loops: {
			position: "middle-left"
			width:    "50%"
			height:   "40%"
			content:  "table of active factory loops"
			columns: ["loop_id", "phase", "iteration", "commits", "reverts"]
		}

		pane_active_mutators: {
			position: "middle-right"
			width:    "50%"
			height:   "40%"
			content:  "swarm status for Synapse"
			format:   "{{active}}/{{spawned}} mutators | {{patches_proposed}} proposed"
		}

		pane_resources: {
			position: "bottom"
			height:   "15%"
			content:  "resource utilization"
			metrics: [
				"RAM: {{used_gb}}/{{total_gb}} GB",
				"GPU: {{tickets_used}}/{{tickets_total}} tickets",
				"Workspaces: {{active}}/{{max}}",
				"Loops: {{active_loops}}/{{max_loops}}",
			]
		}

		pane_metrics: {
			position: "bottom"
			height:   "15%"
			content:  "cumulative statistics"
			metrics: [
				"Commits: {{total_commits}}",
				"Reverts: {{total_reverts}}",
				"Patches absorbed: {{patches_absorbed}}",
				"Tasks completed: {{tasks_completed}}",
			]
		}
	}

	ipc_commands: {
		update_pane:  "zellij action write-chars --pane-id {{pane_id}} '{{content}}'"
		clear_pane:   "zellij action clear-screen --pane-id {{pane_id}}"
		focus_pane:   "zellij action focus-pane --pane-id {{pane_id}}"
		create_pane:  "zellij action new-pane --direction {{direction}}"
	}
}

// ============================================================================
// HARDWARE ASSUMPTIONS
// ============================================================================

#HardwareProfile: {
	description: "Hardware the system is designed for"

	cpu: {
		model:   "AMD 9950X3D"
		cores:   16
		threads: 32
	}

	ram: {
		size_gb: 128
		type:    "DDR5"
	}

	gpu: {
		primary: {
			model:     "RTX 5090"
			vram_gb:   32
			use:       "Primary inference (Qwen 32B)"
		}
		secondary: {
			model:     "RTX 3090"
			vram_gb:   24
			use:       "Batch inference (Qwen 1.5B × 32)"
		}
	}

	storage: {
		nvme: {
			type:       "Gen 5 NVMe"
			speed_gbps: 14
			use:        "Golden master, workspaces"
		}
		tmpfs: {
			path:    "/dev/shm"
			size_gb: 64
			use:     "Ephemeral workspaces (RAM disk)"
		}
	}

	resource_allocation: {
		synapse_batch: {
			gpu:    "3090"
			model:  "Qwen-Coder-1.5B"
			batch:  32
			vram:   "~8GB"
		}
		factory_inference: {
			gpu:    "5090"
			model:  "Qwen-32B"
			vram:   "~24GB"
		}
		workspace_ram: {
			per_workspace_mb: 50  // With reflinks
			max_workspaces:   50
			total_gb:         2.5
		}
	}
}

// ============================================================================
// STARTUP SEQUENCE
// ============================================================================

#StartupSequence: {
	description: "Order of operations when starting the system"

	steps: [
		{
			order: 1
			name:  "verify_hardware"
			action: "Check GPU, RAM, NVMe available"
		},
		{
			order: 2
			name:  "start_llama_cpp"
			action: "Start llama.cpp servers on both GPUs"
		},
		{
			order: 3
			name:  "prepare_golden_master"
			action: "Ensure golden master exists, deps cached, builds"
		},
		{
			order: 4
			name:  "start_resource_governor"
			action: "Initialize resource limits and tracking"
		},
		{
			order: 5
			name:  "start_signal_bus"
			action: "Start pubsub/registry"
		},
		{
			order: 6
			name:  "start_absorber"
			action: "Start merge queue watching for patches"
		},
		{
			order: 7
			name:  "start_heartbeat"
			action: "Start always-running test loop"
		},
		{
			order: 8
			name:  "start_synapse_reactor"
			action: "Subscribe to TestFailure signals"
		},
		{
			order: 9
			name:  "start_factory_dispatcher"
			action: "Subscribe to BeadAssigned signals"
		},
		{
			order: 10
			name:  "start_zellij_dashboard"
			action: "Connect to Zellij for monitoring"
		},
		{
			order: 11
			name:  "connect_beads"
			action: "Watch .beads/ for new tasks"
		},
		{
			order: 12
			name:  "system_ready"
			action: "Log startup complete, begin processing"
		},
	]
}

// ============================================================================
// FAILURE MODES AND MITIGATIONS
// ============================================================================

#FailureModes: {
	push_conflicts: {
		description:  "Multiple loops try to push simultaneously"
		likelihood:   "HIGH at scale"
		impact:       "MEDIUM - wastes compute"
		mitigation:   "Jittered exponential backoff on retry"
		recovery:     "Rebase and re-verify"
	}

	opencode_instability: {
		description:  "opencode crashes, hangs, or leaks memory"
		likelihood:   "MEDIUM"
		impact:       "HIGH - loop stuck"
		mitigation:   "Aggressive timeouts, health checks, process kill"
		recovery:     "Kill port, destroy workspace, retry or fail"
	}

	golden_master_staleness: {
		description:  "Loop working on outdated base"
		likelihood:   "MEDIUM at scale"
		impact:       "MEDIUM - wasted work"
		mitigation:   "Frequent refresh, Evolution signal kills stale work"
		recovery:     "Synapse dies on Evolution, Factory rebases"
	}

	gpu_contention: {
		description:  "Too many concurrent inference requests"
		likelihood:   "MEDIUM"
		impact:       "LOW - delays"
		mitigation:   "Ticket system limits concurrent access"
		recovery:     "Queue and wait"
	}

	ram_exhaustion: {
		description:  "Too many workspaces exhaust RAM"
		likelihood:   "LOW"
		impact:       "HIGH - system crash"
		mitigation:   "Governor checks free RAM before spawn"
		recovery:     "Reject spawn, wait for releases"
	}

	ai_cheating: {
		description:  "LLM games the system (delete tests, hardcode)"
		likelihood:   "MEDIUM"
		impact:       "HIGH - bad code ships"
		mitigation:   "Cheat detection stage in verification"
		recovery:     "Revert, add pattern to detector"
	}

	observability_gaps: {
		description:  "Can't see what's happening"
		likelihood:   "HIGH without investment"
		impact:       "MEDIUM - blind debugging"
		mitigation:   "Structured logging, Zellij dashboard"
		recovery:     "Keep failed workspaces for inspection"
	}
}

// ============================================================================
// IMPLEMENTATION LANGUAGES
// ============================================================================

#ImplementationLanguages: {
	orchestration: {
		language: "Gleam"
		runtime:  "BEAM/OTP"
		reason: [
			"Actor model for concurrent processes",
			"Supervision trees for fault tolerance",
			"Pattern matching for state machines",
			"Immutable by default",
			"Erlang FFI for ports",
		]
	}

	generated_code: {
		languages: ["Gleam", "Rust"]
		reason: [
			"Strong type systems catch errors at compile time",
			"Functional design principles",
			"No nulls, explicit error handling",
			"Fast feedback loops (compiler)",
		]
	}

	shell_scripts: {
		language: "Bash"
		use:      "Golden master maintenance, system setup"
	}
}

// ============================================================================
// EXTENSION POINTS
// ============================================================================

#ExtensionPoints: {
	new_signal_types: {
		how:     "Add to #Signals definition"
		example: "PRCreated, DeploymentFailed, etc."
	}

	new_verification_stages: {
		how:     "Add to #VerificationStages"
		example: "security_scan, performance_test, etc."
	}

	new_roles: {
		how:     "Add to #FactoryRoles"
		example: "security_reviewer, performance_tuner, etc."
	}

	new_llm_endpoints: {
		how:     "Add to #LLMRouter.endpoints"
		example: "ollama, vllm, together.ai, etc."
	}

	custom_prompts: {
		how:     "Modify role prompt_template"
		note:    "Use {{variable}} for interpolation"
	}
}

// ============================================================================
// VERSION AND METADATA
// ============================================================================

#Metadata: {
	version:    "1.0.0"
	created:    "2026-01-06"
	authors:    ["Lewis", "Claude"]
	license:    "MIT"
	repository: "github.com/lewis/factory-synapse"
	
	dependencies: {
		gleam:      ">=1.0.0"
		erlang_otp: ">=26"
		opencode:   ">=1.0.0"
		jj:         ">=0.20.0"
		llama_cpp:  ">=b3000"
		zellij:     ">=0.40.0"
	}
}
