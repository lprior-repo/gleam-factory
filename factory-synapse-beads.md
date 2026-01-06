# Factory-Synapse Implementation Beads

## Tier 0: Foundation - OTP Infrastructure (P0)

### fs-001: Add gleam_otp dependency
- **Type:** task
- **Priority:** P0
- **Description:** Add gleam_otp, gleam_erlang to gleam.toml for actor model support
- **Acceptance:** `gleam build` succeeds with OTP dependencies available

### fs-002: Create signal type definitions
- **Type:** task
- **Priority:** P0
- **Deps:** fs-001
- **Description:** Define all signal types from spec in src/signals.gleam: TestFailure, TestPassing, BeadAssigned, PatchProposed, PatchAccepted, PatchRejected, GoldenMasterUpdated, Evolution, LoopSpawned, LoopComplete, LoopFailed, ResourceExhausted
- **Acceptance:** All signal types compile with proper fields matching CUE spec

### fs-003: Create GitHash opaque type
- **Type:** task
- **Priority:** P0
- **Description:** Create validated GitHash type in src/types.gleam that enforces 40-char hex format
- **Acceptance:** GitHash.parse("abc123...") returns Result, invalid hashes rejected

### fs-004: Create Priority type
- **Type:** task
- **Priority:** P0
- **Description:** Create Priority variant type: Critical, High, Medium, Low in src/types.gleam
- **Acceptance:** Priority type with to_string/from_string functions

### fs-005: Create ProcessId opaque type
- **Type:** task
- **Priority:** P0
- **Description:** Create ProcessId type wrapping Erlang pid for type safety
- **Acceptance:** ProcessId with from_pid/to_pid functions

---

## Tier 0: Foundation - Signal Bus (P0)

### fs-010: Create signal_bus gen_server skeleton
- **Type:** task
- **Priority:** P0
- **Deps:** fs-001, fs-002
- **Description:** Create src/signal_bus.gleam with gleam_otp gen_server for pub/sub. Init, handle_call, handle_cast stubs.
- **Acceptance:** signal_bus.start_link() returns Ok(pid)

### fs-011: Implement signal_bus subscribe
- **Type:** task
- **Priority:** P0
- **Deps:** fs-010
- **Description:** Add subscribe(signal_type, callback_pid) to signal_bus. Store subscriptions in state dict.
- **Acceptance:** Process can subscribe to specific signal types

### fs-012: Implement signal_bus broadcast
- **Type:** task
- **Priority:** P0
- **Deps:** fs-011
- **Description:** Add broadcast(signal) that sends message to all subscribers of that signal type
- **Acceptance:** Broadcast reaches all subscribers, verified with test

### fs-013: Implement signal_bus unsubscribe
- **Type:** task
- **Priority:** P0
- **Deps:** fs-011
- **Description:** Add unsubscribe(signal_type, pid) to remove subscription. Handle process death cleanup.
- **Acceptance:** Unsubscribed processes don't receive broadcasts

### fs-014: Add signal_bus supervision
- **Type:** task
- **Priority:** P0
- **Deps:** fs-010
- **Description:** Create supervisor for signal_bus with restart strategy. Add to application supervision tree.
- **Acceptance:** Signal bus restarts on crash, subscriptions survive

---

## Tier 0: Foundation - Workspace Manager (P0)

### fs-020: Create workspace_manager gen_server skeleton
- **Type:** task
- **Priority:** P0
- **Deps:** fs-001
- **Description:** Create src/workspace_manager.gleam gen_server. Track active workspaces in state.
- **Acceptance:** workspace_manager.start_link() returns Ok(pid)

### fs-021: Implement create_workspace with jj
- **Type:** task
- **Priority:** P0
- **Deps:** fs-020
- **Description:** Add create_workspace(slug, source_path) using existing jj workspace logic. Return Workspace record.
- **Acceptance:** Creates isolated jj workspace, returns path

### fs-022: Implement create_workspace with reflink
- **Type:** task
- **Priority:** P0
- **Deps:** fs-020
- **Description:** Add create_workspace_reflink(slug, source_path, dest_base) using `cp --reflink=always`. Target /dev/shm for RAM disk.
- **Acceptance:** Creates COW copy in <50ms, verifiable with stat

### fs-023: Implement destroy_workspace
- **Type:** task
- **Priority:** P0
- **Deps:** fs-021, fs-022
- **Description:** Add destroy_workspace(workspace_id) that removes directory and updates state tracking
- **Acceptance:** Workspace fully cleaned up, state updated

### fs-024: Implement list_workspaces
- **Type:** task
- **Priority:** P0
- **Deps:** fs-020
- **Description:** Add list_workspaces() returning all active workspace records with metadata
- **Acceptance:** Returns list of Workspace records with paths, types, owners

### fs-025: Add workspace config toggle
- **Type:** task
- **Priority:** P0
- **Deps:** fs-021, fs-022
- **Description:** Add config option to choose workspace strategy: jj | reflink | auto. Auto prefers reflink if /dev/shm available.
- **Acceptance:** Config toggle works, auto-detection functions

### fs-026: Add workspace supervision
- **Type:** task
- **Priority:** P0
- **Deps:** fs-020
- **Description:** Add supervisor for workspace_manager. Handle orphan cleanup on restart.
- **Acceptance:** Manager restarts cleanly, orphan workspaces detected

---

## Tier 1: Platform - Resource Governor (P1)

### fs-030: Create resource_governor gen_server skeleton
- **Type:** task
- **Priority:** P1
- **Deps:** fs-001
- **Description:** Create src/resource_governor.gleam gen_server with limits state: max_mutators, max_loops, max_workspaces, min_free_ram_mb, gpu_tickets
- **Acceptance:** resource_governor.start_link(config) returns Ok(pid)

### fs-031: Implement request_mutator_slot
- **Type:** task
- **Priority:** P1
- **Deps:** fs-030
- **Description:** Add request_mutator_slot() -> Result(slot_id, ResourceError). Check limits, increment counter atomically.
- **Acceptance:** Returns Ok when under limit, Error when exhausted

### fs-032: Implement request_loop_slot
- **Type:** task
- **Priority:** P1
- **Deps:** fs-030
- **Description:** Add request_loop_slot() -> Result(slot_id, ResourceError). Check limits for factory loops.
- **Acceptance:** Returns Ok when under limit, Error when exhausted

### fs-033: Implement release_slot
- **Type:** task
- **Priority:** P1
- **Deps:** fs-031, fs-032
- **Description:** Add release_slot(slot_id) to decrement counters. Handle double-release gracefully.
- **Acceptance:** Counter decrements, double-release is no-op

### fs-034: Implement RAM check
- **Type:** task
- **Priority:** P1
- **Deps:** fs-030
- **Description:** Add check_free_ram() using Erlang memsup or /proc/meminfo. Block spawns when below threshold.
- **Acceptance:** Returns free RAM in MB, threshold check works

### fs-035: Implement GPU ticket system
- **Type:** task
- **Priority:** P1
- **Deps:** fs-030
- **Description:** Add request_gpu_ticket() / release_gpu_ticket() for limiting concurrent GPU inference
- **Acceptance:** Ticket acquisition blocks when exhausted, releases work

### fs-036: Broadcast ResourceExhausted signal
- **Type:** task
- **Priority:** P1
- **Deps:** fs-030, fs-012
- **Description:** Broadcast ResourceExhausted signal when any limit is hit
- **Acceptance:** Signal broadcast includes resource type, current, limit

---

## Tier 1: Platform - Golden Master (P1)

### fs-040: Create golden_master gen_server skeleton
- **Type:** task
- **Priority:** P1
- **Deps:** fs-001, fs-010
- **Description:** Create src/golden_master.gleam gen_server tracking canonical source path and current hash
- **Acceptance:** golden_master.start_link(path) returns Ok(pid)

### fs-041: Implement get_hash
- **Type:** task
- **Priority:** P1
- **Deps:** fs-040
- **Description:** Add get_hash() returning current commit hash of golden master via jj/git
- **Acceptance:** Returns 40-char GitHash

### fs-042: Implement refresh
- **Type:** task
- **Priority:** P1
- **Deps:** fs-040
- **Description:** Add refresh() that pulls latest, rebuilds deps, broadcasts GoldenMasterUpdated if changed
- **Acceptance:** Golden master updated, signal broadcast on change

### fs-043: Subscribe to PatchAccepted for auto-refresh
- **Type:** task
- **Priority:** P1
- **Deps:** fs-040, fs-012
- **Description:** Golden master subscribes to PatchAccepted signal and auto-refreshes
- **Acceptance:** Patch acceptance triggers refresh automatically

---

## Tier 1: Platform - LLM Router (P1)

### fs-050: Create llm_router gen_server skeleton
- **Type:** task
- **Priority:** P1
- **Deps:** fs-001
- **Description:** Create src/llm_router.gleam gen_server with endpoint configs for local and anthropic
- **Acceptance:** llm_router.start_link(config) returns Ok(pid)

### fs-051: Define LLMRequest and LLMResponse types
- **Type:** task
- **Priority:** P1
- **Deps:** fs-050
- **Description:** Create LLMRequest{model, prompt, max_tokens, temperature} and LLMResponse{content, usage} types
- **Acceptance:** Types defined, JSON encoders/decoders work

### fs-052: Implement route_request
- **Type:** task
- **Priority:** P1
- **Deps:** fs-050, fs-051
- **Description:** Add route_request(role, request) that routes to correct endpoint based on role (auditor->local, reviewer->anthropic)
- **Acceptance:** Requests routed correctly per role

### fs-053: Implement local llama.cpp client
- **Type:** task
- **Priority:** P1
- **Deps:** fs-051
- **Description:** Create HTTP client for llama.cpp server at localhost:8080. Handle /completion endpoint.
- **Acceptance:** Can send prompt, receive completion from local server

### fs-054: Implement Anthropic API client
- **Type:** task
- **Priority:** P1
- **Deps:** fs-051
- **Description:** Create HTTP client for api.anthropic.com. Handle authentication, messages endpoint.
- **Acceptance:** Can send prompt to Claude, receive response

### fs-055: Add GPU ticket integration
- **Type:** task
- **Priority:** P1
- **Deps:** fs-052, fs-035
- **Description:** LLM router acquires GPU ticket before local inference, releases after
- **Acceptance:** Local requests blocked when no GPU tickets available

---

## Tier 1: Platform - ACP Protocol (P1)

### fs-060: Create acp_types module
- **Type:** task
- **Priority:** P1
- **Description:** Create src/acp_types.gleam with JSON-RPC 2.0 types: Request, Response, Notification, Error per ACP spec
- **Acceptance:** All ACP message types defined with JSON codecs

### fs-061: Create acp_transport module
- **Type:** task
- **Priority:** P1
- **Deps:** fs-060
- **Description:** Create src/acp_transport.gleam for newline-delimited JSON over stdin/stdout
- **Acceptance:** Can read/write nd-json messages

### fs-062: Create acp_client gen_server skeleton
- **Type:** task
- **Priority:** P1
- **Deps:** fs-001, fs-060, fs-061
- **Description:** Create src/acp_client.gleam gen_server that spawns opencode subprocess and manages communication
- **Acceptance:** acp_client.start_link(opencode_path) spawns process

### fs-063: Implement initialize handshake
- **Type:** task
- **Priority:** P1
- **Deps:** fs-062
- **Description:** Implement ACP initialize request/response with protocol version and capabilities exchange
- **Acceptance:** Handshake completes, capabilities stored

### fs-064: Implement session/new
- **Type:** task
- **Priority:** P1
- **Deps:** fs-063
- **Description:** Implement session/new to create new opencode session, store session_id
- **Acceptance:** Session created, ID returned

### fs-065: Implement session/prompt
- **Type:** task
- **Priority:** P1
- **Deps:** fs-064
- **Description:** Implement session/prompt to send prompts and receive responses via streaming updates
- **Acceptance:** Can send prompt, receive tool calls and final response

### fs-066: Implement session/cancel
- **Type:** task
- **Priority:** P1
- **Deps:** fs-064
- **Description:** Implement session/cancel notification to abort running prompts
- **Acceptance:** Cancel stops in-progress generation

### fs-067: Handle session/update notifications
- **Type:** task
- **Priority:** P1
- **Deps:** fs-062
- **Description:** Handle incoming session/update notifications (agent_message, thought, tool_call_start, tool_call_update)
- **Acceptance:** Updates parsed and available to caller

### fs-068: Implement fs/read_text_file handler
- **Type:** task
- **Priority:** P1
- **Deps:** fs-062
- **Description:** Handle incoming fs/read_text_file requests from opencode, read file, return content
- **Acceptance:** File reads work, errors handled

### fs-069: Implement fs/write_text_file handler
- **Type:** task
- **Priority:** P1
- **Deps:** fs-062
- **Description:** Handle incoming fs/write_text_file requests from opencode with filesystem enforcement
- **Acceptance:** File writes work with proper path validation

### fs-070: Implement permission/request handler
- **Type:** task
- **Priority:** P1
- **Deps:** fs-062
- **Description:** Handle incoming permission/request with auto-grant for allowed tools
- **Acceptance:** Permissions granted/denied based on role config

---

## Tier 2: Factory Core - Loop State Machine (P1)

### fs-100: Create factory_loop_state type
- **Type:** task
- **Priority:** P1
- **Deps:** fs-002
- **Description:** Create FactoryLoopState type in src/factory_loop.gleam: loop_id, task_id, task_spec, workspace_path, phase, iteration, green_count, commit_count, revert_count, history, last_feedback
- **Acceptance:** State type defined matching CUE spec

### fs-101: Create factory_phase type
- **Type:** task
- **Priority:** P1
- **Description:** Create FactoryPhase variant: Auditing, VerifyingRed, Implementing, TcrChecking, Verifying, Refactoring, Reviewing, Pushing, Rebasing, Complete, Failed
- **Acceptance:** All phases defined as variants

### fs-102: Create history_entry type
- **Type:** task
- **Priority:** P1
- **Description:** Create HistoryEntry type: iteration, role, content, timestamp
- **Acceptance:** History entries track all role outputs

### fs-103: Create factory_loop gen_server skeleton
- **Type:** task
- **Priority:** P1
- **Deps:** fs-001, fs-100, fs-101
- **Description:** Create src/factory_loop.gleam gen_server with state machine. Init with BeadAssigned data.
- **Acceptance:** factory_loop.start_link(bead_assigned) returns Ok(pid)

### fs-104: Implement phase transition logic
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103
- **Description:** Add transition(from_phase, event) -> to_phase function implementing state machine from CUE spec
- **Acceptance:** All transitions match spec exactly

### fs-105: Broadcast LoopSpawned on start
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-012
- **Description:** Broadcast LoopSpawned signal when factory loop starts with loop_id, task_id, initial phase
- **Acceptance:** Signal broadcast on spawn

### fs-106: Broadcast LoopComplete on success
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-012
- **Description:** Broadcast LoopComplete signal when loop reaches Complete phase with stats
- **Acceptance:** Signal includes commits, reverts, duration_ms

### fs-107: Broadcast LoopFailed on failure
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-012
- **Description:** Broadcast LoopFailed signal when loop reaches Failed phase with reason
- **Acceptance:** Signal includes loop_id and failure reason

---

## Tier 2: Factory Core - Auditor Role (P1)

### fs-110: Create auditor_role module
- **Type:** task
- **Priority:** P1
- **Deps:** fs-051
- **Description:** Create src/roles/auditor.gleam with role config: model, allowed_tools, filesystem locks
- **Acceptance:** Auditor role config matches CUE spec

### fs-111: Create auditor system prompt
- **Type:** task
- **Priority:** P1
- **Deps:** fs-110
- **Description:** Implement system_prompt_addition from CUE spec emphasizing test-only modifications
- **Acceptance:** System prompt enforces test/ only access

### fs-112: Create auditor prompt template
- **Type:** task
- **Priority:** P1
- **Deps:** fs-110
- **Description:** Implement full prompt_template from CUE spec with all placeholders
- **Acceptance:** Template has {{language}}, {{test_command}}, {{requirements}}, {{iteration}}, {{refactor_interval}}, {{last_feedback}}

### fs-113: Implement auditor execution
- **Type:** task
- **Priority:** P1
- **Deps:** fs-110, fs-065
- **Description:** Create run_auditor(state) that sends prompt via ACP, parses response, returns new test or REQUIREMENTS_COMPLETE
- **Acceptance:** Auditor creates tests or signals completion

### fs-114: Implement filesystem lock enforcement for auditor
- **Type:** task
- **Priority:** P1
- **Deps:** fs-069
- **Description:** Configure fs/write handler to reject writes to src/ when in auditor role
- **Acceptance:** Writes to src/ fail, writes to test/ succeed

---

## Tier 2: Factory Core - Implementer Role (P1)

### fs-120: Create implementer_role module
- **Type:** task
- **Priority:** P1
- **Deps:** fs-051
- **Description:** Create src/roles/implementer.gleam with role config: model, allowed_tools, filesystem locks
- **Acceptance:** Implementer role config matches CUE spec

### fs-121: Create implementer system prompt
- **Type:** task
- **Priority:** P1
- **Deps:** fs-120
- **Description:** Implement system_prompt_addition emphasizing TCR discipline and minimum code
- **Acceptance:** System prompt warns about revert on failure

### fs-122: Create implementer prompt template
- **Type:** task
- **Priority:** P1
- **Deps:** fs-120
- **Description:** Implement full prompt_template with {{auditor_reasoning}}, {{test_output}}, {{attempt}}, {{max_attempts}}
- **Acceptance:** Template includes all placeholders from spec

### fs-123: Implement implementer execution
- **Type:** task
- **Priority:** P1
- **Deps:** fs-120, fs-065
- **Description:** Create run_implementer(state, auditor_reasoning, test_output) that implements minimum code to pass
- **Acceptance:** Implementer modifies src/ to pass failing test

### fs-124: Implement filesystem lock enforcement for implementer
- **Type:** task
- **Priority:** P1
- **Deps:** fs-069
- **Description:** Configure fs/write handler to reject writes to test/ when in implementer role
- **Acceptance:** Writes to test/ fail, writes to src/ succeed

---

## Tier 2: Factory Core - Architect Role (P1)

### fs-130: Create architect_role module
- **Type:** task
- **Priority:** P1
- **Deps:** fs-051
- **Description:** Create src/roles/architect.gleam with role config allowing both src/ and test/ access
- **Acceptance:** Architect role config matches CUE spec

### fs-131: Create architect system prompt
- **Type:** task
- **Priority:** P1
- **Deps:** fs-130
- **Description:** Implement system_prompt_addition with CUPID principles, code minimization focus
- **Acceptance:** System prompt emphasizes refactoring quality

### fs-132: Create architect prompt template
- **Type:** task
- **Priority:** P1
- **Deps:** fs-130
- **Description:** Implement full prompt_template with session_history, hard limits, CUPID properties, line count tracking
- **Acceptance:** Template includes all sections from spec

### fs-133: Implement architect execution
- **Type:** task
- **Priority:** P1
- **Deps:** fs-130, fs-065
- **Description:** Create run_architect(state, session_history) that refactors code while maintaining tests
- **Acceptance:** Architect refactors, reports line count delta

### fs-134: Architect triggers on green_count interval
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-130
- **Description:** Phase transition to Refactoring when green_count >= refactor_interval (default 3)
- **Acceptance:** Refactoring triggered every N green cycles

---

## Tier 2: Factory Core - Reviewer Role (P1)

### fs-140: Create reviewer_role module
- **Type:** task
- **Priority:** P1
- **Deps:** fs-051
- **Description:** Create src/roles/reviewer.gleam with API model config (haiku-4.5)
- **Acceptance:** Reviewer role config uses Anthropic API

### fs-141: Create reviewer system prompt
- **Type:** task
- **Priority:** P1
- **Deps:** fs-140
- **Description:** Implement system_prompt_addition focusing on production readiness and polish
- **Acceptance:** System prompt emphasizes final review quality

### fs-142: Create reviewer prompt template
- **Type:** task
- **Priority:** P1
- **Deps:** fs-140
- **Description:** Implement full prompt_template with requirements, final statistics
- **Acceptance:** Template includes all placeholders from spec

### fs-143: Implement reviewer execution
- **Type:** task
- **Priority:** P1
- **Deps:** fs-140, fs-054
- **Description:** Create run_reviewer(state) that does final polish via Anthropic API
- **Acceptance:** Reviewer completes final review, reports issues

---

## Tier 2: Factory Core - TCR Enforcement (P1)

### fs-150: Implement tcr_checking phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103
- **Description:** In tcr_checking phase: run tests, commit on pass, revert on fail per existing tcr.gleam
- **Acceptance:** TCR discipline enforced after implementation

### fs-151: Track commit_count and revert_count
- **Type:** task
- **Priority:** P1
- **Deps:** fs-150, fs-100
- **Description:** Increment commit_count on successful TCR, revert_count on failure. Store in state.
- **Acceptance:** Counts accurate throughout loop lifecycle

### fs-152: Return to implementing on TCR failure
- **Type:** task
- **Priority:** P1
- **Deps:** fs-150
- **Description:** On TCR revert, transition back to Implementing phase for retry (up to max_impl_attempts)
- **Acceptance:** Retry happens, max attempts enforced

---

## Tier 2: Factory Core - Verification Gauntlet (P1)

### fs-160: Create verification_gauntlet module
- **Type:** task
- **Priority:** P1
- **Description:** Create src/verification_gauntlet.gleam with ordered stage pipeline
- **Acceptance:** Module structure ready for stages

### fs-161: Implement compile stage
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160
- **Description:** Add compile stage using existing stages.gleam gleam_implement
- **Acceptance:** Compile check runs, failures cause revert

### fs-162: Implement unit_test stage
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160
- **Description:** Add unit_test stage using existing stages.gleam gleam_unit_test
- **Acceptance:** Unit tests run, failures cause revert

### fs-163: Implement integration_test stage
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160
- **Description:** Add integration_test stage using existing stages.gleam gleam_integration
- **Acceptance:** Integration tests run, failures cause revert

### fs-164: Implement lint stage
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160
- **Description:** Add lint stage using existing stages.gleam gleam_lint
- **Acceptance:** Lint check runs, failures cause revert

### fs-165: Implement llm_judge stage
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160, fs-054
- **Description:** Add llm_judge stage that sends diff to haiku-4.5 for quality review
- **Acceptance:** LLM reviews diff, PASS/FAIL with reasons

### fs-166: Implement cheat_detection stage
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160
- **Description:** Add cheat_detection checking for: deleted_tests, hardcoded_values, ignored_errors, commented_code, empty_catch patterns
- **Acceptance:** Cheating patterns detected and cause revert

### fs-167: Implement architectural_review stage (conditional)
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160, fs-054
- **Description:** Add architectural_review stage triggered when lines_changed > 100 OR new_modules > 0. Uses opus-4.5.
- **Acceptance:** Big changes get architectural review

### fs-168: Wire gauntlet into verifying phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-160, fs-103
- **Description:** Verifying phase runs full gauntlet in order, any failure causes revert and return to implementing
- **Acceptance:** All stages run in order, failures handled

---

## Tier 2: Factory Core - Phase Implementations (P1)

### fs-170: Implement auditing phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-113
- **Description:** Auditing phase calls run_auditor, transitions to verifying_red or reviewing
- **Acceptance:** Auditor runs, REQUIREMENTS_COMPLETE detected

### fs-171: Implement verifying_red phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103
- **Description:** verifying_red phase runs tests expecting failure. Pass->auditing (test didn't fail), Fail->implementing
- **Acceptance:** Red verification works correctly

### fs-172: Implement implementing phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-123
- **Description:** Implementing phase calls run_implementer, transitions to tcr_checking
- **Acceptance:** Implementer runs, code modified

### fs-173: Implement refactoring phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-133
- **Description:** Refactoring phase calls run_architect, runs tests, reverts if fail, continues to auditing
- **Acceptance:** Architect refactors safely with TCR

### fs-174: Implement reviewing phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-143
- **Description:** Reviewing phase calls run_reviewer, transitions to pushing
- **Acceptance:** Reviewer completes, loop ready to push

### fs-175: Implement pushing phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103
- **Description:** Pushing phase executes jj git push. Success->complete, conflict->rebasing, error->failed
- **Acceptance:** Push works, conflicts detected

### fs-176: Implement rebasing phase
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103
- **Description:** Rebasing phase executes jj git fetch, jj rebase. Success->verifying, conflict->failed
- **Acceptance:** Rebase works, conflicts cause failure

---

## Tier 2: Factory Core - Beads Integration (P1)

### fs-180: Create beads_watcher gen_server skeleton
- **Type:** task
- **Priority:** P1
- **Deps:** fs-001
- **Description:** Create src/beads_watcher.gleam gen_server that watches .beads/ for changes
- **Acceptance:** Watcher starts and detects file changes

### fs-181: Implement bead parsing
- **Type:** task
- **Priority:** P1
- **Deps:** fs-180
- **Description:** Parse bead JSON/JSONL format to extract id, title, description, requirements, priority, state
- **Acceptance:** Beads parsed correctly from JSONL

### fs-182: Detect new open beads
- **Type:** task
- **Priority:** P1
- **Deps:** fs-181
- **Description:** When new bead with state=open detected, prepare to broadcast BeadAssigned
- **Acceptance:** New open beads detected automatically

### fs-183: Broadcast BeadAssigned signal
- **Type:** task
- **Priority:** P1
- **Deps:** fs-182, fs-012
- **Description:** Broadcast BeadAssigned signal with task_id, spec, requirements, priority
- **Acceptance:** Signal triggers factory loop spawning

### fs-184: Implement bead state updates
- **Type:** task
- **Priority:** P1
- **Deps:** fs-181
- **Description:** Add update_bead_state(id, new_state) to mark beads as in_progress, done, failed
- **Acceptance:** Bead state changes persisted

### fs-185: Claim bead on loop start
- **Type:** task
- **Priority:** P1
- **Deps:** fs-103, fs-184
- **Description:** When factory loop starts, update bead state to in_progress with assigned_to=loop_id
- **Acceptance:** Bead claimed, prevents duplicate work

### fs-186: Complete bead on loop success
- **Type:** task
- **Priority:** P1
- **Deps:** fs-106, fs-184
- **Description:** On LoopComplete, update bead state to done with completion stats
- **Acceptance:** Bead marked done with metrics

### fs-187: Fail bead on loop failure
- **Type:** task
- **Priority:** P1
- **Deps:** fs-107, fs-184
- **Description:** On LoopFailed, update bead state to failed with failure reason
- **Acceptance:** Bead marked failed with reason

---

## Tier 2: Factory Core - Factory Dispatcher (P1)

### fs-190: Create factory_dispatcher gen_server
- **Type:** task
- **Priority:** P1
- **Deps:** fs-001, fs-012
- **Description:** Create src/factory_dispatcher.gleam that subscribes to BeadAssigned signals
- **Acceptance:** Dispatcher running and subscribed

### fs-191: Spawn factory_loop on BeadAssigned
- **Type:** task
- **Priority:** P1
- **Deps:** fs-190, fs-103, fs-032
- **Description:** On BeadAssigned, request loop slot from governor, spawn factory_loop if granted
- **Acceptance:** Loops spawned with resource limiting

### fs-192: Handle loop completion cleanup
- **Type:** task
- **Priority:** P1
- **Deps:** fs-190, fs-106
- **Description:** On LoopComplete, release resources, clean up workspace
- **Acceptance:** Resources released, workspace destroyed

### fs-193: Handle loop failure cleanup
- **Type:** task
- **Priority:** P1
- **Deps:** fs-190, fs-107
- **Description:** On LoopFailed, release resources, optionally keep workspace for debugging
- **Acceptance:** Resources released, workspace optionally preserved

---

## Tier 3: Heartbeat (P2)

### fs-200: Create heartbeat gen_server skeleton
- **Type:** task
- **Priority:** P2
- **Deps:** fs-001, fs-040
- **Description:** Create src/heartbeat.gleam gen_server that polls golden master on interval
- **Acceptance:** Heartbeat starts with configurable interval

### fs-201: Implement test polling
- **Type:** task
- **Priority:** P2
- **Deps:** fs-200
- **Description:** Run test command against golden master on each tick, track last status
- **Acceptance:** Tests run on interval, status tracked

### fs-202: Broadcast TestFailure on red
- **Type:** task
- **Priority:** P2
- **Deps:** fs-201, fs-012
- **Description:** When tests fail (and were green), broadcast TestFailure with file, error, context_hash
- **Acceptance:** TestFailure signal broadcast on transition to red

### fs-203: Broadcast TestPassing on green
- **Type:** task
- **Priority:** P2
- **Deps:** fs-201, fs-012
- **Description:** When tests pass (and were red), broadcast TestPassing with hash
- **Acceptance:** TestPassing signal broadcast on transition to green

### fs-204: Handle different error detection
- **Type:** task
- **Priority:** P2
- **Deps:** fs-201
- **Description:** If tests fail with different error than before, broadcast new TestFailure
- **Acceptance:** New failures trigger new signals even if already red

---

## Tier 3: Merge Queue / Absorber (P2)

### fs-210: Create merge_queue gen_server skeleton
- **Type:** task
- **Priority:** P2
- **Deps:** fs-001, fs-012
- **Description:** Create src/merge_queue.gleam gen_server that absorbs patches into golden master
- **Acceptance:** Merge queue starts, subscribes to signals

### fs-211: Subscribe to PatchProposed
- **Type:** task
- **Priority:** P2
- **Deps:** fs-210
- **Description:** Merge queue subscribes to PatchProposed signals from synapse mutators
- **Acceptance:** PatchProposed signals received

### fs-212: Implement first-wins strategy
- **Type:** task
- **Priority:** P2
- **Deps:** fs-211
- **Description:** On PatchProposed, if not absorbing, lock, apply patch, test, commit/reject
- **Acceptance:** First valid patch wins, others ignored

### fs-213: Broadcast PatchAccepted
- **Type:** task
- **Priority:** P2
- **Deps:** fs-212, fs-012
- **Description:** On successful absorption, broadcast PatchAccepted with hash
- **Acceptance:** Signal triggers golden master refresh

### fs-214: Broadcast PatchRejected
- **Type:** task
- **Priority:** P2
- **Deps:** fs-212, fs-012
- **Description:** On failed absorption, broadcast PatchRejected with reason
- **Acceptance:** Signal available for monitoring

---

## Tier 3: Application Supervision Tree (P2)

### fs-220: Create application module
- **Type:** task
- **Priority:** P2
- **Deps:** fs-001
- **Description:** Create src/factory_synapse_app.gleam with application callback starting supervision tree
- **Acceptance:** Application starts all supervisors

### fs-221: Create top-level supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-220
- **Description:** Top supervisor with one_for_one strategy supervising all gen_servers
- **Acceptance:** Supervisor restarts crashed children

### fs-222: Wire signal_bus to supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-221, fs-014
- **Description:** Add signal_bus as supervised child, started first
- **Acceptance:** Signal bus always available

### fs-223: Wire resource_governor to supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-221, fs-030
- **Description:** Add resource_governor as supervised child
- **Acceptance:** Governor always available

### fs-224: Wire workspace_manager to supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-221, fs-026
- **Description:** Add workspace_manager as supervised child
- **Acceptance:** Manager always available

### fs-225: Wire golden_master to supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-221, fs-040
- **Description:** Add golden_master as supervised child
- **Acceptance:** Golden master always available

### fs-226: Wire llm_router to supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-221, fs-050
- **Description:** Add llm_router as supervised child
- **Acceptance:** LLM router always available

### fs-227: Wire factory_dispatcher to supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-221, fs-190
- **Description:** Add factory_dispatcher as supervised child
- **Acceptance:** Dispatcher always available

### fs-228: Wire beads_watcher to supervisor
- **Type:** task
- **Priority:** P2
- **Deps:** fs-221, fs-180
- **Description:** Add beads_watcher as supervised child
- **Acceptance:** Watcher always available

---

## Tier 3: Startup Sequence (P2)

### fs-230: Implement startup verification
- **Type:** task
- **Priority:** P2
- **Deps:** fs-220
- **Description:** On startup, verify: GPU available, RAM sufficient, NVMe available per spec
- **Acceptance:** Startup fails fast with clear error if requirements not met

### fs-231: Implement golden master preparation
- **Type:** task
- **Priority:** P2
- **Deps:** fs-230, fs-040
- **Description:** On startup, ensure golden master exists, deps cached, builds
- **Acceptance:** Golden master ready before processing begins

### fs-232: Log system ready
- **Type:** task
- **Priority:** P2
- **Deps:** fs-230
- **Description:** After all services started, log startup complete, begin processing
- **Acceptance:** Clear startup completion message

---

## Tier 4: Synapse Reactor (P3) - Future

### fs-300: Create synapse_reactor gen_server skeleton
- **Type:** task
- **Priority:** P3
- **Deps:** fs-001, fs-012
- **Description:** Create src/synapse_reactor.gleam that subscribes to TestFailure signals
- **Acceptance:** Reactor starts and subscribed

### fs-301: Implement mutator spawning
- **Type:** task
- **Priority:** P3
- **Deps:** fs-300, fs-031
- **Description:** On TestFailure, spawn swarm_size mutators in parallel (default 32)
- **Acceptance:** Mutators spawned with resource limiting

### fs-302: Create mutator process
- **Type:** task
- **Priority:** P3
- **Deps:** fs-301
- **Description:** Create mutator process: create workspace, call LLM, apply patch, test, propose or die
- **Acceptance:** Mutators work independently

### fs-303: Implement mutator LLM prompt
- **Type:** task
- **Priority:** P3
- **Deps:** fs-302
- **Description:** Implement surgical bug fix prompt from spec with file, error, hash
- **Acceptance:** Prompt generates minimal diffs

### fs-304: Kill mutators on Evolution signal
- **Type:** task
- **Priority:** P3
- **Deps:** fs-302, fs-012
- **Description:** Mutators subscribe to Evolution signal and die when world changes
- **Acceptance:** Stale mutators cleaned up automatically

---

## Tier 4: Zellij Dashboard (P3) - Future

### fs-400: Create dashboard_controller module
- **Type:** task
- **Priority:** P3
- **Description:** Create src/dashboard_controller.gleam for Zellij pane management
- **Acceptance:** Module structure ready

### fs-401: Implement signal bus pane
- **Type:** task
- **Priority:** P3
- **Deps:** fs-400, fs-012
- **Description:** Create pane showing scrolling list of signals
- **Acceptance:** Signals visible in real-time

### fs-402: Implement active loops pane
- **Type:** task
- **Priority:** P3
- **Deps:** fs-400
- **Description:** Create pane showing table of active factory loops
- **Acceptance:** Loops visible with phase, iteration, stats

### fs-403: Implement resources pane
- **Type:** task
- **Priority:** P3
- **Deps:** fs-400
- **Description:** Create pane showing resource utilization
- **Acceptance:** RAM, GPU, workspaces visible

---

## Epic: Factory-Synapse Core

### fs-epic-001: Factory-Synapse Core Implementation
- **Type:** epic
- **Priority:** P0
- **Description:** Complete implementation of Factory-Synapse system from CUE specification. Signal-driven AI coding system combining reactive bug-fixing (Synapse) with deliberate TDD-TCR-REFACTOR loops (Factory).
- **Design:** See factory-synapse-spec.cue for complete specification
