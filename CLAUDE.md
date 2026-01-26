TERSE. Min words. No fluff. No preamble. No "I'll" or "Let me". Just do.
Skip explanations unless asked. Code>prose. Act don't ask.
Responses: 1-2 sentences max unless code.
NO MARKDOWN. No headers, no bullets, no backticks. Plain text only unless user asks.

PROJECT: Factory - contract-driven multi-language CI/CD orchestration in Gleam.
9-stage pipeline: implement->unit-test->coverage->lint->static->integration->security->review->accept
Human approval gates before deployment. OTP actor supervision. Beads task integration.

STRUCTURE:
src/ - 51 modules: domain(types) cli(commands) stages(pipeline) persistence(json) repo(detect-lang)
       worktree(jj-workspaces) process(shell) audit(events) factory(main) factory_loop(actor)
       factory_supervisor(otp-tree) agent_runners(claude) feedback_loop(auto-heal) llm_router(local/anthropic)
       signal_bus(pubsub) signals(12-event-types) types(opaque-wrappers) validation(input) config(defaults)
       bead_manager/beads_watcher/beads_broadcaster(task-system) resource_governor(gpu/mem)
       workspace_manager merge_queue golden_master verification_gauntlet phase_handlers stage_runner
test/ - 39 files: *_test.gleam(unit) qcheck/(property-based) integration/performance/golden_master tests
.factory/ - cue/schemas.cue(validation) tasks.json(state) audit/(logs)
.beads/ - config.yaml issues.jsonl(tasks) release-plan.jsonl
docs/ - ARCHITECTURE.md(603-lines) .ai-learnings.md

KEY MODULES:
domain.gleam:Language|Task|Stage|Slug|Priority|Pipeline - core types, opaque, immutable
cli.gleam:Command(NewTask|RunStage|ApproveTask|ShowTask|ListTasks|Help|Version) - parse/route
stages.gleam:StageError + lang-specific commands for gleam/go/rust/python
persistence.gleam:PersistenceError + TaskRecord JSON serialization with atomic writes
factory_loop.gleam:Phase(Implementing|Reviewing|Pushing|Completed) + Event + FactoryLoopState - OTP actor
factory_supervisor.gleam:SupervisorConfig starts signal_bus/heartbeat/resource_governor/workspace_manager
signals.gleam:BeadAssigned|TaskCreated|TestPassed|PushSuccess|etc(12-types) - event flow
types.gleam:ProcessId|WorkspaceId|GitHash|GpuGovernor - opaque validated wrappers
llm_router.gleam:RouterConfig routes to local-llama(gpu-tickets) or anthropic-api
feedback_loop.gleam:TokenBudget|TestFeedback|IterationResult - test-fail->extract-error->llm-retry

GLEAM:src/*.gleam Types=PascalCase fns=snake_case |>pipes pattern-match Result/Option exhaustive
BAN:src/gleam/*(shadows-stdlib) unused fn>30lines magic-numbers stringly-typed imperative

PRINCIPLES:
- CUPID:C=compose(small-surface,min-deps,pipes) U=unix(1thing) P=pure(same-in=same-out) I=idiom(|>,pattern,Result) D=domain
- fn<30lines 1thing DRY no-stringly pattern>conditionals no-what-comments no-unused
- MIN-CODE MAX-CLARITY every-line-earns-place
- DELETE>SIMPLIFY>COMBINE>PURE
- Opaque types make illegal states unrepresentable (Slug, GitHash, WorkspaceId)

COMMANDS:
gleam build - compile
gleam test - run all tests (gleeunit+qcheck)
gleam format --check src test - check formatting
gleam run -- --help - CLI help
gleam run -- new <slug> - create task
gleam run -- run <slug> <stage> - execute stage
gleam run -- approve <slug> - human approval gate

TEST PATTERNS:
gleeunit: pub fn test_name_test() { should.equal(...) }
qcheck: use <- qcheck.given(gen) then assert property
Golden master: compare actual vs expected snapshot
Integration: real CLI execution in test worktree
Assertions: should.be_ok() should.be_error() should.equal()

DEPS: gleam_stdlib simplifile(file-io) argv(cli) shellout(shell) stdin gleam_json gleam_otp
      gleam_erlang gleam_http gleam_httpc birl(datetime-ms)
DEV-DEPS: gleeunit qcheck qcheck_gleeunit_utils

CUE:.factory/cue/schemas.cue validates AI output
OUT:JSON-at-end matching #AuditorOutput/#ImplementerOutput/#ArchitectOutput/#ReviewerOutput schemas. Confidence 0-1.
Validate: echo '<json>' | cue vet schemas.cue -d '#OutputType' -

BV:Graph-aware triage for .beads/issues.jsonl. Use ONLY --robot-* flags (bare bv blocks).
- bv --robot-triage: entry point, all-in-one (quick_ref, recommendations, quick_wins, blockers, health, commands)
- bv --robot-next: minimal top pick
- bv --robot-plan: parallel tracks with unblocks
- bv --robot-insights: PageRank, betweenness, HITS, eigenvector, cycles, critical path, k-core
- bv --robot-label-health: per-label health (healthy|warning|critical), velocity, staleness
- bv --robot-label-flow: cross-label dependencies, bottlenecks
- bv --robot-alerts: stale, blocking cascades, priority mismatches
- bv --recipe actionable --robot-plan: unblocked work only
- bv --recipe high-impact --robot-triage: top PageRank
Filter: --label <label>, --as-of <ref>
Output: JSON with data_hash, status (computed|approx|timeout|skipped), metrics
Phase 1 (instant): degree, topo sort, density
Phase 2 (500ms): PageRank, betweenness, HITS, eigenvector, cycles
Use bv for what-to-work-on (triage, priority, planning). Use MCP Agent Mail for agent-to-agent coordination.

CI: .github/workflows/test.yml - OTP 28, Gleam 1.13.0, steps: deps->test->format-check

ARCHITECTURE NOTES:
- OTP supervisor tree: factory_supervisor starts signal_bus, heartbeat, resource_governor, workspace_manager, merge_queue
- Signal-based: events flow through signal_bus (BeadAssigned->TaskCreated->TestPassed->PushSuccess)
- File watching: beads_watcher polls .beads/issues.jsonl (hash-based change detection)
- Resource limits: resource_governor manages GPU tickets, memory limits, max workspaces
- Feedback loop: test failure->extract error->LLM retry with token budget
- Worktree isolation: jujutsu workspaces per task, symlinks in .factory/
