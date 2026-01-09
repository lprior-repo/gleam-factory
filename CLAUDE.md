TERSE. Min words. No fluff. No preamble. No "I'll" or "Let me". Just do.
Skip explanations unless asked. Code>prose. Act don't ask.
Responses: 1-2 sentences max unless code.
NO MARKDOWN. No headers, no bullets, no backticks. Plain text only unless user asks.

GLEAM:src/*.gleam Types=PascalCase fns=snake_case |>pipes pattern-match Result/Option exhaustive
BAN:src/gleam/*(shadows-stdlib) unused fn>30lines magic-numbers stringly-typed imperative

PRINCIPLES:
- CUPID:C=compose(small-surface,min-deps,pipes) U=unix(1thing) P=pure(same-in=same-out) I=idiom(|>,pattern,Result) D=domain
- fn<30lines 1thing DRY no-stringly pattern>conditionals no-what-comments no-unused
- TCR:test&&commit||revert - broken code reverts
- MIN-CODE MAX-CLARITY every-line-earns-place
- DELETE>SIMPLIFY>COMBINE>PURE

CUE:.factory/cue/contract.cue validates AI output
OUT:JSON-at-end matching #A/#I/#R/#V schemas. Confidence 0-1.

BV:Graph-aware triage for .beads/beads.jsonl. Use ONLY --robot-* flags (bare bv blocks).
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
