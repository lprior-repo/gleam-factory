TERSE. Min words. No fluff. No preamble. No "I'll" or "Let me". Just do.
Skip explanations unless asked. Code>prose. Act don't ask.
Responses: 1-2 sentences max unless code.
NO MARKDOWN. No headers, no bullets, no backticks. Plain text only unless user asks.

---

Factory is a contract-driven CI/CD pipeline for multi-language projects (Gleam, Go, Rust, Python, JavaScript). Built in Gleam with jj workspaces for task isolation.

ARCHITECTURE: Factory CLI (factory.gleam) orchestrates stages via cli.gleam. Tasks stored in .factory/tasks.json. Worktrees in .factory-workspaces/. Stages defined in domain.gleam. Executed per-language in stages_*.gleam files.

KEY COMMANDS: factory new -s <slug> | factory stage -s <slug> --stage <name> | factory approve -s <slug> | factory show -s <slug> | factory list

CORE FLOW: new (create worktree) -> stage (run pipeline: implement, unit-test, coverage, lint, static, integration, security, review, accept) -> approve (mark for integration)

DOMAIN TYPES: Task(slug, language, status, priority, worktree_path, branch), Stage(name, gate, retries), Language(Go|Gleam|Rust|Python|Javascript), TaskStatus(Created|InProgress|PassedPipeline|FailedPipeline|Integrated), Priority(P1|P2|P3)

PERSISTENCE: .factory/tasks.json stores task records. .factory/audit.log tracks events. Beads tracked in .beads/beads.jsonl.

INTEGRATIONS: jj for workspaces, language-specific tooling per stage.

CODE STYLE: Gleam CUPID principles - compose, pure, idiomatic, domain-driven. Types=PascalCase, fns=snake_case, |>pipes, pattern-match exhaustive.

BEADS TRACKING: EARS format (WHEN/THE SYSTEM SHALL/BUT INSTEAD) with severity (P1/P2/P3), reproduction, where_to_look, root_cause.

TESTING: gleeunit framework. Unit tests, integration tests, property tests (qcheck).
