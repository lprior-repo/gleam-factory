TERSE. Min words. No fluff. No preamble. No "I'll" or "Let me". Just do.
Skip explanations unless asked. Code>prose. Act don't ask.
Responses: 1-2 sentences max unless code.
NO MARKDOWN. No headers, no bullets, no backticks. Plain text only unless user asks.

---

Factory is a contract-driven CI/CD pipeline for multi-language projects built in Gleam. It uses jj workspaces for task isolation and supports Glem, Go, Rust, Python, JavaScript.

CORE ARCHITECTURE: Factory CLI (factory.gleam) parses commands, creates jj worktrees, runs language-specific stages, tracks progress in .factory/tasks.json. Stages: implement, unit-test, coverage, lint, static, integration, security, review, accept.

KEY COMMANDS: factory new -s <slug>, factory stage -s <slug> --stage <name>, factory approve -s <slug>, factory show -s <slug>, factory list.

DOMAIN TYPES: Task(slug, language, status, priority, worktree_path, branch), Stage(name, gate, retries), Language(Go|Gleam|Rust|Python|Javascript), TaskStatus(Created|InProgress|PassedPipeline|FailedPipeline|Integrated), Priority(P1|P2|P3).

INTEGRATIONS: jj for workspace isolation, language tools (gleam build, go build, cargo build, pytest), Beads for issue tracking (.beads/beads.jsonl with EARS format).

CODE STYLE: Gleam CUPID principles - compose, pure, idiomatic, domain-driven. Types=PascalCase, functions=snake_case, |> pipes, pattern-match exhaustive, Result/Option for error handling.
