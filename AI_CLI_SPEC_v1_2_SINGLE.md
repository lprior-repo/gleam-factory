# AI CLI Protocol Specification (Single File)
> **Version**: 1.2
> **Last updated**: January 26, 2026
> **Status**: Production ready
> **License**: CC0 (public domain)

This spec defines a machine-first JSONL protocol for building CLI tools that are safe, deterministic, and maximally usable by AI agents. Let's plan for a migration to this set of principles

---

## Table of contents
- [1. Scope](#1-scope)
- [2. Core principles](#2-core-principles)
- [3. Functional design (normative)](#3-functional-design-normative)
- [4. Wire format: JSONL](#4-wire-format-jsonl)
- [5. Request format](#5-request-format)
- [6. Response format](#6-response-format)
- [7. Response checklist (normative)](#7-response-checklist-normative)
- [8. Error model](#8-error-model)
- [9. Standard error codes](#9-standard-error-codes)
- [10. Required commands](#10-required-commands)
- [11. Recommended commands](#11-recommended-commands)
- [12. v1.2 AI-native enhancements](#12-v12-ai-native-enhancements)
- [13. New v1.2 commands](#13-new-v12-commands)
- [14. Streaming protocol](#14-streaming-protocol)
- [15. Field naming conventions](#15-field-naming-conventions)
- [16. State snapshot minimum](#16-state-snapshot-minimum)
- [17. CUE schemas (canonical)](#17-cue-schemas-canonical)
- [18. Contract enforcement (strict languages)](#18-contract-enforcement-strict-languages)
- [19. Testing strategy (normative)](#19-testing-strategy-normative)
- [20. AI integration prompt](#20-ai-integration-prompt)
- [21. Common gotchas](#21-common-gotchas)
- [Appendix A: Quick reference card](#appendix-a-quick-reference-card)

---

## 1. Scope

This spec applies to any CLI tool that:
- Accepts requests over stdin as JSONL (newline-delimited JSON objects).
- Emits responses over stdout as JSONL.
- Is intended to be driven by AI agents and orchestrators that need deterministic behavior, explicit state, and strict schemas.

Non-goals:
- Human-optimized interactive UX (you may offer `--human` output, but JSONL is the default contract).
- Tool-side "smartness" that guesses intent; the tool must be predictable, not clever.

---

## 2. Core principles

1. **Machine-first, human-compatible**: JSONL is the default output.
2. **Deterministic**: same request + same state + same tool version → same semantic result.
3. **Observable**: every response includes timing and a state snapshot.
4. **Recoverable**: failures include structured error codes and a concrete recovery command.
5. **AI-native**: the tool reports cost, safety, and intent rather than forcing the agent to guess.

---

## 3. Functional design (normative)

The CLI MUST be designed using functional principles:
- A small **imperative shell** handles IO (stdin/stdout), filesystem, network, clocks.
- A large **functional core** contains pure functions:
  - parse → validate → plan → compute → emit response
  - `(request, state) -> (response, new_state)` as the conceptual model

Normative requirements:
- Core logic MUST be deterministic and referentially transparent where practical.
- Side effects MUST be isolated behind interfaces/adapters and be injectable for tests.
- State transitions MUST be explicit (no hidden globals). If anything affects behavior, it must be modeled in state or explicit metadata.
- Any nondeterminism (timestamps, IDs) MUST be clearly identified and testable (e.g., inject a clock/ID generator).

---

## 4. Wire format: JSONL

- Requests: JSON objects, one per line on stdin.
- Responses: JSON objects, one per line on stdout.
- No prompts, banners, progress spinners, or extra logs on stdout.
- Human logs MUST go to stderr.

Example:
```jsonl
{"cmd":"?"}
{"cmd":"state"}
{"cmd":"list","rid":"req-123"}
{"cmd":"create","name":"resource","dry":true}
{"cmd":"batch","ops":[{"cmd":"create","name":"a"},{"cmd":"create","name":"b"}],"atomic":true}
```

---

## 5. Request format

### 5.1 Request schema (high-level)
```jsonl
{"cmd":"mmand>","rid":"<optional-id>","dry":<bool>,...args}
```

### 5.2 Standard fields
- `cmd` (required): Command name.
- `rid` (optional): Request ID for correlation.
- `dry` (optional): Preview mode; default false.
- `priority` (optional, v1.2): `low | normal | high | critical`.
- `deadline_ms` (optional, v1.2): absolute deadline for completion (tool may reject if impossible).
- `budget` (optional, v1.2): budget constraints; tool must enforce if provided.
- `idempotency_key` (optional, v1.2): enables safe retries without duplication.

### 5.3 Budget object (v1.2)
```json
{
  "max_tokens": 500,
  "max_cost_usd": 0.01,
  "max_compute_ms": 5000
}
```

---

## 6. Response format

### 6.1 Success (ok: true)
```json
{
  "ok": true,
  "rid": "<echo>",
  "t": 1737858000000,
  "ms": 42,
  "d": { "...": "..." },
  "next": "<suggestion>",
  "state": { "total": 0, "active": 0 }
}
```

### 6.2 Error (ok: false)
```json
{
  "ok": false,
  "rid": "<echo>",
  "t": 1737858000000,
  "ms": 15,
  "err": { "code": "<CODE>", "msg": "<text>", "ctx": { "...": "..." } },
  "fix": "<recovery_command>"
}
```

### 6.3 Dry run pattern (ok: true, d.dry: true)
```json
{
  "ok": true,
  "t": 1737858000000,
  "ms": 8,
  "d": {
    "dry": true,
    "would_do": [
      { "step": 1, "action": "validate", "target": "resource" },
      { "step": 2, "action": "create", "target": "my-resource" }
    ],
    "estimated_ms": 150,
    "reversible": true,
    "side_effects": ["audit_log"]
  },
  "next": "mytool create my-resource",
  "state": { "total": 0, "active": 0 }
}
```

---

## 7. Response checklist (normative)

### 7.1 Every response MUST have
- `ok` (boolean)
- `t` (unix timestamp in ms)
- `ms` (duration in ms)
- `rid` if provided in request (echo it back)

### 7.2 If ok: true, MUST also have
- `d` (data payload)
- `next` (suggested next command)
- `state` (state snapshot)

### 7.3 If ok: false, MUST also have
- `err.code` (standard code)
- `fix` (recovery command)

---

## 8. Error model

### 8.1 Error object
- `err.code` is required and stable.
- `err.msg` is optional, human-readable.
- `err.ctx` is optional, structured debugging context.
- v1.2 additions:
  - `err.retriable` (boolean)
  - `err.retry_strategy` (object) describing backoff and retry rules

### 8.2 Fix command
- `fix` MUST be a concrete command string the agent can run next.

---

## 9. Standard error codes

| Code | Meaning | HTTP | Fix example |
|------|---------|------|------------|
| `EXISTS` | Already exists | 409 | Delete existing, retry |
| `NOTFOUND` | Not found | 404 | `mytool list` |
| `INVALID` | Bad format | 400 | Check with `mytool ?` |
| `CONFLICT` | State conflict | 409 | `mytool state` |
| `BUSY` | Locked / throttled | 429 | Wait and retry |
| `UNAUTHORIZED` | No permission | 401 | Check credentials |
| `DEPENDENCY` | Missing dep | 424 | Install tool |
| `TIMEOUT` | Too slow | 504 | Retry with more time |
| `INTERNAL` | Bug | 500 | Report issue |
| `SAFETY_GATE` (v1.2) | Safety policy blocked | 403 | Follow confirmation flow |

---

## 10. Required commands

### 10.1 `?` — Self-description
Must return machine-readable capabilities and command schema.

Request:
```jsonl
{"cmd":"?"}
```

Response (shape):
```jsonl
{
  "ok": true,
  "t": 1737858000000,
  "ms": 5,
  "d": {
    "name": "mytool",
    "version": "1.2.0",
    "commands": { "...": "..." },
    "capabilities": { "...": "..." }
  },
  "next": "mytool state",
  "state": { "total": 0, "active": 0 }
}
```

### 10.2 `state` — Full state snapshot
Request:
```jsonl
{"cmd":"state"}
```

### 10.3 `history` — Action log
Request:
```jsonl
{"cmd":"history","n":10}
```

---

## 11. Recommended commands

| Cmd | Purpose | Example |
|-----|---------|---------|
| `checkpoint` | Save state | `{"cmd":"checkpoint","name":"before-migration"}` |
| `restore` | Restore state | `{"cmd":"restore","id":"ckpt_xyz"}` |
| `batch` | Multiple ops | `{"cmd":"batch","ops":[...],"atomic":true}` |
| `lock` | Exclusive access | `{"cmd":"lock","resource":"res_1","agent":"agent-1"}` |
| `agents` | List active agents | `{"cmd":"agents"}` |

---

## 12. v1.2 AI-native enhancements

All enhancements are additive and optional, but strongly recommended for AI-native tools.

### 12.1 Cost tracking (`meta.cost`)
The tool SHOULD report compute and model-related costs per response:
```jsonl
"meta": {
  "cost": {
    "tokens": 247,
    "api_calls": 2,
    "compute_ms": 145,
    "storage_bytes": 0,
    "usd": 0.00034
  }
}
```

### 12.2 Intent classification (`intent`)
Commands SHOULD declare their operational intent and risk profile:
```jsonl
"intent": {
  "category": "mutating",
  "complexity": "low",
  "reversible": true,
  "idempotent": true,
  "side_effects": ["audit_log"]
}
```

### 12.3 Capability flags (`capabilities`)
The `?` response SHOULD expose:
- streaming, dry_mode, atomic_batch, multi_agent
- rate limits, timeouts, feature toggles

### 12.4 Prioritization + budgets
Requests MAY include `priority`, `deadline_ms`, `budget`.
Tools MUST enforce budgets if provided and report remaining budget when possible:
```jsonl
"meta": { "budget_remaining": { "max_tokens": 253, "max_cost_usd": 0.00966 } }
```

### 12.5 Dependency graph
The tool SHOULD describe command dependencies/conflicts in `?` or `schema`.

### 12.6 Retry strategy
Errors SHOULD declare if retriable and how:
```jsonl
"err": {
  "code": "TIMEOUT",
  "retriable": true,
  "retry_strategy": { "backoff":"exponential","base_ms":100,"max_ms":10000,"max_retries":3,"jitter_pct":10 }
}
```

### 12.7 Context window tracking (`context`)
For long-lived AI sessions, tools MAY report context usage and checkpoint suggestions.

### 12.8 Reasoning traces (`reasoning`)
Tools MAY attach structured evidence + alternatives:
```jsonl
"reasoning": {
  "decision_point": "resource_exists_check",
  "evidence": { "existing_id":"res_abc123" },
  "alternatives": [
    { "option":"Rename resource", "command":"mytool create --name other", "risks":[] }
  ]
}
```

### 12.9 Canonical forms (`canonical`)
Tools MAY emit normalized/parsed forms to help agents learn equivalences.

### 12.10 Safety guards (`safety` + `SAFETY_GATE`)
Dangerous operations MUST be explicit and may require confirmation; tools should reject with `SAFETY_GATE` until confirmed.

---

## 13. New v1.2 commands

### 13.1 `schema`
Returns deep command schemas and metadata.

### 13.2 `explain`
Returns structured reasoning about a previous decision.

### 13.3 `simulate`
Runs a what-if execution (no side effects) and returns estimated outcomes, costs, risks.

### 13.4 `plan`
Given an objective + constraints, returns a step plan (including parallelization hints).

---

## 14. Streaming protocol

For long-running operations:
```jsonl
{"seq":0,"ev":"start","total":100}
{"seq":1,"ev":"progress","pct":25,"step":"validating"}
{"seq":2,"ev":"item","ok":true,"d":{"id":"res_1"}}
{"seq":"done","ok":true,"summary":{"pass":98,"fail":2},"ms":5234}
```

---

## 15. Field naming conventions

| Long name | Short | Meaning |
|----------|-------|---------|
| command | `cmd` | Command name |
| request_id | `rid` | Correlation ID |
| timestamp | `t` | Unix ms |
| duration | `ms` | Execution duration |
| data | `d` | Payload |
| error | `err` | Error object |
| message | `msg` | Human text |
| context | `ctx` | Details |
| next | `next` | Suggested next action |
| state | `state` | Snapshot |
| fix | `fix` | Recovery |
| dry | `dry` | Preview |
| sequence | `seq` | Stream sequence |
| event | `ev` | Stream event |

---

## 16. State snapshot minimum

Every response MUST include:
```json
"state": { "total": 42, "active": 38 }
```
Constraint: `total >= active`.

---

## 17. CUE schemas (canonical)

CUE is the canonical schema source for both requests and responses.
- CI MUST run `cue vet` on fixtures and on tool outputs.
- Releases MUST not ship if schemas drift or outputs violate the schema.

### 17.1 CUE schema (single-file baseline)

```cue
package ai_cli_protocol

import (
  "regexp"
  "strings"
)

#UnixTimestampMS: int & >0
#DurationMS: int & >=0

#CommandName: string & strings.MinRunes(1) & strings.MaxRunes(128) &
  regexp.Match("^[a-z][a-z0-9-]*$")

#RequestID: string & strings.MinRunes(1) & strings.MaxRunes(256)

#CommandSuggestion: string & strings.MinRunes(3) & strings.MaxRunes(512)

#ErrorCode:
  "EXISTS" | "NOTFOUND" | "INVALID" | "CONFLICT" |
  "BUSY" | "UNAUTHORIZED" | "DEPENDENCY" | "TIMEOUT" | "INTERNAL" |
  "SAFETY_GATE" |
  (string & regexp.Match("^[A-Z][A-Z0-9_]*$"))

#Budget: {
  max_tokens?: int & >0
  max_cost_usd?: number & >0
  max_compute_ms?: int & >0
}

#RetryStrategy: {
  backoff: "exponential" | "linear" | "constant"
  base_ms: int & >0
  max_ms: int & >0
  max_retries: int & >=0
  jitter_pct?: int & >=0 & <=100
}

#Error: {
  code: #ErrorCode
  msg?: string
  ctx?: {[string]: _}
  retriable?: bool
  retry_strategy?: #RetryStrategy
}

#StateSnapshot: {
  total: int & >=0
  active: int & >=0
  ...
}

#CostMetrics: {
  tokens: int & >=0
  api_calls?: int & >=0
  compute_ms?: int & >=0
  storage_bytes?: int & >=0
  usd?: number & >=0
}

#Meta: {
  cost?: #CostMetrics
  budget_remaining?: #Budget
}

#Intent: {
  category: "mutating" | "querying" | "planning" | "meta" | "system"
  complexity: "low" | "medium" | "high" | "extreme"
  reversible: bool
  idempotent: bool
  side_effects?: [...string]
}

#Alternative: {
  option: string
  command: string
  risks?: [...string]
}

#Reasoning: {
  decision_point: string
  evidence?: {[string]: _}
  alternatives?: [...#Alternative]
  confidence?: number & >=0 & <=1
}

#Context: {
  session_id: string
  tokens_used_this_session?: int & >=0
  tokens_remaining?: int & >=0
  suggested_truncation?: {
    drop_history_before?: #UnixTimestampMS
    keep_last_n_commands?: int & >0
    compress_state?: bool
    recommended_checkpoint?: bool
  }
}

#Canonical: {
  provided?: string
  forms?: [...{
    form: string
    cmd?: string
    args?: {[string]: _}
    manifest?: {[string]: _}
  }]
}

#Request: {
  cmd: #CommandName
  rid?: #RequestID
  dry?: bool
  priority?: "low" | "normal" | "high" | "critical"
  deadline_ms?: int & >0
  budget?: #Budget
  idempotency_key?: string
  ...
}

#SuccessResponse: {
  ok: true
  rid?: #RequestID
  t: #UnixTimestampMS
  ms: #DurationMS
  d: _
  next: #CommandSuggestion
  state: #StateSnapshot
  meta?: #Meta
  intent?: #Intent
  reasoning?: #Reasoning
  context?: #Context
  canonical?: #Canonical
}

#ErrorResponse: {
  ok: false
  rid?: #RequestID
  t: #UnixTimestampMS
  ms: #DurationMS
  err: #Error
  fix: #CommandSuggestion
  reasoning?: #Reasoning
}

#Response: #SuccessResponse | #ErrorResponse
```

### 17.2 CUE validation one-liners
```bash
# Validate a request line
echo '{"cmd":"list"}' | cue vet -d '#Request' - ai_cli_protocol.cue

# Validate a response line
echo '{"ok":true,"t":1,"ms":1,"d":{},"next":"mytool list","state":{"total":0,"active":0}}' \
  | cue vet -d '#Response' - ai_cli_protocol.cue
```

---

## 18. Contract enforcement (strict languages)

Implementation MUST use a strict language that makes schema drift hard:
- **Rust** recommended for strong typing, explicit error handling, and safe protocol parsing.
- **Gleam** recommended for strong static typing and functional programming defaults.

Normative requirements:
- The internal request/response models MUST be versioned and derived from (or proven consistent with) the CUE schema.
- The tool MUST NOT emit JSONL that fails `#Response`.
- The tool MUST NOT reject any request JSONL that validates against `#Request` (unless explicitly blocked by safety policy, authorization, or stated operational constraints).

---

## 19. Testing strategy (normative)

This project MUST have a very large and continuously running test suite, covering:
- unit testing
- integration testing
- end-to-end user path testing
- property-based testing
- mutation testing
- contract tests against CUE (schema oracle)

### 19.1 Property-based testing (PBT)
PBT MUST cover:
- request parsing (including fuzzed and boundary cases)
- schema closure (generated-valid requests are accepted)
- determinism (same inputs produce equivalent outputs)
- invariants (e.g., `total >= active`, idempotency rules, safety behaviors)
- streaming event ordering and final summary integrity

Minimum PBT properties (illustrative):
- **Schema closure property**: for any `req` that validates under `#Request`, `tool(req)` must return a response that validates under `#Response`.
- **Round-trip property**: parse + normalize + emit preserves meaning (canonical equivalences stable).
- **Batch atomicity property** (if atomic batch supported): either all ops succeed or state remains unchanged.

### 19.2 Mutation testing
Mutation testing MUST be used to ensure tests are not superficial.
- CI should track mutation score for core protocol modules (parsing, schema validation integration, error mapping, safety gates).
- Surviving mutants MUST trigger either new tests or design changes.

### 19.3 Unit tests
Must cover:
- each command handler in isolation
- all error codes and their `fix` commands
- dry-run behavior produces `d.dry: true` and never performs side effects
- timeouts and retry strategy formatting

### 19.4 Integration tests
Must cover:
- real IO paths (stdin/stdout), not just function calls
- concurrency/locking correctness
- persistence behavior (checkpoint/restore) if present

### 19.5 End-to-end user-path tests
Must cover realistic flows, e.g.:
- `?` → `state` → `list` → `create` → `list` → `delete` → `list`
- error + recovery: provoke `NOTFOUND`, verify `fix` resolves
- safety gating: provoke `SAFETY_GATE`, confirm explicit confirmation workflow required

### 19.6 Contract tests (CUE as oracle)
CI MUST include:
- fixtures of valid requests (CUE-validated) piped into the tool; tool must accept and respond
- fixtures of tool responses validated via `cue vet -d '#Response'`
- backwards compatibility fixtures so adding optional fields never breaks older clients
- regression suite ensuring tool never "randomly" rejects valid JSONL

---

## 20. AI integration prompt

```markdown
You have access to a CLI tool: mytool.

To discover capabilities:
1. Run: echo '{"cmd":"?"}' | mytool
2. Parse the response JSON
3. Use commands from the "commands" object and honor "capabilities".

Guidelines:
- Treat the tool as deterministic and schema-bound.
- Validate every response against the documented response shape.
- Follow "next" suggestions unless you have a better plan.
- On error, execute the "fix" command (or choose an alternative if safety/risk requires it).
- Prefer dry-run when risk is non-trivial.
- Always track cost and budget if provided.

Task: [user request]
```

---

## 21. Common gotchas

- Don't omit `ok`, `t`, or `ms`.
- Don't output non-JSON to stdout.
- Don't use `"error": "msg"`; use `err: { code, msg, ctx }`.
- Don't skip `next`; always suggest a logical next action.
- Don't silently fail; always return a structured error with a fix.
- Don't break schema compatibility; add fields as optional and validate continuously.

---

# Appendix A: Quick reference card

## Request format
```jsonl
{"cmd":"mmand>","rid":"<optional-id>","dry":<bool>,...args}
```

## Example requests
```jsonl
{"cmd":"?"}
{"cmd":"state"}
{"cmd":"list","rid":"req-123"}
{"cmd":"create","name":"resource","dry":true}
{"cmd":"batch","ops":[...],"atomic":true}
```

## Response format

### Success (ok: true)
```jsonl
{
  "ok":true,
  "rid":"<echo>",
  "t":<timestamp_ms>,
  "ms":<duration>,
  "d":{...},
  "next":"<suggestion>",
  "state":{"total":0,"active":0}
}
```

### Error (ok: false)
```jsonl
{
  "ok":false,
  "rid":"<echo>",
  "t":<timestamp_ms>,
  "ms":<duration>,
  "err":{"code":"<CODE>","msg":"<text>","ctx":{...}},
  "fix":"<recovery_command>"
}
```

### Dry run
```jsonl
{
  "ok":true,
  "d":{
    "dry":true,
    "would_do":[{"step":1,"action":"...","target":"..."}],
    "estimated_ms":150,
    "reversible":true,
    "side_effects":[...]
  },
  "next":"mytool md>",
  "state":{...}
}
```

## Standard error codes (quick)
- `EXISTS`, `NOTFOUND`, `INVALID`, `CONFLICT`, `BUSY`, `UNAUTHORIZED`, `DEPENDENCY`, `TIMEOUT`, `INTERNAL`, `SAFETY_GATE`

## CUE validation one-liners
```bash
echo '{"cmd":"list"}' | cue vet -d '#Request' - ai_cli_protocol.cue
echo '{"ok":true,"t":1,"ms":1,"d":{},"next":"mytool list","state":{"total":0,"active":0}}' \
  | cue vet -d '#Response' - ai_cli_protocol.cue
```
