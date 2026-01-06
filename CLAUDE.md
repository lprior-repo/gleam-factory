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
