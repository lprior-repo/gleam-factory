// TCR AI Contract - Strict typed schemas for runtime validation
// Usage: echo '{"s":"tw",...}' | cue vet contract.cue -d '#A' -
// All agents MUST output JSON matching their schema. Violations = retry.
package tcr

// ═══════════════════════════════════════════════════════════════════════════
// AUDITOR OUTPUT (#A) - Writes failing tests
// ═══════════════════════════════════════════════════════════════════════════
// Status: "tw"=test_written, "rc"=requirements_complete, "e"=error
#A: #ATestWritten | #AComplete | #AError

#ATestWritten: {
    s:  "tw"                           // status: test written
    t: {                               // test details
        n: =~"_test$"                  // name must end with _test
        f: =~"^test/.*\\.gleam$"       // file must be in test/*.gleam
        b: string & !=""               // behavior being tested (non-empty)
    }
    c:  >=0.0 & <=1.0 | *0.8          // confidence score (default 0.8)
    r?: string                         // optional reasoning
}

#AComplete: {
    s: "rc"                            // status: requirements complete
    c: >=0.0 & <=1.0 | *1.0           // confidence (default 1.0 for complete)
    r?: string                         // optional reasoning
}

#AError: {
    s: "e"                             // status: error
    r: string & !=""                   // required error reason
}

// ═══════════════════════════════════════════════════════════════════════════
// IMPLEMENTER OUTPUT (#I) - Makes tests pass with minimum code
// ═══════════════════════════════════════════════════════════════════════════
// Status: "ok"=implemented, "bl"=blocked, "e"=error
// CRITICAL: Files MUST be in src/, NEVER in src/gleam/ (shadows stdlib)
#I: #IOk | #IBlocked | #IError

#IOk: {
    s: "ok"                            // status: implementation ok
    c: [...#IChange] & [_, ...]        // changes (at least one)
    cf: >=0.0 & <=1.0 | *0.9          // confidence tests will pass
    r?: string                         // optional reasoning
}

#IChange: {
    // Files MUST be src/*.gleam - runtime check blocks src/gleam/ (shadows stdlib)
    f: =~"^src/"                       // must start with src/
    a: "+" | "~"                       // action: add or modify
    l: int & >0                        // lines changed (positive int)
    // NOTE: src/gleam/* rejection enforced by tcr_commit() in bash, not CUE
    // CUE regex lacks negative lookahead. Trust runtime validation.
}

#IBlocked: {
    s:  "bl"                           // status: blocked
    b:  string & !=""                  // blocker description (required)
    cf: 0.0                            // confidence = 0 when blocked
}

#IError: {
    s: "e"                             // status: error
    r: string & !=""                   // required error reason
}

// ═══════════════════════════════════════════════════════════════════════════
// ARCHITECT OUTPUT (#R) - Refactors for quality
// ═══════════════════════════════════════════════════════════════════════════
// Status: "rf"=refactored, "nc"=no_changes, "e"=error
#R: #RRefactored | #RNoChanges | #RError

#RRefactored: {
    s:  "rf"                           // status: refactored
    lb: int & >=0                      // lines before
    la: int & >=0 & <=lb              // lines after (must be <= before)
    c:  [...#RChange] & [_, ...]       // changes made (at least one)
    sm?: [...#RSmell]                  // code smells found -> new beads
    cf: >=0.0 & <=1.0 | *0.9          // confidence refactor is safe
}

#RChange: {
    t: "d" | "s" | "x" | "r" | "i"     // type: delete|simplify|extract|rename|inline
    w: string & !=""                   // what was changed
}

#RSmell: {
    t: string & !=""                   // title for new bead
    d: string & !=""                   // description
    p: 0 | 1 | 2 | 3                   // priority (0=critical..3=low)
}

#RNoChanges: {
    s:  "nc"                           // status: no changes needed
    cf: 1.0                            // high confidence = no changes needed
    r?: string                         // optional reasoning
}

#RError: {
    s: "e"                             // status: error
    r: string & !=""                   // required error reason
}

// ═══════════════════════════════════════════════════════════════════════════
// REVIEWER OUTPUT (#V) - Final verification
// ═══════════════════════════════════════════════════════════════════════════
// Status: "p"=pass, "f"=fail
#V: #VPass | #VFail

#VPass: {
    s:  "p"                            // status: pass
    m:  true                           // all criteria met (must be true)
    v:  [...#VCriterion] & [_, ...]    // verifications (at least one)
    cf: >=0.9 & <=1.0 | *0.95         // high confidence for pass
    l?: string                         // optional learnings
}

#VFail: {
    s:  "f"                            // status: fail
    m:  false                          // criteria NOT met (must be false)
    v:  [...#VCriterion] & [_, ...]    // verifications showing failures
    cf: >=0.0 & <0.9                   // low confidence = failure
    l?: string                         // optional learnings
}

#VCriterion: {
    c: string & !=""                   // criterion name
    m: bool                            // met (true/false)
    e: string                          // evidence (file:line or description)
}
