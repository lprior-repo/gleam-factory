# Factory v2: Complete Design Documentation

**Status**: Design phase complete, ready for implementation review

**Last Updated**: January 5, 2026

---

## Quick Navigation

### 📋 If You Have 5 Minutes
→ Read **V2_SUMMARY.md** (executive summary)

### 📖 If You Have 30 Minutes
→ Read in this order:
1. V2_SUMMARY.md (what's changing)
2. V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md (how/why)

### 🔬 If You Have 2 Hours (Deep Review)
→ Read everything in order:
1. V2_SUMMARY.md
2. V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md (all 10 problems in detail)
3. MOON_INTEGRATION_CONTRACT.md (how it integrates)
4. V2_IMPLEMENTATION_ROADMAP.md (step-by-step plan)

### 🏗️ If You're Implementing
→ Start with:
1. V2_IMPLEMENTATION_ROADMAP.md (phases and priorities)
2. V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md (detailed specs for each problem)
3. MOON_INTEGRATION_CONTRACT.md (moon.dev integration details)

---

## The Problem We're Solving

Your original design document (5,000 lines) identified the brilliant vision for Factory v1:
- 10-stage pipeline with TCR discipline
- Contract-driven development
- Language support for Go, Gleam, Rust, Python
- Integration with Intent specs, Beads, Codanna, moon.dev

But it also honestly identified **10 critical problems**:

1. Auto-commit trap (recursive self-approval)
2. Confidence scoring fiction (0.92 precision = guess)
3. Learning loop backfires (becomes conservative, not smarter)
4. Monitoring lag (5-60 min between deploy and detection)
5. Audit trail confusion (system auto-updates, you don't know if you approved)
6. Code boundary enforcement missing (hope prevents scope creep)
7. Coverage metrics meaningless (87% says nothing about critical paths)
8. Error output noise (actual error buried in 100+ lines)
9. Language agnosticism broken (hardcoded for Go, fails for Gleam)
10. Rollback boundaries unclear (TCR reverts to where?)

**v2 fixes all 10.**

---

## What v2 Does

### Removes
- ❌ Auto-commits (no more recursive self-approval)
- ❌ Confidence scoring (no more fake numbers)
- ❌ Hope-based boundary enforcement (pre-commit hook enforces)

### Adds
- ✅ Feature flags (mandatory for deployment, blast radius control)
- ✅ Manual approval gates (you decide before deployment)
- ✅ Explicit audit trail (who approved what, when)
- ✅ Metrics dashboard (real numbers, not scores)
- ✅ Error extraction (clear, actionable summaries)
- ✅ Language detection (works for all 4 languages)
- ✅ Explicit TCR specs (clear what gets reverted when)
- ✅ Coverage analysis (critical paths vs non-critical)
- ✅ Surgical learning (incidents create specific tests)

### Keeps
- ✅ Jujutsu workspaces (still brilliant for isolation)
- ✅ 10-stage pipeline (still gold standard)
- ✅ TCR discipline (still core enforcement)
- ✅ Contract-driven development (still foundation)
- ✅ Gleam implementation (still perfect fit)
- ✅ Integration with Beads, Codanna, Intent, moon.dev

---

## Key Architectural Insights

### Insight 1: Oversight, Not Autonomy
```
v1: "System is confident, auto-deploy"
v2: "System prepares everything, you decide"
```

You stay in the loop, but for 2 minutes per change instead of 30.

### Insight 2: Metrics, Not Scores
```
v1: "Confidence: 0.92"
v2: "Coverage: 89%, Lines: 47, Deps: 0, Risk: MEDIUM"
```

Objective measurements, you judge.

### Insight 3: Surgical Learning, Not General Caution
```
v1: "Rate limiter failed, lower confidence for batch tasks"
v2: "Race condition found, add test_concurrent_requests_50 to all rate limiters"
```

Each incident teaches a specific lesson.

### Insight 4: Gradual Rollout, Not Monitoring Lag
```
v1: "Deploy 100%, monitoring will catch problems"
v2: "Deploy 1% first (instant rollback if problem), then 10%, then 100%"
```

You control blast radius, not monitoring lag.

### Insight 5: Explicit, Not Implicit
```
v1: "TCR reverts (hopefully)"
v2: "jj restore --from @- (explicit, transparent, documented)"
```

Everything documented and clear.

---

## Documents in This Design

### 1. V2_SUMMARY.md
**Length**: ~500 lines
**Purpose**: Executive summary of what's changing and why
**Audience**: Everyone (quick overview)
**Key sections**:
- 10 problems and solutions
- 3 key shifts
- 4 implementation phases
- Success criteria

### 2. V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md
**Length**: ~2,500 lines
**Purpose**: Detailed specification of all 10 problems and solutions
**Audience**: Implementers and deep reviewers
**Key sections**:
- Each problem explained in detail
- v1 issue described
- v2 solution with examples
- YAML/code specifications
- Success criteria per problem

### 3. MOON_INTEGRATION_CONTRACT.md
**Length**: ~1,500 lines
**Purpose**: How Factory integrates with moon.dev
**Audience**: Implementers, DevOps, CI/CD teams
**Key sections**:
- Architecture overview (factory calls moon)
- Task contract specification
- Configuration files structure
- Feature flag integration
- Monitoring integration
- Example complete workflow

### 4. V2_IMPLEMENTATION_ROADMAP.md
**Length**: ~1,000 lines
**Purpose**: Step-by-step implementation plan
**Audience**: Project managers, implementers
**Key sections**:
- Phase 1: Safety (week 1)
- Phase 2: Core (week 2)
- Phase 3: Polish (week 3)
- Phase 4: Integration (ongoing)
- Testing strategy
- Risk mitigation
- Success criteria per phase

### 5. V2_DESIGN_INDEX.md
**Length**: This document
**Purpose**: Navigation and context for all design docs
**Audience**: Everyone

---

## How These Documents Relate

```
Your Original Design Doc (5,000 lines)
↓
Identified 10 Critical Problems (100 lines each)
↓
V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md (detailed solutions)
↓
├─ MOON_INTEGRATION_CONTRACT.md (how it integrates with moon)
├─ V2_IMPLEMENTATION_ROADMAP.md (step-by-step plan)
└─ V2_SUMMARY.md (executive summary)
↓
Ready for Implementation
```

---

## Implementation Timeline

### Week 1: Safety Guarantees (CRITICAL)
- Remove auto-commit
- Add feature flags (mandatory)
- Pre-commit hook boundary enforcement
- Explicit status lifecycle

**Deliverable**: Can't accidentally scope creep or auto-deploy

### Week 2: Core Functionality (ESSENTIAL)
- Error extraction & summarization
- Language detection & auto-config
- Explicit TCR specifications

**Deliverable**: Pipeline works for all 4 languages with clear errors

### Week 3: Polish (NICE TO HAVE)
- Metrics dashboard
- Coverage analysis
- Surgical learning system

**Deliverable**: Better decision-making with objective information

### Ongoing: Integration (STRATEGIC)
- Beads linking
- Codanna pattern matching
- Intent spec automation
- Moon.dev orchestration

**Deliverable**: All tools working together seamlessly

---

## What Stays the Same from v1

✅ **Jujutsu workspaces** - Still best for isolation and speed
✅ **10-stage pipeline** - Still gold standard
✅ **TCR discipline** - Still core enforcement mechanism
✅ **Contract-driven development** - Still prevents bad changes
✅ **Gleam implementation** - Still perfect fit
✅ **Domain model** - Still brilliant
✅ **CLI structure** - Still clean and simple

---

## Review Checklist

Before implementation, verify:

- [ ] All 10 problems clearly understood
- [ ] All 10 solutions make sense
- [ ] Safety guarantees are actually safe (Phase 1)
- [ ] Feature flag implementation is feasible
- [ ] Moon.dev integration contract is acceptable
- [ ] Implementation roadmap is realistic
- [ ] Success criteria are testable
- [ ] Risk mitigations are adequate
- [ ] No major architectural concerns

---

## Questions to Ask Before Starting

1. **Feature Flags**: Which service? (Firebase, LaunchDarkly, custom?)
2. **Moon.dev**: Confirm it's actually how you want to integrate?
3. **Language Priority**: Start with Gleam only, add others later?
4. **Boundary Enforcement**: Pre-commit hook or pre-push? (hook is better)
5. **Monitoring**: What metrics matter most for your context?
6. **Rollout Strategy**: 1% → 10% → 100% for everything, or customize?
7. **Learning System**: When incident happens, who defines the test requirement?
8. **Integration Phases**: Do all at once or one at a time?

---

## Success Metrics for v2 Complete

### Safety
- [ ] 0 auto-commits (100% manual approval)
- [ ] 100% feature flag deployment
- [ ] 0 boundary enforcement failures
- [ ] Audit trail 100% accurate

### Functionality
- [ ] All 10 stages run correctly for Gleam, Go, Rust, Python
- [ ] Error messages clear and actionable
- [ ] TCR behavior transparent
- [ ] Moon tasks execute correctly

### Quality
- [ ] Metrics dashboard informative
- [ ] Coverage analysis highlights critical paths
- [ ] Incident→Test requirement working
- [ ] Surgical learning system operational

### Integration
- [ ] Beads issues link to workspaces
- [ ] Codanna finds and validates patterns
- [ ] Intent specs auto-decompose
- [ ] Moon.dev orchestrates everything

---

## The Real Goal

You wrote:
> "You cannot have genuine autonomous AI without maintaining oversight. The question isn't 'how do we eliminate your involvement' but 'how do we make your involvement faster and more effective.'"

v2 achieves this by:

1. **Removing magic** (no fake confidence scores)
2. **Adding safety** (feature flags, approval gates, enforcement)
3. **Showing metrics** (objective data, your decision)
4. **Learning surgically** (specific lessons, not general caution)
5. **Staying clear** (explicit everything, no mystery behavior)

**Result**: 10x leverage without giving up control.

---

## Next Steps

1. **Read** V2_SUMMARY.md (5 min)
2. **Review** V2_CRITICAL_PROBLEMS_AND_SOLUTIONS.md (30 min)
3. **Consider** MOON_INTEGRATION_CONTRACT.md (15 min)
4. **Plan** V2_IMPLEMENTATION_ROADMAP.md (15 min)
5. **Decide**: Approve? Modify? Reject? Request changes?
6. **Implement**: Phase 1 → Phase 2 → Phase 3 → Phase 4

---

## Conclusion

v1 was architecturally brilliant but needed safety/clarity fixes.

v2 adds those fixes while keeping all the good parts.

**Ready to build?**

