# Bead Specification Scripts

Scripts for managing EARS + KIRK + ATDD bead specifications.

## Quick Start

```bash
# Make scripts executable
chmod +x *.sh

# List beads that need specs
./list-beads-without-specs.sh

# Generate a spec for a bead
./generate-bead-spec.sh factory-xyz

# Edit the spec to fill in TODOs
$EDITOR ../specs/factory-xyz.cue

# Validate the spec
cue vet ../specs/factory-xyz.cue

# Validate all specs
./validate-all-specs.sh

# Update the bead with its spec
./update-bead-from-spec.sh factory-xyz
```

## Scripts

### generate-bead-spec.sh

Generates a CUE specification template for a bead.

```bash
./generate-bead-spec.sh <bead-id>
```

- Reads bead info from `beads.jsonl`
- Creates `specs/<bead-id>.cue` with all 10 required sections
- Includes TODOs for sections that need to be filled in

### update-bead-from-spec.sh

Updates a bead's description with its CUE spec content.

```bash
./update-bead-from-spec.sh <bead-id>
```

- Validates the CUE spec
- Exports to YAML
- Updates the bead description via `bd update`

### validate-all-specs.sh

Validates all CUE specification files.

```bash
./validate-all-specs.sh
```

- Checks every `.cue` file in `specs/`
- Reports pass/fail for each
- Shows errors for failed validations

### list-beads-without-specs.sh

Lists beads that don't have CUE specifications.

```bash
./list-beads-without-specs.sh
```

- Compares open beads in `beads.jsonl` with `specs/` directory
- Shows priority, ID, title, and type for missing specs

## Workflow

1. **Find beads needing specs**: `./list-beads-without-specs.sh`
2. **Generate template**: `./generate-bead-spec.sh factory-xyz`
3. **Edit the spec**: Fill in all TODO sections with real content
4. **Validate**: `cue vet ../specs/factory-xyz.cue`
5. **Update bead**: `./update-bead-from-spec.sh factory-xyz`

## The 10 Required Sections

Every bead spec MUST include:

1. **ears_requirements** - EARS syntax requirements (ubiquitous, event_driven, state_driven, unwanted, complex)
2. **contracts** - KIRK Design by Contract (preconditions, postconditions, invariants)
3. **inversions** - What can go wrong (integration, usability, data integrity)
4. **acceptance_tests** - ATDD with real code (happy paths, error paths, edge cases, contract tests)
5. **e2e_tests** - End-to-end test scenarios
6. **implementation_tasks** - TDD phases (tests first, implementation, integration, verification)
7. **failure_modes** - Debugging guide
8. **completion_checklist** - Tests, code, CI, docs checklists
9. **context** - Related files, dependencies, patterns
10. **ai_hints** - Do/don't guidelines, code patterns

See `../BEAD-SPEC-PROCESS.md` for detailed documentation on each section.
