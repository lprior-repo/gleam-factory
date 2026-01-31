#!/usr/bin/env bash
# update-bead-from-spec.sh - Update a bead's description with its CUE spec
#
# Usage: ./update-bead-from-spec.sh <bead-id>
#
# This script exports the CUE spec to YAML and updates the bead description
# in beads.jsonl with the full specification.

set -euo pipefail

BEADS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BEADS_FILE="${BEADS_DIR}/beads.jsonl"
SPECS_DIR="${BEADS_DIR}/specs"

if [[ $# -lt 1 ]]; then
    echo "Usage: $0 <bead-id>"
    echo "Example: $0 factory-abc"
    exit 1
fi

BEAD_ID="$1"
SPEC_FILE="${SPECS_DIR}/${BEAD_ID}.cue"

# Check if spec exists
if [[ ! -f "$SPEC_FILE" ]]; then
    echo "Error: Spec not found at $SPEC_FILE"
    echo "Generate it first with: ./generate-bead-spec.sh $BEAD_ID"
    exit 1
fi

echo "Validating CUE spec..."
if ! cue vet "$SPEC_FILE" 2>/dev/null; then
    echo "Error: CUE validation failed for $SPEC_FILE"
    echo "Fix the CUE errors before updating the bead."
    exit 1
fi

echo "Exporting to YAML..."
YAML_CONTENT=$(cue export "$SPEC_FILE" --out yaml 2>/dev/null)

if [[ -z "$YAML_CONTENT" ]]; then
    echo "Error: Failed to export CUE to YAML"
    exit 1
fi

echo "Updating bead $BEAD_ID..."

# Use bd CLI to update the bead description
bd update "$BEAD_ID" --description "$YAML_CONTENT"

echo ""
echo "Successfully updated bead $BEAD_ID with CUE spec!"
echo "Verify with: bd show $BEAD_ID"
