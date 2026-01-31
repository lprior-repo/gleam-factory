#!/usr/bin/env bash
# list-beads-without-specs.sh - List beads that don't have CUE specs
#
# Usage: ./list-beads-without-specs.sh
#
# Compares beads.jsonl with specs/ directory and lists beads missing specs.

set -euo pipefail

BEADS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BEADS_FILE="${BEADS_DIR}/beads.jsonl"
SPECS_DIR="${BEADS_DIR}/specs"

echo "Beads without CUE specifications:"
echo ""

# Get all open bead IDs
OPEN_BEADS=$(grep -v '"status":"tombstone"' "$BEADS_FILE" | jq -r '.id' | sort)

MISSING=0
for bead_id in $OPEN_BEADS; do
    spec_file="${SPECS_DIR}/${bead_id}.cue"
    if [[ ! -f "$spec_file" ]]; then
        # Get bead info
        BEAD_INFO=$(grep "\"id\":\"${bead_id}\"" "$BEADS_FILE" 2>/dev/null || true)
        if [[ -n "$BEAD_INFO" ]]; then
            TITLE=$(echo "$BEAD_INFO" | jq -r '.title // "Untitled"')
            PRIORITY=$(echo "$BEAD_INFO" | jq -r '.priority // "?"')
            TYPE=$(echo "$BEAD_INFO" | jq -r '.issue_type // "?"')
            echo "  [$PRIORITY] $bead_id: $TITLE ($TYPE)"
            MISSING=$((MISSING + 1))
        fi
    fi
done

if [[ $MISSING -eq 0 ]]; then
    echo "  All open beads have CUE specifications!"
fi

echo ""
echo "Total missing: $MISSING"
