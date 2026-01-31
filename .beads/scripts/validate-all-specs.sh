#!/usr/bin/env bash
# validate-all-specs.sh - Validate all CUE spec files
#
# Usage: ./validate-all-specs.sh
#
# Validates all .cue files in the specs directory and reports any errors.

set -euo pipefail

BEADS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SPECS_DIR="${BEADS_DIR}/specs"

echo "Validating CUE specs in $SPECS_DIR..."
echo ""

TOTAL=0
PASSED=0
FAILED=0
FAILED_FILES=()

for spec_file in "$SPECS_DIR"/*.cue; do
    if [[ ! -f "$spec_file" ]]; then
        echo "No CUE files found in $SPECS_DIR"
        exit 0
    fi

    TOTAL=$((TOTAL + 1))
    filename=$(basename "$spec_file")

    if cue vet "$spec_file" 2>/dev/null; then
        echo "  PASS: $filename"
        PASSED=$((PASSED + 1))
    else
        echo "  FAIL: $filename"
        FAILED=$((FAILED + 1))
        FAILED_FILES+=("$spec_file")
    fi
done

echo ""
echo "Results: $PASSED/$TOTAL passed, $FAILED failed"

if [[ $FAILED -gt 0 ]]; then
    echo ""
    echo "Failed files:"
    for f in "${FAILED_FILES[@]}"; do
        echo "  - $f"
        echo "    Errors:"
        cue vet "$f" 2>&1 | sed 's/^/      /' || true
    done
    exit 1
fi

echo ""
echo "All specs valid!"
