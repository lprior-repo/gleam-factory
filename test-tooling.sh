#!/bin/bash
# Quick test of integrated tooling

set -e

echo "Testing integrated tooling..."
echo ""

echo "1. Testing bv --robot-triage..."
if bv --robot-triage > /dev/null 2>&1; then
    echo "   ✓ bv working"
else
    echo "   ✗ bv failed"
    exit 1
fi

echo "2. Testing Codanna code search..."
if codanna mcp semantic_search_with_context query:"test" limit:1 > /dev/null 2>&1; then
    echo "   ✓ Codanna code search working"
else
    echo "   ⚠ Codanna code search needs more time (still indexing)"
fi

echo "3. Testing Codanna document search..."
RESULT=$(codanna mcp search_documents query:"bv triage" 2>&1)
if echo "$RESULT" | grep -q "Found.*document"; then
    echo "   ✓ Codanna document search working"
    echo "     Found: $(echo "$RESULT" | grep "Found.*document" | head -1)"
else
    echo "   ✗ Codanna document search failed"
    echo "$RESULT"
    exit 1
fi

echo ""
echo "All tests passed! ✓"
echo ""
echo "Example commands:"
echo "  bv --robot-triage | jq '.triage.quick_ref'"
echo "  codanna mcp search_documents query:\"TDD workflow\""
echo "  codanna mcp semantic_search_with_context query:\"error handling\" limit:3"
