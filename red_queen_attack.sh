#!/usr/bin/env bash

# Red Queen Attack Script for Factory CLI
# Executes adversarial tests and records findings as beads

set -e
FACTORY_CMD="gleam run --"
BEADS_FILE=".beads/beads.jsonl"
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Exit code tracking
EXIT_CODE=0
FINDINGS=0

# Helper: Record bead
record_bead() {
	local severity="$1"
	local trigger="$2"
	local expected="$3"
	local actual="$4"
	local reproduction="$5"
	local where="$6"

	local bead=$(
		cat <<EOF
{
  "timestamp": "$TIMESTAMP",
  "severity": "$severity",
  "trigger": "$trigger",
  "expected": "$expected",
  "actual": "$actual",
  "reproduction": "$reproduction",
  "where": "$where"
}
EOF
	)

	echo "$bead" >>"$BEADS_FILE"
	FINDINGS=$((FINDINGS + 1))

	case $severity in
	P0 | P1) echo -e "${RED}[P${severity}]${NC} $reproduction" ;;
	P2) echo -e "${YELLOW}[P2]${NC} $reproduction" ;;
	P3) echo "[P3] $reproduction" ;;
	esac
}

# Helper: Run attack and check result
run_attack() {
	local attack_name="$1"
	local cmd="$2"
	local expected_pattern="$3"
	local severity="$4"
	local trigger="$5"
	local expected="$6"
	local where="$7"

	echo -n "Testing: $attack_name ... "

	output=$(timeout 10 $cmd 2>&1 || echo "EXIT_CODE:$?")

	if echo "$output" | grep -q "$expected_pattern"; then
		echo -e "${GREEN}PASS${NC}"
		return 0
	else
		echo -e "${RED}FAIL${NC}"
		EXIT_CODE=1
		actual=$(echo "$output" | head -1 | tr '\n' ' ' | sed 's/"/\\"/g')
		record_bead "$severity" "$trigger" "$expected" "$actual" "$cmd" "$where"
		return 1
	fi
}

# Round 1: Input Boundary Attacks
echo "=========================================="
echo "ROUND 1: Input Boundary Attacks"
echo "=========================================="

# Attack 1.1: Empty slug
run_attack \
	"Empty slug" \
	"$FACTORY_CMD new -s ''" \
	"slug cannot be empty" \
	"P1" \
	"Running 'factory new' with empty slug" \
	"Error message: 'slug cannot be empty'" \
	"cli.gleam:69"

# Attack 1.2: Missing required flag
run_attack \
	"Missing --slug flag" \
	"$FACTORY_CMD new" \
	"--slug is required" \
	"P1" \
	"Running 'factory new' without -s flag" \
	"Error message: '--slug is required'" \
	"cli.gleam:63"

# Attack 1.3: Invalid characters (spaces)
run_attack \
	"Spaces in slug" \
	"$FACTORY_CMD new -s 'test slug'" \
	"invalid characters" \
	"P1" \
	"Running 'factory new' with space in slug" \
	"Error message about invalid characters" \
	"domain.gleam:80"

# Attack 1.4: Invalid characters (special chars)
run_attack \
	"Special characters in slug" \
	"$FACTORY_CMD new -s 'a/b\c@' 2>&1 || true" \
	"invalid characters" \
	"P1" \
	"Running 'factory new' with special chars" \
	"Error message about invalid characters" \
	"domain.gleam:80"

# Attack 1.5: Uppercase letters
run_attack \
	"Uppercase in slug" \
	"$FACTORY_CMD new -s 'TestSlug'" \
	"invalid characters" \
	"P2" \
	"Running 'factory new' with uppercase" \
	"Error message about invalid characters" \
	"domain.gleam:80"

# Attack 1.6: Unicode characters
run_attack \
	"Unicode in slug" \
	"$FACTORY_CMD new -s '中文'" \
	"invalid characters" \
	"P2" \
	"Running 'factory new' with unicode" \
	"Error message about invalid characters" \
	"domain.gleam:80"

# Attack 1.7: Very long slug
run_attack \
	"Very long slug" \
	"$FACTORY_CMD new -s $(printf '%0.sx' {1..100})" \
	"must be 1-50" \
	"P1" \
	"Running 'factory new' with 100 char slug" \
	"Error message about max length" \
	"domain.gleam:73"

# Attack 1.8: Null bytes
run_attack \
	"Null byte in slug" \
	"printf 'test\x00slug' | $FACTORY_CMD new -s $(cat) 2>&1 || true" \
	"invalid" \
	"P0" \
	"Running 'factory new' with null byte" \
	"Graceful error or rejection" \
	"cli.gleam:62"

# Attack 1.9: Duplicate slug (if one exists)
if [ -f ".factory/tasks.json" ]; then
	existing_slug=$(jq -r '.[0].slug' .factory/tasks.json 2>/dev/null || echo "")
	if [ -n "$existing_slug" ]; then
		run_attack \
			"Duplicate slug" \
			"$FACTORY_CMD new -s '$existing_slug'" \
			"already exists|Error" \
			"P1" \
			"Running 'factory new' with existing slug" \
			"Error about duplicate task" \
			"cli.gleam:181"
	fi
fi

# Round 2: Command-specific attacks
echo ""
echo "=========================================="
echo "ROUND 2: Command-Specific Attacks"
echo "=========================================="

# Attack 2.1: stage without --stage flag
run_attack \
	"Missing --stage flag" \
	"$FACTORY_CMD stage -s test-123" \
	"--stage is required" \
	"P1" \
	"Running 'factory stage' without --stage" \
	"Error message: '--stage is required'" \
	"cli.gleam:75"

# Attack 2.2: Invalid stage name
run_attack \
	"Invalid stage name" \
	"$FACTORY_CMD stage -s test-123 --stage not-a-stage" \
	"Invalid stage|not found" \
	"P2" \
	"Running 'factory stage' with invalid stage" \
	"Error about invalid stage name" \
	"domain.gleam"

# Attack 2.3: Invalid priority
run_attack \
	"Invalid priority" \
	"$FACTORY_CMD list --priority P4" \
	"Invalid priority" \
	"P1" \
	"Running 'factory list' with P4 priority" \
	"Error message about valid priorities" \
	"cli.gleam:134"

# Attack 2.4: Invalid status
run_attack \
	"Invalid status" \
	"$FACTORY_CMD list --status invalid" \
	"Invalid status" \
	"P1" \
	"Running 'factory list' with invalid status" \
	"Error message about valid statuses" \
	"cli.gleam:142"

# Attack 2.5: Invalid approve strategy
run_attack \
	"Invalid approve strategy" \
	"$FACTORY_CMD approve -s test-123 --strategy invalid" \
	"Invalid strategy" \
	"P1" \
	"Running 'factory approve' with invalid strategy" \
	"Error about valid strategies" \
	"cli.gleam:122"

# Round 3: State attacks
echo ""
echo "=========================================="
echo "ROUND 3: State Attacks"
echo "=========================================="

# Attack 3.1: Non-existent task
run_attack \
	"Show non-existent task" \
	"$FACTORY_CMD show -s definitely-not-a-real-task-slug-12345" \
	"not found|Error" \
	"P1" \
	"Running 'factory show' on non-existent task" \
	"Error message about task not found" \
	"cli.gleam:327"

# Attack 3.2: Approve non-existent task
run_attack \
	"Approve non-existent task" \
	"$FACTORY_CMD approve -s definitely-not-a-real-task-slug-12345" \
	"not found|Error" \
	"P1" \
	"Running 'factory approve' on non-existent task" \
	"Error message about task not found" \
	"cli.gleam:272"

# Attack 3.3: Stage non-existent task
run_attack \
	"Stage non-existent task" \
	"$FACTORY_CMD stage -s definitely-not-a-real-task-slug-12345 --stage implement" \
	"not found|Error" \
	"P1" \
	"Running 'factory stage' on non-existent task" \
	"Error message about task not found" \
	"cli.gleam:209"

# Round 4: Contract attacks
echo ""
echo "=========================================="
echo "ROUND 4: Output Contract Attacks"
echo "=========================================="

# Attack 4.1: Check list output format (should be machine-readable)
echo -n "Testing: list output format ... "
output=$($FACTORY_CMD list 2>&1 || echo "ERROR:$?")
if echo "$output" | grep -qE "^\[|^{|No matching tasks|^"; then
	echo -e "${GREEN}PASS${NC}"
else
	echo -e "${RED}FAIL${NC}"
	EXIT_CODE=1
	record_bead "P2" \
		"Running 'factory list'" \
		"Output should be JSON or well-formatted text" \
		"Output: $(echo "$output" | head -1 | tr '\n' ' ' | sed 's/"/\\"/g')" \
		"factory list" \
		"cli.gleam:354"
fi

# Summary
echo ""
echo "=========================================="
echo "SUMMARY"
echo "=========================================="
echo "Total findings: $FINDINGS"
echo "Beads recorded in: $BEADS_FILE"
echo ""
if [ $EXIT_CODE -eq 0 ]; then
	echo -e "${GREEN}All attacks survived!${NC}"
else
	echo -e "${RED}Some attacks failed. See beads above.${NC}"
fi
echo ""

exit $EXIT_CODE
