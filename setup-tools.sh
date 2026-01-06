#!/bin/bash
#
# Setup script for TDD-TCR-Refactor toolchain
#
# This script installs and configures:
# - bv: Graph-aware beads triage engine
# - bd: Beads CLI (if not already installed)
# - codanna: Semantic code search
#

set -eo pipefail

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

log_info() {
    echo -e "${CYAN}→${NC} $1"
}

log_success() {
    echo -e "${GREEN}✓${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "  TDD-TCR-Refactor Toolchain Setup"
echo "═══════════════════════════════════════════════════════════"
echo ""

# Check for required tools
log_info "Checking for required dependencies..."

missing=()
command -v curl >/dev/null 2>&1 || missing+=("curl")
command -v jq >/dev/null 2>&1 || missing+=("jq")
command -v git >/dev/null 2>&1 || missing+=("git")
command -v cargo >/dev/null 2>&1 || missing+=("cargo (Rust toolchain)")

if [ ${#missing[@]} -gt 0 ]; then
    log_warning "Missing required tools: ${missing[*]}"
    echo "Please install them first:"
    echo "  - Arch: sudo pacman -S curl jq git rust"
    echo "  - Ubuntu/Debian: sudo apt install curl jq git cargo"
    echo "  - macOS: brew install curl jq git rust"
    exit 1
fi

log_success "All required dependencies found"
echo ""

# Install bv (beads viewer)
log_info "Installing bv (beads viewer)..."
if command -v bv >/dev/null 2>&1; then
    log_success "bv already installed at $(which bv)"
else
    curl -fsSL "https://raw.githubusercontent.com/Dicklesworthstone/beads_viewer/main/install.sh?$(date +%s)" | bash
    log_success "bv installed"
fi
echo ""

# Install codanna
log_info "Installing codanna (semantic code search)..."
if command -v codanna >/dev/null 2>&1; then
    log_success "codanna already installed at $(which codanna)"
else
    cargo install codanna --all-features
    log_success "codanna installed"
fi
echo ""

# Initialize codanna for this project
if [ ! -d ".codanna" ]; then
    log_info "Initializing codanna for this project..."
    codanna init
    log_success "codanna initialized"

    log_info "Indexing source code..."
    codanna index src
    log_success "Source code indexed"
else
    log_success "codanna already initialized"
fi
echo ""

# Test installations
log_info "Testing installations..."

echo -n "  bv: "
if bv --robot-triage >/dev/null 2>&1; then
    echo -e "${GREEN}✓ working${NC}"
else
    echo -e "${YELLOW}⚠ installed but may need beads data${NC}"
fi

echo -n "  codanna: "
if codanna mcp semantic_search_with_context query:"test" limit:1 >/dev/null 2>&1; then
    echo -e "${GREEN}✓ working${NC}"
else
    echo -e "${YELLOW}⚠ installed but may need more indexing time${NC}"
fi

echo ""
echo "═══════════════════════════════════════════════════════════"
echo "  Setup Complete!"
echo "═══════════════════════════════════════════════════════════"
echo ""
echo "Next steps:"
echo "  1. Run: ./tdd-tcr-refactor-loop --help"
echo "  2. See: cat CLAUDE.md (for AI agent instructions)"
echo "  3. Try: bv --robot-triage | jq '.triage.quick_ref'"
echo ""
