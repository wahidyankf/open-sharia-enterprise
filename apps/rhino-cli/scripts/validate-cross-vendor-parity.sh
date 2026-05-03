#!/usr/bin/env bash
# Validate cross-vendor behavioral-parity invariants.
#
# This script is the implementation behind the
# rhino-cli:validate:cross-vendor-parity Nx target. It mirrors the five
# invariants validated by the repo-parity-checker agent. Exits 0 if all
# invariants hold, non-zero otherwise.
#
# The script is intentionally implemented as a thin shell wrapper that
# invokes existing tools (rhino-cli vendor-audit, npm sync, ls/grep/diff)
# rather than re-implementing their logic. See:
#   .claude/agents/repo-parity-checker.md
#   governance/conventions/structure/governance-vendor-independence.md

set -euo pipefail

# Resolve repo root (the script lives at apps/rhino-cli/scripts/).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
cd "${REPO_ROOT}"

EXIT_CODE=0
NL=$'\n'

print_invariant() {
  printf '\n[invariant %s] %s\n' "$1" "$2"
}

fail() {
  EXIT_CODE=1
  printf '  FAIL: %s\n' "$1"
}

pass() {
  printf '  pass: %s\n' "$1"
}

# Invariant 1: governance prose vendor-neutrality.
print_invariant 1 "Governance prose vendor-neutrality"
if (cd apps/rhino-cli && CGO_ENABLED=0 go run main.go governance vendor-audit governance/ >/tmp/parity-inv1.log 2>&1); then
  pass "rhino-cli governance vendor-audit governance/ (0 violations)"
else
  cat /tmp/parity-inv1.log
  fail "rhino-cli governance vendor-audit governance/ reported violations"
fi

# Invariant 2: root instruction surface vendor-neutrality.
print_invariant 2 "AGENTS.md and CLAUDE.md vendor-neutrality"
for target in AGENTS.md CLAUDE.md; do
  if (cd apps/rhino-cli && CGO_ENABLED=0 go run main.go governance vendor-audit "${target}" >/tmp/parity-inv2.log 2>&1); then
    pass "rhino-cli governance vendor-audit ${target} (0 violations)"
  else
    cat /tmp/parity-inv2.log
    fail "rhino-cli governance vendor-audit ${target} reported violations"
  fi
done

# Invariant 3: binding sync no-op.
print_invariant 3 "Binding sync no-op (.claude/ -> .opencode/)"
SYNC_OUT=$(npm run sync:claude-to-opencode --silent 2>&1) || {
  echo "${SYNC_OUT}"
  fail "sync:claude-to-opencode exited non-zero"
}
if git diff --quiet -- .opencode/agents/ 2>/dev/null; then
  pass "sync produced no changes in .opencode/agents/"
else
  printf '  diff:%s' "${NL}"
  git --no-pager diff --stat -- .opencode/agents/
  fail "sync produced drift in .opencode/agents/ — commit and re-run"
fi

# Invariant 4: agent count parity.
print_invariant 4 "Agent count parity"
CLAUDE_COUNT=$(find .claude/agents -maxdepth 1 -name '*.md' | wc -l | tr -d ' ')
OPENCODE_COUNT=$(find .opencode/agents -maxdepth 1 -name '*.md' | wc -l | tr -d ' ')
if [ "${CLAUDE_COUNT}" = "${OPENCODE_COUNT}" ]; then
  pass ".claude/agents/*.md == .opencode/agents/*.md (${CLAUDE_COUNT} == ${OPENCODE_COUNT})"
else
  printf '  .claude only:%s' "${NL}"
  comm -23 \
    <(find .claude/agents -maxdepth 1 -name '*.md' -printf '%f\n' | sort) \
    <(find .opencode/agents -maxdepth 1 -name '*.md' -printf '%f\n' | sort) || true
  printf '  .opencode only:%s' "${NL}"
  comm -13 \
    <(find .claude/agents -maxdepth 1 -name '*.md' -printf '%f\n' | sort) \
    <(find .opencode/agents -maxdepth 1 -name '*.md' -printf '%f\n' | sort) || true
  fail "count mismatch (${CLAUDE_COUNT} vs ${OPENCODE_COUNT})"
fi

# Invariant 5a: color-translation map coverage.
print_invariant 5a "Color-translation map coverage"
COLOR_VALUES=$(grep -h '^color:' .claude/agents/*.md 2>/dev/null | awk '{print $2}' | sort -u)
COLOR_MAP_FILE="governance/development/agents/ai-agents.md"
MISSING_COLORS=""
for color in ${COLOR_VALUES}; do
  case "${color}" in
    primary | success | warning | secondary | error | info | accent | muted)
      # OpenCode theme tokens written directly are valid escape-hatch values.
      continue
      ;;
  esac
  # Look for a row like "| `<color>`" in the Color Translation Table.
  if grep -qE "^\| \`${color}\`" "${COLOR_MAP_FILE}"; then
    pass "color '${color}' is mapped"
  else
    fail "color '${color}' is NOT mapped in ${COLOR_MAP_FILE}"
    MISSING_COLORS="${MISSING_COLORS}${color}, "
  fi
done
if [ -z "${MISSING_COLORS}" ]; then
  pass "all distinct colors covered (${COLOR_VALUES//$NL/, })"
fi

# Invariant 5b: capability-tier map coverage.
print_invariant 5b "Capability-tier map coverage"
TIER_VALUES=$(grep -h '^model:' .claude/agents/*.md .opencode/agents/*.md 2>/dev/null | awk '{print $2}' | grep -v '^$' | sort -u || true)
TIER_MAP_FILE="governance/development/agents/model-selection.md"
for tier in ${TIER_VALUES}; do
  if grep -qE "(\`${tier}\`|model: ${tier}\b)" "${TIER_MAP_FILE}"; then
    pass "tier '${tier}' is mapped"
  else
    fail "tier '${tier}' is NOT mapped in ${TIER_MAP_FILE}"
  fi
done
if [ -z "${TIER_VALUES}" ]; then
  pass "no model values to verify (all agents use planning-grade inherit)"
fi

if [ "${EXIT_CODE}" = 0 ]; then
  printf '\nCROSS-VENDOR PARITY VALIDATION PASSED: all invariants hold.\n'
else
  printf '\nCROSS-VENDOR PARITY VALIDATION FAILED: see findings above.\n'
fi

exit "${EXIT_CODE}"
