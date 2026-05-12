#!/usr/bin/env bash
# validate-golden-agents.sh — verifies the 3 golden agents haven't drifted
# from their pre-extraction snapshots (modulo inlined skill content).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
errors=0

for agent in plan-maker plan-checker plan-fixer; do
  golden="/tmp/golden__${agent}__pre.md"
  current="${REPO_ROOT}/.claude/agents/${agent}.md"
  if [ ! -f "$golden" ]; then
    echo "WARN: golden snapshot missing for $agent (run: cp .claude/agents/$agent.md /tmp/golden__${agent}__pre.md)"
    continue
  fi
  if ! diff -q "$golden" "$current" > /dev/null 2>&1; then
    echo "DRIFT: $agent differs from golden snapshot"
    diff "$golden" "$current" || true
    errors=$((errors + 1))
  else
    echo "OK: $agent matches golden snapshot"
  fi
done

if [ "$errors" -gt 0 ]; then
  echo "Golden agent validation FAILED ($errors agents drifted)"
  exit 1
fi
echo "Golden agent validation PASSED"
exit 0
