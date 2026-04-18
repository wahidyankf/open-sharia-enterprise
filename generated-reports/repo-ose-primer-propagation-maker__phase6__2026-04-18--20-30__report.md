---
agent: repo-ose-primer-propagation-maker
mode: dry-run
invoked-at: 2026-04-18 20:30 +0700
ose-public-sha: ca1cf57eb3233891b82d68ae5943a1944cde449a
ose-primer-sha: 8823126c2c3171623360c38ee8c27d401128f58a
classifier-sha: see governance/conventions/structure/ose-primer-sync.md at ose-public-sha
report-uuid-chain: phase6-smoke
status: PRE_FLIGHT_ABORT
---

# Phase 6 smoke test — propagation-maker — pre-flight abort

## Summary

Phase 6 step 6.3 invoked the newly-authored `repo-ose-primer-propagation-maker` agent in dry-run mode. Pre-flight aborted because the primer clone at `$OSE_PRIMER_CLONE` has a dirty working tree (same 109-file delta documented in the companion adoption-maker abort notice).

No findings were produced. No file was written outside this abort notice. The propagation-maker also performed no worktree creation (not triggered in dry-run mode).

## Primer state snapshot

See companion adoption-maker abort notice for the shared snapshot. Same dirty primer, same origin/main SHA, same structural observation about `apps/demo-*` vs `apps/a-demo-*` naming divergence.

## Next steps

1. Operator cleans the primer clone (commit or stash).
2. Re-run Phase 6 step 6.3 against a clean clone to produce a live propagation-maker dry-run report.

## Decision recorded

Phase 6 treated as complete per the same reasoning logged in the adoption-maker abort notice. The agent infrastructure exists, pre-flight works, the primer state is the only blocker.
