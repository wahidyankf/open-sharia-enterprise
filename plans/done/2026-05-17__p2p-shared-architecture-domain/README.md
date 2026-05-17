---
title: "P2P Shared Architecture Domain — Cross-Tutorial Migration"
status: done
created: 2026-05-17
completed: 2026-05-17
owner: tigalakilaki12
---

# P2P Shared Architecture Domain — Cross-Tutorial Migration

## Problem

Each architecture tutorial on ayokoding.com previously used its own domain (Wlaschin order-taking for DDD-FP, generic e-shop for Hex-OOP, abstract `Order` for FSM, Conference Talk Submission Platform for DDD+Hex in-the-field — itself just-finished a week earlier). Reader could not diff a Java port-interface definition for the SAME problem against an F# function-type-alias definition because the problems differed across tutorials.

## Outcome

All 8 pattern-specific architecture tutorial trees now use a single shared Procure-to-Pay procurement-platform-be domain. Reader can directly compare implementations of the SAME problem across DDD vs Hexagonal vs C4 vs FSM vs DDD+Hex in-the-field, and across FP vs OOP tracks.

Generic `architecture/by-example` (SRP/separation principles catalog) was intentionally untouched — it teaches principles, not a specific pattern.

| Tutorial                                     | Before                              | After (post-fix)              |
| -------------------------------------------- | ----------------------------------- | ----------------------------- |
| `domain-driven-design-ddd/fp-by-example`     | Wlaschin order-taking               | P2P procurement (80 examples) |
| `domain-driven-design-ddd/oop-by-example`    | Generic e-shop                      | P2P procurement (75 examples) |
| `hexagonal-architecture/fp-by-example`       | Generic                             | P2P procurement (75 examples) |
| `hexagonal-architecture/oop-by-example`      | Generic                             | P2P procurement (75 examples) |
| `c4-architecture-model/by-example`           | Mixed system examples               | P2P procurement (85 examples) |
| `finite-state-machine-fsm/by-example`        | Generic state-machine examples      | P2P procurement (75 examples) |
| `ddd-hexagonal-in-practice/fp-in-the-field`  | Conference Talk Submission Platform | P2P procurement (27 guides)   |
| `ddd-hexagonal-in-practice/oop-in-the-field` | Conference Talk Submission Platform | P2P procurement (27 guides)   |

Total: 567 examples + guides across 8 tutorial trees, all referencing the same canonical `PurchaseOrder` aggregate, `PurchaseOrderRepository` port, and shared domain events.

## Validation

- 334 unit tests pass, 86.63% coverage (≥ 82% threshold)
- 3,831 internal links checked / 0 broken
- 0 forbidden-string hits (no Conference Talk remnants, no real-codebase references)
- 8 maker-checker-fixer iterations applied to address CRITICAL + HIGH findings

## Lessons

1. **Token budget per maker subagent caps single-file size at ~3500 lines.** A Write tool argument >~32k tokens fails the response output cap. Aggressive trim during initial rewrites took example counts below the 75 minimum, requiring fixers to backfill. Lesson: pre-allocate example count + per-example annotation density in the prompt so makers don't trim those first.

2. **`// =>` is a load-bearing comment prefix.** The FSM tutorial first rewrite used plain `//` for annotations which the density convention does not count, putting all 75 examples at <1.0 density. Lesson: explicitly bake the `// =>` prefix into maker prompts; cheap global replace works as a fixer.

3. **Maker subagents can write whole tutorial trees in parallel** (8 wide tested) when domain spec is fully locked + inlined. Cross-agent consistency held without inter-agent messaging because each agent's prompt referenced the same `tech-docs.md` spec file.

4. **The checker → fixer iteration recovered convention compliance** for all CRITICAL findings (example counts, density global, missing diagrams) and most HIGH findings without a third pass. Remaining deferred items (additional diagrams in OOP variants, Why-It-Matters expansions in some files) are catalogued in the per-tutorial audit reports for a follow-up pass.

5. **Vercel Security Checkpoint blocks curl/WebFetch verification.** Live-deploy verification must use Playwright MCP (which handles the challenge) — pure curl returns 403 even after deploy succeeds. Lesson: prefer Playwright `browser_navigate` + `browser_wait_for` over `curl -s | grep`.

6. **Two-phase ship works**: ship first commit with rewrite even if quality-gates may surface findings, then ship fixer commit. Faster than batching, gives users live URL sooner. Pre-push hook caught one issue (commit-msg body line length >100) on first try — keep commit messages 100-col safe.

## Document index

- [tech-docs.md](./tech-docs.md) — locked P2P domain spec (canonical reference)
- [delivery.md](./delivery.md) — phase checklist
