---
title: "P2P Shared Architecture Domain — Delivery Checklist"
status: in-progress
created: 2026-05-17
owner: tigalakilaki12
---

# Delivery Checklist

## Phase 0: Setup

- [x] Author locked domain spec at [tech-docs.md](./tech-docs.md)
- [x] Confirm `architecture/by-example` is out of scope (generic principles catalog)

## Phase A: by-example DDD + Hex (4 parallel subagents)

- [ ] Rewrite `domain-driven-design-ddd/fp-by-example/{overview,beginner,intermediate,advanced}.md` against P2P spec
- [ ] Rewrite `domain-driven-design-ddd/oop-by-example/{overview,beginner,intermediate,advanced}.md` against P2P spec
- [ ] Rewrite `hexagonal-architecture/fp-by-example/{overview,beginner,intermediate,advanced}.md` against P2P spec
- [ ] Rewrite `hexagonal-architecture/oop-by-example/{overview,beginner,intermediate,advanced}.md` against P2P spec

## Phase B: DDD+Hex in-the-field re-rewrite (2 parallel subagents)

- [ ] Re-rewrite `ddd-hexagonal-in-practice/fp-in-the-field/{overview,beginner,intermediate,advanced,production}.md` from Conference Talks → P2P
- [ ] Re-rewrite `ddd-hexagonal-in-practice/oop-in-the-field/{overview,beginner,intermediate,advanced,production}.md` from Conference Talks → P2P

## Phase C: C4 + FSM (2 parallel subagents)

- [ ] Rewrite `c4-architecture-model/by-example/{overview,beginner,intermediate,advanced}.md` modeling `procurement-platform-be`
- [ ] Rewrite `finite-state-machine-fsm/by-example/{overview,beginner,intermediate,advanced}.md` using `PurchaseOrder` + `Invoice` + `Supplier` state machines

## Phase D: Validate

- [ ] Regenerate `_index.md` via `npx tsx src/scripts/generate-indexes.ts`
- [ ] `nx run ayokoding-web:test:quick` passes (334+ tests, ≥82% coverage, 0 broken links)
- [ ] Grep-verify forbidden strings = 0 hits across rewritten tutorials

## Phase E: Ship

- [ ] Commit on `main`
- [ ] Push `origin main`
- [ ] Invoke `apps-ayokoding-web-deployer` (force-push `main` → `prod-ayokoding-web`)
- [ ] Verify live via Playwright on architecture index page

## Phase F: Archive

- [ ] Move plan to `plans/done/2026-05-17__p2p-shared-architecture-domain/`
- [ ] Update README.md with Outcome + Lessons sections
