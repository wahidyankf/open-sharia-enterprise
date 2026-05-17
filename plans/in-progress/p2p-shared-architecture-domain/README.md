---
title: "P2P Shared Architecture Domain — Cross-Tutorial Migration"
status: in-progress
created: 2026-05-17
owner: tigalakilaki12
---

# P2P Shared Architecture Domain — Cross-Tutorial Migration

## Problem

Each architecture tutorial on ayokoding.com currently uses its own domain (Wlaschin order-taking for DDD-FP, generic e-shop for Hex-OOP, abstract `Order` for FSM, just-finished Conference Talk Submission Platform for DDD+Hex in-the-field). Reader cannot diff Java port-interface definition for the SAME problem against F# function-type-alias definition because the problems differ.

## Outcome (target)

A single Procure-to-Pay procurement platform — `procurement-platform-be` — used as the running domain across every pattern-specific architecture tutorial. Reader can compare:

- DDD FP `PurchaseOrder` aggregate vs DDD OOP `PurchaseOrder` aggregate
- Hex FP `PurchaseOrderRepository` function type vs Hex OOP `PurchaseOrderRepository` interface
- C4 containers of `procurement-platform-be` vs FSM state machine of `PurchaseOrder` aggregate
- DDD+Hex in-the-field FP wiring vs OOP wiring against same business invariants

The generic `architecture/by-example` (SRP/principles catalog) is intentionally untouched — it teaches principles, not a pattern.

## In scope

- `domain-driven-design-ddd/fp-by-example`
- `domain-driven-design-ddd/oop-by-example`
- `hexagonal-architecture/fp-by-example`
- `hexagonal-architecture/oop-by-example`
- `c4-architecture-model/by-example`
- `finite-state-machine-fsm/by-example`
- `ddd-hexagonal-in-practice/fp-in-the-field` (re-rewrite from Conference Talks → P2P)
- `ddd-hexagonal-in-practice/oop-in-the-field` (re-rewrite from Conference Talks → P2P)

## Out of scope

- `architecture/by-example` (generic principles catalog)
- Indonesian-language translations (separate plan if needed)
- New tutorials (Clean, CQRS, ES, EDA) — future plans can adopt this spec as input

## Document index

- [tech-docs.md](./tech-docs.md) — **the locked domain spec** (canonical reference for every maker subagent)
- [delivery.md](./delivery.md) — execution checklist
