---
title: "Overview"
date: 2026-05-17T00:00:00+07:00
draft: false
weight: 10000000
description: "Production wiring tutorials combining Domain-Driven Design and Hexagonal Architecture — two parallel tracks (F# / Giraffe / Npgsql and Java / Spring Boot 4) running against the shared procurement-platform-be Procure-to-Pay domain"
tags: ["ddd", "hexagonal-architecture", "in-the-field", "production-wiring", "fp", "oop"]
---

**You have finished the by-example tracks for both DDD and Hexagonal Architecture. You know what an aggregate is. You know what a port is. Now the question is: how do they wire together in a real production codebase that ships?** This section answers that question through two parallel tracks against the same shared Procure-to-Pay domain — so the only thing that changes between tracks is the language and framework.

## What's in this section

Two production-wiring tracks, both running against the hypothetical `procurement-platform-be` service (Purchase Requisitions → POs → Goods Receipt → Three-Way Matching → Payment):

- [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field) — F# / Giraffe / Npgsql, 27 guides
- [DDD + Hexagonal in Practice — Java in the Field](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field) — Java 25 / Spring Boot 4, 27 guides

Both tracks share the same 27-guide numbering (beginner 1–6/7, intermediate 8–14/15, advanced 15/16–22, production 23–27) so cross-track comparison is trivial: Guide 5's F# port definition versus Guide 5's Java interface definition is a single side-by-side read.

## Prerequisites

Complete the by-example tracks before starting either in-the-field track:

- **DDD by-example**: [FP track](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example) or [OOP track](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-oop-by-example)
- **Hexagonal by-example**: [FP track](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example) or [OOP track](/en/learn/software-engineering/architecture/hexagonal-architecture/in-oop-by-example)

The in-the-field tracks use terms like _aggregate_, _port_, _adapter_, _bounded context_, and _repository pattern_ without redefinition. If any feel unfamiliar, finish the prerequisite track first.

## Running domain — Procure-to-Pay procurement platform

Both tracks reason against the same hypothetical service: `procurement-platform-be`, the backend of a Procure-to-Pay (P2P) procurement platform.

| Bounded context    | Aggregate root                              | Responsibility                                                |
| ------------------ | ------------------------------------------- | ------------------------------------------------------------- |
| `purchasing`       | `PurchaseRequisition`, `PurchaseOrder`      | Requisition lifecycle, approval routing, PO issuance          |
| `supplier`         | `Supplier`                                  | Vendor master, approval state, risk score                     |
| `receiving`        | `GoodsReceiptNote`                          | Goods receipt against PO, quantity verification               |
| `invoicing`        | `Invoice`                                   | Invoice registration, three-way matching (PO ↔ GRN ↔ Invoice) |
| `payments`         | `Payment`                                   | Payment run scheduling, bank disbursement                     |
| `murabaha-finance` | `MurabahaContract` _(optional Sharia tier)_ | Bank-purchased asset, markup contract, repayment schedule     |

Same contexts, same aggregates, same domain events, same ports across both tracks — only the language and framework idioms differ.

## Why two parallel tracks

Comparing the F# wiring of Guide 5 (a port as a function type alias) against the Java wiring of Guide 5 (a port as an interface) against the same business problem is the fastest way to internalise the structural decisions that hexagonal architecture and DDD demand — regardless of which paradigm you ultimately ship with.
