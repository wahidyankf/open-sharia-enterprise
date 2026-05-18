---
title: "Overview"
weight: 10000002
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Overview of the In FP case — how C4, DDD, Hexagonal Architecture, and Finite State Machines compose in an F# / Giraffe / Npgsql codebase running the procurement-platform-be Procure-to-Pay domain"
tags:
  [
    "cases",
    "in-fp",
    "c4-model",
    "ddd",
    "hexagonal-architecture",
    "finite-state-machine",
    "f#",
    "functional-programming",
    "giraffe",
    "npgsql",
  ]
---

**You have finished the by-example tracks for C4, DDD, Hexagonal Architecture, and Finite State Machines. You know what an aggregate is. You know what a port is. You know how to draw a Container diagram. You know how to model a state machine. Now the question is: how do they all wire together in a real production codebase that ships?** This case answers that question using F# / Giraffe / Npgsql, running against a hypothetical Procure-to-Pay procurement platform.

Every guide in this series traces a single wiring seam: how a C4 Container decomposes into bounded-context components, how a Giraffe handler parses an HTTP request into a command, how a domain aggregate processes that command through an explicit state machine, how an output port carries the result to a Npgsql adapter, and how an integration test swaps that adapter for an in-memory stub. No toy examples. One coherent procurement-platform-be P2P domain — production-grade wiring decisions — carried consistently across all twenty-seven guides.

## Prerequisites

**All four of the following by-example tracks are required reading before this case:**

- [C4 By Example](/en/learn/software-engineering/software-architecture/c4-model/by-example/overview) — teaches the four C4 levels (Context, Container, Component, Code) used to draw the procurement platform's architecture.
- [DDD By Example in FP](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/overview) — teaches DDD tactical patterns (aggregates, value objects, domain events, workflows) in F# using the same shared procurement-platform-be P2P domain.
- [Hexagonal Architecture By Example in FP](/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-fp-by-example/overview) — teaches ports-and-adapters structure (primary adapters, output ports, adapter swapping, integration test seams) in F#.
- [FSM By Example in FP](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-fp-by-example/overview) — teaches state types as discriminated unions and transition functions guarding aggregate lifecycles.

**This case does NOT re-teach C4, DDD, hexagonal, or FSM fundamentals.** Terms like _container_, _component_, _aggregate_, _port_, _adapter_, _bounded context_, _repository pattern_, and _state machine_ are used without definition. If any of those feel unfamiliar, complete the prerequisite tracks first. The guides here are about **wiring** — how the pieces connect in production — not about what the pieces are.

## How the Four Families Compose in the F# Codebase

The four architecture pattern families slot together at different levels of the F# codebase:

| Family  | Where It Lives in F#                                                                                      |
| ------- | --------------------------------------------------------------------------------------------------------- |
| **C4**  | Diagram artifacts checked in alongside source; bounded-context names match the C4 Component names         |
| **DDD** | Top-level modules per bounded context (`ProcurementPlatformBe.Purchasing`, etc.); aggregates as records   |
| **Hex** | `Domain.fs` (core) → `Application/Ports.fs` + `*Workflow.fs` → `Adapters/{In,Out}` → `CompositionRoot.fs` |
| **FSM** | Aggregate state as a discriminated union; transitions as pure functions returning `Result<NextState, _>`  |

The composition root assembles concrete adapter function values into the port slots before Giraffe takes over the request pipeline.

## Running Domain — Procure-to-Pay Procurement Platform

Every guide reasons against the same hypothetical service: **`procurement-platform-be`**, the backend of a Procure-to-Pay (P2P) platform. Employees request goods and services, managers approve, suppliers fulfill, and finance pays. Picking a single coherent domain (instead of one toy example per guide) lets the wiring decisions in Guide 14 reference the port introduced in Guide 5 and the aggregate introduced in Guide 3 without re-establishing context.

The platform organizes around six bounded contexts, introduced progressively by tier:

| Bounded context    | Aggregate root     | Responsibility                                                      |
| ------------------ | ------------------ | ------------------------------------------------------------------- |
| `purchasing`       | `PurchaseOrder`    | Requisition lifecycle, approval routing, PO issuance to supplier    |
| `supplier`         | `Supplier`         | Vendor master, approval state, risk score                           |
| `receiving`        | `GoodsReceiptNote` | Goods receipt against PO, quantity verification, QC flag            |
| `invoicing`        | `Invoice`          | Invoice registration, three-way matching (PO ↔ GRN ↔ Invoice)       |
| `payments`         | `Payment`          | Payment run scheduling, bank disbursement, supplier remittance      |
| `murabaha-finance` | `MurabahaContract` | (Optional Sharia angle) bank buys asset, resells to buyer at markup |

Cross-context domain events travel between contexts via the `EventPublisher` port. The most important events used across guides are summarized below; each event is reintroduced inline in the first guide that uses it.

| Event                             | Source context | Consumers                                               |
| --------------------------------- | -------------- | ------------------------------------------------------- |
| `PurchaseOrderIssued`             | purchasing     | supplier-notifier (EDI/email), receiving                |
| `PurchaseOrderAcknowledged`       | purchasing     | receiving (opens GRN expectation)                       |
| `PurchaseOrderCancelled`          | purchasing     | supplier-notifier, accounting                           |
| `GoodsReceived`                   | receiving      | invoicing (enables matching), purchasing (state update) |
| `GoodsReceiptDiscrepancyDetected` | receiving      | invoicing (blocks matching), supplier-notifier          |
| `InvoiceMatched`                  | invoicing      | payments (schedules payment), purchasing                |
| `InvoiceDisputed`                 | invoicing      | supplier-notifier, accounting                           |
| `PaymentDisbursed`                | payments       | supplier-notifier, accounting, purchasing               |
| `SupplierApproved`                | supplier       | purchasing (eligible-for-PO list)                       |

## Code Grounding

Every code block in this case is **production-grade hypothetical F#**. That means:

- **Production-grade**: full error handling, observability hooks where the seam being taught calls for them, no "simplified for clarity" omissions.
- **Hypothetical**: no link to a real file. The snippets describe the wiring shape for the procurement platform. The shape is real; the specific file at any given commit of any real service is not the point.
- **Cross-guide consistent**: a port defined in Guide 5 keeps the same signature in Guide 11. A bounded-context folder shape introduced in Guide 2 stays consistent through Guide 22. A state machine introduced in Guide 3 is referenced unchanged in Guide 18.

If you want to see a real F# / Giraffe / Npgsql codebase that uses these patterns, look at any of the F#-track examples in the [Hexagonal Architecture By Example in FP](/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-fp-by-example/overview) prerequisite.

## Guide Numbering

Guides are numbered monotonically across all difficulty tiers (1, 2, 3 … 27). Guide 1 appears in the beginner tier, Guide 27 appears at the end of the advanced tier. This makes cross-references unambiguous: "see Guide 5" means the same guide regardless of which tier page you are reading.

## Learning Path

- [Beginner (Guides 1–6)](/en/learn/software-engineering/software-architecture/cases/in-fp/beginner) — One context = one hexagon, per-context folder layout, domain types without framework imports, application service signatures, output port as F# function type alias, Giraffe handler as primary adapter.
- [Intermediate (Guides 7–14)](/en/learn/software-engineering/software-architecture/cases/in-fp/intermediate) — Npgsql adapter behind the repository port, in-memory test adapter, domain event publisher port, outbox adapter, full Giraffe pipeline, contract codegen, cross-context ACL, composition root in `Program.fs`.
- [Advanced (Guides 15–27)](/en/learn/software-engineering/software-architecture/cases/in-fp/advanced) — docker-compose integration harness, DbUp migrations, banking port and payment adapter, retry and circuit-breaker, end-to-end domain event flow, OpenTelemetry observability adapter, murabaha-finance optional context, hexagonal anti-patterns, Kubernetes deployment topology, OpenTelemetry deployment wiring, failure-mode degraded adapters, configuration adapter at the deploy seam, background job adapter.

## Sibling Case

The object-oriented parallel of this case uses Java 25 / Spring Boot 4 against the same hypothetical procurement platform:

- [In OOP — Java / Spring Boot](/en/learn/software-engineering/software-architecture/cases/in-oop/overview)

Both cases teach the same wiring concerns against the same domain; comparing the two side-by-side is the fastest way to see what changes when you swap functional F# wiring for object-oriented Java wiring.
