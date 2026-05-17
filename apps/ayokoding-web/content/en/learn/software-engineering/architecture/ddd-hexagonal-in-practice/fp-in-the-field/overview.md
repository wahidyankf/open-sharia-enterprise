---
title: "DDD + Hexagonal in Practice — F# in the Field"
weight: 10000002
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Overview of the production wiring tutorial — how DDD aggregates flow through an F# / Giraffe / Npgsql hexagonal codebase running against a hypothetical Procure-to-Pay procurement platform"
tags: ["ddd", "hexagonal-architecture", "f#", "in-the-field", "giraffe", "npgsql"]
---

**You have finished the by-example tracks. You know what an aggregate is. You know what a port is. Now the question is: how do they wire together in a real production codebase that ships?** This tutorial answers that question using F# / Giraffe / Npgsql, running against a hypothetical Procure-to-Pay procurement platform.

Every guide in this series traces a single wiring seam: how a Giraffe handler parses an HTTP request into a command, how a domain aggregate processes that command, how an output port carries the result to a Npgsql adapter, and how an integration test swaps that adapter for an in-memory stub. No toy examples. No order-taking stories. A single coherent domain — production-grade wiring decisions — carried consistently across all twenty-seven guides.

## Prerequisites

**Both of the following tutorials are required reading before this one:**

- [DDD By Example in FP](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example/overview) — teaches DDD tactical patterns (aggregates, value objects, domain events, workflows) in F# using the same shared procurement-platform-be P2P domain.
- [Hexagonal Architecture By Example in FP](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/overview) — teaches ports-and-adapters structure (primary adapters, output ports, adapter swapping, integration test seams) in F#.

**This tutorial does NOT re-teach DDD or hexagonal fundamentals.** Terms like _aggregate_, _port_, _adapter_, _bounded context_, and _repository pattern_ are used without definition. If any of those feel unfamiliar, complete the prerequisite tracks first. The guides here are about wiring — how the pieces connect in production — not about what the pieces are.

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

Every code block in this tutorial is **production-grade hypothetical F#**. That means:

- **Production-grade**: full error handling, observability hooks where the seam being taught calls for them, no "simplified for clarity" omissions.
- **Hypothetical**: no link to a real file. The snippets describe the wiring shape for the procurement platform. The shape is real; the specific file at any given commit of any real service is not the point.
- **Cross-guide consistent**: a port defined in Guide 5 keeps the same signature in Guide 11. A bounded-context folder shape introduced in Guide 2 stays consistent through Guide 22.

If you want to see a real F# / Giraffe / Npgsql codebase that uses these patterns, look at any of the F#-track examples in the [Hexagonal Architecture By Example in FP](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/overview) prerequisite.

## Guide Numbering

Guides are numbered monotonically across all difficulty tiers (1, 2, 3 … 27). Guide 1 appears in the beginner tier, Guide 27 appears in the production tier. This makes cross-references unambiguous: "see Guide 5" means the same guide regardless of which tier page you are reading.

## Learning Path

- [Beginner (Guides 1–6)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/beginner) — One context = one hexagon, per-context folder layout, domain types without framework imports, application service signatures, output port as F# function type alias, Giraffe handler as primary adapter.
- [Intermediate (Guides 7–14)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/intermediate) — Npgsql adapter behind the repository port, in-memory test adapter, domain event publisher port, outbox adapter, full Giraffe pipeline, contract codegen, cross-context ACL, composition root in `Program.fs`.
- [Advanced (Guides 15–22)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/advanced) — docker-compose integration harness, DbUp migrations, banking port and payment adapter, retry and circuit-breaker, end-to-end domain event flow, OpenTelemetry observability adapter, murabaha-finance optional context, hexagonal anti-patterns.
- [Production (Guides 23–27)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/production) — Kubernetes deployment topology, OpenTelemetry deployment wiring, failure-mode degraded adapters, configuration adapter at the deploy seam, background job adapter.

## Sibling Tutorial

The object-oriented parallel of this tutorial uses Java 25 / Spring Boot 4 against the same hypothetical procurement platform:

- [DDD + Hexagonal in Practice — Java in the Field](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/overview)

Both tracks teach the same wiring concerns against the same domain; comparing the two side-by-side is the fastest way to see what changes when you swap functional F# wiring for object-oriented Java wiring.
