---
title: "Overview"
date: 2026-05-09T00:00:00+07:00
draft: false
weight: 10000002
description: "Overview of DDD using Functional Programming in F# — type-driven design, railway-oriented programming, and workflow pipelines for a Procure-to-Pay procurement platform"
tags:
  [
    "ddd",
    "f#",
    "functional-programming",
    "domain-modeling",
    "railway-oriented-programming",
    "by-example",
    "software-architecture",
    "tutorial",
  ]
---

**Want to model complex business domains so that illegal states are literally unrepresentable at compile time?** This tutorial teaches Domain-Driven Design through a functional programming lens, using F# as the implementation language and the backend of a Procure-to-Pay (P2P) procurement platform as the running domain.

## What This Tutorial Covers

This tutorial explores three interlocking ideas that make F# an unusually powerful DDD tool:

**Type-driven design** — F# discriminated unions and record types let you encode business rules directly in the type system. An `UnvalidatedRequisition` and a `ValidatedRequisition` are different types; the compiler prevents you from accidentally treating one as the other. The domain model documents itself.

**Railway-Oriented Programming (ROP)** — Error handling becomes a first-class design concern. Functions that can fail return `Result<'a, 'e>`. Multiple fallible steps compose cleanly into pipelines using `Result.bind`, and a single `result` computation expression reads like imperative code while remaining purely functional.

**Workflow pipelines** — Business workflows are modelled as plain functions: `UnvalidatedRequisition -> Result<RequisitionSubmitted list, ProcurementError>`. Dependencies are injected via partial application, effects are pushed to the edges, and the domain core stays pure and easily testable.

## Running Domain

All 80 examples use the same **Procure-to-Pay (P2P) procurement platform** — the backend service (`procurement-platform-be`) that employees use to request goods and services, managers use to approve them, suppliers use to fulfill them, and finance uses to reconcile and pay. The core workflow is:

```
UnvalidatedRequisition → SubmitRequisition → RequisitionSubmitted
                       → ApprovePO         → PurchaseOrderIssued
                       → ReceiveGoods      → GoodsReceived
                       → MatchInvoice      → InvoiceMatched
```

Using a single running domain across all examples lets you see how individual pieces — a `Money` value object, a `Result.bind` chain, a repository modelled as a function type — fit together into a coherent procurement system.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships involve state transitions, pipelines, or bounded-context maps. Skipped for straightforward type or function definitions.
3. **Heavily Annotated F# Code**: A single, self-contained code block that runs under `dotnet fsi` or as a minimal `dotnet run` project. Annotations use `// =>` notation to show values, types, states, and effects at each step, targeting 1.0–2.25 comment lines per code line.
4. **Key Takeaway**: The single most important principle from this example (1–2 sentences).
5. **Why It Matters**: Real-world context — why this pattern matters in production systems and how it connects to type-driven DDD (50–100 words).

## Learning Path

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/beginner) — Types as the design. Covers ubiquitous language, bounded contexts, record and union types, smart constructors, and the full set of value types used by the purchasing context (`PurchaseRequisition`, `Money`, `SkuCode`, `Quantity`, `RequisitionId`).
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/intermediate) — Pipelines, Railway-Oriented Programming, effects, and dependency injection. Covers function composition, `Result`, `Async`, validation accumulation, workflow signatures, domain events (`PurchaseOrderIssued`, `RequisitionApproved`), and the `PurchaseOrder` state machine.
- [Advanced (Examples 56–80)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/advanced) — Persistence, serialization, CQRS, cross-context Anti-Corruption Layers, factory functions, repository as function-type alias, dependency rejection, and testing strategies across the `receiving` and `invoicing` contexts.
