---
title: "Overview"
date: 2026-05-15T00:00:00+07:00
draft: false
weight: 10000002
description: "Overview of Hexagonal Architecture using Functional Programming in F# — ports as function types, adapters as implementations, and dependency injection via partial application, illustrated through procurement-platform-be"
tags:
  [
    "hexagonal-architecture",
    "f#",
    "functional-programming",
    "ports-and-adapters",
    "railway-oriented-programming",
    "by-example",
  ]
---

**Want to build systems where business logic is a pure function that cannot touch a database even if it tries?** This tutorial teaches Hexagonal Architecture through a functional programming lens, using F# as the implementation language. The central observation is that functional programming and hexagonal architecture solve the same problem from different angles: both insist that the domain core is pure and that all effects live at the edges.

## What This Tutorial Covers

Hexagonal Architecture in F# rests on three interlocking ideas that make the structural boundaries impossible to violate accidentally:

**Ports as function type aliases** — An output port is not an interface with methods; it is a function type alias. `type PurchaseOrderRepository = { save: PurchaseOrder -> Async<Result<unit, RepoError>>; load: PurchaseOrderId -> Async<Result<PurchaseOrder, RepoError>> }` is the complete contract. Any record with these fields satisfies the port. The compiler enforces substitutability without a single interface declaration.

**Adapters as function implementations** — An adapter is a record literal satisfying a port type alias. A PostgreSQL adapter is a record with `save` and `load` functions. An in-memory test adapter is a different record. Same type, different implementation. Swap adapters by passing different records at startup.

**Dependency injection via partial application** — Application services take their output port records as parameters. Partial application bakes the production adapters in for production, and simple record literals for tests. No DI container, no interface registration, no reflection — just function application.

## The Functional Core / Imperative Shell Connection

The functional core / imperative shell pattern and hexagonal architecture are the same insight expressed in different vocabularies:

| Functional term                | Hexagonal term                   |
| ------------------------------ | -------------------------------- |
| Functional core                | Domain core                      |
| Imperative shell               | Adapters                         |
| Effect-free function           | Domain function                  |
| Side-effecting function        | Adapter function                 |
| Partial application of effects | Dependency injection of adapters |

Both demand that the centre is pure. Both push effects to the boundary. Both enable easy testing by substituting the effectful shell.

## Running Domain

All 75 examples use the same **procurement-platform-be** — the backend of a Procure-to-Pay (P2P) platform where employees request goods and services, managers approve, suppliers fulfil, and finance pays. The core workflow is:

```
Employee submits PurchaseOrder draft
  → AwaitingApproval (approval router port routes to manager)
  → Approved (L1/L2/L3 based on PO total)
  → Issued (supplier notifier port sends EDI/email)
  → Received (goods receipt note recorded)
  → Invoiced (three-way match: PO ↔ GRN ↔ Invoice)
  → Paid (banking port disburses funds)
```

This is the same domain used in the [DDD FP tutorial](/en/learn/software-engineering/architecture/domain-driven-design-ddd/fp-by-example/overview). The two tutorials complement each other: the DDD tutorial teaches how to model the domain; this tutorial teaches how to isolate it from infrastructure.

## Prerequisites

- **F# basics**: comfortable with `let` bindings, function definitions, modules, discriminated unions, and record types.
- **Result and Async**: familiar with `Result<'a, 'e>` and `Async<'a>` from the standard library. Several examples use `asyncResult { }` computation expressions from [FsToolkit.ErrorHandling](https://github.com/demystifyfp/FsToolkit.ErrorHandling) — a widely-used community library; add the NuGet package `FsToolkit.ErrorHandling` to follow along.
- **DDD FP tutorial helpful but not required**: if you have read the DDD by-example tutorial first, you will recognise the domain types and smart constructors used here.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What hexagonal concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships involve zones, port/adapter boundaries, or flow across layers. Skipped for straightforward type or function definitions.
3. **Heavily Annotated F# Code**: A single, self-contained code block. Annotations use `// =>` notation to show values, types, zones, and flow at each step, targeting 1.0–2.25 comment lines per code line.
4. **Key Takeaway**: The single most important principle from this example (1–2 sentences).
5. **Why It Matters**: Real-world context — why this structural boundary matters in production systems (50–100 words).

## Learning Path

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/beginner) — The three zones, ports as function types, adapters as function modules, the dependency rule, partial application as DI, in-memory adapters, and the full flow from HTTP to domain to repository — all within the `purchasing` bounded context.
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/intermediate) — Composition root, adapter swapping, integration test seams with stub adapters, dependency rejection, event publishing, the `supplier` context, `ApprovalRouterPort`, multi-context wiring, cross-context event flow, conditional adapter selection, and Railway-Oriented Programming across port boundaries.
- [Advanced (Examples 56–75)](/en/learn/software-engineering/architecture/hexagonal-architecture/fp-by-example/advanced) — Multi-context wiring across `receiving`, `invoicing`, and `payments`, anti-corruption layer at port boundaries, retry adapter wrapping, `BankingPort`, `SupplierNotifierPort`, `Observability`, and a full production reference.
