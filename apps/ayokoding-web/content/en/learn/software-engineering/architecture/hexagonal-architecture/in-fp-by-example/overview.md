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

This is the same domain used in the [DDD FP tutorial](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example/overview). The two tutorials complement each other: the DDD tutorial teaches how to model the domain; this tutorial teaches how to isolate it from infrastructure.

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

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner) — The three zones, ports as function types, adapters as function modules, the dependency rule, partial application as DI, in-memory adapters, and the full flow from HTTP to domain to repository — all within the `purchasing` bounded context.
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate) — Composition root, adapter swapping, integration test seams with stub adapters, dependency rejection, event publishing, the `supplier` context, `ApprovalRouterPort`, multi-context wiring, cross-context event flow, conditional adapter selection, and Railway-Oriented Programming across port boundaries.
- [Advanced (Examples 56–75)](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced) — Multi-context wiring across `receiving`, `invoicing`, and `payments`, anti-corruption layer at port boundaries, retry adapter wrapping, `BankingPort`, `SupplierNotifierPort`, `Observability`, and a full production reference.

## Examples by Level

### Beginner (Examples 1–25)

- Example 1: The Hexagon Metaphor — Three Zones as F# Namespaces
- Example 2: Domain Isolation — A Pure Domain Function with No Infrastructure Imports
- Example 3: Input Port as a Function Type Alias
- Example 4: Output Port as a Record Type — `PurchaseOrderRepository`
- Example 5: The `Clock` Output Port — Injecting Time
- Example 6: The Dependency Rule — Direction of Imports
- Example 7: Zone Boundaries as File Organisation
- Example 8: Output Port — The `PurchaseOrderRepository` Record Type
- Example 9: Output Port — Minimal vs Full Signatures
- Example 10: Output Port — Async vs Sync Signatures
- Example 11: Output Port — Error Type Design
- Example 12: Input Port — Receiving from HTTP vs CLI vs Message Bus
- Example 13: Composing Multiple Output Ports
- Example 14: Port as a Named Record vs Curried Parameters
- Example 15: In-Memory Adapter — Satisfying `PurchaseOrderRepository`
- Example 16: Primary Adapter — HTTP Handler as a Function
- Example 17: The Composition Root — Wiring Adapters to Ports
- Example 18: Spy Adapter — Verifying Port Calls in Tests
- Example 19: Failing Adapter — Testing Error Paths
- Example 20: Partial Application as Dependency Injection
- Example 21: Domain Function vs Application Service vs Adapter — Three Responsibilities
- Example 22: Testing the Domain Without Infrastructure
- Example 23: Testing the Application Service with In-Memory Adapters
- Example 24: The Anti-Corruption Layer — Translating External DTOs
- Example 25: Full Hexagonal Flow — HTTP to Domain to Repository to Response

### Intermediate (Examples 26–55)

- Example 26: Command Port vs Query Port — CQRS at the Port Boundary
- Example 27: Read Model vs Domain Model — Two Separate Output Ports
- Example 28: Async Output Port — `Async<Result<>>` Composition
- Example 29: Railway-Oriented Programming Across Async Port Calls
- Example 30: Error Union Across Port and Domain Layers
- Example 31: Repository Port as a Record of Functions
- Example 32: SupplierRepository Port — Cross-Context Dependency
- Example 33: EventPublisher Port — Domain Events as Output Port
- Example 34: ApprovalRouterPort — Routing Logic Behind a Port
- Example 35: The Composition Root — Wiring Adapters to Ports
- Example 36: Adapter Swapping for Tests — Same Application Service, Two Adapters
- Example 37: Integration Test Seam with Stub Adapter
- Example 38: Dependency Rejection — The Application Service Refuses Infrastructure
- Example 39: Two Bounded Contexts — Purchasing + Supplier in One Composition Root
- Example 40: Cross-Context Event Flow — SupplierApproved Consumed by Purchasing
- Example 41: Spy Adapter — Recording Port Calls for Test Assertions
- Example 42: Conditional Adapter Selection at the Composition Root
- Example 43: Full Flow — HTTP Request to Domain to Repository to Event Bus
- Example 44: Port Contract Testing — Verifying Every Adapter Satisfies the Same Spec
- Example 45: Approval Router Port — Routing Based on PO Total
- Example 46: Dependency Rejection — Refusing Infrastructure at the Domain Boundary
- Example 47: Port Versioning — Evolving a Port Without Breaking Adapters
- Example 48: Receiving Context — `GoodsReceiptNote` Repository Port
- Example 49: Three-Way Match Port — Invoicing Context
- Example 50: Retry Adapter Wrapper — Adding Resilience Without Touching Application Services
- Example 51: Caching Adapter Wrapper — Read-Through Cache at the Port
- Example 52: Audit Log Adapter — Side-Effecting Wrapper
- Example 53: Input Port Multiplexer — Routing One Input to Multiple Handlers
- Example 54: Observability Port — Structured Metrics Without Infrastructure Imports
- Example 55: Composition Root for the Full Purchasing + Receiving Flow

### Advanced (Examples 56–75)

- Example 56: Ports for the `receiving` Context — `GoodsReceiptRepository`
- Example 57: Ports for the `invoicing` Context — `InvoiceRepository` and Three-Way Match
- Example 58: `BankingPort` — Initiating a Disbursement
- Example 59: `SupplierNotifierPort` — SMTP and EDI Fallback
- Example 60: `Observability` Port — Emitting Metrics and Traces
- Example 61: Multi-Context Composition Root — Wiring Four Contexts
- Example 62: Retry Adapter — Decorator over `BankingPort`
- Example 63: Circuit Breaker Adapter — Wrapping `BankingPort`
- Example 64: Anti-Corruption Layer at the `BankingPort` Boundary
- Example 65: Port Versioning at the Composition Root
- Example 66: Outbox Pattern at the Adapter Level
- Example 67: Cross-Context Event — `GoodsReceived` Triggers Invoicing
- Example 68: Three-Way Match Across Context Ports
- Example 69: `PaymentScheduled` — Payments Context Consumes `InvoiceMatched`
- Example 70: Full Port Suite Spy — Testing the Payments Application Service
- Example 71: Observability-Driven Testing — Asserting Metrics Were Emitted
- Example 72: Contract Test — `BankingPort` Adapter Must Honour Domain Errors
- Example 73: Property-Based Testing — Domain Invariants Across All Inputs
- Example 74: Adapter Replacement — Swapping `GoodsReceiptRepository` from Postgres to S3
- Example 75: Complete Composition Root Wiring Verified by a Smoke Test
