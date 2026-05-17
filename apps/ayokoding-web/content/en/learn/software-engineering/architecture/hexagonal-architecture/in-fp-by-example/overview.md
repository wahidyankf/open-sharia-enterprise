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

- [Example 1: The Hexagon Metaphor — Three Zones as F# Namespaces](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-1-the-hexagon-metaphor--three-zones-as-f-namespaces)
- [Example 2: Domain Isolation — A Pure Domain Function with No Infrastructure Imports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-2-domain-isolation--a-pure-domain-function-with-no-infrastructure-imports)
- [Example 3: Input Port as a Function Type Alias](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-3-input-port-as-a-function-type-alias)
- [Example 4: Output Port as a Record Type — `PurchaseOrderRepository`](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-4-output-port-as-a-record-type--purchaseorderrepository)
- [Example 5: The `Clock` Output Port — Injecting Time](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-5-the-clock-output-port--injecting-time)
- [Example 6: The Dependency Rule — Direction of Imports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-6-the-dependency-rule--direction-of-imports)
- [Example 7: Zone Boundaries as File Organisation](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-7-zone-boundaries-as-file-organisation)
- [Example 8: Output Port — The `PurchaseOrderRepository` Record Type](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-8-output-port--the-purchaseorderrepository-record-type)
- [Example 9: Output Port — Minimal vs Full Signatures](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-9-output-port--minimal-vs-full-signatures)
- [Example 10: Output Port — Async vs Sync Signatures](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-10-output-port--async-vs-sync-signatures)
- [Example 11: Output Port — Error Type Design](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-11-output-port--error-type-design)
- [Example 12: Input Port — Receiving from HTTP vs CLI vs Message Bus](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-12-input-port--receiving-from-http-vs-cli-vs-message-bus)
- [Example 13: Composing Multiple Output Ports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-13-composing-multiple-output-ports)
- [Example 14: Port as a Named Record vs Curried Parameters](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-14-port-as-a-named-record-vs-curried-parameters)
- [Example 15: In-Memory Adapter — Satisfying `PurchaseOrderRepository`](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-15-in-memory-adapter--satisfying-purchaseorderrepository)
- [Example 16: Primary Adapter — HTTP Handler as a Function](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-16-primary-adapter--http-handler-as-a-function)
- [Example 17: The Composition Root — Wiring Adapters to Ports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-17-the-composition-root--wiring-adapters-to-ports)
- [Example 18: Spy Adapter — Verifying Port Calls in Tests](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-18-spy-adapter--verifying-port-calls-in-tests)
- [Example 19: Failing Adapter — Testing Error Paths](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-19-failing-adapter--testing-error-paths)
- [Example 20: Partial Application as Dependency Injection](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-20-partial-application-as-dependency-injection)
- [Example 21: Domain Function vs Application Service vs Adapter — Three Responsibilities](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-21-domain-function-vs-application-service-vs-adapter--three-responsibilities)
- [Example 22: Testing the Domain Without Infrastructure](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-22-testing-the-domain-without-infrastructure)
- [Example 23: Testing the Application Service with In-Memory Adapters](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-23-testing-the-application-service-with-in-memory-adapters)
- [Example 24: The Anti-Corruption Layer — Translating External DTOs](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-24-the-anti-corruption-layer--translating-external-dtos)
- [Example 25: Full Hexagonal Flow — HTTP to Domain to Repository to Response](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/beginner#example-25-full-hexagonal-flow--http-to-domain-to-repository-to-response)

### Intermediate (Examples 26–55)

- [Example 26: Command Port vs Query Port — CQRS at the Port Boundary](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-26-command-port-vs-query-port--cqrs-at-the-port-boundary)
- [Example 27: Read Model vs Domain Model — Two Separate Output Ports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-27-read-model-vs-domain-model--two-separate-output-ports)
- [Example 28: Async Output Port — `Async<Result<>>` Composition](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-28-async-output-port--asyncresult-composition)
- [Example 29: Railway-Oriented Programming Across Async Port Calls](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-29-railway-oriented-programming-across-async-port-calls)
- [Example 30: Error Union Across Port and Domain Layers](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-30-error-union-across-port-and-domain-layers)
- [Example 31: Repository Port as a Record of Functions](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-31-repository-port-as-a-record-of-functions)
- [Example 32: SupplierRepository Port — Cross-Context Dependency](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-32-supplierrepository-port--cross-context-dependency)
- [Example 33: EventPublisher Port — Domain Events as Output Port](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-33-eventpublisher-port--domain-events-as-output-port)
- [Example 34: ApprovalRouterPort — Routing Logic Behind a Port](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-34-approvalrouterport--routing-logic-behind-a-port)
- [Example 35: The Composition Root — Wiring Adapters to Ports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-35-the-composition-root--wiring-adapters-to-ports)
- [Example 36: Adapter Swapping for Tests — Same Application Service, Two Adapters](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-36-adapter-swapping-for-tests--same-application-service-two-adapters)
- [Example 37: Integration Test Seam with Stub Adapter](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-37-integration-test-seam-with-stub-adapter)
- [Example 38: Dependency Rejection — The Application Service Refuses Infrastructure](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-38-dependency-rejection--the-application-service-refuses-infrastructure)
- [Example 39: Two Bounded Contexts — Purchasing + Supplier in One Composition Root](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-39-two-bounded-contexts--purchasing--supplier-in-one-composition-root)
- [Example 40: Cross-Context Event Flow — SupplierApproved Consumed by Purchasing](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-40-cross-context-event-flow--supplierapproved-consumed-by-purchasing)
- [Example 41: Spy Adapter — Recording Port Calls for Test Assertions](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-41-spy-adapter--recording-port-calls-for-test-assertions)
- [Example 42: Conditional Adapter Selection at the Composition Root](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-42-conditional-adapter-selection-at-the-composition-root)
- [Example 43: Full Flow — HTTP Request to Domain to Repository to Event Bus](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-43-full-flow--http-request-to-domain-to-repository-to-event-bus)
- [Example 44: Port Contract Testing — Verifying Every Adapter Satisfies the Same Spec](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-44-port-contract-testing--verifying-every-adapter-satisfies-the-same-spec)
- [Example 45: Approval Router Port — Routing Based on PO Total](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-45-approval-router-port--routing-based-on-po-total)
- [Example 46: Dependency Rejection — Refusing Infrastructure at the Domain Boundary](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-46-dependency-rejection--refusing-infrastructure-at-the-domain-boundary)
- [Example 47: Port Versioning — Evolving a Port Without Breaking Adapters](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-47-port-versioning--evolving-a-port-without-breaking-adapters)
- [Example 48: Receiving Context — `GoodsReceiptNote` Repository Port](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-48-receiving-context--goodsreceiptnote-repository-port)
- [Example 49: Three-Way Match Port — Invoicing Context](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-49-three-way-match-port--invoicing-context)
- [Example 50: Retry Adapter Wrapper — Adding Resilience Without Touching Application Services](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-50-retry-adapter-wrapper--adding-resilience-without-touching-application-services)
- [Example 51: Caching Adapter Wrapper — Read-Through Cache at the Port](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-51-caching-adapter-wrapper--read-through-cache-at-the-port)
- [Example 52: Audit Log Adapter — Side-Effecting Wrapper](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-52-audit-log-adapter--side-effecting-wrapper)
- [Example 53: Input Port Multiplexer — Routing One Input to Multiple Handlers](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-53-input-port-multiplexer--routing-one-input-to-multiple-handlers)
- [Example 54: Observability Port — Structured Metrics Without Infrastructure Imports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-54-observability-port--structured-metrics-without-infrastructure-imports)
- [Example 55: Composition Root for the Full Purchasing + Receiving Flow](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/intermediate#example-55-composition-root-for-the-full-purchasing--receiving-flow)

### Advanced (Examples 56–75)

- [Example 56: Ports for the `receiving` Context — `GoodsReceiptRepository`](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-56-ports-for-the-receiving-context--goodsreceiptrepository)
- [Example 57: Ports for the `invoicing` Context — `InvoiceRepository` and Three-Way Match](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-57-ports-for-the-invoicing-context--invoicerepository-and-three-way-match)
- [Example 58: `BankingPort` — Initiating a Disbursement](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-58-bankingport--initiating-a-disbursement)
- [Example 59: `SupplierNotifierPort` — SMTP and EDI Fallback](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-59-suppliernotifierport--smtp-and-edi-fallback)
- [Example 60: `Observability` Port — Emitting Metrics and Traces](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-60-observability-port--emitting-metrics-and-traces)
- [Example 61: Multi-Context Composition Root — Wiring Four Contexts](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-61-multi-context-composition-root--wiring-four-contexts)
- [Example 62: Retry Adapter — Decorator over `BankingPort`](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-62-retry-adapter--decorator-over-bankingport)
- [Example 63: Circuit Breaker Adapter — Wrapping `BankingPort`](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-63-circuit-breaker-adapter--wrapping-bankingport)
- [Example 64: Anti-Corruption Layer at the `BankingPort` Boundary](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-64-anti-corruption-layer-at-the-bankingport-boundary)
- [Example 65: Port Versioning at the Composition Root](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-65-port-versioning-at-the-composition-root)
- [Example 66: Outbox Pattern at the Adapter Level](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-66-outbox-pattern-at-the-adapter-level)
- [Example 67: Cross-Context Event — `GoodsReceived` Triggers Invoicing](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-67-cross-context-event--goodsreceived-triggers-invoicing)
- [Example 68: Three-Way Match Across Context Ports](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-68-three-way-match-across-context-ports)
- [Example 69: `PaymentScheduled` — Payments Context Consumes `InvoiceMatched`](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-69-paymentscheduled--payments-context-consumes-invoicematched)
- [Example 70: Full Port Suite Spy — Testing the Payments Application Service](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-70-full-port-suite-spy--testing-the-payments-application-service)
- [Example 71: Observability-Driven Testing — Asserting Metrics Were Emitted](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-71-observability-driven-testing--asserting-metrics-were-emitted)
- [Example 72: Contract Test — `BankingPort` Adapter Must Honour Domain Errors](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-72-contract-test--bankingport-adapter-must-honour-domain-errors)
- [Example 73: Property-Based Testing — Domain Invariants Across All Inputs](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-73-property-based-testing--domain-invariants-across-all-inputs)
- [Example 74: Adapter Replacement — Swapping `GoodsReceiptRepository` from Postgres to S3](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-74-adapter-replacement--swapping-goodsreceiptrepository-from-postgres-to-s3)
- [Example 75: Complete Composition Root Wiring Verified by a Smoke Test](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/advanced#example-75-complete-composition-root-wiring-verified-by-a-smoke-test)
