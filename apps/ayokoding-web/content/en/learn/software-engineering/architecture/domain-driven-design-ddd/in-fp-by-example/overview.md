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

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example/beginner) — Types as the design. Covers ubiquitous language, bounded contexts, record and union types, smart constructors, and the full set of value types used by the purchasing context (`PurchaseRequisition`, `Money`, `SkuCode`, `Quantity`, `RequisitionId`).
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example/intermediate) — Pipelines, Railway-Oriented Programming, effects, and dependency injection. Covers function composition, `Result`, `Async`, validation accumulation, workflow signatures, domain events (`PurchaseOrderIssued`, `RequisitionApproved`), and the `PurchaseOrder` state machine.
- [Advanced (Examples 56–80)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example/advanced) — Persistence, serialization, CQRS, cross-context Anti-Corruption Layers, factory functions, repository as function-type alias, dependency rejection, and testing strategies across the `receiving` and `invoicing` contexts.

## Examples by Level

### Beginner (Examples 1–25)

- Example 1: Ubiquitous Language as F# Type Aliases
- Example 2: Domain Event Named in Past Tense
- Example 3: Bounded Context as F# Module
- Example 4: AND Type — Record
- Example 5: OR Type — Discriminated Union
- Example 6: Workflow Expressed as a Function Type
- Example 7: Single-Case Discriminated Union Wrapper
- Example 8: Smart Constructor Returning Result
- Example 9: Pattern Matching on a Discriminated Union
- Example 10: Exhaustive Match — Compiler-Enforced
- Example 11: Option Type Replacing Null
- Example 12: Constrained String — SkuCode
- Example 13: Quantity as a Smart-Constructed Value Object
- Example 14: Money Record with Currency
- Example 15: Lifecycle States as a Discriminated Union
- Example 16: State Machine Encoded Purely by Type Transitions
- Example 17: Domain Primitive Wrapping Decimal — Unit Price
- Example 18: Units of Measure
- Example 19: Email Value via Regex Validation
- Example 20: ProductCode as a Union of Two Subtypes
- Example 21: PurchaseRequisitionLine Record — Composing Value Objects
- Example 22: PurchaseRequisition Aggregate Record
- Example 23: UnvalidatedRequisition DTO-Shaped Record
- Example 24: Approval Level Derived from Requisition Total
- Example 25: Workflow Type Alias — Full SubmitRequisition Signature

### Intermediate (Examples 26–55)

- Example 26: Function Composition with >>
- Example 27: Pipe Operator |>
- Example 28: Currying — Every F# Function is One-Arg
- Example 29: Workflow Expressed as Function Composition
- Example 30: Result Type — Ok and Error
- Example 31: Result.bind — Chaining Fallible Steps
- Example 32: Result.map — Transforming the Success Value
- Example 33: Validation Accumulation with List of Errors
- Example 34: Computation Expression for Result
- Example 35: Async Result — Effects at the Edges
- Example 36: Domain Error DU — Every Failure Mode Named
- Example 37: PurchaseOrder Aggregate — Full State Machine
- Example 38: Domain Events from State Transitions
- Example 39: Supplier Aggregate — Lifecycle States
- Example 40: Aggregate Boundary — What Goes Inside
- Example 41: Refactor Primitive Obsession — Typed Wrapper
- Example 42: ValidatedPurchaseOrder Type — Emitted by Validation Step
- Example 43: IssuedPurchaseOrder Type — Emitted by Issue Step
- Example 44: ApprovePO Workflow Signature with Dependencies
- Example 45: IssuePO Workflow Signature with Dependencies
- Example 46: AcknowledgePO Workflow Signature
- Example 47: Pipeline Composition — Wiring Three Workflow Steps
- Example 48: Domain Error DU — Every Purchasing Failure Named
- Example 49: Mapping Domain Error to API Error at the Boundary
- Example 50: Pushing Effects to the Edges
- Example 51: Pure Core Wrapping at the Edge
- Example 52: Dependency Injection via Partial Application
- Example 53: Persistence Interface as a Record of Functions
- Example 54: Approval Level Enforcement — Invariant in the Domain
- Example 55: Cancellation Workflow — Off-Ramp from Any Pre-Paid State

### Advanced (Examples 56–80)

- Example 56: Serialization — JSON via DTO Boundary
- Example 57: Date/Time as a Domain Concept
- Example 58: GoodsReceiptNote Aggregate — Receiving Context
- Example 59: Invoice Aggregate — Three-Way Matching
- Example 60: EventStore vs Repository — Trade-offs
- Example 61: Bounded Context Boundary as Module + Signature
- Example 62: ACL as a Translation Function Between Contexts
- Example 63: Published Language — DU of Public Events
- Example 64: Factory Function for PurchaseOrder
- Example 65: Repository as Function-Type Alias
- Example 66: Dependency Rejection — No Optional Dependencies
- Example 67: Cross-Context Consistency — Eventual vs Strong
- Example 68: Property-Based Test for an Invariant — FsCheck
- Example 69: Compile-Time vs Runtime Check — Comparison
- Example 70: Workflow Testing Without Mocks
- Example 71: Evolution Scenario 1 — Adding a Supplier Preferred Currency
- Example 72: Evolution Scenario 2 — Adding a Three-Way Match Tolerance Override
- Example 73: Evolution Scenario 3 — Murabaha Finance Context (Optional)
- Example 74: Bounded Context Integration Map
- Example 75: Long-Running Workflow — Approval Saga
- Example 76: Interop with C# Caller — Workflow Exposed as Task
- Example 77: CQRS — Separate Read and Write Models
- Example 78: Invoice Payment Workflow — Full Pipeline
- Example 79: Domain Model Evolution — Adding a New State
- Example 80: Full System Sketch — Procurement Platform End-to-End
