---
title: "Overview"
date: 2026-05-09T00:00:00+07:00
draft: false
weight: 10000002
description: "Overview of DDD using Functional Programming — type-driven design, railway-oriented programming, and workflow pipelines for a Procure-to-Pay procurement platform, shown in F# (canonical), Clojure, and TypeScript"
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
    "clojure",
    "typescript",
  ]
---

**Want to model complex business domains so that illegal states are literally unrepresentable at compile time?** This tutorial teaches Domain-Driven Design through a functional programming lens, using three languages — F# (canonical), Clojure, and TypeScript — and the backend of a Procure-to-Pay (P2P) procurement platform as the running domain. Each example presents all three languages as parallel tabs; F# carries the deepest annotations and the framing prose, while Clojure and TypeScript are first-class variants that show the same domain patterns in their respective idioms.

## What This Tutorial Covers

This tutorial explores three interlocking ideas that make functional programming an unusually powerful DDD tool. Each idea is shown in F# (canonical), Clojure, and TypeScript:

**Type-driven design** — F# discriminated unions and record types, Clojure spec-validated maps, and TypeScript union types + Zod schemas all let you encode business rules at the type/validation boundary. An `UnvalidatedRequisition` and a `ValidatedRequisition` are different types (or shapes); the language prevents you from accidentally treating one as the other. The domain model documents itself regardless of which language you use.

**Railway-Oriented Programming (ROP)** — Error handling becomes a first-class design concern. F# uses `Result<'a, 'e>` with `Result.bind`; Clojure uses `either` monads or threading macros with tagged maps; TypeScript uses a `Result` type or `neverthrow`. Multiple fallible steps compose cleanly into pipelines in all three.

**Workflow pipelines** — Business workflows are modelled as plain functions. Dependencies are injected via partial application (F#), higher-order functions (Clojure), or constructor injection (TypeScript). Effects are pushed to the edges, and the domain core stays pure and easily testable in all three languages.

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
3. **Heavily Annotated Code**: Parallel tabs showing F# (canonical), Clojure, and TypeScript. Each tab is a single, self-contained code block. Annotations use `// =>` notation (or `;;` / `// =>` in Clojure) to show values, types, states, and effects at each step, targeting 1.0–2.25 comment lines per code line per tab.
4. **Key Takeaway**: The single most important principle from this example (1–2 sentences).
5. **Why It Matters**: Real-world context — why this pattern matters in production systems and how it connects to type-driven DDD (50–100 words).

## Learning Path

- [Beginner (Examples 1–25)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner) — Types as the design. Covers ubiquitous language, bounded contexts, record and union types, smart constructors, and the full set of value types used by the purchasing context (`PurchaseRequisition`, `Money`, `SkuCode`, `Quantity`, `RequisitionId`).
- [Intermediate (Examples 26–55)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate) — Pipelines, Railway-Oriented Programming, effects, and dependency injection. Covers function composition, `Result`, `Async`, validation accumulation, workflow signatures, domain events (`PurchaseOrderIssued`, `RequisitionApproved`), and the `PurchaseOrder` state machine.
- [Advanced (Examples 56–80)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced) — Persistence, serialization, CQRS, cross-context Anti-Corruption Layers, factory functions, repository as function-type alias, dependency rejection, and testing strategies across the `receiving` and `invoicing` contexts.

## Examples by Level

### Beginner (Examples 1–25)

- [Example 1: Ubiquitous Language as F# Type Aliases](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-1-ubiquitous-language-as-f-type-aliases)
- [Example 2: Domain Event Named in Past Tense](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-2-domain-event-named-in-past-tense)
- [Example 3: Bounded Context as F# Module](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-3-bounded-context-as-f-module)
- [Example 4: AND Type — Record](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-4-and-type--record)
- [Example 5: OR Type — Discriminated Union](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-5-or-type--discriminated-union)
- [Example 6: Workflow Expressed as a Function Type](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-6-workflow-expressed-as-a-function-type)
- [Example 7: Single-Case Discriminated Union Wrapper](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-7-single-case-discriminated-union-wrapper)
- [Example 8: Smart Constructor Returning Result](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-8-smart-constructor-returning-result)
- [Example 9: Pattern Matching on a Discriminated Union](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-9-pattern-matching-on-a-discriminated-union)
- [Example 10: Exhaustive Match — Compiler-Enforced](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-10-exhaustive-match--compiler-enforced)
- [Example 11: Option Type Replacing Null](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-11-option-type-replacing-null)
- [Example 12: Constrained String — SkuCode](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-12-constrained-string--skucode)
- [Example 13: Quantity as a Smart-Constructed Value Object](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-13-quantity-as-a-smart-constructed-value-object)
- [Example 14: Money Record with Currency](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-14-money-record-with-currency)
- [Example 15: Lifecycle States as a Discriminated Union](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-15-lifecycle-states-as-a-discriminated-union)
- [Example 16: State Machine Encoded Purely by Type Transitions](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-16-state-machine-encoded-purely-by-type-transitions)
- [Example 17: Domain Primitive Wrapping Decimal — Unit Price](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-17-domain-primitive-wrapping-decimal--unit-price)
- [Example 18: Units of Measure](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-18-units-of-measure)
- [Example 19: Email Value via Regex Validation](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-19-email-value-via-regex-validation)
- [Example 20: ProductCode as a Union of Two Subtypes](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-20-productcode-as-a-union-of-two-subtypes)
- [Example 21: PurchaseRequisitionLine Record — Composing Value Objects](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-21-purchaserequisitionline-record--composing-value-objects)
- [Example 22: PurchaseRequisition Aggregate Record](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-22-purchaserequisition-aggregate-record)
- [Example 23: UnvalidatedRequisition DTO-Shaped Record](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-23-unvalidatedrequisition-dto-shaped-record)
- [Example 24: Approval Level Derived from Requisition Total](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-24-approval-level-derived-from-requisition-total)
- [Example 25: Workflow Type Alias — Full SubmitRequisition Signature](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/beginner#example-25-workflow-type-alias--full-submitrequisition-signature)

### Intermediate (Examples 26–55)

- [Example 26: Function Composition with >>](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-26-function-composition-with-)
- [Example 27: Pipe Operator |>](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-27-pipe-operator-)
- [Example 28: Currying — Every F# Function is One-Arg](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-28-currying--every-f-function-is-one-arg)
- [Example 29: Workflow Expressed as Function Composition](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-29-workflow-expressed-as-function-composition)
- [Example 30: Result Type — Ok and Error](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-30-result-type--ok-and-error)
- [Example 31: Result.bind — Chaining Fallible Steps](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-31-resultbind--chaining-fallible-steps)
- [Example 32: Result.map — Transforming the Success Value](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-32-resultmap--transforming-the-success-value)
- [Example 33: Validation Accumulation with List of Errors](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-33-validation-accumulation-with-list-of-errors)
- [Example 34: Computation Expression for Result](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-34-computation-expression-for-result)
- [Example 35: Async Result — Effects at the Edges](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-35-async-result--effects-at-the-edges)
- [Example 36: Domain Error DU — Every Failure Mode Named](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-36-domain-error-du--every-failure-mode-named)
- [Example 37: PurchaseOrder Aggregate — Full State Machine](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-37-purchaseorder-aggregate--full-state-machine)
- [Example 38: Domain Events from State Transitions](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-38-domain-events-from-state-transitions)
- [Example 39: Supplier Aggregate — Lifecycle States](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-39-supplier-aggregate--lifecycle-states)
- [Example 40: Aggregate Boundary — What Goes Inside](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-40-aggregate-boundary--what-goes-inside)
- [Example 41: Refactor Primitive Obsession — Typed Wrapper](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-41-refactor-primitive-obsession--typed-wrapper)
- [Example 42: ValidatedPurchaseOrder Type — Emitted by Validation Step](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-42-validatedpurchaseorder-type--emitted-by-validation-step)
- [Example 43: IssuedPurchaseOrder Type — Emitted by Issue Step](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-43-issuedpurchaseorder-type--emitted-by-issue-step)
- [Example 44: ApprovePO Workflow Signature with Dependencies](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-44-approvepo-workflow-signature-with-dependencies)
- [Example 45: IssuePO Workflow Signature with Dependencies](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-45-issuepo-workflow-signature-with-dependencies)
- [Example 46: AcknowledgePO Workflow Signature](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-46-acknowledgepo-workflow-signature)
- [Example 47: Pipeline Composition — Wiring Three Workflow Steps](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-47-pipeline-composition--wiring-three-workflow-steps)
- [Example 48: Domain Error DU — Every Purchasing Failure Named](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-48-domain-error-du--every-purchasing-failure-named)
- [Example 49: Mapping Domain Error to API Error at the Boundary](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-49-mapping-domain-error-to-api-error-at-the-boundary)
- [Example 50: Pushing Effects to the Edges](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-50-pushing-effects-to-the-edges)
- [Example 51: Pure Core Wrapping at the Edge](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-51-pure-core-wrapping-at-the-edge)
- [Example 52: Dependency Injection via Partial Application](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-52-dependency-injection-via-partial-application)
- [Example 53: Persistence Interface as a Record of Functions](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-53-persistence-interface-as-a-record-of-functions)
- [Example 54: Approval Level Enforcement — Invariant in the Domain](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-54-approval-level-enforcement--invariant-in-the-domain)
- [Example 55: Cancellation Workflow — Off-Ramp from Any Pre-Paid State](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/intermediate#example-55-cancellation-workflow--off-ramp-from-any-pre-paid-state)

### Advanced (Examples 56–80)

- [Example 56: Serialization — JSON via DTO Boundary](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-56-serialization--json-via-dto-boundary)
- [Example 57: Date/Time as a Domain Concept](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-57-datetime-as-a-domain-concept)
- [Example 58: GoodsReceiptNote Aggregate — Receiving Context](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-58-goodsreceiptnote-aggregate--receiving-context)
- [Example 59: Invoice Aggregate — Three-Way Matching](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-59-invoice-aggregate--three-way-matching)
- [Example 60: EventStore vs Repository — Trade-offs](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-60-eventstore-vs-repository--trade-offs)
- [Example 61: Bounded Context Boundary as Module + Signature](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-61-bounded-context-boundary-as-module--signature)
- [Example 62: ACL as a Translation Function Between Contexts](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-62-acl-as-a-translation-function-between-contexts)
- [Example 63: Published Language — DU of Public Events](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-63-published-language--du-of-public-events)
- [Example 64: Factory Function for PurchaseOrder](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-64-factory-function-for-purchaseorder)
- [Example 65: Repository as Function-Type Alias](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-65-repository-as-function-type-alias)
- [Example 66: Dependency Rejection — No Optional Dependencies](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-66-dependency-rejection--no-optional-dependencies)
- [Example 67: Cross-Context Consistency — Eventual vs Strong](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-67-cross-context-consistency--eventual-vs-strong)
- [Example 68: Property-Based Test for an Invariant — FsCheck](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-68-property-based-test-for-an-invariant--fscheck)
- [Example 69: Compile-Time vs Runtime Check — Comparison](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-69-compile-time-vs-runtime-check--comparison)
- [Example 70: Workflow Testing Without Mocks](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-70-workflow-testing-without-mocks)
- [Example 71: Evolution Scenario 1 — Adding a Supplier Preferred Currency](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-71-evolution-scenario-1--adding-a-supplier-preferred-currency)
- [Example 72: Evolution Scenario 2 — Adding a Three-Way Match Tolerance Override](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-72-evolution-scenario-2--adding-a-three-way-match-tolerance-override)
- [Example 73: Evolution Scenario 3 — Murabaha Finance Context (Optional)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-73-evolution-scenario-3--murabaha-finance-context-optional)
- [Example 74: Bounded Context Integration Map](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-74-bounded-context-integration-map)
- [Example 75: Long-Running Workflow — Approval Saga](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-75-long-running-workflow--approval-saga)
- [Example 76: Interop with C# Caller — Workflow Exposed as Task](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-76-interop-with-c-caller--workflow-exposed-as-task)
- [Example 77: CQRS — Separate Read and Write Models](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-77-cqrs--separate-read-and-write-models)
- [Example 78: Invoice Payment Workflow — Full Pipeline](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-78-invoice-payment-workflow--full-pipeline)
- [Example 79: Domain Model Evolution — Adding a New State](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-79-domain-model-evolution--adding-a-new-state)
- [Example 80: Full System Sketch — Procurement Platform End-to-End](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/advanced#example-80-full-system-sketch--procurement-platform-end-to-end)
