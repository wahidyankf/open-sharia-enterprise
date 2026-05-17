---
title: "Overview"
weight: 10000002
date: 2026-05-09T00:00:00+07:00
draft: false
description: "DDD By Example in OOP: 75 annotated examples in Java 21+ using the procurement-platform-be P2P domain"
tags: ["ddd", "domain-driven-design", "tutorial", "by-example", "oop", "java", "kotlin", "csharp"]
---

**Want to apply DDD with modern OOP languages?** This tutorial teaches Domain-Driven Design tactical and strategic patterns through 75 heavily annotated examples in Java 21+.

## What This Tutorial Is

This tutorial presents 75 examples showing DDD concepts implemented in Java 21+ with heavily annotated code. Each example is self-contained, runnable, and annotated with `// =>` markers that show values, types, states, and effects at each step.

Each example demonstrates a focused DDD concept. Examples build progressively: tactical building blocks appear first, integration and layering patterns follow, and strategic patterns close the tutorial. Every example follows a consistent five-part structure (see below).

## Prerequisites

- Comfortable with at least one of Java, Kotlin, or C# at an intermediate level
- Familiar with OOP fundamentals: classes, interfaces, inheritance, and composition
- Has read the paradigm-agnostic DDD overview at [DDD Overview](/en/learn/software-engineering/architecture/domain-driven-design-ddd/overview)

## How to Read This Tutorial

This tutorial is code-first. Each example leads with working, self-contained code annotated with `// =>` markers that show values, types, states, and effects at each step.

Read one language deeply if you want to build fluency in that language's DDD idioms. Scan all three languages on each example if you want cross-language insight into how the same DDD concept maps onto different type systems and idioms. Language contrasts are noted in examples where the difference matters to the DDD pattern.

The running domain across all examples is the **`procurement-platform-be`** — the backend of a Procure-to-Pay (P2P) platform through which employees submit purchase requisitions, managers approve them, purchase orders are issued to suppliers, goods are received, invoices are matched against deliveries, and payments are disbursed. Using a single domain lets you see how individual DDD building blocks fit together into a coherent system.

## What This Tutorial Covers

**Tactical patterns**:

- Value Objects — identity-less, equality-by-value domain concepts
- Entities — objects with identity that persist through state changes
- Aggregates — consistency boundaries enforced through a root entity
- Repositories — collection-oriented persistence abstractions
- Domain Services — stateless logic that does not belong on any entity
- Domain Events — records of meaningful occurrences in the domain
- Application Services — orchestration layer between domain and infrastructure

**Strategic patterns**:

- Bounded Contexts — explicit model boundaries within a large domain
- Context Maps — relationships between bounded contexts (partnership, customer/supplier, conformist)
- Anti-Corruption Layer (ACL) — translation boundary protecting a model from external concepts

## What This Tutorial Does NOT Cover

- Language tutorials: Java, Kotlin, and C# each have their own by-example tutorials for language fundamentals
- Deep DDD theory: read the [DDD Overview](/en/learn/software-engineering/architecture/domain-driven-design-ddd/overview) for conceptual grounding; Evans's _Domain-Driven Design_ (Addison-Wesley, 2003) for comprehensive theory
- Event sourcing internals beyond the introductory example in the Advanced section
- CQRS infrastructure (projections, read-model persistence) beyond the pattern itself

## Sibling Tutorial: Functional Programming Approach

If you prefer a functional programming treatment of DDD, see [DDD By Example in FP](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-fp-by-example/overview). That tutorial covers the same strategic and tactical patterns through F# discriminated unions, Railway-Oriented Programming, and workflow pipelines, using the same shared procurement-platform-be P2P domain.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What DDD concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships are complex enough to warrant visual representation. Skipped for straightforward code definitions.
3. **Code Block(s)**: Primarily Java 21+ implementations with `// =>` annotations explaining values, states, and effects. A small number of examples sprinkle Kotlin variants to highlight idiom differences.
4. **Key Takeaway**: The core DDD principle to retain (1–2 sentences).
5. **Why It Matters**: Real-world business impact and production system context (50–100 words).

## Tutorial Structure

- [Beginner (Examples 1–25)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-oop-by-example/beginner) — Tactical building blocks: Value Objects, Entities, Aggregates, Repositories, Domain Services, Application Services, and Domain Events using the `purchasing` bounded context.
- [Intermediate (Examples 26–51)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-oop-by-example/intermediate) — Integration and layering patterns: Specifications, Factories, CQRS, hexagonal architecture, domain exception hierarchies, and Bounded Context packaging, adding the `supplier` context.
- [Advanced (Examples 56–79)](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-oop-by-example/advanced) — Strategic patterns and advanced tactical: Context Maps, Anti-Corruption Layers, cross-context ACL, repositories with infra adapters, event sourcing, sagas, temporal modelling, and common anti-patterns, adding the `receiving` and `invoicing` contexts. The `murabaha-finance` context is optional and introduced only where Sharia-compliant financing patterns add pedagogical value.

## Examples by Level

### Beginner (Examples 1–25)

- Example 1: Ubiquitous Language — naming domain types from the purchasing glossary
- Example 2: Value Object — immutable `Money`
- Example 3: Value Object — `SkuCode` with regex validation
- Example 4: Value Object — `Quantity` with `UnitOfMeasure`
- Example 5: Value Object — `RequisitionId` as a typed identity handle
- Example 6: Smart constructor — preventing invalid `Money` creation
- Example 7: Java 21 record compact constructor for `Quantity`
- Example 8: `ApprovalLevel` derived from `Money` total
- Example 9: Kotlin data class as Value Object — `Money`
- Example 10: C# record as Value Object — `SkuCode`
- Example 11: Entity vs Value Object — identity matters
- Example 12: `PurchaseRequisition` as the Aggregate Root
- Example 13: Adding line items and computing `estimatedTotal`
- Example 14: Immutability in practice — `with`-style copy via records
- Example 15: Factory method — `PurchaseRequisition.create`
- Example 16: State machine — `PurchaseRequisition` lifecycle
- Example 17: Guard methods — `canSubmit` and `canApprove`
- Example 18: Domain events — recording what happened
- Example 19: Optional — representing absent domain values
- Example 20: Kotlin data class — `PurchaseRequisition` line item (variety)
- Example 21: Value Object comparison and ordering — `Money`
- Example 22: Defensive copying — protecting mutable collections in the aggregate
- Example 23: C# record with `with`-expression — immutable `Quantity` revision
- Example 24: Aggregate boundary — rejecting external line item mutation
- Example 25: Putting it together — full `PurchaseRequisition` lifecycle in Java 21

### Intermediate (Examples 26–51)

- Example 26: Aggregate root — `PurchaseOrder` identity and boundary
- Example 27: Adding lines with aggregate-level invariant enforcement
- Example 28: `ApprovalLevel` derived value object — computed from `Money`
- Example 29: `Supplier` aggregate root with approval lifecycle
- Example 30: Sealed types for exhaustive state modeling
- Example 31: State machine transitions on the aggregate root
- Example 32: Handling invalid transitions with domain exceptions
- Example 33: Cancellation off-ramp — pre-Paid states
- Example 34: `Quantity` value object and tolerance for goods receipt matching
- Example 35: Immutable domain events as records
- Example 36: Collecting domain events on the aggregate
- Example 37: `RequisitionApproved` event triggering PO creation — domain event handler
- Example 38: `SupplierApproved` event — cross-context notification
- Example 39: Factory method on the aggregate — `PurchaseOrder.create`
- Example 40: Repository interface — persistence contract without infrastructure
- Example 41: Specification pattern for querying the repository
- Example 42: `GoodsReceiptNote` aggregate — receiving context introduction
- Example 43: Aggregate reconstitution — loading from an event store
- Example 44: Aggregate with `BankAccount` value object — supplier payment details
- Example 45: Anti-corruption layer — translating a legacy supplier DTO into the domain
- Example 46: Bounded context as a Java package — module boundary enforcement
- Example 47: Specification pattern — composing query predicates from domain concepts
- Example 48: CQRS split — separate read model for the approval dashboard
- Example 49: Hexagonal architecture — port and adapter for `PurchaseOrderRepository`
- Example 50: Domain exception hierarchy — modelling procurement failures
- Example 51: Domain service — three-currency budget check across `purchasing` lines

### Advanced (Examples 56–79)

- Example 56: Anti-Corruption Layer — translating `purchasing` vocabulary into `receiving`
- Example 57: ACL with sealed-type state translation — `Invoice` matching status
- Example 58: Domain event as cross-context integration contract — `InvoiceMatched`
- Example 59: Context Map — Open Host Service for supplier-facing API
- Example 60: Factory method — creating `GoodsReceiptNote` from validated inputs
- Example 61: Static factory with sealed result type — `Invoice.register`
- Example 62: Factory with external dependencies — `GoodsReceiptNote` using repository check
- Example 63: Abstract factory — constructing `Invoice` variants for standard vs Murabaha procurement
- Example 64: Repository interface — domain owns the contract, infrastructure provides the implementation
- Example 65: Repository with Unit of Work — coordinating `Invoice` and `GoodsReceiptNote` in one transaction
- Example 66: Repository with specification pattern — querying `Invoice` by business criteria
- Example 67: Repository + domain event outbox — `InvoiceMatched` written atomically with aggregate state
- Example 68: Dependency Inversion in application services — `PaymentSchedulingService`
- Example 69: `MurabahaContract` aggregate — Sharia-compliant procurement financing
- Example 70: Kotlin — data-class aggregates with `copy` for immutable state transitions
- Example 71: C# — record aggregates with `with` expression for immutable receiving
- Example 72: Three-way match domain service — `receiving`, `invoicing`, and `purchasing` coordinated
- Example 73: Aggregate boundary decision — why `Invoice` and `GoodsReceiptNote` are separate aggregates
- Example 74: Temporal value object — `ValidityPeriod` for contract terms
- Example 75: Saga — coordinating `PurchaseOrder` issuance across `purchasing` and `supplier` contexts
- Example 76: Event sourcing — replaying `PurchaseRequisition` history
- Example 77: Common anti-pattern — Anemic Domain Model in procurement
- Example 78: Common anti-pattern — Leaky Abstraction and primitive obsession
- Example 79: Open Host Service — publishing a `supplier` API for external consumers
