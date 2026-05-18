---
title: "Overview"
weight: 10000002
date: 2026-05-09T00:00:00+07:00
draft: false
description: "DDD By Example in OOP: 75 annotated examples in Java 21+, Kotlin, C#, and TypeScript using the procurement-platform-be P2P domain"
tags:
  [
    "ddd",
    "domain-driven-design",
    "tutorial",
    "by-example",
    "oop",
    "java",
    "kotlin",
    "csharp",
    "typescript",
    "nestjs",
    "asp-net-core",
  ]
---

**Want to apply DDD with modern OOP languages?** This tutorial teaches Domain-Driven Design tactical and strategic patterns through 75 heavily annotated examples in Java 21+, Kotlin, C#, and TypeScript.

## What This Tutorial Is

This tutorial presents 75 examples showing DDD concepts implemented in Java 21+, Kotlin, C#, and TypeScript with heavily annotated code. Each example is self-contained, runnable, and annotated with `// =>` markers that show values, types, states, and effects at each step. Java is the canonical language carrying the deepest annotations; Kotlin, C#, and TypeScript variants appear where language-idiomatic differences matter to the DDD pattern.

Each example demonstrates a focused DDD concept. Examples build progressively: tactical building blocks appear first, integration and layering patterns follow, and strategic patterns close the tutorial. Every example follows a consistent five-part structure (see below).

## Prerequisites

- Comfortable with at least one of Java, Kotlin, C#, or TypeScript at an intermediate level (whichever language you ship with)
- Familiar with OOP fundamentals: classes, interfaces, inheritance, and composition
- Has read the paradigm-agnostic DDD overview at [DDD Overview](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/overview)

## How to Read This Tutorial

This tutorial is code-first. Each example leads with working, self-contained code annotated with `// =>` markers that show values, types, states, and effects at each step.

Read one language deeply if you want to build fluency in that language's DDD idioms. Scan all four languages on each example if you want cross-language insight into how the same DDD concept maps onto different type systems and idioms. Language contrasts are noted in examples where the difference matters to the DDD pattern.

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

- Language tutorials: Java, Kotlin, C#, and TypeScript each have their own by-example tutorials for language fundamentals
- Deep DDD theory: read the [DDD Overview](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/overview) for conceptual grounding; Evans's _Domain-Driven Design_ (Addison-Wesley, 2003) for comprehensive theory
- Event sourcing internals beyond the introductory example in the Advanced section
- CQRS infrastructure (projections, read-model persistence) beyond the pattern itself

## Sibling Tutorial: Functional Programming Approach

If you prefer a functional programming treatment of DDD, see [DDD By Example in FP](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-fp-by-example/overview). That tutorial covers the same strategic and tactical patterns through F# discriminated unions, Railway-Oriented Programming, and workflow pipelines, using the same shared procurement-platform-be P2P domain.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What DDD concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships are complex enough to warrant visual representation. Skipped for straightforward code definitions.
3. **Code Block(s)**: Java 21+ implementations (canonical) with `// =>` annotations explaining values, states, and effects. Kotlin, C#, and TypeScript variants appear where language-idiomatic differences matter to the DDD pattern.
4. **Key Takeaway**: The core DDD principle to retain (1–2 sentences).
5. **Why It Matters**: Real-world business impact and production system context (50–100 words).

## Tutorial Structure

- [Beginner (Examples 1–25)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner) — Tactical building blocks: Value Objects, Entities, Aggregates, Repositories, Domain Services, Application Services, and Domain Events using the `purchasing` bounded context.
- [Intermediate (Examples 26–51)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate) — Integration and layering patterns: Specifications, Factories, CQRS, hexagonal architecture, domain exception hierarchies, and Bounded Context packaging, adding the `supplier` context.
- [Advanced (Examples 56–79)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced) — Strategic patterns and advanced tactical: Context Maps, Anti-Corruption Layers, cross-context ACL, repositories with infra adapters, event sourcing, sagas, temporal modelling, and common anti-patterns, adding the `receiving` and `invoicing` contexts. The `murabaha-finance` context is optional and introduced only where Sharia-compliant financing patterns add pedagogical value.

## Examples by Level

### Beginner (Examples 1–25)

- [Example 1: Ubiquitous Language — naming domain types from the purchasing glossary](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-1-ubiquitous-language--naming-domain-types-from-the-purchasing-glossary)
- [Example 2: Value Object — immutable `Money`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-2-value-object--immutable-money)
- [Example 3: Value Object — `SkuCode` with regex validation](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-3-value-object--skucode-with-regex-validation)
- [Example 4: Value Object — `Quantity` with `UnitOfMeasure`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-4-value-object--quantity-with-unitofmeasure)
- [Example 5: Value Object — `RequisitionId` as a typed identity handle](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-5-value-object--requisitionid-as-a-typed-identity-handle)
- [Example 6: Smart constructor — preventing invalid `Money` creation](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-6-smart-constructor--preventing-invalid-money-creation)
- [Example 7: Java 21 record compact constructor for `Quantity`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-7-java-21-record-compact-constructor-for-quantity)
- [Example 8: `ApprovalLevel` derived from `Money` total](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-8-approvallevel-derived-from-money-total)
- [Example 9: Kotlin data class as Value Object — `Money`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-9-kotlin-data-class-as-value-object--money)
- [Example 10: C# record as Value Object — `SkuCode`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-10-c-record-as-value-object--skucode)
- [Example 11: Entity vs Value Object — identity matters](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-11-entity-vs-value-object--identity-matters)
- [Example 12: `PurchaseRequisition` as the Aggregate Root](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-12-purchaserequisition-as-the-aggregate-root)
- [Example 13: Adding line items and computing `estimatedTotal`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-13-adding-line-items-and-computing-estimatedtotal)
- [Example 14: Immutability in practice — `with`-style copy via records](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-14-immutability-in-practice--with-style-copy-via-records)
- [Example 15: Factory method — `PurchaseRequisition.create`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-15-factory-method--purchaserequisitioncreate)
- [Example 16: State machine — `PurchaseRequisition` lifecycle](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-16-state-machine--purchaserequisition-lifecycle)
- [Example 17: Guard methods — `canSubmit` and `canApprove`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-17-guard-methods--cansubmit-and-canapprove)
- [Example 18: Domain events — recording what happened](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-18-domain-events--recording-what-happened)
- [Example 19: Optional — representing absent domain values](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-19-optional--representing-absent-domain-values)
- [Example 20: Kotlin data class — `PurchaseRequisition` line item (variety)](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-20-kotlin-data-class--purchaserequisition-line-item-variety)
- [Example 21: Value Object comparison and ordering — `Money`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-21-value-object-comparison-and-ordering--money)
- [Example 22: Defensive copying — protecting mutable collections in the aggregate](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-22-defensive-copying--protecting-mutable-collections-in-the-aggregate)
- [Example 23: C# record with `with`-expression — immutable `Quantity` revision](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-23-c-record-with-with-expression--immutable-quantity-revision)
- [Example 24: Aggregate boundary — rejecting external line item mutation](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-24-aggregate-boundary--rejecting-external-line-item-mutation)
- [Example 25: Putting it together — full `PurchaseRequisition` lifecycle in Java 21](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/beginner#example-25-putting-it-together--full-purchaserequisition-lifecycle-in-java-21)

### Intermediate (Examples 26–51)

- [Example 26: Aggregate root — `PurchaseOrder` identity and boundary](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-26-aggregate-root--purchaseorder-identity-and-boundary)
- [Example 27: Adding lines with aggregate-level invariant enforcement](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-27-adding-lines-with-aggregate-level-invariant-enforcement)
- [Example 28: `ApprovalLevel` derived value object — computed from `Money`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-28-approvallevel-derived-value-object--computed-from-money)
- [Example 29: `Supplier` aggregate root with approval lifecycle](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-29-supplier-aggregate-root-with-approval-lifecycle)
- [Example 30: Sealed types for exhaustive state modeling](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-30-sealed-types-for-exhaustive-state-modeling)
- [Example 31: State machine transitions on the aggregate root](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-31-state-machine-transitions-on-the-aggregate-root)
- [Example 32: Handling invalid transitions with domain exceptions](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-32-handling-invalid-transitions-with-domain-exceptions)
- [Example 33: Cancellation off-ramp — pre-Paid states](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-33-cancellation-off-ramp--pre-paid-states)
- [Example 34: `Quantity` value object and tolerance for goods receipt matching](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-34-quantity-value-object-and-tolerance-for-goods-receipt-matching)
- [Example 35: Immutable domain events as records](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-35-immutable-domain-events-as-records)
- [Example 36: Collecting domain events on the aggregate](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-36-collecting-domain-events-on-the-aggregate)
- [Example 37: `RequisitionApproved` event triggering PO creation — domain event handler](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-37-requisitionapproved-event-triggering-po-creation--domain-event-handler)
- [Example 38: `SupplierApproved` event — cross-context notification](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-38-supplierapproved-event--cross-context-notification)
- [Example 39: Factory method on the aggregate — `PurchaseOrder.create`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-39-factory-method-on-the-aggregate--purchaseordercreate)
- [Example 40: Repository interface — persistence contract without infrastructure](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-40-repository-interface--persistence-contract-without-infrastructure)
- [Example 41: Specification pattern for querying the repository](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-41-specification-pattern-for-querying-the-repository)
- [Example 42: `GoodsReceiptNote` aggregate — receiving context introduction](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-42-goodsreceiptnote-aggregate--receiving-context-introduction)
- [Example 43: Aggregate reconstitution — loading from an event store](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-43-aggregate-reconstitution--loading-from-an-event-store)
- [Example 44: Aggregate with `BankAccount` value object — supplier payment details](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-44-aggregate-with-bankaccount-value-object--supplier-payment-details)
- [Example 45: Anti-corruption layer — translating a legacy supplier DTO into the domain](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-45-anti-corruption-layer--translating-a-legacy-supplier-dto-into-the-domain)
- [Example 46: Bounded context as a Java package — module boundary enforcement](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-46-bounded-context-as-a-java-package--module-boundary-enforcement)
- [Example 47: Specification pattern — composing query predicates from domain concepts](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-47-specification-pattern--composing-query-predicates-from-domain-concepts)
- [Example 48: CQRS split — separate read model for the approval dashboard](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-48-cqrs-split--separate-read-model-for-the-approval-dashboard)
- [Example 49: Hexagonal architecture — port and adapter for `PurchaseOrderRepository`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-49-hexagonal-architecture--port-and-adapter-for-purchaseorderrepository)
- [Example 50: Domain exception hierarchy — modelling procurement failures](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-50-domain-exception-hierarchy--modelling-procurement-failures)
- [Example 51: Domain service — three-currency budget check across `purchasing` lines](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/intermediate#example-51-domain-service--three-currency-budget-check-across-purchasing-lines)

### Advanced (Examples 56–79)

- [Example 56: Anti-Corruption Layer — translating `purchasing` vocabulary into `receiving`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-56-anti-corruption-layer--translating-purchasing-vocabulary-into-receiving)
- [Example 57: ACL with sealed-type state translation — `Invoice` matching status](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-57-acl-with-sealed-type-state-translation--invoice-matching-status)
- [Example 58: Domain event as cross-context integration contract — `InvoiceMatched`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-58-domain-event-as-cross-context-integration-contract--invoicematched)
- [Example 59: Context Map — Open Host Service for supplier-facing API](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-59-context-map--open-host-service-for-supplier-facing-api)
- [Example 60: Factory method — creating `GoodsReceiptNote` from validated inputs](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-60-factory-method--creating-goodsreceiptnote-from-validated-inputs)
- [Example 61: Static factory with sealed result type — `Invoice.register`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-61-static-factory-with-sealed-result-type--invoiceregister)
- [Example 62: Factory with external dependencies — `GoodsReceiptNote` using repository check](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-62-factory-with-external-dependencies--goodsreceiptnote-using-repository-check)
- [Example 63: Abstract factory — constructing `Invoice` variants for standard vs Murabaha procurement](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-63-abstract-factory--constructing-invoice-variants-for-standard-vs-murabaha-procurement)
- [Example 64: Repository interface — domain owns the contract, infrastructure provides the implementation](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-64-repository-interface--domain-owns-the-contract-infrastructure-provides-the-implementation)
- [Example 65: Repository with Unit of Work — coordinating `Invoice` and `GoodsReceiptNote` in one transaction](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-65-repository-with-unit-of-work--coordinating-invoice-and-goodsreceiptnote-in-one-transaction)
- [Example 66: Repository with specification pattern — querying `Invoice` by business criteria](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-66-repository-with-specification-pattern--querying-invoice-by-business-criteria)
- [Example 67: Repository + domain event outbox — `InvoiceMatched` written atomically with aggregate state](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-67-repository--domain-event-outbox--invoicematched-written-atomically-with-aggregate-state)
- [Example 68: Dependency Inversion in application services — `PaymentSchedulingService`](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-68-dependency-inversion-in-application-services--paymentschedulingservice)
- [Example 69: `MurabahaContract` aggregate — Sharia-compliant procurement financing](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-69-murabahacontract-aggregate--sharia-compliant-procurement-financing)
- [Example 70: Kotlin — data-class aggregates with `copy` for immutable state transitions](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-70-kotlin--data-class-aggregates-with-copy-for-immutable-state-transitions)
- [Example 71: C# — record aggregates with `with` expression for immutable receiving](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-71-c--record-aggregates-with-with-expression-for-immutable-receiving)
- [Example 72: Three-way match domain service — `receiving`, `invoicing`, and `purchasing` coordinated](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-72-three-way-match-domain-service--receiving-invoicing-and-purchasing-coordinated)
- [Example 73: Aggregate boundary decision — why `Invoice` and `GoodsReceiptNote` are separate aggregates](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-73-aggregate-boundary-decision--why-invoice-and-goodsreceiptnote-are-separate-aggregates)
- [Example 74: Temporal value object — `ValidityPeriod` for contract terms](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-74-temporal-value-object--validityperiod-for-contract-terms)
- [Example 75: Saga — coordinating `PurchaseOrder` issuance across `purchasing` and `supplier` contexts](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-75-saga--coordinating-purchaseorder-issuance-across-purchasing-and-supplier-contexts)
- [Example 76: Event sourcing — replaying `PurchaseRequisition` history](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-76-event-sourcing--replaying-purchaserequisition-history)
- [Example 77: Common anti-pattern — Anemic Domain Model in procurement](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-77-common-anti-pattern--anemic-domain-model-in-procurement)
- [Example 78: Common anti-pattern — Leaky Abstraction and primitive obsession](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-78-common-anti-pattern--leaky-abstraction-and-primitive-obsession)
- [Example 79: Open Host Service — publishing a `supplier` API for external consumers](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/advanced#example-79-open-host-service--publishing-a-supplier-api-for-external-consumers)
