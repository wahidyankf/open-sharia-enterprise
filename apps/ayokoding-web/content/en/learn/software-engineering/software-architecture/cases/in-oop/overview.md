---
title: "Overview"
weight: 10000011
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Overview of the In OOP case — how C4, DDD, Hexagonal Architecture, and Finite State Machines compose in a Java 25 / Spring Boot 4 codebase running the procurement-platform-be Procure-to-Pay domain"
tags: ["cases", "in-oop", "c4-model", "ddd", "hexagonal-architecture", "finite-state-machine", "java", "spring-boot"]
---

**You have finished the by-example tracks for C4, DDD, Hexagonal Architecture, and Finite State Machines. You know what an aggregate is. You know what a port is. You know how to draw a Container diagram. You know how to model a state machine. Now the question is: how do they all wire together in a real production Java codebase that ships?** This case answers that question using Java 25 / Spring Boot 4, running against a hypothetical Procure-to-Pay procurement platform backend.

Every guide in this series traces a single wiring seam: how a C4 Container decomposes into bounded-context components, how a Spring `@RestController` parses an HTTP request into a command record, how a domain aggregate processes that command through an explicit state machine, how an output port interface carries the result to a Spring-managed adapter, and how a unit test swaps that adapter for an in-memory stub. No toy examples. One coherent procurement-platform-be P2P domain — production-grade Spring wiring decisions — carried consistently across all twenty-seven guides.

## Prerequisites

**All four of the following by-example tracks are required reading before this case:**

- [C4 By Example](/en/learn/software-engineering/software-architecture/c4-model/by-example/overview) — teaches the four C4 levels (Context, Container, Component, Code) used to draw the procurement platform's architecture.
- [DDD By Example in OOP](/en/learn/software-engineering/software-architecture/domain-driven-design-ddd/in-oop-by-example/overview) — teaches DDD tactical patterns (aggregates, value objects, domain events, factories, repositories) primarily in Java 21+ using the same shared procurement-platform-be P2P domain.
- [Hexagonal Architecture By Example in OOP](/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-oop-by-example/overview) — teaches ports-and-adapters structure (primary adapters, output port interfaces, adapter swapping, integration test seams) in Java and related OOP languages.
- [FSM By Example in OOP](/en/learn/software-engineering/software-architecture/finite-state-machine-fsm/in-oop-by-example/overview) — teaches state-as-enum, transition methods on aggregates, and guard-condition encoding for aggregate lifecycles.

**This case does NOT re-teach C4, DDD, hexagonal, or FSM fundamentals.** Terms like _container_, _component_, _aggregate_, _port_, _adapter_, _bounded context_, _repository pattern_, and _state machine_ are used without definition. If any of those feel unfamiliar, complete the prerequisite tracks first. The guides here are about **wiring** — how the pieces connect in production — not about what the pieces are.

## How the Four Families Compose in the Java Codebase

The four architecture pattern families slot together at different levels of the Java codebase:

| Family  | Where It Lives in Java                                                                                           |
| ------- | ---------------------------------------------------------------------------------------------------------------- |
| **C4**  | Diagram artifacts checked in alongside source; bounded-context names match the C4 Component names                |
| **DDD** | Top-level packages per bounded context (`com.procurement.platform.purchasing`, etc.); aggregates as Java classes |
| **Hex** | `domain/` → `application/` (ports + use cases) → `infrastructure/` + `presentation/` → Spring `@Configuration`   |
| **FSM** | Aggregate state as a `state` field with `enum` or sealed `State` interface; transitions as aggregate methods     |

The composition root assembles concrete Spring beans into the port slots in `@Configuration` classes, then Spring Boot takes over the request pipeline.

## Running Domain — Procure-to-Pay Procurement Platform

Every guide reasons against the same hypothetical service: **`procurement-platform-be`**, the backend of a Procure-to-Pay (P2P) platform where employees request goods or services, managers approve, suppliers fulfill, and finance pays. Picking a single coherent domain lets the wiring decisions in Guide 15 reference the port introduced in Guide 5 and the aggregate introduced in Guide 3 without re-establishing context.

The platform is organized around six bounded contexts, introduced progressively across tiers:

| Bounded context    | Aggregate root                         | Responsibility                                                      |
| ------------------ | -------------------------------------- | ------------------------------------------------------------------- |
| `purchasing`       | `PurchaseRequisition`, `PurchaseOrder` | Requisition lifecycle, approval routing, PO issuance to supplier    |
| `supplier`         | `Supplier`                             | Vendor master, approval state, risk score                           |
| `receiving`        | `GoodsReceiptNote`                     | Goods receipt against PO, quantity verification, QC flag            |
| `invoicing`        | `Invoice`                              | Invoice registration, three-way matching (PO ↔ GRN ↔ Invoice)       |
| `payments`         | `Payment`                              | Payment run scheduling, bank disbursement, supplier remittance      |
| `murabaha-finance` | `MurabahaContract`                     | (Optional Sharia angle) bank buys asset, resells to buyer at markup |

The intended Java package layout for each context follows the hexagonal split:

```
com.procurement.platform.<context>/
  domain/         // aggregates, value objects, domain events, state machines (no Spring annotations)
  application/    // application services, output port interfaces
  infrastructure/ // Spring-managed adapters (JdbcClient repos, RestClient, messaging)
  presentation/   // Spring @RestController adapters (HTTP entry points)
```

Cross-context domain events travel between contexts via the `EventPublisher` port. The most important events used across guides are summarized below; each event is reintroduced inline in the first guide that uses it.

| Event                             | Source context | Consumers                                               |
| --------------------------------- | -------------- | ------------------------------------------------------- |
| `RequisitionSubmitted`            | purchasing     | approval-router                                         |
| `RequisitionApproved`             | purchasing     | purchasing (auto-converts to PO Draft)                  |
| `PurchaseOrderIssued`             | purchasing     | supplier-notifier (EDI/email), receiving                |
| `PurchaseOrderAcknowledged`       | purchasing     | receiving (opens GRN expectation)                       |
| `PurchaseOrderCancelled`          | purchasing     | supplier-notifier, accounting                           |
| `GoodsReceived`                   | receiving      | invoicing (enables matching), purchasing (state update) |
| `GoodsReceiptDiscrepancyDetected` | receiving      | invoicing (blocks matching), supplier-notifier          |
| `InvoiceMatched`                  | invoicing      | payments (schedules payment), purchasing                |
| `PaymentDisbursed`                | payments       | supplier-notifier, accounting, purchasing               |
| `SupplierApproved`                | supplier       | purchasing (eligible-for-PO list)                       |

## Code Grounding

Every code block in this case is **production-grade hypothetical Java**. That means:

- **Production-grade**: full error handling, observability hooks where the seam being taught calls for them, no "simplified for clarity" omissions.
- **Hypothetical**: no link to a real file. The snippets describe the wiring shape for the procurement platform. The shape is real; the specific file at any given commit of any real service is not the point.
- **Cross-guide consistent**: a port interface defined in Guide 5 keeps the same method signature in Guide 12. A package layout introduced in Guide 2 stays consistent through Guide 22. A state machine introduced in Guide 3 is referenced unchanged in Guide 18.

If you want to see real Java / Spring Boot codebases that use these patterns, look at any of the Java-track examples in the [Hexagonal Architecture By Example in OOP](/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-oop-by-example/overview) prerequisite.

## Guide Numbering

Guides are numbered monotonically across all difficulty tiers (1, 2, 3 … 27). Guide 1 appears in the beginner tier, Guide 27 appears at the end of the advanced tier. This makes cross-references unambiguous: "see Guide 5" means the same guide regardless of which tier page you are reading.

## Learning Path

- [Beginner (Guides 1–7)](/en/learn/software-engineering/software-architecture/cases/in-oop/beginner) — One context = one hexagon, per-context package layout, domain types without framework annotations, application service signatures, output port as Java interface, Spring `@RestController` as primary adapter, Spring `@Configuration` as composition root.
- [Intermediate (Guides 8–15)](/en/learn/software-engineering/software-architecture/cases/in-oop/intermediate) — Spring Data JDBC adapter behind the repository port, in-memory test adapter, domain event publisher port, outbox event adapter, full `@RestController` pipeline, contract codegen, cross-context ACL, composition root `@Configuration`.
- [Advanced (Guides 16–27)](/en/learn/software-engineering/software-architecture/cases/in-oop/advanced) — Testcontainers harness, Flyway migration adapter, banking port and Spring `RestClient` adapter, Resilience4j retry / circuit-breaker, Micrometer tracing observability adapter, end-to-end domain event flow, hexagonal anti-patterns, Kubernetes deployment topology, Micrometer + OTLP + Prometheus observability stack, failure-mode `HealthIndicator` wiring, Flyway at deploy time, configuration adapter (Secret to typed `@ConfigurationProperties`).

## Sibling Case

The functional programming parallel of this case uses F# / Giraffe / Npgsql against the same hypothetical Procure-to-Pay procurement platform:

- [In FP — F# / Giraffe / Npgsql](/en/learn/software-engineering/software-architecture/cases/in-fp/overview)

Both cases teach the same wiring concerns against the same domain; comparing the two side-by-side is the fastest way to see what changes when you swap object-oriented Java wiring for functional F# wiring.
