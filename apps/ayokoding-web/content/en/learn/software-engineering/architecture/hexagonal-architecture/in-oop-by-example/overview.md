---
title: "Overview"
weight: 10000002
date: 2026-05-15T00:00:00+07:00
draft: false
description: "Hexagonal Architecture By Example in OOP: 75 annotated examples in Java 21+ using the procurement-platform-be domain — covering port interfaces, adapter implementations, application services, and strategic design"
tags: ["hexagonal-architecture", "ports-and-adapters", "tutorial", "by-example", "oop", "java"]
---

**Want to apply Hexagonal Architecture with modern Java?** This tutorial teaches the Ports and Adapters pattern through 75 heavily annotated examples in Java 21+.

## What This Tutorial Is

This tutorial presents 75 examples showing hexagonal architecture concepts implemented in idiomatic Java 21+. Every example is self-contained, runnable, and annotated with `// =>` markers that show values, states, and effects at each step. Examples build progressively from three-zone structure through advanced multi-context patterns.

Each example demonstrates a focused hexagonal concept. Examples build progressively: the three-zone structure and basic port/adapter separation appear first, application service orchestration and infrastructure adapters follow, and strategic multi-context patterns close the tutorial. Every example follows a consistent five-part structure (see below).

## Prerequisites

- Comfortable with Java at an intermediate level
- Familiar with OOP fundamentals: classes, interfaces, inheritance, and composition
- Has read the paradigm-agnostic overview at [Hexagonal Architecture Overview](/en/learn/software-engineering/architecture/hexagonal-architecture/overview)

## How to Read This Tutorial

This tutorial is code-first. Each example leads with working, self-contained Java code annotated with `// =>` markers that show values, types, zones, and effects at each step.

The running domain across all examples is **procurement-platform-be** — a Procure-to-Pay (P2P) platform where employees request goods and services, managers approve, suppliers fulfill, and finance pays. The same domain is used in the [DDD OOP tutorial](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-oop-by-example/overview), letting you see how hexagonal boundaries fit around DDD building blocks.

## What This Tutorial Covers

**Core structural patterns**:

- The three-zone model — domain core, application layer, adapters
- Input ports (driving ports) — use case interfaces called by adapters
- Output ports (driven ports) — repository, notification, and service interfaces called by the application
- Primary adapters — HTTP controllers, CLI handlers, message consumers
- Secondary adapters — database repositories, email senders, external API clients
- In-memory adapters — fast, deterministic implementations for tests
- Dependency injection wiring — Spring `@Configuration` and manual constructor injection

**Intermediate patterns**:

- CQRS with separate command/query ports
- Domain events and event publishing ports (`EventPublisher`)
- Clock, approval-router, and configuration ports
- Retry, circuit-breaker, and idempotency in adapters
- Multiple bounded contexts as separate hexagons (`purchasing` + `supplier`)
- Anti-Corruption Layer adapters between contexts

**Advanced patterns**:

- Bounded context maps — hexagon relationships across `receiving`, `invoicing`, and `payments`
- `BankingPort` and retry-decorator adapter
- `SupplierNotifierPort` with SMTP/EDI fallback
- Observability adapters (OpenTelemetry via `Observability` port)
- Domain evolution — adding a new port without breaking the domain
- Adapter replacement without touching the domain

## What This Tutorial Does NOT Cover

- Language tutorials: Java has its own by-example tutorial for language fundamentals
- DDD tactical patterns in depth: read the [DDD OOP tutorial](/en/learn/software-engineering/architecture/domain-driven-design-ddd/in-oop-by-example/overview) for entities, aggregates, value objects, and domain services
- Framework setup and project bootstrapping (Spring Boot initialisation, Gradle/Maven configuration)
- Kubernetes, Docker, or deployment infrastructure

## Sibling Tutorial: Functional Programming Approach

If you prefer a functional programming treatment of Hexagonal Architecture, see [Hexagonal Architecture By Example in FP](/en/learn/software-engineering/architecture/hexagonal-architecture/in-fp-by-example/overview). That tutorial covers the same patterns through F# function type aliases, partial application, and Railway-Oriented Programming pipelines, using the same procurement-platform-be domain.

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation**: What hexagonal concept the example demonstrates (2–3 sentences).
2. **Optional Diagram**: A Mermaid diagram when concept relationships are complex enough to warrant visual representation. Skipped for straightforward code definitions.
3. **Code Block(s)**: Java 21+ implementation(s) with `// =>` annotations explaining values, states, zones, and effects. Multi-block examples separate distinct approaches with explanatory text between blocks.
4. **Key Takeaway**: The core hexagonal principle to retain (1–2 sentences).
5. **Why It Matters**: Real-world business and production system impact (50–100 words).

## Tutorial Structure

- [Beginner (Examples 1–20)](/en/learn/software-engineering/architecture/hexagonal-architecture/in-oop-by-example/beginner) — The three zones, port interfaces, adapter classes, package structure, dependency direction, in-memory adapters, application service wiring, and the full request/response flow — all using the `purchasing` context of `procurement-platform-be`.
- [Intermediate (Examples 21–55)](/en/learn/software-engineering/architecture/hexagonal-architecture/in-oop-by-example/intermediate) — `SupplierRepository`, `EventPublisher`, `ApprovalRouterPort`, adapter swapping, anti-corruption layer, integration test seam, composition root via Spring `@Configuration`, and CQRS command/query port split — purchasing and supplier contexts.
- [Advanced (Examples 56–75)](/en/learn/software-engineering/architecture/hexagonal-architecture/in-oop-by-example/advanced) — Strategic multi-hexagon design across `purchasing`, `receiving`, `invoicing`, and `payments` contexts; `BankingPort` with retry decorator; `SupplierNotifierPort`; `Observability` adapter; domain evolution; adapter replacement; anti-patterns; and a full production reference architecture.

## Examples by Level

### Beginner (Examples 1–20)

- Example 1: The hexagon metaphor — three zones as packages
- Example 2: Domain entity — pure Java record, no framework annotations
- Example 3: Value object — PurchaseOrderId and Money
- Example 4: The dependency rule — what can import what
- Example 5: Output port interface — PurchaseOrderRepository
- Example 6: Clock output port — making time testable
- Example 7: In-memory adapter implementing PurchaseOrderRepository
- Example 8: JPA adapter implementing PurchaseOrderRepository
- Example 9: Input port interface — use-case contract
- Example 10: Application service implementing the input port
- Example 11: Naming conventions — ports and adapters
- Example 12: Package/namespace structure for hexagonal
- Example 13: HTTP input adapter — thin controller
- Example 14: DTO at the adapter boundary — inbound and outbound
- Example 15: Multi-adapter — HTTP and CLI calling the same use case
- Example 16: Composition root — wiring the hexagon at startup
- Example 17: Dependency injection wiring without Spring
- Example 18: Testing with the in-memory adapter — fast, no DB
- Example 19: Domain error hierarchy at port boundaries
- Example 20: Full hexagonal flow — HTTP request to domain to response

### Intermediate (Examples 21–55)

- Example 21: SupplierRepository output port
- Example 22: Supplier domain aggregate with lifecycle states
- Example 23: In-memory SupplierRepository adapter
- Example 24: EventPublisher output port — decoupling cross-context side effects
- Example 25: ApprovalRouterPort — routing approval requests
- Example 26: Adapter swapping — switching from in-memory to Postgres at the composition root
- Example 27: Spring @Profile-based adapter selection
- Example 28: Integration test seam — testing the application service with real ports
- Example 29: Dependency rejection — refusing a supplier that is not APPROVED
- Example 30: Testing supplier eligibility rejection — fast unit test
- Example 31: Anti-corruption layer — translating supplier context types into purchasing
- Example 32: Kotlin — SupplierRepository port and in-memory adapter
- Example 33: EventPublisher — outbox adapter pattern (sketch)
- Example 34: ApprovalRouterPort — workflow engine adapter (sketch)
- Example 35: Full intermediate flow — two contexts, four ports, one request
- Example 36: Spring @Configuration with multiple bounded contexts
- Example 37: Constructor injection depth — no @Autowired in business classes
- Example 38: Kotlin — data class as command DTO with validation
- Example 39: ArchUnit — enforcing hexagonal dependency rules in CI
- Example 40: Full intermediate test suite — unit + integration coverage map
- Example 41: CQRS — separating command and query input ports
- Example 42: CQRS command service implementation
- Example 43: CQRS query service with read-only output port
- Example 44: CQRS — wiring command and query services at the composition root
- Example 45: CQRS — in-memory read adapter for fast query tests
- Example 46: Specialised query — findPendingApprovalByLevel
- Example 47: PO summary read model — projecting domain state for the UI
- Example 48: Paginated query port — findByStatus with pagination
- Example 49: Sorting output port parameter — domain-neutral sort specification
- Example 50: Composite query — combining sort and pagination
- Example 51: Adding a method to a port — backward-compatible evolution
- Example 52: Deprecating a port method without breaking adapters
- Example 53: Port interface segregation — splitting a fat port
- Example 54: Notification port — SupplierNotifierPort with multiple adapter strategies
- Example 55: Full intermediate flow — CQRS + query facade + notifier in one request lifecycle

### Advanced (Examples 56–75)

- Example 56: Receiving context — GoodsReceiptNote aggregate and port
- Example 57: Invoicing context — three-way match port
- Example 58: Payments context — Payment aggregate and BankingPort
- Example 59: SupplierNotifierPort — email and EDI fallback
- Example 60: Observability port — metrics and traces without framework lock-in
- Example 61: RetryingBankingAdapter — retry decorator for BankingPort
- Example 62: CircuitBreakingBankingAdapter — circuit-breaker decorator
- Example 63: Anti-corruption layer — receiving context translating purchasing events
- Example 64: Port versioning — adding a field without breaking existing adapters
- Example 65: Outbox pattern — EventPublisher adapter that guarantees delivery
- Example 66: Clock port — deterministic time in payments scheduling
- Example 67: Multi-context application service — InvoiceMatchingService with three ports
- Example 68: Murabaha context (optional) — port extension for Sharia financing
- Example 69: Anti-pattern — domain importing framework annotations
- Example 70: Anti-pattern — application service instantiating adapters directly
- Example 71: Anti-pattern — skipping the port (direct adapter call from domain)
- Example 72: Anti-pattern — fat adapter (business logic in adapter)
- Example 73: Composition root — wiring all contexts with Spring @Configuration
- Example 74: Integration test — wiring in-memory adapters for a full slice
- Example 75: Port adapter test contract — verifying all adapters meet port guarantees
