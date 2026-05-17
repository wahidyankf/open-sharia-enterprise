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
