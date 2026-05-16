---
title: "DDD + Hexagonal in Practice — Java in the Field"
weight: 10000011
date: 2026-05-16T00:00:00+07:00
draft: false
description: "Overview of the production wiring tutorial — how DDD aggregates flow through a real Java / Spring Boot 4 hexagonal codebase dogfooded against apps/organiclever-be"
tags: ["ddd", "hexagonal-architecture", "java", "spring-boot", "in-the-field", "organiclever-be"]
---

**You have finished the by-example tracks. You know what an aggregate is. You know what a port is. Now the question is: how do they wire together in a real production Java codebase that ships?** This tutorial answers that question using `apps/organiclever-be` — the Java 25 / Spring Boot 4 backend of the OrganicLever productivity tracker — as the running domain.

Every guide in this series traces a single wiring seam: how a Spring `@RestController` parses an HTTP request into a command record, how a domain aggregate processes that command, how an output port interface carries the result to a Spring-managed adapter, and how a unit test swaps that adapter for an in-memory stub. No toy examples. No generic order-taking stories. Real packages, real Spring wiring decisions, real production constraints.

## Prerequisites

**Both of the following tutorials are required reading before this one:**

- [Domain-Driven Design — OOP by Example](/en/learn/software-engineering/architecture/domain-driven-design-ddd/oop-by-example/overview) — teaches DDD tactical patterns (aggregates, value objects, domain events, factories, repositories) using Java, Kotlin, and C# examples.
- [Hexagonal Architecture — OOP by Example](/en/learn/software-engineering/architecture/hexagonal-architecture/oop-by-example/overview) — teaches ports-and-adapters structure (primary adapters, output port interfaces, adapter swapping, integration test seams) in Java and related OOP languages.

**This tutorial does NOT re-teach DDD or hexagonal fundamentals.** Terms like _aggregate_, _port_, _adapter_, _bounded context_, and _repository pattern_ are used without definition. If any of those feel unfamiliar, complete the prerequisite tracks first. The guides here are about wiring — how the pieces connect in production — not about what the pieces are.

## Running Domain

The running domain for all guides is `apps/organiclever-be` — the Java 25 / Spring Boot 4.0.6 backend of the OrganicLever productivity tracker.

The application is in its early scaffold phase. The production codebase today contains:

| Path | Purpose |
| --- | --- |
| `src/main/java/com/organicleverbe/OrganicleverBeApplication.java` | Spring Boot bootstrap (`@SpringBootApplication`) |
| `src/main/java/com/organicleverbe/config/CorsConfig.java` | CORS configuration (`WebMvcConfigurer`) |
| `src/main/java/com/organicleverbe/config/GlobalExceptionHandler.java` | Global exception handler (`@RestControllerAdvice`) |
| `src/main/java/com/organicleverbe/health/controller/HealthController.java` | Health check controller (`GET /api/v1/health`) |

The intended layout for production bounded contexts follows the hexagonal package structure:

```
com.organicleverbe.<context>/
  domain/       — aggregates, value objects, domain events (no Spring annotations)
  application/  — application services, output port interfaces
  infrastructure/ — Spring-managed adapters (JPA repos, HTTP clients, messaging)
  presentation/ — Spring @RestController adapters (HTTP entry points)
```

None of the `<context>/{domain,application,infrastructure,presentation}` packages exist yet — they are the intended layout that the OOP track guides describe. The health controller sits in `com.organicleverbe.health.controller/`, a flat layout that predates the hexagonal migration.

## Dogfooding Modes

Every code block in this tutorial is grounded in one of three modes:

**Mirror mode** (preferred) — the snippet copies a real file at authoring time. A `Source:` line immediately following the block links to the original file. Example:

> Source: [apps/organiclever-be/src/main/java/com/organicleverbe/health/controller/HealthController.java](../../../../../../organiclever-be/src/main/java/com/organicleverbe/health/controller/HealthController.java)

**Intended-layout mode** — when the per-context package exists in design but not yet on disk, the snippet shows the target file. A callout marks this explicitly:

> _New file — intended layout, scaffolding exists at `apps/organiclever-be/src/main/java/com/organicleverbe/<ctx>/<layer>/`_

**Illustrative mode** — a snippet that demonstrates a stdlib or language pattern in isolation, not grounded in `apps/organiclever-be` at all. A callout marks this explicitly:

> _Illustrative snippet — not from `apps/organiclever-be`; demonstrates the stdlib pattern in isolation._

Bare snippets without one of these three citations are forbidden by the checker for this tutorial.

## Sibling Tutorial

The functional programming parallel of this tutorial uses F# / Giraffe / Npgsql against `apps/ose-app-be`:

- [DDD + Hexagonal in Practice — F# in the Field](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/fp-in-the-field/overview)

Both tracks teach the same wiring concerns; the OOP track uses Java records, interfaces, Spring Boot 4 constructor injection, and JUnit 5 / Cucumber test infrastructure.

## Learning Path

- [Beginner (Guides 1–7)](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/beginner) — One context = one hexagon, reading the current flat layout, domain types without framework annotations, application service signatures, output port as Java interface, Spring `@RestController` as primary adapter, and Spring `@Configuration` as composition root.
- [Intermediate](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/intermediate) — Spring Data JPA adapter behind the repository port, domain event publisher port, Cucumber integration test seam wiring, and contract codegen consumed by a controller.
- [Advanced](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/advanced) — Cross-context Anti-Corruption Layer, docker-compose integration harness, external service port + adapter swap, and domain event flow inside a context.
- [Production](/en/learn/software-engineering/architecture/ddd-hexagonal-in-practice/oop-in-the-field/production) — Observability, failure-mode wiring, deployment hooks, and migration notes.
