---
title: Hexagonal Architecture
description: OSE Platform authoritative standards for Hexagonal Architecture (Ports and Adapters) — port design, adapter conventions, composition root wiring, and testing boundaries
category: explanation
subcategory: architecture
tags:
  - hexagonal-architecture
  - ports-and-adapters
  - standards
  - organiclever
created: 2026-05-17
---

# Hexagonal Architecture

**This is THE authoritative reference** for Hexagonal Architecture (Ports and Adapters) standards in OSE Platform.

All hexagonal architecture implementations in OSE Platform MUST comply with the standards documented here. These standards are mandatory, not optional. Non-compliance blocks code review and merge approval.

## Prerequisite Knowledge

**REQUIRED**: This documentation assumes you have completed the AyoKoding Hexagonal Architecture learning path. These are **OSE Platform-specific standards**, not educational tutorials.

**You MUST understand hexagonal architecture fundamentals before using these standards:**

- **[Hexagonal Architecture Overview](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/overview.md)** — Core concepts: ports, adapters, domain isolation, driving vs driven ports
- **[Hexagonal Architecture By Example in FP](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-fp-by-example/overview.md)** — Ports as function type aliases, adapters as record literals, partial application DI
- **[Hexagonal Architecture By Example in OOP](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-oop-by-example/overview.md)** — Ports as interfaces, adapters as implementing classes, constructor DI

**What this documentation covers**: OSE Platform-specific port naming, adapter package placement, composition root wiring in Spring Boot and F#/Giraffe, Nx target integration, OrganicLever bounded context wiring, and hexagonal-specific testing conventions.

**What this documentation does NOT cover**: hexagonal architecture concepts, port/adapter theory, general DI patterns (those are in ayokoding-web).

**See**: [Programming Language Documentation Separation Convention](../../../../../repo-governance/conventions/structure/programming-language-docs-separation.md) for content separation rules.

## OSE Platform Hexagonal Architecture Standards (Authoritative)

**MUST follow these mandatory standards for all hexagonal implementations in OSE Platform:**

1. **[Port Standards](./port-standards.md)** — Port (interface) naming, ownership, package placement, and OrganicLever port catalog
2. **[Adapter Standards](./adapter-standards.md)** — Adapter (implementation) naming, package placement, forbidden imports, and polyglot conventions
3. **[Composition Root Standards](./composition-root-standards.md)** — DI wiring conventions for Spring Boot (Java) and Giraffe (F#), Nx-aware startup
4. **[Testing Standards](./testing-standards.md)** — Port contract tests, in-memory adapter swap, integration boundary rules, Nx target mapping

## OrganicLever Bounded Context Overview

OSE Platform Phase 1 centers on **OrganicLever** — a Sharia-compliant productivity tracker. The hexagonal boundaries align with the following bounded contexts (each maps to an Nx app boundary per [DDD Bounded Context Standards](../domain-driven-design-ddd/bounded-context-standards.md)):

| Bounded Context | Nx App             | Primary Stack      | Status  |
| --------------- | ------------------ | ------------------ | ------- |
| Purchasing      | `organiclever-be`  | Java/Spring Boot   | Active  |
| Supplier        | `organiclever-be`  | Java/Spring Boot   | Active  |
| Receiving       | `organiclever-be`  | Java/Spring Boot   | Active  |
| Invoicing       | `organiclever-be`  | Java/Spring Boot   | Planned |
| Payments        | `organiclever-be`  | Java/Spring Boot   | Planned |
| Frontend shell  | `organiclever-web` | Next.js/TypeScript | Active  |

The F#/Giraffe backend (`organiclever-be` F# layer) exposes the composition root and HTTP adapter while the domain and port definitions are language-agnostic by design.

## Dependency Direction Rule

**REQUIRED (all stacks)**: The dependency direction MUST always point inward.

```
Adapters → Application Layer → Domain Core
```

- Domain core imports nothing outside itself
- Application layer imports domain; imports port interfaces (never adapter implementations)
- Adapters import application layer and domain; never import other adapters

Violation of this rule (domain importing infrastructure, application importing a concrete adapter class) is a CRITICAL finding in code review.

## Integration with OSE Platform Architecture

### DDD Alignment

Hexagonal Architecture in OSE Platform operates **inside** DDD bounded contexts. The mapping is:

| DDD Concept         | Hexagonal Concept           |
| ------------------- | --------------------------- |
| Aggregate           | Domain core object          |
| Repository          | Output port + adapter       |
| Domain Service      | Domain core function/class  |
| Application Service | Input port implementation   |
| Domain Event        | Emitted through output port |

**See**: [DDD Standards](../domain-driven-design-ddd/README.md) for aggregate, value object, and domain event standards that govern what lives inside the hexagon.

### FSM Integration

Entity lifecycle state machines (FSM) live entirely in the domain core. Output ports carry state transition events outward. Adapters never contain state machine logic.

**See**: [FSM Standards](../finite-state-machine-fsm/README.md)

### C4 Alignment

Each bounded context hexagon corresponds to a C4 Container. Ports correspond to C4 Component interfaces. Adapters correspond to C4 Components that cross the container boundary.

**See**: [C4 Architecture Model](../c4-architecture-model/README.md)

## Principles Implemented/Respected

- **[Explicit Over Implicit](../../../../../repo-governance/principles/software-engineering/explicit-over-implicit.md)**: Port interfaces make every dependency contract explicit and visible at the boundary of the hexagon.
- **[Pure Functions Over Side Effects](../../../../../repo-governance/principles/software-engineering/pure-functions.md)**: Domain core MUST be pure; all effects are pushed to adapters outside the hexagon.
- **[Immutability Over Mutability](../../../../../repo-governance/principles/software-engineering/immutability.md)**: Domain objects passed through ports MUST be immutable value types.

## Related Documentation

- **[DDD Standards](../domain-driven-design-ddd/README.md)** — What lives inside the hexagon (aggregates, value objects, domain events)
- **[FSM Standards](../finite-state-machine-fsm/README.md)** — Entity lifecycle management within the domain core
- **[C4 Architecture Model](../c4-architecture-model/README.md)** — Container and component visualization of hexagonal layers
- **[Architecture Index](../README.md)** — All architecture pattern documentation
- **[Hexagonal Architecture Tutorials](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/)** — Educational counterpart (conceptual foundation and worked examples)
