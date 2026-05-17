---
title: Architecture
description: Comprehensive documentation on software architecture patterns, models, and design approaches
category: explanation
subcategory: architecture
tags:
  - architecture
  - c4-model
  - domain-driven-design
  - software-design
  - index
created: 2026-01-20
---

# Architecture

**Understanding-oriented documentation** on software architecture patterns, models, and design approaches for building scalable, maintainable enterprise systems.

## Overview

**The Challenge**: You're building a complex system. Stakeholders ask "how does it work?" Junior developers ask "where should this code go?" Teams in different contexts use different terms for the same concept. Architecture documentation gets outdated the moment you write it.

**Our Approach**: We combine two complementary practices that solve different but related problems:

1. **C4 Architecture Model** - Visual communication of software architecture through hierarchical diagrams
2. **Domain-Driven Design (DDD)** - Strategic and tactical patterns for modeling complex business domains

These approaches work together to help teams design, communicate, and implement robust software systems that align with business needs while maintaining technical excellence.

## Why Architecture Documentation Matters

Clear architecture documentation delivers tangible benefits:

- **Faster Onboarding** - New developers understand system structure in hours instead of weeks
- **Better Communication** - Stakeholders, developers, and domain experts share a common visual language
- **Reduced Technical Debt** - Explicit boundaries and responsibilities prevent "big ball of mud" architectures
- **Confident Evolution** - Teams make changes knowing the ripple effects and integration points

## Quick Decision: Which Documentation Do I Need?

| Your Situation                                       | Start With                                                 |
| ---------------------------------------------------- | ---------------------------------------------------------- |
| Need to explain system to stakeholders               | [C4 System Context](./c4-architecture-model/README.md)     |
| Building complex business rules system               | [DDD Introduction](./domain-driven-design-ddd/README.md)   |
| Aligning multiple teams on integration               | DDD Context Mapping                                        |
| Creating architecture diagrams from scratch          | [C4 Architecture Model](./c4-architecture-model/README.md) |
| Modeling domain logic with functional programming    | DDD and Functional Programming                             |
| Combining strategic design with visual communication | DDD and C4 Integration                                     |

---

### 🎨 [C4 Architecture Model](./c4-architecture-model/README.md)

**Visualizing software architecture through hierarchical abstraction levels**

The C4 model provides a systematic way to create architecture diagrams at four levels of detail (Context, Container, Component, Code). Created by Simon Brown, it offers a developer-friendly alternative to heavyweight modeling approaches.

**Use C4 when you need to:**

- Create clear, consistent architecture diagrams for diverse audiences
- Document systems at multiple levels of abstraction
- Communicate technical decisions to both developers and stakeholders
- Maintain lightweight but rigorous architecture documentation

### 🏛️ [Domain-Driven Design (DDD)](./domain-driven-design-ddd/README.md)

**Strategic and tactical patterns for modeling complex business domains**

Domain-Driven Design places the business domain at the center of software design. Introduced by Eric Evans in 2003, DDD helps teams manage complexity in large-scale systems.

DDD provides two complementary pattern sets: strategic design for understanding the business and tactical patterns for implementing the domain model.

**Use DDD when you have:**

- Complex business logic with numerous rules and invariants
- Access to domain experts for collaboration
- Long-lived systems expected to evolve over years
- High cost of defects or regulatory compliance requirements

### ⬡ [Hexagonal Architecture](./hexagonal-architecture/README.md)

**OSE Platform port, adapter, composition root, and testing conventions**

Hexagonal Architecture (Ports and Adapters) isolates the domain core from all infrastructure concerns. OSE Platform conventions govern port naming and ownership, adapter package placement and forbidden imports, composition root wiring for Spring Boot (Java) and Giraffe (F#), and the port-contract-test pattern that maps to Nx `test:unit` / `test:integration` targets.

**Use Hexagonal Architecture conventions when you are:**

- Designing a new port interface for an OrganicLever bounded context
- Implementing a database, messaging, or HTTP adapter
- Wiring adapters in a Spring Boot `@Configuration` class or F# `CompositionRoot`
- Writing port contract tests or setting up an in-memory adapter for `test:unit`

### 🔄 [Finite State Machine (FSM)](./finite-state-machine-fsm/README.md)

**Standards for entity lifecycle management using finite state machines**

Finite State Machines provide a formal model for managing entity lifecycle transitions with explicit states, events, and guards. OSE Platform uses FSMs for modeling domain entity lifecycles, workflow state management, and business process automation.

**Use FSM when you have:**

- Entities with well-defined lifecycle states (e.g., order: pending → confirmed → shipped)
- Business processes requiring explicit state transition rules
- Domain logic that depends on current state
- Need for auditable state history

### 🧩 [DDD + Hexagonal In Practice](./ddd-hexagonal-in-practice/README.md)

**OSE Platform conventions for combining DDD with Hexagonal Architecture**

Strategic DDD (bounded contexts, context maps) and tactical DDD (aggregates, value objects, domain events) integrate cleanly with Hexagonal port-and-adapter structure. OSE Platform conventions govern bounded-context-to-port mapping, aggregate-port boundaries, cross-context integration via domain events and ACLs, and module organization for combined projects across Java/Kotlin and F#/Giraffe stacks.

**Use these conventions when you are:**

- Mapping a new bounded context onto port/adapter structure for OrganicLever
- Deciding aggregate-to-port boundaries and write-port granularity
- Designing cross-context integration with domain events, ACL, OHS, or sagas
- Organizing modules/packages for a combined DDD + Hexagonal codebase

---

## How C4 and DDD Work Together

C4 and DDD complement each other throughout the design process:

| DDD Concept             | Maps to C4 Level | Purpose                                                       |
| ----------------------- | ---------------- | ------------------------------------------------------------- |
| **Context Maps**        | System Context   | Shows how bounded contexts relate to external systems         |
| **Bounded Contexts**    | Containers       | Each bounded context typically becomes one or more containers |
| **Aggregates**          | Components       | Major aggregates often become components within a container   |
| **Domain Events**       | Dynamic Diagrams | Event flows visualized across components and containers       |
| **Ubiquitous Language** | Diagram Labels   | Consistent terminology across all diagrams                    |

**Example workflow:**

1. Use **Event Storming** (DDD) to discover domain events and bounded contexts
2. Create **Context Map** (DDD) showing relationships between bounded contexts
3. Draw **System Context diagram** (C4) showing bounded contexts as containers
4. Design **Aggregates** (DDD) within each bounded context
5. Create **Component diagrams** (C4) showing aggregates and their relationships
6. Document **runtime behavior** with Dynamic diagrams (C4) and Domain Events (DDD)

See DDD and C4 Integration for comprehensive examples and guidance.

---

### For Architects and Technical Leads

1. **Apply to projects** - Apply C4 and DDD patterns to your architecture

### For Developers

1. **Quick visualization start** - Follow [C4 5-Minute Quick Start](./c4-architecture-model/README.md#-5-minute-quick-start-why-c4-matters)

## Related Documentation

- **[Software Design Index](../README.md)** - Parent software design documentation
- **[Explanation Documentation Index](../../README.md)** - All conceptual documentation
- **[Repository Governance Architecture](../../../../repo-governance/repository-governance-architecture.md)** - Six-layer governance hierarchy
- **[Functional Programming Principles](../../../../repo-governance/development/pattern/functional-programming.md)** - FP practices in this repository
- **[Diagram Standards](../../../../repo-governance/conventions/formatting/diagrams.md)** - Mermaid and accessibility requirements
- **[Content Quality Standards](../../../../repo-governance/conventions/writing/quality.md)** - Documentation writing guidelines
