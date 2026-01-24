---
title: "FSM Templates - Overview"
description: Production-ready templates for FSM design, implementation, and testing
tags:
  - explanation
  - software
  - architecture
  - finite-state-machine
  - fsm
  - templates
  - readme
  - islamic-finance
last_updated: 2026-01-21
---

# FSM Templates

**Don't start from scratch. Build on proven patterns.**

Production-ready templates for Finite State Machine design, implementation, and testing. Every template includes complete Islamic Finance examples, WCAG-compliant diagrams, and implementation guidance.

## Most Popular Templates

**New to FSM or just want the essentials?** Start here:

1. **[Starter Full Documentation](./ex-so-ar-fsm-te__starter-full-documentation.md)** - Complete FSM package (diagram, spec, tests, deployment) in one template
2. **[Blank State Machine Diagram](./ex-so-ar-fsm-te__blank-state-machine-diagram.md)** - Visual FSM with Mermaid syntax and state tables
3. **[Framework: Spring State Machine](./ex-so-ar-fsm-te__framework-spring-state-machine.md)** - Java/Spring Boot implementation template

**These three templates cover 90% of FSM use cases.**

## Available Templates

### Diagram Templates

1. **[Blank State Machine Diagram](./ex-so-ar-fsm-te__blank-state-machine-diagram.md)**
   - Visual FSM with Mermaid syntax
   - State/transition tables
   - WCAG AA colors
   - Example: Zakat Assessment FSM

2. **[Hierarchical State Machine Diagram](./ex-so-ar-fsm-te__hierarchical-state-machine-diagram.md)**
   - Nested/composite states
   - Parallel regions
   - History states
   - Example: Islamic Contract Approval with nested states

### Documentation Templates

1. **[State Transition Table](./ex-so-ar-fsm-te__state-transition-table.md)**
   - Tabular transition documentation
   - Guard/action specifications
   - Invalid transitions
   - Example: Qard Hasan (Interest-Free Loan) FSM

2. **[State Machine Specification](./ex-so-ar-fsm-te__state-machine-specification.md)**
   - Formal FSM specification
   - Implementation contract
   - Business rules
   - Example: Donation Campaign FSM

3. **[FSM Testing Strategy](./ex-so-ar-fsm-te__testing-strategy.md)**
   - State/transition coverage
   - Guard/action testing
   - Integration tests
   - Example: Islamic Financial Contract FSM

4. **[Implementation Checklist](./ex-so-ar-fsm-te__implementation-checklist.md)**
   - 7-phase implementation tracking
   - Design validation
   - Paradigm-specific checklists
   - Example: Zakat Calculation Workflow

5. **[Starter Full Documentation](./ex-so-ar-fsm-te__starter-full-documentation.md)**
   - All-in-one FSM package
   - Diagram + Spec + Tests + Deployment
   - Single-source documentation
   - Example: Murabaha Contract FSM

### Implementation Templates

1. **[Framework: Spring State Machine](./ex-so-ar-fsm-te__framework-spring-state-machine.md)**
   - Java/Spring Boot integration
   - Configuration, guards, actions
   - JPA persistence
   - Example: Zakat Distribution FSM

2. **[Framework: XState](./ex-so-ar-fsm-te__framework-xstate.md)**
   - TypeScript/JavaScript + React
   - Machine definition
   - React hooks integration
   - Example: Donation Campaign FSM

3. **[Event-Driven: Saga Pattern](./ex-so-ar-fsm-te__event-driven-saga-pattern.md)**
   - Distributed FSM with sagas
   - Orchestration/choreography
   - Compensation logic
   - Example: Islamic Contract Approval Saga

## Quick Start

### 1. Choose Your Starting Point

**New FSM Project**:
→ Start with [Starter Full Documentation](./ex-so-ar-fsm-te__starter-full-documentation.md)

**Design-First Approach**:
→ [Blank State Machine Diagram](./ex-so-ar-fsm-te__blank-state-machine-diagram.md) + [State Machine Specification](./ex-so-ar-fsm-te__state-machine-specification.md)

**Code-First Approach**:
→ Framework template ([Spring SSM](./ex-so-ar-fsm-te__framework-spring-state-machine.md) or [XState](./ex-so-ar-fsm-te__framework-xstate.md))

### 2. Select Templates by Need

| Need                    | Templates                        |
| ----------------------- | -------------------------------- |
| Visual design           | Blank/Hierarchical Diagram       |
| Implementation contract | Specification + Transition Table |
| Testing plan            | Testing Strategy                 |
| Project tracking        | Implementation Checklist         |
| Java/Spring Boot        | Spring State Machine             |
| TypeScript/React        | XState                           |
| Distributed system      | Saga Pattern                     |

### 3. Customize for Your Domain

All templates include OSE domain examples (Zakat, Contracts, Campaigns, Loans). Replace placeholders with your specific domain entities.

## Template Selection Matrix

| FSM Complexity | States | Recommended Templates            |
| -------------- | ------ | -------------------------------- |
| Simple         | 3-5    | Blank Diagram + Specification    |
| Medium         | 6-10   | Starter Full Documentation       |
| Complex        | 11+    | Hierarchical Diagram + Checklist |
| Distributed    | Any    | Saga Pattern + Specification     |

| Paradigm         | Framework            | Template              |
| ---------------- | -------------------- | --------------------- |
| OOP (Java)       | Spring State Machine | Spring SSM Template   |
| OOP (TypeScript) | XState               | XState Template       |
| Event-Driven     | Saga/Temporal        | Saga Pattern Template |
| Custom           | Hand-rolled          | Blank Diagram + Spec  |

## Usage Workflow

### Typical FSM Development Flow

1. **Requirements** (Week 1)
   - Use [Implementation Checklist](./ex-so-ar-fsm-te__implementation-checklist.md) Phase 1
   - Document business requirements

2. **Design** (Week 1-2)
   - Create diagram: [Blank](./ex-so-ar-fsm-te__blank-state-machine-diagram.md) or [Hierarchical](./ex-so-ar-fsm-te__hierarchical-state-machine-diagram.md)
   - Write [Specification](./ex-so-ar-fsm-te__state-machine-specification.md)
   - Fill [Transition Table](./ex-so-ar-fsm-te__state-transition-table.md)

3. **Implementation** (Week 2-3)
   - Follow paradigm-specific template ([Spring SSM](./ex-so-ar-fsm-te__framework-spring-state-machine.md) or [XState](./ex-so-ar-fsm-te__framework-xstate.md))
   - Track progress with [Checklist](./ex-so-ar-fsm-te__implementation-checklist.md)

4. **Testing** (Week 3-4)
   - Execute [Testing Strategy](./ex-so-ar-fsm-te__testing-strategy.md)
   - Verify coverage targets

5. **Deployment** (Week 4)
   - Complete [Checklist](./ex-so-ar-fsm-te__implementation-checklist.md) Phase 6
   - Deploy to production

## OSE Domain Examples

All templates use Open Sharia Enterprise domain examples:

- **Zakat**: Assessment lifecycle, distribution workflow
- **Contracts**: Islamic contracts (Murabaha, Ijara, Mudarabah)
- **Campaigns**: Donation fundraising campaigns
- **Loans**: Qard Hasan (interest-free loans)

These examples demonstrate FSM patterns in Islamic finance context while remaining applicable to any domain.

## Template Standards

All templates follow OSE quality standards:

- **Naming**: UPPER_SNAKE_CASE for states/events
- **Colors**: WCAG AA compliant (color-blind friendly)
- **Examples**: Complete filled examples, not partial
- **Placeholders**: [BRACKETS] for replaceable content
- **No estimates**: No time/size estimates

## Integration with FSM Documentation

These templates complement the main FSM documentation:

- **Theory**: [FSM Types](../ex-so-ar-fsm__03-fsm-types-and-classifications.md), [Hierarchical States](../ex-so-ar-fsm__05-hierarchical-and-nested-states.md)
- **Design**: [State Explosion](../ex-so-ar-fsm__04-state-explosion-and-mitigation.md), [Design Patterns](../ex-so-ar-fsm__07-design-patterns-and-best-practices.md)
- **Implementation**: [OOP Patterns](../ex-so-ar-fsm__08-oop-implementation-patterns.md), [FP Patterns](../ex-so-ar-fsm__09-fp-implementation-patterns.md)
- **Frameworks**: [Spring SSM & XState](../ex-so-ar-fsm__13-framework-spring-state-machine-xstate.md), [Temporal/Cadence](../ex-so-ar-fsm__14-framework-statecharts-temporal-cadence.md)
- **Guidance**: [Decision Trees](../ex-so-ar-fsm__16-decision-trees-and-guidelines.md), [Best Practices](../ex-so-ar-fsm__17-best-practices.md)

## Getting Help

- **Conceptual questions**: See main [FSM README](../README.md)
- **Design decisions**: [Decision Trees](../ex-so-ar-fsm__16-decision-trees-and-guidelines.md)
- **Common mistakes**: [Best Practices](../ex-so-ar-fsm__17-best-practices.md)
- **Framework selection**: [Framework comparison](../ex-so-ar-fsm__13-framework-spring-state-machine-xstate.md)

## Contributing

When adding new templates:

1. Follow existing template structure
2. Include complete OSE domain example
3. Provide usage instructions
4. Update this README
5. Cross-reference with main FSM docs

## Summary

**10 production-ready templates** covering:

- **Diagrams** (2): Flat + Hierarchical visualizations
- **Documentation** (5): Specifications, tables, testing, checklists, starter pack
- **Implementation** (3): Spring SSM, XState, Saga pattern

**Start here**:

- New project → [Starter Full Documentation](./ex-so-ar-fsm-te__starter-full-documentation.md)
- Java → [Spring State Machine](./ex-so-ar-fsm-te__framework-spring-state-machine.md)
- TypeScript → [XState](./ex-so-ar-fsm-te__framework-xstate.md)
- Distributed → [Saga Pattern](./ex-so-ar-fsm-te__event-driven-saga-pattern.md)
