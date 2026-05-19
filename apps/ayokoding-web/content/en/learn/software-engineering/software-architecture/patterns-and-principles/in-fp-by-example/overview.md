---
title: "Overview"
weight: 10000000
date: 2026-05-17T00:00:00+07:00
draft: false
description: "FP variant — learn software architecture through 93 heavily annotated examples (85 canonical + 5 FP-native extras + 3 OOP-native stubs for parity) in F# (canonical), Clojure, TypeScript, and Haskell, covering 95% of essential concepts (ideal for experienced developers)"
tags:
  [
    "software-architecture",
    "tutorial",
    "by-example",
    "code-first",
    "design-patterns",
    "fp",
    "fsharp",
    "clojure",
    "typescript",
    "haskell",
  ]
---

This section provides a code-first approach to learning software architecture through heavily annotated examples in four functional languages: F# (canonical), Clojure, TypeScript, and Haskell. Each example presents all four languages as parallel tabs — F# carries the deepest annotations and the framing prose, while Clojure, TypeScript, and Haskell are first-class variants showing the same architectural decision in their respective idioms. Each example also mirrors its counterpart in the OOP variant (same number, same conceptual title) so the two paradigms can be compared side-by-side.

## What You Will Learn

The examples in this section cover patterns, principles, architectural styles, trade-offs, and real-world architectural decisions across three progressive levels, expressed in idiomatic F#, Clojure, TypeScript, and Haskell:

- **Beginner**: Foundational architectural concepts with simple, self-contained examples in all four languages
- **Intermediate**: Composite patterns and common enterprise architecture challenges
- **Advanced**: Complex systems, distributed architecture, and nuanced trade-off analysis

## How to Use This Section

Each example is self-contained and annotated to explain not just what the code does, but why each architectural decision was made. The F# tab runs under `dotnet fsi`; the Clojure tab runs as a standalone namespace; the TypeScript tab runs under `ts-node` or `deno`; the Haskell tab runs under `runghc` or `cabal run`. Start at the level that matches your current understanding and progress through the examples in order. Each FP example shares the same number as its OOP counterpart in the [in-oop-by-example](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-oop-by-example) sibling tutorial, enabling cross-paradigm comparison.

## Paradigm-Fit Legend

Not every architectural pattern fits every paradigm equally. Many examples carry a **Paradigm Note** banner explaining whether the pattern is FP-native, OOP-native, or paradigm-neutral. The classification follows authoritative sources:

- **NEUTRAL** — paradigm-agnostic concept (microservices, distributed tracing, hexagonal architecture). Both tracks teach legitimately.
- **OOP-NATIVE** — pattern emerged from OOP and is absorbed by FP language features. Norvig (1996, [Design Patterns in Dynamic Languages](https://norvig.com/design-patterns/)) classified 16 of 23 GoF patterns this way. Examples in the FP track show the _native FP idiom_ (HOF, ADT, fold, FRP) rather than reproducing the OOP shape.
- **OOP-NATIVE-BUT-TRANSFERABLE** — OOP roots but the concept transfers cleanly (SOLID, DDD aggregates, Repository). The paradigm note explains the FP encoding.
- **FP-NATIVE** — pattern emerged from or expresses most naturally in FP (Railway-Oriented Programming, Free Monads, Reader/State monads, Event Sourcing fold, FRP, Kleisli composition). Examples 86–90 are FP-native extras; the OOP track carries stubs pointing here.

Authority basis: Norvig 1996; Seemann ([Design patterns across paradigms](https://blog.ploeh.dk/2012/05/25/Designpatternsacrossparadigms/), 2012; [SOLID: the next step is Functional](https://blog.ploeh.dk/2014/03/10/solid-the-next-step-is-functional/), 2014); Wlaschin ([Domain Modeling Made Functional](https://pragprog.com/titles/swdddf/domain-modeling-made-functional/)); Hickey ([Simple Made Easy](https://www.infoq.com/presentations/Simple-Made-Easy/)); Evans (_Domain-Driven Design_); Fowler ([PEAA](https://martinfowler.com/eaaCatalog/)).

## Structure of Each Example

Every example follows a consistent five-part format:

1. **Brief Explanation** — what the pattern or principle addresses and why it matters (2-3 sentences)
2. **Mermaid Diagram** — visual representation of component relationships, pipelines, or data flow (when appropriate)
3. **Heavily Annotated Code** — parallel tabs showing F# (canonical), Clojure, TypeScript, and Haskell, each with `// =>` (or `;;` in Clojure, `-- =>` in Haskell) comments documenting architectural decisions and trade-offs
4. **Key Takeaway** — the core insight to retain from the example (1-2 sentences)
5. **Why It Matters** — production relevance and real-world impact (50-100 words)

## Examples by Level

### Beginner (Examples 1–28)

- [Example 1: No Separation vs. Clear Separation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-1-no-separation-vs-clear-separation)
- [Example 2: Single Responsibility Principle](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-2-single-responsibility-principle)
- [Example 3: Three-Layer Architecture](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-3-three-layer-architecture)
- [Example 4: Presentation Layer Isolation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-4-presentation-layer-isolation)
- [Example 5: Model-View-Controller Basics](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-5-model-view-controller-basics)
- [Example 6: Model Encapsulates Validation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-6-model-encapsulates-validation)
- [Example 7: Manual Dependency Injection](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-7-manual-dependency-injection)
- [Example 8: Constructor Injection vs. Method Injection](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-8-constructor-injection-vs-method-injection)
- [Example 9: Interface Segregation Principle](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-9-interface-segregation-principle)
- [Example 10: Open for Extension, Closed for Modification](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-10-open-for-extension-closed-for-modification)
- [Example 11: Subtypes Must Be Substitutable](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-11-subtypes-must-be-substitutable)
- [Example 12: DRY — Don't Repeat Yourself](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-12-dry--dont-repeat-yourself)
- [Example 13: KISS — Keep It Simple, Stupid](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-13-kiss--keep-it-simple-stupid)
- [Example 14: YAGNI — You Aren't Gonna Need It](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-14-yagni--you-arent-gonna-need-it)
- [Example 15: High Coupling — The Problem](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-15-high-coupling--the-problem)
- [Example 16: Low Coupling Through Encapsulation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-16-low-coupling-through-encapsulation)
- [Example 17: Cohesion — Grouping Related Behavior](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-17-cohesion--grouping-related-behavior)
- [Example 18: Encapsulation with Private State](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-18-encapsulation-with-private-state)
- [Example 19: Preferring Composition](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-19-preferring-composition)
- [Example 20: Mixin vs. Composition](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-20-mixin-vs-composition)
- [Example 21: Repository Pattern Basics](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-21-repository-pattern-basics)
- [Example 22: Repository with Query Methods](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-22-repository-with-query-methods)
- [Example 23: Service Layer Coordinates Use Cases](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-23-service-layer-coordinates-use-cases)
- [Example 24: Service Layer with Error Handling](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-24-service-layer-with-error-handling)
- [Example 25: Data Transfer Objects](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-25-data-transfer-objects)
- [Example 26: DTO Validation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-26-dto-validation)
- [Example 27: Small Layered Application](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-27-small-layered-application)
- [Example 28: Recognizing Architecture Smells](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/beginner#example-28-recognizing-architecture-smells)

### Intermediate (Examples 29–57)

- [Example 29: Hexagonal Architecture — Ports and Adapters](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-29-hexagonal-architecture--ports-and-adapters)
- [Example 30: Clean Architecture — Layer Separation with Dependency Rule](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-30-clean-architecture--layer-separation-with-dependency-rule)
- [Example 31: Onion Architecture — Domain at the Center](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-31-onion-architecture--domain-at-the-center)
- [Example 32: Observer Pattern — Event Notification Without Coupling](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-32-observer-pattern--event-notification-without-coupling)
- [Example 33: Domain Events — Signaling State Changes Within a Bounded Context](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-33-domain-events--signaling-state-changes-within-a-bounded-context)
- [Example 34: Event-Driven Architecture — Async Message Passing Between Services](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-34-event-driven-architecture--async-message-passing-between-services)
- [Example 35: Strategy Pattern — Swappable Algorithms](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-35-strategy-pattern--swappable-algorithms)
- [Example 36: Factory Pattern — Centralized Object Creation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-36-factory-pattern--centralized-object-creation)
- [Example 37: Builder Pattern — Constructing Complex Objects Step by Step](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-37-builder-pattern--constructing-complex-objects-step-by-step)
- [Example 38: Adapter Pattern — Bridging Incompatible Interfaces](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-38-adapter-pattern--bridging-incompatible-interfaces)
- [Example 39: Decorator Pattern — Adding Behavior Without Subclassing](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-39-decorator-pattern--adding-behavior-without-subclassing)
- [Example 40: Facade Pattern — Simplified Interface to a Subsystem](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-40-facade-pattern--simplified-interface-to-a-subsystem)
- [Example 41: Command Pattern — Encapsulate Actions as Objects](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-41-command-pattern--encapsulate-actions-as-objects)
- [Example 42: Mediator Pattern — Centralized Component Coordination](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-42-mediator-pattern--centralized-component-coordination)
- [Example 43: State Pattern — Objects That Change Behavior Based on State](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-43-state-pattern--objects-that-change-behavior-based-on-state)
- [Example 44: HOF with Hole-Filling Steps](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-44-hof-with-hole-filling-steps)
- [Example 45: Value Objects — Immutable Domain Concepts Without Identity](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-45-value-objects--immutable-domain-concepts-without-identity)
- [Example 46: Aggregate Roots — Consistency Boundaries in DDD](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-46-aggregate-roots--consistency-boundaries-in-ddd)
- [Example 47: Bounded Contexts — Separating Domain Models by Responsibility](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-47-bounded-contexts--separating-domain-models-by-responsibility)
- [Example 48: Anti-Corruption Layer — Protecting the Domain from External Models](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-48-anti-corruption-layer--protecting-the-domain-from-external-models)
- [Example 49: CQRS Pattern — Separate Read and Write Models](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-49-cqrs-pattern--separate-read-and-write-models)
- [Example 50: Middleware Pattern — Processing Pipeline for Cross-Cutting Concerns](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-50-middleware-pattern--processing-pipeline-for-cross-cutting-concerns)
- [Example 51: Plugin Architecture — Extending Systems Without Modifying Core](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-51-plugin-architecture--extending-systems-without-modifying-core)
- [Example 52: Repository Pattern — Abstracting Data Access](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-52-repository-pattern--abstracting-data-access)
- [Example 53: Unit of Work Pattern — Grouping Operations into Atomic Transactions](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-53-unit-of-work-pattern--grouping-operations-into-atomic-transactions)
- [Example 54: Specification Pattern — Composable Business Rules](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-54-specification-pattern--composable-business-rules)
- [Example 55: CQRS with Event Sourcing — State as a Sequence of Events](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-55-cqrs-with-event-sourcing--state-as-a-sequence-of-events)
- [Example 56: Saga Pattern — Managing Distributed Transactions](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-56-saga-pattern--managing-distributed-transactions)
- [Example 57: Circuit Breaker Pattern — Preventing Cascade Failures](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/intermediate#example-57-circuit-breaker-pattern--preventing-cascade-failures)

### Advanced (Examples 58–85)

- [Example 58: Microservices Decomposition by Business Capability](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-58-microservices-decomposition-by-business-capability)
- [Example 59: Strangler Fig Pattern](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-59-strangler-fig-pattern)
- [Example 60: Saga Orchestration](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-60-saga-orchestration)
- [Example 61: Saga Choreography](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-61-saga-choreography)
- [Example 62: API Versioning Strategies](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-62-api-versioning-strategies)
- [Example 63: Backend for Frontend (BFF) Pattern](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-63-backend-for-frontend-bff-pattern)
- [Example 64: Circuit Breaker with Fallback](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-64-circuit-breaker-with-fallback)
- [Example 65: Bulkhead Pattern](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-65-bulkhead-pattern)
- [Example 66: Retry with Exponential Backoff and Jitter](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-66-retry-with-exponential-backoff-and-jitter)
- [Example 67: Distributed Tracing Architecture](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-67-distributed-tracing-architecture)
- [Example 68: Sidecar Pattern](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-68-sidecar-pattern)
- [Example 69: Ambassador Pattern](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-69-ambassador-pattern)
- [Example 70: Event Sourcing Implementation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-70-event-sourcing-implementation)
- [Example 71: Modular Monolith](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-71-modular-monolith)
- [Example 72: Vertical Slice Architecture](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-72-vertical-slice-architecture)
- [Example 73: Shared Kernel](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-73-shared-kernel)
- [Example 74: Specification Pattern](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-74-specification-pattern)
- [Example 75: Chain of Responsibility](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-75-chain-of-responsibility)
- [Example 76: Visitor Pattern in Architecture](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-76-visitor-pattern-in-architecture)
- [Example 77: Database per Service Pattern](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-77-database-per-service-pattern)
- [Example 78: Feature Toggle Architecture](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-78-feature-toggle-architecture)
- [Example 79: Service Mesh Architecture](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-79-service-mesh-architecture)
- [Example 80: Interpreter Pattern for Configuration DSL](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-80-interpreter-pattern-for-configuration-dsl)
- [Example 81: CQRS (Command Query Responsibility Segregation)](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-81-cqrs-command-query-responsibility-segregation)
- [Example 82: Outbox Pattern for Reliable Event Publishing](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-82-outbox-pattern-for-reliable-event-publishing)
- [Example 83: Anti-Corruption Layer](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-83-anti-corruption-layer)
- [Example 84: Ports and Adapters (Hexagonal Architecture)](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-84-ports-and-adapters-hexagonal-architecture)
- [Example 85: Reactive Architecture with Backpressure](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-85-reactive-architecture-with-backpressure)

### FP-Native Extras (Examples 86–90)

Patterns that have no natural OOP counterpart — they exist in FP because the paradigm makes them ergonomic.

- [Example 86: Railway-Oriented Programming (Result/Either Chains)](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-86-railway-oriented-programming-resulteither-chains)
- [Example 87: Free Monads / Tagless Final (Embedded DSLs)](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-87-free-monads--tagless-final-embedded-dsls)
- [Example 88: Reader Monad for Dependency Injection](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-88-reader-monad-for-dependency-injection)
- [Example 89: Kleisli Composition for Effectful Pipelines](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-89-kleisli-composition-for-effectful-pipelines)
- [Example 90: State Monad for Pure Stateful Computation](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-90-state-monad-for-pure-stateful-computation)

### OOP-Native Stubs (Examples 91–93)

Numbering parity with the OOP track; full treatment lives there.

- [Example 91: Active Record (OOP-Native)](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-91-active-record-oop-native)
- [Example 92: GRASP Responsibility Assignment (OOP-Native)](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-92-grasp-responsibility-assignment-oop-native)
- [Example 93: Singleton with FP Counterexample](/en/learn/software-engineering/software-architecture/patterns-and-principles/in-fp-by-example/advanced#example-93-singleton-with-fp-counterexample)
