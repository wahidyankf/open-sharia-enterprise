---
title: Composition Root Standards
description: OSE Platform standards for composition root design and dependency injection wiring in Spring Boot (Java) and Giraffe (F#) hexagonal architecture implementations
category: explanation
subcategory: architecture
tags:
  - hexagonal-architecture
  - ports-and-adapters
  - composition-root
  - dependency-injection
  - standards
  - organiclever
created: 2026-05-17
---

# Composition Root Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [Port Standards](./port-standards.md) and [Adapter Standards](./adapter-standards.md) before using these standards. The composition root wires ports to adapters — you must understand both before wiring them.

## Purpose

OSE Platform standards for the composition root: the single location in each bounded context where port interfaces are bound to concrete adapter implementations. These conventions apply to both Java/Spring Boot and F#/Giraffe stacks in `organiclever-be`.

## Standard 1: Single Composition Root Per Bounded Context

**REQUIRED**: Each bounded context MUST have exactly one composition root. The composition root is the only place where adapter implementations are selected and wired to port interfaces.

**PROHIBITED**: Wiring adapters to ports in multiple locations (service classes, domain objects, or test fixtures that share production wiring code).

**Rationale**: A single composition root makes the active adapter set visible in one place. Swapping a database adapter for tests or for a technology migration is a one-file change.

## Standard 2: Composition Root Placement — Java/Spring Boot

**REQUIRED**: In Spring Boot, the composition root is implemented as a `@Configuration` class in the `configuration` package of each bounded context.

**Package placement**:

```
com.organicleverbe.
  purchasing.
    configuration.
      PurchasingContextConfiguration.java   ← Composition root
  supplier.
    configuration.
      SupplierContextConfiguration.java
  receiving.
    configuration.
      ReceivingContextConfiguration.java
```

**PROHIBITED**: Scattering `@Bean` declarations across `@Service` classes or `@RestController` classes. All adapter wiring belongs in the `@Configuration` class.

#### `Java`

```java
package com.organicleverbe.purchasing.configuration;

@Configuration
class PurchasingContextConfiguration {

  // Wire output adapter to output port
  @Bean
  PurchaseOrderRepositoryPort purchaseOrderRepositoryPort(
      PurchaseOrderJpaRepository jpaRepository,
      PurchaseOrderMapper mapper) {
    return new JpaPurchaseOrderRepositoryAdapter(jpaRepository, mapper);
  }

  @Bean
  SupplierNotifierPort supplierNotifierPort(KafkaTemplate<String, String> kafka) {
    return new KafkaSupplierNotifierAdapter(kafka);
  }

  // Wire use case (input port implementation) with its output port dependencies
  @Bean
  CreatePurchaseOrderUseCase createPurchaseOrderUseCase(
      PurchaseOrderRepositoryPort repository,
      SupplierNotifierPort notifier) {
    return new CreatePurchaseOrderService(repository, notifier);
  }
}
```

## Standard 3: Composition Root Placement — F#/Giraffe

**REQUIRED**: In F#/Giraffe, the composition root is a dedicated module named `CompositionRoot` in the top-level application namespace. It constructs all adapters and partially applies them into workflow functions.

**Module placement**:

```
OrganicLeverBe/
  CompositionRoot.fs        ← Single composition root for the whole application
  Program.fs                ← Entry point; calls CompositionRoot to build the app
```

For larger applications with multiple bounded contexts in one F# project, the composition root MAY be split per context:

```
OrganicLeverBe/
  Purchasing/
    PurchasingCompositionRoot.fs
  Supplier/
    SupplierCompositionRoot.fs
  CompositionRoot.fs        ← Assembles context roots into the full application
```

#### `F#`

```fsharp
module OrganicLeverBe.CompositionRoot

open OrganicLeverBe.Purchasing.Application.Ports
open OrganicLeverBe.Purchasing.Adapters

let build (config: AppConfig) =
  // Construct output adapters
  let purchaseOrderRepo : PurchaseOrderRepositoryPort =
    PostgresPurchaseOrderRepository.makePort config.ConnectionString

  let supplierNotifier : SupplierNotifierPort =
    KafkaSupplierNotifier.makePort config.KafkaBroker

  // Partially apply adapters into workflow functions (input ports)
  let createPurchaseOrder : CreatePurchaseOrderWorkflow =
    Workflows.createPurchaseOrder purchaseOrderRepo supplierNotifier

  // Return the assembled application
  { CreatePurchaseOrder = createPurchaseOrder }
```

## Standard 4: No New Operators Outside Composition Root — Java

**REQUIRED**: In Java, `new` MUST NOT be used to construct adapters or use-case services outside the `@Configuration` class. Domain value objects and commands are exempt (they are not wired components).

**PROHIBITED**:

```java
// WRONG — adapter constructed inside a service class
@Service
class CreatePurchaseOrderService implements CreatePurchaseOrderUseCase {
  private final PurchaseOrderRepositoryPort repository =
      new JpaPurchaseOrderRepositoryAdapter(...);  // breaks wiring isolation
}
```

**REQUIRED**:

```java
// CORRECT — adapter injected through constructor
@Service
class CreatePurchaseOrderService implements CreatePurchaseOrderUseCase {
  private final PurchaseOrderRepositoryPort repository;

  CreatePurchaseOrderService(PurchaseOrderRepositoryPort repository) {
    this.repository = repository;
  }
}
```

## Standard 5: Test Composition Root

**REQUIRED**: Unit test suites MUST use a separate test composition root that wires in-memory adapters in place of production adapters. The test composition root MUST NOT import any production `@Configuration` class.

**Java — Spring Boot test slice**:

```java
// Test composition root for purchasing context unit tests
@TestConfiguration
class PurchasingTestContextConfiguration {

  @Bean
  PurchaseOrderRepositoryPort purchaseOrderRepositoryPort() {
    return new InMemoryPurchaseOrderRepositoryAdapter();
  }

  @Bean
  SupplierNotifierPort supplierNotifierPort() {
    return new FakeSupplierNotifierAdapter();
  }

  @Bean
  CreatePurchaseOrderUseCase createPurchaseOrderUseCase(
      PurchaseOrderRepositoryPort repository,
      SupplierNotifierPort notifier) {
    return new CreatePurchaseOrderService(repository, notifier);
  }
}
```

**F# — test composition**:

```fsharp
module OrganicLeverBe.Tests.Purchasing.TestCompositionRoot

open OrganicLeverBe.Purchasing.Application.Ports
open OrganicLeverBe.Purchasing.Adapters

let build () =
  let purchaseOrderRepo : PurchaseOrderRepositoryPort =
    InMemoryPurchaseOrderRepository.makePort ()

  let supplierNotifier : SupplierNotifierPort =
    FakeSupplierNotifier.makePort ()

  Workflows.createPurchaseOrder purchaseOrderRepo supplierNotifier
```

**See**: [Testing Standards](./testing-standards.md) for how test composition roots map to `test:unit` and `test:integration` Nx targets.

## Standard 6: Nx Target Awareness

The composition root determines which Nx targets can run without external infrastructure.

| Composition root type  | Nx target          | Infrastructure required    |
| ---------------------- | ------------------ | -------------------------- |
| Production (JPA/Kafka) | `test:integration` | Docker (PostgreSQL, Kafka) |
| Test (InMemory/Fake)   | `test:unit`        | None                       |
| Production startup     | `dev`              | Docker or remote env       |

**REQUIRED**: The `test:unit` target MUST be runnable without Docker. If a test requires a real database, it belongs in `test:integration`, and the test composition root MUST NOT be used for it.

**See**: [Nx Target Standards](../../../../../repo-governance/development/infra/nx-targets.md) for the three-level testing standard and caching rules.

## Rationale

A single, explicit composition root is the architectural guarantee that the hexagon works. It makes the active adapter set auditable (one file to read), swappable (one file to change), and testable (one file to replace for tests). Distributing wiring across service annotations or factory methods silently collapses the boundary — the domain starts depending on concrete adapter classes through Spring's classpath scanning rather than through explicit port injection.

## Related Documentation

- **[Port Standards](./port-standards.md)** — Port contracts assembled in the composition root
- **[Adapter Standards](./adapter-standards.md)** — Adapter implementations assembled in the composition root
- **[Testing Standards](./testing-standards.md)** — Test composition root patterns and Nx target mapping
- **[Hexagonal Architecture Overview](../README.md)** — Dependency direction rule and bounded context overview
- **[Nx Target Standards](../../../../../repo-governance/development/infra/nx-targets.md)** — Canonical target names and caching rules
- **[Hexagonal Architecture FP Tutorial](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-fp-by-example/overview.md)** — Educational foundation: partial application as dependency injection
- **[Hexagonal Architecture OOP Tutorial](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-oop-by-example/overview.md)** — Educational foundation: constructor injection wiring
