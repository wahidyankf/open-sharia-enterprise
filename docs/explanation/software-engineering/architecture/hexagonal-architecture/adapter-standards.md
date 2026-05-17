---
title: Adapter Standards
description: OSE Platform standards for adapter (implementation) design, naming, package placement, and forbidden import rules across the polyglot stack
category: explanation
subcategory: architecture
tags:
  - hexagonal-architecture
  - ports-and-adapters
  - adapters
  - standards
  - organiclever
created: 2026-05-17
---

# Adapter Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [Hexagonal Architecture Overview](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/overview.md) and [Port Standards](./port-standards.md) before using these standards. Adapters implement ports — you must understand port contracts first.

## Purpose

OSE Platform standards for implementing, naming, and placing adapters across the Java/Spring Boot and F#/Giraffe stacks. Adapters are the only layer permitted to import infrastructure libraries, framework APIs, and external clients.

## Standard 1: Adapter Naming — Java

**REQUIRED**: Java adapter class names MUST identify both the technology and the port they implement.

| Adapter kind   | Naming pattern              | Example                                  |
| -------------- | --------------------------- | ---------------------------------------- |
| Persistence    | `[Technology][Port]Adapter` | `JpaPurchaseOrderRepositoryAdapter`      |
| Messaging      | `[Technology][Port]Adapter` | `KafkaSupplierNotifierAdapter`           |
| HTTP client    | `[Technology][Port]Adapter` | `RestApprovalRouterAdapter`              |
| In-memory test | `InMemory[Port]Adapter`     | `InMemoryPurchaseOrderRepositoryAdapter` |
| Fake/stub test | `Fake[Port]Adapter`         | `FakeSupplierNotifierAdapter`            |

**Package placement**:

```
com.organicleverbe.
  purchasing.
    adapter.
      in.
        web.                        ← HTTP input adapters (Spring MVC controllers)
          PurchaseOrderController.java
      out.
        persistence.                ← JPA / JDBC output adapters
          JpaPurchaseOrderRepositoryAdapter.java
          PurchaseOrderJpaEntity.java
          PurchaseOrderJpaRepository.java
        messaging.                  ← Kafka / AMQP output adapters
          KafkaSupplierNotifierAdapter.java
```

#### `Java`

```java
// Output adapter — persistence
package com.organicleverbe.purchasing.adapter.out.persistence;

@Repository
class JpaPurchaseOrderRepositoryAdapter implements PurchaseOrderRepositoryPort {

  private final PurchaseOrderJpaRepository jpaRepository;
  private final PurchaseOrderMapper mapper;

  JpaPurchaseOrderRepositoryAdapter(
      PurchaseOrderJpaRepository jpaRepository,
      PurchaseOrderMapper mapper) {
    this.jpaRepository = jpaRepository;
    this.mapper = mapper;
  }

  @Override
  public void save(PurchaseOrder order) {
    jpaRepository.save(mapper.toEntity(order));
  }

  @Override
  public Optional<PurchaseOrder> findById(PurchaseOrderId id) {
    return jpaRepository.findById(id.value()).map(mapper::toDomain);
  }
}
```

## Standard 2: Adapter Naming — F

**REQUIRED**: F# adapter modules MUST be placed in the `Adapters` namespace and named by technology.

**Module placement**:

```
OrganicLeverBe/
  Purchasing/
    Adapters/
      PostgresPurchaseOrderRepository.fs   ← Output adapter: PostgreSQL
      HttpPurchaseOrderController.fs       ← Input adapter: HTTP handler
      InMemoryPurchaseOrderRepository.fs   ← Test adapter
```

#### `F#`

```fsharp
// Output adapter — PostgreSQL
module OrganicLeverBe.Purchasing.Adapters.PostgresPurchaseOrderRepository

open OrganicLeverBe.Purchasing.Application.Ports
open OrganicLeverBe.Purchasing.Domain
open Npgsql.FSharp

let makePort (connectionString: string) : PurchaseOrderRepositoryPort = {
  save = fun order -> async {
    // SQL via Npgsql.FSharp
    return Ok ()
  }
  findById = fun id -> async {
    // SQL via Npgsql.FSharp
    return Ok None
  }
}
```

## Standard 3: Forbidden Imports in Domain and Application Layers

**REQUIRED**: The following import categories are PROHIBITED in domain and application layer packages.

| Import category             | Prohibited in     | Reason                                   |
| --------------------------- | ----------------- | ---------------------------------------- |
| JPA / Hibernate             | Domain, App layer | Infrastructure leaks into business logic |
| Spring `@Autowired`         | Domain layer      | Framework coupling in pure domain        |
| `javax.persistence.*`       | Domain, App layer | Persistence annotation in domain         |
| `org.springframework.web.*` | Domain layer      | HTTP framework in domain                 |
| Npgsql / EF Core            | Domain, App layer | Database driver in business logic        |
| `System.Net.Http`           | Domain layer      | HTTP client in domain                    |

**REQUIRED**: Code review MUST reject any import from the above categories found outside the `adapter` package.

**Rationale**: A single leaking import collapses the hexagonal boundary. The domain and application layers must remain framework-free so they can be tested without starting any infrastructure.

## Standard 4: Input Adapter Responsibilities

**REQUIRED**: Input adapters (HTTP controllers, CLI handlers, message consumers) MUST:

1. Translate the external representation (HTTP request body, CLI args, message payload) into a domain command or query object
2. Call the input port (use case / workflow)
3. Translate the result back to the external representation (HTTP response, exit code, acknowledgement)

**PROHIBITED** in input adapters:

- Business logic or domain rule evaluation
- Direct calls to output ports (bypassing the application layer)
- Domain object construction beyond mapping

#### `Java`

```java
// Input adapter — Spring MVC controller
package com.organicleverbe.purchasing.adapter.in.web;

@RestController
@RequestMapping("/api/purchase-orders")
class PurchaseOrderController {

  private final CreatePurchaseOrderUseCase createUseCase;

  PurchaseOrderController(CreatePurchaseOrderUseCase createUseCase) {
    this.createUseCase = createUseCase;
  }

  @PostMapping
  ResponseEntity<PurchaseOrderResponse> create(@RequestBody @Valid CreatePurchaseOrderRequest req) {
    // Map request → command (no business logic)
    var command = new CreatePurchaseOrderCommand(req.supplierId(), req.lineItems());
    // Delegate to input port
    var id = createUseCase.create(command);
    // Map result → response (no business logic)
    return ResponseEntity.status(HttpStatus.CREATED).body(new PurchaseOrderResponse(id.value()));
  }
}
```

## Standard 5: Output Adapter Responsibilities

**REQUIRED**: Output adapters MUST:

1. Implement exactly the output port interface they satisfy
2. Map domain objects to the infrastructure representation (JPA entity, SQL row, HTTP payload)
3. Map infrastructure responses back to domain objects or Result types

**PROHIBITED** in output adapters:

- Business logic or invariant enforcement
- Calling other output adapters directly
- Owning or modifying domain state

## Standard 6: In-Memory Adapters

**REQUIRED**: Every output port used in unit tests MUST have a corresponding in-memory adapter in the test source set.

**Package placement** (Java):

```
src/test/java/com/organicleverbe/purchasing/adapter/out/
  InMemoryPurchaseOrderRepositoryAdapter.java
  FakeSupplierNotifierAdapter.java
```

**Package placement** (F#):

```
OrganicLeverBe.Tests/
  Purchasing/
    Adapters/
      InMemoryPurchaseOrderRepository.fs
```

In-memory adapters MUST implement the full port contract and be the only persistence mechanism used in `test:unit` targets. They MUST NOT be deployed to any environment.

**See**: [Testing Standards](./testing-standards.md) for how in-memory adapters map to Nx test targets.

## Standard 7: Mapper Placement

**REQUIRED**: Mapping logic between domain objects and infrastructure representations MUST live in the adapter package — never in the domain or application layer.

#### `Java`

```java
// Mapper lives alongside the adapter, not in domain/application
package com.organicleverbe.purchasing.adapter.out.persistence;

@Component
class PurchaseOrderMapper {
  PurchaseOrderJpaEntity toEntity(PurchaseOrder domain) { /* ... */ }
  PurchaseOrder toDomain(PurchaseOrderJpaEntity entity) { /* ... */ }
}
```

## Rationale

Adapter naming by technology (`Jpa`, `Kafka`, `Postgres`, `InMemory`) makes the active infrastructure visible at the class/module level without reading implementation internals. The forbidden-import rule enforces the dependency direction mechanically — static analysis tools can check it on every build. Keeping mappers in the adapter package ensures domain objects never carry persistence annotations.

## Related Documentation

- **[Port Standards](./port-standards.md)** — Port contracts that adapters implement
- **[Composition Root Standards](./composition-root-standards.md)** — Where adapters are instantiated and wired
- **[Testing Standards](./testing-standards.md)** — In-memory adapter swap and Nx target mapping
- **[Hexagonal Architecture Overview](../README.md)** — Dependency direction rule and bounded context overview
- **[DDD Domain Event Standards](../domain-driven-design-ddd/domain-event-standards.md)** — Domain events emitted through output port adapters
- **[Hexagonal Architecture FP Tutorial](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-fp-by-example/overview.md)** — Educational foundation for F# adapter record literals
- **[Hexagonal Architecture OOP Tutorial](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/in-oop-by-example/overview.md)** — Educational foundation for Java adapter classes
