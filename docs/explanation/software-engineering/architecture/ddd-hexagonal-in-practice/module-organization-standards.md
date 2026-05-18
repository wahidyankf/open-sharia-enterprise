---
title: Module Organization Standards
description: OSE Platform standards for Java/Spring Boot package structure and F# module hierarchy in combined DDD+Hexagonal projects — package naming, layer ordering, ubiquitous language encoding, and OrganicLever module catalog
category: explanation
subcategory: architecture
tags:
  - ddd
  - hexagonal-architecture
  - package-structure
  - module-organization
  - standards
  - organiclever
created: 2026-05-17
---

# Module Organization Standards

OSE Platform standards for organizing Java/Spring Boot packages and F# modules in combined DDD+Hexagonal projects. Package names encode bounded context identity, ubiquitous language, and architectural layer — making the codebase navigable without a diagram.

## Prerequisite Knowledge

**REQUIRED**: Complete [Bounded Context Mapping Standards](./bounded-context-mapping-standards.md), [Adapter Standards](../hexagonal-architecture/adapter-standards.md), and [Composition Root Standards](../hexagonal-architecture/composition-root-standards.md) before applying these standards.

## Standard 1: Root Package Encodes the Bounded Context

**REQUIRED**: Every class, interface, and module MUST live under a root package that identifies both the application and the bounded context.

**Java root package pattern**: `com.organicleverbe.[context]`

**F# root namespace pattern**: `OrganicLeverBe.[Context]`

**PROHIBITED**: A flat root package with no context segment:

```
// WRONG — context identity invisible
com.organicleverbe.PurchaseOrderService
com.organicleverbe.SupplierRepository
```

**REQUIRED**: Context identity in the second segment:

```
// CORRECT
com.organicleverbe.purchasing.application.service.ApprovePurchaseOrderService
com.organicleverbe.supplier.domain.model.Supplier
```

**Rationale**: When the bounded context is visible in every fully-qualified name, accidental cross-context imports produce an obvious code smell. A class in `com.organicleverbe.receiving` importing from `com.organicleverbe.purchasing.domain` is immediately visible in the import block — a signal for code review.

## Standard 2: Layer Ordering Within Each Context Package

**REQUIRED**: Each bounded context package MUST follow this four-layer ordering. The layer name is always the third segment after the context root.

```
com.organicleverbe.[context].
  domain/          ← Domain core: aggregates, value objects, domain services, domain events
  application/     ← Application layer: use case implementations, ports (in/ and out/)
  adapter/         ← Adapter layer: in/ (driving) and out/ (driven)
  configuration/   ← Composition root: @Configuration classes wiring ports to adapters
```

**F# module ordering** within `OrganicLeverBe.[Context]`:

```
OrganicLeverBe.[Context]/
  Domain.fs          ← Aggregates, value objects, domain functions, domain events
  Application/
    Ports.fs         ← All input and output port type aliases
    [UseCase].fs     ← One file per use case workflow implementation
  Adapters/
    In/
      HttpHandler.fs ← HTTP input adapter (Giraffe route handlers)
    Out/
      [Technology][Resource]Adapter.fs
  CompositionRoot.fs ← Wires ports to adapters, registers with Giraffe app
```

**PROHIBITED**: Skipping the layer segment or merging layers:

```
// WRONG — no layer visibility
com.organicleverbe.purchasing.PurchaseOrderService   // is this domain or application?
com.organicleverbe.purchasing.PurchaseOrderRepo      // is this a port or an adapter?
```

**See**: [Composition Root Standards](../hexagonal-architecture/composition-root-standards.md) for the `configuration/` package wiring rules.

## Standard 3: Domain Package Sub-structure

**REQUIRED**: The `domain` package MUST be subdivided by DDD tactical pattern, not by aggregate name.

#### `Java`

```
com.organicleverbe.purchasing.domain.
  model/            ← Aggregate roots and their child entities
    PurchaseRequisition.java
    PurchaseOrder.java
    PurchaseOrderItem.java   ← Child entity (not an aggregate root)
  valueobject/      ← Value objects
    PurchaseOrderId.java
    LineItemQuantity.java
  event/            ← Domain events
    PurchaseOrderApproved.java
    PurchaseOrderIssued.java
  service/          ← Domain services (stateless, multi-aggregate coordination)
    PurchaseOrderPricingService.java
```

**PROHIBITED**: Placing domain events in the `application` layer or placing application services in the `domain` layer.

**PROHIBITED**: Naming the aggregate package after the aggregate root when multiple aggregates share the context (`model/` is preferred over `purchaseorder/` + `purchaserequisition/` sibling packages at the same level as `domain`).

#### `F#`

In F#, domain types are defined in a single `Domain.fs` file per context, ordered so that types are declared before their dependents (F# requires top-to-bottom declaration order):

```fsharp
// OrganicLeverBe/Purchasing/Domain.fs
module OrganicLeverBe.Purchasing.Domain

// 1. Value objects first
type PurchaseOrderId = PurchaseOrderId of System.Guid
type LineItemQuantity = private LineItemQuantity of int

// 2. Aggregate roots and child entities
type PurchaseOrderItem = { ... }
type PurchaseOrder = { ... }

// 3. Domain events
type PurchaseOrderApproved = { OrderId: PurchaseOrderId; ApprovedBy: ApproverId; OccurredAt: System.DateTimeOffset }

// 4. Domain functions (pure)
let approve (approver: ApproverId) (order: PurchaseOrder) : Result<PurchaseOrder, DomainError> = ...
```

## Standard 4: Application Layer Sub-structure

**REQUIRED**: The `application` package MUST separate ports from use case implementations.

#### `Java`

```
com.organicleverbe.purchasing.application.
  port.
    in/             ← Input port interfaces (driving)
      CreatePurchaseOrderUseCase.java
      ApprovePurchaseOrderUseCase.java
    out/            ← Output port interfaces (driven)
      PurchaseOrderRepositoryPort.java
      PurchaseOrderSummaryQueryPort.java
      PurchaseOrderEventPublisherPort.java
      SupplierQueryPort.java
  service/          ← Use case implementations (implements in/ ports, uses out/ ports)
    CreatePurchaseOrderService.java
    ApprovePurchaseOrderService.java
  command/          ← Command objects (input to use cases)
    CreatePurchaseOrderCommand.java
    ApprovePurchaseOrderCommand.java
```

**PROHIBITED**: Placing command objects in the `domain` package. Commands are application-layer input — they carry the caller's intent, which may differ from the aggregate's internal representation.

**F# application layer** — `Ports.fs` followed by one file per use case:

```
OrganicLeverBe/Purchasing/Application/
  Ports.fs                              ← All input + output port type aliases
  CreatePurchaseOrderWorkflow.fs        ← Implements CreatePurchaseOrderWorkflow port
  ApprovePurchaseOrderWorkflow.fs       ← Implements ApprovePurchaseOrderWorkflow port
```

## Standard 5: Adapter Package Sub-structure

**REQUIRED**: The `adapter` package MUST split into `in/` (driving adapters) and `out/` (driven adapters), with technology sub-packages under `out/`.

#### `Java`

```
com.organicleverbe.purchasing.adapter.
  in.
    web/            ← HTTP driving adapters (Spring MVC controllers)
      PurchaseOrderController.java
      PurchaseRequisitionController.java
  out.
    persistence/    ← JPA driven adapters
      JpaPurchaseOrderRepositoryAdapter.java
      PurchaseOrderJpaEntity.java
      PurchaseOrderJpaRepository.java   ← Spring Data interface
    messaging/      ← Kafka / AMQP driven adapters
      KafkaPurchaseOrderEventPublisherAdapter.java
    acl/            ← Anti-Corruption Layer driven adapters (cross-context)
      AclPurchasingSupplierQueryAdapter.java
```

**PROHIBITED**: Placing ACL adapters in `out/persistence/` or `out/messaging/`. ACL adapters cross context boundaries — they belong in their own `out/acl/` sub-package so the dependency is visible.

**F# adapter structure**:

```
OrganicLeverBe/Purchasing/Adapters/
  In/
    HttpHandler.fs          ← Giraffe route handlers (HTTP input adapter)
  Out/
    NpgsqlRepositoryAdapter.fs
    KafkaEventPublisherAdapter.fs
    AclSupplierQueryAdapter.fs
```

## Standard 6: Coordination and Shared Kernel Placement

**REQUIRED**: Saga orchestrators and shared kernel types that span multiple bounded contexts MUST NOT live inside any single context's package.

**Java placement**:

```
com.organicleverbe.
  coordination.
    saga/
      ProcureToPaySaga.java
  sharedkernel/
    Money.java
    SupplierId.java         ← Used as FK in Purchasing and Receiving
```

**F# placement**:

```
OrganicLeverBe/
  SharedKernel.fs           ← Money, shared value object types
  Coordination/
    ProcureToPaySaga.fs
```

**Rationale**: Placing sagas inside one bounded context implies that context "owns" the process — a strategic design mistake. Sagas coordinate between peers; no peer should host the coordinator.

## Standard 7: OrganicLever Module Catalog

Authoritative list of all required packages/modules for `organiclever-be` Phase 1.

### Java Package Catalog (Active Contexts)

```
com.organicleverbe.
  sharedkernel/                          ← Shared value objects (Money, ids)
  coordination.saga/                     ← ProcureToPaySaga
  purchasing.domain.{model,valueobject,event,service}/
  purchasing.application.{port.in,port.out,service,command}/
  purchasing.adapter.{in.web,out.persistence,out.messaging,out.acl}/
  purchasing.configuration/
  supplier.domain.{model,valueobject,event}/
  supplier.application.{port.in,port.out,service,command}/
  supplier.adapter.{in.web,out.persistence,out.messaging}/
  supplier.configuration/
  receiving.domain.{model,valueobject,event}/
  receiving.application.{port.in,port.out,service,command}/
  receiving.adapter.{in.web,out.persistence,out.acl}/
  receiving.configuration/
```

### F# Module Catalog (Active Contexts)

```
OrganicLeverBe/
  SharedKernel.fs
  Coordination/ProcureToPaySaga.fs
  Purchasing/{Domain,Application/Ports,Application/*Workflow,Adapters/**,CompositionRoot}.fs
  Supplier/{Domain,Application/Ports,Application/*Workflow,Adapters/**,CompositionRoot}.fs
  Receiving/{Domain,Application/Ports,Application/*Workflow,Adapters/**,CompositionRoot}.fs
```

Invoicing and Payments modules are added when those contexts enter active development.

## Rationale

Package structure is the most visible architectural decision in a codebase. When packages encode bounded context, layer, and ubiquitous language simultaneously, accidental coupling is detectable by reading import statements. When packages are organized by technical type alone (`controllers/`, `services/`, `repositories/`), context boundaries are invisible and the hexagonal layering rule can be violated silently. These standards make every structural violation an obvious import smell.

**Educational counterpart**: [Cases](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/overview.md)

## Related Documentation

- **[Bounded Context Mapping Standards](./bounded-context-mapping-standards.md)** — Context identity rules that drive the root package convention
- **[Aggregate Port Boundary Standards](./aggregate-port-boundary-standards.md)** — Which types belong in `domain/` vs `application/port/`
- **[Cross-Context Integration Standards](./cross-context-integration-standards.md)** — Saga and ACL placement rules
- **[Port Standards](../hexagonal-architecture/port-standards.md)** — Port naming conventions applied in `application/port/in/` and `application/port/out/`
- **[Adapter Standards](../hexagonal-architecture/adapter-standards.md)** — Adapter naming conventions applied in `adapter/in/` and `adapter/out/`
- **[Composition Root Standards](../hexagonal-architecture/composition-root-standards.md)** — `configuration/` package wiring
- **[Cases — In OOP](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/in-oop/overview.md)** — Full Java module structure in production context
- **[Cases — In FP](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/cases/in-fp/overview.md)** — Full F# module structure in production context
