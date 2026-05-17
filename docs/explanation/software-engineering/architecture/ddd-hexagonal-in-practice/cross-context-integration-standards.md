---
title: Cross-Context Integration Standards
description: OSE Platform standards for integrating DDD bounded contexts across hexagonal boundaries — domain events, Anti-Corruption Layers, Open Host Services, and saga patterns for OrganicLever Procure-to-Pay
category: explanation
subcategory: architecture
tags:
  - ddd
  - hexagonal-architecture
  - domain-events
  - anticorruption-layer
  - saga
  - standards
  - organiclever
created: 2026-05-17
---

# Cross-Context Integration Standards

OSE Platform standards for communication between DDD bounded contexts across hexagonal boundaries. Defines domain event envelope conventions, Anti-Corruption Layer (ACL) adapter placement, Open Host Service (OHS) patterns, and saga orchestration for OrganicLever Procure-to-Pay.

## Prerequisite Knowledge

**REQUIRED**: Complete [DDD Domain Event Standards](../domain-driven-design-ddd/domain-event-standards.md), [Bounded Context Mapping Standards](./bounded-context-mapping-standards.md), and [DDD + Hexagonal in Practice Tutorials](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/overview.md) before applying these standards.

## Standard 1: Domain Events Are the Primary Cross-Context Integration Mechanism

**REQUIRED**: Bounded contexts MUST communicate asynchronously through domain events whenever the business process tolerates eventual consistency. Synchronous query calls (output port + HTTP adapter) are permitted only when the downstream needs data before it can complete its own use case.

**REQUIRED**: Domain events MUST be immutable. Once emitted, an event record is never modified.

**REQUIRED**: Domain event names MUST use past tense and encode both the originating context and the business occurrence:

```
[Context][AggregateConcept][PastTenseVerb]
```

| Originating Context | Domain Event Name       | Triggered By                  |
| ------------------- | ----------------------- | ----------------------------- |
| Purchasing          | `PurchaseOrderApproved` | `ApprovePurchaseOrderUseCase` |
| Purchasing          | `PurchaseOrderIssued`   | `IssuePurchaseOrderUseCase`   |
| Supplier            | `SupplierApproved`      | `ApproveSupplierUseCase`      |
| Receiving           | `GoodsReceived`         | `RecordGoodsReceiptUseCase`   |
| Invoicing           | `InvoiceMatched`        | `MatchInvoiceUseCase`         |
| Payments            | `PaymentDisbursed`      | `AuthorizePaymentUseCase`     |

**See**: [DDD Domain Event Standards](../domain-driven-design-ddd/domain-event-standards.md) for immutability, naming, and publishing patterns. [DDD + Hexagonal F# Track — Advanced](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/in-fp-in-the-field/advanced.md) for cross-context event wiring examples.

## Standard 2: Domain Event Output Ports Own the Publishing Contract

**REQUIRED**: Domain event publishing MUST go through a dedicated output port defined in the originating context's application layer — not through direct message broker calls in the application service.

**PROHIBITED**: An application service importing a Kafka producer directly:

```java
// WRONG — application service coupled to Kafka
public class ApprovePurchaseOrderService implements ApprovePurchaseOrderUseCase {
  private final KafkaProducer<String, String> kafka; // infrastructure in application layer

  public PurchaseOrderId approve(ApprovePurchaseOrderCommand cmd) {
    PurchaseOrder approved = repository.findById(cmd.orderId()).orElseThrow().approve(cmd.approver());
    repository.save(approved);
    kafka.send("purchase-orders", toJson(new PurchaseOrderApproved(approved.id()))); // WRONG
    return approved.id();
  }
}
```

**REQUIRED**: Application service calls an event publisher output port:

#### `Java`

```java
// CORRECT — output port abstracts the broker
public class ApprovePurchaseOrderService implements ApprovePurchaseOrderUseCase {
  private final PurchaseOrderRepositoryPort repository;
  private final PurchaseOrderEventPublisherPort eventPublisher;

  public PurchaseOrderId approve(ApprovePurchaseOrderCommand cmd) {
    PurchaseOrder approved = repository.findById(cmd.orderId()).orElseThrow().approve(cmd.approver());
    repository.save(approved);
    eventPublisher.publish(new PurchaseOrderApproved(approved.id(), approved.approvedBy(),
        Instant.now()));
    return approved.id();
  }
}
```

#### `F#`

```fsharp
// CORRECT — output port abstracts the broker
let approvePurchaseOrder
  (repository: PurchaseOrderRepositoryPort)
  (eventPublisher: PurchaseOrderEventPublisherPort)
  (cmd: ApprovePurchaseOrderCommand) : Async<Result<PurchaseOrderId, DomainError>> =
  async {
    let! orderResult = repository.findById cmd.OrderId
    match orderResult with
    | Error e -> return Error e
    | Ok order ->
      let! approved = PurchaseOrder.approve cmd.Approver order |> Result.mapError DomainError |> async.Return
      match approved with
      | Error e -> return Error e
      | Ok approvedOrder ->
        do! repository.save approvedOrder
        do! eventPublisher.publish (PurchaseOrderApproved (approvedOrder.Id, approvedOrder.ApprovedBy, DateTimeOffset.UtcNow))
        return Ok approvedOrder.Id
  }
```

## Standard 3: Anti-Corruption Layer Adapters Contain All Translation Logic

**REQUIRED**: When a downstream context consumes data from an upstream context (or external system) with a different model, an Anti-Corruption Layer (ACL) adapter MUST perform all translation between the upstream model and the downstream ubiquitous language. No translation logic appears in application services or domain objects.

**ACL adapter naming**: `Acl[UpstreamContext][Resource]Adapter`

**ACL adapter placement**: downstream context's `adapter/out/acl/` package.

**PROHIBITED**: A downstream aggregate holding a field typed as an upstream DTO:

```java
// WRONG — Receiving aggregate polluted with Purchasing model
public record GoodsReceiptNote(
  GoodsReceiptId id,
  PurchaseOrderDto purchaseOrder, // upstream DTO leaking into downstream aggregate
  ...
) {}
```

**REQUIRED**: The ACL adapter translates before the domain sees the data:

#### `Java`

```java
// ACL adapter in Receiving context — translates Purchasing model to Receiving model
package com.organicleverbe.receiving.adapter.out.acl;

public class AclPurchasingPurchaseOrderAdapter implements PurchaseOrderQueryPort {
  private final PurchasingContextClient purchasingClient;

  public Optional<ApprovedPurchaseOrderRef> findApprovedOrder(PurchaseOrderId id) {
    return purchasingClient.getOrder(id.value())
        .map(dto -> new ApprovedPurchaseOrderRef(
            PurchaseOrderId.of(dto.orderId()),
            SupplierId.of(dto.supplierId()),
            Money.of(dto.totalAmount(), dto.currency())));
  }
}
```

`ApprovedPurchaseOrderRef` is the Receiving context's own value type — the upstream `PurchaseOrderDto` never crosses into the domain core.

**See**: [Adapter Standards](../hexagonal-architecture/adapter-standards.md) for ACL adapter package placement and naming. [Bounded Context Mapping Standards — Standard 2](./bounded-context-mapping-standards.md) for ACL as a context map pattern.

## Standard 4: Open Host Service Ports Define the Stable Upstream Contract

**REQUIRED**: When a bounded context acts as an Open Host Service (OHS) — serving multiple downstream consumers — it MUST define a stable output port interface representing its published contract. This port is versioned independently of the context's internal domain model.

**OHS port naming**: `[UpstreamContext][Resource]PublishedPort` or the domain event publisher port.

**Stability rule**: OHS port method signatures MUST NOT change in a breaking way without a versioning decision. Internal aggregate refactoring does not break the OHS contract.

**OrganicLever OHS ports**:

| Context    | OHS Port                                                                                 | Downstream Consumers  |
| ---------- | ---------------------------------------------------------------------------------------- | --------------------- |
| Purchasing | `PurchaseOrderEventPublisherPort` (emits `PurchaseOrderApproved`, `PurchaseOrderIssued`) | Receiving, Invoicing  |
| Supplier   | `SupplierEventPublisherPort` (emits `SupplierApproved`)                                  | Purchasing, Receiving |
| Receiving  | `GoodsReceiptEventPublisherPort` (emits `GoodsReceived`)                                 | Invoicing, Payments   |
| Invoicing  | `InvoiceEventPublisherPort` (emits `InvoiceMatched`)                                     | Payments              |

## Standard 5: Sagas Orchestrate Multi-Context Business Processes

**REQUIRED**: Business processes spanning multiple bounded contexts (e.g., Procure-to-Pay flow across Purchasing → Receiving → Invoicing → Payments) MUST be coordinated by a saga. OSE Platform uses **orchestration-style sagas** — a dedicated saga class or module holds the process state and issues commands to each context via input ports.

**REQUIRED**: The saga MUST live in a dedicated coordination module outside any single bounded context's hexagon. It is NOT part of any aggregate.

**REQUIRED**: The saga reacts to domain events and issues commands through input ports only — never through direct aggregate method calls.

**Saga placement**:

```
com.organicleverbe.
  coordination.
    saga.
      ProcureToPaySaga.java        ← Orchestrates the full P2P flow
```

**REQUIRED**: Each saga step MUST be idempotent. The saga coordinator persists its state after each step so that a process restart can resume from the last completed step.

**Permitted saga steps for OrganicLever ProcureToPaySaga**:

1. `PurchaseOrderApproved` received → issue command to Receiving via `ActivateGoodsReceiptUseCase`
2. `GoodsReceived` received → issue command to Invoicing via `RegisterInvoiceUseCase`
3. `InvoiceMatched` received → issue command to Payments via `AuthorizePaymentUseCase`
4. `PaymentDisbursed` received → mark saga complete

**See**: [DDD + Hexagonal in Practice — Advanced](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/in-oop-in-the-field/advanced.md) for saga implementation examples.

## Standard 6: Shared Kernel Types Live in `libs/`

**REQUIRED**: Domain types shared between two or more bounded contexts (e.g., `Money`, `SupplierId`, `PurchaseOrderId` as a foreign key type) MUST reside in a shared library under `libs/` — not duplicated in each context's domain package.

**PROHIBITED**: Two contexts defining their own incompatible `Money` type. Cross-context event payloads carrying monetary values will silently use different precision or currency handling.

**REQUIRED**: Shared kernel types MUST be immutable value objects with no behavioral dependencies on any single context's domain logic.

**Current shared kernel location**: `libs/golang-commons` (Go utilities). Java/F# shared types are collocated in `organiclever-be` under a `shared-kernel` module until a dedicated library is warranted.

## Rationale

Cross-context integration is where DDD and Hexagonal Architecture decisions interact most visibly. Domain events crossing context boundaries must go through output ports to remain testable. ACL adapters must contain all translation to keep downstream domain models clean. Without these rules, context map decisions made in strategic design evaporate at implementation time — the hexagons collapse into a distributed monolith sharing domain models and bypassing port boundaries.

**Educational counterpart**: [DDD + Hexagonal in Practice Tutorials](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/overview.md)

## Related Documentation

- **[Bounded Context Mapping Standards](./bounded-context-mapping-standards.md)** — Context map patterns that drive ACL and OHS decisions
- **[Aggregate Port Boundary Standards](./aggregate-port-boundary-standards.md)** — Event publisher output ports within each aggregate's boundary
- **[Module Organization Standards](./module-organization-standards.md)** — Package placement for sagas, ACL adapters, and shared kernel types
- **[DDD Domain Event Standards](../domain-driven-design-ddd/domain-event-standards.md)** — Event naming, immutability, and publishing patterns
- **[Adapter Standards](../hexagonal-architecture/adapter-standards.md)** — ACL adapter naming and package placement
- **[DDD + Hexagonal F# Track](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/in-fp-in-the-field/overview.md)** — F# cross-context wiring examples
- **[DDD + Hexagonal Java Track](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/in-oop-in-the-field/overview.md)** — Java cross-context wiring examples
