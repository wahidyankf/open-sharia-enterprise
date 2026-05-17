---
title: Bounded Context Mapping Standards
description: OSE Platform standards for mapping DDD bounded contexts onto Hexagonal Architecture port/adapter structure — context boundary rules, context map pattern translation, and OrganicLever context catalog
category: explanation
subcategory: architecture
tags:
  - ddd
  - hexagonal-architecture
  - bounded-contexts
  - ports-and-adapters
  - standards
  - organiclever
created: 2026-05-17
---

# Bounded Context Mapping Standards

OSE Platform standards for translating DDD bounded context decisions into Hexagonal Architecture structure. Each bounded context becomes one hexagon; context map relationships become specific port and adapter patterns.

## Prerequisite Knowledge

**REQUIRED**: Complete [DDD Bounded Context Standards](../domain-driven-design-ddd/bounded-context-standards.md), [Hexagonal Architecture Overview](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/hexagonal-architecture/overview.md), and [DDD + Hexagonal in Practice Overview](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/overview.md) before applying these standards.

## Standard 1: One Bounded Context — One Hexagon

**REQUIRED**: Each DDD bounded context MUST correspond to exactly one hexagon (one set of domain core, application layer, ports, and adapters). The boundary of the hexagon and the boundary of the bounded context are the same boundary.

**PROHIBITED**: A single hexagon spanning two bounded contexts. This is the DDD equivalent of the monolith — ubiquitous languages from different contexts collide in the domain core.

**PROHIBITED**: Two hexagons sharing a domain core. If `purchasing` and `supplier` shared one aggregate package, neither would have a clean dependency direction.

The rule maps directly onto Nx app boundaries per [DDD Bounded Context Standards](../domain-driven-design-ddd/bounded-context-standards.md). OrganicLever hosts multiple bounded contexts within one Nx app (`organiclever-be`) during Phase 1 — each context still has its own isolated hexagon within that app's package structure.

**Rationale**: Hexagonal architecture's value — replaceable adapters, testable domain core — is lost when the hexagon boundary does not coincide with the context boundary. Mixing contexts inside the hexagon means the domain core cannot be independently reasoned about, tested, or evolved.

**See**: [DDD + Hexagonal Tutorials — Bounded Context Wiring](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/in-oop-in-the-field/beginner.md) for a worked example.

## Standard 2: Context Map Patterns Translate to Port/Adapter Patterns

**REQUIRED**: Every context map relationship MUST be expressed as a specific port/adapter arrangement. The context map is not a documentation artifact only — it drives implementation decisions.

| Context Map Pattern        | Hexagonal Translation                                                      | Implementation                                                                          |
| -------------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| Customer/Supplier          | Downstream uses an output port + HTTP adapter calling upstream             | Output port in downstream application layer; `Rest[Resource]QueryAdapter` implements it |
| Conformist                 | Downstream adopts upstream model directly; no translation adapter          | Shared value type or DTO imported from `libs/`                                          |
| Anticorruption Layer (ACL) | Downstream wraps upstream call in ACL output port + translation adapter    | `Acl[UpstreamContext][Resource]Adapter` contains all translation logic                  |
| Open Host Service (OHS)    | Upstream publishes a stable port contract consumed by multiple downstreams | Port interface in upstream `application/port/out/`; consumed via REST or messaging      |
| Published Language         | Upstream emits domain events as a stable schema; downstreams subscribe     | Event envelope type in `libs/`; event consumer adapter in downstream                    |
| Partnership                | Two contexts collaborate bidirectionally; no upstream/downstream           | Each context defines its own ports; shared domain events carry the coupling             |
| Shared Kernel              | Two contexts share a small, co-owned model fragment                        | Shared type in `libs/golang-commons` or a dedicated shared library                      |

**PROHIBITED**: An adapter that translates between context models without a named port interface. The translation contract must be visible as a port — otherwise swapping the integration technology requires understanding the entire adapter's internals.

**See**: [Cross-Context Integration Standards](./cross-context-integration-standards.md) for domain event and ACL wiring in OrganicLever.

## Standard 3: OrganicLever Context Boundary Catalog

Authoritative mapping of OrganicLever bounded contexts to hexagonal structure. All five contexts live within `organiclever-be`.

### Purchasing Context

- **Hexagon root package**: `com.organicleverbe.purchasing` (Java) / `OrganicLeverBe.Purchasing` (F#)
- **Context map relationships**: Customer to Supplier (queries vendor master); Open Host Service to Receiving (exposes approved PO)
- **Cross-context ports**: `SupplierQueryPort` (output, calls Supplier context); `ApprovedPurchaseOrderPublisherPort` (output, publishes domain event)
- **Primary aggregates**: `PurchaseRequisition`, `PurchaseOrder`

### Supplier Context

- **Hexagon root package**: `com.organicleverbe.supplier` / `OrganicLeverBe.Supplier`
- **Context map relationships**: Open Host Service to Purchasing and Receiving (exposes vendor master read model)
- **Cross-context ports**: `SupplierApprovalEventPublisherPort` (output, publishes `SupplierApproved`)
- **Primary aggregate**: `Supplier`

### Receiving Context

- **Hexagon root package**: `com.organicleverbe.receiving` / `OrganicLeverBe.Receiving`
- **Context map relationships**: Conformist to Purchasing (accepts PO as-is); Customer to Supplier (queries vendor)
- **Cross-context ports**: `PurchaseOrderQueryPort` (output, calls Purchasing); `GoodsReceiptEventPublisherPort` (output, publishes `GoodsReceived`)
- **Primary aggregate**: `GoodsReceiptNote`

### Invoicing Context (Planned)

- **Hexagon root package**: `com.organicleverbe.invoicing` / `OrganicLeverBe.Invoicing`
- **Context map relationships**: Customer to Purchasing and Receiving (three-way match); Anticorruption Layer to external invoice provider
- **Cross-context ports**: `PurchaseOrderMatchQueryPort`, `GoodsReceiptMatchQueryPort`, `InvoiceMatchedEventPublisherPort`
- **Primary aggregate**: `Invoice`

### Payments Context (Planned)

- **Hexagon root package**: `com.organicleverbe.payments` / `OrganicLeverBe.Payments`
- **Context map relationships**: Customer to Invoicing (queries matched invoices); Open Host Service to external banking
- **Cross-context ports**: `MatchedInvoiceQueryPort`, `BankingGatewayPort`, `PaymentDisbursedEventPublisherPort`
- **Primary aggregate**: `Payment`

## Standard 4: Ubiquitous Language Encodes Context Identity in Port Names

**REQUIRED**: Port and adapter names MUST reflect the bounded context's ubiquitous language, not generic technical terms.

**PROHIBITED**: Generic names that erase context identity:

```
// WRONG — could belong to any context
RepositoryPort
QueryPort
EventPublisherPort
```

**REQUIRED**: Names encoding both the domain concept and the context:

```
// CORRECT — context and domain concept both visible
PurchaseOrderRepositoryPort       // Purchasing context, PurchaseOrder concept
SupplierQueryPort                 // call into Supplier context
GoodsReceiptEventPublisherPort    // Receiving context, domain event
```

**Rationale**: When a port name contains the domain term, accidental coupling between contexts becomes visible. A developer who sees `SupplierQueryPort` inside `com.organicleverbe.invoicing` immediately knows this is a cross-context dependency — an important architectural signal.

**See**: [Port Standards](../hexagonal-architecture/port-standards.md) for the full naming convention. [Module Organization Standards](./module-organization-standards.md) for package placement rules.

## Rationale

Context mapping is the strategic design decision. Port/adapter structure is the implementation decision. Without an explicit rule connecting the two, teams make the context map correctly in theory and then muddy it in code by letting contexts share adapters, merge repository implementations, or skip the translation layer. These standards close that gap.

**Educational counterpart**: [DDD + Hexagonal in Practice Tutorials](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/overview.md)

## Related Documentation

- **[Aggregate Port Boundary Standards](./aggregate-port-boundary-standards.md)** — How aggregates within each context map to port contracts
- **[Cross-Context Integration Standards](./cross-context-integration-standards.md)** — Domain events, ACL adapters, and saga wiring
- **[Module Organization Standards](./module-organization-standards.md)** — Package structure encoding these context boundaries
- **[DDD Bounded Context Standards](../domain-driven-design-ddd/bounded-context-standards.md)** — Nx app alignment and context naming rules
- **[Port Standards](../hexagonal-architecture/port-standards.md)** — OrganicLever port catalog and naming conventions
- **[Adapter Standards](../hexagonal-architecture/adapter-standards.md)** — ACL and cross-context adapter naming
