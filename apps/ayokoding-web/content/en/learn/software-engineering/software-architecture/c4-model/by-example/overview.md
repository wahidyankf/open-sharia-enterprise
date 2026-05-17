---
title: "Overview"
date: 2026-01-31T00:00:00+07:00
draft: false
weight: 10000000
description: "Learn C4 Model through practical diagram examples using the procurement-platform-be Procure-to-Pay system — system context, containers, components, and code-level views"
tags: ["c4-model", "architecture", "tutorial", "by-example", "diagrams", "visualization"]
---

**Want to master C4 Model architecture diagrams through practical examples?** This by-example guide teaches C4 Model through annotated diagram examples using a real-world **Procure-to-Pay (P2P) procurement platform** as the consistent domain across all 85 examples.

## What Is C4 Model By-Example Learning?

C4 Model by-example learning is a **diagram-first approach** where you learn through annotated, practical architecture diagrams rather than narrative explanations. Each example shows:

- **What the diagram represents** — Clear explanation of the architectural view
- **Key elements** — Identification of systems, containers, components, and relationships
- **Design decisions** — Rationale behind structural choices
- **Mermaid diagrams** — Working C4 diagrams you can modify and use

This approach is **ideal for software architects and developers** who need to document and communicate system architecture using the C4 Model framework.

## The Domain: Procurement Platform

Every example in this guide models the `procurement-platform-be` — the REST API backend of a generic **Procure-to-Pay (P2P) platform**. The reader framing: "Employees request goods/services, managers approve, suppliers fulfill, finance pays."

Using one consistent domain means you can compare diagrams across levels and see exactly how C4 zooms in from a high-level system landscape down to individual method implementations.

## Learning Path

The C4 Model by-example tutorial guides you through examples organized into three progressive levels.

### Beginner: System Context (Examples 1–30)

Level 1 diagrams. You see the Procurement Platform as a black box surrounded by Buyer Employees, Suppliers, the Bank, and the Internal ERP. Diagrams stay at the person/system boundary — no implementation detail.

### Intermediate: Containers and Components (Examples 31–60)

Level 2 and Level 3 diagrams. You zoom inside the platform and see `web-ui`, `purchasing-api`, `receiving-api`, `invoicing-api`, `payments-worker`, the `event-bus`, `postgres`, and `read-store`. Then you zoom further into `purchasing-api` to see HTTP controllers, application services, the domain layer, and infrastructure adapters.

### Advanced: Code, Dynamic, and Deployment Views (Examples 61–85)

Level 4 diagrams plus supplementary views. You examine `PurchaseOrder.approve()` at code level, trace the Requisition → Approval → PO → GRN flow as a dynamic sequence, and model the Kubernetes deployment topology.

## Examples by Level

### Beginner (Examples 1–30)

- [Example 1: The Four Levels of C4](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-1-the-four-levels-of-c4)
- [Example 2: C4 Notation Basics — Person, System, External System](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-2-c4-notation-basics--person-system-external-system)
- [Example 3: Relationship Labels and Direction](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-3-relationship-labels-and-direction)
- [Example 4: System Boundary Box](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-4-system-boundary-box)
- [Example 5: C4 Level Selection Guide](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-5-c4-level-selection-guide)
- [Example 6: Minimal System Context — Buyer and Platform](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-6-minimal-system-context--buyer-and-platform)
- [Example 7: Adding the Supplier Actor](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-7-adding-the-supplier-actor)
- [Example 8: Adding the Bank External System](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-8-adding-the-bank-external-system)
- [Example 9: Adding the Internal ERP / GL](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-9-adding-the-internal-erp--gl)
- [Example 10: Full Level 1 — All Four Actors](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-10-full-level-1--all-four-actors)
- [Example 11: Approval Workflow Actor — The Approving Manager](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-11-approval-workflow-actor--the-approving-manager)
- [Example 12: Supplier as External System — EDI Integration](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-12-supplier-as-external-system--edi-integration)
- [Example 13: Bank Integration — Payment Disbursement](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-13-bank-integration--payment-disbursement)
- [Example 14: ERP Integration — Chart of Accounts](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-14-erp-integration--chart-of-accounts)
- [Example 15: Supplier Notification System](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-15-supplier-notification-system)
- [Example 16: Secret Manager Integration](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-16-secret-manager-integration)
- [Example 17: Murabaha Bank — Optional Sharia Financing](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-17-murabaha-bank--optional-sharia-financing)
- [Example 18: System Context with Compliance and Audit](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-18-system-context-with-compliance-and-audit)
- [Example 19: Notification Service — Multiple Channels](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-19-notification-service--multiple-channels)
- [Example 20: Observability and Monitoring Integration](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-20-observability-and-monitoring-integration)
- [Example 21: Synchronous vs. Asynchronous Relationships](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-21-synchronous-vs-asynchronous-relationships)
- [Example 22: Approval Workflow — Three Levels Shown](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-22-approval-workflow--three-levels-shown)
- [Example 23: Goods Receipt — Warehouse Actor](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-23-goods-receipt--warehouse-actor)
- [Example 24: Invoice Registration — Finance Clerk Actor](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-24-invoice-registration--finance-clerk-actor)
- [Example 25: Complete P2P Flow — All Actors](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-25-complete-p2p-flow--all-actors)
- [Example 26: System Boundary — What Is In and What Is Out](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-26-system-boundary--what-is-in-and-what-is-out)
- [Example 27: Multiple Buyer Organizations — Multi-Tenancy at Context](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-27-multiple-buyer-organizations--multi-tenancy-at-context)
- [Example 28: Geographic Distribution — Regional Deployments](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-28-geographic-distribution--regional-deployments)
- [Example 29: System Context with Security Boundary](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-29-system-context-with-security-boundary)
- [Example 30: Context Diagram Anti-Patterns to Avoid](/en/learn/software-engineering/software-architecture/c4-model/by-example/beginner#example-30-context-diagram-anti-patterns-to-avoid)

### Intermediate (Examples 31–60)

- [Example 31: Minimal Container Diagram — web-ui and purchasing-api](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-31-minimal-container-diagram--web-ui-and-purchasing-api)
- [Example 32: Adding PostgreSQL — the Write Store](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-32-adding-postgresql--the-write-store)
- [Example 33: Adding the Event Bus — Kafka](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-33-adding-the-event-bus--kafka)
- [Example 34: Adding the payments-worker Container](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-34-adding-the-payments-worker-container)
- [Example 35: Adding the read-store Container](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-35-adding-the-read-store-container)
- [Example 36: Adding the secret-manager Container](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-36-adding-the-secret-manager-container)
- [Example 37: Full Container Diagram — All Nine Containers](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-37-full-container-diagram--all-nine-containers)
- [Example 38: Container Diagram — Technology Choices](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-38-container-diagram--technology-choices)
- [Example 39: Request-Response vs. Event-Driven Containers](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-39-request-response-vs-event-driven-containers)
- [Example 40: Container Diagram — Scaling Annotations](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-40-container-diagram--scaling-annotations)
- [Example 41: Container Diagram — Failure Modes](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-41-container-diagram--failure-modes)
- [Example 42: Container Diagram — Network Topology](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-42-container-diagram--network-topology)
- [Example 43: Container Diagram — Data Ownership](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-43-container-diagram--data-ownership)
- [Example 44: Three-Way Match Flow — Container Interaction](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-44-three-way-match-flow--container-interaction)
- [Example 45: Container Diagram — Deployment Units and Teams](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-45-container-diagram--deployment-units-and-teams)
- [Example 46: Container Diagram — Health and Readiness Boundaries](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-46-container-diagram--health-and-readiness-boundaries)
- [Example 47: Component Overview — purchasing-api Layer Structure](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-47-component-overview--purchasing-api-layer-structure)
- [Example 48: HTTP Layer Components — Controllers and DTOs](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-48-http-layer-components--controllers-and-dtos)
- [Example 49: Application Services — Use Case Handlers](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-49-application-services--use-case-handlers)
- [Example 50: Domain Layer — Aggregate Components](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-50-domain-layer--aggregate-components)
- [Example 51: Infrastructure Adapters — Repository Implementations](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-51-infrastructure-adapters--repository-implementations)
- [Example 52: Component Diagram — SubmitRequisitionHandler Flow](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-52-component-diagram--submitrequisitionhandler-flow)
- [Example 53: Component Diagram — ApprovePOHandler with FSM Guard](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-53-component-diagram--approvepohandler-with-fsm-guard)
- [Example 54: Component Diagram — Infrastructure Adapter Swapping](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-54-component-diagram--infrastructure-adapter-swapping)
- [Example 55: Component Diagram — Receiving-api Internal Structure](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-55-component-diagram--receiving-api-internal-structure)
- [Example 56: Component Diagram — invoicing-api Three-Way Match](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-56-component-diagram--invoicing-api-three-way-match)
- [Example 57: Component Diagram — payments-worker Internal Structure](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-57-component-diagram--payments-worker-internal-structure)
- [Example 58: Component Diagram — Approval Router Component](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-58-component-diagram--approval-router-component)
- [Example 59: Component Diagram — Event Consumer Registration Pattern](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-59-component-diagram--event-consumer-registration-pattern)
- [Example 60: Component Diagram — Anti-Corruption Layer Between Contexts](/en/learn/software-engineering/software-architecture/c4-model/by-example/intermediate#example-60-component-diagram--anti-corruption-layer-between-contexts)

### Advanced (Examples 61–85)

- [Example 61: PurchaseOrder Aggregate — Class Structure](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-61-purchaseorder-aggregate--class-structure)
- [Example 62: PurchaseOrder.approve() — FSM Transition Guard](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-62-purchaseorderapprove--fsm-transition-guard)
- [Example 63: PurchaseOrder State Machine — Full Transition Diagram](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-63-purchaseorder-state-machine--full-transition-diagram)
- [Example 64: PurchaseRequisition — Simplified State Machine](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-64-purchaserequisition--simplified-state-machine)
- [Example 65: PurchaseOrderId Value Object — Code Level](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-65-purchaseorderid-value-object--code-level)
- [Example 66: Money Value Object — Arithmetic Safety](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-66-money-value-object--arithmetic-safety)
- [Example 67: GoodsReceiptNote Aggregate — Code Level](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-67-goodsreceiptnote-aggregate--code-level)
- [Example 68: Domain Event — Code Level Structure](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-68-domain-event--code-level-structure)
- [Example 69: Requisition Submission Flow — Dynamic Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-69-requisition-submission-flow--dynamic-sequence)
- [Example 70: PO Approval Flow — Multi-Level Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-70-po-approval-flow--multi-level-sequence)
- [Example 71: Goods Receipt Flow — Dynamic Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-71-goods-receipt-flow--dynamic-sequence)
- [Example 72: Three-Way Match Flow — Dynamic Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-72-three-way-match-flow--dynamic-sequence)
- [Example 73: Payment Run Flow — Dynamic Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-73-payment-run-flow--dynamic-sequence)
- [Example 74: Dispute Resolution Flow — Dynamic Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-74-dispute-resolution-flow--dynamic-sequence)
- [Example 75: Cancelled PO Flow — Off-Ramp Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-75-cancelled-po-flow--off-ramp-sequence)
- [Example 76: Full P2P Happy Path — Abbreviated Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-76-full-p2p-happy-path--abbreviated-sequence)
- [Example 77: Murabaha Financing Flow — Dynamic Sequence](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-77-murabaha-financing-flow--dynamic-sequence)
- [Example 78: Kubernetes Deployment — Basic Pod Layout](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-78-kubernetes-deployment--basic-pod-layout)
- [Example 79: Kubernetes — Health Check and Rolling Update](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-79-kubernetes--health-check-and-rolling-update)
- [Example 80: Kubernetes — Horizontal Pod Autoscaler](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-80-kubernetes--horizontal-pod-autoscaler)
- [Example 81: Deployment Diagram — Multi-Region Active-Active](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-81-deployment-diagram--multi-region-active-active)
- [Example 82: Deployment Diagram — Blue-Green Deployment](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-82-deployment-diagram--blue-green-deployment)
- [Example 83: Deployment Diagram — Database Migration Strategy](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-83-deployment-diagram--database-migration-strategy)
- [Example 84: Deployment Diagram — Observability Stack](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-84-deployment-diagram--observability-stack)
- [Example 85: C4 Diagram Versioning and Change Management](/en/learn/software-engineering/software-architecture/c4-model/by-example/advanced#example-85-c4-diagram-versioning-and-change-management)

## Structure of Each Example

Every example follows a consistent format:

1. **Brief Explanation** — What the example demonstrates and why it matters
2. **C4 Diagram** — Mermaid diagram showing the architecture
3. **Key Elements** — Explanation of major components and relationships
4. **Design Rationale** — Why this structure was chosen
5. **Key Takeaway** — The core insight to retain
6. **Why It Matters** — Business impact and real-world significance

## What Is NOT Covered

- Deep architectural theory (see by-concept tutorials for conceptual understanding)
- Tool-specific implementations beyond Mermaid
- Organization-specific notation conventions

## Prerequisites

- Basic understanding of software architecture concepts
- Familiarity with system design principles
- Experience reading technical diagrams
