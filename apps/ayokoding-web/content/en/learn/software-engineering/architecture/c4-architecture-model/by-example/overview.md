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

- Example 1: The Four Levels of C4
- Example 2: C4 Notation Basics — Person, System, External System
- Example 3: Relationship Labels and Direction
- Example 4: System Boundary Box
- Example 5: C4 Level Selection Guide
- Example 6: Minimal System Context — Buyer and Platform
- Example 7: Adding the Supplier Actor
- Example 8: Adding the Bank External System
- Example 9: Adding the Internal ERP / GL
- Example 10: Full Level 1 — All Four Actors
- Example 11: Approval Workflow Actor — The Approving Manager
- Example 12: Supplier as External System — EDI Integration
- Example 13: Bank Integration — Payment Disbursement
- Example 14: ERP Integration — Chart of Accounts
- Example 15: Supplier Notification System
- Example 16: Secret Manager Integration
- Example 17: Murabaha Bank — Optional Sharia Financing
- Example 18: System Context with Compliance and Audit
- Example 19: Notification Service — Multiple Channels
- Example 20: Observability and Monitoring Integration
- Example 21: Synchronous vs. Asynchronous Relationships
- Example 22: Approval Workflow — Three Levels Shown
- Example 23: Goods Receipt — Warehouse Actor
- Example 24: Invoice Registration — Finance Clerk Actor
- Example 25: Complete P2P Flow — All Actors
- Example 26: System Boundary — What Is In and What Is Out
- Example 27: Multiple Buyer Organizations — Multi-Tenancy at Context
- Example 28: Geographic Distribution — Regional Deployments
- Example 29: System Context with Security Boundary
- Example 30: Context Diagram Anti-Patterns to Avoid

### Intermediate (Examples 31–60)

- Example 31: Minimal Container Diagram — web-ui and purchasing-api
- Example 32: Adding PostgreSQL — the Write Store
- Example 33: Adding the Event Bus — Kafka
- Example 34: Adding the payments-worker Container
- Example 35: Adding the read-store Container
- Example 36: Adding the secret-manager Container
- Example 37: Full Container Diagram — All Nine Containers
- Example 38: Container Diagram — Technology Choices
- Example 39: Request-Response vs. Event-Driven Containers
- Example 40: Container Diagram — Scaling Annotations
- Example 41: Container Diagram — Failure Modes
- Example 42: Container Diagram — Network Topology
- Example 43: Container Diagram — Data Ownership
- Example 44: Three-Way Match Flow — Container Interaction
- Example 45: Container Diagram — Deployment Units and Teams
- Example 46: Container Diagram — Health and Readiness Boundaries
- Example 47: Component Overview — purchasing-api Layer Structure
- Example 48: HTTP Layer Components — Controllers and DTOs
- Example 49: Application Services — Use Case Handlers
- Example 50: Domain Layer — Aggregate Components
- Example 51: Infrastructure Adapters — Repository Implementations
- Example 52: Component Diagram — SubmitRequisitionHandler Flow
- Example 53: Component Diagram — ApprovePOHandler with FSM Guard
- Example 54: Component Diagram — Infrastructure Adapter Swapping
- Example 55: Component Diagram — Receiving-api Internal Structure
- Example 56: Component Diagram — invoicing-api Three-Way Match
- Example 57: Component Diagram — payments-worker Internal Structure
- Example 58: Component Diagram — Approval Router Component
- Example 59: Component Diagram — Event Consumer Registration Pattern
- Example 60: Component Diagram — Anti-Corruption Layer Between Contexts

### Advanced (Examples 61–85)

- Example 61: PurchaseOrder Aggregate — Class Structure
- Example 62: PurchaseOrder.approve() — FSM Transition Guard
- Example 63: PurchaseOrder State Machine — Full Transition Diagram
- Example 64: PurchaseRequisition — Simplified State Machine
- Example 65: PurchaseOrderId Value Object — Code Level
- Example 66: Money Value Object — Arithmetic Safety
- Example 67: GoodsReceiptNote Aggregate — Code Level
- Example 68: Domain Event — Code Level Structure
- Example 69: Requisition Submission Flow — Dynamic Sequence
- Example 70: PO Approval Flow — Multi-Level Sequence
- Example 71: Goods Receipt Flow — Dynamic Sequence
- Example 72: Three-Way Match Flow — Dynamic Sequence
- Example 73: Payment Run Flow — Dynamic Sequence
- Example 74: Dispute Resolution Flow — Dynamic Sequence
- Example 75: Cancelled PO Flow — Off-Ramp Sequence
- Example 76: Full P2P Happy Path — Abbreviated Sequence
- Example 77: Murabaha Financing Flow — Dynamic Sequence
- Example 78: Kubernetes Deployment — Basic Pod Layout
- Example 79: Kubernetes — Health Check and Rolling Update
- Example 80: Kubernetes — Horizontal Pod Autoscaler
- Example 81: Deployment Diagram — Multi-Region Active-Active
- Example 82: Deployment Diagram — Blue-Green Deployment
- Example 83: Deployment Diagram — Database Migration Strategy
- Example 84: Deployment Diagram — Observability Stack
- Example 85: C4 Diagram Versioning and Change Management

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
