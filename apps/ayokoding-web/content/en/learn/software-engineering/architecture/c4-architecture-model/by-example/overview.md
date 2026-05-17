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
