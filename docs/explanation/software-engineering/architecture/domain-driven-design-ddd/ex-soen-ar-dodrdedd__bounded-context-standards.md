---
title: "DDD Bounded Context Standards"
description: OSE Platform standards for bounded context organization, Nx app alignment, and context mapping
category: explanation
subcategory: architecture
tags:
  - ddd
  - bounded-contexts
  - nx
  - standards
principles:
  - explicit-over-implicit
created: 2026-02-09
updated: 2026-02-09
---

# DDD Bounded Context Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding Domain-Driven Design](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/architecture/domain-driven-design-ddd/) before using these standards.

**This document is OSE Platform-specific**, defining how to organize bounded contexts in THIS codebase.

## Purpose

OSE Platform bounded context organization standards. MUST/SHOULD/MAY rules for context boundaries.

## REQUIRED: One Bounded Context = One Nx App

**REQUIRED**: Each bounded context MUST be organized as a separate Nx app under `apps/`.

```
apps/
├── zakat-context/          # Zakat calculation bounded context
├── donation-context/       # Donation management
├── beneficiary-context/    # Beneficiary registry
└── contract-context/       # Islamic contract management
```

**PROHIBITED**: Multi-context apps. Each app = one bounded context only.

## Context Naming

**Format**: `[domain]-context`

**Examples**:

- `zakat-context`
- `donation-context`
- `beneficiary-context`

## Layered Structure

**REQUIRED**: Each bounded context MUST use layered architecture:

```
zakat-context/
├── domain/             # Aggregates, value objects, domain services
├── application/        # Application services, use cases
├── infrastructure/     # Persistence, messaging, external APIs
└── presentation/       # Controllers, GraphQL resolvers
```

## Context Mapping Patterns

**REQUIRED**: Document context relationships using standard patterns.

| Pattern              | OSE Usage                               | Implementation               |
| -------------------- | --------------------------------------- | ---------------------------- |
| Customer/Supplier    | Donation → Beneficiary (requests data)  | REST API calls               |
| Partnership          | Zakat ↔ Payment (collaborate)           | Shared domain events         |
| Shared Kernel        | Multiple contexts share Money           | Shared library (libs/ts-\*/) |
| Conformist           | Reporting → Zakat (conforms to API)     | Client-side adapter          |
| Anticorruption Layer | Internal → Legacy External (translates) | Adapter layer                |

---

**Last Updated**: 2026-02-09
