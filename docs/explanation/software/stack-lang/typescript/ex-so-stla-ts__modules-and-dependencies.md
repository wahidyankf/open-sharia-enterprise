---
title: "TypeScript Modules and Dependencies"
description: Module systems and dependency management in TypeScript
category: explanation
subcategory: stack-lang
tags:
  - typescript
  - modules
  - esm
  - npm
  - pnpm
  - bun
related:
  - ./ex-so-stla-ts__best-practices.md
  - ../../../../../governance/principles/software-engineering/reproducibility.md
principles:
  - reproducibility
  - explicit-over-implicit
last_updated: 2025-01-23
---

# TypeScript Modules and Dependencies

**Quick Reference**: [Overview](#overview) | [ES Modules](#es-modules) | [Package Managers](#package-managers) | [Module Resolution](#module-resolution) | [Monorepos](#workspaces-and-monorepos) | [Related Documentation](#related-documentation)

## Overview

TypeScript uses ES modules as its primary module system. Understanding module resolution, package managers, and dependency management is essential for reproducible builds.

## ES Modules

### Module Dependency Graph

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    App["app.ts<br/>#40;Entry Point#41;"]:::blue
    ZakatService["zakat-service.ts"]:::orange
    ZakatCalc["zakat-calculator.ts"]:::teal
    Money["money.ts<br/>#40;Value Object#41;"]:::brown
    DonRepo["donation-repository.ts"]:::purple

    App --> ZakatService
    ZakatService --> ZakatCalc
    ZakatService --> DonRepo
    ZakatCalc --> Money
    DonRepo --> Money

    Note1["Dependencies flow<br/>from high-level<br/>to low-level<br/>modules"]

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef brown fill:#CA9161,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

```typescript
// Exporting
export interface Money {
  readonly amount: number;
  readonly currency: string;
}

export function createMoney(amount: number, currency: string): Money {
  return Object.freeze({ amount, currency });
}

export default class ZakatCalculator {
  calculate(wealth: Money, nisab: Money): Money {
    if (wealth.amount < nisab.amount) {
      return createMoney(0, wealth.currency);
    }
    return createMoney(wealth.amount * 0.025, wealth.currency);
  }
}

// Importing
import ZakatCalculator, { Money, createMoney } from "./zakat";

const calculator = new ZakatCalculator();
const wealth = createMoney(100000, "USD");
```

### ESM Import/Export Flow

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph LR
    Module["Module<br/>zakat.ts"]:::blue
    Export1["export default<br/>ZakatCalculator"]:::orange
    Export2["export interface<br/>Money"]:::orange
    Export3["export function<br/>createMoney"]:::orange

    Import["Importing Module<br/>app.ts"]:::teal
    Default["Default Import<br/>ZakatCalculator"]:::purple
    Named["Named Imports<br/>Money, createMoney"]:::purple

    Module --> Export1
    Module --> Export2
    Module --> Export3

    Export1 -.-> Default
    Export2 -.-> Named
    Export3 -.-> Named

    Default --> Import
    Named --> Import

    Note1["ESM is static:<br/>imports resolved<br/>at compile time"]

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

### Barrel Export Pattern

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Barrel["index.ts<br/>#40;Barrel#41;"]:::blue
    Module1["money.ts"]:::orange
    Module2["zakat-calculator.ts"]:::orange
    Module3["donation.ts"]:::orange
    Consumer["app.ts<br/>#40;Consumer#41;"]:::teal

    Module1 --> Barrel
    Module2 --> Barrel
    Module3 --> Barrel
    Barrel --> Consumer

    Note1["Barrel pattern:<br/>Single import point<br/>for related modules"]
    Note2["import { Money, ZakatCalculator }<br/>from './domain'"]

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

## Package Managers

```json
// package.json with Volta
{
  "name": "ose-platform",
  "volta": {
    "node": "24.11.1",
    "npm": "11.6.3"
  },
  "dependencies": {
    "typescript": "5.7.3",
    "express": "5.2.1"
  },
  "devDependencies": {
    "@types/node": "24.11.1",
    "vitest": "4.0.18"
  }
}
```

### npm (11.8.0)

```bash
npm ci              # Install exact versions from lockfile
npm install pkg     # Add dependency
npm update          # Update within semver range
npm audit           # Check for vulnerabilities
```

### pnpm (10.28.1)

```bash
pnpm install        # Fast, disk-efficient installs
pnpm add pkg        # Add dependency
pnpm update         # Update dependencies
```

### bun (1.3.6)

```bash
bun install         # Ultra-fast installs
bun add pkg         # Add dependency
bun update          # Update dependencies
```

## Module Resolution

```json
// tsconfig.json
{
  "compilerOptions": {
    "moduleResolution": "Bundler", // or "Node16", "NodeNext"
    "module": "ESNext",
    "target": "ES2023",
    "baseUrl": "./src",
    "paths": {
      "@domain/*": ["domain/*"],
      "@infrastructure/*": ["infrastructure/*"]
    }
  }
}
```

## Workspaces and Monorepos

### Nx Workspace

```json
// nx.json
{
  "affected": {
    "defaultBase": "main"
  },
  "targetDefaults": {
    "build": {
      "cache": true
    }
  }
}
```

### pnpm Workspaces

```yaml
# pnpm-workspace.yaml
packages:
  - "apps/*"
  - "libs/*"
```

## Related Documentation

- **[TypeScript Best Practices](./ex-so-stla-ts__best-practices.md)** - Coding standards
- **[Reproducibility Principle](../../../../../governance/principles/software-engineering/reproducibility.md)** - Reproducible builds

---

**Last Updated**: 2025-01-23
**TypeScript Version**: 5.0+ (baseline), 5.4+ (milestone), 5.6+ (stable), 5.7.3+ (latest stable)
**Maintainers**: OSE Documentation Team
