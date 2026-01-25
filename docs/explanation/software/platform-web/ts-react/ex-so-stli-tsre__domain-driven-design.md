---
title: "React Domain-Driven Design"
description: Implementing DDD patterns in React applications
category: explanation
subcategory: stack-libs
tags:
  - react
  - ddd
  - domain-driven-design
  - architecture
  - typescript
related:
  - ./ex-so-stli-tsre__best-practices.md
  - ./ex-so-stli-tsre__component-architecture.md
principles:
  - explicit-over-implicit
last_updated: 2026-01-25
---

# React Domain-Driven Design

## Quick Reference

**Navigation**: [Stack Libraries](../README.md) > [TypeScript React](./README.md) > Domain-Driven Design

**Related Guides**:

- [Component Architecture](./ex-so-stli-tsre__component-architecture.md) - Component patterns
- [TypeScript](./ex-so-stli-tsre__typescript.md) - Domain modeling
- [Best Practices](./ex-so-stli-tsre__best-practices.md) - Architecture standards

## Overview

Domain-Driven Design (DDD) helps organize React applications around business domains. This guide covers feature-based organization, bounded contexts, domain models, and separation of concerns.

**Target Audience**: Developers building complex React applications with multiple business domains, particularly Islamic finance platforms with distinct domains like Zakat, Donations, Murabaha, and Waqf.

**React Version**: React 18.2+ with TypeScript 5+

## Feature-Based Organization

### Domain Structure

```
src/
├── features/
│   ├── zakat/
│   │   ├── components/
│   │   │   ├── ZakatCalculator.tsx
│   │   │   ├── ZakatHistory.tsx
│   │   │   └── index.ts
│   │   ├── hooks/
│   │   │   ├── useZakatCalculation.ts
│   │   │   └── index.ts
│   │   ├── api/
│   │   │   └── zakatApi.ts
│   │   ├── types/
│   │   │   └── index.ts
│   │   └── utils/
│   │       └── calculations.ts
│   │
│   ├── donations/
│   │   ├── components/
│   │   ├── hooks/
│   │   ├── api/
│   │   ├── types/
│   │   └── utils/
│   │
│   └── murabaha/
│       ├── components/
│       ├── hooks/
│       ├── api/
│       ├── types/
│       └── utils/
│
├── shared/
│   ├── components/
│   ├── hooks/
│   ├── types/
│   └── utils/
│
└── core/
    ├── api/
    ├── auth/
    └── config/
```

### Domain Models

```typescript
// features/zakat/types/index.ts

// Value Objects
export class Money {
  private constructor(
    private readonly amount: number,
    private readonly currency: string,
  ) {
    if (amount < 0) {
      throw new Error("Amount cannot be negative");
    }
  }

  static create(amount: number, currency: string): Money {
    return new Money(amount, currency);
  }

  add(other: Money): Money {
    if (this.currency !== other.currency) {
      throw new Error("Cannot add different currencies");
    }
    return new Money(this.amount + other.amount, this.currency);
  }

  multiply(factor: number): Money {
    return new Money(this.amount * factor, this.currency);
  }

  isGreaterThan(other: Money): boolean {
    if (this.currency !== other.currency) {
      throw new Error("Cannot compare different currencies");
    }
    return this.amount > other.amount;
  }

  toString(): string {
    return `${this.currency} ${this.amount.toFixed(2)}`;
  }
}

// Entities
export interface ZakatCalculation {
  id: string;
  userId: string;
  assets: Asset[];
  nisabThreshold: Money;
  totalWealth: Money;
  zakatDue: Money;
  isEligible: boolean;
  calculatedAt: Date;
}

export interface Asset {
  id: string;
  type: AssetType;
  description: string;
  value: Money;
}

export type AssetType = "CASH" | "GOLD" | "SILVER" | "INVESTMENT" | "PROPERTY";
```

### Domain Services

```typescript
// features/zakat/services/zakatCalculationService.ts

import { Money } from "../types";

export class ZakatCalculationService {
  private readonly ZAKAT_RATE = 0.025; // 2.5%

  calculate(assets: Asset[], nisabThreshold: Money): ZakatCalculation {
    // Calculate total wealth
    const totalWealth = assets.reduce((sum, asset) => sum.add(asset.value), Money.create(0, nisabThreshold.currency));

    // Check eligibility
    const isEligible = totalWealth.isGreaterThan(nisabThreshold);

    // Calculate zakat
    const zakatDue = isEligible ? totalWealth.multiply(this.ZAKAT_RATE) : Money.create(0, nisabThreshold.currency);

    return {
      id: crypto.randomUUID(),
      userId: "",
      assets,
      nisabThreshold,
      totalWealth,
      zakatDue,
      isEligible,
      calculatedAt: new Date(),
    };
  }
}
```

### Repository Pattern

```typescript
// features/donations/repositories/donationRepository.ts

export interface DonationRepository {
  findAll(): Promise<Donation[]>;
  findById(id: string): Promise<Donation | null>;
  create(donation: NewDonation): Promise<Donation>;
  update(id: string, updates: Partial<Donation>): Promise<Donation>;
  delete(id: string): Promise<void>;
}

// Implementation
export class ApiDonationRepository implements DonationRepository {
  constructor(private apiClient: ApiClient) {}

  async findAll(): Promise<Donation[]> {
    const response = await this.apiClient.get<Donation[]>("/donations");
    return response.data;
  }

  async findById(id: string): Promise<Donation | null> {
    try {
      const response = await this.apiClient.get<Donation>(`/donations/${id}`);
      return response.data;
    } catch (error) {
      if (error.status === 404) return null;
      throw error;
    }
  }

  async create(donation: NewDonation): Promise<Donation> {
    const response = await this.apiClient.post<Donation>("/donations", donation);
    return response.data;
  }

  async update(id: string, updates: Partial<Donation>): Promise<Donation> {
    const response = await this.apiClient.patch<Donation>(`/donations/${id}`, updates);
    return response.data;
  }

  async delete(id: string): Promise<void> {
    await this.apiClient.delete(`/donations/${id}`);
  }
}
```

### Bounded Context Integration

```typescript
// Zakat context
export const ZakatProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const calculationService = new ZakatCalculationService();
  const repository = new ZakatRepository();

  return (
    <ZakatContext.Provider value={{ calculationService, repository }}>
      {children}
    </ZakatContext.Provider>
  );
};

// Donation context
export const DonationProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const repository = new ApiDonationRepository(apiClient);

  return (
    <DonationContext.Provider value={{ repository }}>
      {children}
    </DonationContext.Provider>
  );
};

// App with multiple bounded contexts
export const App: React.FC = () => (
  <ZakatProvider>
    <DonationProvider>
      <MurabahaProvider>
        <Router>
          <Routes />
        </Router>
      </MurabahaProvider>
    </DonationProvider>
  </ZakatProvider>
);
```

## Related Documentation

- **[Component Architecture](./ex-so-stli-tsre__component-architecture.md)** - Component patterns
- **[TypeScript](./ex-so-stli-tsre__typescript.md)** - Domain modeling
- **[Best Practices](./ex-so-stli-tsre__best-practices.md)** - Architecture standards

---

**Last Updated**: 2026-01-25
