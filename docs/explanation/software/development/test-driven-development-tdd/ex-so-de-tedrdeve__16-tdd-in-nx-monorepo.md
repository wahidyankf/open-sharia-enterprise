# TDD in Nx Monorepo

## Metadata

- **Parent Topic**: [Test-Driven Development (TDD)](./README.md)
- **Related Files**:
  - [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md)

- **Prerequisites**: Understanding of Nx, monorepo architecture
- **Complexity**: Intermediate

## Table of Contents

- [Overview](#overview)
- [Nx Testing Commands](#nx-testing-commands)
- [Shared Test Utilities in libs/](#shared-test-utilities-in-libs)
- [Testing Apps vs Libraries](#testing-apps-vs-libraries)
- [Test Organization in Monorepo](#test-organization-in-monorepo)
- [Running Affected Tests Only](#running-affected-tests-only)
- [Example: Testing Money Library](#example-testing-money-library)
- [Nx-Specific Testing Patterns](#nx-specific-testing-patterns)
- [CI/CD Integration for Nx](#cicd-integration-for-nx)
- [Summary](#summary)

## Overview

Nx provides powerful tooling for managing tests in a monorepo. This document covers TDD practices specific to Nx workspaces, including running affected tests, sharing test utilities, and organizing tests across apps and libraries.

**Key Benefits of TDD in Nx**:

- **Affected Tests**: Run only tests impacted by changes
- **Shared Test Utilities**: Reuse test builders, fixtures, and helpers
- **Parallel Execution**: Run tests in parallel for faster feedback
- **Dependency Graph**: Understand test impact across projects
- **Cache**: Skip tests that haven't changed

## Nx Testing Commands

**Version Note**: This documentation uses Nx commands compatible with Nx 19+. Some commands have changed in recent versions:

- **Nx 19+**: Use `nx affected -t test` (short syntax) or `nx affected --target=test` (long syntax)
- **Nx 19+**: Use `nx graph --affected` instead of the deprecated `affected:graph` command
- **Nx 18 and earlier**: Commands like `nx affected:graph` and `nx affected:test` were used but are now superseded

Both short (`-t`) and long (`--target=`) flag syntaxes are supported. This documentation shows both formats for clarity.

### Basic Test Commands

```bash
# Run tests for a specific project
nx test my-app

# Run tests for a specific library
nx test ts-money

# Run all tests in workspace
nx run-many --target=test --all

# Run tests with coverage
nx test my-app --coverage

# Run tests in watch mode
nx test my-app --watch

# Run tests with specific configuration
nx test my-app --configuration=ci
```

### Affected Commands

```bash
# Run tests only for affected projects
nx affected --target=test

# Run tests affected by changes on main branch
nx affected --target=test --base=main

# Run tests affected by specific commit range
nx affected --target=test --base=HEAD~1 --head=HEAD

# Run tests affected by uncommitted changes
nx affected --target=test --uncommitted

# Run tests affected by changes in specific files
nx affected --target=test --files=libs/ts-money/src/money.ts
```

### Parallel Execution

```bash
# Run tests in parallel (default: 3 parallel tasks)
nx affected --target=test --parallel=3

# Run tests in parallel with maximum parallelization
nx affected --target=test --parallel=auto

# Run tests sequentially (no parallelization)
nx affected --target=test --parallel=1
```

### Running Multiple Test Types

```bash
# Run unit tests
nx run-many --target=test --all

# Run integration tests
nx run-many --target=test:integration --all

# Run E2E tests
nx run-many --target=e2e --all

# Run all test types for affected projects
nx affected --target=test && \
nx affected --target=test:integration && \
nx affected --target=e2e
```

### Viewing Dependency Graph

```bash
# Open interactive dependency graph
nx graph

# View affected projects (Nx 19+)
nx graph --affected
# OR
nx affected --graph

# View dependency graph for specific project
nx graph --focus=ts-money
```

**Note**: The `nx affected:graph` command was removed in Nx 19. Use `nx graph --affected` or `nx affected --graph` instead.

## Shared Test Utilities in libs/

### Creating Shared Test Library

```bash
# Generate shared test utilities library
nx generate @nx/js:library ts-test-utils --directory=libs/ts-test-utils

# Structure:
# libs/
#   ts-test-utils/
#     src/
#       index.ts
#       builders/              # Test data builders
#         money-builder.ts
#         tax-assessment-builder.ts
#       fixtures/              # Test fixtures
#         money-fixtures.ts
#         tax-fixtures.ts
#       helpers/               # Test helpers
#         test-database.ts
#         test-assertions.ts
#       mocks/                 # Mock implementations
#         mock-repository.ts
```

### Example: Shared Test Builders

```typescript
// libs/ts-test-utils/src/builders/money-builder.ts
import { Money } from "@open-compliance-enterprise/ts-money";

export class MoneyBuilder {
  private amount: number = 0;
  private currency: string = "USD";

  withAmount(amount: number): this {
    this.amount = amount;
    return this;
  }

  withCurrency(currency: string): this {
    this.currency = currency;
    return this;
  }

  build(): Money {
    return Money.fromAmount(this.amount, this.currency);
  }

  // Convenience methods
  static usd(amount: number): Money {
    return new MoneyBuilder().withAmount(amount).withCurrency("USD").build();
  }

  static eur(amount: number): Money {
    return new MoneyBuilder().withAmount(amount).withCurrency("EUR").build();
  }

  static zero(currency: string = "USD"): Money {
    return new MoneyBuilder().withAmount(0).withCurrency(currency).build();
  }
}

// Usage in tests across multiple projects
import { MoneyBuilder } from "@open-compliance-enterprise/ts-test-utils";

describe("TaxCalculator", () => {
  it("calculates Tax correctly", () => {
    const wealth = MoneyBuilder.usd(10000);
    const threshold = MoneyBuilder.usd(2000);

    const calculator = new TaxCalculator();
    const tax = calculator.calculateTax(wealth, threshold);

    expect(tax.amount).toBe(250);
  });
});
```

### Example: Shared Test Fixtures

```typescript
// libs/ts-test-utils/src/fixtures/tax-fixtures.ts
import { Money } from "@open-compliance-enterprise/ts-money";
import { TaxAssessment } from "@open-compliance-enterprise/ts-tax";

export const taxFixtures = {
  wealthAboveThreshold: {
    cash: Money.fromAmount(10000, "USD"),
    gold: Money.fromAmount(5000, "USD"),
    investments: Money.fromAmount(3000, "USD"),
    totalWealth: Money.fromAmount(18000, "USD"),
  },

  wealthBelowThreshold: {
    cash: Money.fromAmount(500, "USD"),
    gold: Money.fromAmount(300, "USD"),
    investments: Money.fromAmount(0, "USD"),
    totalWealth: Money.fromAmount(800, "USD"),
  },

  incomeThresholds: {
    usd: Money.fromAmount(2000, "USD"),
    eur: Money.fromAmount(1800, "EUR"),
    gbp: Money.fromAmount(1600, "GBP"),
  },

  taxAssessments: {
    typical: {
      id: "tax-001",
      userId: "user-123",
      wealth: Money.fromAmount(10000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
      taxDue: Money.fromAmount(250, "USD"),
      assessmentDate: new Date("2024-01-01"),
    },

    noTaxDue: {
      id: "tax-002",
      userId: "user-456",
      wealth: Money.fromAmount(1000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
      taxDue: Money.fromAmount(0, "USD"),
      assessmentDate: new Date("2024-01-01"),
    },
  },
};

// Usage in tests
import { taxFixtures } from "@open-compliance-enterprise/ts-test-utils";

describe("TaxRepository", () => {
  it("saves Tax assessment", async () => {
    const repository = new TaxRepository();
    const assessment = taxFixtures.taxAssessments.typical;

    await repository.save(assessment);

    const retrieved = await repository.findById(assessment.id);
    expect(retrieved).toEqual(assessment);
  });
});
```

### Example: Shared Test Helpers

```typescript
// libs/ts-test-utils/src/helpers/test-database.ts
import { PostgreSqlContainer, StartedPostgreSqlContainer } from '@testcontainers/postgresql';
import { DataSource } from 'typeorm';

export class TestDatabase {
  private static container: StartedPostgreSqlContainer;
  private static dataSource: DataSource;

  static async start(): Promise<DataSource> {
    if (!TestDatabase.container) {
      TestDatabase.container = await new PostgreSqlContainer('postgres:15')
        .withDatabase('test')
        .withUsername('test')
        .withPassword('test')
        .start();

      TestDatabase.dataSource = new DataSource({
        type: 'postgres',
        host: TestDatabase.container.getHost(),
        port: TestDatabase.container.getPort(),
        username: TestDatabase.container.getUsername(),
        password: TestDatabase.container.getPassword(),
        database: TestDatabase.container.getDatabase(),
        entities: ['**/*.entity.ts'],
        synchronize: true
      });

      await TestDatabase.dataSource.initialize();
    }

    return TestDatabase.dataSource;
  }

  static async stop(): Promise<void> {
    if (TestDatabase.dataSource) {
      await TestDatabase.dataSource.destroy();
    }

    if (TestDatabase.container) {
      await TestDatabase.container.stop();
    }
  }

  static async reset(): Promise<void> {
    if (TestDatabase.dataSource) {
      const entities = TestDatabase.dataSource.entityMetadatas;
      for (const entity of entities) {
        const repository = TestDatabase.dataSource.getRepository(entity.name);
        await repository.clear();
      }
    }
  }
}

// Usage in integration tests
import { TestDatabase } from '@open-compliance-enterprise/ts-test-utils';

describe('TaxRepository Integration Tests', () => {
  let dataSource: DataSource;

  beforeAll(async () => {
    dataSource = await TestDatabase.start();
  });

  afterEach(async () => {
    await TestDatabase.reset(); // Clean data between tests
  });

  afterAll(async () => {
    await TestDatabase.stop();
  });

  it('saves and retrieves Tax assessment', async () => {
    const repository = new TaxRepository(dataSource);
    const assessment = /* ... */;

    await repository.save(assessment);
    const retrieved = await repository.findById(assessment.id);

    expect(retrieved).toEqual(assessment);
  });
});
```

### Example: Shared Mock Implementations

```typescript
// libs/ts-test-utils/src/mocks/mock-repository.ts
export class InMemoryRepository<T extends { id: string }> {
  private items: Map<string, T> = new Map();

  async save(item: T): Promise<void> {
    this.items.set(item.id, item);
  }

  async findById(id: string): Promise<T | null> {
    return this.items.get(id) ?? null;
  }

  async findAll(): Promise<T[]> {
    return Array.from(this.items.values());
  }

  async delete(id: string): Promise<void> {
    this.items.delete(id);
  }

  async clear(): Promise<void> {
    this.items.clear();
  }
}

// Usage in tests
import { InMemoryRepository } from "@open-compliance-enterprise/ts-test-utils";

describe("TaxService", () => {
  it("calculates and saves Tax", async () => {
    const repository = new InMemoryRepository<TaxAssessment>();
    const service = new TaxService(repository);

    const result = await service.calculateTax({
      wealth: Money.fromAmount(10000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    });

    const saved = await repository.findById(result.id);
    expect(saved).toEqual(result);
  });
});
```

## Testing Apps vs Libraries

### Library Testing Strategy

**Libraries should have high test coverage** (90-100%) because they are reused across multiple apps.

```typescript
// libs/ts-money/src/money.spec.ts
describe("Money Library", () => {
  // Comprehensive tests for all public APIs
  describe("fromAmount", () => {
    it("creates Money from amount and currency", () => {
      const money = Money.fromAmount(100, "USD");

      expect(money.amount).toBe(100);
      expect(money.currency).toBe("USD");
    });

    it("throws error for negative amount", () => {
      expect(() => Money.fromAmount(-100, "USD")).toThrow("Amount cannot be negative");
    });

    it("throws error for invalid currency", () => {
      expect(() => Money.fromAmount(100, "INVALID")).toThrow("Invalid currency code");
    });
  });

  describe("add", () => {
    it("adds two Money instances correctly", () => {
      const money1 = Money.fromAmount(100, "USD");
      const money2 = Money.fromAmount(50, "USD");

      const sum = money1.add(money2);

      expect(sum.amount).toBe(150);
    });

    it("throws error when adding different currencies", () => {
      const usd = Money.fromAmount(100, "USD");
      const eur = Money.fromAmount(50, "EUR");

      expect(() => usd.add(eur)).toThrow("Cannot add different currencies");
    });
  });

  // ... comprehensive tests for all methods
});
```

### App Testing Strategy

**Apps should focus on integration and E2E tests**, relying on well-tested libraries for business logic.

```typescript
// apps/tax-app/src/app/tax.service.spec.ts
describe("Tax App - TaxService", () => {
  // Integration tests focusing on service orchestration
  it("calculates Tax using Money library", async () => {
    const service = new TaxService();

    const result = await service.calculateTax({
      userId: "user-123",
      wealth: { cash: 10000, gold: 5000 },
      currency: "USD",
    });

    expect(result.taxDue.amount).toBe(375); // 2.5% of 15000
  });

  it("saves Tax assessment to repository", async () => {
    const repository = new InMemoryTaxRepository();
    const service = new TaxService(repository);

    const result = await service.calculateTax({
      userId: "user-123",
      wealth: { cash: 10000 },
      currency: "USD",
    });

    const saved = await repository.findById(result.id);
    expect(saved).toEqual(result);
  });
});
```

### Nx Project Configuration

```json
// libs/ts-money/project.json
{
  "name": "ts-money",
  "targets": {
    "test": {
      "executor": "@nx/jest:jest",
      "options": {
        "jestConfig": "libs/ts-money/jest.config.ts",
        "passWithNoTests": false,
        "coverage": true,
        "coverageThreshold": {
          "global": {
            "branches": 90,
            "functions": 90,
            "lines": 90,
            "statements": 90
          }
        }
      }
    }
  }
}

// apps/tax-app/project.json
{
  "name": "tax-app",
  "targets": {
    "test": {
      "executor": "@nx/jest:jest",
      "options": {
        "jestConfig": "apps/tax-app/jest.config.ts",
        "passWithNoTests": false,
        "coverage": true,
        "coverageThreshold": {
          "global": {
            "branches": 70,
            "functions": 70,
            "lines": 70,
            "statements": 70
          }
        }
      }
    },
    "test:integration": {
      "executor": "@nx/jest:jest",
      "options": {
        "jestConfig": "apps/tax-app/jest.integration.config.ts",
        "testPathPattern": ".integration.spec.ts$"
      }
    },
    "e2e": {
      "executor": "@nx/playwright:playwright",
      "options": {
        "config": "apps/tax-app/playwright.config.ts"
      }
    }
  }
}
```

## Test Organization in Monorepo

### Directory Structure

```
open-compliance-enterprise/
├── apps/
│   ├── tax-app/
│   │   ├── src/
│   │   │   ├── app/
│   │   │   │   ├── tax.service.ts
│   │   │   │   ├── tax.service.spec.ts          # Unit tests
│   │   │   │   ├── tax.service.integration.spec.ts  # Integration tests
│   │   │   └── test/
│   │   │       ├── fixtures/                      # App-specific fixtures
│   │   │       └── helpers/                       # App-specific helpers
│   │   └── e2e/
│   │       ├── tax-calculator.spec.ts           # E2E tests
│   │       └── tax-payment.spec.ts
│   │
│   ├── permitted-certification-app/
│   │   ├── src/
│   │   │   ├── app/
│   │   │   │   ├── certification.service.ts
│   │   │   │   └── certification.service.spec.ts
│   │   │   └── test/
│   │   │       └── fixtures/
│   │   └── e2e/
│   │       └── certification-workflow.spec.ts
│
├── libs/
│   ├── ts-money/                                  # Shared library
│   │   ├── src/
│   │   │   ├── money.ts
│   │   │   ├── money.spec.ts                      # Comprehensive unit tests
│   │   │   ├── percentage.ts
│   │   │   └── percentage.spec.ts
│   │   └── README.md
│   │
│   ├── ts-tax/                                  # Domain library
│   │   ├── src/
│   │   │   ├── tax-calculator.ts
│   │   │   ├── tax-calculator.spec.ts
│   │   │   ├── tax-assessment.ts
│   │   │   └── tax-assessment.spec.ts
│   │   └── README.md
│   │
│   ├── ts-test-utils/                             # Shared test utilities
│   │   ├── src/
│   │   │   ├── builders/
│   │   │   │   ├── money-builder.ts
│   │   │   │   └── tax-assessment-builder.ts
│   │   │   ├── fixtures/
│   │   │   │   ├── money-fixtures.ts
│   │   │   │   └── tax-fixtures.ts
│   │   │   ├── helpers/
│   │   │   │   ├── test-database.ts
│   │   │   │   └── test-assertions.ts
│   │   │   └── mocks/
│   │   │       └── mock-repository.ts
│   │   └── README.md
```

### Naming Conventions

```typescript
// Unit tests: *.spec.ts
money.spec.ts;
tax - calculator.spec.ts;

// Integration tests: *.integration.spec.ts
tax - repository.integration.spec.ts;
payment - service.integration.spec.ts;

// E2E tests: *.e2e.spec.ts or in e2e/ directory
tax - calculator.e2e.spec.ts;
apps / tax - app / e2e / user - journey.spec.ts;
```

## Running Affected Tests Only

### How Nx Determines Affected Projects

Nx analyzes the dependency graph and git changes to determine which projects are affected:

```bash
# Projects affected by changes since main branch
nx affected:graph --base=main

# Projects affected by changes in specific files
nx affected:graph --files=libs/ts-money/src/money.ts
```

### Example: Affected Tests Workflow

```typescript
// Scenario: You modified libs/ts-money/src/money.ts

// Nx dependency graph:
// libs/ts-money
//   ↓ (used by)
// libs/ts-tax
//   ↓ (used by)
// apps/tax-app

// Running affected tests will test:
// 1. libs/ts-money (direct change)
// 2. libs/ts-tax (depends on ts-money)
// 3. apps/tax-app (depends on ts-tax)

nx affected --target=test
// Output:
// ✅ ts-money:test
// ✅ ts-tax:test
// ✅ tax-app:test
// ⏭️ permitted-certification-app:test (not affected, skipped)
```

### Affected Tests in CI/CD

```yaml
# .github/workflows/ci.yml
name: CI

on:
  pull_request:
    branches: [main]

jobs:
  test-affected:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0 # Fetch all history for Nx affected

      - name: Derive SHAs for Nx affected
        uses: nrwl/nx-set-shas@v3

      - name: Install dependencies
        run: npm ci

      - name: Run affected tests
        run: npx nx affected --target=test --parallel=3 --coverage

      - name: Run affected integration tests
        run: npx nx affected --target=test:integration --parallel=2

      - name: Run affected E2E tests
        run: npx nx affected --target=e2e --parallel=1
```

### Force Running All Tests

```bash
# Run all tests (ignore affected analysis)
nx run-many --target=test --all

# Run all tests with coverage
nx run-many --target=test --all --coverage

# Run all tests in parallel
nx run-many --target=test --all --parallel=auto
```

## Example: Testing Money Library

### Money Library Structure

```typescript
// libs/ts-money/src/money.ts
export class Money {
  private constructor(
    private readonly _amount: number,
    private readonly _currency: string,
  ) {
    if (_amount < 0) {
      throw new Error("Amount cannot be negative");
    }
    if (!this.isValidCurrency(_currency)) {
      throw new Error("Invalid currency code");
    }
  }

  static fromAmount(amount: number, currency: string): Money {
    return new Money(amount, currency);
  }

  get amount(): number {
    return this._amount;
  }

  get currency(): string {
    return this._currency;
  }

  add(other: Money): Money {
    if (this.currency !== other.currency) {
      throw new Error("Cannot add different currencies");
    }
    return Money.fromAmount(this.amount + other.amount, this.currency);
  }

  subtract(other: Money): Money {
    if (this.currency !== other.currency) {
      throw new Error("Cannot subtract different currencies");
    }
    return Money.fromAmount(this.amount - other.amount, this.currency);
  }

  multiply(multiplier: number): Money {
    return Money.fromAmount(this.amount * multiplier, this.currency);
  }

  divide(divisor: number): Money {
    if (divisor === 0) {
      throw new Error("Cannot divide by zero");
    }
    return Money.fromAmount(this.amount / divisor, this.currency);
  }

  isGreaterThan(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount > other.amount;
  }

  isLessThan(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount < other.amount;
  }

  isEqual(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error("Cannot compare different currencies");
    }
  }

  private isValidCurrency(currency: string): boolean {
    const validCurrencies = ["USD", "EUR", "GBP", "JPY", "SAR", "AED"];
    return validCurrencies.includes(currency);
  }
}
```

### Comprehensive Money Tests (TDD Approach)

```typescript
// libs/ts-money/src/money.spec.ts
describe("Money", () => {
  describe("fromAmount", () => {
    it("creates Money with correct amount and currency", () => {
      const money = Money.fromAmount(100, "USD");

      expect(money.amount).toBe(100);
      expect(money.currency).toBe("USD");
    });

    it("creates Money with zero amount", () => {
      const money = Money.fromAmount(0, "USD");

      expect(money.amount).toBe(0);
    });

    it("creates Money with decimal amount", () => {
      const money = Money.fromAmount(99.99, "USD");

      expect(money.amount).toBe(99.99);
    });

    it("throws error for negative amount", () => {
      expect(() => Money.fromAmount(-100, "USD")).toThrow("Amount cannot be negative");
    });

    it("throws error for invalid currency", () => {
      expect(() => Money.fromAmount(100, "INVALID")).toThrow("Invalid currency code");
    });

    it("accepts all valid currency codes", () => {
      const currencies = ["USD", "EUR", "GBP", "JPY", "SAR", "AED"];

      currencies.forEach((currency) => {
        const money = Money.fromAmount(100, currency);
        expect(money.currency).toBe(currency);
      });
    });
  });

  describe("add", () => {
    it("adds two Money instances with same currency", () => {
      const money1 = Money.fromAmount(100, "USD");
      const money2 = Money.fromAmount(50, "USD");

      const sum = money1.add(money2);

      expect(sum.amount).toBe(150);
      expect(sum.currency).toBe("USD");
    });

    it("adds zero amount", () => {
      const money1 = Money.fromAmount(100, "USD");
      const money2 = Money.fromAmount(0, "USD");

      const sum = money1.add(money2);

      expect(sum.amount).toBe(100);
    });

    it("throws error when adding different currencies", () => {
      const usd = Money.fromAmount(100, "USD");
      const eur = Money.fromAmount(50, "EUR");

      expect(() => usd.add(eur)).toThrow("Cannot add different currencies");
    });
  });

  describe("subtract", () => {
    it("subtracts two Money instances with same currency", () => {
      const money1 = Money.fromAmount(100, "USD");
      const money2 = Money.fromAmount(30, "USD");

      const difference = money1.subtract(money2);

      expect(difference.amount).toBe(70);
      expect(difference.currency).toBe("USD");
    });

    it("throws error when subtracting different currencies", () => {
      const usd = Money.fromAmount(100, "USD");
      const eur = Money.fromAmount(50, "EUR");

      expect(() => usd.subtract(eur)).toThrow("Cannot subtract different currencies");
    });
  });

  describe("multiply", () => {
    it("multiplies Money by integer", () => {
      const money = Money.fromAmount(100, "USD");

      const result = money.multiply(3);

      expect(result.amount).toBe(300);
      expect(result.currency).toBe("USD");
    });

    it("multiplies Money by decimal", () => {
      const money = Money.fromAmount(100, "USD");

      const result = money.multiply(0.025);

      expect(result.amount).toBe(2.5);
    });

    it("multiplies Money by zero", () => {
      const money = Money.fromAmount(100, "USD");

      const result = money.multiply(0);

      expect(result.amount).toBe(0);
    });
  });

  describe("divide", () => {
    it("divides Money by integer", () => {
      const money = Money.fromAmount(100, "USD");

      const result = money.divide(4);

      expect(result.amount).toBe(25);
      expect(result.currency).toBe("USD");
    });

    it("divides Money by decimal", () => {
      const money = Money.fromAmount(100, "USD");

      const result = money.divide(0.5);

      expect(result.amount).toBe(200);
    });

    it("throws error when dividing by zero", () => {
      const money = Money.fromAmount(100, "USD");

      expect(() => money.divide(0)).toThrow("Cannot divide by zero");
    });
  });

  describe("comparison methods", () => {
    describe("isGreaterThan", () => {
      it("returns true when amount is greater", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(50, "USD");

        expect(money1.isGreaterThan(money2)).toBe(true);
      });

      it("returns false when amount is less", () => {
        const money1 = Money.fromAmount(50, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isGreaterThan(money2)).toBe(false);
      });

      it("returns false when amounts are equal", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isGreaterThan(money2)).toBe(false);
      });

      it("throws error when comparing different currencies", () => {
        const usd = Money.fromAmount(100, "USD");
        const eur = Money.fromAmount(50, "EUR");

        expect(() => usd.isGreaterThan(eur)).toThrow("Cannot compare different currencies");
      });
    });

    describe("isLessThan", () => {
      it("returns true when amount is less", () => {
        const money1 = Money.fromAmount(50, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isLessThan(money2)).toBe(true);
      });

      it("returns false when amount is greater", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(50, "USD");

        expect(money1.isLessThan(money2)).toBe(false);
      });

      it("returns false when amounts are equal", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isLessThan(money2)).toBe(false);
      });
    });

    describe("isEqual", () => {
      it("returns true when amount and currency match", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isEqual(money2)).toBe(true);
      });

      it("returns false when amounts differ", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(50, "USD");

        expect(money1.isEqual(money2)).toBe(false);
      });

      it("returns false when currencies differ", () => {
        const usd = Money.fromAmount(100, "USD");
        const eur = Money.fromAmount(100, "EUR");

        expect(usd.isEqual(eur)).toBe(false);
      });
    });
  });

  describe("immutability", () => {
    it("does not mutate original Money when adding", () => {
      const original = Money.fromAmount(100, "USD");
      const other = Money.fromAmount(50, "USD");

      const sum = original.add(other);

      expect(original.amount).toBe(100); // Original unchanged
      expect(sum.amount).toBe(150);
    });

    it("does not mutate original Money when multiplying", () => {
      const original = Money.fromAmount(100, "USD");

      const result = original.multiply(2);

      expect(original.amount).toBe(100); // Original unchanged
      expect(result.amount).toBe(200);
    });
  });
});
```

### Using Money in Other Projects

```typescript
// libs/ts-tax/src/tax-calculator.spec.ts
import { Money } from "@open-compliance-enterprise/ts-money";
import { MoneyBuilder } from "@open-compliance-enterprise/ts-test-utils";

describe("TaxCalculator", () => {
  it("uses Money library for calculations", () => {
    const calculator = new TaxCalculator();
    const wealth = MoneyBuilder.usd(10000);
    const threshold = MoneyBuilder.usd(2000);

    const tax = calculator.calculateTax(wealth, threshold);

    expect(tax.amount).toBe(250);
    expect(tax.currency).toBe("USD");
  });
});

// apps/tax-app/src/app/tax.service.spec.ts
import { Money } from "@open-compliance-enterprise/ts-money";

describe("TaxService", () => {
  it("calculates Tax using Money library", async () => {
    const service = new TaxService();

    const result = await service.calculateTax({
      wealth: Money.fromAmount(10000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    });

    expect(result.taxDue).toEqual(Money.fromAmount(250, "USD"));
  });
});
```

## Nx-Specific Testing Patterns

### Pattern 1: Shared Test Configuration

```typescript
// libs/ts-test-utils/src/jest-preset.ts
export const jestPreset = {
  coverageReporters: ["text", "lcov", "html"],
  collectCoverageFrom: ["src/**/*.ts", "!src/**/*.spec.ts", "!src/**/*.integration.spec.ts", "!src/index.ts"],
  testEnvironment: "node",
  transform: {
    "^.+\\.ts$": "ts-jest",
  },
};

// Usage in library jest.config.ts
import { jestPreset } from "@open-compliance-enterprise/ts-test-utils";

export default {
  ...jestPreset,
  displayName: "ts-money",
  testMatch: ["**/*.spec.ts"],
};
```

### Pattern 2: Dependency Injection for Testability

```typescript
// libs/ts-tax/src/tax-service.ts
export class TaxService {
  constructor(
    private calculator: TaxCalculator,
    private repository: TaxRepository,
  ) {}

  async calculateTax(input: TaxInput): Promise<TaxAssessment> {
    const tax = this.calculator.calculateTax(input.wealth, input.threshold);

    const assessment = new TaxAssessment(generateId(), input.userId, input.wealth, input.threshold, tax, new Date());

    await this.repository.save(assessment);

    return assessment;
  }
}

// Testing with dependency injection
describe("TaxService", () => {
  it("uses injected dependencies", async () => {
    const calculator = new TaxCalculator(); // Real
    const repository = new InMemoryRepository(); // Test double

    const service = new TaxService(calculator, repository);

    const result = await service.calculateTax({
      userId: "user-123",
      wealth: Money.fromAmount(10000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    });

    expect(result.taxDue.amount).toBe(250);

    const saved = await repository.findById(result.id);
    expect(saved).toEqual(result);
  });
});
```

### Pattern 3: Feature Flags for Testing

```typescript
// libs/ts-feature-flags/src/feature-flags.ts
export class FeatureFlags {
  constructor(private flags: Record<string, boolean> = {}) {}

  isEnabled(flag: string): boolean {
    return this.flags[flag] ?? false;
  }

  static forTesting(flags: Record<string, boolean>): FeatureFlags {
    return new FeatureFlags(flags);
  }
}

// Testing with feature flags
describe("TaxService", () => {
  it("uses new Tax calculation when feature flag enabled", () => {
    const featureFlags = FeatureFlags.forTesting({
      "new-tax-calculation": true,
    });

    const service = new TaxService(featureFlags);

    const result = service.calculateTax(/* ... */);

    // Verify new calculation logic used
    expect(result).toMatchSnapshot();
  });
});
```

## CI/CD Integration for Nx

### Optimized CI Pipeline

```yaml
# .github/workflows/ci.yml
name: Nx CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          fetch-depth: 0

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: "20"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Derive SHAs for Nx affected
        uses: nrwl/nx-set-shas@v3

      # Run affected tests in parallel
      - name: Run affected unit tests
        run: npx nx affected --target=test --parallel=3 --coverage

      # Run affected integration tests (fewer parallel tasks)
      - name: Run affected integration tests
        run: npx nx affected --target=test:integration --parallel=2

      # Run affected E2E tests sequentially
      - name: Run affected E2E tests
        run: npx nx affected --target=e2e --parallel=1

      # Upload coverage reports
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          directory: ./coverage

      # Check coverage thresholds
      - name: Check coverage
        run: npx nx affected --target=test --coverage --coverageReporters=text-summary
```

### Nx Cloud Integration (Optional)

```yaml
# nx.json
{
  "tasksRunnerOptions":
    {
      "default":
        {
          "runner": "@nrwl/nx-cloud",
          "options": { "cacheableOperations": ["test", "lint", "build"], "accessToken": "YOUR_NX_CLOUD_TOKEN" },
        },
    },
}
```

## Summary

**Key Takeaways**:

1. **Nx Testing Commands**: Use `nx test`, `nx affected`, and `nx run-many` for targeted test execution
2. **Shared Test Utilities**: Create `ts-test-utils` library for builders, fixtures, and helpers
3. **Testing Strategy**: Libraries need high coverage (90-100%), apps focus on integration/E2E
4. **Affected Tests**: Run only tests impacted by changes for faster feedback
5. **Test Organization**: Co-locate tests with source code, use naming conventions
6. **Dependency Injection**: Enable testability by injecting dependencies
7. **CI/CD Optimization**: Use Nx affected commands in CI for efficient testing

**Next Steps**:

- Review [FAQ](./ex-so-de-tedrdeve__17-faq.md) for common TDD questions
- Explore [Templates](./templates/) for standardized test structures

**Related Resources**:

- [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md)
