# Test-Driven Development: Test Data Builders

## Overview

Test data builders are patterns for constructing test fixtures—the objects and data needed to set up test scenarios. Rather than repeating complex object construction in every test, builders encapsulate creation logic in reusable, expressive functions.

The Builder pattern and Object Mother pattern are the two primary approaches. Builders provide fluent, chainable APIs for incremental construction, while Object Mothers provide factory functions for common scenarios. Both patterns reduce duplication, improve test readability, and make tests resilient to domain model changes.

Mastering test data builders is essential for maintaining large test suites. They transform brittle, verbose test setup into expressive, maintainable code.

## Core Principles

Test data builders embody foundational principles from this repository's [software engineering principles](../../../../../governance/principles/software-engineering/):

**[Immutability Over Mutability](../../../../../governance/principles/software-engineering/immutability.md)**: Builders create immutable test fixtures. Each builder method returns a new builder instance rather than mutating the existing one. This ensures test data remains consistent and predictable—tests cannot accidentally modify shared fixtures.

**[Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md)**: Builders provide deterministic test data. The same builder configuration always produces identical fixtures, enabling reproducible test execution across environments and team members. Sensible defaults eliminate "works on my machine" test failures.

These principles make test data builders reliable infrastructure for maintainable test suites. Immutable fixtures prevent test interference, while reproducible data ensures consistent results.

## Why Test Data Builders Matter

### The Problem: Verbose Test Setup

```typescript
// BAD: Repeated construction in every test
describe("LoanContractService", () => {
  it("should calculate monthly installment", () => {
    const contract = {
      id: "CONTRACT-001",
      customerId: "CUST-001",
      assetDescription: "Toyota Camry 2024",
      assetPrice: { amount: 50000, currency: "USD" },
      markup: { amount: 2500, currency: "USD" },
      termMonths: 12,
      startDate: new Date("2024-01-01"),
      status: "ACTIVE",
      installments: [],
      createdAt: new Date("2024-01-01"),
      updatedAt: new Date("2024-01-01"),
    };

    const installment = service.calculateMonthlyInstallment(contract);
    expect(installment.amount).toBeCloseTo(4375, 2);
  });

  it("should validate markup percentage", () => {
    const contract = {
      id: "CONTRACT-002",
      customerId: "CUST-002",
      assetDescription: "Honda Accord 2024",
      assetPrice: { amount: 40000, currency: "USD" },
      markup: { amount: 5000, currency: "USD" }, // 12.5% - invalid
      termMonths: 24,
      startDate: new Date("2024-02-01"),
      status: "PENDING",
      installments: [],
      createdAt: new Date("2024-02-01"),
      updatedAt: new Date("2024-02-01"),
    };

    expect(() => service.validateContract(contract)).toThrow("Markup exceeds 10%");
  });
});
```

**Problems:**

- Duplication of construction logic
- Changes to `LoanContract` break all tests
- Hard to identify what matters in each test (signal vs noise)
- Error-prone (easy to forget required fields)

### The Solution: Test Data Builders

```typescript
// GOOD: Builder pattern
describe("LoanContractService", () => {
  it("should calculate monthly installment", () => {
    const contract = buildLoanContract()
      .withAssetPrice(Money.fromUSD(50000))
      .withMarkup(Money.fromUSD(2500))
      .withTermMonths(12)
      .build();

    const installment = service.calculateMonthlyInstallment(contract);
    expect(installment.amount).toBeCloseTo(4375, 2);
  });

  it("should validate markup percentage", () => {
    const contract = buildLoanContract()
      .withAssetPrice(Money.fromUSD(40000))
      .withMarkup(Money.fromUSD(5000)) // 12.5% - invalid
      .build();

    expect(() => service.validateContract(contract)).toThrow("Markup exceeds 10%");
  });
});
```

**Benefits:**

- Centralized construction logic
- Expressive, readable tests
- Changes to domain model isolated to builder
- Only specify what matters for each test

## Builder Pattern

The Builder pattern provides a fluent API for constructing objects step by step. Each method returns `this` to enable method chaining.

### Basic Builder Implementation

```typescript
class LoanContractBuilder {
  private id: string = "CONTRACT-DEFAULT";
  private customerId: string = "CUST-DEFAULT";
  private assetDescription: string = "Default Asset";
  private assetPrice: Money = Money.fromUSD(10000);
  private markup: Money = Money.fromUSD(500);
  private termMonths: number = 12;
  private startDate: Date = new Date("2024-01-01");
  private status: ContractStatus = "PENDING";

  withId(id: string): this {
    this.id = id;
    return this;
  }

  withCustomerId(customerId: string): this {
    this.customerId = customerId;
    return this;
  }

  withAssetPrice(price: Money): this {
    this.assetPrice = price;
    return this;
  }

  withMarkup(markup: Money): this {
    this.markup = markup;
    return this;
  }

  withTermMonths(months: number): this {
    this.termMonths = months;
    return this;
  }

  withStartDate(date: Date): this {
    this.startDate = date;
    return this;
  }

  withStatus(status: ContractStatus): this {
    this.status = status;
    return this;
  }

  build(): LoanContract {
    return {
      id: this.id,
      customerId: this.customerId,
      assetDescription: this.assetDescription,
      assetPrice: this.assetPrice,
      markup: this.markup,
      termMonths: this.termMonths,
      startDate: this.startDate,
      status: this.status,
      installments: [],
      createdAt: new Date(),
      updatedAt: new Date(),
    };
  }
}

// Factory function for convenience
function buildLoanContract(): LoanContractBuilder {
  return new LoanContractBuilder();
}
```

### Usage Examples

```typescript
describe("LoanContractService", () => {
  it("should create contract with defaults", () => {
    const contract = buildLoanContract().build();

    expect(contract.assetPrice.equals(Money.fromUSD(10000))).toBe(true);
    expect(contract.termMonths).toBe(12);
  });

  it("should create contract with custom values", () => {
    const contract = buildLoanContract()
      .withAssetPrice(Money.fromUSD(75000))
      .withMarkup(Money.fromUSD(3750))
      .withTermMonths(24)
      .build();

    expect(contract.assetPrice.equals(Money.fromUSD(75000))).toBe(true);
    expect(contract.termMonths).toBe(24);
  });

  it("should create active contract", () => {
    const contract = buildLoanContract().withStatus("ACTIVE").withStartDate(new Date("2024-03-01")).build();

    expect(contract.status).toBe("ACTIVE");
  });
});
```

## Object Mother Pattern

The Object Mother pattern provides factory functions for common test scenarios. Unlike builders (which construct incrementally), Object Mothers return fully-configured objects for specific use cases.

### Basic Object Mother Implementation

```typescript
// Object Mother functions
function buildDefaultLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  return {
    id: "CONTRACT-001",
    customerId: "CUST-001",
    assetDescription: "Default Asset",
    assetPrice: Money.fromUSD(10000),
    markup: Money.fromUSD(500),
    termMonths: 12,
    startDate: new Date("2024-01-01"),
    status: "PENDING",
    installments: [],
    createdAt: new Date("2024-01-01"),
    updatedAt: new Date("2024-01-01"),
    ...overrides,
  };
}

function buildActiveLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  return buildDefaultLoanContract({
    status: "ACTIVE",
    startDate: new Date("2024-01-01"),
    ...overrides,
  });
}

function buildCompletedLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  return buildDefaultLoanContract({
    status: "COMPLETED",
    startDate: new Date("2023-01-01"),
    installments: [
      // All installments paid
    ],
    ...overrides,
  });
}

function buildInvalidLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  return buildDefaultLoanContract({
    markup: Money.fromUSD(1500), // 15% - exceeds limit
    ...overrides,
  });
}
```

### Usage Examples

```typescript
describe("LoanContractService", () => {
  it("should approve valid contract", () => {
    const contract = buildDefaultLoanContract();

    const result = service.validateContract(contract);

    expect(result.valid).toBe(true);
  });

  it("should reject contract with excessive markup", () => {
    const contract = buildInvalidLoanContract();

    expect(() => service.validateContract(contract)).toThrow("Markup exceeds 10%");
  });

  it("should calculate remaining balance for active contract", () => {
    const contract = buildActiveLoanContract({
      assetPrice: Money.fromUSD(50000),
      markup: Money.fromUSD(2500),
    });

    const remaining = service.calculateRemainingBalance(contract);

    expect(remaining.amount).toBeGreaterThan(0);
  });
});
```

## Builder vs Object Mother: When to Use Each

```mermaid
flowchart TD
    Start[Need test data?] --> Question1{How many<br/>variations?}

    Question1 -->|Few common<br/>scenarios| ObjectMother[Use Object Mother]
    Question1 -->|Many combinations| Question2{Need incremental<br/>construction?}

    Question2 -->|Yes| Builder[Use Builder]
    Question2 -->|No| Both[Use Both:<br/>Object Mother<br/>+ overrides]

    ObjectMother --> Example1["buildApprovedCertification()<br/>buildRejectedCertification()"]
    Builder --> Example2["buildDonation()<br/>.withBeneficiaries([...])<br/>.withAssets([...])"]
    Both --> Example3["buildDefaultContract({<br/>  markup: Money.fromUSD(2000)<br/>})"]

    style Start fill:#0173B2,stroke:#000,color:#FFFFFF
    style ObjectMother fill:#029E73,stroke:#000,color:#FFFFFF
    style Builder fill:#DE8F05,stroke:#000,color:#000000
    style Both fill:#CC78BC,stroke:#000,color:#FFFFFF
    style Question1 fill:#808080,stroke:#000,color:#000000
    style Question2 fill:#808080,stroke:#000,color:#000000
    style Example1 fill:#808080,stroke:#000,color:#000000
    style Example2 fill:#808080,stroke:#000,color:#000000
    style Example3 fill:#808080,stroke:#000,color:#000000
```

**Decision Matrix:**

| Scenario                  | Pattern                          | Example                                                      |
| ------------------------- | -------------------------------- | ------------------------------------------------------------ |
| Few well-defined states   | **Object Mother**                | `buildApprovedClaim()`, `buildRejectedClaim()`               |
| Many field combinations   | **Builder**                      | `buildDonation().withBeneficiaries([...]).withAssets([...])` |
| Simple overrides          | **Object Mother with overrides** | `buildContract({ markup: Money.fromUSD(1000) })`             |
| Complex nested objects    | **Builder**                      | Fluent API for step-by-step construction                     |
| Domain-specific scenarios | **Object Mother**                | `buildPermittedCertifiedProduct()`                           |

## Combining Builders and Object Mothers

The most powerful approach combines both patterns: use Object Mothers for common scenarios, and builders when more flexibility is needed.

```typescript
// Object Mothers for common scenarios
function buildPendingTaxCalculation(overrides?: Partial<TaxCalculation>): TaxCalculation {
  return {
    id: "CALC-001",
    userId: "USER-001",
    wealth: Money.fromGold(100, "grams"),
    threshold: Money.fromGold(85, "grams"),
    taxDue: Money.fromGold(2.5, "grams"),
    hawlCompleted: true,
    calculatedAt: new Date("2024-01-15"),
    status: "PENDING",
    ...overrides,
  };
}

function buildPaidTaxCalculation(overrides?: Partial<TaxCalculation>): TaxCalculation {
  return buildPendingTaxCalculation({
    status: "PAID",
    paidAt: new Date("2024-01-20"),
    ...overrides,
  });
}

// Builder for complex scenarios
class TaxCalculationBuilder {
  private data: Partial<TaxCalculation> = {
    id: "CALC-DEFAULT",
    userId: "USER-DEFAULT",
    wealth: Money.fromGold(100, "grams"),
    threshold: Money.fromGold(85, "grams"),
    taxDue: Money.fromGold(2.5, "grams"),
    hawlCompleted: true,
    status: "PENDING",
  };

  withWealth(wealth: Money): this {
    this.data.wealth = wealth;
    this.data.taxDue = wealth.multiply(0.025);
    return this;
  }

  withThreshold(threshold: Money): this {
    this.data.threshold = threshold;
    return this;
  }

  notCompletedHawl(): this {
    this.data.hawlCompleted = false;
    this.data.taxDue = Money.zero("gold");
    return this;
  }

  asPaid(paidAt?: Date): this {
    this.data.status = "PAID";
    this.data.paidAt = paidAt || new Date();
    return this;
  }

  build(): TaxCalculation {
    return {
      calculatedAt: new Date(),
      ...this.data,
    } as TaxCalculation;
  }
}

function buildTaxCalculation(): TaxCalculationBuilder {
  return new TaxCalculationBuilder();
}

// Usage: Choose the right tool for the job
describe("TaxReportingService", () => {
  it("should generate report for paid calculations", () => {
    const calc1 = buildPaidTaxCalculation(); // Object Mother - simple
    const calc2 = buildPaidTaxCalculation({ wealth: Money.fromGold(200, "grams") });

    const report = service.generateReport([calc1, calc2]);

    expect(report.totalPaid.equals(Money.fromGold(7.5, "grams"))).toBe(true);
  });

  it("should exclude incomplete Hawl from report", () => {
    const completeHawl = buildPaidTaxCalculation();
    const incompleteHawl = buildTaxCalculation() // Builder - complex scenario
      .withWealth(Money.fromGold(500, "grams"))
      .notCompletedHawl()
      .build();

    const report = service.generateReport([completeHawl, incompleteHawl]);

    expect(report.calculationsIncluded).toBe(1); // Only complete Hawl
  });
});
```

## Advanced Builder Patterns

### 1. Builder with Validation

```typescript
class DonationBuilder {
  private beneficiaries: Beneficiary[] = [];
  private assets: DonationAsset[] = [];
  private custodian?: string;

  withBeneficiaries(beneficiaries: Beneficiary[]): this {
    this.beneficiaries = beneficiaries;
    return this;
  }

  withAssets(assets: DonationAsset[]): this {
    this.assets = assets;
    return this;
  }

  withCustodian(custodian: string): this {
    this.custodian = custodian;
    return this;
  }

  build(): Donation {
    // Validation before build
    if (this.beneficiaries.length === 0) {
      throw new Error("Donation must have at least one beneficiary");
    }

    if (this.assets.length === 0) {
      throw new Error("Donation must have at least one asset");
    }

    const totalShares = this.beneficiaries.reduce((sum, b) => sum + b.share, 0);
    if (Math.abs(totalShares - 100) > 0.01) {
      throw new Error(`Beneficiary shares must sum to 100%, got ${totalShares}%`);
    }

    return {
      id: generateDonationId(),
      beneficiaries: this.beneficiaries,
      assets: this.assets,
      custodian: this.custodian || "DEFAULT_CUSTODIAN",
      createdAt: new Date(),
      status: "ACTIVE",
    };
  }
}
```

### 2. Builder with Nested Builders

```typescript
class TakafulPolicyBuilder {
  private policyholder?: Policyholder;
  private coverage: Coverage[] = [];
  private premium?: Money;

  withPolicyholder(configure: (builder: PolicyholderBuilder) => PolicyholderBuilder): this {
    const builder = new PolicyholderBuilder();
    this.policyholder = configure(builder).build();
    return this;
  }

  withCoverage(configure: (builder: CoverageBuilder) => CoverageBuilder): this {
    const builder = new CoverageBuilder();
    this.coverage.push(configure(builder).build());
    return this;
  }

  withPremium(premium: Money): this {
    this.premium = premium;
    return this;
  }

  build(): TakafulPolicy {
    if (!this.policyholder) {
      throw new Error("Policy must have a policyholder");
    }

    return {
      id: generatePolicyId(),
      policyholder: this.policyholder,
      coverage: this.coverage,
      premium: this.premium || Money.fromSAR(1000),
      status: "ACTIVE",
      effectiveDate: new Date(),
    };
  }
}

// Usage
const policy = buildTakafulPolicy()
  .withPolicyholder((builder) => builder.withName("Ahmad Hassan").withAge(35).withOccupation("Engineer"))
  .withCoverage((builder) => builder.withType("MEDICAL").withLimit(Money.fromSAR(500000)))
  .withCoverage((builder) => builder.withType("LIFE").withLimit(Money.fromSAR(1000000)))
  .build();
```

### 3. Builder with Random Values (for Fuzz Testing)

```typescript
import { faker } from "@faker-js/faker";

class RandomTaxCalculationBuilder {
  private wealth?: Money;
  private threshold?: Money;

  withRandomWealth(min: number = 0, max: number = 1000000): this {
    const amount = faker.number.float({ min, max, precision: 0.01 });
    this.wealth = Money.fromGold(amount, "grams");
    return this;
  }

  withRandomThreshold(): this {
    this.threshold = Money.fromGold(85, "grams"); // Fixed threshold
    return this;
  }

  build(): TaxCalculation {
    const wealth = this.wealth || Money.fromGold(faker.number.float({ min: 0, max: 10000, precision: 0.01 }), "grams");
    const threshold = this.threshold || Money.fromGold(85, "grams");

    return {
      id: faker.string.uuid(),
      userId: faker.string.uuid(),
      wealth,
      threshold,
      taxDue: wealth.gte(threshold) ? wealth.multiply(0.025) : Money.zero("gold"),
      hawlCompleted: faker.datatype.boolean(),
      calculatedAt: faker.date.recent(),
      status: faker.helpers.arrayElement(["PENDING", "PAID", "CANCELLED"]),
    };
  }
}

// Usage in property-based tests
it("should always calculate non-negative tax", () => {
  for (let i = 0; i < 100; i++) {
    const calculation = new RandomTaxCalculationBuilder().withRandomWealth().withRandomThreshold().build();

    expect(calculation.taxDue.amount).toBeGreaterThanOrEqual(0);
  }
});
```

## Test Data Builders for Islamic Finance Domain

### Permitted Certification Application Builder

```typescript
function buildPermittedCertificationApplication(
  overrides?: Partial<PermittedCertificationApplication>,
): PermittedCertificationApplication {
  return {
    id: "APP-001",
    productName: "Permitted Chicken Sausage",
    manufacturer: "Permitted Foods Ltd",
    ingredients: [
      { name: "chicken", source: "permitted-certified-farm", percentage: 80 },
      { name: "spices", source: "natural", percentage: 15 },
      { name: "water", source: "municipal", percentage: 5 },
    ],
    certificationBody: "Islamic Food Council",
    submittedAt: new Date("2024-01-15"),
    status: "PENDING",
    reviewedBy: undefined,
    approvedAt: undefined,
    ...overrides,
  };
}

function buildApprovedPermittedCertification(
  overrides?: Partial<PermittedCertificationApplication>,
): PermittedCertificationApplication {
  return buildPermittedCertificationApplication({
    status: "APPROVED",
    reviewedBy: "REVIEWER-001",
    approvedAt: new Date("2024-01-20"),
    ...overrides,
  });
}

function buildRejectedPermittedCertification(
  reason: string,
  overrides?: Partial<PermittedCertificationApplication>,
): PermittedCertificationApplication {
  return buildPermittedCertificationApplication({
    status: "REJECTED",
    reviewedBy: "REVIEWER-001",
    rejectionReason: reason,
    rejectedAt: new Date("2024-01-20"),
    ingredients: [
      { name: "pork", source: "farm", percentage: 50 }, // Non-permitted
      { name: "spices", source: "natural", percentage: 50 },
    ],
    ...overrides,
  });
}
```

### Sukuk Investment Builder

```typescript
class SukukInvestmentBuilder {
  private principal: Money = Money.fromUSD(10000);
  private rate: number = 0.05; // 5% profit rate
  private termYears: number = 5;
  private assetBacked: boolean = true;
  private sukukType: SukukType = "IJARAH";

  withPrincipal(amount: Money): this {
    this.principal = amount;
    return this;
  }

  withProfitRate(rate: number): this {
    this.rate = rate;
    return this;
  }

  withTermYears(years: number): this {
    this.termYears = years;
    return this;
  }

  asIjarah(): this {
    this.sukukType = "IJARAH";
    this.assetBacked = true;
    return this;
  }

  asMusharakah(): this {
    this.sukukType = "MUSHARAKAH";
    this.assetBacked = false;
    return this;
  }

  asMudarabah(): this {
    this.sukukType = "MUDARABAH";
    this.assetBacked = false;
    return this;
  }

  build(): SukukInvestment {
    return {
      id: generateSukukId(),
      principal: this.principal,
      profitRate: this.rate,
      termYears: this.termYears,
      sukukType: this.sukukType,
      assetBacked: this.assetBacked,
      issueDate: new Date(),
      maturityDate: addYears(new Date(), this.termYears),
      status: "ACTIVE",
    };
  }
}

function buildSukukInvestment(): SukukInvestmentBuilder {
  return new SukukInvestmentBuilder();
}

// Usage
describe("SukukInvestmentService", () => {
  it("should calculate profit for Ijarah sukuk", () => {
    const sukuk = buildSukukInvestment()
      .withPrincipal(Money.fromUSD(100000))
      .withProfitRate(0.06)
      .withTermYears(3)
      .asIjarah()
      .build();

    const profit = service.calculateAnnualProfit(sukuk);

    expect(profit.equals(Money.fromUSD(6000))).toBe(true);
  });
});
```

## Best Practices

### 1. Sensible Defaults

Builders and Object Mothers should provide realistic defaults that work for most tests:

```typescript
// GOOD: Sensible defaults
function buildLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  return {
    assetPrice: Money.fromUSD(10000), // Reasonable default
    markup: Money.fromUSD(500), // 5% - valid
    termMonths: 12, // Common term
    status: "PENDING", // Typical starting state
    ...overrides,
  };
}

// BAD: Unrealistic defaults
function buildLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  return {
    assetPrice: Money.fromUSD(0), // Unrealistic
    markup: Money.fromUSD(-100), // Invalid
    termMonths: 0, // Invalid
    status: "UNKNOWN", // Not a real status
    ...overrides,
  };
}
```

### 2. Named Scenarios Over Comments

```typescript
// GOOD: Named functions describe scenario
function buildExpiredTakafulPolicy(): TakafulPolicy {
  return buildTakafulPolicy({
    effectiveDate: new Date("2020-01-01"),
    expiryDate: new Date("2023-12-31"),
    status: "EXPIRED",
  });
}

// BAD: Comment explains unnamed builder
it("should not renew policy", () => {
  // Build policy that expired last year
  const policy = buildTakafulPolicy({
    effectiveDate: new Date("2020-01-01"),
    expiryDate: new Date("2023-12-31"),
    status: "EXPIRED",
  });
});
```

### 3. Keep Builders in Test Directory

```
src/
  domain/
    LoanContract.ts
  __tests__/
    builders/
      LoanContractBuilder.ts
      TakafulPolicyBuilder.ts
      DonationBuilder.ts
    LoanContractService.test.ts
```

### 4. One Builder File Per Domain Object

Organize builders by domain object, not by test file:

```typescript
// src/__tests__/builders/LoanContractBuilder.ts
export function buildLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  // Implementation
}

export function buildActiveLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  // Implementation
}

export function buildCompletedLoanContract(overrides?: Partial<LoanContract>): LoanContract {
  // Implementation
}
```

### 5. Use TypeScript's `Partial<T>` for Overrides

```typescript
// GOOD: Flexible overrides
function buildDonation(overrides?: Partial<Donation>): Donation {
  return {
    id: "DONATION-001",
    beneficiaries: [],
    assets: [],
    ...overrides,
  };
}

// Usage: Only override what you need
const donation = buildDonation({ id: "DONATION-CUSTOM" });
```

## Related Principles

**Software Engineering Principles**:

- [Immutability Over Mutability](../../../../../governance/principles/software-engineering/immutability.md) - Builders create immutable fixtures that prevent test interference
- [Reproducibility First](../../../../../governance/principles/software-engineering/reproducibility.md) - Builders provide deterministic test data across environments
- [Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md) - Sensible defaults reduce complexity, add specificity only when needed

**Related Practices**:

- [Functional Programming](../../../../../governance/development/pattern/functional-programming.md) - Builders for immutable data structures
- [Code Quality](../../../../../governance/development/quality/code.md) - Test code quality standards apply to builders

## Summary

Test data builders transform test setup from verbose, brittle code into expressive, maintainable patterns:

- **Builder Pattern**: Fluent API for incremental construction
- **Object Mother**: Factory functions for common scenarios
- **Hybrid Approach**: Combine both for maximum flexibility

**Key Benefits:**

1. Reduce duplication across test suite
2. Make tests resilient to domain model changes
3. Improve test readability (highlight what matters)
4. Provide sensible defaults
5. Support complex object graphs

**Best Practices:**

- Sensible defaults for common scenarios
- Named scenarios over comments
- Organize builders by domain object
- Use `Partial<T>` for flexible overrides
- Validate in builders when appropriate

Test data builders are essential infrastructure for maintainable test suites. Invest in them early—they pay dividends as the codebase grows.

## Related Documentation

- **[04. Unit Testing Fundamentals](ex-soen-de-tedrdetd__04-unit-testing-fundamentals.md)** - Test structure and organization
- **[06. Testing Patterns](ex-soen-de-tedrdetd__06-testing-patterns.md)** - AAA pattern and test organization
- **[11. TDD and Functional Programming](ex-soen-de-tedrdetd__11-tdd-and-functional-programming.md)** - Builders for functional data structures
- **[12. TDD and DDD](ex-soen-de-tedrdetd__12-tdd-and-ddd.md)** - Builders for aggregates and value objects
