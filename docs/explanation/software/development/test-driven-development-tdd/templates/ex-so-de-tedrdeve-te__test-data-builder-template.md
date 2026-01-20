# Test Data Builder Template

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md)
  - [Integration Test Template](./ex-so-de-tedrdeve-te__integration-test-template.md)
  - [Property-Based Test Template](./ex-so-de-tedrdeve-te__property-based-test-template.md)
- **Use Case**: Creating reusable, fluent test data builders for complex objects
- **Template Size**: ~18 KB
- **Complexity**: Intermediate

## Overview

Test Data Builders use the Builder pattern to create test objects with sensible defaults while allowing easy customization. They improve test readability, reduce duplication, and make tests resilient to changes in object construction.

## When to Use Test Data Builders

**Use Test Data Builders When**:

- Objects have many constructor parameters or properties
- Tests need similar objects with slight variations
- Default values make sense for most tests
- Object creation involves complex setup
- You want to make test intent clear through fluent API
- Reducing test maintenance burden

**Don't Use Test Data Builders For**:

- Simple objects with 1-2 properties (use direct construction)
- One-off test objects with no reuse
- Objects where defaults don't make sense

## Template Structure

```typescript
// File: [entity-name].builder.ts
export class [EntityName]Builder {
  // Default values
  private id: string = "default-id";
  private field1: string = "default-value-1";
  private field2: number = 100;
  private field3: Date = new Date("2024-01-01");

  // Fluent setter methods
  withId(id: string): this {
    this.id = id;
    return this;
  }

  withField1(field1: string): this {
    this.field1 = field1;
    return this;
  }

  withField2(field2: number): this {
    this.field2 = field2;
    return this;
  }

  withField3(field3: Date): this {
    this.field3 = field3;
    return this;
  }

  // Build method
  build(): [EntityName] {
    return new `EntityName`(this.id, this.field1, this.field2, this.field3);
  }

  // Convenience methods for common scenarios
  static defaultEntity(): [EntityName] {
    return new [EntityName]Builder().build();
  }

  static validEntity(): [EntityName] {
    return new [EntityName]Builder().withField1("valid-value").build();
  }
}
```

## Islamic Finance Example: Tax Assessment Builder

### Domain Entity

```typescript
// File: tax-assessment.ts
import { Money } from "./money";

export class TaxAssessment {
  constructor(
    public readonly id: string,
    public readonly donorId: string,
    public readonly wealth: Money,
    public readonly threshold: Money,
    public readonly taxAmount: Money,
    public readonly assessmentDate: Date,
    public readonly status: TaxStatus,
    public readonly notes?: string,
  ) {}

  isPending(): boolean {
    return this.status === TaxStatus.PENDING;
  }

  isPaid(): boolean {
    return this.status === TaxStatus.PAID;
  }

  isTaxDue(): boolean {
    return this.wealth.isGreaterThanOrEqual(this.threshold);
  }
}

export enum TaxStatus {
  PENDING = "PENDING",
  PAID = "PAID",
  CANCELLED = "CANCELLED",
}
```

### Complete Test Data Builder

```typescript
// File: tax-assessment.builder.ts
import { v4 as uuidv4 } from "uuid";
import { TaxAssessment, TaxStatus } from "./tax-assessment";
import { Money } from "./money";

export class TaxAssessmentBuilder {
  // Sensible defaults
  private id: string = uuidv4();
  private donorId: string = "donor-default";
  private wealth: Money = Money.fromAmount(10000, "USD");
  private threshold: Money = Money.fromAmount(2000, "USD");
  private taxAmount: Money = Money.fromAmount(250, "USD"); // 2.5% of 10000
  private assessmentDate: Date = new Date("2024-01-01");
  private status: TaxStatus = TaxStatus.PENDING;
  private notes?: string;

  // Fluent setter methods
  withId(id: string): this {
    this.id = id;
    return this;
  }

  withDonorId(donorId: string): this {
    this.donorId = donorId;
    return this;
  }

  withWealth(wealth: Money): this {
    this.wealth = wealth;
    return this;
  }

  withWealthAmount(amount: number, currency: string = "USD"): this {
    this.wealth = Money.fromAmount(amount, currency);
    return this;
  }

  withThreshold(threshold: Money): this {
    this.threshold = threshold;
    return this;
  }

  withThresholdAmount(amount: number, currency: string = "USD"): this {
    this.threshold = Money.fromAmount(amount, currency);
    return this;
  }

  withTaxAmount(taxAmount: Money): this {
    this.taxAmount = taxAmount;
    return this;
  }

  withTaxAmountValue(amount: number, currency: string = "USD"): this {
    this.taxAmount = Money.fromAmount(amount, currency);
    return this;
  }

  withAssessmentDate(date: Date): this {
    this.assessmentDate = date;
    return this;
  }

  withStatus(status: TaxStatus): this {
    this.status = status;
    return this;
  }

  withNotes(notes: string): this {
    this.notes = notes;
    return this;
  }

  // Build method
  build(): TaxAssessment {
    return new TaxAssessment(
      this.id,
      this.donorId,
      this.wealth,
      this.threshold,
      this.taxAmount,
      this.assessmentDate,
      this.status,
      this.notes,
    );
  }

  // Common scenario builders
  static aValidAssessment(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder();
  }

  static aPendingAssessment(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder().withStatus(TaxStatus.PENDING);
  }

  static aPaidAssessment(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder().withStatus(TaxStatus.PAID).withNotes("Paid via bank transfer");
  }

  static aCancelledAssessment(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder().withStatus(TaxStatus.CANCELLED).withNotes("Cancelled by donor");
  }

  static anAssessmentAboveThreshold(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder()
      .withWealthAmount(10000, "USD")
      .withThresholdAmount(2000, "USD")
      .withTaxAmountValue(250, "USD");
  }

  static anAssessmentBelowThreshold(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder()
      .withWealthAmount(1000, "USD")
      .withThresholdAmount(2000, "USD")
      .withTaxAmountValue(0, "USD");
  }

  static anAssessmentAtThreshold(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder()
      .withWealthAmount(2000, "USD")
      .withThresholdAmount(2000, "USD")
      .withTaxAmountValue(50, "USD");
  }

  static anAssessmentForDonor(donorId: string): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder().withDonorId(donorId);
  }

  static anAssessmentInCurrency(currency: string): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder()
      .withWealthAmount(10000, currency)
      .withThresholdAmount(2000, currency)
      .withTaxAmountValue(250, currency);
  }

  static anAssessmentOnDate(date: Date): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder().withAssessmentDate(date);
  }
}
```

### Using the Builder in Tests

```typescript
// File: tax-assessment.spec.ts
import { TaxAssessmentBuilder } from "./tax-assessment.builder";
import { TaxStatus } from "./tax-assessment";

describe("TaxAssessment", () => {
  describe("isPending", () => {
    it("should return true for pending assessment", () => {
      // Arrange - using builder
      const assessment = TaxAssessmentBuilder.aPendingAssessment().build();

      // Act
      const result = assessment.isPending();

      // Assert
      expect(result).toBe(true);
    });

    it("should return false for paid assessment", () => {
      // Arrange
      const assessment = TaxAssessmentBuilder.aPaidAssessment().build();

      // Act
      const result = assessment.isPending();

      // Assert
      expect(result).toBe(false);
    });
  });

  describe("isTaxDue", () => {
    it("should return true when wealth is above threshold", () => {
      // Arrange
      const assessment = TaxAssessmentBuilder.anAssessmentAboveThreshold().build();

      // Act
      const result = assessment.isTaxDue();

      // Assert
      expect(result).toBe(true);
    });

    it("should return false when wealth is below threshold", () => {
      // Arrange
      const assessment = TaxAssessmentBuilder.anAssessmentBelowThreshold().build();

      // Act
      const result = assessment.isTaxDue();

      // Assert
      expect(result).toBe(false);
    });

    it("should return true when wealth equals threshold", () => {
      // Arrange
      const assessment = TaxAssessmentBuilder.anAssessmentAtThreshold().build();

      // Act
      const result = assessment.isTaxDue();

      // Assert
      expect(result).toBe(true);
    });
  });

  describe("with custom values", () => {
    it("should handle custom donor and status", () => {
      // Arrange - fluent customization
      const assessment = TaxAssessmentBuilder.aValidAssessment()
        .withDonorId("donor-123")
        .withStatus(TaxStatus.PAID)
        .withNotes("Paid in full")
        .build();

      // Assert
      expect(assessment.donorId).toBe("donor-123");
      expect(assessment.status).toBe(TaxStatus.PAID);
      expect(assessment.notes).toBe("Paid in full");
    });

    it("should handle custom amounts in different currency", () => {
      // Arrange
      const assessment = TaxAssessmentBuilder.anAssessmentInCurrency("EUR")
        .withWealthAmount(8000, "EUR")
        .withThresholdAmount(1800, "EUR")
        .withTaxAmountValue(200, "EUR")
        .build();

      // Assert
      expect(assessment.wealth.currency).toBe("EUR");
      expect(assessment.wealth.amount).toBe(8000);
      expect(assessment.taxAmount.amount).toBe(200);
    });
  });
});
```

## Additional Example: Money Builder

```typescript
// File: money.builder.ts
import { Money } from "./money";

export class MoneyBuilder {
  private amount: number = 100;
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

  // Common scenarios
  static usd(amount: number = 100): Money {
    return new MoneyBuilder().withAmount(amount).withCurrency("USD").build();
  }

  static eur(amount: number = 100): Money {
    return new MoneyBuilder().withAmount(amount).withCurrency("EUR").build();
  }

  static sar(amount: number = 100): Money {
    return new MoneyBuilder().withAmount(amount).withCurrency("SAR").build();
  }

  static zero(currency: string = "USD"): Money {
    return new MoneyBuilder().withAmount(0).withCurrency(currency).build();
  }

  static thresholdUsd(): Money {
    return new MoneyBuilder().withAmount(2000).withCurrency("USD").build();
  }

  static wealthAboveThresholdUsd(): Money {
    return new MoneyBuilder().withAmount(10000).withCurrency("USD").build();
  }

  static wealthBelowThresholdUsd(): Money {
    return new MoneyBuilder().withAmount(1000).withCurrency("USD").build();
  }
}

// Usage in tests
describe("TaxCalculator", () => {
  it("should calculate tax for wealth above threshold", () => {
    const wealth = MoneyBuilder.wealthAboveThresholdUsd();
    const threshold = MoneyBuilder.thresholdUsd();

    const tax = calculator.calculateTax(wealth, threshold);

    expect(tax.amount).toBe(250);
  });

  it("should handle EUR currency", () => {
    const wealth = MoneyBuilder.eur(8000);
    const threshold = MoneyBuilder.eur(1800);

    const tax = calculator.calculateTax(wealth, threshold);

    expect(tax.currency).toBe("EUR");
  });
});
```

## Complex Example: Loan Contract Builder

```typescript
// File: loan-contract.ts
import { Money } from "./money";

export class LoanContract {
  constructor(
    public readonly id: string,
    public readonly customerId: string,
    public readonly assetDescription: string,
    public readonly costPrice: Money,
    public readonly profitMargin: number,
    public readonly sellingPrice: Money,
    public readonly installments: number,
    public readonly installmentAmount: Money,
    public readonly startDate: Date,
    public readonly endDate: Date,
    public readonly status: ContractStatus,
    public readonly collateral?: string,
  ) {}

  getTotalProfit(): Money {
    return this.sellingPrice.subtract(this.costPrice);
  }

  isActive(): boolean {
    return this.status === ContractStatus.ACTIVE;
  }
}

export enum ContractStatus {
  DRAFT = "DRAFT",
  ACTIVE = "ACTIVE",
  COMPLETED = "COMPLETED",
  DEFAULTED = "DEFAULTED",
}
```

```typescript
// File: loan-contract.builder.ts
import { v4 as uuidv4 } from "uuid";
import { LoanContract, ContractStatus } from "./loan-contract";
import { Money } from "./money";

export class LoanContractBuilder {
  private id: string = uuidv4();
  private customerId: string = "customer-001";
  private assetDescription: string = "Honda Civic 2024";
  private costPrice: Money = Money.fromAmount(25000, "USD");
  private profitMargin: number = 0.1; // 10%
  private sellingPrice: Money = Money.fromAmount(27500, "USD"); // Cost + 10%
  private installments: number = 12;
  private installmentAmount: Money = Money.fromAmount(2291.67, "USD");
  private startDate: Date = new Date("2024-01-01");
  private endDate: Date = new Date("2024-12-31");
  private status: ContractStatus = ContractStatus.ACTIVE;
  private collateral?: string;

  withId(id: string): this {
    this.id = id;
    return this;
  }

  withCustomerId(customerId: string): this {
    this.customerId = customerId;
    return this;
  }

  withAssetDescription(description: string): this {
    this.assetDescription = description;
    return this;
  }

  withCostPrice(price: Money): this {
    this.costPrice = price;
    return this;
  }

  withProfitMargin(margin: number): this {
    this.profitMargin = margin;
    // Recalculate selling price
    const profitAmount = this.costPrice.multiply(margin);
    this.sellingPrice = this.costPrice.add(profitAmount);
    // Recalculate installment amount
    this.installmentAmount = this.sellingPrice.divide(this.installments);
    return this;
  }

  withSellingPrice(price: Money): this {
    this.sellingPrice = price;
    // Recalculate installment amount
    this.installmentAmount = price.divide(this.installments);
    return this;
  }

  withInstallments(count: number): this {
    this.installments = count;
    // Recalculate installment amount
    this.installmentAmount = this.sellingPrice.divide(count);
    return this;
  }

  withInstallmentAmount(amount: Money): this {
    this.installmentAmount = amount;
    return this;
  }

  withStartDate(date: Date): this {
    this.startDate = date;
    return this;
  }

  withEndDate(date: Date): this {
    this.endDate = date;
    return this;
  }

  withStatus(status: ContractStatus): this {
    this.status = status;
    return this;
  }

  withCollateral(collateral: string): this {
    this.collateral = collateral;
    return this;
  }

  build(): LoanContract {
    return new LoanContract(
      this.id,
      this.customerId,
      this.assetDescription,
      this.costPrice,
      this.profitMargin,
      this.sellingPrice,
      this.installments,
      this.installmentAmount,
      this.startDate,
      this.endDate,
      this.status,
      this.collateral,
    );
  }

  // Common scenarios
  static aValidContract(): LoanContractBuilder {
    return new LoanContractBuilder();
  }

  static anActiveContract(): LoanContractBuilder {
    return new LoanContractBuilder().withStatus(ContractStatus.ACTIVE);
  }

  static aCompletedContract(): LoanContractBuilder {
    return new LoanContractBuilder().withStatus(ContractStatus.COMPLETED);
  }

  static aDefaultedContract(): LoanContractBuilder {
    return new LoanContractBuilder().withStatus(ContractStatus.DEFAULTED);
  }

  static aCarFinancingContract(): LoanContractBuilder {
    return new LoanContractBuilder()
      .withAssetDescription("Honda Civic 2024")
      .withCostPrice(Money.fromAmount(25000, "USD"))
      .withProfitMargin(0.1)
      .withInstallments(12);
  }

  static aPropertyFinancingContract(): LoanContractBuilder {
    return new LoanContractBuilder()
      .withAssetDescription("Residential Property - 123 Main St")
      .withCostPrice(Money.fromAmount(300000, "USD"))
      .withProfitMargin(0.15)
      .withInstallments(60) // 5 years
      .withCollateral("Property title deed");
  }

  static aShortTermContract(): LoanContractBuilder {
    return new LoanContractBuilder().withInstallments(6).withEndDate(new Date("2024-06-30"));
  }

  static aLongTermContract(): LoanContractBuilder {
    return new LoanContractBuilder().withInstallments(60).withEndDate(new Date("2028-12-31"));
  }
}
```

## Builder Pattern Best Practices

### 1. Provide Sensible Defaults

```typescript
// ✅ GOOD - Defaults make simple tests easy
export class TaxAssessmentBuilder {
  private wealth: Money = Money.fromAmount(10000, "USD");
  private threshold: Money = Money.fromAmount(2000, "USD");
  private status: TaxStatus = TaxStatus.PENDING;
  // ...
}

// Usage - minimal customization needed
const assessment = new TaxAssessmentBuilder().build();

// ❌ BAD - No defaults, all values required
export class TaxAssessmentBuilder {
  private wealth?: Money;
  private threshold?: Money;
  // Must set everything
}
```

### 2. Use Fluent Interface

```typescript
// ✅ GOOD - Fluent chaining
const assessment = TaxAssessmentBuilder.aValidAssessment()
  .withDonorId("donor-123")
  .withWealthAmount(15000, "USD")
  .withStatus(TaxStatus.PAID)
  .build();

// ❌ BAD - Verbose setter calls
const builder = new TaxAssessmentBuilder();
builder.setDonorId("donor-123");
builder.setWealthAmount(15000, "USD");
builder.setStatus(TaxStatus.PAID);
const assessment = builder.build();
```

### 3. Provide Named Constructors for Common Scenarios

```typescript
// ✅ GOOD - Express intent clearly
const pending = TaxAssessmentBuilder.aPendingAssessment().build();
const aboveThreshold = TaxAssessmentBuilder.anAssessmentAboveThreshold().build();
const inEur = TaxAssessmentBuilder.anAssessmentInCurrency("EUR").build();

// ❌ BAD - Have to know implementation details
const pending = new TaxAssessmentBuilder().withStatus(TaxStatus.PENDING).build();
```

### 4. Calculate Dependent Values Automatically

```typescript
// ✅ GOOD - Recalculate dependent values
withProfitMargin(margin: number): this {
  this.profitMargin = margin;
  // Automatically update selling price
  this.sellingPrice = this.costPrice.multiply(1 + margin);
  return this;
}

// ❌ BAD - Require manual calculation
withProfitMargin(margin: number): this {
  this.profitMargin = margin;
  // User must manually set selling price
  return this;
}
```

### 5. Use Builders in Builders

```typescript
// ✅ GOOD - Compose builders
export class TaxAssessmentBuilder {
  private wealth: Money = MoneyBuilder.wealthAboveThresholdUsd();
  private threshold: Money = MoneyBuilder.thresholdUsd();
  // ...
}

// ❌ BAD - Duplicate default creation logic
export class TaxAssessmentBuilder {
  private wealth: Money = Money.fromAmount(10000, "USD");
  private threshold: Money = Money.fromAmount(2000, "USD");
  // ...
}
```

### 6. Make Builders Immutable (Clone Pattern)

```typescript
// ✅ GOOD - Immutable builder (advanced)
export class TaxAssessmentBuilder {
  private constructor(private readonly data: TaxData) {}

  static create(): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder({
      /* defaults */
    });
  }

  withDonorId(donorId: string): TaxAssessmentBuilder {
    return new TaxAssessmentBuilder({
      ...this.data,
      donorId,
    });
  }
}

// Can reuse builders safely
const base = TaxAssessmentBuilder.create().withDonorId("donor-001");
const variant1 = base.withStatus(TaxStatus.PAID); // Doesn't affect base
const variant2 = base.withStatus(TaxStatus.PENDING);
```

## Checklist for Test Data Builders

- [ ] Builder class named `[EntityName]Builder`
- [ ] All fields have sensible defaults
- [ ] Fluent setter methods return `this`
- [ ] Build method creates the entity
- [ ] Named constructors for common scenarios (static methods)
- [ ] Complex calculations handled automatically
- [ ] Uses other builders for complex dependencies
- [ ] Clear, descriptive method names
- [ ] Supports easy test data variation
- [ ] Reduces test duplication
- [ ] Makes test intent clear

## Related Templates

- [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md) - Using builders in unit tests
- [Integration Test Template](./ex-so-de-tedrdeve-te__integration-test-template.md) - Using builders in integration tests
- [Property-Based Test Template](./ex-so-de-tedrdeve-te__property-based-test-template.md) - Combining builders with property tests

## Summary

**Key Takeaways**:

1. **Sensible Defaults**: Make simple tests trivial to write
2. **Fluent Interface**: Chain method calls for readability
3. **Named Constructors**: Express test intent clearly
4. **Automatic Calculations**: Handle dependent values automatically
5. **Composability**: Use builders within builders
6. **Reduce Duplication**: Share test data creation logic
7. **Improve Readability**: Self-documenting test setup
8. **Maintenance**: Change defaults in one place

Test Data Builders significantly improve test quality by making test data creation consistent, readable, and maintainable.
