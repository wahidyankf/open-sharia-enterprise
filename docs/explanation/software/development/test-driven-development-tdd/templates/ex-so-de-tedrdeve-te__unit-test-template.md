# Unit Test Template

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Integration Test Template](./ex-so-de-tedrdeve-te__integration-test-template.md)
  - [Test Data Builder Template](./ex-so-de-tedrdeve-te__test-data-builder-template.md)
- **Use Case**: Testing isolated units (functions, classes, value objects)
- **Template Size**: ~15 KB
- **Complexity**: Beginner to Intermediate

## Overview

This template provides a standardized structure for writing unit tests using the AAA (Arrange-Act-Assert) pattern. Unit tests should be fast, isolated, and focused on testing a single unit of behavior.

## Core Principles

Unit testing aligns with software engineering principles:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Tests explicitly document expected behavior through clear assertions, making system contracts visible and verifiable.
- **[Pure Functions Over Side Effects](../../../../../../governance/principles/software-engineering/pure-functions.md)** - Unit tests verify pure function behavior by testing deterministic inputs and outputs without external dependencies.

## Template Structure

```typescript
// File: [component-name].spec.ts
import { ComponentUnderTest } from "./component-under-test";
import { Dependency } from "./dependency";

describe("[ComponentUnderTest]", () => {
  // Group related tests
  describe("[Method or Feature Name]", () => {
    // Happy path test
    it("[should do expected behavior when conditions are met]", () => {
      // Arrange - Set up test data and dependencies
      const dependency = createDependency();
      const sut = new ComponentUnderTest(dependency);
      const input = createInput();

      // Act - Execute the behavior under test
      const result = sut.methodUnderTest(input);

      // Assert - Verify the expected outcome
      expect(result).toBe(expectedValue);
    });

    // Edge case test
    it("[should handle edge case appropriately]", () => {
      // Arrange
      const sut = new ComponentUnderTest();
      const edgeCaseInput = createEdgeCaseInput();

      // Act
      const result = sut.methodUnderTest(edgeCaseInput);

      // Assert
      expect(result).toBe(expectedEdgeCaseValue);
    });

    // Error case test
    it("[should throw error when invalid input provided]", () => {
      // Arrange
      const sut = new ComponentUnderTest();
      const invalidInput = createInvalidInput();

      // Act & Assert
      expect(() => sut.methodUnderTest(invalidInput)).toThrow("[Expected error message]");
    });
  });

  // Add more describe blocks for other methods/features
  describe("[Another Method]", () => {
    // Tests...
  });
});
```

## Islamic Finance Example: Tax Calculator Unit Tests

### Component Under Test

```typescript
// File: tax-calculator.ts
import { Money } from "./money";

export class TaxCalculator {
  private readonly TAX_RATE = 0.025; // 2.5%

  calculateTax(wealth: Money, threshold: Money): Money {
    if (wealth.isLessThan(threshold)) {
      return Money.zero(wealth.currency);
    }

    return wealth.multiply(this.TAX_RATE);
  }

  isTaxDue(wealth: Money, threshold: Money): boolean {
    return wealth.isGreaterThanOrEqual(threshold);
  }

  calculateThreshold(goldPricePerGram: Money): Money {
    const THRESHOLD_GOLD_GRAMS = 85; // 85 grams of gold
    return goldPricePerGram.multiply(THRESHOLD_GOLD_GRAMS);
  }
}
```

### Complete Unit Test Suite

```typescript
// File: tax-calculator.spec.ts
import { TaxCalculator } from "./tax-calculator";
import { Money } from "./money";

describe("TaxCalculator", () => {
  let calculator: TaxCalculator;

  // Setup - runs before each test
  beforeEach(() => {
    calculator = new TaxCalculator();
  });

  describe("calculateTax", () => {
    describe("when wealth is above threshold", () => {
      it("should calculate 2.5% of wealth as Tax", () => {
        // Arrange
        const wealth = Money.fromAmount(10000, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(250); // 2.5% of 10000
        expect(tax.currency).toBe("USD");
      });

      it("should handle large wealth amounts correctly", () => {
        // Arrange
        const wealth = Money.fromAmount(1000000, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(25000); // 2.5% of 1000000
      });

      it("should handle decimal wealth amounts correctly", () => {
        // Arrange
        const wealth = Money.fromAmount(10000.5, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBeCloseTo(250.01, 2); // 2.5% of 10000.50
      });
    });

    describe("when wealth equals threshold", () => {
      it("should calculate Tax (threshold is inclusive)", () => {
        // Arrange
        const wealth = Money.fromAmount(2000, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(50); // 2.5% of 2000
      });
    });

    describe("when wealth is below threshold", () => {
      it("should return zero Tax", () => {
        // Arrange
        const wealth = Money.fromAmount(1000, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(0);
        expect(tax.currency).toBe("USD");
      });

      it("should return zero for wealth just below threshold", () => {
        // Arrange
        const wealth = Money.fromAmount(1999.99, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(0);
      });
    });

    describe("when wealth is zero", () => {
      it("should return zero Tax", () => {
        // Arrange
        const wealth = Money.fromAmount(0, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(0);
      });
    });

    describe("with different currencies", () => {
      it("should calculate Tax in USD", () => {
        // Arrange
        const wealth = Money.fromAmount(10000, "USD");
        const threshold = Money.fromAmount(2000, "USD");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.currency).toBe("USD");
      });

      it("should calculate Tax in EUR", () => {
        // Arrange
        const wealth = Money.fromAmount(8000, "EUR");
        const threshold = Money.fromAmount(1800, "EUR");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(200); // 2.5% of 8000
        expect(tax.currency).toBe("EUR");
      });

      it("should calculate Tax in SAR (Saudi Riyal)", () => {
        // Arrange
        const wealth = Money.fromAmount(40000, "SAR");
        const threshold = Money.fromAmount(7500, "SAR");

        // Act
        const tax = calculator.calculateTax(wealth, threshold);

        // Assert
        expect(tax.amount).toBe(1000); // 2.5% of 40000
        expect(tax.currency).toBe("SAR");
      });
    });
  });

  describe("isTaxDue", () => {
    it("should return true when wealth is above threshold", () => {
      // Arrange
      const wealth = Money.fromAmount(10000, "USD");
      const threshold = Money.fromAmount(2000, "USD");

      // Act
      const isDue = calculator.isTaxDue(wealth, threshold);

      // Assert
      expect(isDue).toBe(true);
    });

    it("should return true when wealth equals threshold", () => {
      // Arrange
      const wealth = Money.fromAmount(2000, "USD");
      const threshold = Money.fromAmount(2000, "USD");

      // Act
      const isDue = calculator.isTaxDue(wealth, threshold);

      // Assert
      expect(isDue).toBe(true);
    });

    it("should return false when wealth is below threshold", () => {
      // Arrange
      const wealth = Money.fromAmount(1000, "USD");
      const threshold = Money.fromAmount(2000, "USD");

      // Act
      const isDue = calculator.isTaxDue(wealth, threshold);

      // Assert
      expect(isDue).toBe(false);
    });

    it("should return false when wealth is zero", () => {
      // Arrange
      const wealth = Money.fromAmount(0, "USD");
      const threshold = Money.fromAmount(2000, "USD");

      // Act
      const isDue = calculator.isTaxDue(wealth, threshold);

      // Assert
      expect(isDue).toBe(false);
    });
  });

  describe("calculateThreshold", () => {
    it("should calculate threshold based on gold price (85 grams)", () => {
      // Arrange
      const goldPricePerGram = Money.fromAmount(60, "USD"); // $60 per gram

      // Act
      const threshold = calculator.calculateThreshold(goldPricePerGram);

      // Assert
      expect(threshold.amount).toBe(5100); // 85 * 60
      expect(threshold.currency).toBe("USD");
    });

    it("should handle decimal gold prices", () => {
      // Arrange
      const goldPricePerGram = Money.fromAmount(62.5, "USD");

      // Act
      const threshold = calculator.calculateThreshold(goldPricePerGram);

      // Assert
      expect(threshold.amount).toBe(5312.5); // 85 * 62.50
    });

    it("should calculate threshold in different currencies", () => {
      // Arrange
      const goldPricePerGram = Money.fromAmount(55, "EUR");

      // Act
      const threshold = calculator.calculateThreshold(goldPricePerGram);

      // Assert
      expect(threshold.amount).toBe(4675); // 85 * 55
      expect(threshold.currency).toBe("EUR");
    });
  });

  // Test immutability
  describe("immutability", () => {
    it("should not mutate input Money objects", () => {
      // Arrange
      const originalWealth = Money.fromAmount(10000, "USD");
      const originalThreshold = Money.fromAmount(2000, "USD");
      const wealthSnapshot = originalWealth.amount;
      const thresholdSnapshot = originalThreshold.amount;

      // Act
      calculator.calculateTax(originalWealth, originalThreshold);

      // Assert - original objects unchanged
      expect(originalWealth.amount).toBe(wealthSnapshot);
      expect(originalThreshold.amount).toBe(thresholdSnapshot);
    });
  });
});
```

## Key Patterns and Best Practices

### 1. AAA Pattern (Arrange-Act-Assert)

```typescript
describe("Component", () => {
  it("should do something", () => {
    // Arrange - Set up test conditions
    const input = createInput();
    const component = new Component();

    // Act - Execute the behavior
    const result = component.method(input);

    // Assert - Verify the outcome
    expect(result).toBe(expected);
  });
});
```

### 2. One Assertion Per Test (guideline, not rule)

```typescript
// ✅ GOOD - Clear, focused test
it("should calculate Tax as 2.5% of wealth", () => {
  const tax = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

  expect(tax.amount).toBe(250);
});

// ⚠️ OK - Multiple assertions on same concept
it("should return Money with correct amount and currency", () => {
  const tax = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

  expect(tax.amount).toBe(250);
  expect(tax.currency).toBe("USD"); // Related assertion
});

// ❌ BAD - Testing multiple behaviors
it("should calculate Tax and check if due", () => {
  const tax = calculator.calculateTax(/* ... */); // Behavior 1
  expect(tax.amount).toBe(250);

  const isDue = calculator.isTaxDue(/* ... */); // Behavior 2 (separate test)
  expect(isDue).toBe(true);
});
```

### 3. Descriptive Test Names

```typescript
// ✅ GOOD - Describes behavior and context
it("should return zero Tax when wealth is below threshold", () => {});

it("should calculate 2.5% of wealth as Tax when above threshold", () => {});

it("should throw error when wealth currency differs from threshold currency", () => {});

// ❌ BAD - Vague or implementation-focused
it("should work", () => {});

it("should call multiply with 0.025", () => {}); // Implementation detail

it("test1", () => {}); // No description
```

### 4. Test Data Builders

```typescript
// Create test data builder for reusability
class TaxTestBuilder {
  static wealthAboveThreshold(): { wealth: Money; threshold: Money } {
    return {
      wealth: Money.fromAmount(10000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    };
  }

  static wealthBelowThreshold(): { wealth: Money; threshold: Money } {
    return {
      wealth: Money.fromAmount(1000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    };
  }

  static wealthAtThreshold(): { wealth: Money; threshold: Money } {
    return {
      wealth: Money.fromAmount(2000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    };
  }
}

// Use in tests
describe("TaxCalculator", () => {
  it("should calculate Tax when wealth above threshold", () => {
    const { wealth, threshold } = TaxTestBuilder.wealthAboveThreshold();

    const tax = calculator.calculateTax(wealth, threshold);

    expect(tax.amount).toBe(250);
  });
});
```

### 5. Setup and Teardown

```typescript
describe("Component", () => {
  let component: Component;
  let dependency: Dependency;

  // Runs before each test
  beforeEach(() => {
    dependency = new Dependency();
    component = new Component(dependency);
  });

  // Runs after each test (cleanup)
  afterEach(() => {
    // Clean up resources if needed
  });

  // Runs once before all tests in this describe block
  beforeAll(() => {
    // One-time setup
  });

  // Runs once after all tests in this describe block
  afterAll(() => {
    // One-time cleanup
  });

  it("should use setup component", () => {
    // component is already initialized
    expect(component).toBeDefined();
  });
});
```

### 6. Testing Exceptions

```typescript
describe("Money", () => {
  it("should throw error when creating Money with negative amount", () => {
    expect(() => Money.fromAmount(-100, "USD")).toThrow("Amount cannot be negative");
  });

  it("should throw error when adding different currencies", () => {
    const usd = Money.fromAmount(100, "USD");
    const eur = Money.fromAmount(50, "EUR");

    expect(() => usd.add(eur)).toThrow("Cannot add different currencies");
  });

  // Async exception
  it("should throw error when saving invalid data", async () => {
    const repository = new Repository();

    await expect(repository.save(invalidData)).rejects.toThrow("Invalid data");
  });
});
```

## Value Object Example: Money

```typescript
// File: money.ts
export class Money {
  private constructor(
    private readonly _amount: number,
    private readonly _currency: string,
  ) {
    if (_amount < 0) {
      throw new Error("Amount cannot be negative");
    }
  }

  static fromAmount(amount: number, currency: string): Money {
    return new Money(amount, currency);
  }

  static zero(currency: string): Money {
    return new Money(0, currency);
  }

  get amount(): number {
    return this._amount;
  }

  get currency(): string {
    return this._currency;
  }

  add(other: Money): Money {
    this.ensureSameCurrency(other);
    return Money.fromAmount(this.amount + other.amount, this.currency);
  }

  multiply(multiplier: number): Money {
    return Money.fromAmount(this.amount * multiplier, this.currency);
  }

  isGreaterThanOrEqual(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount >= other.amount;
  }

  isLessThan(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount < other.amount;
  }

  private ensureSameCurrency(other: Money): void {
    if (this.currency !== other.currency) {
      throw new Error("Cannot operate on different currencies");
    }
  }
}
```

```typescript
// File: money.spec.ts
import { Money } from "./money";

describe("Money", () => {
  describe("fromAmount", () => {
    it("should create Money with correct amount and currency", () => {
      const money = Money.fromAmount(100, "USD");

      expect(money.amount).toBe(100);
      expect(money.currency).toBe("USD");
    });

    it("should throw error for negative amount", () => {
      expect(() => Money.fromAmount(-100, "USD")).toThrow("Amount cannot be negative");
    });
  });

  describe("zero", () => {
    it("should create Money with zero amount", () => {
      const money = Money.zero("USD");

      expect(money.amount).toBe(0);
      expect(money.currency).toBe("USD");
    });
  });

  describe("add", () => {
    it("should add two Money instances with same currency", () => {
      const money1 = Money.fromAmount(100, "USD");
      const money2 = Money.fromAmount(50, "USD");

      const sum = money1.add(money2);

      expect(sum.amount).toBe(150);
      expect(sum.currency).toBe("USD");
    });

    it("should throw error when adding different currencies", () => {
      const usd = Money.fromAmount(100, "USD");
      const eur = Money.fromAmount(50, "EUR");

      expect(() => usd.add(eur)).toThrow("Cannot operate on different currencies");
    });
  });

  describe("multiply", () => {
    it("should multiply Money by number", () => {
      const money = Money.fromAmount(100, "USD");

      const result = money.multiply(2.5);

      expect(result.amount).toBe(250);
      expect(result.currency).toBe("USD");
    });

    it("should multiply Money by decimal", () => {
      const money = Money.fromAmount(10000, "USD");

      const result = money.multiply(0.025); // Tax rate

      expect(result.amount).toBe(250);
    });
  });

  describe("comparison methods", () => {
    describe("isGreaterThanOrEqual", () => {
      it("should return true when amount is greater", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(50, "USD");

        expect(money1.isGreaterThanOrEqual(money2)).toBe(true);
      });

      it("should return true when amounts are equal", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isGreaterThanOrEqual(money2)).toBe(true);
      });

      it("should return false when amount is less", () => {
        const money1 = Money.fromAmount(50, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isGreaterThanOrEqual(money2)).toBe(false);
      });
    });

    describe("isLessThan", () => {
      it("should return true when amount is less", () => {
        const money1 = Money.fromAmount(50, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isLessThan(money2)).toBe(true);
      });

      it("should return false when amounts are equal", () => {
        const money1 = Money.fromAmount(100, "USD");
        const money2 = Money.fromAmount(100, "USD");

        expect(money1.isLessThan(money2)).toBe(false);
      });
    });
  });

  describe("immutability", () => {
    it("should not mutate original Money when adding", () => {
      const original = Money.fromAmount(100, "USD");
      const other = Money.fromAmount(50, "USD");

      original.add(other);

      expect(original.amount).toBe(100); // Unchanged
    });

    it("should not mutate original Money when multiplying", () => {
      const original = Money.fromAmount(100, "USD");

      original.multiply(2);

      expect(original.amount).toBe(100); // Unchanged
    });
  });
});
```

## Checklist for Unit Tests

- [ ] Test file name matches source file (`[name].spec.ts`)
- [ ] All public methods have tests
- [ ] Happy path tested
- [ ] Edge cases tested (zero, null, empty, boundary values)
- [ ] Error cases tested (invalid input, exceptions)
- [ ] Tests use AAA pattern (Arrange-Act-Assert)
- [ ] Test names are descriptive (should/when/given format)
- [ ] No dependencies on external systems (database, API, file system)
- [ ] Tests are fast (<100ms each)
- [ ] Tests are isolated (can run in any order)
- [ ] Tests are deterministic (same input → same output)
- [ ] Immutability verified (original objects unchanged)
- [ ] Setup/teardown used appropriately
- [ ] Test data builders used for complex objects

## Related Templates

- [Integration Test Template](./ex-so-de-tedrdeve-te__integration-test-template.md) - Testing with real dependencies
- [Test Data Builder Template](./ex-so-de-tedrdeve-te__test-data-builder-template.md) - Creating test data
- [Property-Based Test Template](./ex-so-de-tedrdeve-te__property-based-test-template.md) - Testing with generated data

## Related Principles

Unit testing demonstrates alignment with:

- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Tests serve as explicit documentation of behavior contracts.

See [Software Engineering Principles](../../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Summary

**Key Takeaways**:

1. **AAA Pattern**: Arrange-Act-Assert for clear test structure
2. **Descriptive Names**: Tests document expected behavior
3. **Fast and Isolated**: No external dependencies, can run in any order
4. **Edge Cases**: Test boundaries, zeros, nulls, empty values
5. **Immutability**: Verify objects aren't mutated
6. **Test Builders**: Reuse test data creation logic
7. **One Behavior Per Test**: Focus on single aspect of functionality

Use this template as a starting point for writing comprehensive, maintainable unit tests.
