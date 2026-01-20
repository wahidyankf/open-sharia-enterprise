# Test-Driven Development: Unit Testing Fundamentals

## Overview

Unit tests are the foundation of Test-Driven Development. They verify individual pieces of code—functions, classes, or modules—in complete isolation from external dependencies. Fast, focused, and deterministic, unit tests provide immediate feedback during development and form the base of the testing pyramid.

This document covers the fundamentals of writing effective unit tests: test structure (Arrange-Act-Assert), isolation principles, test naming, assertions, and organizing test suites. Mastering these fundamentals is essential for productive TDD practice.

## What Makes a Good Unit Test?

A good unit test has five key characteristics, summarized by the **FIRST** principles:

### F - Fast

**Characteristic**: Unit tests execute in milliseconds.

**Why it matters**: Developers run tests constantly during TDD. Slow tests break flow and discourage frequent execution.

**How to achieve**:

- No I/O operations (database, file system, network)
- No real external dependencies (use test doubles)
- Minimal setup and teardown
- Pure computation only

```typescript
// GOOD: Fast test (< 1ms)
it("should calculate tax at 2.5% rate", () => {
  const wealth = Money.fromGold(100, "grams");
  const tax = wealth.multiply(0.025);
  expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});

// BAD: Slow test (> 500ms)
it("should save tax calculation to database", async () => {
  const wealth = Money.fromGold(100, "grams");
  await database.connect(); // Slow I/O
  await database.save({ wealth, tax: wealth.multiply(0.025) });
  const result = await database.query("SELECT * FROM tax");
  expect(result.rows[0].tax).toBe(2.5);
});
```

### I - Independent (Isolated)

**Characteristic**: Each test runs independently without shared state.

**Why it matters**: Tests that depend on each other are fragile. Changing one test breaks others. Parallel execution becomes impossible.

**How to achieve**:

- No shared mutable state between tests
- Each test creates its own test data
- Tests can run in any order
- Use `beforeEach` for common setup (creates fresh instances)

```typescript
// GOOD: Independent tests
describe("Money", () => {
  it("should add two amounts", () => {
    const five = Money.usd(5); // Fresh instance
    const three = Money.usd(3);
    expect(five.add(three).equals(Money.usd(8))).toBe(true);
  });

  it("should multiply by percentage", () => {
    const hundred = Money.usd(100); // Fresh instance
    expect(hundred.multiply(0.025).equals(Money.usd(2.5))).toBe(true);
  });
});

// BAD: Dependent tests (shared state)
describe("TaxCalculator", () => {
  let calculator = new TaxCalculator(); // Shared instance

  it("should set threshold threshold", () => {
    calculator.setThreshold(Money.fromGold(85, "grams"));
    expect(calculator.threshold.amount).toBe(85);
  });

  it("should calculate tax", () => {
    // Depends on previous test setting threshold ❌
    const tax = calculator.calculate(Money.fromGold(100, "grams"));
    expect(tax.amount).toBe(2.5);
  });
});
```

### R - Repeatable

**Characteristic**: Running the same test multiple times always produces the same result.

**Why it matters**: Flaky tests (pass/fail randomly) destroy confidence in the test suite. Developers ignore failures if tests are unreliable.

**How to achieve**:

- No dependency on current date/time (inject or mock)
- No dependency on random values (use fixed seeds or inject)
- No dependency on external services (use test doubles)
- Deterministic algorithms only

```typescript
// GOOD: Repeatable test
it("should verify Hawl period completion", () => {
  const acquisitionDate = HijriDate.of(1445, 1, 1); // Fixed date
  const currentDate = HijriDate.of(1446, 1, 1); // Fixed date

  const hasCompleted = Hawl.hasCompleted(acquisitionDate, currentDate);

  expect(hasCompleted).toBe(true);
});

// BAD: Non-repeatable test
it("should calculate tax for current year", () => {
  const currentYear = new Date().getFullYear(); // Changes every year ❌
  const threshold = getThresholdForYear(currentYear);
  expect(threshold).toBeDefined();
  // Test will break on January 1st next year
});
```

### S - Self-Validating

**Characteristic**: Test either passes (green) or fails (red) automatically. No manual verification needed.

**Why it matters**: Manual verification doesn't scale. Automated pass/fail enables continuous integration.

**How to achieve**:

- Use assertions to verify outcomes
- No console.log or manual inspection
- Boolean result: green or red

```typescript
// GOOD: Self-validating
it("should reject negative threshold amount", () => {
  expect(() => IncomeThreshold.fromGold(-85, "grams")).toThrow("Threshold cannot be negative");
});

// BAD: Requires manual verification
it("should reject negative threshold amount", () => {
  try {
    IncomeThreshold.fromGold(-85, "grams");
    console.log("FAIL: Should have thrown error"); // Manual check ❌
  } catch (e) {
    console.log("PASS: Error thrown correctly"); // Manual check ❌
  }
});
```

### T - Thorough (Timely)

**Characteristic**: Tests cover important behaviors, edge cases, and error conditions. Written at the right time (before production code in TDD).

**Why it matters**: Incomplete tests give false confidence. Critical bugs slip through.

**How to achieve**:

- Test happy path (normal behavior)
- Test edge cases (boundaries, empty inputs, nulls)
- Test error conditions (invalid inputs, violations)
- Write test BEFORE implementation (TDD)

```typescript
// GOOD: Thorough testing
describe("Money.add", () => {
  it("should add two positive amounts", () => {
    // Happy path
  });

  it("should handle zero amount", () => {
    // Edge case
  });

  it("should throw on currency mismatch", () => {
    // Error condition
  });

  it("should handle very large amounts without overflow", () => {
    // Edge case
  });
});

// BAD: Incomplete testing
describe("Money.add", () => {
  it("should add two amounts", () => {
    // Only happy path, missing edge cases and errors ❌
  });
});
```

## Test Structure: Arrange-Act-Assert (AAA)

The Arrange-Act-Assert pattern provides a clear, consistent structure for unit tests.

### The Three Phases

**1. Arrange (Given)**: Set up test conditions

- Create test data (objects, values)
- Configure mocks/stubs
- Establish preconditions

**2. Act (When)**: Execute the behavior under test

- Call the method/function
- Trigger the action
- Only ONE action (not multiple)

**3. Assert (Then)**: Verify the outcome

- Check return values
- Verify state changes
- Assert side effects occurred

### AAA Pattern Example

```typescript
describe("calculateTax", () => {
  it("should return zero when wealth is below threshold", () => {
    // ARRANGE: Set up test data
    const wealth = Money.fromGold(50, "grams"); // Below threshold
    const threshold = Money.fromGold(85, "grams");
    const rate = TaxRate.standard();

    // ACT: Execute the function
    const taxAmount = calculateTax(wealth, threshold, rate);

    // ASSERT: Verify the result
    expect(taxAmount.equals(Money.zero("gold"))).toBe(true);
  });
});
```

### AAA with Comments (for Clarity)

Use comments to visually separate the three phases when tests are complex:

```typescript
it("should calculate profit for Loan contract", () => {
  // Arrange
  const costPrice = Money.usd(10000);
  const profitRate = Percentage.of(15);
  const contract = LoanContract.create(costPrice, profitRate);

  // Act
  const sellingPrice = contract.calculateSellingPrice();

  // Assert
  expect(sellingPrice.equals(Money.usd(11500))).toBe(true);
  expect(contract.profitAmount.equals(Money.usd(1500))).toBe(true);
});
```

### One Assert Per Concept (Not Necessarily Per Test)

**Guideline**: Each test should verify **one logical concept**, but may have multiple assertion statements.

```typescript
// GOOD: Multiple asserts for one concept (object equality)
it("should create tax assessment with correct properties", () => {
  const assessment = TaxAssessment.create(Money.fromGold(100, "grams"), Money.fromGold(85, "grams"));

  expect(assessment.wealth.amount).toBe(100);
  expect(assessment.threshold.amount).toBe(85);
  expect(assessment.meetsThreshold()).toBe(true);
  // All assertions verify the "assessment creation" concept ✅
});

// BAD: Multiple unrelated concepts in one test
it("should handle tax calculations and storage", () => {
  const tax = calculateTax(wealth, threshold, rate);
  expect(tax.amount).toBe(2.5); // Testing calculation

  await repository.save(tax);
  expect(await repository.findById(tax.id)).toBeDefined();
  // Testing storage - should be separate test ❌
});
```

## Test Isolation: The Unit in "Unit Test"

Unit tests must be **isolated** from external dependencies. This ensures tests are fast, deterministic, and focused.

### What to Isolate From

1. **Database**: Use in-memory data structures or test doubles
2. **File System**: Mock file operations
3. **Network/HTTP**: Mock API calls
4. **External Services**: Stub third-party integrations
5. **System Clock**: Inject dates/times
6. **Random Values**: Inject or use fixed seed

### Isolation Techniques

#### 1. Dependency Injection

Pass dependencies as parameters instead of hardcoding:

```typescript
// BAD: Hard-coded dependency (not testable)
class TaxCalculator {
  calculate(wealth: Money): Money {
    const threshold = database.query("SELECT threshold FROM config"); // Hard-coded DB ❌
    return wealth.isGreaterThan(threshold) ? wealth.multiply(0.025) : Money.zero();
  }
}

// GOOD: Dependency injection (testable)
class TaxCalculator {
  constructor(private thresholdProvider: ThresholdProvider) {}

  calculate(wealth: Money): Money {
    const threshold = this.thresholdProvider.getThreshold();
    return wealth.isGreaterThan(threshold) ? wealth.multiply(0.025) : Money.zero();
  }
}

// Test with mock
it("should calculate tax using provided threshold", () => {
  const mockThresholdProvider = {
    getThreshold: () => Money.fromGold(85, "grams"),
  };
  const calculator = new TaxCalculator(mockThresholdProvider);

  const tax = calculator.calculate(Money.fromGold(100, "grams"));

  expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});
```

#### 2. Pure Functions (Best Isolation)

Pure functions have no dependencies—they're inherently isolated:

```typescript
// Pure function - perfect for unit testing
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// Test is trivial - no mocking needed ✅
it("should calculate tax", () => {
  const tax = calculateTax(Money.fromGold(100, "grams"), Money.fromGold(85, "grams"), TaxRate.standard());
  expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});
```

#### 3. Test Doubles (Mocks, Stubs, Fakes)

When dependencies are necessary, use test doubles. See [05. Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md) for detailed coverage.

**Quick Reference**:

- **Stub**: Returns canned responses
- **Mock**: Verifies interactions
- **Fake**: Simplified working implementation (in-memory database)

```typescript
// Using stub for PermittedCertificationAuthority
it("should certify product when authority approves", async () => {
  const stubAuthority = {
    verify: () => Promise.resolve({ approved: true }),
  };

  const service = new CertificationService(stubAuthority);
  const result = await service.certifyProduct(product);

  expect(result.certified).toBe(true);
});
```

## Test Naming Conventions

Good test names serve as documentation. They should describe **behavior**, not implementation.

### Template: `should [expected behavior] when [context]`

```typescript
// GOOD: Descriptive behavior
describe("Threshold Threshold Validation", () => {
  it("should return true when wealth meets threshold threshold", () => {});
  it("should return true when wealth exceeds threshold threshold", () => {});
  it("should return false when wealth is below threshold threshold", () => {});
  it("should throw error when wealth and threshold have different currencies", () => {});
});

// BAD: Implementation-focused or vague
describe("Threshold Threshold Validation", () => {
  it("test1", () => {}); // Meaningless ❌
  it("threshold works", () => {}); // Vague ❌
  it("should call isGreaterThan method", () => {}); // Implementation detail ❌
  it("should return boolean", () => {}); // Too generic ❌
});
```

### Describe Blocks for Organization

Use nested `describe` blocks to organize related tests:

```typescript
describe("Money", () => {
  describe("Addition", () => {
    it("should add two amounts in same currency", () => {});
    it("should throw error when currencies differ", () => {});
  });

  describe("Multiplication", () => {
    it("should multiply by positive percentage", () => {});
    it("should throw error for negative percentage", () => {});
  });

  describe("Comparison", () => {
    it("should return true when amounts and currencies match", () => {});
    it("should return false when amounts differ", () => {});
  });
});
```

### Test Names in Plain Language

Test names should be readable without code knowledge:

```typescript
// GOOD: Business language
it("should exempt personal residence from tax calculation", () => {});
it("should apply 2.5% rate to gold and silver assets", () => {});
it("should verify Hawl period of one lunar year has passed", () => {});

// BAD: Technical jargon
it("should filter assets where taxable === false", () => {}); // Use "exempt" not "taxable === false"
it("should apply TAX_RATE constant to metals array", () => {}); // Use "2.5% rate" not "TAX_RATE constant"
```

## Assertions: Verifying Outcomes

Assertions are how tests express expected behavior. Good assertions are clear, specific, and fail with helpful messages.

### Common Assertion Patterns

#### 1. Equality Assertions

```typescript
// Primitive equality
expect(taxAmount.amount).toBe(2.5);
expect(thresholdMet).toBe(true);

// Object equality (deep comparison)
expect(money).toEqual({ amount: 100, currency: "USD" });

// Custom equality (value objects)
expect(money.equals(Money.usd(100))).toBe(true);
```

#### 2. Truthiness Assertions

```typescript
expect(result).toBeDefined();
expect(result).not.toBeNull();
expect(list.length).toBeGreaterThan(0);
expect(wealth.amount).toBeGreaterThanOrEqual(threshold.amount);
```

#### 3. Error Assertions

```typescript
// Assert function throws
expect(() => Money.usd(-10)).toThrow("Money cannot be negative");

// Assert async function rejects
await expect(service.certifyProduct(invalidProduct)).rejects.toThrow(ValidationError);
```

#### 4. Array/Collection Assertions

```typescript
expect(assets).toHaveLength(3);
expect(assets).toContain(goldAsset);
expect(taxableAssets).toEqual(expect.arrayContaining([goldAsset, silverAsset]));
```

### Custom Matchers for Domain Objects

Create custom matchers for clearer assertions:

```typescript
// Custom matcher for Money
expect.extend({
  toEqualMoney(received: Money, expected: Money) {
    const pass = received.equals(expected);
    return {
      pass,
      message: () => `expected ${received.toString()} to equal ${expected.toString()}`,
    };
  },
});

// Usage
expect(taxAmount).toEqualMoney(Money.fromGold(2.5, "grams"));
```

## Organizing Test Files

### File Structure

Match test file structure to source file structure:

```
src/
├── domain/
│   ├── money.ts
│   ├── tax-calculator.ts
│   └── threshold-threshold.ts
└── domain/
    ├── __tests__/
    │   ├── money.spec.ts
    │   ├── tax-calculator.spec.ts
    │   └── threshold-threshold.spec.ts
```

Or use co-located pattern:

```
src/
└── domain/
    ├── money.ts
    ├── money.spec.ts
    ├── tax-calculator.ts
    ├── tax-calculator.spec.ts
    ├── threshold-threshold.ts
    └── threshold-threshold.spec.ts
```

### Test Suite Organization

```typescript
// One describe block per class/module
describe("TaxCalculator", () => {
  // Nested describe for each public method/behavior
  describe("calculate", () => {
    // Tests for happy path
    it("should calculate 2.5% tax when wealth exceeds threshold", () => {});

    // Tests for edge cases
    it("should return zero when wealth equals threshold threshold", () => {});
    it("should handle very large wealth amounts", () => {});

    // Tests for error conditions
    it("should throw when wealth and threshold currencies differ", () => {});
    it("should throw when rate is negative", () => {});
  });

  describe("setThreshold", () => {
    it("should update threshold threshold", () => {});
    it("should throw when threshold is negative", () => {});
  });
});
```

## Test Data: Builders and Fixtures

### Test Data Builders

Use builder pattern for complex test objects. See [07. Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md) for details.

```typescript
// Builder for complex domain objects
class TaxAssessmentBuilder {
  private wealth = Money.fromGold(100, "grams");
  private threshold = Money.fromGold(85, "grams");
  private date = HijriDate.now();

  withWealth(wealth: Money): this {
    this.wealth = wealth;
    return this;
  }

  withThreshold(threshold: Money): this {
    this.threshold = threshold;
    return this;
  }

  withDate(date: HijriDate): this {
    this.date = date;
    return this;
  }

  build(): TaxAssessment {
    return new TaxAssessment(AssessmentId.generate(), this.wealth, this.threshold, this.date);
  }
}

// Usage in tests
it("should calculate tax for assessment", () => {
  const assessment = new TaxAssessmentBuilder()
    .withWealth(Money.fromGold(200, "grams"))
    .withThreshold(Money.fromGold(85, "grams"))
    .build();

  const tax = assessment.calculateTax(TaxRate.standard());

  expect(tax.equals(Money.fromGold(5.0, "grams"))).toBe(true);
});
```

### Factory Functions

Simple factory functions for common test data:

```typescript
function createGoldAsset(grams: number): Asset {
  return new Asset(AssetId.generate(), "gold", Money.fromGold(grams, "grams"));
}

function createSilverAsset(grams: number): Asset {
  return new Asset(AssetId.generate(), "silver", Money.fromSilver(grams, "grams"));
}

// Usage
it("should calculate tax for multiple assets", () => {
  const assets = [createGoldAsset(100), createSilverAsset(600)];
  // ...
});
```

## Testing Islamic Finance Domain: Examples

### Example 1: Tax Calculation for Different Asset Types

```typescript
describe("Tax Calculation", () => {
  describe("Gold and Silver Assets", () => {
    it("should apply 2.5% rate to gold when above 85g threshold", () => {
      const wealth = Money.fromGold(100, "grams");
      const threshold = Money.fromGold(85, "grams");
      const rate = TaxRate.precious Metals(); // 2.5%

      const tax = calculateTax(wealth, threshold, rate);

      expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true);
    });

    it("should apply 2.5% rate to silver when above 595g threshold", () => {
      const wealth = Money.fromSilver(600, "grams");
      const threshold = Money.fromSilver(595, "grams");
      const rate = TaxRate.preciousMetals();

      const tax = calculateTax(wealth, threshold, rate);

      expect(tax.equals(Money.fromSilver(15.0, "grams"))).toBe(true);
    });
  });

  describe("Agricultural Produce", () => {
    it("should apply 10% rate to irrigated crops", () => {
      const harvest = AgricultureYield.fromTons(50, "wheat", "irrigated");

      const tax = calculateAgriculturalTax(harvest);

      expect(tax.tons).toBe(5.0); // 10% of 50 tons
    });

    it("should apply 20% rate to rain-fed crops", () => {
      const harvest = AgricultureYield.fromTons(50, "wheat", "rain-fed");

      const tax = calculateAgriculturalTax(harvest);

      expect(tax.tons).toBe(10.0); // 20% of 50 tons
    });
  });

  describe("Exemptions", () => {
    it("should exempt personal residence from wealth calculation", () => {
      const assets = [
        Asset.cash(Money.usd(50000)),
        Asset.personalResidence(Money.usd(300000)), // Exempt
        Asset.investmentProperty(Money.usd(200000)),
      ];

      const taxableWealth = calculateTaxableWealth(assets);

      expect(taxableWealth.equals(Money.usd(250000))).toBe(true);
    });
  });
});
```

### Example 2: Hawl Period Verification

```typescript
describe("Hawl Period (Lunar Year)", () => {
  it("should return false when less than full lunar year has passed", () => {
    const acquisitionDate = HijriDate.of(1445, 1, 1);
    const currentDate = HijriDate.of(1445, 12, 29); // 354 days (1 day short)

    const hasCompleted = Hawl.hasCompleted(acquisitionDate, currentDate);

    expect(hasCompleted).toBe(false);
  });

  it("should return true when exactly one lunar year has passed", () => {
    const acquisitionDate = HijriDate.of(1445, 1, 1);
    const currentDate = HijriDate.of(1446, 1, 1); // Exactly 354/355 days

    const hasCompleted = Hawl.hasCompleted(acquisitionDate, currentDate);

    expect(hasCompleted).toBe(true);
  });

  it("should return true when more than one lunar year has passed", () => {
    const acquisitionDate = HijriDate.of(1445, 1, 1);
    const currentDate = HijriDate.of(1447, 6, 15); // 2.5 lunar years

    const hasCompleted = Hawl.hasCompleted(acquisitionDate, currentDate);

    expect(hasCompleted).toBe(true);
  });
});
```

### Example 3: Loan Contract Profit Calculation

```typescript
describe("Loan Contract", () => {
  describe("Profit Calculation", () => {
    it("should calculate selling price as cost plus profit margin", () => {
      const costPrice = Money.usd(10000);
      const profitRate = Percentage.of(15); // 15% profit

      const contract = LoanContract.create(costPrice, profitRate);

      expect(contract.sellingPrice.equals(Money.usd(11500))).toBe(true);
      expect(contract.profitAmount.equals(Money.usd(1500))).toBe(true);
    });

    it("should handle zero profit margin", () => {
      const costPrice = Money.usd(10000);
      const profitRate = Percentage.of(0);

      const contract = LoanContract.create(costPrice, profitRate);

      expect(contract.sellingPrice.equals(Money.usd(10000))).toBe(true);
      expect(contract.profitAmount.equals(Money.zero("USD"))).toBe(true);
    });
  });

  describe("Compliance Compliance", () => {
    it("should require bank ownership before sale", () => {
      const asset = Asset.create("car", Money.usd(20000));
      const buyer = Customer.create("Ahmad");

      expect(() => LoanContract.sellWithoutOwnership(asset, buyer)).toThrow(
        "Bank must own asset before selling in Loan",
      );
    });

    it("should prohibit interest-based pricing (Interest)", () => {
      const costPrice = Money.usd(10000);
      const interestRate = InterestRate.of(5); // Interest, not profit

      expect(() => LoanContract.create(costPrice, interestRate)).toThrow(InterestProhibitionError);
    });
  });
});
```

## Common Unit Testing Mistakes

### Mistake 1: Testing Implementation Instead of Behavior

```typescript
// BAD: Testing how it works
it("should call multiply method with 0.025", () => {
  const spy = jest.spyOn(money, "multiply");
  calculateTax(money, threshold, rate);
  expect(spy).toHaveBeenCalledWith(0.025); // Testing implementation ❌
});

// GOOD: Testing what it does
it("should calculate 2.5% tax", () => {
  const tax = calculateTax(money, threshold, rate);
  expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true); // Testing behavior ✅
});
```

### Mistake 2: Multiple Behaviors in One Test

```typescript
// BAD: Testing multiple unrelated things
it("should handle tax calculation and validation", () => {
  const result = calculateTax(wealth, threshold, rate);
  expect(result.amount).toBe(2.5); // Testing calculation

  expect(() => calculateTax(invalidWealth, threshold, rate)).toThrow(); // Testing validation
  // Split into two tests ❌
});

// GOOD: One behavior per test
it("should calculate 2.5% tax when wealth exceeds threshold", () => {
  const result = calculateTax(wealth, threshold, rate);
  expect(result.amount).toBe(2.5);
});

it("should throw error for negative wealth", () => {
  expect(() => calculateTax(Money.usd(-100), threshold, rate)).toThrow();
});
```

### Mistake 3: Over-Mocking (London School Excess)

```typescript
// BAD: Mocking value objects (no side effects, pure data)
it("should calculate tax", () => {
  const mockMoney = jest.fn().mockReturnValue({ amount: 2.5 });
  const mockThreshold = jest.fn().mockReturnValue({ amount: 85 });
  // Mocking simple value objects adds no value ❌
});

// GOOD: Use real value objects
it("should calculate tax", () => {
  const wealth = Money.fromGold(100, "grams"); // Real object
  const threshold = Money.fromGold(85, "grams"); // Real object
  const tax = calculateTax(wealth, threshold, TaxRate.standard());
  expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});
```

## Summary

### Key Principles

1. **FIRST Properties**: Fast, Independent, Repeatable, Self-Validating, Thorough
2. **AAA Structure**: Arrange-Act-Assert for clarity
3. **Isolation**: No I/O, use test doubles for dependencies
4. **Descriptive Names**: Behavior-focused, not implementation-focused
5. **One Concept**: Each test verifies one logical behavior
6. **Pure Functions**: Easiest to test, no mocking needed

### Quick Reference: Anatomy of a Good Unit Test

```typescript
describe("FeatureName", () => {
  describe("methodOrBehavior", () => {
    it("should [expected behavior] when [context]", () => {
      // Arrange: Set up test data
      const input = createTestData();

      // Act: Execute behavior
      const result = systemUnderTest.method(input);

      // Assert: Verify outcome
      expect(result).toEqual(expectedValue);
    });
  });
});
```

### Unit Test Checklist

- [ ] Runs in milliseconds (no I/O)
- [ ] Independent (no shared state)
- [ ] Repeatable (deterministic)
- [ ] Self-validating (automated pass/fail)
- [ ] Tests one behavior
- [ ] Uses AAA pattern
- [ ] Descriptive name explaining behavior
- [ ] Verifies behavior, not implementation
- [ ] Isolated from external dependencies

### Next Steps

- **[05. Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md)**: Mocks, stubs, spies, fakes
- **[06. Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md)**: AAA, Given-When-Then, table-driven tests
- **[07. Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md)**: Builder pattern for test fixtures

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Test-Driven Development
- **Tags**: Unit Testing, Test Structure, AAA Pattern, Test Isolation, FIRST Principles, Test Naming
- **Related Files**:
  - [README](./README.md) - Documentation overview
  - [02. Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) - TDD workflow
  - [03. Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md) - Unit vs integration vs E2E
  - [05. Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md) - Mocking dependencies
- **Prerequisites**: [02. Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md), [03. Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)
- **Next Steps**: [05. Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md)
