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
it("should calculate zakat at 2.5% rate", () => {
  const wealth = Money.fromGold(100, "grams");
  const zakat = wealth.multiply(0.025);
  expect(zakat.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});

// BAD: Slow test (> 500ms)
it("should save zakat calculation to database", async () => {
  const wealth = Money.fromGold(100, "grams");
  await database.connect(); // Slow I/O
  await database.save({ wealth, zakat: wealth.multiply(0.025) });
  const result = await database.query("SELECT * FROM zakat");
  expect(result.rows[0].zakat).toBe(2.5);
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
describe("ZakatCalculator", () => {
  let calculator = new ZakatCalculator(); // Shared instance

  it("should set nisab threshold", () => {
    calculator.setNisab(Money.fromGold(85, "grams"));
    expect(calculator.nisab.amount).toBe(85);
  });

  it("should calculate zakat", () => {
    // Depends on previous test setting nisab ❌
    const zakat = calculator.calculate(Money.fromGold(100, "grams"));
    expect(zakat.amount).toBe(2.5);
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
it("should calculate zakat for current year", () => {
  const currentYear = new Date().getFullYear(); // Changes every year ❌
  const nisab = getNisabForYear(currentYear);
  expect(nisab).toBeDefined();
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
it("should reject negative nisab amount", () => {
  expect(() => NisabThreshold.fromGold(-85, "grams")).toThrow("Nisab cannot be negative");
});

// BAD: Requires manual verification
it("should reject negative nisab amount", () => {
  try {
    NisabThreshold.fromGold(-85, "grams");
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
describe("calculateZakat", () => {
  it("should return zero when wealth is below nisab", () => {
    // ARRANGE: Set up test data
    const wealth = Money.fromGold(50, "grams"); // Below nisab
    const nisab = Money.fromGold(85, "grams");
    const rate = ZakatRate.standard();

    // ACT: Execute the function
    const zakatAmount = calculateZakat(wealth, nisab, rate);

    // ASSERT: Verify the result
    expect(zakatAmount.equals(Money.zero("gold"))).toBe(true);
  });
});
```

### AAA with Comments (for Clarity)

Use comments to visually separate the three phases when tests are complex:

```typescript
it("should calculate profit for Murabaha contract", () => {
  // Arrange
  const costPrice = Money.usd(10000);
  const profitRate = Percentage.of(15);
  const contract = MurabahaContract.create(costPrice, profitRate);

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
it("should create zakat assessment with correct properties", () => {
  const assessment = ZakatAssessment.create(Money.fromGold(100, "grams"), Money.fromGold(85, "grams"));

  expect(assessment.wealth.amount).toBe(100);
  expect(assessment.nisab.amount).toBe(85);
  expect(assessment.meetsNisab()).toBe(true);
  // All assertions verify the "assessment creation" concept ✅
});

// BAD: Multiple unrelated concepts in one test
it("should handle zakat calculations and storage", () => {
  const zakat = calculateZakat(wealth, nisab, rate);
  expect(zakat.amount).toBe(2.5); // Testing calculation

  await repository.save(zakat);
  expect(await repository.findById(zakat.id)).toBeDefined();
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
class ZakatCalculator {
  calculate(wealth: Money): Money {
    const nisab = database.query("SELECT nisab FROM config"); // Hard-coded DB ❌
    return wealth.isGreaterThan(nisab) ? wealth.multiply(0.025) : Money.zero();
  }
}

// GOOD: Dependency injection (testable)
class ZakatCalculator {
  constructor(private nisabProvider: NisabProvider) {}

  calculate(wealth: Money): Money {
    const nisab = this.nisabProvider.getNisab();
    return wealth.isGreaterThan(nisab) ? wealth.multiply(0.025) : Money.zero();
  }
}

// Test with mock
it("should calculate zakat using provided nisab", () => {
  const mockNisabProvider = {
    getNisab: () => Money.fromGold(85, "grams"),
  };
  const calculator = new ZakatCalculator(mockNisabProvider);

  const zakat = calculator.calculate(Money.fromGold(100, "grams"));

  expect(zakat.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});
```

#### 2. Pure Functions (Best Isolation)

Pure functions have no dependencies—they're inherently isolated:

```typescript
// Pure function - perfect for unit testing
function calculateZakat(wealth: Money, nisab: Money, rate: ZakatRate): Money {
  if (wealth.isLessThan(nisab)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// Test is trivial - no mocking needed ✅
it("should calculate zakat", () => {
  const zakat = calculateZakat(Money.fromGold(100, "grams"), Money.fromGold(85, "grams"), ZakatRate.standard());
  expect(zakat.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});
```

#### 3. Test Doubles (Mocks, Stubs, Fakes)

When dependencies are necessary, use test doubles. See [05. Test Doubles](./ex-sode-tedrdeve__05-test-doubles.md) for detailed coverage.

**Quick Reference**:

- **Stub**: Returns canned responses
- **Mock**: Verifies interactions
- **Fake**: Simplified working implementation (in-memory database)

```typescript
// Using stub for HalalCertificationAuthority
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
describe("Nisab Threshold Validation", () => {
  it("should return true when wealth meets nisab threshold", () => {});
  it("should return true when wealth exceeds nisab threshold", () => {});
  it("should return false when wealth is below nisab threshold", () => {});
  it("should throw error when wealth and nisab have different currencies", () => {});
});

// BAD: Implementation-focused or vague
describe("Nisab Threshold Validation", () => {
  it("test1", () => {}); // Meaningless ❌
  it("nisab works", () => {}); // Vague ❌
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
it("should exempt personal residence from zakat calculation", () => {});
it("should apply 2.5% rate to gold and silver assets", () => {});
it("should verify Hawl period of one lunar year has passed", () => {});

// BAD: Technical jargon
it("should filter assets where taxable === false", () => {}); // Use "exempt" not "taxable === false"
it("should apply ZAKAT_RATE constant to metals array", () => {}); // Use "2.5% rate" not "ZAKAT_RATE constant"
```

## Assertions: Verifying Outcomes

Assertions are how tests express expected behavior. Good assertions are clear, specific, and fail with helpful messages.

### Common Assertion Patterns

#### 1. Equality Assertions

```typescript
// Primitive equality
expect(zakatAmount.amount).toBe(2.5);
expect(nisabMet).toBe(true);

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
expect(wealth.amount).toBeGreaterThanOrEqual(nisab.amount);
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
expect(zakatableAssets).toEqual(expect.arrayContaining([goldAsset, silverAsset]));
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
expect(zakatAmount).toEqualMoney(Money.fromGold(2.5, "grams"));
```

## Organizing Test Files

### File Structure

Match test file structure to source file structure:

```
src/
├── domain/
│   ├── money.ts
│   ├── zakat-calculator.ts
│   └── nisab-threshold.ts
└── domain/
    ├── __tests__/
    │   ├── money.spec.ts
    │   ├── zakat-calculator.spec.ts
    │   └── nisab-threshold.spec.ts
```

Or use co-located pattern:

```
src/
└── domain/
    ├── money.ts
    ├── money.spec.ts
    ├── zakat-calculator.ts
    ├── zakat-calculator.spec.ts
    ├── nisab-threshold.ts
    └── nisab-threshold.spec.ts
```

### Test Suite Organization

```typescript
// One describe block per class/module
describe("ZakatCalculator", () => {
  // Nested describe for each public method/behavior
  describe("calculate", () => {
    // Tests for happy path
    it("should calculate 2.5% zakat when wealth exceeds nisab", () => {});

    // Tests for edge cases
    it("should return zero when wealth equals nisab threshold", () => {});
    it("should handle very large wealth amounts", () => {});

    // Tests for error conditions
    it("should throw when wealth and nisab currencies differ", () => {});
    it("should throw when rate is negative", () => {});
  });

  describe("setNisab", () => {
    it("should update nisab threshold", () => {});
    it("should throw when nisab is negative", () => {});
  });
});
```

## Test Data: Builders and Fixtures

### Test Data Builders

Use builder pattern for complex test objects. See [07. Test Data Builders](./ex-sode-tedrdeve__07-test-data-builders.md) for details.

```typescript
// Builder for complex domain objects
class ZakatAssessmentBuilder {
  private wealth = Money.fromGold(100, "grams");
  private nisab = Money.fromGold(85, "grams");
  private date = HijriDate.now();

  withWealth(wealth: Money): this {
    this.wealth = wealth;
    return this;
  }

  withNisab(nisab: Money): this {
    this.nisab = nisab;
    return this;
  }

  withDate(date: HijriDate): this {
    this.date = date;
    return this;
  }

  build(): ZakatAssessment {
    return new ZakatAssessment(AssessmentId.generate(), this.wealth, this.nisab, this.date);
  }
}

// Usage in tests
it("should calculate zakat for assessment", () => {
  const assessment = new ZakatAssessmentBuilder()
    .withWealth(Money.fromGold(200, "grams"))
    .withNisab(Money.fromGold(85, "grams"))
    .build();

  const zakat = assessment.calculateZakat(ZakatRate.standard());

  expect(zakat.equals(Money.fromGold(5.0, "grams"))).toBe(true);
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
it("should calculate zakat for multiple assets", () => {
  const assets = [createGoldAsset(100), createSilverAsset(600)];
  // ...
});
```

## Testing Islamic Finance Domain: Examples

### Example 1: Zakat Calculation for Different Asset Types

```typescript
describe("Zakat Calculation", () => {
  describe("Gold and Silver Assets", () => {
    it("should apply 2.5% rate to gold when above 85g nisab", () => {
      const wealth = Money.fromGold(100, "grams");
      const nisab = Money.fromGold(85, "grams");
      const rate = ZakatRate.precious Metals(); // 2.5%

      const zakat = calculateZakat(wealth, nisab, rate);

      expect(zakat.equals(Money.fromGold(2.5, "grams"))).toBe(true);
    });

    it("should apply 2.5% rate to silver when above 595g nisab", () => {
      const wealth = Money.fromSilver(600, "grams");
      const nisab = Money.fromSilver(595, "grams");
      const rate = ZakatRate.preciousMetals();

      const zakat = calculateZakat(wealth, nisab, rate);

      expect(zakat.equals(Money.fromSilver(15.0, "grams"))).toBe(true);
    });
  });

  describe("Agricultural Produce", () => {
    it("should apply 10% rate to irrigated crops", () => {
      const harvest = AgricultureYield.fromTons(50, "wheat", "irrigated");

      const zakat = calculateAgriculturalZakat(harvest);

      expect(zakat.tons).toBe(5.0); // 10% of 50 tons
    });

    it("should apply 20% rate to rain-fed crops", () => {
      const harvest = AgricultureYield.fromTons(50, "wheat", "rain-fed");

      const zakat = calculateAgriculturalZakat(harvest);

      expect(zakat.tons).toBe(10.0); // 20% of 50 tons
    });
  });

  describe("Exemptions", () => {
    it("should exempt personal residence from wealth calculation", () => {
      const assets = [
        Asset.cash(Money.usd(50000)),
        Asset.personalResidence(Money.usd(300000)), // Exempt
        Asset.investmentProperty(Money.usd(200000)),
      ];

      const zakatableWealth = calculateZakatableWealth(assets);

      expect(zakatableWealth.equals(Money.usd(250000))).toBe(true);
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

### Example 3: Murabaha Contract Profit Calculation

```typescript
describe("Murabaha Contract", () => {
  describe("Profit Calculation", () => {
    it("should calculate selling price as cost plus profit margin", () => {
      const costPrice = Money.usd(10000);
      const profitRate = Percentage.of(15); // 15% profit

      const contract = MurabahaContract.create(costPrice, profitRate);

      expect(contract.sellingPrice.equals(Money.usd(11500))).toBe(true);
      expect(contract.profitAmount.equals(Money.usd(1500))).toBe(true);
    });

    it("should handle zero profit margin", () => {
      const costPrice = Money.usd(10000);
      const profitRate = Percentage.of(0);

      const contract = MurabahaContract.create(costPrice, profitRate);

      expect(contract.sellingPrice.equals(Money.usd(10000))).toBe(true);
      expect(contract.profitAmount.equals(Money.zero("USD"))).toBe(true);
    });
  });

  describe("Shariah Compliance", () => {
    it("should require bank ownership before sale", () => {
      const asset = Asset.create("car", Money.usd(20000));
      const buyer = Customer.create("Ahmad");

      expect(() => MurabahaContract.sellWithoutOwnership(asset, buyer)).toThrow(
        "Bank must own asset before selling in Murabaha",
      );
    });

    it("should prohibit interest-based pricing (Riba)", () => {
      const costPrice = Money.usd(10000);
      const interestRate = InterestRate.of(5); // Interest, not profit

      expect(() => MurabahaContract.create(costPrice, interestRate)).toThrow(RibaProhibitionError);
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
  calculateZakat(money, nisab, rate);
  expect(spy).toHaveBeenCalledWith(0.025); // Testing implementation ❌
});

// GOOD: Testing what it does
it("should calculate 2.5% zakat", () => {
  const zakat = calculateZakat(money, nisab, rate);
  expect(zakat.equals(Money.fromGold(2.5, "grams"))).toBe(true); // Testing behavior ✅
});
```

### Mistake 2: Multiple Behaviors in One Test

```typescript
// BAD: Testing multiple unrelated things
it("should handle zakat calculation and validation", () => {
  const result = calculateZakat(wealth, nisab, rate);
  expect(result.amount).toBe(2.5); // Testing calculation

  expect(() => calculateZakat(invalidWealth, nisab, rate)).toThrow(); // Testing validation
  // Split into two tests ❌
});

// GOOD: One behavior per test
it("should calculate 2.5% zakat when wealth exceeds nisab", () => {
  const result = calculateZakat(wealth, nisab, rate);
  expect(result.amount).toBe(2.5);
});

it("should throw error for negative wealth", () => {
  expect(() => calculateZakat(Money.usd(-100), nisab, rate)).toThrow();
});
```

### Mistake 3: Over-Mocking (London School Excess)

```typescript
// BAD: Mocking value objects (no side effects, pure data)
it("should calculate zakat", () => {
  const mockMoney = jest.fn().mockReturnValue({ amount: 2.5 });
  const mockNisab = jest.fn().mockReturnValue({ amount: 85 });
  // Mocking simple value objects adds no value ❌
});

// GOOD: Use real value objects
it("should calculate zakat", () => {
  const wealth = Money.fromGold(100, "grams"); // Real object
  const nisab = Money.fromGold(85, "grams"); // Real object
  const zakat = calculateZakat(wealth, nisab, ZakatRate.standard());
  expect(zakat.equals(Money.fromGold(2.5, "grams"))).toBe(true);
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

- **[05. Test Doubles](./ex-sode-tedrdeve__05-test-doubles.md)**: Mocks, stubs, spies, fakes
- **[06. Testing Patterns](./ex-sode-tedrdeve__06-testing-patterns.md)**: AAA, Given-When-Then, table-driven tests
- **[07. Test Data Builders](./ex-sode-tedrdeve__07-test-data-builders.md)**: Builder pattern for test fixtures

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Test-Driven Development
- **Tags**: Unit Testing, Test Structure, AAA Pattern, Test Isolation, FIRST Principles, Test Naming
- **Related Files**:
  - [README](./README.md) - Documentation overview
  - [02. Red-Green-Refactor Cycle](./ex-sode-tedrdeve__02-red-green-refactor-cycle.md) - TDD workflow
  - [03. Test Types and Pyramid](./ex-sode-tedrdeve__03-test-types-and-pyramid.md) - Unit vs integration vs E2E
  - [05. Test Doubles](./ex-sode-tedrdeve__05-test-doubles.md) - Mocking dependencies
- **Prerequisites**: [02. Red-Green-Refactor Cycle](./ex-sode-tedrdeve__02-red-green-refactor-cycle.md), [03. Test Types and Pyramid](./ex-sode-tedrdeve__03-test-types-and-pyramid.md)
- **Next Steps**: [05. Test Doubles](./ex-sode-tedrdeve__05-test-doubles.md)
