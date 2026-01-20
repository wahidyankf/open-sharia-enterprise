# Test-Driven Development: The Red-Green-Refactor Cycle

## Overview

The Red-Green-Refactor cycle is the heartbeat of Test-Driven Development. This three-phase rhythm—write a failing test (Red), make it pass (Green), improve the design (Refactor)—is the fundamental practice that distinguishes TDD from other development approaches.

Each cycle is intentionally small, typically taking just a few minutes. The discipline lies not in writing lots of code at once, but in taking tiny, verified steps forward. This incremental approach provides continuous feedback, maintains momentum, and builds confidence through observable progress.

Understanding this cycle deeply is essential to practicing TDD effectively. Each phase has specific goals, techniques, and common pitfalls. Mastering the Red-Green-Refactor rhythm transforms how you approach software development.

## The Three Phases

### Red Phase: Write a Failing Test

**Goal**: Specify **one** desired behavior through a failing test.

**Key Principles**:

1. **Write the test you wish you had**: Design the API from the caller's perspective
2. **Make it fail for the right reason**: Verify the test actually fails because functionality is missing
3. **Write only enough test to fail**: Don't test multiple behaviors at once
4. **Use descriptive test names**: Test name should describe the behavior being specified

**Why This Phase Matters**:

- Forces you to clarify **what** you're building before **how**
- Ensures tests actually test something (prevents false positives)
- Prevents writing unnecessary production code
- Drives interface design from consumer's perspective

#### Anatomy of a Good Failing Test

A well-written test follows the **Arrange-Act-Assert (AAA)** pattern:

```typescript
describe("Income Threshold Validation", () => {
  it("should return true when wealth meets or exceeds threshold", () => {
    // ARRANGE: Set up test data
    const wealth = Money.usd(100000);
    const threshold = Money.usd(50000);

    // ACT: Execute the behavior
    const meetsThreshold = wealth.meetsThreshold(threshold);

    // ASSERT: Verify the outcome
    expect(meetsThreshold).toBe(true);
  });
});

// RED PHASE: Test fails because Money.meetsThreshold doesn't exist yet
// Error: Property 'meetsThreshold' does not exist on type 'Money'
```

**Arrange** (Given): Set up the context

- Create test data (wealth, threshold amounts)
- Initialize dependencies (mocks, stubs if needed)
- Establish preconditions

**Act** (When): Execute the behavior being tested

- Call the method/function under test
- Perform the action we're specifying

**Assert** (Then): Verify the expected outcome

- Check the result matches expectations
- Verify side effects if applicable
- One logical assertion per test (though may be multiple assertion statements)

#### Test Naming Conventions

Good test names describe behavior in plain language:

```typescript
// GOOD: Describes behavior and context
describe("Tax Calculation", () => {
  it("should return zero when wealth is below income threshold", () => {
    /* ... */
  });
  it("should calculate 2.5% of wealth when above income threshold", () => {
    /* ... */
  });
  it("should throw error when wealth and threshold have different currencies", () => {
    /* ... */
  });
});

// BAD: Describes implementation or uses vague names
describe("Tax Calculation", () => {
  it("test1", () => {
    /* ... */
  });
  it("should call multiply method", () => {
    /* ... */
  });
  it("works correctly", () => {
    /* ... */
  });
});
```

**Naming Template**: `should [expected behavior] when [context/condition]`

#### Red Phase: Complete Example

Let's specify a Tax calculation feature using TDD:

```typescript
// RED PHASE: Write failing test for first behavior
describe("calculateTax", () => {
  it("should calculate 20% tax when wealth exceeds threshold", () => {
    // Arrange
    const wealth = Money.usd(100000);
    const threshold = Money.usd(50000);
    const taxRate = TaxRate.standard(); // 2.5%

    // Act
    const taxAmount = calculateTax(wealth, threshold, taxRate);

    // Assert
    expect(taxAmount.equals(Money.usd(20000))).toBe(true);
  });
});

// Test fails: ReferenceError: calculateTax is not defined
// This is correct - we've specified the behavior, now we implement it
```

#### Verify the Test Fails for the Right Reason

**Critical Step**: Run the test and verify it fails **because the functionality is missing**, not because of typos or incorrect test setup.

```typescript
// GOOD: Fails because calculateTax doesn't exist
// Error: calculateTax is not defined ✅

// BAD: Fails because of typo in test
it("should calculate tax", () => {
  expect(calculateZakt(wealth, threshold, rate)).toBe(2.5); // Typo: calculateZakt
});
// Error: calculateZakt is not defined ❌ (wrong reason)

// BAD: Fails because test data is wrong
it("should calculate tax", () => {
  const wealth = undefined; // Wrong test data
  expect(calculateTax(wealth, threshold, rate)).toBe(2.5);
});
// Error: Cannot read property of undefined ❌ (wrong reason)
```

**Best Practice**: Watch the test **fail** before writing any implementation. This confirms the test is actually testing something.

### Green Phase: Make it Pass (Simplest Implementation)

**Goal**: Write **just enough** code to make the failing test pass.

**Key Principles**:

1. **Fake it till you make it**: Hardcode values if needed to get to green quickly
2. **Obvious implementation**: If the solution is trivial, implement it directly
3. **Triangulation**: Add more test cases to force generalization
4. **No premature optimization**: Performance comes later in refactor phase
5. **No gold-plating**: Don't add features not covered by tests

**Why This Phase Matters**:

- Maintains rapid feedback loops (seconds to green, not hours)
- Prevents over-engineering (only write code that's actually needed)
- Keeps you focused on passing **this** test, not solving all future problems
- Builds confidence through incremental progress

#### Strategy 1: Fake It (Simplest Possible Implementation)

When starting, use the absolutely simplest approach—even if it's obviously incomplete:

```typescript
// GREEN PHASE: Fake it with hardcoded value
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  return Money.usd(20000); // Hardcoded! But test passes ✅
}

// Test passes ✅
// Yes, this is "cheating" - but it gets us to green quickly
// Next test will force us to generalize
```

**When to Fake It**: When you're uncertain about the implementation or the algorithm is complex. Faking gets you to green fast, then you add more tests to force the real implementation.

#### Strategy 2: Obvious Implementation

When the solution is straightforward, implement it directly:

```typescript
// GREEN PHASE: Obvious implementation
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// Test passes ✅
```

**When to Use Obvious Implementation**: When the logic is simple and clear. Don't fake it if the real implementation is just as quick to write.

#### Strategy 3: Triangulation (Force Generalization)

Add another test case with different inputs to force generalizing the implementation:

```typescript
// First test with hardcoded result
it("should calculate 20% tax for 100 grams of gold", () => {
  const wealth = Money.usd(100000);
  const threshold = Money.usd(50000);
  const rate = TaxRate.standard();

  const tax = calculateTax(wealth, threshold, rate);

  expect(tax.equals(Money.usd(20000))).toBe(true);
});

// Second test forces generalization
it("should calculate 20% tax for 200 grams of gold", () => {
  const wealth = Money.usd(200000);
  const threshold = Money.usd(50000);
  const rate = TaxRate.standard();

  const tax = calculateTax(wealth, threshold, rate);

  expect(tax.equals(Money.usd(40000))).toBe(true); // Different amount
});

// Hardcoded value no longer works - must implement real calculation
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage); // Real implementation ✅
}
```

**When to Triangulate**: When you're unsure of the algorithm or want to ensure your implementation is general, not specific to one test case.

#### Green Phase: Complete Example

Let's continue our Tax calculation TDD session:

```typescript
// RED PHASE: Test for wealth below threshold
it("should return zero when wealth is below income threshold", () => {
  const wealth = Money.usd(30000); // Below 85g threshold
  const threshold = Money.usd(50000);
  const rate = TaxRate.standard();

  const tax = calculateTax(wealth, threshold, rate);

  expect(tax.equals(Money.zero("USD"))).toBe(true);
});

// Test fails ❌: Expected 0 but got 1.25 (50 * 0.20)

// GREEN PHASE: Add threshold check
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency); // NEW: Handle below-threshold case
  }
  return wealth.multiply(rate.percentage);
}

// All tests pass ✅
```

```typescript
// RED PHASE: Test for currency mismatch
it("should throw error when wealth and threshold have different currencies", () => {
  const wealth = Money.usd(1000);
  const threshold = Money.usd(50000);
  const rate = TaxRate.standard();

  expect(() => calculateTax(wealth, threshold, rate)).toThrow("Cannot compare different currencies");
});

// Test fails ❌: No error thrown

// GREEN PHASE: Add currency validation
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (!wealth.hasSameCurrency(threshold)) {
    throw new CurrencyMismatchError("Cannot compare different currencies"); // NEW
  }
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// All tests pass ✅
```

#### Resist the Temptation to Add "Obvious" Features

**Common mistake**: Adding functionality not covered by current tests.

```typescript
// GREEN PHASE: Just make the test pass
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (!wealth.hasSameCurrency(threshold)) {
    throw new CurrencyMismatchError("Cannot compare different currencies");
  }
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// TEMPTATION: "I should add logging, caching, validation, etc."
// RESIST! Only add features when tests require them.
```

**TDD Rule**: Only write production code to pass a failing test. This prevents:

- Feature creep (YAGNI - You Ain't Gonna Need It)
- Untested code paths
- Complexity without clear requirements

### Refactor Phase: Improve Design While Keeping Tests Green

**Goal**: Improve code design and remove duplication **without changing behavior**.

**Key Principles**:

1. **Tests stay green**: Refactor only when all tests pass
2. **Change structure, not behavior**: Improve how code is organized, not what it does
3. **Small steps**: Refactor incrementally with frequent test runs
4. **Remove duplication**: DRY (Don't Repeat Yourself)
5. **Improve names**: Make code self-documenting
6. **Extract abstractions**: Create reusable components when patterns emerge

**Why This Phase Matters**:

- Prevents technical debt accumulation
- Improves code readability and maintainability
- Extracts reusable patterns
- Safely evolves design with test safety net

#### What to Refactor

**Code Smells to Address**:

1. **Duplication**: Same logic in multiple places
2. **Long functions**: Extract smaller, focused functions
3. **Poor names**: Unclear variable/function names
4. **Magic numbers**: Hardcoded values without explanation
5. **Complex conditionals**: Extract to named functions
6. **God objects**: Classes with too many responsibilities

**Refactoring Catalog**: Martin Fowler's "Refactoring" book lists 60+ refactorings. Common ones:

- Extract Method/Function
- Rename Variable/Function
- Extract Variable (for complex expressions)
- Replace Magic Number with Named Constant
- Extract Class
- Inline Temporary Variable

#### Refactor Phase: Examples

**Example 1: Extract Magic Number to Named Constant**

```typescript
// BEFORE REFACTORING: Magic number
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (!wealth.hasSameCurrency(threshold)) {
    throw new CurrencyMismatchError("Cannot compare different currencies");
  }
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(0.2); // What is 0.20? ❌
}

// Tests pass ✅

// AFTER REFACTORING: Named constant
const STANDARD_TAX_RATE = 0.2; // 20%

function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (!wealth.hasSameCurrency(threshold)) {
    throw new CurrencyMismatchError("Cannot compare different currencies");
  }
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(STANDARD_TAX_RATE); // Clear intent ✅
}

// Tests still pass ✅
```

**Example 2: Extract Validation to Separate Function**

```typescript
// BEFORE REFACTORING: Complex function
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (!wealth.hasSameCurrency(threshold)) {
    throw new CurrencyMismatchError("Cannot compare different currencies");
  }
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// Tests pass ✅

// AFTER REFACTORING: Extract validation
function validateCurrency(wealth: Money, threshold: Money): void {
  if (!wealth.hasSameCurrency(threshold)) {
    throw new CurrencyMismatchError("Cannot compare different currencies");
  }
}

function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  validateCurrency(wealth, threshold);

  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// Tests still pass ✅
// calculateTax is now clearer - validation separated from calculation
```

**Example 3: Extract Class for Complex Logic**

```typescript
// BEFORE REFACTORING: All logic in one function
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  if (!wealth.hasSameCurrency(threshold)) {
    throw new CurrencyMismatchError("Cannot compare different currencies");
  }
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(rate.percentage);
}

// Tests pass ✅

// AFTER REFACTORING: Extract TaxCalculator class
class TaxCalculator {
  constructor(
    private threshold: Money,
    private rate: TaxRate,
  ) {}

  calculate(wealth: Money): Money {
    this.validateCurrency(wealth);

    if (this.isBelowThreshold(wealth)) {
      return Money.zero(wealth.currency);
    }

    return this.applyTaxRate(wealth);
  }

  private validateCurrency(wealth: Money): void {
    if (!wealth.hasSameCurrency(this.threshold)) {
      throw new CurrencyMismatchError("Cannot compare different currencies");
    }
  }

  private isBelowThreshold(wealth: Money): boolean {
    return wealth.isLessThan(this.threshold);
  }

  private applyTaxRate(wealth: Money): Money {
    return wealth.multiply(this.rate.percentage);
  }
}

// Update function to use class
function calculateTax(wealth: Money, threshold: Money, rate: TaxRate): Money {
  const calculator = new TaxCalculator(threshold, rate);
  return calculator.calculate(wealth);
}

// Tests still pass ✅
// Logic now organized into cohesive class with intention-revealing method names
```

#### When to Refactor

**Refactor when**:

- All tests are green ✅
- You notice duplication
- Code is hard to understand
- You see an opportunity to simplify

**Don't refactor when**:

- Tests are failing ❌ (finish getting to green first)
- You're in the middle of Red or Green phase
- You're adding new functionality (refactor **after** new test passes)

**Refactoring Rhythm**:

```
Red → Green → Refactor → Red → Green → Refactor → ...
        ✅       Clean       ❌      ✅       Clean
```

Each cycle should be quick (2-5 minutes). Don't accumulate technical debt across cycles—clean as you go.

#### Refactoring Safety: Keep Tests Green

The cardinal rule of refactoring: **Run tests frequently during refactoring**.

```typescript
// REFACTORING WORKFLOW:

// 1. All tests pass ✅
npm test

// 2. Make small refactoring change (e.g., rename variable)
const taxableWealth = wealth; // Renamed from 'w'

// 3. Run tests ✅
npm test

// 4. Make another small change (e.g., extract function)
function isExempt(wealth: Money, threshold: Money): boolean {
  return wealth.isLessThan(threshold);
}

// 5. Run tests ✅
npm test

// Repeat: Small change → Test → Small change → Test
```

**Why small steps matter**: If tests suddenly fail, you know the exact change that broke them (the last refactoring). If you make 10 changes before testing, debugging is harder.

## Complete Red-Green-Refactor Example: IncomeThreshold Value Object

Let's apply the full cycle to build a `IncomeThreshold` value object for Tax calculations.

### Cycle 1: Create IncomeThreshold

**RED PHASE: Write failing test**

```typescript
describe("IncomeThreshold", () => {
  it("should create income threshold for standard income", () => {
    // Arrange & Act
    const threshold = IncomeThreshold.forStandardIncome(85, "grams");

    // Assert
    expect(threshold.amount).toBe(85);
    expect(threshold.assetType).toBe("gold");
  });
});

// Test fails ❌: IncomeThreshold is not defined
```

**GREEN PHASE: Simplest implementation**

```typescript
class IncomeThreshold {
  constructor(
    readonly amount: number,
    readonly assetType: string,
  ) {}

  static forStandardIncome(amount: number, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, "gold");
  }
}

// Test passes ✅
```

**REFACTOR PHASE: No duplication or complexity yet - skip**

### Cycle 2: Add Silver Threshold

**RED PHASE: Write failing test**

```typescript
it("should create income threshold for capital gains", () => {
  const threshold = IncomeThreshold.forCapitalGains(75000);

  expect(threshold.amount).toBe(595);
  expect(threshold.assetType).toBe("capital-gains");
});

// Test fails ❌: IncomeThreshold.forCapitalGains is not defined
```

**GREEN PHASE: Add forCapitalGains method**

```typescript
class IncomeThreshold {
  constructor(
    readonly amount: number,
    readonly assetType: string,
  ) {}

  static forStandardIncome(amount: number, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, "gold");
  }

  static forCapitalGains(amount: number, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, "capital-gains");
  }
}

// Test passes ✅
```

**REFACTOR PHASE: Extract duplication in factory methods**

```typescript
class IncomeThreshold {
  constructor(
    readonly amount: number,
    readonly assetType: string,
  ) {}

  static forStandardIncome(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "gold", unit);
  }

  static forCapitalGains(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "capital-gains", unit);
  }

  private static create(amount: number, assetType: string, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, assetType);
  }
}

// Tests still pass ✅
```

### Cycle 3: Prevent Negative Threshold

**RED PHASE: Write failing test**

```typescript
it("should throw error for negative threshold amount", () => {
  expect(() => IncomeThreshold.forStandardIncome(-50000)).toThrow("Threshold amount cannot be negative");
});

// Test fails ❌: Expected error to be thrown, but got IncomeThreshold object
```

**GREEN PHASE: Add validation**

```typescript
class IncomeThreshold {
  constructor(
    readonly amount: number,
    readonly assetType: string,
  ) {
    if (amount < 0) {
      throw new Error("Threshold amount cannot be negative");
    }
  }

  static forStandardIncome(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "gold", unit);
  }

  static forCapitalGains(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "capital-gains", unit);
  }

  private static create(amount: number, assetType: string, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, assetType);
  }
}

// Test passes ✅
```

**REFACTOR PHASE: Extract custom error class**

```typescript
class InvalidThresholdError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "InvalidThresholdError";
  }
}

class IncomeThreshold {
  constructor(
    readonly amount: number,
    readonly assetType: string,
  ) {
    if (amount < 0) {
      throw new InvalidThresholdError("Threshold amount cannot be negative");
    }
  }

  static forStandardIncome(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "gold", unit);
  }

  static forCapitalGains(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "capital-gains", unit);
  }

  private static create(amount: number, assetType: string, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, assetType);
  }
}

// Tests still pass ✅
```

### Cycle 4: Comparison Between Wealth and Threshold

**RED PHASE: Write failing test**

```typescript
it("should compare if wealth meets income threshold", () => {
  const threshold = IncomeThreshold.forStandardIncome(85, "grams");
  const wealth = Money.usd(100000);

  const meetsThreshold = threshold.isMetBy(wealth);

  expect(meetsThreshold).toBe(true);
});

// Test fails ❌: threshold.isMetBy is not a function
```

**GREEN PHASE: Add comparison method**

```typescript
class IncomeThreshold {
  constructor(
    readonly amount: number,
    readonly assetType: string,
  ) {
    if (amount < 0) {
      throw new InvalidThresholdError("Threshold amount cannot be negative");
    }
  }

  static forStandardIncome(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "gold", unit);
  }

  static forCapitalGains(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "capital-gains", unit);
  }

  private static create(amount: number, assetType: string, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, assetType);
  }

  isMetBy(wealth: Money): boolean {
    return wealth.amount >= this.amount;
  }
}

// Test passes ✅
```

**REFACTOR PHASE: No obvious improvements - skip**

### Final Result After 4 Cycles

```typescript
class InvalidThresholdError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "InvalidThresholdError";
  }
}

class IncomeThreshold {
  constructor(
    readonly amount: number,
    readonly assetType: string,
  ) {
    if (amount < 0) {
      throw new InvalidThresholdError("Threshold amount cannot be negative");
    }
  }

  static forStandardIncome(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "gold", unit);
  }

  static forCapitalGains(amount: number, unit: string): IncomeThreshold {
    return IncomeThreshold.create(amount, "capital-gains", unit);
  }

  private static create(amount: number, assetType: string, unit: string): IncomeThreshold {
    return new IncomeThreshold(amount, assetType);
  }

  isMetBy(wealth: Money): boolean {
    return wealth.amount >= this.amount;
  }
}

// Test suite (12 assertions across 4 tests)
describe("IncomeThreshold", () => {
  it("should create income threshold for standard income", () => {
    const threshold = IncomeThreshold.forStandardIncome(85, "grams");
    expect(threshold.amount).toBe(85);
    expect(threshold.assetType).toBe("gold");
  });

  it("should create income threshold for capital gains", () => {
    const threshold = IncomeThreshold.forCapitalGains(75000);
    expect(threshold.amount).toBe(595);
    expect(threshold.assetType).toBe("capital-gains");
  });

  it("should throw error for negative threshold amount", () => {
    expect(() => IncomeThreshold.forStandardIncome(-50000)).toThrow("Threshold amount cannot be negative");
  });

  it("should compare if wealth meets income threshold", () => {
    const threshold = IncomeThreshold.forStandardIncome(85, "grams");
    const wealth = Money.usd(100000);
    expect(threshold.isMetBy(wealth)).toBe(true);
  });
});

// All tests pass ✅
// Code evolved incrementally through 4 Red-Green-Refactor cycles
```

## Common Mistakes and How to Avoid Them

### Mistake 1: Skipping the Red Phase

**Problem**: Writing implementation without first writing a failing test.

**Why it's bad**:

- No verification that test actually tests anything
- Risk of false positives (test that always passes)
- Lose design benefits of test-first thinking

**Solution**: Always see red before green.

```typescript
// BAD: Write implementation first
function calculateTax(wealth: Money): Money {
  return wealth.multiply(0.2);
}

// Then write test
it("should calculate tax", () => {
  const tax = calculateTax(Money.usd(100));
  expect(tax.amount).toBe(2.5);
}); // Passes, but did we verify it fails first? ❌

// GOOD: Write test first, watch it fail
it("should calculate tax", () => {
  const tax = calculateTax(Money.usd(100));
  expect(tax.amount).toBe(2.5);
});
// FAIL: calculateTax is not defined ✅ (correct)

// Then implement
function calculateTax(wealth: Money): Money {
  return wealth.multiply(0.2);
}
// PASS ✅
```

### Mistake 2: Writing Too Much in Green Phase

**Problem**: Implementing multiple features when only one test is failing.

**Why it's bad**:

- Untested code paths
- Loss of incremental progress
- Harder to debug when tests break

**Solution**: Only write code needed to pass the current failing test.

```typescript
// BAD: Implementing too much at once
// Only one failing test: "should calculate tax for gold"
function calculateTax(wealth: Money, threshold: Money): Money {
  // Unnecessary currency conversion not tested yet
  const normalizedWealth = this.convertToBaseCurrency(wealth);
  const normalizedThreshold = this.convertToBaseCurrency(threshold);

  // Unnecessary caching not tested yet
  const cacheKey = `${normalizedWealth.amount}-${normalizedThreshold.amount}`;
  if (this.cache.has(cacheKey)) {
    return this.cache.get(cacheKey);
  }

  // Unnecessary logging not tested yet
  this.logger.info(`Calculating tax for ${wealth.amount}`);

  // The only logic actually tested
  if (normalizedWealth.isLessThan(normalizedThreshold)) {
    return Money.zero();
  }
  return normalizedWealth.multiply(0.2);
}

// GOOD: Only implement what's tested
function calculateTax(wealth: Money, threshold: Money): Money {
  if (wealth.isLessThan(threshold)) {
    return Money.zero(wealth.currency);
  }
  return wealth.multiply(0.2);
}
// Add caching, logging, conversion later when tests require them
```

### Mistake 3: Refactoring While Red or Not Testing During Refactoring

**Problem**: Changing code structure while tests are failing, or making many refactoring changes without running tests.

**Why it's bad**:

- If tests fail, unclear if it's the refactoring or the feature implementation
- Risk of breaking working code during refactoring

**Solution**: Only refactor when green. Run tests after each small refactoring step.

```typescript
// BAD: Refactoring while red
it("should calculate tax", () => {
  /* test code */
});
// FAIL ❌

// Start refactoring variable names, extracting functions...
// Tests still failing, now hard to know if refactoring broke something ❌

// GOOD: Refactor only when green
it("should calculate tax", () => {
  /* test code */
});
// FAIL ❌

// First, get to green
function calculateTax(w: Money, n: Money): Money {
  return w.multiply(0.2);
}
// PASS ✅

// Now refactor
function calculateTax(wealth: Money, threshold: Money): Money {
  // Renamed parameters
  return wealth.multiply(0.2);
}
// PASS ✅ (run tests after each small refactoring)
```

### Mistake 4: Testing Implementation Details Instead of Behavior

**Problem**: Tests verify **how** code works internally, not **what** it does externally.

**Why it's bad**:

- Tests break when refactoring (brittle tests)
- Tests don't document actual behavior
- Refactoring becomes impossible

**Solution**: Test public API behavior, not internal implementation.

```typescript
// BAD: Testing implementation details
class TaxCalculator {
  private thresholdAmount: Money;

  calculate(wealth: Money): Money {
    return wealth.isGreaterThan(this.thresholdAmount) ? wealth.multiply(0.2) : Money.zero();
  }
}

// Bad test: Checks internal state
it("should set thresholdAmount", () => {
  const calculator = new TaxCalculator(Money.fromGold(85));
  expect(calculator["thresholdAmount"].amount).toBe(85); // Accessing private field ❌
});

// GOOD: Testing public behavior
it("should calculate tax when wealth exceeds threshold", () => {
  const calculator = new TaxCalculator(Money.fromGold(85));
  const tax = calculator.calculate(Money.fromGold(100));

  expect(tax.equals(Money.fromGold(2.5))).toBe(true); // Testing outcome ✅
});
```

### Mistake 5: Large Refactorings Without Small Steps

**Problem**: Making many refactoring changes at once without running tests between steps.

**Why it's bad**:

- If tests break, hard to identify which change caused the failure
- Increased risk of introducing bugs

**Solution**: Refactor in tiny increments with test runs between each change.

```typescript
// BAD: Multiple refactorings at once
// Before
function calc(w, n) {
  if (w < n) return 0;
  return w * 0.2;
}

// After (many changes at once)
class TaxCalculator {
  constructor(private thresholdThreshold: IncomeThreshold) {}

  calculate(wealth: Money): Money {
    if (this.isBelowThreshold(wealth)) {
      return Money.zero(wealth.currency);
    }
    return this.applyTaxRate(wealth);
  }

  private isBelowThreshold(wealth: Money): boolean {
    return this.thresholdThreshold.isMetBy(wealth);
  }

  private applyTaxRate(wealth: Money): Money {
    return wealth.multiply(TaxRate.STANDARD);
  }
}
// Tests fail ❌ - which change broke it?

// GOOD: Incremental refactorings with tests between each
// Step 1: Rename variables
function calc(wealth, threshold) {
  if (wealth < threshold) return 0;
  return wealth * 0.2;
}
// Run tests ✅

// Step 2: Extract constant
const TAX_RATE = 0.2;
function calc(wealth, threshold) {
  if (wealth < threshold) return 0;
  return wealth * TAX_RATE;
}
// Run tests ✅

// Step 3: Rename function
function calculateTax(wealth, threshold) {
  if (wealth < threshold) return 0;
  return wealth * TAX_RATE;
}
// Run tests ✅

// Continue with small, verified steps...
```

## TDD Rhythm and Flow

### The Steady Drumbeat

TDD works best when you maintain a steady rhythm:

```
Red (30 seconds - 2 minutes)
  ↓
Green (1-5 minutes)
  ↓
Refactor (1-3 minutes)
  ↓
Red (next behavior)
  ↓
...
```

**Each cycle should be quick**. If you're spending 20 minutes writing one test or one implementation, the step is too large.

### Signs You're in the TDD Flow

- Tests run constantly (every 1-2 minutes)
- Always know the next small step
- Steady accumulation of features
- No long debugging sessions
- Codebase stays clean through frequent refactoring

### Signs You're Out of Flow

- Haven't run tests in 15+ minutes
- Uncertain what to do next
- Debugging complex failures
- Accumulating duplication and code smells
- Feeling stuck or overwhelmed

**Recovery**: Stop, take a step back, and return to the smallest possible next test.

## Summary

The Red-Green-Refactor cycle is the heartbeat of TDD:

1. **Red**: Write a failing test that specifies one behavior
2. **Green**: Write the simplest code to make it pass
3. **Refactor**: Improve design while keeping tests green

**Key Principles**:

- Take small steps (each cycle = 2-10 minutes)
- See red before green (verify tests fail for right reason)
- Simplest implementation first (no gold-plating)
- Refactor frequently (clean as you go)
- Run tests constantly (every 1-2 minutes)

This rhythm produces code that is:

- **Thoroughly tested** (every line has a reason to exist)
- **Well-designed** (refactored continuously)
- **Documented** (tests show how it works)
- **Safe to change** (comprehensive test suite)

Master this cycle, and you've mastered TDD.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Test-Driven Development
- **Tags**: TDD, Red-Green-Refactor, Testing Workflow, Incremental Development, Refactoring, Test-First
- **Related Files**:
  - [README](./README.md) - Documentation overview
  - [01. Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md) - TDD foundations
  - [04. Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Test structure and isolation
  - [14. Refactoring with Tests](./ex-so-de-tedrdeve__14-refactoring-with-tests.md) - Advanced refactoring patterns
- **Prerequisites**: [01. Introduction and Philosophy](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)
- **Next Steps**: [03. Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md) or [04. Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md)
