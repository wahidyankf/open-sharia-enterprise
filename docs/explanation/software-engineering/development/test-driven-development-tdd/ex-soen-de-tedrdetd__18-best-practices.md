# TDD Best Practices

**Created**: 2026-01-20
**Updated**: 2026-01-20
**Status**: Active

> **Companion Document**: For common pitfalls to avoid, see [TDD Antipatterns](ex-soen-de-tedrdetd__19-anti-patterns.md)

## Overview

Test-Driven Development (TDD) is a disciplined approach to software development where tests drive design and implementation decisions. When practiced correctly, TDD produces clean, maintainable code with comprehensive test coverage.

This guide presents proven best practices that maximize TDD's value. Understanding and applying these practices helps teams build robust test suites that support rapid, confident iteration.

**Value of TDD Best Practices:**

- **Design Feedback**: Tests reveal design problems before implementation solidifies
- **Living Documentation**: Tests serve as executable specifications of behavior
- **Regression Safety**: Comprehensive coverage catches breaking changes immediately
- **Refactoring Confidence**: Well-structured tests enable fearless code improvements
- **Faster Debugging**: Failures pinpoint exact problem locations
- **Team Velocity**: Initial investment pays dividends through reduced debugging time

## Core Principles

TDD best practices embody foundational principles from this repository's [software engineering principles](../../../../../governance/principles/software-engineering/):

**[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)**: TDD automates verification through comprehensive test suites. Best practices ensure tests run automatically (CI/CD integration), catch errors early (pre-commit hooks), and provide fast feedback loops. Automation eliminates manual verification and enables continuous quality assurance.

**[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)**: Well-written tests make behavior explicit. Descriptive test names, clear assertions, and focused tests document exactly what code should do. Tests serve as executable specifications that never go stale—explicit requirements enforced by automation.

**[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)**: Best practices favor simple, focused tests over complex, multi-purpose ones. One behavior per test, minimal setup, straightforward assertions. Start simple (Red-Green), add complexity only when needed (Refactor).

These principles guide TDD practices toward maintainable, reliable test suites that support long-term development velocity.

## Best Practices

### 1. Follow the Red-Green-Refactor Cycle Strictly

**Practice**: Adhere to the three-phase TDD rhythm without shortcuts.

**Red Phase**: Write a failing test that defines desired behavior. The test should fail for the right reason (missing functionality, not syntax errors).

**Green Phase**: Write the minimum code necessary to make the test pass. Resist the urge to add extra functionality.

**Refactor Phase**: Improve code structure while keeping tests green. Both production code and test code should be refactored.

**Why It Matters**: This cycle ensures tests genuinely verify new functionality rather than existing code. Skipping the red phase risks writing tests that always pass. This embodies **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)**—seeing the test fail first verifies that your automated check actually detects the missing functionality.

**Example: Tax Calculation**

```typescript
// ❌ WRONG: Writing implementation first
class TaxCalculator {
  calculateTax(wealth: number): number {
    const threshold = 85 * 4.25; // 85 grams of gold at $4.25/gram
    const taxRate = 0.025; // 2.5%

    return wealth >= threshold ? wealth * taxRate : 0;
  }
}

// Then writing test...

// ✅ RIGHT: Red phase first
describe("TaxCalculator", () => {
  it("should return 0 for wealth below threshold", () => {
    const calculator = new TaxCalculator();
    const result = calculator.calculateTax(300); // Below threshold

    expect(result).toBe(0); // This FAILS - good!
  });
});

// Green phase: Minimal implementation
class TaxCalculator {
  calculateTax(wealth: number): number {
    return 0; // Simplest code to pass
  }
}

// Next test drives real logic
it("should calculate 2.5% for wealth at threshold", () => {
  const calculator = new TaxCalculator();
  const threshold = 85 * 4.25; // ~361.25
  const result = calculator.calculateTax(threshold);

  expect(result).toBe(threshold * 0.025); // Fails - drives implementation
});

// Refactor phase: Extract constants, improve naming
class TaxCalculator {
  private static readonly GOLD_GRAMS_FOR_THRESHOLD = 85;
  private static readonly GOLD_PRICE_PER_GRAM = 4.25;
  private static readonly TAX_RATE = 0.025;

  private static get threshold(): number {
    return this.GOLD_GRAMS_FOR_THRESHOLD * this.GOLD_PRICE_PER_GRAM;
  }

  calculateTax(wealth: number): number {
    if (wealth < TaxCalculator.threshold) {
      return 0;
    }
    return wealth * TaxCalculator.TAX_RATE;
  }
}
```

**Checklist**:

- [ ] Test fails before implementation exists
- [ ] Test fails for expected reason (not syntax/setup error)
- [ ] Implementation is minimal to pass test
- [ ] Refactoring maintains green state
- [ ] Both test and production code are refactored

### 2. Keep Tests Small and Focused

**Practice**: Each test verifies one specific aspect of behavior. Tests should have a single logical assertion or closely related group of assertions.

**Why It Matters**: Focused tests provide precise failure messages, are easier to understand, and resist becoming brittle. When a test fails, you immediately know what broke.

**Example: Hijri Date Validation**

```typescript
// ❌ WRONG: Giant test
it("should validate hijri dates", () => {
  const validator = new HijriDateValidator();

  expect(validator.isValid(1, 1, 1445)).toBe(true); // Valid date
  expect(validator.isValid(13, 1, 1445)).toBe(false); // Invalid month
  expect(validator.isValid(1, 31, 1445)).toBe(false); // Invalid day
  expect(validator.isValid(1, 1, 0)).toBe(false); // Invalid year
  expect(validator.getDaysInMonth(1, 1445)).toBe(30); // Different behavior!
  expect(validator.getDaysInMonth(12, 1445)).toBe(29); // Different behavior!
});

// ✅ RIGHT: Focused tests
describe("HijriDateValidator", () => {
  describe("isValid", () => {
    it("should accept valid date components", () => {
      const validator = new HijriDateValidator();
      const result = validator.isValid(1, 1, 1445);

      expect(result).toBe(true);
    });

    it("should reject month above 12", () => {
      const validator = new HijriDateValidator();
      const result = validator.isValid(13, 1, 1445);

      expect(result).toBe(false);
    });

    it("should reject month below 1", () => {
      const validator = new HijriDateValidator();
      const result = validator.isValid(0, 1, 1445);

      expect(result).toBe(false);
    });

    it("should reject day exceeding month length", () => {
      const validator = new HijriDateValidator();
      const result = validator.isValid(1, 31, 1445); // Muharram has 30 days

      expect(result).toBe(false);
    });

    it("should reject year of 0", () => {
      const validator = new HijriDateValidator();
      const result = validator.isValid(1, 1, 0);

      expect(result).toBe(false);
    });
  });

  describe("getDaysInMonth", () => {
    it("should return 30 days for odd months 1-11", () => {
      const validator = new HijriDateValidator();
      const result = validator.getDaysInMonth(1, 1445);

      expect(result).toBe(30);
    });

    it("should return 29 days for Dhul Hijjah in common year", () => {
      const validator = new HijriDateValidator();
      const result = validator.getDaysInMonth(12, 1445);

      expect(result).toBe(29);
    });
  });
});
```

**Checklist**:

- [ ] Test name describes one specific behavior
- [ ] Test exercises one code path
- [ ] Failure message pinpoints exact problem
- [ ] Test can be understood in isolation
- [ ] Multiple related assertions are for same behavior

### 3. Start Simple and Stay Consistent

**Practice**: Begin with the simplest possible test case. Progress incrementally to more complex scenarios. Maintain consistent naming, structure, and assertion patterns across the test suite.

**Why It Matters**: Simple starting points build confidence and momentum. Consistency reduces cognitive load when reading or maintaining tests.

**Example: Loan Profit Calculation**

```typescript
// ✅ RIGHT: Start with simplest case
describe("LoanProfitCalculator", () => {
  // Test 1: Simplest possible case
  it("should calculate profit for 1 year term with 10% markup", () => {
    const calculator = new LoanProfitCalculator();
    const cost = 1000;
    const markupRate = 0.1; // 10%
    const termYears = 1;

    const profit = calculator.calculate(cost, markupRate, termYears);

    expect(profit).toBe(100); // 1000 * 0.10 * 1
  });

  // Test 2: Build on simplest case
  it("should calculate profit for multi-year term", () => {
    const calculator = new LoanProfitCalculator();
    const cost = 1000;
    const markupRate = 0.1;
    const termYears = 3;

    const profit = calculator.calculate(cost, markupRate, termYears);

    expect(profit).toBe(300); // 1000 * 0.10 * 3
  });

  // Test 3: Add complexity incrementally
  it("should calculate profit for fractional year term", () => {
    const calculator = new LoanProfitCalculator();
    const cost = 1000;
    const markupRate = 0.1;
    const termYears = 0.5; // 6 months

    const profit = calculator.calculate(cost, markupRate, termYears);

    expect(profit).toBe(50); // 1000 * 0.10 * 0.5
  });

  // Test 4: Edge cases come after happy path
  it("should return 0 profit for 0% markup rate", () => {
    const calculator = new LoanProfitCalculator();
    const cost = 1000;
    const markupRate = 0;
    const termYears = 1;

    const profit = calculator.calculate(cost, markupRate, termYears);

    expect(profit).toBe(0);
  });
});

// Consistent structure across all tests:
// 1. Arrange: Create calculator and inputs
// 2. Act: Call calculate method
// 3. Assert: Verify profit matches expected value
```

**Consistency Patterns**:

```typescript
// Consistent naming convention
describe("ClassName", () => {
  describe("methodName", () => {
    it("should [expected behavior] when [condition]", () => {
      // Test body
    });
  });
});

// Consistent test structure (Arrange-Act-Assert)
it("should ...", () => {
  // Arrange
  const instance = new ClassName();
  const input = setupInput();

  // Act
  const result = instance.method(input);

  // Assert
  expect(result).toBe(expected);
});
```

**Checklist**:

- [ ] First test covers simplest case
- [ ] Subsequent tests build complexity incrementally
- [ ] All tests follow consistent naming convention
- [ ] All tests use consistent structure (AAA pattern)
- [ ] Assertion style is uniform across suite

### 4. Focus on Behavior Over Implementation

**Practice**: Test what the code does (outputs, side effects, interactions), not how it does it (internal algorithms, data structures, private methods).

**Why It Matters**: Behavior-focused tests survive refactoring. Implementation-focused tests break with every internal change, creating maintenance burden without safety benefits.

**Example: Qibla Direction Calculator**

```typescript
// ❌ WRONG: Testing implementation details
describe("QiblaCalculator", () => {
  it("should use Haversine formula for distance calculation", () => {
    const calculator = new QiblaCalculator();
    const spy = jest.spyOn(calculator as any, "haversineDistance");

    calculator.getQiblaDirection(40.7128, -74.006); // New York

    expect(spy).toHaveBeenCalled(); // Fragile! Breaks if algorithm changes
  });

  it("should store Kaaba coordinates in private field", () => {
    const calculator = new QiblaCalculator();

    expect((calculator as any).kaabaLatitude).toBe(21.4225);
    expect((calculator as any).kaabaLongitude).toBe(39.8262);
  });
});

// ✅ RIGHT: Testing behavior
describe("QiblaCalculator", () => {
  it("should return northeast direction for New York coordinates", () => {
    const calculator = new QiblaCalculator();
    const latitude = 40.7128; // New York
    const longitude = -74.006;

    const direction = calculator.getQiblaDirection(latitude, longitude);

    // Test observable behavior: direction falls in northeast quadrant
    expect(direction.bearingDegrees).toBeGreaterThan(0);
    expect(direction.bearingDegrees).toBeLessThan(90);
    expect(direction.compassDirection).toBe("NE");
  });

  it("should return consistent direction for same coordinates", () => {
    const calculator = new QiblaCalculator();
    const latitude = 40.7128;
    const longitude = -74.006;

    const direction1 = calculator.getQiblaDirection(latitude, longitude);
    const direction2 = calculator.getQiblaDirection(latitude, longitude);

    expect(direction1.bearingDegrees).toBe(direction2.bearingDegrees);
  });

  it("should calculate different directions for different hemispheres", () => {
    const calculator = new QiblaCalculator();
    const sydney = calculator.getQiblaDirection(-33.8688, 151.2093); // Australia
    const london = calculator.getQiblaDirection(51.5074, -0.1278); // UK

    expect(sydney.bearingDegrees).not.toBe(london.bearingDegrees);
  });
});
```

**Behavior vs Implementation**:

| Behavior (Test This)    | Implementation (Don't Test) |
| ----------------------- | --------------------------- |
| Return values           | Private method calls        |
| Thrown exceptions       | Internal data structures    |
| State changes (public)  | Algorithm choice            |
| Observable side effects | Variable names              |
| Integration outputs     | Calculation intermediates   |

**Checklist**:

- [ ] Tests verify public API only
- [ ] Tests don't access private fields/methods
- [ ] Tests don't mock internal collaborators
- [ ] Tests survive algorithm changes
- [ ] Tests describe "what" not "how"

### 5. Achieve Comprehensive Coverage

**Practice**: Test positive cases, negative cases, boundary values, and edge cases. Coverage should encompass business logic thoroughly, not just achieve metric targets.

**Why It Matters**: Real-world bugs often hide in edge cases and error conditions. Comprehensive coverage catches these before production deployment.

**Example: Sadaqah Donation Validator**

```typescript
describe("SadaqahDonationValidator", () => {
  // Positive cases
  describe("valid donations", () => {
    it("should accept minimum donation amount", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: 1, currency: "USD" });

      expect(result.isValid).toBe(true);
    });

    it("should accept large donation amounts", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: 1000000, currency: "USD" });

      expect(result.isValid).toBe(true);
    });

    it("should accept supported currencies", () => {
      const validator = new SadaqahDonationValidator();
      const currencies = ["USD", "EUR", "GBP", "SAR"];

      currencies.forEach((currency) => {
        const result = validator.validate({ amount: 100, currency });
        expect(result.isValid).toBe(true);
      });
    });
  });

  // Negative cases
  describe("invalid donations", () => {
    it("should reject negative amounts", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: -100, currency: "USD" });

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain("Amount must be positive");
    });

    it("should reject zero amount", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: 0, currency: "USD" });

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain("Amount must be greater than zero");
    });

    it("should reject unsupported currencies", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: 100, currency: "XXX" });

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain("Unsupported currency: XXX");
    });

    it("should reject missing currency", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: 100, currency: "" });

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain("Currency is required");
    });
  });

  // Boundary values
  describe("boundary conditions", () => {
    it("should accept amount exactly at minimum threshold", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: 0.01, currency: "USD" });

      expect(result.isValid).toBe(true);
    });

    it("should accept amount exactly at maximum threshold", () => {
      const validator = new SadaqahDonationValidator();
      const maxAmount = 999999999.99;
      const result = validator.validate({ amount: maxAmount, currency: "USD" });

      expect(result.isValid).toBe(true);
    });

    it("should reject amount exceeding maximum threshold", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({
        amount: 1000000000,
        currency: "USD",
      });

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain("Amount exceeds maximum");
    });
  });

  // Edge cases
  describe("edge cases", () => {
    it("should handle very small fractional amounts", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({ amount: 0.001, currency: "USD" });

      expect(result.isValid).toBe(true);
    });

    it("should handle floating point precision issues", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({
        amount: 0.1 + 0.2, // Classic floating point issue
        currency: "USD",
      });

      expect(result.isValid).toBe(true);
    });

    it("should reject NaN amounts", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({
        amount: NaN,
        currency: "USD",
      });

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain("Amount must be a valid number");
    });

    it("should reject Infinity amounts", () => {
      const validator = new SadaqahDonationValidator();
      const result = validator.validate({
        amount: Infinity,
        currency: "USD",
      });

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain("Amount must be finite");
    });
  });
});
```

**Coverage Dimensions**:

- **Positive Cases**: Valid inputs producing expected outputs
- **Negative Cases**: Invalid inputs triggering error handling
- **Boundary Values**: Minimum, maximum, just-inside, just-outside limits
- **Edge Cases**: Null, undefined, empty, special values (NaN, Infinity)
- **Equivalence Classes**: Representative samples from input ranges

**Checklist**:

- [ ] Happy path scenarios covered
- [ ] Error conditions tested
- [ ] Boundary values verified
- [ ] Edge cases handled
- [ ] All significant code paths exercised

### 6. Integrate Tests with CI/CD Pipeline

**Practice**: Run tests automatically on every commit, pull request, and deployment. Block merges and deployments when tests fail.

**Why It Matters**: Automated testing provides continuous feedback, prevents regressions from reaching production, and enforces team discipline around test quality.

**Example: GitHub Actions Workflow**

```yaml
# .github/workflows/test.yml
name: Test Suite

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: "24.11.1"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run unit tests
        run: npm run test:unit
        timeout-minutes: 5

      - name: Run integration tests
        run: npm run test:integration
        timeout-minutes: 10

      - name: Upload coverage reports
        uses: codecov/codecov-action@v4
        with:
          files: ./coverage/coverage-final.json
          fail_ci_if_error: true

      - name: Check coverage thresholds
        run: npm run test:coverage-check
        # Fails if coverage drops below thresholds:
        # - Statements: 80%
        # - Branches: 75%
        # - Functions: 80%
        # - Lines: 80%

  test-affected:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v4
        with:
          fetch-depth: 0 # Need full history for affected detection

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: "24.11.1"
          cache: "npm"

      - name: Install dependencies
        run: npm ci

      - name: Run tests for affected projects
        run: npx nx affected:test --base=origin/main --head=HEAD
```

**Package.json Scripts**:

```json
{
  "scripts": {
    "test": "nx run-many --target=test --all",
    "test:unit": "nx run-many --target=test --all --configuration=unit",
    "test:integration": "nx run-many --target=test --all --configuration=integration",
    "test:coverage": "nx run-many --target=test --all --coverage",
    "test:coverage-check": "node scripts/check-coverage-thresholds.js",
    "test:watch": "nx run-many --target=test --all --watch",
    "test:affected": "nx affected:test"
  }
}
```

**Branch Protection Rules**:

```yaml
# GitHub repository settings
branches:
  main:
    protection:
      required_status_checks:
        strict: true
        contexts:
          - "test"
          - "test-affected"
      required_pull_request_reviews:
        required_approving_review_count: 1
      enforce_admins: false
      restrictions: null
```

**Checklist**:

- [ ] Tests run on every commit
- [ ] Tests run on every pull request
- [ ] Failed tests block merges
- [ ] Coverage thresholds enforced
- [ ] Fast feedback loop (tests complete in <10 minutes)

### 7. Write Tests That Complement Agile and DevOps

**Practice**: Design tests to support rapid iteration, frequent deployments, and continuous delivery. Tests should be fast, reliable, and provide clear feedback.

**Why It Matters**: Agile and DevOps depend on confidence to move quickly. Trustworthy tests enable frequent releases without fear of regressions.

**Example: Fast, Reliable Test Suite**

```typescript
// ✅ RIGHT: Fast, isolated unit tests
describe("IslamicLoanCalculator (Unit)", () => {
  // Each test runs in <10ms
  it("should calculate monthly payment for Loan contract", () => {
    const calculator = new IslamicLoanCalculator();
    const principal = 100000;
    const profitRate = 0.05;
    const termMonths = 60;

    const payment = calculator.calculateMonthlyPayment(principal, profitRate, termMonths);

    // Direct calculation, no external dependencies
    const totalAmount = principal * (1 + profitRate);
    const expected = totalAmount / termMonths;

    expect(payment).toBeCloseTo(expected, 2);
  });

  // Test uses test doubles for fast execution
  it("should fetch current gold price for threshold calculation", async () => {
    const mockPriceService = {
      getCurrentGoldPrice: jest.fn().mockResolvedValue(65.5),
    };
    const calculator = new IslamicLoanCalculator(mockPriceService);

    const threshold = await calculator.calculateThreshold();

    expect(threshold).toBeCloseTo(85 * 65.5, 2); // 85 grams * price
    expect(mockPriceService.getCurrentGoldPrice).toHaveBeenCalledTimes(1);
  });
});

// Integration tests run less frequently
describe("IslamicLoanCalculator (Integration)", () => {
  // Mark as integration test for selective execution
  it("should calculate payment using real gold price API", async () => {
    const realPriceService = new GoldPriceService();
    const calculator = new IslamicLoanCalculator(realPriceService);

    const threshold = await calculator.calculateThreshold();

    // Verify integration but allow reasonable range
    expect(threshold).toBeGreaterThan(5000); // Gold prices fluctuate
    expect(threshold).toBeLessThan(10000);
  }, 30000); // Longer timeout for network call
});
```

**Test Organization for Agile/DevOps**:

```typescript
// Fast tests (run on every save, pre-commit)
// Unit tests: <10ms each, no external dependencies
describe("Unit: TaxCalculator", () => {
  /* ... */
});

// Medium tests (run on pre-push, CI pull requests)
// Integration tests: <1s each, use test database/mocks
describe("Integration: TaxReportGenerator", () => {
  /* ... */
});

// Slow tests (run on CI main branch, pre-deployment)
// E2E tests: <30s each, full system
describe("E2E: Tax Submission Flow", () => {
  /* ... */
});
```

**Test Reliability Patterns**:

```typescript
// ✅ Deterministic: Always produces same result
it("should calculate Tax for fixed wealth amount", () => {
  const calculator = new TaxCalculator();
  const result = calculator.calculate(10000);

  expect(result).toBe(250); // Always 250
});

// ✅ Idempotent: Can run multiple times safely
it("should create donor account with unique ID", async () => {
  const service = new DonorService();
  const donor1 = await service.create({ name: "Ahmed" });
  const donor2 = await service.create({ name: "Ahmed" });

  expect(donor1.id).not.toBe(donor2.id); // Different IDs

  // Cleanup
  await service.delete(donor1.id);
  await service.delete(donor2.id);
});

// ✅ Isolated: No dependencies on test execution order
it("should process donation independently", async () => {
  const service = new DonationService();
  const donation = { amount: 100, donorId: "test-123" };

  const result = await service.process(donation);

  expect(result.status).toBe("completed");

  // Each test cleans up after itself
  await service.rollback(result.transactionId);
});
```

**Checklist**:

- [ ] Unit tests run in <10ms each
- [ ] Integration tests run in <1s each
- [ ] E2E tests run in <30s each
- [ ] Tests are deterministic (no random failures)
- [ ] Tests are idempotent (can run repeatedly)
- [ ] Tests are isolated (no execution order dependencies)

### 8. Treat Test Code as Production Code

**Practice**: Apply the same quality standards to test code as production code. Refactor tests, eliminate duplication, use meaningful names, and maintain readability.

**Why It Matters**: Test code represents significant codebase investment. Poor test quality leads to maintenance burden, fragile tests, and reduced confidence.

**Example: Well-Structured Test Code**

```typescript
// ❌ WRONG: Poor test code quality
describe("tests", () => {
  it("test1", () => {
    const c = new Calculator();
    const r = c.calc(1000, 0.025, 1);
    expect(r).toBe(25);
  });

  it("test2", () => {
    const c = new Calculator();
    const r = c.calc(2000, 0.025, 1);
    expect(r).toBe(50);
  });

  it("test3", () => {
    const c = new Calculator();
    const r = c.calc(1000, 0.05, 1);
    expect(r).toBe(50);
  });
});

// ✅ RIGHT: Production-quality test code
describe("TaxCalculator", () => {
  describe("calculateTax", () => {
    let calculator: TaxCalculator;

    // Setup/teardown for reusable test fixtures
    beforeEach(() => {
      calculator = new TaxCalculator();
    });

    // Helper function to reduce duplication
    const calculateForWealth = (wealth: number): number => {
      const taxRate = 0.025;
      const termYears = 1;
      return calculator.calculateTax(wealth, taxRate, termYears);
    };

    it("should calculate 25 for 1000 wealth at standard rate", () => {
      const result = calculateForWealth(1000);
      expect(result).toBe(25);
    });

    it("should calculate 50 for 2000 wealth at standard rate", () => {
      const result = calculateForWealth(2000);
      expect(result).toBe(50);
    });

    it("should calculate 50 for 1000 wealth at double rate", () => {
      const wealth = 1000;
      const doubleRate = 0.05;
      const termYears = 1;

      const result = calculator.calculateTax(wealth, doubleRate, termYears);

      expect(result).toBe(50);
    });
  });
});

// Even better: Extract test data builders
class TaxTestDataBuilder {
  private wealth: number = 10000;
  private taxRate: number = 0.025;
  private termYears: number = 1;

  withWealth(wealth: number): this {
    this.wealth = wealth;
    return this;
  }

  withRate(rate: number): this {
    this.taxRate = rate;
    return this;
  }

  withTerm(years: number): this {
    this.termYears = years;
    return this;
  }

  build() {
    return {
      wealth: this.wealth,
      taxRate: this.taxRate,
      termYears: this.termYears,
    };
  }
}

describe("TaxCalculator (with builder)", () => {
  let calculator: TaxCalculator;

  beforeEach(() => {
    calculator = new TaxCalculator();
  });

  it("should calculate correctly for standard scenario", () => {
    const scenario = new TaxTestDataBuilder().withWealth(1000).build();

    const result = calculator.calculateTax(scenario.wealth, scenario.taxRate, scenario.termYears);

    expect(result).toBe(25);
  });

  it("should calculate correctly for high-wealth scenario", () => {
    const scenario = new TaxTestDataBuilder().withWealth(100000).build();

    const result = calculator.calculateTax(scenario.wealth, scenario.taxRate, scenario.termYears);

    expect(result).toBe(2500);
  });
});
```

**Test Code Quality Checklist**:

- [ ] Descriptive test names (behavior, not implementation)
- [ ] No magic numbers (use named constants)
- [ ] DRY principle (extract common setup/helpers)
- [ ] Clear arrange-act-assert structure
- [ ] Meaningful variable names
- [ ] Comments explain "why" not "what"
- [ ] Refactored for readability

### 9. Use Descriptive Test Names

**Practice**: Test names should read like specifications. Use full sentences that describe the expected behavior and conditions.

**Why It Matters**: Good names serve as documentation and produce readable test reports. When tests fail, clear names immediately communicate what broke. This aligns with **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)**—test names explicitly state what behavior is being verified, requiring no insider knowledge to understand.

**Example: Naming Conventions**

```typescript
// ❌ WRONG: Vague, technical names
describe("TaxService", () => {
  it("works", () => {
    /* ... */
  });
  it("test calculate", () => {
    /* ... */
  });
  it("handles edge case", () => {
    /* ... */
  });
  it("returns number", () => {
    /* ... */
  });
});

// ✅ RIGHT: Descriptive, behavior-focused names
describe("TaxService", () => {
  describe("calculateTax", () => {
    it("should return 2.5% of wealth when above threshold threshold", () => {
      // Test implementation
    });

    it("should return zero when wealth is below threshold threshold", () => {
      // Test implementation
    });

    it("should throw InvalidWealthError when wealth is negative", () => {
      // Test implementation
    });

    it("should round result to 2 decimal places for currency precision", () => {
      // Test implementation
    });
  });

  describe("isWealthAboveThreshold", () => {
    it("should return true when wealth equals threshold exactly", () => {
      // Test implementation
    });

    it("should return true when wealth exceeds threshold by any amount", () => {
      // Test implementation
    });

    it("should return false when wealth is one cent below threshold", () => {
      // Test implementation
    });
  });
});
```

**Naming Patterns**:

```typescript
// Pattern 1: should [expected behavior] when [condition]
it("should calculate profit when term is in years", () => {});
it("should throw error when currency is unsupported", () => {});

// Pattern 2: should [expected behavior] for [scenario]
it("should return zero for wealth below threshold", () => {});
it("should apply discount for early payment", () => {});

// Pattern 3: [behavior] with [specific condition]
it("calculates Tax with standard 2.5% rate", () => {});
it("validates Hijri date with leap year rules", () => {});

// Pattern 4: given [precondition], when [action], then [outcome]
it("given valid donation, when processing, then creates receipt", () => {});
```

**Checklist**:

- [ ] Name describes behavior, not implementation
- [ ] Name includes condition/scenario
- [ ] Name is readable as English sentence
- [ ] Name avoids abbreviations
- [ ] Name is specific, not generic

### 10. Maintain a Fast Feedback Loop

**Practice**: Optimize test execution speed to enable rapid iteration. Developers should receive test results within seconds for unit tests, minutes for integration tests.

**Why It Matters**: Slow tests disrupt flow, discourage frequent testing, and delay problem detection. Fast tests enable TDD's red-green-refactor rhythm.

**Example: Test Speed Optimization**

```typescript
// ❌ SLOW: Unnecessary async operations
describe("TaxCalculator (Slow)", () => {
  it("should calculate Tax amount", async () => {
    const calculator = new TaxCalculator();

    // Unnecessary database call for static data
    const incomeThreshold = await database.getIncomeThreshold();

    // Unnecessary API call for calculation
    const result = await calculator.calculateFromAPI(10000);

    expect(result).toBe(250);
  });
});

// ✅ FAST: Synchronous, isolated unit test
describe("TaxCalculator (Fast)", () => {
  it("should calculate Tax amount", () => {
    const calculator = new TaxCalculator();
    const wealth = 10000;
    const threshold = 361.25; // Static value, no DB needed

    const result = calculator.calculate(wealth, threshold);

    expect(result).toBe(250);
  });
});

// For tests requiring external data, use test doubles
describe("TaxCalculator (with dependencies)", () => {
  it("should fetch current threshold from price service", async () => {
    // Fast: Mock returns immediately
    const mockPriceService = {
      getCurrentGoldPrice: jest.fn().mockResolvedValue(65.5),
    };
    const calculator = new TaxCalculator(mockPriceService);

    const threshold = await calculator.getThreshold();

    expect(threshold).toBeCloseTo(5567.5, 2); // 85g * $65.50
  });
});
```

**Speed Optimization Strategies**:

```typescript
// 1. Use in-memory databases for integration tests
beforeAll(async () => {
  // Fast: SQLite in-memory database
  database = await createInMemoryDatabase();
});

// 2. Run tests in parallel
// jest.config.js
module.exports = {
  maxWorkers: "50%", // Use half of CPU cores
  testTimeout: 5000,
};

// 3. Skip slow tests in watch mode
describe.skip("Slow E2E Tests", () => {
  // Only run in CI, not during development
});

// 4. Use test data fixtures instead of factories
const standardDonation = {
  id: "test-123",
  amount: 100,
  currency: "USD",
  donorId: "donor-456",
  timestamp: new Date("2024-01-15T10:00:00Z"),
};

// Instead of:
const donation = await donationFactory.create(); // Slow
```

**Performance Targets**:

| Test Type   | Execution Time | When to Run             |
| ----------- | -------------- | ----------------------- |
| Unit        | <10ms per test | Every save (watch mode) |
| Integration | <1s per test   | Pre-commit, pre-push    |
| E2E         | <30s per test  | CI pipeline, pre-deploy |

**Checklist**:

- [ ] Unit tests complete in <1 second total
- [ ] Integration tests complete in <10 seconds total
- [ ] E2E tests complete in <5 minutes total
- [ ] Tests run in parallel where possible
- [ ] Slow tests clearly marked and separated
- [ ] Watch mode provides instant feedback

## Decision Framework

Use this framework to evaluate your TDD practices:

### When Writing Tests

**Ask These Questions**:

1. **Red Phase**: Did I see this test fail before writing implementation?
2. **Focus**: Does this test verify one specific behavior?
3. **Clarity**: Will the failure message clearly indicate what broke?
4. **Behavior**: Am I testing what the code does, not how it does it?
5. **Coverage**: Have I tested positive cases, negative cases, and boundaries?
6. **Speed**: Can this test run in under 10ms?
7. **Reliability**: Will this test produce the same result every time?

### Green Flags

- [x] Test name describes specific behavior
- [x] Test follows Arrange-Act-Assert pattern
- [x] Test is independent (can run in isolation)
- [x] Test uses meaningful variable names
- [x] Test verifies observable behavior
- [x] Test runs quickly (<10ms for unit tests)
- [x] Failure message is self-explanatory

## Summary

### Best Practices Checklist

**Process**:

- [ ] Follow red-green-refactor cycle strictly
- [ ] Start with simplest test case
- [ ] Refactor after each passing test
- [ ] Integrate tests with CI/CD pipeline

**Test Design**:

- [ ] Keep tests small and focused (one behavior per test)
- [ ] Focus on behavior over implementation
- [ ] Achieve comprehensive coverage (positive, negative, boundary, edge)
- [ ] Use descriptive test names
- [ ] Maintain fast feedback loop (<10ms unit tests)

**Code Quality**:

- [ ] Treat test code as production code
- [ ] Extract common setup to helpers/builders
- [ ] Apply consistent naming conventions
- [ ] Write clear, self-documenting tests

**Team Practices**:

- [ ] Run tests automatically on every commit
- [ ] Block merges when tests fail
- [ ] Fix or delete failing tests immediately
- [ ] Monitor test suite health metrics

## Related Principles

**Software Engineering Principles**:

- [Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md) - TDD automates verification and enables CI/CD integration
- [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md) - Tests make behavior explicit through descriptive names and assertions
- [Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md) - Start simple, add complexity only when needed

**Related Practices**:

- [Implementation Workflow](../../../../../governance/development/workflow/implementation.md) - Make it work → Make it right → Make it fast
- [Functional Programming](../../../../../governance/development/pattern/functional-programming.md) - Pure functions for testability
- [Code Quality](../../../../../governance/development/quality/code.md) - Automated quality gates

## Quick Reference

**Red-Green-Refactor Cycle**:

1. **Red**: Write failing test for desired behavior
2. **Green**: Write minimal code to pass test
3. **Refactor**: Improve structure while keeping tests green

**Test Structure (AAA Pattern)**:

```typescript
it("should [behavior] when [condition]", () => {
  // Arrange: Set up test data and dependencies
  const instance = new ClassName();
  const input = createTestInput();

  // Act: Execute the behavior being tested
  const result = instance.method(input);

  // Assert: Verify expected outcome
  expect(result).toBe(expected);
});
```

**When to Use Test Doubles**:

- External APIs (network calls)
- Databases (for unit tests)
- File systems
- Time-dependent operations
- Random number generators
- Third-party services

**When to Use Real Implementation**:

- Pure functions (no side effects)
- Internal collaborators (same module)
- Value objects
- Simple data transformations

## Related Documentation

**TDD Fundamentals**:

- [TDD Core Concepts](ex-soen-de-tedrdetd__01-introduction-and-philosophy.md) - Three-phase cycle, benefits, principles
- [TDD vs Traditional Testing](ex-soen-de-tedrdetd__02-red-green-refactor-cycle.md) - Comparison and contrasts
- [Red-Green-Refactor Cycle](ex-soen-de-tedrdetd__03-test-types-and-pyramid.md) - Detailed cycle explanation

**TDD in Practice**:

- [Unit Testing in TDD](ex-soen-de-tedrdetd__04-unit-testing-fundamentals.md) - Unit test strategies
- [Test Doubles and Mocking](ex-soen-de-tedrdetd__05-test-doubles.md) - Mocks, stubs, fakes
- [TDD for Different Architectures](ex-soen-de-tedrdetd__11-tdd-and-functional-programming.md) - TDD across architectures
- [TDD Metrics and Measurement](ex-soen-de-tedrdetd__16-tdd-in-nx-monorepo.md) - Coverage and quality metrics

**Antipatterns**:

- [TDD Antipatterns](ex-soen-de-tedrdetd__19-anti-patterns.md) - Common pitfalls and how to avoid them

**Related Testing Approaches**:

- [BDD Best Practices](../behavior-driven-development-bdd/ex-soen-de-bedrdebd__17-best-practices.md) - BDD best practices
- [BDD Antipatterns](../behavior-driven-development-bdd/ex-soen-de-bedrdebd__18-anti-patterns.md) - BDD antipatterns

**Process Integration**:

- [TDD in Agile Workflows](ex-soen-de-tedrdetd__12-tdd-and-ddd.md) - Agile integration
- [TDD in CI/CD Pipelines](ex-soen-de-tedrdetd__13-legacy-code-and-characterization-tests.md) - Continuous integration

**Repository Standards**:

- [Code Quality Standards](../../../../../governance/development/quality/code.md) - Quality automation
- [Testing Conventions](../../../../../governance/development/pattern/functional-programming.md) - Testing standards
- [Functional Programming](../../../../../governance/development/pattern/functional-programming.md) - FP principles

## Sources

- [Monday.com: Test-Driven Development (TDD)](https://monday.com/blog/rnd/test-driven-development-tdd/)
- [Learn Go with Tests: Anti-Patterns](https://quii.gitbook.io/learn-go-with-tests/meta/anti-patterns)
- [Marabesi: TDD Anti-Patterns](https://marabesi.com/tdd/tdd-anti-patterns.html)
- [Code Pipes: Software Testing Anti-Patterns](https://blog.codepipes.com/testing/software-testing-antipatterns.html)
