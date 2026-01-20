# TDD Best Practices and Antipatterns

**Created**: 2026-01-20
**Updated**: 2026-01-20
**Status**: Active

## Overview

Test-Driven Development (TDD) is a disciplined approach to software development where tests drive design and implementation decisions. When practiced correctly, TDD produces clean, maintainable code with comprehensive test coverage. However, common antipatterns can undermine these benefits, leading to brittle tests, poor coverage, and false confidence in code quality.

This guide presents proven best practices that maximize TDD's value alongside antipatterns that should be avoided. Understanding both perspectives helps teams build robust test suites that support rapid, confident iteration.

**Value of TDD Best Practices:**

- **Design Feedback**: Tests reveal design problems before implementation solidifies
- **Living Documentation**: Tests serve as executable specifications of behavior
- **Regression Safety**: Comprehensive coverage catches breaking changes immediately
- **Refactoring Confidence**: Well-structured tests enable fearless code improvements
- **Faster Debugging**: Failures pinpoint exact problem locations
- **Team Velocity**: Initial investment pays dividends through reduced debugging time

**Cost of TDD Antipatterns:**

- **False Security**: Tests pass but bugs exist in production
- **Maintenance Burden**: Fragile tests break with every refactoring
- **Slow Feedback**: Excessive setup or slow tests delay development
- **Reduced Coverage**: Giant tests miss edge cases and boundary conditions
- **Design Degradation**: Implementation-coupled tests prevent refactoring

## Part 1: Best Practices

### 1. Follow the Red-Green-Refactor Cycle Strictly

**Practice**: Adhere to the three-phase TDD rhythm without shortcuts.

**Red Phase**: Write a failing test that defines desired behavior. The test should fail for the right reason (missing functionality, not syntax errors).

**Green Phase**: Write the minimum code necessary to make the test pass. Resist the urge to add extra functionality.

**Refactor Phase**: Improve code structure while keeping tests green. Both production code and test code should be refactored.

**Why It Matters**: This cycle ensures tests genuinely verify new functionality rather than existing code. Skipping the red phase risks writing tests that always pass (see "Tests That Can't Fail" antipattern).

**Example: Zakat Calculation**

```typescript
// ❌ WRONG: Writing implementation first
class ZakatCalculator {
  calculateZakat(wealth: number): number {
    const nisab = 85 * 4.25; // 85 grams of gold at $4.25/gram
    const zakatRate = 0.025; // 2.5%

    return wealth >= nisab ? wealth * zakatRate : 0;
  }
}

// Then writing test...

// ✅ RIGHT: Red phase first
describe("ZakatCalculator", () => {
  it("should return 0 for wealth below nisab", () => {
    const calculator = new ZakatCalculator();
    const result = calculator.calculateZakat(300); // Below nisab

    expect(result).toBe(0); // This FAILS - good!
  });
});

// Green phase: Minimal implementation
class ZakatCalculator {
  calculateZakat(wealth: number): number {
    return 0; // Simplest code to pass
  }
}

// Next test drives real logic
it("should calculate 2.5% for wealth at nisab", () => {
  const calculator = new ZakatCalculator();
  const nisab = 85 * 4.25; // ~361.25
  const result = calculator.calculateZakat(nisab);

  expect(result).toBe(nisab * 0.025); // Fails - drives implementation
});

// Refactor phase: Extract constants, improve naming
class ZakatCalculator {
  private static readonly GOLD_GRAMS_FOR_NISAB = 85;
  private static readonly GOLD_PRICE_PER_GRAM = 4.25;
  private static readonly ZAKAT_RATE = 0.025;

  private static get nisab(): number {
    return this.GOLD_GRAMS_FOR_NISAB * this.GOLD_PRICE_PER_GRAM;
  }

  calculateZakat(wealth: number): number {
    if (wealth < ZakatCalculator.nisab) {
      return 0;
    }
    return wealth * ZakatCalculator.ZAKAT_RATE;
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
// ❌ WRONG: Giant test (antipattern covered later)
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

**Example: Murabaha Profit Calculation**

```typescript
// ✅ RIGHT: Start with simplest case
describe("MurabahaProfitCalculator", () => {
  // Test 1: Simplest possible case
  it("should calculate profit for 1 year term with 10% markup", () => {
    const calculator = new MurabahaProfitCalculator();
    const cost = 1000;
    const markupRate = 0.1; // 10%
    const termYears = 1;

    const profit = calculator.calculate(cost, markupRate, termYears);

    expect(profit).toBe(100); // 1000 * 0.10 * 1
  });

  // Test 2: Build on simplest case
  it("should calculate profit for multi-year term", () => {
    const calculator = new MurabahaProfitCalculator();
    const cost = 1000;
    const markupRate = 0.1;
    const termYears = 3;

    const profit = calculator.calculate(cost, markupRate, termYears);

    expect(profit).toBe(300); // 1000 * 0.10 * 3
  });

  // Test 3: Add complexity incrementally
  it("should calculate profit for fractional year term", () => {
    const calculator = new MurabahaProfitCalculator();
    const cost = 1000;
    const markupRate = 0.1;
    const termYears = 0.5; // 6 months

    const profit = calculator.calculate(cost, markupRate, termYears);

    expect(profit).toBe(50); // 1000 * 0.10 * 0.5
  });

  // Test 4: Edge cases come after happy path
  it("should return 0 profit for 0% markup rate", () => {
    const calculator = new MurabahaProfitCalculator();
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
  it("should calculate monthly payment for Murabaha contract", () => {
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
  it("should fetch current gold price for nisab calculation", async () => {
    const mockPriceService = {
      getCurrentGoldPrice: jest.fn().mockResolvedValue(65.5),
    };
    const calculator = new IslamicLoanCalculator(mockPriceService);

    const nisab = await calculator.calculateNisab();

    expect(nisab).toBeCloseTo(85 * 65.5, 2); // 85 grams * price
    expect(mockPriceService.getCurrentGoldPrice).toHaveBeenCalledTimes(1);
  });
});

// Integration tests run less frequently
describe("IslamicLoanCalculator (Integration)", () => {
  // Mark as integration test for selective execution
  it("should calculate payment using real gold price API", async () => {
    const realPriceService = new GoldPriceService();
    const calculator = new IslamicLoanCalculator(realPriceService);

    const nisab = await calculator.calculateNisab();

    // Verify integration but allow reasonable range
    expect(nisab).toBeGreaterThan(5000); // Gold prices fluctuate
    expect(nisab).toBeLessThan(10000);
  }, 30000); // Longer timeout for network call
});
```

**Test Organization for Agile/DevOps**:

```typescript
// Fast tests (run on every save, pre-commit)
// Unit tests: <10ms each, no external dependencies
describe("Unit: ZakatCalculator", () => {
  /* ... */
});

// Medium tests (run on pre-push, CI pull requests)
// Integration tests: <1s each, use test database/mocks
describe("Integration: ZakatReportGenerator", () => {
  /* ... */
});

// Slow tests (run on CI main branch, pre-deployment)
// E2E tests: <30s each, full system
describe("E2E: Zakat Submission Flow", () => {
  /* ... */
});
```

**Test Reliability Patterns**:

```typescript
// ✅ Deterministic: Always produces same result
it("should calculate Zakat for fixed wealth amount", () => {
  const calculator = new ZakatCalculator();
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
describe("ZakatCalculator", () => {
  describe("calculateZakat", () => {
    let calculator: ZakatCalculator;

    // Setup/teardown for reusable test fixtures
    beforeEach(() => {
      calculator = new ZakatCalculator();
    });

    // Helper function to reduce duplication
    const calculateForWealth = (wealth: number): number => {
      const zakatRate = 0.025;
      const termYears = 1;
      return calculator.calculateZakat(wealth, zakatRate, termYears);
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

      const result = calculator.calculateZakat(wealth, doubleRate, termYears);

      expect(result).toBe(50);
    });
  });
});

// Even better: Extract test data builders
class ZakatTestDataBuilder {
  private wealth: number = 10000;
  private zakatRate: number = 0.025;
  private termYears: number = 1;

  withWealth(wealth: number): this {
    this.wealth = wealth;
    return this;
  }

  withRate(rate: number): this {
    this.zakatRate = rate;
    return this;
  }

  withTerm(years: number): this {
    this.termYears = years;
    return this;
  }

  build() {
    return {
      wealth: this.wealth,
      zakatRate: this.zakatRate,
      termYears: this.termYears,
    };
  }
}

describe("ZakatCalculator (with builder)", () => {
  let calculator: ZakatCalculator;

  beforeEach(() => {
    calculator = new ZakatCalculator();
  });

  it("should calculate correctly for standard scenario", () => {
    const scenario = new ZakatTestDataBuilder().withWealth(1000).build();

    const result = calculator.calculateZakat(scenario.wealth, scenario.zakatRate, scenario.termYears);

    expect(result).toBe(25);
  });

  it("should calculate correctly for high-wealth scenario", () => {
    const scenario = new ZakatTestDataBuilder().withWealth(100000).build();

    const result = calculator.calculateZakat(scenario.wealth, scenario.zakatRate, scenario.termYears);

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

**Why It Matters**: Good names serve as documentation and produce readable test reports. When tests fail, clear names immediately communicate what broke.

**Example: Naming Conventions**

```typescript
// ❌ WRONG: Vague, technical names
describe("ZakatService", () => {
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
describe("ZakatService", () => {
  describe("calculateZakat", () => {
    it("should return 2.5% of wealth when above nisab threshold", () => {
      // Test implementation
    });

    it("should return zero when wealth is below nisab threshold", () => {
      // Test implementation
    });

    it("should throw InvalidWealthError when wealth is negative", () => {
      // Test implementation
    });

    it("should round result to 2 decimal places for currency precision", () => {
      // Test implementation
    });
  });

  describe("isWealthAboveNisab", () => {
    it("should return true when wealth equals nisab exactly", () => {
      // Test implementation
    });

    it("should return true when wealth exceeds nisab by any amount", () => {
      // Test implementation
    });

    it("should return false when wealth is one cent below nisab", () => {
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
it("should return zero for wealth below nisab", () => {});
it("should apply discount for early payment", () => {});

// Pattern 3: [behavior] with [specific condition]
it("calculates Zakat with standard 2.5% rate", () => {});
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
describe("ZakatCalculator (Slow)", () => {
  it("should calculate Zakat amount", async () => {
    const calculator = new ZakatCalculator();

    // Unnecessary database call for static data
    const nisabThreshold = await database.getNisabThreshold();

    // Unnecessary API call for calculation
    const result = await calculator.calculateFromAPI(10000);

    expect(result).toBe(250);
  });
});

// ✅ FAST: Synchronous, isolated unit test
describe("ZakatCalculator (Fast)", () => {
  it("should calculate Zakat amount", () => {
    const calculator = new ZakatCalculator();
    const wealth = 10000;
    const nisab = 361.25; // Static value, no DB needed

    const result = calculator.calculate(wealth, nisab);

    expect(result).toBe(250);
  });
});

// For tests requiring external data, use test doubles
describe("ZakatCalculator (with dependencies)", () => {
  it("should fetch current nisab from price service", async () => {
    // Fast: Mock returns immediately
    const mockPriceService = {
      getCurrentGoldPrice: jest.fn().mockResolvedValue(65.5),
    };
    const calculator = new ZakatCalculator(mockPriceService);

    const nisab = await calculator.getNisab();

    expect(nisab).toBeCloseTo(5567.5, 2); // 85g * $65.50
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

## Part 2: Antipatterns

### 1. The Liar

**Antipattern**: A test that passes but doesn't actually verify the claimed behavior. The test may have incorrect assertions, missing validations, or test the wrong thing entirely.

**Why It's Harmful**: Liars create false confidence. Teams believe functionality is tested when it isn't, allowing bugs to reach production.

**Example: Inadequate Assertions**

```typescript
// ❌ THE LIAR: Test passes but doesn't verify behavior
describe("HalalCertificationValidator", () => {
  it("should reject non-halal ingredients", () => {
    const validator = new HalalCertificationValidator();
    const ingredients = ["gelatin", "alcohol", "pork"];

    validator.validate(ingredients);

    // BUG: No assertion! Test always passes
  });

  it("should throw error for prohibited ingredients", () => {
    const validator = new HalalCertificationValidator();
    const ingredients = ["gelatin"];

    try {
      validator.validate(ingredients);
    } catch (error) {
      // BUG: Catches error but doesn't verify it
      // Test passes even if wrong error thrown
    }
  });

  it("should return validation result", () => {
    const validator = new HalalCertificationValidator();
    const ingredients = ["chicken", "rice", "vegetables"];

    const result = validator.validate(ingredients);

    // BUG: Assertion too weak - doesn't verify result content
    expect(result).toBeDefined();
  });
});

// ✅ HONEST TESTS: Verify actual behavior
describe("HalalCertificationValidator", () => {
  it("should reject non-halal ingredients", () => {
    const validator = new HalalCertificationValidator();
    const ingredients = ["gelatin", "alcohol", "pork"];

    const result = validator.validate(ingredients);

    expect(result.isValid).toBe(false);
    expect(result.violations).toHaveLength(3);
    expect(result.violations).toContain("gelatin is not halal-certified");
    expect(result.violations).toContain("alcohol is prohibited");
    expect(result.violations).toContain("pork is prohibited");
  });

  it("should throw ProhibitedIngredientError for prohibited ingredients", () => {
    const validator = new HalalCertificationValidator();
    const ingredients = ["pork"];

    expect(() => validator.validateStrict(ingredients)).toThrow(ProhibitedIngredientError);
    expect(() => validator.validateStrict(ingredients)).toThrow("Prohibited ingredient found: pork");
  });

  it("should return valid result for halal ingredients", () => {
    const validator = new HalalCertificationValidator();
    const ingredients = ["chicken", "rice", "vegetables"];

    const result = validator.validate(ingredients);

    expect(result.isValid).toBe(true);
    expect(result.violations).toHaveLength(0);
    expect(result.certificationLevel).toBe("halal");
  });
});
```

**Common Liar Patterns**:

```typescript
// Missing assertion entirely
it("should process donation", () => {
  service.processDonation({ amount: 100 });
  // Passes whether it works or not!
});

// Assertion too weak
it("should return donation receipt", () => {
  const receipt = service.createReceipt(donation);
  expect(receipt).toBeTruthy(); // Could be {}, [], 1, "x", etc.
});

// Testing wrong thing
it("should calculate Zakat correctly", () => {
  const calculator = new ZakatCalculator();
  expect(calculator).toBeDefined(); // Tests constructor, not calculation!
});

// Catching exceptions incorrectly
it("should throw on invalid input", async () => {
  try {
    await service.process(invalidInput);
    // If we reach here, test should fail but doesn't
  } catch (e) {
    // Test passes, but any error (even unexpected) satisfies this
  }
});
```

**How to Avoid**:

- [ ] Every test has explicit assertions
- [ ] Assertions verify specific values, not just existence
- [ ] Exception tests use `expect().toThrow()`
- [ ] Async tests properly handle promises
- [ ] Assertions match test description

### 2. Excessive Setup

**Antipattern**: Tests require elaborate, complex setup that obscures the test's purpose and makes tests fragile and difficult to maintain.

**Why It's Harmful**: Excessive setup creates coupling between tests and implementation details. When setup changes, many tests break. Developers struggle to understand what the test actually verifies.

**Example: Over-Complicated Test Setup**

```typescript
// ❌ EXCESSIVE SETUP: Obscures test intent
describe("ZakatDistributionService", () => {
  it("should distribute Zakat to eligible recipients", async () => {
    // 50 lines of setup!
    const database = new TestDatabase();
    await database.connect();
    await database.migrations.run();

    const recipient1 = await database.recipients.create({
      name: "Ahmed",
      category: "poor",
      income: 1000,
      familySize: 4,
      location: "City A",
      verified: true,
      verificationDate: new Date("2024-01-01"),
      verificationMethod: "in-person",
      verifier: "Agent123",
      bankAccount: "1234567890",
      taxId: "TAX-001",
    });

    const recipient2 = await database.recipients.create({
      name: "Fatima",
      category: "needy",
      income: 1500,
      familySize: 3,
      location: "City A",
      verified: true,
      verificationDate: new Date("2024-01-02"),
      verificationMethod: "document",
      verifier: "Agent456",
      bankAccount: "0987654321",
      taxId: "TAX-002",
    });

    const zakatPool = await database.zakatPools.create({
      totalAmount: 10000,
      currency: "USD",
      collectionYear: 1445,
      status: "ready",
      approvedBy: "Admin789",
      approvalDate: new Date("2024-01-10"),
    });

    const distributionRules = await database.rules.create({
      poorPercentage: 0.4,
      needyPercentage: 0.3,
      otherCategories: 0.3,
      minAllocation: 100,
      maxAllocation: 5000,
    });

    const service = new ZakatDistributionService(
      database,
      distributionRules,
      new NotificationService(),
      new AuditLogger(),
      new PaymentGateway(),
    );

    // Finally, the actual test!
    const result = await service.distribute(zakatPool.id);

    expect(result.totalDistributed).toBe(10000);

    // Teardown
    await database.disconnect();
  });
});

// ✅ MINIMAL SETUP: Clear and focused
describe("ZakatDistributionService", () => {
  // Shared test data builders
  const createRecipient = (overrides = {}) => ({
    id: "recipient-123",
    category: "poor",
    verified: true,
    ...overrides,
  });

  const createZakatPool = (overrides = {}) => ({
    id: "pool-456",
    totalAmount: 10000,
    currency: "USD",
    status: "ready",
    ...overrides,
  });

  it("should distribute Zakat to eligible recipients", async () => {
    const recipients = [
      createRecipient({ id: "r1", category: "poor" }),
      createRecipient({ id: "r2", category: "needy" }),
    ];
    const zakatPool = createZakatPool();

    // Use test doubles instead of real database
    const mockRepository = {
      getEligibleRecipients: jest.fn().mockResolvedValue(recipients),
      getZakatPool: jest.fn().mockResolvedValue(zakatPool),
      recordDistribution: jest.fn(),
    };

    const service = new ZakatDistributionService(mockRepository);
    const result = await service.distribute(zakatPool.id);

    expect(result.totalDistributed).toBe(10000);
    expect(mockRepository.recordDistribution).toHaveBeenCalledTimes(2);
  });
});
```

**How to Avoid**:

- [ ] Use test data builders for complex objects
- [ ] Extract common setup to `beforeEach` only when truly shared
- [ ] Prefer test doubles over real infrastructure
- [ ] Only setup what the specific test needs
- [ ] Make setup code read like documentation

### 3. The Giant

**Antipattern**: A single test that verifies multiple behaviors, has dozens of assertions, or tests many scenarios in one test case.

**Why It's Harmful**: Giants are difficult to understand, produce unclear failure messages, and make it hard to identify the root cause when they fail. They often test multiple unrelated behaviors, violating the single responsibility principle.

**Example: Oversized Test**

```typescript
// ❌ THE GIANT: Too much in one test
describe("IslamicFinanceCalculator", () => {
  it("should handle all calculation scenarios", () => {
    const calculator = new IslamicFinanceCalculator();

    // Murabaha calculations
    expect(calculator.murabaha(1000, 0.05, 12)).toBe(1050);
    expect(calculator.murabaha(2000, 0.05, 12)).toBe(2100);
    expect(calculator.murabaha(1000, 0.1, 12)).toBe(1100);

    // Ijara calculations
    expect(calculator.ijara(5000, 0.04, 24)).toBe(5200);
    expect(calculator.ijara(10000, 0.04, 24)).toBe(10400);

    // Zakat calculations
    expect(calculator.zakat(10000)).toBe(250);
    expect(calculator.zakat(20000)).toBe(500);
    expect(calculator.zakat(300)).toBe(0); // Below nisab

    // Profit sharing calculations
    expect(calculator.profitShare(1000, 0.6)).toBe(600);
    expect(calculator.profitShare(2000, 0.4)).toBe(800);

    // Edge cases
    expect(calculator.murabaha(0, 0.05, 12)).toBe(0);
    expect(() => calculator.murabaha(-1000, 0.05, 12)).toThrow();
    expect(() => calculator.zakat(-100)).toThrow();

    // Integration scenarios
    const contract = calculator.createMurabahaContract(1000, 0.05, 12);
    expect(contract.totalAmount).toBe(1050);
    expect(contract.monthlyPayment).toBeCloseTo(87.5, 2);
    expect(contract.startDate).toBeDefined();
    expect(contract.endDate).toBeDefined();
  });
});

// ✅ FOCUSED TESTS: One behavior per test
describe("IslamicFinanceCalculator", () => {
  describe("murabaha", () => {
    it("should calculate total amount with 5% profit for 1000 principal", () => {
      const calculator = new IslamicFinanceCalculator();
      const result = calculator.murabaha(1000, 0.05, 12);
      expect(result).toBe(1050);
    });

    it("should scale linearly with principal amount", () => {
      const calculator = new IslamicFinanceCalculator();
      const result = calculator.murabaha(2000, 0.05, 12);
      expect(result).toBe(2100);
    });

    it("should return 0 for 0 principal", () => {
      const calculator = new IslamicFinanceCalculator();
      const result = calculator.murabaha(0, 0.05, 12);
      expect(result).toBe(0);
    });

    it("should throw InvalidPrincipalError for negative amount", () => {
      const calculator = new IslamicFinanceCalculator();
      expect(() => calculator.murabaha(-1000, 0.05, 12)).toThrow(InvalidPrincipalError);
    });
  });

  describe("ijara", () => {
    it("should calculate total rent with 4% markup for 5000 asset value", () => {
      const calculator = new IslamicFinanceCalculator();
      const result = calculator.ijara(5000, 0.04, 24);
      expect(result).toBe(5200);
    });
  });

  describe("zakat", () => {
    it("should calculate 2.5% for wealth above nisab", () => {
      const calculator = new IslamicFinanceCalculator();
      const result = calculator.zakat(10000);
      expect(result).toBe(250);
    });

    it("should return 0 for wealth below nisab", () => {
      const calculator = new IslamicFinanceCalculator();
      const result = calculator.zakat(300);
      expect(result).toBe(0);
    });
  });
});
```

**How to Avoid**:

- [ ] One test verifies one behavior
- [ ] Test name accurately describes single purpose
- [ ] Related assertions are for same behavior
- [ ] Separate tests for different scenarios
- [ ] Split giant tests into multiple focused tests

### 4. Neglecting Refactoring

**Antipattern**: Skipping the refactor phase of red-green-refactor cycle. Tests pass, so developers move on without improving code structure.

**Why It's Harmful**: Technical debt accumulates, code becomes harder to maintain, and future changes become risky. The design benefits of TDD are lost.

**Example: Skipping Refactoring**

```typescript
// ❌ NEGLECTING REFACTORING: Tests pass but code needs improvement
class ZakatCalculator {
  calculate(wealth: number, goldPrice: number, silverPrice: number): number {
    // Test passes, but code is messy
    if (wealth >= 85 * goldPrice || wealth >= 595 * silverPrice) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Tests pass - developer moves on
describe("ZakatCalculator", () => {
  it("should calculate Zakat for wealth above gold nisab", () => {
    const calculator = new ZakatCalculator();
    const result = calculator.calculate(10000, 50, 3);
    expect(result).toBe(250); // ✅ Passes
  });

  it("should calculate Zakat for wealth above silver nisab", () => {
    const calculator = new ZakatCalculator();
    const result = calculator.calculate(2000, 100, 3);
    expect(result).toBe(50); // ✅ Passes
  });
});

// ✅ WITH REFACTORING: Tests still pass, code is cleaner
class ZakatCalculator {
  private static readonly GOLD_NISAB_GRAMS = 85;
  private static readonly SILVER_NISAB_GRAMS = 595;
  private static readonly ZAKAT_RATE = 0.025;

  calculate(wealth: number, goldPrice: number, silverPrice: number): number {
    if (this.isAboveNisab(wealth, goldPrice, silverPrice)) {
      return this.calculateZakatAmount(wealth);
    }
    return 0;
  }

  private isAboveNisab(wealth: number, goldPrice: number, silverPrice: number): boolean {
    const goldNisab = this.calculateGoldNisab(goldPrice);
    const silverNisab = this.calculateSilverNisab(silverPrice);

    return wealth >= goldNisab || wealth >= silverNisab;
  }

  private calculateGoldNisab(goldPrice: number): number {
    return ZakatCalculator.GOLD_NISAB_GRAMS * goldPrice;
  }

  private calculateSilverNisab(silverPrice: number): number {
    return ZakatCalculator.SILVER_NISAB_GRAMS * silverPrice;
  }

  private calculateZakatAmount(wealth: number): number {
    return wealth * ZakatCalculator.ZAKAT_RATE;
  }
}

// Same tests, better code
describe("ZakatCalculator", () => {
  it("should calculate Zakat for wealth above gold nisab", () => {
    const calculator = new ZakatCalculator();
    const result = calculator.calculate(10000, 50, 3);
    expect(result).toBe(250); // ✅ Still passes
  });

  it("should calculate Zakat for wealth above silver nisab", () => {
    const calculator = new ZakatCalculator();
    const result = calculator.calculate(2000, 100, 3);
    expect(result).toBe(50); // ✅ Still passes
  });
});
```

**Refactoring Opportunities**:

- Extract magic numbers to named constants
- Extract complex conditions to named methods
- Remove duplication
- Improve variable/method names
- Simplify complex logic
- Apply design patterns where appropriate

**How to Avoid**:

- [ ] Refactor after each green test
- [ ] Keep refactorings small and safe
- [ ] Run tests after each refactoring
- [ ] Refactor both production and test code
- [ ] Don't add functionality during refactoring

### 5. Tests That Can't Fail

**Antipattern**: Tests that always pass regardless of implementation correctness. Often caused by skipping the "red" phase or having no assertions.

**Why It's Harmful**: These tests provide zero value while creating false confidence. They consume maintenance effort without catching bugs.

**Example: Unfailable Tests**

```typescript
// ❌ CAN'T FAIL: No assertions
describe("DonationService", () => {
  it("should process donation", async () => {
    const service = new DonationService();
    await service.process({ amount: 100, donorId: "123" });
    // Test always passes - no verification!
  });
});

// ❌ CAN'T FAIL: Assertion too weak
describe("ReceiptGenerator", () => {
  it("should generate receipt", () => {
    const generator = new ReceiptGenerator();
    const receipt = generator.create({ amount: 100 });

    expect(receipt).toBeDefined(); // Almost anything passes
  });
});

// ❌ CAN'T FAIL: Testing mocks instead of behavior
describe("ZakatCalculator", () => {
  it("should calculate Zakat", () => {
    const mockCalculator = {
      calculate: jest.fn().mockReturnValue(250),
    };

    const result = mockCalculator.calculate(10000);

    expect(result).toBe(250); // Tests the mock, not real code!
  });
});

// ✅ CAN FAIL: Proper verification
describe("DonationService", () => {
  it("should create transaction record when processing donation", async () => {
    const mockRepository = {
      saveTransaction: jest.fn().mockResolvedValue({ id: "tx-123" }),
    };
    const service = new DonationService(mockRepository);

    await service.process({ amount: 100, donorId: "donor-456" });

    expect(mockRepository.saveTransaction).toHaveBeenCalledWith({
      amount: 100,
      donorId: "donor-456",
      status: "completed",
      timestamp: expect.any(Date),
    });
  });

  it("should return transaction ID after processing", async () => {
    const service = new DonationService();
    const result = await service.process({
      amount: 100,
      donorId: "donor-456",
    });

    expect(result.transactionId).toMatch(/^tx-[a-z0-9]+$/);
    expect(result.status).toBe("completed");
  });
});

describe("ReceiptGenerator", () => {
  it("should generate receipt with amount and donor information", () => {
    const generator = new ReceiptGenerator();
    const donation = {
      amount: 100,
      donorId: "donor-456",
      donorName: "Ahmed Ali",
      date: new Date("2024-01-15"),
    };

    const receipt = generator.create(donation);

    expect(receipt.amount).toBe(100);
    expect(receipt.donorId).toBe("donor-456");
    expect(receipt.donorName).toBe("Ahmed Ali");
    expect(receipt.receiptNumber).toBeDefined();
    expect(receipt.date).toEqual(new Date("2024-01-15"));
  });
});

describe("ZakatCalculator", () => {
  it("should calculate 2.5% of wealth above nisab", () => {
    const calculator = new ZakatCalculator(); // Real instance
    const wealth = 10000;
    const nisab = 361.25;

    const result = calculator.calculate(wealth, nisab);

    expect(result).toBe(250); // Tests real calculation
  });
});
```

**How to Avoid**:

- [ ] Always see test fail before implementation
- [ ] Delete test and verify it was actually running
- [ ] Use specific assertions, not weak ones
- [ ] Test real code, not test doubles
- [ ] Regularly review test failure messages

### 6. Code Coverage Obsession

**Antipattern**: Pursuing high coverage metrics (e.g., 100%) as a goal rather than as a byproduct of thorough testing. Writing tests solely to hit coverage targets without verifying meaningful behavior.

**Why It's Harmful**: Coverage metrics measure lines executed, not bugs caught. High coverage with poor assertions provides false security. Teams waste time on trivial tests instead of critical scenarios.

**Example: Coverage-Driven Testing**

```typescript
// ❌ COVERAGE OBSESSION: Tests hit lines but don't verify behavior
class IslamicDateConverter {
  convertToHijri(gregorianDate: Date): HijriDate {
    const daysSinceEpoch = this.calculateDaysSinceEpoch(gregorianDate);
    const hijriYear = this.calculateHijriYear(daysSinceEpoch);
    const hijriMonth = this.calculateHijriMonth(daysSinceEpoch, hijriYear);
    const hijriDay = this.calculateHijriDay(daysSinceEpoch, hijriYear, hijriMonth);

    return { year: hijriYear, month: hijriMonth, day: hijriDay };
  }

  private calculateDaysSinceEpoch(date: Date): number {
    /* ... */
  }
  private calculateHijriYear(days: number): number {
    /* ... */
  }
  private calculateHijriMonth(days: number, year: number): number {
    /* ... */
  }
  private calculateHijriDay(days: number, year: number, month: number): number {
    /* ... */
  }
}

// Tests written only to achieve 100% coverage
describe("IslamicDateConverter", () => {
  it("should execute all code paths", () => {
    const converter = new IslamicDateConverter();

    // Just calling method to hit coverage
    const result = converter.convertToHijri(new Date("2024-01-15"));

    // Weak assertion - doesn't verify correctness
    expect(result).toBeDefined();
    expect(result.year).toBeGreaterThan(0);
    expect(result.month).toBeGreaterThan(0);
    expect(result.day).toBeGreaterThan(0);
  });
});
// Coverage: 100% ✅ but actual verification: 0%

// ✅ BEHAVIOR-DRIVEN TESTING: Meaningful verification
describe("IslamicDateConverter", () => {
  it("should convert January 15, 2024 to 4 Rajab 1445", () => {
    const converter = new IslamicDateConverter();
    const gregorianDate = new Date("2024-01-15");

    const result = converter.convertToHijri(gregorianDate);

    expect(result.year).toBe(1445);
    expect(result.month).toBe(7); // Rajab
    expect(result.day).toBe(4);
  });

  it("should convert Ramadan start date correctly", () => {
    const converter = new IslamicDateConverter();
    const ramadanStart = new Date("2024-03-11"); // Example date

    const result = converter.convertToHijri(ramadanStart);

    expect(result.month).toBe(9); // Ramadan
    expect(result.day).toBe(1); // First day
  });

  it("should handle leap year conversions", () => {
    const converter = new IslamicDateConverter();
    const leapYearDate = new Date("2024-02-29");

    const result = converter.convertToHijri(leapYearDate);

    // Specific expected values based on Islamic calendar
    expect(result).toEqual({ year: 1445, month: 8, day: 19 });
  });
});
// Coverage: May be less than 100%, but tests verify actual correctness
```

**Coverage as Indicator, Not Goal**:

```typescript
// Good: Coverage reveals untested scenarios
describe("ZakatCalculator", () => {
  it("should calculate Zakat for wealth above nisab", () => {
    const calculator = new ZakatCalculator();
    const result = calculator.calculate(10000);
    expect(result).toBe(250);
  });

  // Coverage report shows below-nisab path untested
  // Add meaningful test for that scenario:
  it("should return 0 for wealth below nisab", () => {
    const calculator = new ZakatCalculator();
    const result = calculator.calculate(300);
    expect(result).toBe(0);
  });

  // Coverage shows error handling untested
  // Add meaningful test for error path:
  it("should throw for negative wealth", () => {
    const calculator = new ZakatCalculator();
    expect(() => calculator.calculate(-1000)).toThrow(InvalidWealthError);
  });
});
```

**How to Avoid**:

- [ ] Focus on testing behaviors, not covering lines
- [ ] Use coverage to find missed scenarios
- [ ] Don't test trivial code (getters, setters)
- [ ] Prioritize critical paths over 100% coverage
- [ ] Review test quality, not just coverage percentage

### 7. Flaky Tests

**Antipattern**: Tests that sometimes pass and sometimes fail without code changes. Often caused by timing issues, external dependencies, or test execution order dependencies.

**Why It's Harmful**: Flaky tests erode trust in the test suite. Teams ignore failures, missing real bugs. Debugging flaky tests wastes enormous time.

**Example: Timing-Dependent Test**

```typescript
// ❌ FLAKY: Depends on timing
describe("RamadanScheduler", () => {
  it("should send notification at Iftar time", async () => {
    const scheduler = new RamadanScheduler();
    const iftarTime = new Date();
    iftarTime.setHours(18, 30, 0, 0); // 6:30 PM

    scheduler.scheduleIftarNotification(iftarTime);

    // Wait for notification - FLAKY! May take longer
    await new Promise((resolve) => setTimeout(resolve, 100));

    const notifications = getNotifications();
    expect(notifications).toContain("Iftar time!"); // Sometimes fails
  });
});

// ❌ FLAKY: Depends on external API
describe("GoldPriceService", () => {
  it("should fetch current gold price", async () => {
    const service = new GoldPriceService();

    // Real API call - fails when network is slow or API is down
    const price = await service.getCurrentPrice();

    expect(price).toBeGreaterThan(0);
  });
});

// ❌ FLAKY: Depends on test execution order
describe("DonationRepository", () => {
  it("should save donation", async () => {
    const repo = new DonationRepository();
    await repo.save({ id: "123", amount: 100 });

    const donations = await repo.findAll();
    expect(donations).toHaveLength(1); // Fails if other tests ran first
  });
});

// ✅ DETERMINISTIC: No timing dependencies
describe("RamadanScheduler", () => {
  it("should schedule notification for Iftar time", () => {
    const mockNotifier = {
      scheduleAt: jest.fn(),
    };
    const scheduler = new RamadanScheduler(mockNotifier);
    const iftarTime = new Date("2024-03-15T18:30:00Z");

    scheduler.scheduleIftarNotification(iftarTime);

    // Verify scheduling happened, don't wait for execution
    expect(mockNotifier.scheduleAt).toHaveBeenCalledWith(iftarTime, "Iftar time!");
  });
});

// ✅ RELIABLE: Uses test double
describe("GoldPriceService", () => {
  it("should fetch current gold price from API", async () => {
    const mockHttpClient = {
      get: jest.fn().mockResolvedValue({ data: { price: 65.5 } }),
    };
    const service = new GoldPriceService(mockHttpClient);

    const price = await service.getCurrentPrice();

    expect(price).toBe(65.5);
    expect(mockHttpClient.get).toHaveBeenCalledWith("/gold/current");
  });
});

// ✅ ISOLATED: No execution order dependency
describe("DonationRepository", () => {
  let repository: DonationRepository;

  beforeEach(async () => {
    // Fresh database for each test
    repository = new DonationRepository(await createTestDatabase());
  });

  afterEach(async () => {
    // Clean up
    await repository.deleteAll();
  });

  it("should save donation", async () => {
    await repository.save({ id: "123", amount: 100 });

    const donations = await repository.findAll();
    expect(donations).toHaveLength(1);
  });

  it("should find donation by ID", async () => {
    await repository.save({ id: "456", amount: 200 });

    const donation = await repository.findById("456");
    expect(donation?.amount).toBe(200);
  });
});
```

**How to Avoid**:

- [ ] Use test doubles for external dependencies
- [ ] Avoid `setTimeout` in tests
- [ ] Isolate tests from each other
- [ ] Use deterministic data (no random values)
- [ ] Run tests in random order during development

### 8. Testing Implementation Details

**Antipattern**: Tests tightly coupled to internal implementation rather than external behavior. Tests verify how code works instead of what it does.

**Why It's Harmful**: Implementation-detail tests break with every refactoring, even when behavior is unchanged. They create maintenance burden and discourage design improvements.

**Example: Over-Specified Tests**

```typescript
// ❌ TESTING IMPLEMENTATION: Brittle and coupled
class ZakatReportGenerator {
  generate(donations: Donation[]): Report {
    const categories = this.categorizeByType(donations);
    const totals = this.calculateTotals(categories);
    const formatted = this.formatReport(totals);
    return formatted;
  }

  private categorizeByType(donations: Donation[]): Map<string, Donation[]> {
    // Implementation details
  }

  private calculateTotals(categories: Map<string, Donation[]>): Totals {
    // Implementation details
  }

  private formatReport(totals: Totals): Report {
    // Implementation details
  }
}

describe("ZakatReportGenerator", () => {
  it("should call internal methods in correct order", () => {
    const generator = new ZakatReportGenerator();
    const categorizeSpy = jest.spyOn(generator as any, "categorizeByType");
    const totalsSpy = jest.spyOn(generator as any, "calculateTotals");
    const formatSpy = jest.spyOn(generator as any, "formatReport");

    generator.generate(sampleDonations);

    // Tests implementation, not behavior
    expect(categorizeSpy).toHaveBeenCalledBefore(totalsSpy);
    expect(totalsSpy).toHaveBeenCalledBefore(formatSpy);
  });

  it("should use Map for categorization", () => {
    const generator = new ZakatReportGenerator();
    const spy = jest.spyOn(generator as any, "categorizeByType");

    generator.generate(sampleDonations);

    const result = spy.mock.results[0].value;
    expect(result).toBeInstanceOf(Map); // Couples to data structure
  });
});

// ✅ TESTING BEHAVIOR: Resilient and meaningful
describe("ZakatReportGenerator", () => {
  it("should generate report with correct category totals", () => {
    const generator = new ZakatReportGenerator();
    const donations = [
      { type: "zakat", amount: 1000 },
      { type: "zakat", amount: 500 },
      { type: "sadaqah", amount: 200 },
    ];

    const report = generator.generate(donations);

    // Tests observable behavior only
    expect(report.categories.zakat.total).toBe(1500);
    expect(report.categories.sadaqah.total).toBe(200);
    expect(report.grandTotal).toBe(1700);
  });

  it("should include donation count per category", () => {
    const generator = new ZakatReportGenerator();
    const donations = [
      { type: "zakat", amount: 1000 },
      { type: "zakat", amount: 500 },
      { type: "sadaqah", amount: 200 },
    ];

    const report = generator.generate(donations);

    expect(report.categories.zakat.count).toBe(2);
    expect(report.categories.sadaqah.count).toBe(1);
  });

  // Refactoring internal methods doesn't break these tests
});
```

**How to Avoid**:

- [ ] Test public API only
- [ ] Don't spy on private methods
- [ ] Don't verify internal data structures
- [ ] Don't test method call order
- [ ] Focus on inputs and outputs

### 9. Ignoring Test Failures

**Antipattern**: Allowing tests to fail intermittently without investigation. Teams work around broken tests by re-running or commenting them out.

**Why It's Harmful**: Broken windows theory applies to tests. Once teams accept failures, test discipline erodes completely. Real bugs hide among ignored failures.

**Example: Test Neglect**

```typescript
// ❌ IGNORED FAILURES: Commented out or skipped
describe("DonationProcessor", () => {
  // FIXME: This test fails sometimes, skipping for now
  it.skip("should process batch of donations", async () => {
    const processor = new DonationProcessor();
    const result = await processor.processBatch(largeDonationSet);
    expect(result.successCount).toBe(100);
  });

  /* Commented out because it's flaky
  it('should handle concurrent processing', async () => {
    const processor = new DonationProcessor();
    const results = await Promise.all([
      processor.process(donation1),
      processor.process(donation2)
    ]);
    expect(results).toHaveLength(2);
  });
  */
});

// ✅ ADDRESSED FAILURES: Fixed or properly documented
describe("DonationProcessor", () => {
  it("should process batch of donations", async () => {
    const processor = new DonationProcessor();
    // Fixed: Use isolated test database to avoid state issues
    const testDb = await createIsolatedTestDatabase();
    const processorWithTestDb = new DonationProcessor(testDb);

    const result = await processorWithTestDb.processBatch(largeDonationSet);

    expect(result.successCount).toBe(100);

    await testDb.cleanup();
  });

  it("should handle concurrent processing safely", async () => {
    const processor = new DonationProcessor();
    // Fixed: Use unique IDs to avoid conflicts
    const donation1 = { id: "test-1", amount: 100 };
    const donation2 = { id: "test-2", amount: 200 };

    const results = await Promise.all([processor.process(donation1), processor.process(donation2)]);

    expect(results).toHaveLength(2);
    expect(results[0].id).toBe("test-1");
    expect(results[1].id).toBe("test-2");
  });
});
```

**How to Avoid**:

- [ ] Treat test failures as production bugs
- [ ] Fix or delete failing tests immediately
- [ ] Never commit commented-out tests
- [ ] Use `.skip` only temporarily with ticket reference
- [ ] Monitor test suite health metrics

### 10. Unclear Test Failures

**Antipattern**: Test failures that don't clearly communicate what went wrong, requiring extensive debugging to understand the problem.

**Why It's Harmful**: Unclear failures waste developer time and reduce confidence in the test suite. Teams avoid running tests because failures are hard to interpret.

**Example: Cryptic Failure Messages**

```typescript
// ❌ UNCLEAR FAILURES: Hard to debug
describe("IslamicLoanCalculator", () => {
  it("should work correctly", () => {
    const calculator = new IslamicLoanCalculator();
    const result = calculator.calculate(data);

    expect(result).toBe(expected); // What data? What expected?
  });
  // Failure: "Expected 1234.56 to be 1234.57"
  // Why? What inputs? What calculation failed?
});

// ✅ CLEAR FAILURES: Self-documenting
describe("IslamicLoanCalculator", () => {
  describe("calculateMonthlyPayment", () => {
    it("should calculate correct monthly payment for 100K Murabaha over 5 years at 5% profit", () => {
      const calculator = new IslamicLoanCalculator();
      const principal = 100000;
      const profitRate = 0.05;
      const termYears = 5;

      const monthlyPayment = calculator.calculateMonthlyPayment(principal, profitRate, termYears);

      const totalAmount = principal * (1 + profitRate); // 105,000
      const termMonths = termYears * 12; // 60
      const expected = totalAmount / termMonths; // 1,750

      expect(monthlyPayment).toBeCloseTo(expected, 2);
    });
    // Failure: "Expected 1750.00 to be close to 1750.00 (2 decimals)"
    // Context: 100K principal, 5% profit, 5 years
    // Calculation: (100000 * 1.05) / 60 = 1750
  });

  it("should throw InvalidTermError when term is 0 years", () => {
    const calculator = new IslamicLoanCalculator();
    const principal = 100000;
    const profitRate = 0.05;
    const invalidTerm = 0;

    expect(() => {
      calculator.calculateMonthlyPayment(principal, profitRate, invalidTerm);
    }).toThrow(InvalidTermError);

    expect(() => {
      calculator.calculateMonthlyPayment(principal, profitRate, invalidTerm);
    }).toThrow("Loan term must be greater than 0 years");
  });
  // Failure clearly shows what validation failed
});
```

**How to Avoid**:

- [ ] Use descriptive variable names in tests
- [ ] Include context in test names
- [ ] Add comments explaining expected values
- [ ] Use custom error messages in assertions
- [ ] Show both expected and actual with context

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

### When Reviewing Test Code

**Red Flags**:

- [ ] Test has no assertions
- [ ] Test name is generic ("should work", "test method")
- [ ] Test has >20 lines of setup
- [ ] Test verifies private methods
- [ ] Test has >5 assertions for unrelated behaviors
- [ ] Test uses `setTimeout` or other timing mechanisms
- [ ] Test depends on external services without mocking
- [ ] Test is commented out or skipped
- [ ] Failure message requires debugging to understand

**Green Flags**:

- [x] Test name describes specific behavior
- [x] Test follows Arrange-Act-Assert pattern
- [x] Test is independent (can run in isolation)
- [x] Test uses meaningful variable names
- [x] Test verifies observable behavior
- [x] Test runs quickly (<10ms for unit tests)
- [x] Failure message is self-explanatory

### When Maintaining Test Suites

**Monthly Health Check**:

1. Run tests in random order - do any fail?
2. Check test execution time - are any tests slow?
3. Review skipped tests - can they be fixed or deleted?
4. Analyze coverage gaps - are critical paths tested?
5. Examine flaky tests - can they be made deterministic?

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

### Antipatterns Checklist

**Avoid These**:

- [ ] The Liar (tests that pass without verifying behavior)
- [ ] Excessive Setup (complex, obscure test arrangements)
- [ ] The Giant (too many assertions in one test)
- [ ] Neglecting Refactoring (skipping the refactor phase)
- [ ] Tests That Can't Fail (always pass regardless of code)
- [ ] Code Coverage Obsession (chasing metrics over quality)
- [ ] Flaky Tests (non-deterministic results)
- [ ] Testing Implementation Details (coupled to internals)
- [ ] Ignoring Test Failures (allowing broken tests)
- [ ] Unclear Test Failures (cryptic error messages)

### Quick Reference

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

- [TDD Core Concepts](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md) - Three-phase cycle, benefits, principles
- [TDD vs Traditional Testing](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) - Comparison and contrasts
- [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md) - Detailed cycle explanation

**TDD in Practice**:

- [Unit Testing in TDD](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Unit test strategies
- [Test Doubles and Mocking](./ex-so-de-tedrdeve__05-test-doubles.md) - Mocks, stubs, fakes
- [TDD for Different Architectures](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md) - TDD across architectures
- [TDD Metrics and Measurement](./ex-so-de-tedrdeve__17-tdd-in-nx-monorepo.md) - Coverage and quality metrics

**Related Testing Approaches**:

- [BDD Best Practices and Antipatterns](../behavior-driven-development-bdd/ex-so-de-bdd__18-best-practices-and-antipatterns.md) - BDD testing patterns

**Process Integration**:

- [TDD in Agile Workflows](./ex-so-de-tedrdeve__12-tdd-and-ddd.md) - Agile integration
- [TDD in CI/CD Pipelines](./ex-so-de-tedrdeve__13-legacy-code-and-characterization-tests.md) - Continuous integration

**Repository Standards**:

- [Code Quality Standards](../../../../../governance/development/quality/code.md) - Quality automation
- [Testing Conventions](../../../../../governance/development/pattern/functional-programming.md) - Testing standards
- [Functional Programming](../../../../../governance/development/pattern/functional-programming.md) - FP principles

## Sources

- [Monday.com: Test-Driven Development (TDD)](https://monday.com/blog/rnd/test-driven-development-tdd/)
- [Learn Go with Tests: Anti-Patterns](https://quii.gitbook.io/learn-go-with-tests/meta/anti-patterns)
- [Marabesi: TDD Anti-Patterns](https://marabesi.com/tdd/tdd-anti-patterns.html)
- [Code Pipes: Software Testing Anti-Patterns](https://blog.codepipes.com/testing/software-testing-antipatterns.html)
