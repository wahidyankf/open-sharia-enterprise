# TDD Antipatterns

**Created**: 2026-01-20
**Updated**: 2026-01-20
**Status**: Active

## Overview

> **Companion Document**: This antipatterns guide complements the [TDD Best Practices](./ex-so-de-tedrdeve__18-best-practices.md) document. Together they provide comprehensive guidance on effective TDD.

Test-Driven Development (TDD) antipatterns are common mistakes that undermine the benefits of TDD, leading to brittle tests, poor coverage, and false confidence in code quality. Understanding these antipatterns helps teams avoid pitfalls and build robust test suites that support rapid, confident iteration.

**Cost of TDD Antipatterns:**

- **False Security**: Tests pass but bugs exist in production
- **Maintenance Burden**: Fragile tests break with every refactoring
- **Slow Feedback**: Excessive setup or slow tests delay development
- **Reduced Coverage**: Giant tests miss edge cases and boundary conditions
- **Design Degradation**: Implementation-coupled tests prevent refactoring

## Antipattern 1: The Liar

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

## Antipattern 2: Excessive Setup

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

## Antipattern 3: The Giant

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

## Antipattern 4: Neglecting Refactoring

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

## Antipattern 5: Tests That Can't Fail

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

## Antipattern 6: Code Coverage Obsession

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

## Antipattern 7: Flaky Tests

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

## Antipattern 8: Testing Implementation Details

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

## Antipattern 9: Ignoring Test Failures

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

## Antipattern 10: Unclear Test Failures

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

Use this framework to evaluate your TDD practices when reviewing test code.

### Red Flags for Reviewing Test Code

**Warning Signs of Antipatterns**:

- [ ] Test has no assertions
- [ ] Test name is generic ("should work", "test method")
- [ ] Test has >20 lines of setup
- [ ] Test verifies private methods
- [ ] Test has >5 assertions for unrelated behaviors
- [ ] Test uses `setTimeout` or other timing mechanisms
- [ ] Test depends on external services without mocking
- [ ] Test is commented out or skipped
- [ ] Failure message requires debugging to understand

**Green Flags for Quality Tests**:

- [x] Test name describes specific behavior
- [x] Test follows Arrange-Act-Assert pattern
- [x] Test is independent (can run in isolation)
- [x] Test uses meaningful variable names
- [x] Test verifies observable behavior
- [x] Test runs quickly (<10ms for unit tests)
- [x] Failure message is self-explanatory

### Monthly Health Check

**Test Suite Maintenance**:

1. Run tests in random order - do any fail?
2. Check test execution time - are any tests slow?
3. Review skipped tests - can they be fixed or deleted?
4. Analyze coverage gaps - are critical paths tested?
5. Examine flaky tests - can they be made deterministic?

## Summary

### Antipatterns Checklist

**Avoid These Common Mistakes**:

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

**What to Avoid**:

| Antipattern                    | Key Symptom                   | Quick Fix                          |
| ------------------------------ | ----------------------------- | ---------------------------------- |
| The Liar                       | No assertions                 | Add explicit behavior verification |
| Excessive Setup                | >20 lines of arrange          | Use test data builders             |
| The Giant                      | >5 unrelated assertions       | Split into focused tests           |
| Neglecting Refactoring         | Duplicate code, magic numbers | Refactor after green               |
| Tests That Can't Fail          | Never saw red phase           | Delete and write from red          |
| Code Coverage Obsession        | Weak assertions               | Focus on behavior verification     |
| Flaky Tests                    | Random failures               | Remove timing/external deps        |
| Testing Implementation Details | Spying on private methods     | Test public API only               |
| Ignoring Test Failures         | Commented/skipped tests       | Fix or delete immediately          |
| Unclear Test Failures          | Cryptic error messages        | Use descriptive names and comments |

## Related Documentation

**TDD Best Practices**:

- [TDD Best Practices](./ex-so-de-tedrdeve__18-best-practices.md) - Companion document with positive patterns

**TDD Fundamentals**:

- [TDD Core Concepts](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md) - Three-phase cycle, benefits, principles
- [Red-Green-Refactor Cycle](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md) - Detailed cycle explanation
- [Test Types and Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md) - Testing strategy

**TDD in Practice**:

- [Unit Testing in TDD](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md) - Unit test strategies
- [Test Doubles and Mocking](./ex-so-de-tedrdeve__05-test-doubles.md) - Mocks, stubs, fakes
- [Testing Patterns](./ex-so-de-tedrdeve__06-testing-patterns.md) - Common testing patterns
- [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md) - When to use specific approaches

**Related Testing Approaches**:

- [BDD Best Practices](../behavior-driven-development-bdd/ex-so-de-bdd__17-best-practices.md) - BDD best practices
- [BDD Antipatterns](../behavior-driven-development-bdd/ex-so-de-bdd__18-antipatterns.md) - BDD antipatterns

**Repository Standards**:

- [Code Quality Standards](../../../../../governance/development/quality/code.md) - Quality automation
- [Testing Conventions](../../../../../governance/development/pattern/functional-programming.md) - Testing standards
- [Functional Programming](../../../../../governance/development/pattern/functional-programming.md) - FP principles

## Sources

- [Monday.com: Test-Driven Development (TDD)](https://monday.com/blog/rnd/test-driven-development-tdd/)
- [Learn Go with Tests: Anti-Patterns](https://quii.gitbook.io/learn-go-with-tests/meta/anti-patterns)
- [Marabesi: TDD Anti-Patterns](https://marabesi.com/tdd/tdd-anti-patterns.html)
- [Code Pipes: Software Testing Anti-Patterns](https://blog.codepipes.com/testing/software-testing-antipatterns.html)
