# TDD Frequently Asked Questions (FAQ)

## Metadata

- **Parent Topic**: [Test-Driven Development (TDD)](./README.md)
- **Related Files**:
  - [Core Concepts](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)
  - [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md)

- **Prerequisites**: Basic understanding of TDD
- **Complexity**: Beginner to Advanced

## Table of Contents

- [Overview](#overview)
- [General TDD Questions](#general-tdd-questions)
- [Coverage and Quality Questions](#coverage-and-quality-questions)
- [Technical Implementation Questions](#technical-implementation-questions)
- [Islamic Finance Specific Questions](#islamic-finance-specific-questions)
- [Organizational Questions](#organizational-questions)
- [Summary](#summary)

## Overview

This FAQ addresses common questions about Test-Driven Development (TDD), covering both general practices and Islamic finance-specific scenarios. Questions are organized by category and include practical examples.

## Core Principles

TDD practices align with software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - TDD makes requirements explicit through tests before writing implementation. Each test explicitly documents expected behavior, transforming implicit assumptions into verifiable specifications.
- **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)** - TDD automates verification of correctness through executable tests rather than manual inspection. This automation enables confident refactoring and rapid feedback on code changes.

## General TDD Questions

### Q1: How much test coverage is enough?

**Short Answer**: It depends on the criticality of the code. Aim for 80-100% on business logic, 60-80% on infrastructure.

**Detailed Answer**:

Coverage targets vary by component type:

| Component Type     | Coverage Target | Rationale                               |
| ------------------ | --------------- | --------------------------------------- |
| **Domain Logic**   | 90-100%         | Business rules, compliance requirements |
| **API Endpoints**  | 80-95%          | Contract validation, error handling     |
| **UI Components**  | 70-85%          | User interactions, visual feedback      |
| **Infrastructure** | 60-80%          | Database, external services             |
| **Configuration**  | 40-60%          | Environment setup, config loading       |

**Example (Tax Calculator)**:

```typescript
// CRITICAL: 100% coverage required
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    // Business logic - must be fully tested
    if (wealth.isLessThan(threshold)) {
      return Money.zero(wealth.currency);
    }
    return wealth.multiply(0.025); // 2.5% rate
  }
}

// INFRASTRUCTURE: 60-80% coverage acceptable
class TaxRepository {
  async save(assessment: TaxAssessment): Promise<void> {
    // Database operations - test happy path + major errors
    await this.db.insert("tax_assessments", assessment);
  }
}
```

**Important**: Coverage is a metric, not a goal. 100% coverage doesn't guarantee bug-free code. Focus on testing behavior, not lines of code.

---

### Q2: Should I test getters/setters?

**Short Answer**: No, unless they contain business logic.

**Detailed Answer**:

Simple getters/setters don't need tests:

```typescript
// ❌ Don't test simple getters/setters
class Money {
  constructor(
    private readonly _amount: number,
    private readonly _currency: string,
  ) {}

  // No test needed - trivial getter
  get amount(): number {
    return this._amount;
  }

  // No test needed - trivial getter
  get currency(): string {
    return this._currency;
  }
}

describe("Money", () => {
  it("has amount property", () => {
    // Unnecessary test
    const money = new Money(100, "USD");
    expect(money.amount).toBe(100);
  });
});
```

Test getters/setters with logic:

```typescript
// ✅ Test getters/setters with business logic
class TaxAssessment {
  constructor(
    private wealth: Money,
    private threshold: Money,
    private _taxDue: Money,
  ) {}

  // Test this - contains business logic
  get isTaxDue(): boolean {
    return this.wealth.isGreaterThanOrEqual(this.threshold);
  }

  // Test this - validation logic
  set taxDue(value: Money) {
    if (value.isNegative()) {
      throw new Error("Tax due cannot be negative");
    }
    this._taxDue = value;
  }
}

describe("TaxAssessment", () => {
  it("returns true when wealth is above threshold", () => {
    const assessment = new TaxAssessment(
      Money.fromAmount(10000, "USD"),
      Money.fromAmount(2000, "USD"),
      Money.fromAmount(250, "USD"),
    );

    expect(assessment.isTaxDue).toBe(true);
  });

  it("throws error when setting negative Tax", () => {
    const assessment = new TaxAssessment(/* ... */);

    expect(() => {
      assessment.taxDue = Money.fromAmount(-100, "USD");
    }).toThrow("Tax due cannot be negative");
  });
});
```

---

### Q3: How do I test private methods?

**Short Answer**: Don't test private methods directly. Test them indirectly through public APIs.

**Detailed Answer**:

Private methods are implementation details. Test the public behavior that uses them:

```typescript
// ❌ BAD - Testing private method directly
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    if (this.isAboveThreshold(wealth, threshold)) {
      return this.applyRate(wealth);
    }
    return Money.zero(wealth.currency);
  }

  private isAboveThreshold(wealth: Money, threshold: Money): boolean {
    return wealth.isGreaterThanOrEqual(threshold);
  }

  private applyRate(wealth: Money): Money {
    return wealth.multiply(0.025);
  }
}

describe("TaxCalculator", () => {
  it("tests private method isAboveThreshold", () => {
    const calculator = new TaxCalculator();
    // @ts-ignore - accessing private method
    const result = calculator.isAboveThreshold(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));
    expect(result).toBe(true); // Brittle test
  });
});

// ✅ GOOD - Testing private methods through public API
describe("TaxCalculator", () => {
  it("calculates Tax when wealth above threshold", () => {
    const calculator = new TaxCalculator();

    // This indirectly tests isAboveThreshold and applyRate
    const tax = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(250);
  });

  it("returns zero when wealth below threshold", () => {
    const calculator = new TaxCalculator();

    // This also tests isAboveThreshold (false path)
    const tax = calculator.calculateTax(Money.fromAmount(1000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(0);
  });
});
```

**When to extract private method to separate class**:

If a private method is complex enough that you want to test it directly, extract it to a separate class:

```typescript
// Extract complex private logic to separate class
class ThresholdChecker {
  isAboveThreshold(wealth: Money, threshold: Money): boolean {
    return wealth.isGreaterThanOrEqual(threshold);
  }
}

class TaxCalculator {
  constructor(private thresholdChecker: ThresholdChecker) {}

  calculateTax(wealth: Money, threshold: Money): Money {
    if (this.thresholdChecker.isAboveThreshold(wealth, threshold)) {
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

// Now you can test ThresholdChecker directly
describe("ThresholdChecker", () => {
  it("returns true when wealth above threshold", () => {
    const checker = new ThresholdChecker();

    const result = checker.isAboveThreshold(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(result).toBe(true);
  });
});
```

---

### Q4: Is TDD slower than writing code first?

**Short Answer**: TDD is slower initially but faster overall due to fewer bugs and easier maintenance.

**Detailed Answer**:

**Short-term (first implementation)**:

- TDD is 15-30% slower initially
- You write tests before code
- More upfront thinking required

**Long-term (maintenance and debugging)**:

- TDD is 2-3x faster overall
- Fewer bugs reach production
- Refactoring is safer and faster
- New team members onboard faster

**Time Comparison**:

| Activity                  | Without TDD  | With TDD     | TDD Advantage       |
| ------------------------- | ------------ | ------------ | ------------------- |
| Initial development       | 10 hours     | 13 hours     | -3 hours            |
| Bug fixing (3 months)     | 8 hours      | 2 hours      | +6 hours            |
| Adding feature (6 months) | 12 hours     | 6 hours      | +6 hours            |
| Refactoring (1 year)      | 20 hours     | 8 hours      | +12 hours           |
| **Total**                 | **50 hours** | **29 hours** | **+21 hours saved** |

**Example (Tax Calculator)**:

```typescript
// Without TDD: Quick implementation, bugs discovered later
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    return wealth.multiply(0.025); // BUG: Doesn't check threshold threshold
  }
}

// Bug discovered in production after 3 months
// Time to fix: 2 hours (find bug, fix, test manually, deploy)

// With TDD: Bug caught immediately
describe("TaxCalculator", () => {
  it("returns zero when wealth below threshold", () => {
    const calculator = new TaxCalculator();

    const tax = calculator.calculateTax(
      Money.fromAmount(1000, "USD"), // Below threshold
      Money.fromAmount(2000, "USD"),
    );

    expect(tax.amount).toBe(0); // Test fails immediately
  });
});

// Fix bug during development (5 minutes)
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    if (wealth.isLessThan(threshold)) {
      return Money.zero(wealth.currency);
    }
    return wealth.multiply(0.025);
  }
}
```

---

### Q5: Should I use TDD for everything?

**Short Answer**: Use TDD for business logic and critical paths. Spike solutions and prototypes can skip TDD.

**Detailed Answer**:

**Use TDD for**:

- Business logic (calculations, validations)
- Domain models (entities, value objects)
- API endpoints
- Critical user workflows
- Compliance requirements (Compliance rules)

**Skip TDD for**:

- Exploratory prototypes (throwaway code)
- UI styling and layout
- Simple CRUD operations (already tested by framework)
- Configuration files
- Spike solutions (proof of concept)

**Example Decision Matrix**:

```typescript
// ✅ USE TDD - Business logic
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    // Critical business logic - must use TDD
  }
}

// ✅ USE TDD - Domain validation
class Money {
  static fromAmount(amount: number, currency: string): Money {
    if (amount < 0) {
      throw new Error("Amount cannot be negative");
    }
    // Validation logic - use TDD
  }
}

// ⏭️ SKIP TDD - Simple data transfer
interface TaxAssessmentDTO {
  id: string;
  userId: string;
  taxAmount: number;
  currency: string;
}

// ⏭️ SKIP TDD - UI styling
const StyledButton = styled.button`
  background-color: blue;
  color: white;
  padding: 10px;
`;

// ⏭️ SKIP TDD - Spike/prototype
// Quick prototype to explore feasibility
function experimentalTaxCalculation() {
  // Throwaway code - no tests needed
}

// ✅ USE TDD - Production API endpoint
@Controller("/tax")
class TaxController {
  @Post("/calculate")
  async calculateTax(@Body() input: TaxInput): Promise<TaxResponse> {
    // API contract - use TDD
  }
}
```

---

### Q6: How do I practice TDD when learning?

**Short Answer**: Start with katas, then apply to small features in real projects.

**Detailed Answer**:

**Step 1: Start with Katas** (1-2 weeks)
Practice with simple exercises:

```typescript
// Kata 1: FizzBuzz
describe("FizzBuzz", () => {
  it('returns "Fizz" for multiples of 3', () => {
    expect(fizzBuzz(3)).toBe("Fizz");
  });

  it('returns "Buzz" for multiples of 5', () => {
    expect(fizzBuzz(5)).toBe("Buzz");
  });

  it('returns "FizzBuzz" for multiples of 15', () => {
    expect(fizzBuzz(15)).toBe("FizzBuzz");
  });
});

// Kata 2: Prime Factors
describe("Prime Factors", () => {
  it("returns empty array for 1", () => {
    expect(primeFactors(1)).toEqual([]);
  });

  it("returns [2] for 2", () => {
    expect(primeFactors(2)).toEqual([2]);
  });

  it("returns [2, 2] for 4", () => {
    expect(primeFactors(4)).toEqual([2, 2]);
  });
});
```

**Step 2: Apply to Simple Features** (2-4 weeks)

```typescript
// Simple feature: Money addition
describe("Money", () => {
  it("adds two Money instances", () => {
    const money1 = Money.fromAmount(100, "USD");
    const money2 = Money.fromAmount(50, "USD");

    const sum = money1.add(money2);

    expect(sum.amount).toBe(150);
  });
});
```

**Step 3: Build Complete Features** (ongoing)

```typescript
// Complete feature: Tax calculation
describe("TaxCalculator", () => {
  it("calculates Tax for wealth above threshold", () => {
    // Test complete feature
  });

  it("returns zero for wealth below threshold", () => {
    // Test edge case
  });

  it("handles multiple wealth types", () => {
    // Test complex scenario
  });
});
```

**Recommended Resources**:

- Katas: <https://kata-log.rocks/>
- TDD by Example (Kent Beck)
- Growing Object-Oriented Software, Guided by Tests

---

## Coverage and Quality Questions

### Q7: What if I can't reach 100% coverage?

**Short Answer**: 100% coverage is a guideline, not a requirement. Focus on critical paths.

**Detailed Answer**:

Some code is difficult or impractical to test:

```typescript
// Hard to test: External API calls
class PermittedCertificationService {
  async verifyCertification(productId: string): Promise<boolean> {
    // Real HTTP call - use integration test instead
    const response = await fetch(`https://api.permitted.com/verify/${productId}`);
    return response.ok;
  }
}

// Solution: Mock HTTP client in unit test, use integration test for real call
describe("PermittedCertificationService", () => {
  it("verifies certification (unit test with mock)", async () => {
    const mockFetch = jest.fn().mockResolvedValue({ ok: true });
    global.fetch = mockFetch;

    const service = new PermittedCertificationService();
    const result = await service.verifyCertification("prod-123");

    expect(result).toBe(true);
  });
});

// Hard to test: Process exit
function exitOnError(error: Error): never {
  console.error(error);
  process.exit(1); // Hard to test
}

// Solution: Extract testable logic, test process.exit separately
class ErrorHandler {
  handleError(error: Error): void {
    this.logger.error(error);
    this.exitProcess(1);
  }

  protected exitProcess(code: number): void {
    process.exit(code); // Isolate untestable code
  }
}

// Test with inheritance
class TestableErrorHandler extends ErrorHandler {
  protected exitProcess(code: number): void {
    // Don't actually exit in tests
  }
}

describe("ErrorHandler", () => {
  it("logs error when handling", () => {
    const logger = { error: jest.fn() };
    const handler = new TestableErrorHandler(logger);

    handler.handleError(new Error("Test error"));

    expect(logger.error).toHaveBeenCalledWith(expect.any(Error));
  });
});
```

**Acceptable Coverage Gaps**:

- Error handling paths that are difficult to trigger
- Framework/library integration code (tested by framework tests)
- Defensive programming checks (should never happen in practice)
- Process lifecycle code (startup, shutdown)

---

### Q8: How do I test legacy code without tests?

**Short Answer**: Write characterization tests first, then refactor using TDD.

**Detailed Answer**:

**Step 1: Write Characterization Tests**
Document current behavior (even if buggy):

```typescript
// Legacy code without tests
class LegacyTaxCalculator {
  calculate(wealth: number, threshold: number): number {
    // Complex, undocumented logic
    if (wealth > threshold) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Characterization test - document current behavior
describe("LegacyTaxCalculator (characterization)", () => {
  it("calculates 2.5% for wealth above threshold", () => {
    const calculator = new LegacyTaxCalculator();

    // Document current behavior
    const result = calculator.calculate(10000, 2000);

    expect(result).toBe(250); // Current behavior
  });

  it("returns 0 for wealth at threshold", () => {
    const calculator = new LegacyTaxCalculator();

    // This might be a bug (should be > not >=), but document it
    const result = calculator.calculate(2000, 2000);

    expect(result).toBe(0); // Current (buggy?) behavior
  });
});
```

**Step 2: Refactor with Safety Net**

```typescript
// Refactor to use Money value object
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    if (wealth.isGreaterThanOrEqual(threshold)) {
      // Fixed bug
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

// New tests for refactored code
describe("TaxCalculator (refactored)", () => {
  it("calculates Tax for wealth at threshold", () => {
    const calculator = new TaxCalculator();

    const tax = calculator.calculateTax(Money.fromAmount(2000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(50); // Fixed behavior
  });
});
```

**Step 3: Gradually Improve**

- Add new features using TDD
- Refactor legacy code with tests as safety net
- Increase coverage over time

---

### Q9: Should I delete tests when refactoring?

**Short Answer**: Update tests to match new API, but preserve test cases (behavior).

**Detailed Answer**:

**What to keep**: Test cases (behavior specifications)
**What to update**: Test implementation (API calls)

```typescript
// Original implementation
class TaxCalculator {
  calculate(wealth: number, threshold: number): number {
    if (wealth >= threshold) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Original tests
describe("TaxCalculator", () => {
  it("calculates 2.5% for wealth above threshold", () => {
    const calculator = new TaxCalculator();

    const tax = calculator.calculate(10000, 2000);

    expect(tax).toBe(250);
  });
});

// Refactored implementation - using Money value object
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    if (wealth.isGreaterThanOrEqual(threshold)) {
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

// Updated tests - same behavior, different API
describe("TaxCalculator", () => {
  it("calculates 2.5% for wealth above threshold", () => {
    // Same test case
    const calculator = new TaxCalculator();

    // Updated to use Money value object
    const tax = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(250); // Same expected behavior
  });
});
```

**When to delete tests**:

- Test is testing implementation details (not behavior)
- Behavior no longer exists in refactored code
- Test is duplicate of another test

---

## Technical Implementation Questions

### Q10: How do I test asynchronous code?

**Short Answer**: Use `async/await` in tests and test framework's async support.

**Detailed Answer**:

```typescript
// Async function to test
class TaxRepository {
  async save(assessment: TaxAssessment): Promise<void> {
    await this.db.insert("tax_assessments", assessment);
  }

  async findById(id: string): Promise<TaxAssessment | null> {
    const result = await this.db.query("SELECT * FROM tax_assessments WHERE id = ?", [id]);
    return result ? this.mapToAssessment(result) : null;
  }
}

// ✅ GOOD - Test async code with async/await
describe("TaxRepository", () => {
  it("saves and retrieves assessment", async () => {
    const repository = new TaxRepository(db);
    const assessment = new TaxAssessment(/* ... */);

    await repository.save(assessment);
    const retrieved = await repository.findById(assessment.id);

    expect(retrieved).toEqual(assessment);
  });

  it("returns null when assessment not found", async () => {
    const repository = new TaxRepository(db);

    const result = await repository.findById("non-existent-id");

    expect(result).toBeNull();
  });
});

// ❌ BAD - Forgetting to await
describe("TaxRepository", () => {
  it("saves assessment", () => {
    // Missing async
    const repository = new TaxRepository(db);
    const assessment = new TaxAssessment(/* ... */);

    repository.save(assessment); // Missing await - test passes before save completes

    // This assertion might run before save completes
    expect(/* ... */);
  });
});
```

**Testing with timeouts**:

```typescript
// Function with timeout
async function calculateTaxWithTimeout(wealth: Money, threshold: Money, timeoutMs: number = 5000): Promise<Money> {
  return Promise.race([
    calculateTax(wealth, threshold),
    new Promise((_, reject) => setTimeout(() => reject(new Error("Timeout")), timeoutMs)),
  ]);
}

// Test with timeout
describe("calculateTaxWithTimeout", () => {
  it("completes before timeout", async () => {
    const result = await calculateTaxWithTimeout(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"), 1000);

    expect(result.amount).toBe(250);
  });

  it("throws error on timeout", async () => {
    // Mock slow calculation
    jest.spyOn(global, "calculateTax").mockImplementation(() => new Promise((resolve) => setTimeout(resolve, 2000)));

    await expect(
      calculateTaxWithTimeout(
        Money.fromAmount(10000, "USD"),
        Money.fromAmount(2000, "USD"),
        100, // Short timeout
      ),
    ).rejects.toThrow("Timeout");
  });
});
```

---

### Q11: How do I test code that depends on external services?

**Short Answer**: Use test doubles (mocks, stubs, fakes) for unit tests, real services for integration tests.

**Detailed Answer**:

**Unit Tests - Use Test Doubles**:

```typescript
// Service depending on external API
class PermittedCertificationVerifier {
  constructor(private httpClient: HttpClient) {}

  async verify(productId: string): Promise<boolean> {
    const response = await this.httpClient.get(`https://api.permitted.com/verify/${productId}`);
    return response.data.certified === true;
  }
}

// Unit test - mock HTTP client
describe("PermittedCertificationVerifier (unit)", () => {
  it("returns true when product is certified", async () => {
    const mockHttpClient = {
      get: jest.fn().mockResolvedValue({
        data: { certified: true },
      }),
    };

    const verifier = new PermittedCertificationVerifier(mockHttpClient);
    const result = await verifier.verify("prod-123");

    expect(result).toBe(true);
    expect(mockHttpClient.get).toHaveBeenCalledWith("https://api.permitted.com/verify/prod-123");
  });

  it("returns false when product is not certified", async () => {
    const mockHttpClient = {
      get: jest.fn().mockResolvedValue({
        data: { certified: false },
      }),
    };

    const verifier = new PermittedCertificationVerifier(mockHttpClient);
    const result = await verifier.verify("prod-456");

    expect(result).toBe(false);
  });

  it("throws error when API call fails", async () => {
    const mockHttpClient = {
      get: jest.fn().mockRejectedValue(new Error("Network error")),
    };

    const verifier = new PermittedCertificationVerifier(mockHttpClient);

    await expect(verifier.verify("prod-789")).rejects.toThrow("Network error");
  });
});
```

**Integration Tests - Use Real Services or Test Containers**:

```typescript
// Integration test - real database with test containers
describe("TaxRepository (integration)", () => {
  let db: DataSource;
  let repository: TaxRepository;

  beforeAll(async () => {
    // Start real PostgreSQL container
    db = await TestDatabase.start();
    repository = new TaxRepository(db);
  });

  afterAll(async () => {
    await TestDatabase.stop();
  });

  it("saves and retrieves assessment from real database", async () => {
    const assessment = new TaxAssessment(/* ... */);

    await repository.save(assessment);
    const retrieved = await repository.findById(assessment.id);

    expect(retrieved).toEqual(assessment);
  });
});
```

---

### Q12: How do I test code with side effects (database, file system)?

**Short Answer**: Isolate side effects, use test doubles for unit tests, real systems for integration tests.

**Detailed Answer**:

**Pattern 1: Dependency Injection**

```typescript
// Inject repository to isolate database side effect
class TaxService {
  constructor(
    private calculator: TaxCalculator,
    private repository: TaxRepository
  ) {}

  async calculateAndSave(input: TaxInput): Promise<TaxAssessment> {
    const tax = this.calculator.calculateTax(input.wealth, input.threshold);

    const assessment = new TaxAssessment(/* ... */, tax);

    await this.repository.save(assessment); // Side effect isolated

    return assessment;
  }
}

// Unit test - mock repository
describe('TaxService (unit)', () => {
  it('calculates and saves assessment', async () => {
    const calculator = new TaxCalculator();
    const mockRepository = {
      save: jest.fn().mockResolvedValue(undefined)
    };

    const service = new TaxService(calculator, mockRepository);

    const result = await service.calculateAndSave({
      wealth: Money.fromAmount(10000, 'USD'),
      threshold: Money.fromAmount(2000, 'USD')
    });

    expect(result.taxDue.amount).toBe(250);
    expect(mockRepository.save).toHaveBeenCalledWith(result);
  });
});
```

**Pattern 2: Test Containers for Integration Tests**

```typescript
// Integration test with real database
describe("TaxService (integration)", () => {
  let db: DataSource;
  let service: TaxService;

  beforeAll(async () => {
    db = await TestDatabase.start();
    const repository = new TaxRepository(db);
    const calculator = new TaxCalculator();
    service = new TaxService(calculator, repository);
  });

  afterEach(async () => {
    await TestDatabase.reset(); // Clean between tests
  });

  afterAll(async () => {
    await TestDatabase.stop();
  });

  it("persists assessment to real database", async () => {
    const result = await service.calculateAndSave({
      wealth: Money.fromAmount(10000, "USD"),
      threshold: Money.fromAmount(2000, "USD"),
    });

    // Verify persistence by querying database directly
    const saved = await db.query("SELECT * FROM tax_assessments WHERE id = ?", [result.id]);

    expect(saved).toBeTruthy();
    expect(saved.tax_amount).toBe(250);
  });
});
```

---

## Islamic Finance Specific Questions

### Q13: How do I test Compliance compliance rules?

**Short Answer**: Treat Compliance rules as business requirements and test them explicitly.

**Detailed Answer**:

```typescript
// Compliance rule: Interest (interest) is prohibited
describe("Compliance Compliance - Interest Prevention", () => {
  it("rejects transactions with interest", () => {
    const transaction = {
      principal: 10000,
      interest: 500, // Forbidden
      duration: 12,
    };

    const validator = new ComplianceValidator();

    expect(() => validator.validateTransaction(transaction)).toThrow("Transaction contains interest (interest)");
  });

  it("accepts profit-sharing transactions", () => {
    const musharakaTransaction = {
      capitalContribution: 10000,
      profitSharingRatio: 0.6, // Permitted
      duration: 12,
    };

    const validator = new ComplianceValidator();

    expect(() => validator.validateTransaction(musharakaTransaction)).not.toThrow();
  });
});

// Compliance rule: Gharar (uncertainty) is prohibited
describe("Compliance Compliance - Gharar Prevention", () => {
  it("rejects contracts with excessive uncertainty", () => {
    const contract = {
      asset: "Unknown Goods",
      quantity: "Unspecified",
      price: "To be determined",
    };

    const validator = new ComplianceValidator();

    expect(() => validator.validateContract(contract)).toThrow("Contract contains excessive gharar (uncertainty)");
  });

  it("accepts contracts with clear terms", () => {
    const contract = {
      asset: "100 barrels of oil",
      quantity: 100,
      price: 5000,
      deliveryDate: new Date("2024-06-01"),
    };

    const validator = new ComplianceValidator();

    expect(() => validator.validateContract(contract)).not.toThrow();
  });
});

// Compliance rule: Tax is 2.5% on wealth above threshold
describe("Compliance Compliance - Tax Calculation", () => {
  it("applies 2.5% rate on wealth above threshold", () => {
    const calculator = new TaxCalculator();

    const tax = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(250); // Exactly 2.5%
  });

  it("exempts wealth below threshold", () => {
    const calculator = new TaxCalculator();

    const tax = calculator.calculateTax(Money.fromAmount(1000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(0); // No Tax due
  });
});
```

---

### Q14: How do I test Permitted certification validation?

**Short Answer**: Test certification validation logic with mock certification API, use real API for integration tests.

**Detailed Answer**:

```typescript
// Permitted certification validator
class PermittedCertificationValidator {
  constructor(private certificationAPI: CertificationAPI) {}

  async validateProduct(product: Product): Promise<ValidationResult> {
    // Check ingredients
    const forbiddenIngredients = this.checkIngredients(product.ingredients);
    if (forbiddenIngredients.length > 0) {
      return {
        isPermitted: false,
        reason: `Contains forbidden ingredients: ${forbiddenIngredients.join(", ")}`,
      };
    }

    // Verify certification
    const certificationValid = await this.certificationAPI.verify(product.certificationId);

    if (!certificationValid) {
      return {
        isPermitted: false,
        reason: "Invalid or expired certification",
      };
    }

    return {
      isPermitted: true,
      reason: "Product meets Permitted requirements",
    };
  }

  private checkIngredients(ingredients: string[]): string[] {
    const forbiddenList = ["pork", "alcohol", "blood", "carrion"];
    return ingredients.filter((ingredient) =>
      forbiddenList.some((forbidden) => ingredient.toLowerCase().includes(forbidden)),
    );
  }
}

// Unit tests with mock API
describe("PermittedCertificationValidator", () => {
  describe("ingredient validation", () => {
    it("rejects products with pork", async () => {
      const mockAPI = { verify: jest.fn() };
      const validator = new PermittedCertificationValidator(mockAPI);

      const product = {
        id: "prod-123",
        name: "Bacon",
        ingredients: ["pork", "salt", "preservatives"],
        certificationId: "cert-456",
      };

      const result = await validator.validateProduct(product);

      expect(result.isPermitted).toBe(false);
      expect(result.reason).toContain("pork");
    });

    it("accepts products with Permitted ingredients", async () => {
      const mockAPI = { verify: jest.fn().mockResolvedValue(true) };
      const validator = new PermittedCertificationValidator(mockAPI);

      const product = {
        id: "prod-789",
        name: "Chicken",
        ingredients: ["chicken", "salt", "spices"],
        certificationId: "cert-456",
      };

      const result = await validator.validateProduct(product);

      expect(result.isPermitted).toBe(true);
    });
  });

  describe("certification validation", () => {
    it("rejects products with invalid certification", async () => {
      const mockAPI = { verify: jest.fn().mockResolvedValue(false) };
      const validator = new PermittedCertificationValidator(mockAPI);

      const product = {
        id: "prod-123",
        name: "Chicken",
        ingredients: ["chicken", "salt"],
        certificationId: "invalid-cert",
      };

      const result = await validator.validateProduct(product);

      expect(result.isPermitted).toBe(false);
      expect(result.reason).toContain("Invalid or expired certification");
    });
  });
});

// Integration test with real API
describe("PermittedCertificationValidator (integration)", () => {
  it("validates product using real certification API", async () => {
    const realAPI = new CertificationAPI("https://api.permitted.com");
    const validator = new PermittedCertificationValidator(realAPI);

    const product = {
      id: "prod-real-123",
      name: "Permitted Chicken",
      ingredients: ["chicken", "salt", "spices"],
      certificationId: "real-cert-456",
    };

    const result = await validator.validateProduct(product);

    expect(result.isPermitted).toBe(true);
  }, 10000); // 10 second timeout for real API call
});
```

---

## Organizational Questions

### Q15: How do I convince my team to adopt TDD?

**Short Answer**: Start small, demonstrate value, lead by example.

**Detailed Answer**:

**Step 1: Start with Critical Code**
Begin TDD on high-value, high-risk components:

```typescript
// Start with critical business logic
class TaxCalculator {
  // This is critical - must be correct
  calculateTax(wealth: Money, threshold: Money): Money {
    // TDD ensures correctness
  }
}

// Skip TDD on simple CRUD
class UserRepository {
  // Standard repository - framework-tested
  async findById(id: string): Promise<User> {
    return this.db.findOne(id);
  }
}
```

**Step 2: Show Metrics**
Track and share results:

| Metric                | Before TDD | After TDD (3 months) |
| --------------------- | ---------- | -------------------- |
| Bug rate              | 15/month   | 5/month              |
| Time to fix bugs      | 4 hours    | 1 hour               |
| Code coverage         | 40%        | 85%                  |
| Deployment confidence | Low        | High                 |

**Step 3: Pair Programming**
Pair with team members on TDD sessions:

```typescript
// Pair programming session - demonstrate TDD
// Developer 1: "Let's write a test for Tax calculation"
describe("TaxCalculator", () => {
  it("calculates 2.5% for wealth above threshold", () => {
    const calculator = new TaxCalculator();

    const tax = calculator.calculateTax(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(250);
  });
});

// Developer 2: "Now let's implement just enough to pass"
class TaxCalculator {
  calculateTax(wealth: Money, threshold: Money): Money {
    return Money.fromAmount(250, "USD"); // Hardcoded - simplest solution
  }
}

// Developer 1: "Test passes! Let's add another test to force generalization"
```

**Step 4: Celebrate Wins**
Share success stories:

- "TDD caught a Tax calculation bug before production!"
- "Refactored payment system with confidence thanks to tests"
- "New developer onboarded faster with comprehensive test suite"

---

### Q16: How do I balance TDD with deadlines?

**Short Answer**: Use TDD for critical paths, skip for throwaway code.

**Detailed Answer**:

**Critical Path - Always TDD**:

```typescript
// Payment processing - ALWAYS use TDD
class PaymentProcessor {
  async processTaxPayment(payment: TaxPayment): Promise<PaymentResult> {
    // Financial transaction - must be correct
    // TDD is non-negotiable here
  }
}

// Tests are mandatory
describe("PaymentProcessor", () => {
  it("processes valid Tax payment", async () => {
    // Test critical path
  });

  it("handles payment failures", async () => {
    // Test error handling
  });
});
```

**Prototype - Skip TDD**:

```typescript
// Quick UI prototype - skip TDD temporarily
function ExperimentalTaxCalculatorUI() {
  // Throwaway code for stakeholder demo
  // No tests needed (yet)
}

// If prototype is approved, rewrite with TDD:
describe("TaxCalculatorUI", () => {
  it("displays Tax calculation", () => {
    // Write tests for production version
  });
});
```

**Time Management**:

| Activity          | Without TDD | With TDD | Decision                  |
| ----------------- | ----------- | -------- | ------------------------- |
| Payment processor | 20 hours    | 26 hours | Use TDD - too critical    |
| UI prototype      | 8 hours     | 12 hours | Skip TDD - throwaway code |
| Tax calculator    | 10 hours    | 13 hours | Use TDD - core logic      |
| Config loading    | 2 hours     | 3 hours  | Skip TDD - low risk       |

---

### Q17: How do I maintain test suites as the codebase grows?

**Short Answer**: Regular refactoring, shared utilities, and consistent patterns.

**Detailed Answer**:

**Strategy 1: Extract Common Test Utilities**

```typescript
// Before: Duplicated test setup
describe("TaxCalculator", () => {
  it("test 1", () => {
    const wealth = Money.fromAmount(10000, "USD");
    const threshold = Money.fromAmount(2000, "USD");
    // Test...
  });

  it("test 2", () => {
    const wealth = Money.fromAmount(5000, "USD");
    const threshold = Money.fromAmount(2000, "USD");
    // Test...
  });
});

// After: Shared test builders
class MoneyBuilder {
  static usd(amount: number): Money {
    return Money.fromAmount(amount, "USD");
  }
}

describe("TaxCalculator", () => {
  it("test 1", () => {
    const wealth = MoneyBuilder.usd(10000);
    const threshold = MoneyBuilder.usd(2000);
    // Test...
  });

  it("test 2", () => {
    const wealth = MoneyBuilder.usd(5000);
    const threshold = MoneyBuilder.usd(2000);
    // Test...
  });
});
```

**Strategy 2: Consistent Naming and Organization**

```
src/
  tax/
    tax-calculator.ts
    tax-calculator.spec.ts              # Unit tests
    tax-calculator.integration.spec.ts  # Integration tests
```

**Strategy 3: Delete Obsolete Tests**

```typescript
// Delete tests for removed features
describe("OldTaxCalculator", () => {
  // DELETE - feature removed
  it("old behavior", () => {
    // This feature no longer exists
  });
});

// Keep tests for current features
describe("TaxCalculator", () => {
  it("current behavior", () => {
    // This feature still exists
  });
});
```

---

### Q18: Can I do TDD in brownfield projects?

**Short Answer**: Yes! Start with new features and gradually add tests to existing code.

**Detailed Answer**:

**Strategy: "Test and Learn" Approach**

```typescript
// Existing code (no tests)
class LegacyTaxCalculator {
  calculate(wealth: number, threshold: number): number {
    // Complex, untested logic
    if (wealth > threshold) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Step 1: Add characterization tests
describe("LegacyTaxCalculator (characterization)", () => {
  it("documents current behavior", () => {
    const calculator = new LegacyTaxCalculator();

    const result = calculator.calculate(10000, 2000);

    expect(result).toBe(250); // Document what it does now
  });
});

// Step 2: Add new features with TDD
class ModernTaxCalculator {
  calculateWithCurrency(wealth: Money, threshold: Money): Money {
    // New feature - developed with TDD
    if (wealth.isGreaterThanOrEqual(threshold)) {
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

describe("ModernTaxCalculator", () => {
  it("calculates Tax with currency support", () => {
    const calculator = new ModernTaxCalculator();

    const tax = calculator.calculateWithCurrency(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(tax.amount).toBe(250);
  });
});

// Step 3: Gradually migrate legacy code
// Over time, move logic from LegacyTaxCalculator to ModernTaxCalculator
// with tests as safety net
```

---

## Summary

**Key Takeaways**:

1. **Coverage**: Aim for 80-100% on critical code, 60-80% on infrastructure
2. **Private Methods**: Test through public APIs, extract to separate class if needed
3. **TDD Speed**: Slower initially, faster long-term due to fewer bugs
4. **Async Code**: Use `async/await` in tests, leverage test framework support
5. **External Dependencies**: Mock for unit tests, use real services for integration tests
6. **Compliance Compliance**: Treat as explicit business requirements with dedicated tests
7. **Team Adoption**: Start small, demonstrate value, pair program
8. **Brownfield Projects**: Add characterization tests, use TDD for new features

**Next Steps**:

- Review [Templates](./templates/) for standardized test structures
- Explore [Decision Trees and Best Practices](./ex-so-de-tedrdeve__15-decision-trees-and-best-practices.md)

**Related Resources**:

- [Core Concepts](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)
- [Red-Green-Refactor](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)
- [Testing Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)

## Related Principles

- [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)
- [Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)
