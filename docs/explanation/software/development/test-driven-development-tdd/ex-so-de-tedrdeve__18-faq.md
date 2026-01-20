# TDD Frequently Asked Questions (FAQ)

## Metadata

- **Parent Topic**: [Test-Driven Development (TDD)](./README.md)
- **Related Files**:
  - [Core Concepts](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)
  - [Decision Trees and Best Practices](./ex-so-de-tedrdeve__16-decision-trees-and-best-practices.md)

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

**Example (Zakat Calculator)**:

```typescript
// CRITICAL: 100% coverage required
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
    // Business logic - must be fully tested
    if (wealth.isLessThan(nisab)) {
      return Money.zero(wealth.currency);
    }
    return wealth.multiply(0.025); // 2.5% rate
  }
}

// INFRASTRUCTURE: 60-80% coverage acceptable
class ZakatRepository {
  async save(assessment: ZakatAssessment): Promise<void> {
    // Database operations - test happy path + major errors
    await this.db.insert("zakat_assessments", assessment);
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
class ZakatAssessment {
  constructor(
    private wealth: Money,
    private nisab: Money,
    private _zakatDue: Money,
  ) {}

  // Test this - contains business logic
  get isZakatDue(): boolean {
    return this.wealth.isGreaterThanOrEqual(this.nisab);
  }

  // Test this - validation logic
  set zakatDue(value: Money) {
    if (value.isNegative()) {
      throw new Error("Zakat due cannot be negative");
    }
    this._zakatDue = value;
  }
}

describe("ZakatAssessment", () => {
  it("returns true when wealth is above nisab", () => {
    const assessment = new ZakatAssessment(
      Money.fromAmount(10000, "USD"),
      Money.fromAmount(2000, "USD"),
      Money.fromAmount(250, "USD"),
    );

    expect(assessment.isZakatDue).toBe(true);
  });

  it("throws error when setting negative Zakat", () => {
    const assessment = new ZakatAssessment(/* ... */);

    expect(() => {
      assessment.zakatDue = Money.fromAmount(-100, "USD");
    }).toThrow("Zakat due cannot be negative");
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
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
    if (this.isAboveNisab(wealth, nisab)) {
      return this.applyRate(wealth);
    }
    return Money.zero(wealth.currency);
  }

  private isAboveNisab(wealth: Money, nisab: Money): boolean {
    return wealth.isGreaterThanOrEqual(nisab);
  }

  private applyRate(wealth: Money): Money {
    return wealth.multiply(0.025);
  }
}

describe("ZakatCalculator", () => {
  it("tests private method isAboveNisab", () => {
    const calculator = new ZakatCalculator();
    // @ts-ignore - accessing private method
    const result = calculator.isAboveNisab(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));
    expect(result).toBe(true); // Brittle test
  });
});

// ✅ GOOD - Testing private methods through public API
describe("ZakatCalculator", () => {
  it("calculates Zakat when wealth above nisab", () => {
    const calculator = new ZakatCalculator();

    // This indirectly tests isAboveNisab and applyRate
    const zakat = calculator.calculateZakat(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(250);
  });

  it("returns zero when wealth below nisab", () => {
    const calculator = new ZakatCalculator();

    // This also tests isAboveNisab (false path)
    const zakat = calculator.calculateZakat(Money.fromAmount(1000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(0);
  });
});
```

**When to extract private method to separate class**:

If a private method is complex enough that you want to test it directly, extract it to a separate class:

```typescript
// Extract complex private logic to separate class
class NisabChecker {
  isAboveNisab(wealth: Money, nisab: Money): boolean {
    return wealth.isGreaterThanOrEqual(nisab);
  }
}

class ZakatCalculator {
  constructor(private nisabChecker: NisabChecker) {}

  calculateZakat(wealth: Money, nisab: Money): Money {
    if (this.nisabChecker.isAboveNisab(wealth, nisab)) {
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

// Now you can test NisabChecker directly
describe("NisabChecker", () => {
  it("returns true when wealth above nisab", () => {
    const checker = new NisabChecker();

    const result = checker.isAboveNisab(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

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

**Example (Zakat Calculator)**:

```typescript
// Without TDD: Quick implementation, bugs discovered later
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
    return wealth.multiply(0.025); // BUG: Doesn't check nisab threshold
  }
}

// Bug discovered in production after 3 months
// Time to fix: 2 hours (find bug, fix, test manually, deploy)

// With TDD: Bug caught immediately
describe("ZakatCalculator", () => {
  it("returns zero when wealth below nisab", () => {
    const calculator = new ZakatCalculator();

    const zakat = calculator.calculateZakat(
      Money.fromAmount(1000, "USD"), // Below nisab
      Money.fromAmount(2000, "USD"),
    );

    expect(zakat.amount).toBe(0); // Test fails immediately
  });
});

// Fix bug during development (5 minutes)
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
    if (wealth.isLessThan(nisab)) {
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
- Compliance requirements (Sharia rules)

**Skip TDD for**:

- Exploratory prototypes (throwaway code)
- UI styling and layout
- Simple CRUD operations (already tested by framework)
- Configuration files
- Spike solutions (proof of concept)

**Example Decision Matrix**:

```typescript
// ✅ USE TDD - Business logic
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
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
interface ZakatAssessmentDTO {
  id: string;
  userId: string;
  zakatAmount: number;
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
function experimentalZakatCalculation() {
  // Throwaway code - no tests needed
}

// ✅ USE TDD - Production API endpoint
@Controller("/zakat")
class ZakatController {
  @Post("/calculate")
  async calculateZakat(@Body() input: ZakatInput): Promise<ZakatResponse> {
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
// Complete feature: Zakat calculation
describe("ZakatCalculator", () => {
  it("calculates Zakat for wealth above nisab", () => {
    // Test complete feature
  });

  it("returns zero for wealth below nisab", () => {
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
class HalalCertificationService {
  async verifyCertification(productId: string): Promise<boolean> {
    // Real HTTP call - use integration test instead
    const response = await fetch(`https://api.halal.com/verify/${productId}`);
    return response.ok;
  }
}

// Solution: Mock HTTP client in unit test, use integration test for real call
describe("HalalCertificationService", () => {
  it("verifies certification (unit test with mock)", async () => {
    const mockFetch = jest.fn().mockResolvedValue({ ok: true });
    global.fetch = mockFetch;

    const service = new HalalCertificationService();
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
class LegacyZakatCalculator {
  calculate(wealth: number, nisab: number): number {
    // Complex, undocumented logic
    if (wealth > nisab) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Characterization test - document current behavior
describe("LegacyZakatCalculator (characterization)", () => {
  it("calculates 2.5% for wealth above nisab", () => {
    const calculator = new LegacyZakatCalculator();

    // Document current behavior
    const result = calculator.calculate(10000, 2000);

    expect(result).toBe(250); // Current behavior
  });

  it("returns 0 for wealth at nisab", () => {
    const calculator = new LegacyZakatCalculator();

    // This might be a bug (should be > not >=), but document it
    const result = calculator.calculate(2000, 2000);

    expect(result).toBe(0); // Current (buggy?) behavior
  });
});
```

**Step 2: Refactor with Safety Net**

```typescript
// Refactor to use Money value object
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
    if (wealth.isGreaterThanOrEqual(nisab)) {
      // Fixed bug
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

// New tests for refactored code
describe("ZakatCalculator (refactored)", () => {
  it("calculates Zakat for wealth at nisab", () => {
    const calculator = new ZakatCalculator();

    const zakat = calculator.calculateZakat(Money.fromAmount(2000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(50); // Fixed behavior
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
class ZakatCalculator {
  calculate(wealth: number, nisab: number): number {
    if (wealth >= nisab) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Original tests
describe("ZakatCalculator", () => {
  it("calculates 2.5% for wealth above nisab", () => {
    const calculator = new ZakatCalculator();

    const zakat = calculator.calculate(10000, 2000);

    expect(zakat).toBe(250);
  });
});

// Refactored implementation - using Money value object
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
    if (wealth.isGreaterThanOrEqual(nisab)) {
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

// Updated tests - same behavior, different API
describe("ZakatCalculator", () => {
  it("calculates 2.5% for wealth above nisab", () => {
    // Same test case
    const calculator = new ZakatCalculator();

    // Updated to use Money value object
    const zakat = calculator.calculateZakat(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(250); // Same expected behavior
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
class ZakatRepository {
  async save(assessment: ZakatAssessment): Promise<void> {
    await this.db.insert("zakat_assessments", assessment);
  }

  async findById(id: string): Promise<ZakatAssessment | null> {
    const result = await this.db.query("SELECT * FROM zakat_assessments WHERE id = ?", [id]);
    return result ? this.mapToAssessment(result) : null;
  }
}

// ✅ GOOD - Test async code with async/await
describe("ZakatRepository", () => {
  it("saves and retrieves assessment", async () => {
    const repository = new ZakatRepository(db);
    const assessment = new ZakatAssessment(/* ... */);

    await repository.save(assessment);
    const retrieved = await repository.findById(assessment.id);

    expect(retrieved).toEqual(assessment);
  });

  it("returns null when assessment not found", async () => {
    const repository = new ZakatRepository(db);

    const result = await repository.findById("non-existent-id");

    expect(result).toBeNull();
  });
});

// ❌ BAD - Forgetting to await
describe("ZakatRepository", () => {
  it("saves assessment", () => {
    // Missing async
    const repository = new ZakatRepository(db);
    const assessment = new ZakatAssessment(/* ... */);

    repository.save(assessment); // Missing await - test passes before save completes

    // This assertion might run before save completes
    expect(/* ... */);
  });
});
```

**Testing with timeouts**:

```typescript
// Function with timeout
async function calculateZakatWithTimeout(wealth: Money, nisab: Money, timeoutMs: number = 5000): Promise<Money> {
  return Promise.race([
    calculateZakat(wealth, nisab),
    new Promise((_, reject) => setTimeout(() => reject(new Error("Timeout")), timeoutMs)),
  ]);
}

// Test with timeout
describe("calculateZakatWithTimeout", () => {
  it("completes before timeout", async () => {
    const result = await calculateZakatWithTimeout(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"), 1000);

    expect(result.amount).toBe(250);
  });

  it("throws error on timeout", async () => {
    // Mock slow calculation
    jest.spyOn(global, "calculateZakat").mockImplementation(() => new Promise((resolve) => setTimeout(resolve, 2000)));

    await expect(
      calculateZakatWithTimeout(
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
class HalalCertificationVerifier {
  constructor(private httpClient: HttpClient) {}

  async verify(productId: string): Promise<boolean> {
    const response = await this.httpClient.get(`https://api.halal.com/verify/${productId}`);
    return response.data.certified === true;
  }
}

// Unit test - mock HTTP client
describe("HalalCertificationVerifier (unit)", () => {
  it("returns true when product is certified", async () => {
    const mockHttpClient = {
      get: jest.fn().mockResolvedValue({
        data: { certified: true },
      }),
    };

    const verifier = new HalalCertificationVerifier(mockHttpClient);
    const result = await verifier.verify("prod-123");

    expect(result).toBe(true);
    expect(mockHttpClient.get).toHaveBeenCalledWith("https://api.halal.com/verify/prod-123");
  });

  it("returns false when product is not certified", async () => {
    const mockHttpClient = {
      get: jest.fn().mockResolvedValue({
        data: { certified: false },
      }),
    };

    const verifier = new HalalCertificationVerifier(mockHttpClient);
    const result = await verifier.verify("prod-456");

    expect(result).toBe(false);
  });

  it("throws error when API call fails", async () => {
    const mockHttpClient = {
      get: jest.fn().mockRejectedValue(new Error("Network error")),
    };

    const verifier = new HalalCertificationVerifier(mockHttpClient);

    await expect(verifier.verify("prod-789")).rejects.toThrow("Network error");
  });
});
```

**Integration Tests - Use Real Services or Test Containers**:

```typescript
// Integration test - real database with test containers
describe("ZakatRepository (integration)", () => {
  let db: DataSource;
  let repository: ZakatRepository;

  beforeAll(async () => {
    // Start real PostgreSQL container
    db = await TestDatabase.start();
    repository = new ZakatRepository(db);
  });

  afterAll(async () => {
    await TestDatabase.stop();
  });

  it("saves and retrieves assessment from real database", async () => {
    const assessment = new ZakatAssessment(/* ... */);

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
class ZakatService {
  constructor(
    private calculator: ZakatCalculator,
    private repository: ZakatRepository
  ) {}

  async calculateAndSave(input: ZakatInput): Promise<ZakatAssessment> {
    const zakat = this.calculator.calculateZakat(input.wealth, input.nisab);

    const assessment = new ZakatAssessment(/* ... */, zakat);

    await this.repository.save(assessment); // Side effect isolated

    return assessment;
  }
}

// Unit test - mock repository
describe('ZakatService (unit)', () => {
  it('calculates and saves assessment', async () => {
    const calculator = new ZakatCalculator();
    const mockRepository = {
      save: jest.fn().mockResolvedValue(undefined)
    };

    const service = new ZakatService(calculator, mockRepository);

    const result = await service.calculateAndSave({
      wealth: Money.fromAmount(10000, 'USD'),
      nisab: Money.fromAmount(2000, 'USD')
    });

    expect(result.zakatDue.amount).toBe(250);
    expect(mockRepository.save).toHaveBeenCalledWith(result);
  });
});
```

**Pattern 2: Test Containers for Integration Tests**

```typescript
// Integration test with real database
describe("ZakatService (integration)", () => {
  let db: DataSource;
  let service: ZakatService;

  beforeAll(async () => {
    db = await TestDatabase.start();
    const repository = new ZakatRepository(db);
    const calculator = new ZakatCalculator();
    service = new ZakatService(calculator, repository);
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
      nisab: Money.fromAmount(2000, "USD"),
    });

    // Verify persistence by querying database directly
    const saved = await db.query("SELECT * FROM zakat_assessments WHERE id = ?", [result.id]);

    expect(saved).toBeTruthy();
    expect(saved.zakat_amount).toBe(250);
  });
});
```

---

## Islamic Finance Specific Questions

### Q13: How do I test Sharia compliance rules?

**Short Answer**: Treat Sharia rules as business requirements and test them explicitly.

**Detailed Answer**:

```typescript
// Sharia rule: Riba (interest) is prohibited
describe("Sharia Compliance - Riba Prevention", () => {
  it("rejects transactions with interest", () => {
    const transaction = {
      principal: 10000,
      interest: 500, // Haram
      duration: 12,
    };

    const validator = new ShariaComplianceValidator();

    expect(() => validator.validateTransaction(transaction)).toThrow("Transaction contains riba (interest)");
  });

  it("accepts profit-sharing transactions", () => {
    const musharakaTransaction = {
      capitalContribution: 10000,
      profitSharingRatio: 0.6, // Halal
      duration: 12,
    };

    const validator = new ShariaComplianceValidator();

    expect(() => validator.validateTransaction(musharakaTransaction)).not.toThrow();
  });
});

// Sharia rule: Gharar (uncertainty) is prohibited
describe("Sharia Compliance - Gharar Prevention", () => {
  it("rejects contracts with excessive uncertainty", () => {
    const contract = {
      asset: "Unknown Goods",
      quantity: "Unspecified",
      price: "To be determined",
    };

    const validator = new ShariaComplianceValidator();

    expect(() => validator.validateContract(contract)).toThrow("Contract contains excessive gharar (uncertainty)");
  });

  it("accepts contracts with clear terms", () => {
    const contract = {
      asset: "100 barrels of oil",
      quantity: 100,
      price: 5000,
      deliveryDate: new Date("2024-06-01"),
    };

    const validator = new ShariaComplianceValidator();

    expect(() => validator.validateContract(contract)).not.toThrow();
  });
});

// Sharia rule: Zakat is 2.5% on wealth above nisab
describe("Sharia Compliance - Zakat Calculation", () => {
  it("applies 2.5% rate on wealth above nisab", () => {
    const calculator = new ZakatCalculator();

    const zakat = calculator.calculateZakat(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(250); // Exactly 2.5%
  });

  it("exempts wealth below nisab", () => {
    const calculator = new ZakatCalculator();

    const zakat = calculator.calculateZakat(Money.fromAmount(1000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(0); // No Zakat due
  });
});
```

---

### Q14: How do I test Halal certification validation?

**Short Answer**: Test certification validation logic with mock certification API, use real API for integration tests.

**Detailed Answer**:

```typescript
// Halal certification validator
class HalalCertificationValidator {
  constructor(private certificationAPI: CertificationAPI) {}

  async validateProduct(product: Product): Promise<ValidationResult> {
    // Check ingredients
    const haramIngredients = this.checkIngredients(product.ingredients);
    if (haramIngredients.length > 0) {
      return {
        isHalal: false,
        reason: `Contains haram ingredients: ${haramIngredients.join(", ")}`,
      };
    }

    // Verify certification
    const certificationValid = await this.certificationAPI.verify(product.certificationId);

    if (!certificationValid) {
      return {
        isHalal: false,
        reason: "Invalid or expired certification",
      };
    }

    return {
      isHalal: true,
      reason: "Product meets Halal requirements",
    };
  }

  private checkIngredients(ingredients: string[]): string[] {
    const haramList = ["pork", "alcohol", "blood", "carrion"];
    return ingredients.filter((ingredient) => haramList.some((haram) => ingredient.toLowerCase().includes(haram)));
  }
}

// Unit tests with mock API
describe("HalalCertificationValidator", () => {
  describe("ingredient validation", () => {
    it("rejects products with pork", async () => {
      const mockAPI = { verify: jest.fn() };
      const validator = new HalalCertificationValidator(mockAPI);

      const product = {
        id: "prod-123",
        name: "Bacon",
        ingredients: ["pork", "salt", "preservatives"],
        certificationId: "cert-456",
      };

      const result = await validator.validateProduct(product);

      expect(result.isHalal).toBe(false);
      expect(result.reason).toContain("pork");
    });

    it("accepts products with Halal ingredients", async () => {
      const mockAPI = { verify: jest.fn().mockResolvedValue(true) };
      const validator = new HalalCertificationValidator(mockAPI);

      const product = {
        id: "prod-789",
        name: "Chicken",
        ingredients: ["chicken", "salt", "spices"],
        certificationId: "cert-456",
      };

      const result = await validator.validateProduct(product);

      expect(result.isHalal).toBe(true);
    });
  });

  describe("certification validation", () => {
    it("rejects products with invalid certification", async () => {
      const mockAPI = { verify: jest.fn().mockResolvedValue(false) };
      const validator = new HalalCertificationValidator(mockAPI);

      const product = {
        id: "prod-123",
        name: "Chicken",
        ingredients: ["chicken", "salt"],
        certificationId: "invalid-cert",
      };

      const result = await validator.validateProduct(product);

      expect(result.isHalal).toBe(false);
      expect(result.reason).toContain("Invalid or expired certification");
    });
  });
});

// Integration test with real API
describe("HalalCertificationValidator (integration)", () => {
  it("validates product using real certification API", async () => {
    const realAPI = new CertificationAPI("https://api.halal.com");
    const validator = new HalalCertificationValidator(realAPI);

    const product = {
      id: "prod-real-123",
      name: "Halal Chicken",
      ingredients: ["chicken", "salt", "spices"],
      certificationId: "real-cert-456",
    };

    const result = await validator.validateProduct(product);

    expect(result.isHalal).toBe(true);
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
class ZakatCalculator {
  // This is critical - must be correct
  calculateZakat(wealth: Money, nisab: Money): Money {
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
// Developer 1: "Let's write a test for Zakat calculation"
describe("ZakatCalculator", () => {
  it("calculates 2.5% for wealth above nisab", () => {
    const calculator = new ZakatCalculator();

    const zakat = calculator.calculateZakat(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(250);
  });
});

// Developer 2: "Now let's implement just enough to pass"
class ZakatCalculator {
  calculateZakat(wealth: Money, nisab: Money): Money {
    return Money.fromAmount(250, "USD"); // Hardcoded - simplest solution
  }
}

// Developer 1: "Test passes! Let's add another test to force generalization"
```

**Step 4: Celebrate Wins**
Share success stories:

- "TDD caught a Zakat calculation bug before production!"
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
  async processZakatPayment(payment: ZakatPayment): Promise<PaymentResult> {
    // Financial transaction - must be correct
    // TDD is non-negotiable here
  }
}

// Tests are mandatory
describe("PaymentProcessor", () => {
  it("processes valid Zakat payment", async () => {
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
function ExperimentalZakatCalculatorUI() {
  // Throwaway code for stakeholder demo
  // No tests needed (yet)
}

// If prototype is approved, rewrite with TDD:
describe("ZakatCalculatorUI", () => {
  it("displays Zakat calculation", () => {
    // Write tests for production version
  });
});
```

**Time Management**:

| Activity          | Without TDD | With TDD | Decision                  |
| ----------------- | ----------- | -------- | ------------------------- |
| Payment processor | 20 hours    | 26 hours | Use TDD - too critical    |
| UI prototype      | 8 hours     | 12 hours | Skip TDD - throwaway code |
| Zakat calculator  | 10 hours    | 13 hours | Use TDD - core logic      |
| Config loading    | 2 hours     | 3 hours  | Skip TDD - low risk       |

---

### Q17: How do I maintain test suites as the codebase grows?

**Short Answer**: Regular refactoring, shared utilities, and consistent patterns.

**Detailed Answer**:

**Strategy 1: Extract Common Test Utilities**

```typescript
// Before: Duplicated test setup
describe("ZakatCalculator", () => {
  it("test 1", () => {
    const wealth = Money.fromAmount(10000, "USD");
    const nisab = Money.fromAmount(2000, "USD");
    // Test...
  });

  it("test 2", () => {
    const wealth = Money.fromAmount(5000, "USD");
    const nisab = Money.fromAmount(2000, "USD");
    // Test...
  });
});

// After: Shared test builders
class MoneyBuilder {
  static usd(amount: number): Money {
    return Money.fromAmount(amount, "USD");
  }
}

describe("ZakatCalculator", () => {
  it("test 1", () => {
    const wealth = MoneyBuilder.usd(10000);
    const nisab = MoneyBuilder.usd(2000);
    // Test...
  });

  it("test 2", () => {
    const wealth = MoneyBuilder.usd(5000);
    const nisab = MoneyBuilder.usd(2000);
    // Test...
  });
});
```

**Strategy 2: Consistent Naming and Organization**

```
src/
  zakat/
    zakat-calculator.ts
    zakat-calculator.spec.ts              # Unit tests
    zakat-calculator.integration.spec.ts  # Integration tests
```

**Strategy 3: Delete Obsolete Tests**

```typescript
// Delete tests for removed features
describe("OldZakatCalculator", () => {
  // DELETE - feature removed
  it("old behavior", () => {
    // This feature no longer exists
  });
});

// Keep tests for current features
describe("ZakatCalculator", () => {
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
class LegacyZakatCalculator {
  calculate(wealth: number, nisab: number): number {
    // Complex, untested logic
    if (wealth > nisab) {
      return wealth * 0.025;
    }
    return 0;
  }
}

// Step 1: Add characterization tests
describe("LegacyZakatCalculator (characterization)", () => {
  it("documents current behavior", () => {
    const calculator = new LegacyZakatCalculator();

    const result = calculator.calculate(10000, 2000);

    expect(result).toBe(250); // Document what it does now
  });
});

// Step 2: Add new features with TDD
class ModernZakatCalculator {
  calculateWithCurrency(wealth: Money, nisab: Money): Money {
    // New feature - developed with TDD
    if (wealth.isGreaterThanOrEqual(nisab)) {
      return wealth.multiply(0.025);
    }
    return Money.zero(wealth.currency);
  }
}

describe("ModernZakatCalculator", () => {
  it("calculates Zakat with currency support", () => {
    const calculator = new ModernZakatCalculator();

    const zakat = calculator.calculateWithCurrency(Money.fromAmount(10000, "USD"), Money.fromAmount(2000, "USD"));

    expect(zakat.amount).toBe(250);
  });
});

// Step 3: Gradually migrate legacy code
// Over time, move logic from LegacyZakatCalculator to ModernZakatCalculator
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
6. **Sharia Compliance**: Treat as explicit business requirements with dedicated tests
7. **Team Adoption**: Start small, demonstrate value, pair program
8. **Brownfield Projects**: Add characterization tests, use TDD for new features

**Next Steps**:

- Review [Templates](./templates/) for standardized test structures
- Explore [Decision Trees and Best Practices](./ex-so-de-tedrdeve__16-decision-trees-and-best-practices.md)

**Related Resources**:

- [Core Concepts](./ex-so-de-tedrdeve__01-introduction-and-philosophy.md)
- [Red-Green-Refactor](./ex-so-de-tedrdeve__02-red-green-refactor-cycle.md)
- [Testing Pyramid](./ex-so-de-tedrdeve__03-test-types-and-pyramid.md)
