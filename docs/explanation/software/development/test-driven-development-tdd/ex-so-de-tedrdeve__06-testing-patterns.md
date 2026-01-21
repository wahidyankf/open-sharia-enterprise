# Test-Driven Development: Testing Patterns

## Overview

Testing patterns are structured approaches to organizing and writing tests. They provide consistency, readability, and maintainability across test suites. Well-established patterns make tests easier to understand, debug, and extend.

This document covers the most important testing patterns in TDD: Arrange-Act-Assert (AAA), Given-When-Then (BDD style), table-driven tests, parameterized tests, snapshot testing, and test organization strategies. Mastering these patterns accelerates test writing and improves test quality.

## Core Principles

Testing patterns align with software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - AAA and Given-When-Then patterns make test structure explicit: setup, execution, and verification are clearly delineated. Table-driven tests explicitly show all test cases in one place, making coverage immediately visible.

## Arrange-Act-Assert (AAA) Pattern

The AAA pattern is the fundamental structure for unit tests. It divides each test into three clear phases:

1. **Arrange**: Set up test data and dependencies
2. **Act**: Execute the behavior being tested
3. **Assert**: Verify the expected outcome

### Basic Structure

```typescript
describe("TaxCalculator", () => {
  it("should calculate 2.5% tax on eligible wealth", () => {
    // ARRANGE - Set up test conditions
    const calculator = new TaxCalculator();
    const wealth = Money.fromGold(100, "grams");
    const threshold = Money.fromGold(85, "grams");

    // ACT - Execute the behavior
    const tax = calculator.calculate(wealth, threshold);

    // ASSERT - Verify the outcome
    expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true);
  });
});
```

### Benefits

- **Visual Clarity**: Blank lines separate phases, making test flow obvious
- **Single Responsibility**: Each section has one job
- **Easy Debugging**: Failures point to specific phase (setup vs logic vs verification)
- **Maintainability**: Changes to one phase rarely affect others

### AAA in Asynchronous Tests

```typescript
describe("LoanContractService", () => {
  it("should create contract and persist to database", async () => {
    // ARRANGE
    const repository = new InMemoryContractRepository();
    const service = new LoanContractService(repository);

    const contractRequest = {
      customerId: "CUST-001",
      assetPrice: Money.fromUSD(50000),
      markup: Money.fromUSD(2500),
      installments: 12,
    };

    // ACT
    const contract = await service.createContract(contractRequest);

    // ASSERT
    expect(contract.id).toBeDefined();
    expect(contract.totalDue.equals(Money.fromUSD(52500))).toBe(true);

    const persisted = await repository.findById(contract.id);
    expect(persisted).toBeDefined();
  });
});
```

### Multiple Acts and Asserts (Anti-Pattern)

```typescript
// BAD: Multiple act-assert cycles in single test
it("should handle contract lifecycle", async () => {
  const service = new LoanContractService(repository);

  // Act 1
  const contract = await service.createContract(request);
  // Assert 1
  expect(contract.status).toBe("PENDING");

  // Act 2
  await service.approveContract(contract.id);
  // Assert 2
  const approved = await service.getContract(contract.id);
  expect(approved.status).toBe("APPROVED");

  // Act 3
  await service.activateContract(contract.id);
  // Assert 3
  const active = await service.getContract(contract.id);
  expect(active.status).toBe("ACTIVE");
});

// GOOD: Separate tests for each behavior
describe("LoanContractService lifecycle", () => {
  it("should create contract in pending status", async () => {
    // Arrange
    const service = new LoanContractService(repository);

    // Act
    const contract = await service.createContract(request);

    // Assert
    expect(contract.status).toBe("PENDING");
  });

  it("should approve pending contract", async () => {
    // Arrange
    const service = new LoanContractService(repository);
    const contract = await service.createContract(request);

    // Act
    await service.approveContract(contract.id);

    // Assert
    const approved = await service.getContract(contract.id);
    expect(approved.status).toBe("APPROVED");
  });

  it("should activate approved contract", async () => {
    // Arrange
    const service = new LoanContractService(repository);
    const contract = await service.createContract(request);
    await service.approveContract(contract.id);

    // Act
    await service.activateContract(contract.id);

    // Assert
    const active = await service.getContract(contract.id);
    expect(active.status).toBe("ACTIVE");
  });
});
```

## Given-When-Then (BDD Pattern)

Given-When-Then is the Behavior-Driven Development (BDD) variant of AAA. It emphasizes user-facing behavior and domain language. The structure is identical to AAA but uses domain terminology.

- **Given**: Preconditions and context (Arrange)
- **When**: Action or event (Act)
- **Then**: Expected outcome (Assert)

### Basic Structure

```typescript
describe("Permitted Certification", () => {
  it("should approve certification when all ingredients are permitted", () => {
    // GIVEN - Context and preconditions
    const validator = new PermittedIngredientValidator();
    const application = buildCertificationApplication({
      productName: "Chicken Nuggets",
      ingredients: [
        { name: "chicken", source: "permitted-certified-farm" },
        { name: "breadcrumbs", source: "permitted-supplier" },
        { name: "spices", source: "natural" },
      ],
    });

    // WHEN - Action occurs
    const result = validator.validate(application);

    // THEN - Expected outcome
    expect(result.approved).toBe(true);
    expect(result.violations).toHaveLength(0);
  });
});
```

### BDD with Domain Events

```typescript
describe("Takaful Claim Processing", () => {
  it("should publish claim approved event when claim is approved", async () => {
    // GIVEN - Claim submitted and under review
    const eventBus = new InMemoryEventBus();
    const claimService = new TakafulClaimService(eventBus);

    const claim = buildClaim({
      policyId: "POL-001",
      amount: Money.fromSAR(10000),
      incidentDate: new Date("2024-01-10"),
      description: "Vehicle accident",
    });

    await claimService.submitClaim(claim);

    // WHEN - Approver approves the claim
    await claimService.approveClaim(claim.id, "Valid claim, all documentation provided");

    // THEN - Claim approved event is published
    const events = eventBus.getPublishedEvents();
    expect(events).toHaveLength(1);
    expect(events[0].type).toBe("CLAIM_APPROVED");
    expect(events[0].payload.claimId).toBe(claim.id);
    expect(events[0].payload.amount.equals(Money.fromSAR(10000))).toBe(true);
  });
});
```

### When to Use AAA vs Given-When-Then

| Context                      | Pattern             | Reason                             |
| ---------------------------- | ------------------- | ---------------------------------- |
| Unit tests (technical focus) | **AAA**             | Clear, concise, developer-friendly |
| Integration/E2E tests        | **Given-When-Then** | Aligns with user scenarios         |
| Domain-driven design         | **Given-When-Then** | Uses ubiquitous language           |
| BDD with stakeholders        | **Given-When-Then** | Non-technical readability          |
| Pure functions               | **AAA**             | Simple input → output              |

## Table-Driven Tests (Parameterized Tests)

Table-driven tests verify the same behavior across multiple input/output combinations. They reduce duplication and make test cases explicit.

### Basic Table-Driven Test

```typescript
describe("Tax Calculator - Multiple Scenarios", () => {
  it.each([
    // [wealth, threshold, expectedTax]
    [100, 85, 2.5], // Above threshold
    [85, 85, 2.125], // Exactly at threshold
    [84.99, 85, 0], // Below threshold
    [0, 85, 0], // Zero wealth
    [1000, 85, 25], // Large amount
  ])("should calculate tax correctly for wealth=%d, threshold=%d", (wealth, threshold, expected) => {
    // Arrange
    const calculator = new TaxCalculator();
    const wealthAmount = Money.fromGold(wealth, "grams");
    const thresholdAmount = Money.fromGold(threshold, "grams");

    // Act
    const tax = calculator.calculate(wealthAmount, thresholdAmount);

    // Assert
    expect(tax.equals(Money.fromGold(expected, "grams"))).toBe(true);
  });
});
```

### Table-Driven Tests with Complex Objects

```typescript
describe("Loan Installment Calculator", () => {
  const testCases = [
    {
      name: "12-month contract with small principal",
      principal: 10000,
      markup: 500,
      months: 12,
      expectedMonthlyPayment: 875,
      expectedTotal: 10500,
    },
    {
      name: "24-month contract with moderate principal",
      principal: 50000,
      markup: 5000,
      months: 24,
      expectedMonthlyPayment: 2291.67,
      expectedTotal: 55000,
    },
    {
      name: "6-month contract with large principal",
      principal: 100000,
      markup: 3000,
      months: 6,
      expectedMonthlyPayment: 17166.67,
      expectedTotal: 103000,
    },
  ];

  testCases.forEach(({ name, principal, markup, months, expectedMonthlyPayment, expectedTotal }) => {
    it(`should calculate installments for ${name}`, () => {
      // Arrange
      const calculator = new LoanInstallmentCalculator();
      const contract = {
        principal: Money.fromUSD(principal),
        markup: Money.fromUSD(markup),
        termMonths: months,
      };

      // Act
      const schedule = calculator.calculateSchedule(contract);

      // Assert
      expect(schedule.monthlyPayment.amount).toBeCloseTo(expectedMonthlyPayment, 2);
      expect(schedule.totalPayable.amount).toBeCloseTo(expectedTotal, 2);
      expect(schedule.installments).toHaveLength(months);
    });
  });
});
```

### Benefits of Table-Driven Tests

- **Comprehensive Coverage**: Easily add edge cases
- **Documentation**: Test cases serve as specification
- **Reduced Duplication**: Test logic written once
- **Regression Safety**: New test case = one line addition

## Snapshot Testing

Snapshot testing captures the output of a function and compares it to a saved "snapshot" on subsequent runs. Useful for complex outputs that are tedious to assert manually.

### When to Use Snapshot Testing

**Good Use Cases:**

- Complex data structures (JSON API responses)
- Generated reports or documents
- UI component rendering (React, Vue)
- Domain event payloads

**Bad Use Cases:**

- Simple values (use exact assertions instead)
- Non-deterministic outputs (random IDs, timestamps)
- Critical calculations (snapshot hides business logic)

### Example: Tax Report Snapshot

```typescript
describe("TaxAnnualReport", () => {
  it("should generate annual report with correct structure", () => {
    // Arrange
    const reportGenerator = new TaxAnnualReportGenerator();
    const records = [
      buildTaxRecord({ date: new Date("2024-03-15"), amount: Money.fromUSD(250) }),
      buildTaxRecord({ date: new Date("2024-06-10"), amount: Money.fromUSD(300) }),
      buildTaxRecord({ date: new Date("2024-09-20"), amount: Money.fromUSD(400) }),
    ];

    // Act
    const report = reportGenerator.generate(records, 2024);

    // Assert
    expect(report).toMatchSnapshot();
    // Snapshot includes:
    // - Total tax paid
    // - Breakdown by quarter
    // - Payment dates
    // - Asset types
  });
});
```

### Snapshot Test Best Practices

1. **Review snapshots carefully**: Ensure initial snapshot is correct
2. **Keep snapshots small**: Large snapshots are hard to review
3. **Use inline snapshots for critical values**: `expect(value).toMatchInlineSnapshot()`
4. **Combine with explicit assertions**: Snapshot for structure, assertions for critical values

```typescript
it("should generate sukuk dividend report", () => {
  const report = generator.generateSukukReport(holdings);

  // Explicit assertion for critical value
  expect(report.totalDividend.equals(Money.fromUSD(1250))).toBe(true);

  // Snapshot for overall structure
  expect(report).toMatchSnapshot();
});
```

## Test Organization Patterns

### 1. One Test Class Per Production Class

```
src/
  domain/
    TaxCalculator.ts
  __tests__/
    TaxCalculator.test.ts
```

### 2. Nested Describe Blocks (Context Organization)

```typescript
describe("DonationManagementService", () => {
  describe("createDonation", () => {
    it("should create donation with valid beneficiaries", async () => {
      // Test implementation
    });

    it("should reject donation with no beneficiaries", async () => {
      // Test implementation
    });

    it("should reject donation with negative asset value", async () => {
      // Test implementation
    });
  });

  describe("distributeIncome", () => {
    it("should distribute income proportionally to beneficiaries", async () => {
      // Test implementation
    });

    it("should skip distribution when no income available", async () => {
      // Test implementation
    });
  });

  describe("transferDonationOwnership", () => {
    it("should transfer ownership to new custodian", async () => {
      // Test implementation
    });

    it("should reject transfer to invalid custodian", async () => {
      // Test implementation
    });
  });
});
```

### 3. Setup and Teardown Hooks

```typescript
describe("TakafulPolicyService", () => {
  let repository: InMemoryPolicyRepository;
  let service: TakafulPolicyService;

  beforeEach(() => {
    // Run before EACH test
    repository = new InMemoryPolicyRepository();
    service = new TakafulPolicyService(repository);
  });

  afterEach(() => {
    // Run after EACH test (cleanup)
    repository.clear();
  });

  it("should create policy", async () => {
    const policy = await service.createPolicy(policyInput);
    expect(policy.id).toBeDefined();
  });

  it("should find policy by id", async () => {
    const created = await service.createPolicy(policyInput);
    const found = await service.findPolicy(created.id);
    expect(found).toEqual(created);
  });
});
```

**Hooks Summary:**

- `beforeAll`: Run once before all tests in suite (expensive setup)
- `beforeEach`: Run before each test (fresh state)
- `afterEach`: Run after each test (cleanup)
- `afterAll`: Run once after all tests (teardown shared resources)

### 4. Test Builders (Object Mother Pattern)

Extract common test data creation into builder functions:

```typescript
// Test builders
function buildPermittedCertificationApplication(overrides?: Partial<Application>): Application {
  return {
    id: "APP-001",
    productName: "Permitted Chicken Sausage",
    manufacturer: "Permitted Foods Ltd",
    ingredients: [
      { name: "chicken", source: "permitted-farm" },
      { name: "spices", source: "natural" },
    ],
    submittedAt: new Date("2024-01-15"),
    status: "PENDING",
    ...overrides,
  };
}

function buildRejectedApplication(overrides?: Partial<Application>): Application {
  return buildPermittedCertificationApplication({
    status: "REJECTED",
    ingredients: [
      { name: "pork", source: "conventional-farm" }, // Non-permitted
    ],
    ...overrides,
  });
}

// Usage in tests
describe("PermittedCertificationService", () => {
  it("should approve application with permitted ingredients", () => {
    const application = buildPermittedCertificationApplication();

    const result = service.review(application);

    expect(result.approved).toBe(true);
  });

  it("should reject application with non-permitted ingredients", () => {
    const application = buildRejectedApplication();

    const result = service.review(application);

    expect(result.approved).toBe(false);
  });
});
```

See **[07. Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md)** for comprehensive coverage.

## Property-Based Testing

Property-based testing generates random test inputs to verify properties (invariants) that should always hold true. Complements example-based tests.

### Example: Tax Invariants

```typescript
import fc from "fast-check";

describe("TaxCalculator - Property-Based Tests", () => {
  it("should always return non-negative tax", () => {
    fc.assert(
      fc.property(
        fc.double({ min: 0, max: 1000000 }), // Random wealth
        fc.double({ min: 0, max: 100000 }), // Random threshold
        (wealth, threshold) => {
          const calculator = new TaxCalculator();
          const wealthAmount = Money.fromGold(wealth, "grams");
          const thresholdAmount = Money.fromGold(threshold, "grams");

          const tax = calculator.calculate(wealthAmount, thresholdAmount);

          // Property: Tax is never negative
          expect(tax.amount).toBeGreaterThanOrEqual(0);
        },
      ),
    );
  });

  it("should calculate tax at exactly 2.5% when wealth exceeds threshold", () => {
    fc.assert(
      fc.property(
        fc.double({ min: 100, max: 1000000 }), // Random wealth > threshold
        (wealth) => {
          const calculator = new TaxCalculator();
          const threshold = Money.fromGold(85, "grams");
          const wealthAmount = Money.fromGold(wealth, "grams");

          const tax = calculator.calculate(wealthAmount, threshold);

          // Property: Tax is exactly 2.5% of wealth
          const expected = wealth * 0.025;
          expect(tax.amount).toBeCloseTo(expected, 2);
        },
      ),
    );
  });
});
```

### Benefits of Property-Based Testing

- Discovers edge cases you didn't think of
- Tests invariants (properties that must always hold)
- Generates hundreds of test cases automatically
- Shrinks failing cases to minimal examples

## Test Naming Conventions

### Pattern 1: "should [expected behavior] when [condition]"

```typescript
it("should calculate tax when wealth exceeds threshold", () => {});
it("should return zero when wealth is below threshold", () => {});
it("should reject contract when markup exceeds 10%", () => {});
```

### Pattern 2: "should [expected behavior] given [context]"

```typescript
it("should approve certification given all permitted ingredients", () => {});
it("should distribute income proportionally given multiple beneficiaries", () => {});
```

### Pattern 3: "[method name] - [scenario] - [expected result]"

```typescript
it("calculateTax - wealth below threshold - returns zero", () => {});
it("approveContract - invalid customer - throws error", () => {});
```

### Bad Test Names (Avoid)

```typescript
// Too vague
it("works correctly", () => {});
it("test tax", () => {});

// Implementation details
it("calls getThreshold and multiplies by 0.025", () => {});

// Testing multiple behaviors
it("creates contract, approves it, and activates it", () => {});
```

## Testing Exceptions and Error Cases

### Pattern 1: Expect Thrown Error

```typescript
describe("LoanContractService", () => {
  it("should throw error when markup exceeds 10%", () => {
    const service = new LoanContractService();

    const invalidContract = {
      principal: Money.fromUSD(10000),
      markup: Money.fromUSD(1500), // 15% - exceeds limit
      termMonths: 12,
    };

    expect(() => service.validateContract(invalidContract)).toThrow("Markup cannot exceed 10% of principal");
  });
});
```

### Pattern 2: Async Error Testing

```typescript
it("should reject contract when customer has poor credit score", async () => {
  const creditChecker = {
    checkCredit: async () => ({ score: 400, approved: false }),
  };
  const service = new LoanContractService(creditChecker);

  await expect(service.createContract(contractRequest)).rejects.toThrow("Customer credit score too low");
});
```

### Pattern 3: Verify Error Details

```typescript
it("should throw validation error with specific details", () => {
  try {
    service.validateDonation(invalidDonation);
    fail("Expected validation error");
  } catch (error) {
    expect(error).toBeInstanceOf(ValidationError);
    expect(error.message).toContain("Beneficiary shares must sum to 100%");
    expect(error.details.totalShares).toBe(85);
  }
});
```

## Testing Asynchronous Code

### Pattern 1: async/await (Preferred)

```typescript
it("should save contract to repository", async () => {
  const repository = new InMemoryContractRepository();
  const service = new LoanContractService(repository);

  const contract = await service.createContract(request);

  const saved = await repository.findById(contract.id);
  expect(saved).toBeDefined();
});
```

### Pattern 2: Promise Chains (Legacy)

```typescript
it("should save contract to repository", () => {
  const repository = new InMemoryContractRepository();
  const service = new LoanContractService(repository);

  return service
    .createContract(request)
    .then((contract) => repository.findById(contract.id))
    .then((saved) => {
      expect(saved).toBeDefined();
    });
});
```

### Pattern 3: Callbacks (Avoid)

```typescript
// AVOID - Use async/await instead
it("should save contract to repository", (done) => {
  const service = new LoanContractService(repository);

  service.createContract(request, (error, contract) => {
    expect(error).toBeNull();
    expect(contract.id).toBeDefined();
    done();
  });
});
```

## Summary

Testing patterns provide structure and consistency:

- **AAA Pattern**: Arrange-Act-Assert for clear test structure
- **Given-When-Then**: BDD variant emphasizing domain behavior
- **Table-Driven Tests**: Verify multiple scenarios with same logic
- **Snapshot Testing**: Capture complex outputs for regression testing
- **Property-Based Testing**: Verify invariants with generated inputs
- **Test Organization**: Nested describes, setup/teardown hooks, test builders
- **Naming Conventions**: Clear, behavior-focused test names
- **Error Testing**: Explicit exception verification
- **Async Testing**: async/await for promises

**Key Principles:**

1. One assertion focus per test (single responsibility)
2. Tests should be readable as documentation
3. Extract common setup into builders or hooks
4. Use table-driven tests to reduce duplication
5. Combine snapshot tests with explicit assertions
6. Property-based tests complement example-based tests

## Related Documentation

- **[04. Unit Testing Fundamentals](./ex-so-de-tedrdeve__04-unit-testing-fundamentals.md)** - Test structure and FIRST principles
- **[05. Test Doubles](./ex-so-de-tedrdeve__05-test-doubles.md)** - Mocks, stubs, fakes, spies
- **[07. Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md)** - Builder pattern and Object Mother
- **[08. Assertion Patterns](./ex-so-de-tedrdeve__08-assertion-patterns.md)** - Effective assertions and custom matchers
- **[11. TDD and Functional Programming](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md)** - Property-based testing in depth

## Related Principles

- [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)
