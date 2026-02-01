# Test-Driven Development: Test Doubles

## Overview

Test doubles are objects or functions that stand in for real dependencies during testing. They enable fast, isolated unit tests by replacing expensive or unpredictable components (databases, external APIs, time providers) with controllable alternatives.

The term "test double" comes from the film industry's "stunt double"—just as a stunt performer stands in for an actor during dangerous scenes, a test double stands in for a real dependency during testing. Test doubles allow you to verify behavior in isolation, control test conditions precisely, and execute tests in milliseconds rather than seconds.

Mastering test doubles is essential for effective TDD. They enable the "Fast" and "Independent" principles of good unit tests, prevent test fragility, and provide surgical precision when verifying interactions between components.

## Core Principles

Test doubles align with software engineering principles:

- **[Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)** - Test doubles isolate side effects (I/O, time, randomness) from pure business logic, enabling fast deterministic tests. By mocking external dependencies while keeping domain logic pure, you verify correctness without slow integration tests.
- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Each test double type has explicit semantics: stubs return canned data, mocks verify interactions, fakes provide working implementations. This explicitness makes test intent immediately clear.

## Types of Test Doubles

Gerard Meszaros's "xUnit Test Patterns" (2007) cataloged five types of test doubles, each serving different purposes. Understanding when to use each type is critical for writing maintainable tests.

```mermaid
graph TD
    A[Test Double] --> B[Dummy]
    A --> C[Stub]
    A --> D[Spy]
    A --> E[Mock]
    A --> F[Fake]

    B --> B1[Never called<br/>Satisfies parameter]
    C --> C1[Returns canned data<br/>No verification]
    D --> D1[Records calls<br/>Verify after act]
    E --> E1[Expects specific calls<br/>Verify during act]
    F --> F1[Working implementation<br/>Simplified version]

    style A fill:#0173B2,stroke:#000,color:#FFFFFF
    style B fill:#029E73,stroke:#000,color:#FFFFFF
    style C fill:#029E73,stroke:#000,color:#FFFFFF
    style D fill:#DE8F05,stroke:#000,color:#000000
    style E fill:#DE8F05,stroke:#000,color:#000000
    style F fill:#CC78BC,stroke:#000,color:#FFFFFF
    style B1 fill:#808080,stroke:#000,color:#000000
    style C1 fill:#808080,stroke:#000,color:#000000
    style D1 fill:#808080,stroke:#000,color:#000000
    style E1 fill:#808080,stroke:#000,color:#000000
    style F1 fill:#808080,stroke:#000,color:#000000
```

### 1. Dummy Objects

**Definition**: Objects passed to satisfy parameter requirements but never actually used.

**When to use**: Method requires a parameter that won't be called in the specific test scenario.

**Example - Islamic Calendar Date (Unused Parameter):**

```typescript
// DUMMY: DateProvider passed but never called
describe("TaxCalculator", () => {
  it("should return zero when wealth below threshold", () => {
    // Arrange
    const thresholdProvider = { getThreshold: () => Money.fromGold(85, "grams") };
    const dateProvider = null as any; // DUMMY - not used in this test
    const calculator = new TaxCalculator(thresholdProvider, dateProvider);

    const wealth = Money.fromGold(50, "grams");

    // Act
    const tax = calculator.calculate(wealth);

    // Assert
    expect(tax.equals(Money.zero("gold"))).toBe(true);
  });
});
```

**Characteristics:**

- Never actually invoked during test
- Simplest form of test double
- Often `null`, `undefined`, or empty object
- Used to satisfy type requirements

**When NOT to use**: If the parameter is actually called, use a stub or mock instead.

### 2. Stubs

**Definition**: Objects that return pre-configured responses (canned data) without verification logic.

**When to use**: You need to control the response from a dependency but don't care how many times it's called or with what arguments.

**Example - Threshold Provider Stub:**

```typescript
// STUB: Returns fixed threshold value, no verification
describe("TaxCalculator", () => {
  it("should calculate 2.5% when wealth exceeds threshold", () => {
    // Arrange - Stub returns fixed value
    const thresholdProvider = {
      getThreshold: (assetType: string) => Money.fromGold(85, "grams"),
    };

    const calculator = new TaxCalculator(thresholdProvider);
    const wealth = Money.fromGold(100, "grams");

    // Act
    const tax = calculator.calculate(wealth);

    // Assert - Only verify calculation result
    expect(tax.equals(Money.fromGold(2.5, "grams"))).toBe(true);
    // No verification of thresholdProvider calls
  });
});
```

**Characteristics:**

- Returns hardcoded values
- No verification logic
- Simplest useful test double
- State-based testing (verify return values, not interactions)

**Good for:**

- External data sources (API responses, configuration)
- Time providers (current date/time)
- Random value generators (fixed seed)

### 3. Spies

**Definition**: Objects that record how they were called (arguments, call count, call order), allowing verification **after** the action.

**When to use**: You need to verify that a dependency was called correctly, but you want the verification to happen after the act phase (cleaner AAA structure).

**Example - Audit Logger Spy:**

```typescript
// SPY: Records calls for later verification
describe("PermittedCertification", () => {
  it("should log certification approval to audit trail", () => {
    // Arrange
    const auditLogSpy = {
      calls: [] as any[],
      log(event: AuditEvent) {
        this.calls.push(event);
      },
    };

    const certificationService = new PermittedCertificationService(auditLogSpy);
    const application = buildCertificationApplication({
      productName: "Permitted Beef Sausage",
      ingredients: ["beef", "spices", "water"],
    });

    // Act
    certificationService.approve(application);

    // Assert - Verify spy recorded the call
    expect(auditLogSpy.calls).toHaveLength(1);
    expect(auditLogSpy.calls[0].eventType).toBe("CERTIFICATION_APPROVED");
    expect(auditLogSpy.calls[0].productName).toBe("Permitted Beef Sausage");
  });
});
```

**Characteristics:**

- Records all calls
- Verification happens **after** act phase
- More flexible than mocks
- Supports state-based verification of interactions

**Spy with Sinon.js (JavaScript):**

```typescript
import sinon from "sinon";

it("should call audit log with certification event", () => {
  const auditLog = { log: sinon.spy() };
  const service = new PermittedCertificationService(auditLog);

  service.approve(application);

  expect(auditLog.log.calledOnce).toBe(true);
  expect(auditLog.log.firstCall.args[0].eventType).toBe("CERTIFICATION_APPROVED");
});
```

### 4. Mocks

**Definition**: Objects with pre-programmed expectations that verify calls **during** the test execution. Mocks fail the test if expectations aren't met.

**When to use**: You need to verify precise interactions (method calls, arguments, call order) and want the test framework to enforce expectations automatically.

**Example - Payment Gateway Mock:**

```typescript
// MOCK: Expects specific interaction pattern
describe("TakafulClaimService", () => {
  it("should process claim payment through Islamic bank", () => {
    // Arrange - Mock expects specific calls
    const bankGatewayMock = {
      _expectedCalls: [] as any[],
      _actualCalls: [] as any[],

      expectTransfer(from: Account, to: Account, amount: Money) {
        this._expectedCalls.push({ method: "transfer", from, to, amount });
        return this;
      },

      transfer(from: Account, to: Account, amount: Money): Promise<TransferResult> {
        this._actualCalls.push({ method: "transfer", from, to, amount });
        return Promise.resolve({ success: true, transactionId: "TXN-123" });
      },

      verify() {
        expect(this._actualCalls).toEqual(this._expectedCalls);
      },
    };

    bankGatewayMock.expectTransfer(Account.fromIban("SA123456"), Account.fromIban("SA789012"), Money.fromSAR(5000));

    const claimService = new TakafulClaimService(bankGatewayMock);

    // Act
    claimService.payApprovedClaim(claimId);

    // Assert - Mock verifies expectations
    bankGatewayMock.verify();
  });
});
```

**Mock with Jest (JavaScript/TypeScript):**

```typescript
it("should transfer funds through Islamic bank", async () => {
  const bankGateway = {
    transfer: jest.fn().mockResolvedValue({ success: true, id: "TXN-123" }),
  };

  const service = new TakafulClaimService(bankGateway);

  await service.payApprovedClaim(claimId);

  expect(bankGateway.transfer).toHaveBeenCalledWith(
    Account.fromIban("SA123456"),
    Account.fromIban("SA789012"),
    Money.fromSAR(5000),
  );
  expect(bankGateway.transfer).toHaveBeenCalledTimes(1);
});
```

**Characteristics:**

- Pre-programmed expectations
- Fails test if expectations not met
- Verification integrated into mock itself
- Interaction-based testing (verify method calls)

**Warning**: Overuse of mocks leads to brittle tests coupled to implementation details. Prefer spies when possible.

### 5. Fakes

**Definition**: Working implementations with simplified logic, often in-memory alternatives to real dependencies.

**When to use**: Real implementation is too slow or complex, but you need more realistic behavior than a stub provides.

**Example - In-Memory Donation Repository (Fake Database):**

```typescript
// FAKE: In-memory implementation of repository
class InMemoryDonationRepository implements DonationRepository {
  private donations: Map<DonationId, Donation> = new Map();

  async save(donation: Donation): Promise<void> {
    this.donations.set(donation.id, donation);
  }

  async findById(id: DonationId): Promise<Donation | null> {
    return this.donations.get(id) || null;
  }

  async findByBeneficiary(beneficiaryId: string): Promise<Donation[]> {
    return Array.from(this.donations.values()).filter((w) => w.beneficiaries.some((b) => b.id === beneficiaryId));
  }

  clear() {
    this.donations.clear();
  }
}

describe("DonationManagementService", () => {
  let repository: InMemoryDonationRepository;
  let service: DonationManagementService;

  beforeEach(() => {
    repository = new InMemoryDonationRepository();
    service = new DonationManagementService(repository);
  });

  it("should create and retrieve donation by id", async () => {
    // Arrange
    const donationInput = {
      beneficiaries: [{ id: "mosque-001", share: 100 }],
      assets: [{ type: "LAND", value: Money.fromUSD(100000) }],
    };

    // Act
    const donationId = await service.createDonation(donationInput);
    const retrieved = await service.getDonation(donationId);

    // Assert
    expect(retrieved).toBeDefined();
    expect(retrieved!.beneficiaries).toHaveLength(1);
    expect(retrieved!.assets[0].value.equals(Money.fromUSD(100000))).toBe(true);
  });
});
```

**Characteristics:**

- Working implementation (not just canned responses)
- Simplified (in-memory vs. database, synchronous vs. async)
- Shared across multiple tests
- Realistic behavior without real infrastructure

**Common Fakes:**

- In-memory databases
- In-memory file systems
- Simplified HTTP servers
- Local time providers

**Trade-off**: Fakes require maintenance (must stay consistent with real implementation).

## When to Use Each Type

```mermaid
flowchart TD
    Start[Need test double?] --> Question1{Is parameter<br/>ever used?}
    Question1 -->|No| Dummy[Use DUMMY]
    Question1 -->|Yes| Question2{Need to verify<br/>interactions?}

    Question2 -->|No| Question3{Need realistic<br/>behavior?}
    Question3 -->|No| Stub[Use STUB]
    Question3 -->|Yes| Fake[Use FAKE]

    Question2 -->|Yes| Question4{Need strict<br/>expectations?}
    Question4 -->|No| Spy[Use SPY]
    Question4 -->|Yes| Mock[Use MOCK]

    style Start fill:#0173B2,stroke:#000,color:#FFFFFF
    style Dummy fill:#029E73,stroke:#000,color:#FFFFFF
    style Stub fill:#029E73,stroke:#000,color:#FFFFFF
    style Spy fill:#DE8F05,stroke:#000,color:#000000
    style Mock fill:#DE8F05,stroke:#000,color:#000000
    style Fake fill:#CC78BC,stroke:#000,color:#FFFFFF
    style Question1 fill:#808080,stroke:#000,color:#000000
    style Question2 fill:#808080,stroke:#000,color:#000000
    style Question3 fill:#808080,stroke:#000,color:#000000
    style Question4 fill:#808080,stroke:#000,color:#000000
```

**Decision Matrix:**

| Scenario                       | Test Double Type | Reason                                        |
| ------------------------------ | ---------------- | --------------------------------------------- |
| Unused constructor parameter   | **Dummy**        | Not called in test                            |
| External API returning data    | **Stub**         | Control response, no verification needed      |
| Time/date provider             | **Stub**         | Fixed time for deterministic tests            |
| Repository (simple queries)    | **Fake**         | Need realistic CRUD behavior                  |
| Event publisher verification   | **Spy**          | Verify calls after action                     |
| Payment gateway (strict order) | **Mock**         | Enforce exact call sequence                   |
| Notification service           | **Spy**          | Verify notification sent, flexible assertions |

## Test Doubles in Practice

### Example 1: Tax Calculation with Multiple Doubles

```typescript
describe("TaxCalculationService", () => {
  it("should calculate tax and record in audit log", async () => {
    // STUB: Fixed threshold threshold
    const thresholdProvider = {
      getThreshold: (assetType: string) => Money.fromGold(85, "grams"),
    };

    // STUB: Fixed date for Hawl calculation
    const clockStub = {
      now: () => new Date("2024-01-15"),
    };

    // SPY: Verify audit logging
    const auditLogSpy = {
      entries: [] as any[],
      log(entry: AuditEntry) {
        this.entries.push(entry);
      },
    };

    // FAKE: In-memory repository
    const repository = new InMemoryTaxRepository();

    const service = new TaxCalculationService({
      thresholdProvider,
      clock: clockStub,
      auditLog: auditLogSpy,
      repository,
    });

    // Act
    const wealth = Money.fromGold(100, "grams");
    const result = await service.calculateAndRecord(wealth, "gold");

    // Assert - Verify calculation
    expect(result.taxDue.equals(Money.fromGold(2.5, "grams"))).toBe(true);

    // Assert - Verify audit logging (spy)
    expect(auditLogSpy.entries).toHaveLength(1);
    expect(auditLogSpy.entries[0].action).toBe("TAX_CALCULATED");

    // Assert - Verify persistence (fake)
    const stored = await repository.findByDate(new Date("2024-01-15"));
    expect(stored).toHaveLength(1);
  });
});
```

### Example 2: Loan Contract with Mock Expectations

```typescript
describe("LoanContractService", () => {
  it("should execute contract flow with bank approval", async () => {
    // MOCK: Islamic bank requires specific approval sequence
    const bankMock = {
      calls: [] as string[],

      async validateCustomer(customerId: string): Promise<boolean> {
        this.calls.push(`validateCustomer:${customerId}`);
        return true;
      },

      async checkCreditworthiness(customerId: string): Promise<CreditScore> {
        this.calls.push(`checkCredit:${customerId}`);
        return { score: 750, approved: true };
      },

      async approvePurchase(contractId: string, amount: Money): Promise<Approval> {
        this.calls.push(`approvePurchase:${contractId}:${amount.amount}`);
        return { approved: true, approvalId: "APR-123" };
      },

      verify() {
        // Verify exact sequence
        expect(this.calls).toEqual([
          "validateCustomer:CUST-001",
          "checkCredit:CUST-001",
          "approvePurchase:CONTRACT-001:10000",
        ]);
      },
    };

    const service = new LoanContractService(bankMock);

    // Act
    await service.executeContract({
      contractId: "CONTRACT-001",
      customerId: "CUST-001",
      assetPrice: Money.fromUSD(10000),
      markup: Money.fromUSD(500),
    });

    // Assert
    bankMock.verify();
  });
});
```

### Example 3: Test Double Progression (Evolution)

Show how test doubles evolve as requirements change:

```typescript
// ITERATION 1: Simple stub
it("should calculate tax - v1", () => {
  const thresholdProvider = { getThreshold: () => 85 }; // Simple stub
  const calculator = new TaxCalculator(thresholdProvider);

  const tax = calculator.calculate(100);
  expect(tax).toBe(2.5);
});

// ITERATION 2: Stub with different asset types
it("should calculate tax - v2", () => {
  const thresholdProvider = {
    getThreshold: (assetType: string) => {
      return assetType === "gold" ? 85 : 595; // Silver threshold
    },
  };

  const calculator = new TaxCalculator(thresholdProvider);

  expect(calculator.calculate(100, "gold")).toBe(2.5);
  expect(calculator.calculate(600, "silver")).toBe(15);
});

// ITERATION 3: Spy to verify caching behavior
it("should calculate tax - v3 (cached)", () => {
  const thresholdProviderSpy = {
    callCount: 0,
    getThreshold: (assetType: string) => {
      thresholdProviderSpy.callCount++;
      return assetType === "gold" ? 85 : 595;
    },
  };

  const calculator = new TaxCalculator(thresholdProviderSpy);

  calculator.calculate(100, "gold");
  calculator.calculate(100, "gold"); // Should use cache

  expect(thresholdProviderSpy.callCount).toBe(1); // Called only once
});

// ITERATION 4: Fake repository for integration
it("should calculate tax - v4 (persisted)", async () => {
  const repository = new InMemoryTaxRepository();
  const thresholdProvider = { getThreshold: (type: string) => 85 };
  const service = new TaxCalculationService(thresholdProvider, repository);

  await service.calculateAndStore(100, "gold", "USER-001");

  const history = await repository.findByUser("USER-001");
  expect(history).toHaveLength(1);
  expect(history[0].amount).toBe(2.5);
});
```

## Mocking Frameworks

Popular frameworks for creating test doubles in different languages:

### JavaScript/TypeScript

**Jest (Built-in Mocking):**

```typescript
// Mock function
const mockFn = jest.fn();
mockFn.mockReturnValue(42);
mockFn.mockResolvedValue(Promise.resolve(42));

// Mock module
jest.mock("./thresholdProvider", () => ({
  getThreshold: jest.fn().mockReturnValue(85),
}));

// Spy on existing method
const spy = jest.spyOn(calculator, "calculate");
expect(spy).toHaveBeenCalledWith(100, "gold");
```

**Sinon.js:**

```typescript
import sinon from "sinon";

// Stub
const stub = sinon.stub().returns(85);

// Spy
const spy = sinon.spy(auditLog, "log");
expect(spy.calledOnce).toBe(true);

// Mock with expectations
const mock = sinon.mock(bankGateway);
mock.expects("transfer").once().withArgs(account, Money.fromUSD(100));
mock.verify();
```

### Java

**Mockito:**

```java
// Mock
ThresholdProvider thresholdProvider = mock(ThresholdProvider.class);
when(thresholdProvider.getThreshold("gold")).thenReturn(Money.fromGold(85));

// Verify
verify(auditLog).log(any(AuditEvent.class));
verify(auditLog, times(1)).log(argThat(e -> e.getType().equals("TAX")));

// Spy (partial mock)
TaxCalculator calculator = spy(new TaxCalculator());
doReturn(Money.fromGold(2.5)).when(calculator).calculate(any());
```

### C

**.NET Moq:**

```csharp
// Mock
var thresholdProvider = new Mock<IThresholdProvider>();
thresholdProvider.Setup(p => p.GetThreshold("gold")).Returns(Money.FromGold(85));

// Verify
auditLog.Verify(log => log.Log(It.IsAny<AuditEvent>()), Times.Once);

// Strict mock (fails on unexpected calls)
var strictMock = new Mock<IBankGateway>(MockBehavior.Strict);
strictMock.Setup(b => b.Transfer(It.IsAny<Account>(), It.IsAny<Money>()));
```

## Anti-Patterns and Pitfalls

### 1. Over-Mocking (Test Fragility)

**Problem**: Mocking too many collaborators couples tests to implementation details.

```typescript
// BAD: Over-mocked test
it("should process permitted certification", async () => {
  const validatorMock = jest.fn().mockResolvedValue(true);
  const loggerMock = jest.fn();
  const cacheMock = { get: jest.fn(), set: jest.fn() };
  const dbMock = { save: jest.fn() };
  const eventBusMock = { publish: jest.fn() };
  const notifierMock = { notify: jest.fn() };

  const service = new PermittedCertificationService(
    validatorMock,
    loggerMock,
    cacheMock,
    dbMock,
    eventBusMock,
    notifierMock,
  );

  await service.certify(application);

  // Test breaks whenever implementation changes
  expect(validatorMock).toHaveBeenCalled();
  expect(loggerMock).toHaveBeenCalledTimes(3);
  expect(cacheMock.set).toHaveBeenCalled();
  expect(dbMock.save).toHaveBeenCalled();
  expect(eventBusMock.publish).toHaveBeenCalled();
  expect(notifierMock.notify).toHaveBeenCalled();
});

// GOOD: Test behavior, not implementation
it("should approve certification for compliant product", async () => {
  const repository = new InMemoryRepository();
  const validator = new RealPermittedValidator(); // Real validator
  const service = new PermittedCertificationService(validator, repository);

  const result = await service.certify(compliantApplication);

  expect(result.status).toBe("APPROVED");

  const stored = await repository.findById(result.id);
  expect(stored.status).toBe("APPROVED");
});
```

**Fix**: Only mock external dependencies (I/O). Use real objects for domain logic.

### 2. Mocking Value Objects

**Problem**: Value objects are simple, immutable data structures. Mocking them adds no value.

```typescript
// BAD: Mocking value object
it("should calculate tax", () => {
  const moneyMock = jest.fn().mockReturnValue({
    amount: 100,
    multiply: jest.fn().mockReturnValue({ amount: 2.5 }),
  });

  const calculator = new TaxCalculator();
  const result = calculator.calculate(moneyMock);

  expect(result.amount).toBe(2.5);
});

// GOOD: Use real value objects
it("should calculate tax", () => {
  const wealth = Money.fromGold(100, "grams");
  const calculator = new TaxCalculator();

  const result = calculator.calculate(wealth);

  expect(result.equals(Money.fromGold(2.5, "grams"))).toBe(true);
});
```

**Fix**: Never mock value objects. They're cheap to construct and deterministic.

### 3. Mock Leakage Between Tests

**Problem**: Mocks persist between tests, causing false passes/failures.

```typescript
// BAD: Shared mock state
describe("TaxService", () => {
  const repositoryMock = { save: jest.fn() }; // Shared across tests

  it("should save calculation - test 1", async () => {
    const service = new TaxService(repositoryMock);
    await service.calculate(100);

    expect(repositoryMock.save).toHaveBeenCalledTimes(1);
  });

  it("should save calculation - test 2", async () => {
    const service = new TaxService(repositoryMock);
    await service.calculate(200);

    // FAILS: Called twice (once from previous test)
    expect(repositoryMock.save).toHaveBeenCalledTimes(1);
  });
});

// GOOD: Fresh mock per test
describe("TaxService", () => {
  let repositoryMock: any;

  beforeEach(() => {
    repositoryMock = { save: jest.fn() }; // Fresh mock each test
  });

  it("should save calculation - test 1", async () => {
    const service = new TaxService(repositoryMock);
    await service.calculate(100);

    expect(repositoryMock.save).toHaveBeenCalledTimes(1);
  });

  it("should save calculation - test 2", async () => {
    const service = new TaxService(repositoryMock);
    await service.calculate(200);

    expect(repositoryMock.save).toHaveBeenCalledTimes(1); // PASSES
  });
});
```

**Fix**: Reset mocks in `beforeEach` or use framework's automatic reset.

### 4. Testing the Mock Instead of the System

**Problem**: Tests verify mock behavior rather than system behavior.

```typescript
// BAD: Testing mock configuration
it("should call bank transfer", async () => {
  const bankMock = { transfer: jest.fn().mockResolvedValue({ success: true }) };
  const service = new TakafulClaimService(bankMock);

  await service.payClaim(claimId);

  // This only tests that we configured the mock correctly
  expect(bankMock.transfer).toHaveBeenCalled();
});

// GOOD: Test system outcome
it("should mark claim as paid after transfer", async () => {
  const bankStub = { transfer: async () => ({ success: true }) };
  const repository = new InMemoryClaimRepository();
  const service = new TakafulClaimService(bankStub, repository);

  await service.payClaim(claimId);

  const claim = await repository.findById(claimId);
  expect(claim.status).toBe("PAID");
  expect(claim.paidAt).toBeDefined();
});
```

**Fix**: Focus assertions on system state changes, not mock interactions.

## Best Practices

### 1. Prefer Fakes for Repositories

Use in-memory fakes for data access rather than mocks:

```typescript
// Repository interface
interface TaxRepository {
  save(record: TaxRecord): Promise<void>;
  findByUser(userId: string): Promise<TaxRecord[]>;
  findByDateRange(start: Date, end: Date): Promise<TaxRecord[]>;
}

// Fake implementation (reusable across all tests)
class InMemoryTaxRepository implements TaxRepository {
  private records: TaxRecord[] = [];

  async save(record: TaxRecord): Promise<void> {
    this.records.push(record);
  }

  async findByUser(userId: string): Promise<TaxRecord[]> {
    return this.records.filter((r) => r.userId === userId);
  }

  async findByDateRange(start: Date, end: Date): Promise<TaxRecord[]> {
    return this.records.filter((r) => r.calculatedAt >= start && r.calculatedAt <= end);
  }

  clear(): void {
    this.records = [];
  }
}

// Usage in tests
describe("TaxReportingService", () => {
  let repository: InMemoryTaxRepository;

  beforeEach(() => {
    repository = new InMemoryTaxRepository();
  });

  it("should generate annual report", async () => {
    const service = new TaxReportingService(repository);

    await repository.save(buildTaxRecord({ userId: "USER-001", amount: 100 }));
    await repository.save(buildTaxRecord({ userId: "USER-001", amount: 200 }));

    const report = await service.generateAnnualReport("USER-001", 2024);

    expect(report.totalTax).toBe(300);
  });
});
```

**Benefits:**

- Realistic behavior (supports complex queries)
- Reusable across all tests
- Fast (in-memory)
- No mock configuration needed

### 2. Use Stubs for External Services

Stub external dependencies with minimal logic:

```typescript
// Stub for Hijri calendar service
const hijriCalendarStub = {
  toHijri: (gregorianDate: Date) => ({
    year: 1445,
    month: 7,
    day: 15,
    monthName: "Rajab",
  }),
};

// Stub for currency exchange rate
const exchangeRateStub = {
  getRate: (from: string, to: string) => {
    if (from === "USD" && to === "SAR") return 3.75;
    if (from === "SAR" && to === "USD") return 0.27;
    throw new Error(`Unknown currency pair: ${from}/${to}`);
  },
};
```

### 3. Isolate Mock Setup in Helper Functions

Extract complex mock configuration into reusable helpers:

```typescript
// Helper function for common mock setup
function createMockBankGateway(overrides?: Partial<BankGateway>): BankGateway {
  return {
    transfer: jest.fn().mockResolvedValue({ success: true, id: "TXN-123" }),
    checkBalance: jest.fn().mockResolvedValue(Money.fromSAR(10000)),
    getTransactionHistory: jest.fn().mockResolvedValue([]),
    ...overrides,
  };
}

// Usage
it("should transfer funds", async () => {
  const bankGateway = createMockBankGateway({
    checkBalance: jest.fn().mockResolvedValue(Money.fromSAR(5000)),
  });

  const service = new TakafulClaimService(bankGateway);
  await service.payClaim(claimId);

  expect(bankGateway.transfer).toHaveBeenCalled();
});
```

### 4. Verify Behavior, Not Implementation

Focus on observable outcomes, not internal method calls:

```typescript
// BAD: Testing implementation details
it("should use cache when available", async () => {
  const cacheMock = { get: jest.fn(), set: jest.fn() };
  const service = new TaxService(cacheMock);

  await service.calculate(100);

  expect(cacheMock.get).toHaveBeenCalled(); // Implementation detail
  expect(cacheMock.set).toHaveBeenCalled();
});

// GOOD: Test observable behavior
it("should calculate tax faster on second call (cached)", async () => {
  const repository = new InMemoryRepository();
  const service = new TaxService(repository);

  const start1 = Date.now();
  await service.calculate(100);
  const duration1 = Date.now() - start1;

  const start2 = Date.now();
  await service.calculate(100);
  const duration2 = Date.now() - start2;

  expect(duration2).toBeLessThan(duration1 * 0.5); // Cached is faster
});
```

## Related Documentation

- **[04. Unit Testing Fundamentals](ex-soen-de-tedrdetd__04-unit-testing-fundamentals.md)** - Test structure and isolation principles
- **[06. Testing Patterns](ex-soen-de-tedrdetd__06-testing-patterns.md)** - AAA, Given-When-Then, table-driven tests
- **[09. Integration Testing](ex-soen-de-tedrdetd__09-integration-testing.md)** - When to use real dependencies vs test doubles
- **[18. Best Practices](ex-soen-de-tedrdetd__18-best-practices.md)** - TDD best practices
- **[19. Antipatterns](ex-soen-de-tedrdetd__19-anti-patterns.md)** - Common mistakes with test doubles

## Related Principles

- [Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)
- [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

## Summary

Test doubles enable fast, isolated unit tests by replacing real dependencies with controlled alternatives:

- **Dummy**: Unused parameter (satisfies type requirements)
- **Stub**: Returns canned data (no verification)
- **Spy**: Records calls for later verification
- **Mock**: Pre-programmed expectations (strict verification)
- **Fake**: Working implementation (simplified, in-memory)

**Key Principles:**

1. Only mock external dependencies (I/O, time, randomness)
2. Never mock value objects or domain entities
3. Prefer fakes for repositories (realistic behavior)
4. Prefer spies over mocks (less brittle)
5. Verify behavior, not implementation details
6. Reset mocks between tests

Mastering test doubles unlocks the full power of TDD—fast feedback, precise isolation, and confidence in refactoring.
