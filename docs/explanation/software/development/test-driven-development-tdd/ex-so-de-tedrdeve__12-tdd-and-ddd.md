# Test-Driven Development: TDD and Domain-Driven Design

## Overview

Domain-Driven Design (DDD) and Test-Driven Development are complementary practices. DDD provides tactical patterns—aggregates, value objects, entities, domain events, repositories—that encapsulate business logic. TDD ensures these domain models are correct, maintainable, and aligned with business requirements.

This document explores how to test DDD building blocks: aggregates (with invariants), value objects (with equality), entities (with identity), domain events, repositories (with persistence ignorance), domain services, and factories. Each pattern has specific testing considerations that emerge from its role in the domain model.

Mastering TDD with DDD results in domain models that are expressive, testable, and resilient to change. Tests become living documentation of business rules.

## Core Principles

TDD with DDD aligns with software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - DDD tactical patterns (aggregates, value objects, entities) make domain boundaries and business rules explicit. Tests verify these explicit contracts, ensuring invariants are enforced and domain logic is correct.
- **[Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)** - Value objects and domain entities encapsulate pure business logic without I/O side effects. This separation enables fast unit tests for domain rules while isolating persistence concerns to repositories.

## Testing Aggregates

### What Are Aggregates?

**Aggregate**: Cluster of domain objects (entities + value objects) with a root entity that enforces invariants.

**Invariants**: Business rules that must **always** hold true.

**Example**: `LoanContract` aggregate ensures:

1. Markup percentage never exceeds 10%
2. Contract cannot be activated without complete information
3. Installments sum equals asset price + markup

### Testing Aggregate Invariants

```typescript
// Aggregate Root
class LoanContract {
  private constructor(
    readonly id: ContractId,
    readonly customerId: CustomerId,
    readonly assetDescription: string,
    private _assetPrice: Money,
    private _markup: Money,
    private _status: ContractStatus,
    private _installments: Installment[],
  ) {
    this.validateInvariants();
  }

  static create(
    customerId: CustomerId,
    assetDescription: string,
    assetPrice: Money,
    markup: Money,
    termMonths: number,
  ): LoanContract {
    const contract = new LoanContract(
      ContractId.generate(),
      customerId,
      assetDescription,
      assetPrice,
      markup,
      "PENDING",
      [],
    );

    contract.generateInstallments(termMonths);
    return contract;
  }

  activate(): void {
    if (this._status !== "PENDING") {
      throw new Error("Contract must be in PENDING status to activate");
    }
    if (this._installments.length === 0) {
      throw new Error("Contract must have installments to activate");
    }

    this._status = "ACTIVE";
  }

  private validateInvariants(): void {
    // Invariant 1: Markup <= 10%
    const markupPercentage = this._markup.amount / this._assetPrice.amount;
    if (markupPercentage > 0.1) {
      throw new Error("Markup cannot exceed 10% of asset price");
    }

    // Invariant 2: Asset price > 0
    if (this._assetPrice.amount <= 0) {
      throw new Error("Asset price must be positive");
    }
  }

  private generateInstallments(termMonths: number): void {
    const totalAmount = this._assetPrice.add(this._markup);
    const monthlyAmount = totalAmount.divide(termMonths);

    this._installments = Array.from({ length: termMonths }, (_, i) => {
      const dueDate = new Date();
      dueDate.setMonth(dueDate.getMonth() + i + 1);
      return new Installment(InstallmentId.generate(), this.id, monthlyAmount, dueDate, "PENDING");
    });
  }

  get status(): ContractStatus {
    return this._status;
  }

  get installments(): readonly Installment[] {
    return this._installments;
  }
}

// GOOD: Test aggregate invariants
describe("LoanContract - Aggregate", () => {
  describe("Invariant: Markup <= 10%", () => {
    it("should create contract with valid markup", () => {
      const contract = LoanContract.create(
        CustomerId.generate(),
        "Toyota Camry",
        Money.usd(50000),
        Money.usd(5000), // 10% markup ✅
        12,
      );

      expect(contract).toBeDefined();
    });

    it("should reject markup > 10%", () => {
      expect(() =>
        LoanContract.create(
          CustomerId.generate(),
          "Toyota Camry",
          Money.usd(50000),
          Money.usd(5500), // 11% markup ❌
          12,
        ),
      ).toThrow("Markup cannot exceed 10% of asset price");
    });
  });

  describe("Invariant: Asset price > 0", () => {
    it("should reject zero asset price", () => {
      expect(() => LoanContract.create(CustomerId.generate(), "Toyota Camry", Money.usd(0), Money.usd(0), 12)).toThrow(
        "Asset price must be positive",
      );
    });
  });

  describe("Activation Rules", () => {
    it("should activate when in PENDING status with installments", () => {
      const contract = LoanContract.create(
        CustomerId.generate(),
        "Toyota Camry",
        Money.usd(50000),
        Money.usd(5000),
        12,
      );

      contract.activate();

      expect(contract.status).toBe("ACTIVE");
    });

    it("should reject activation when not PENDING", () => {
      const contract = LoanContract.create(
        CustomerId.generate(),
        "Toyota Camry",
        Money.usd(50000),
        Money.usd(5000),
        12,
      );
      contract.activate(); // Now ACTIVE

      expect(() => contract.activate()).toThrow("Contract must be in PENDING status to activate");
    });
  });

  describe("Installment Generation", () => {
    it("should generate correct number of installments", () => {
      const contract = LoanContract.create(
        CustomerId.generate(),
        "Toyota Camry",
        Money.usd(50000),
        Money.usd(5000),
        12,
      );

      expect(contract.installments).toHaveLength(12);
    });

    it("should distribute total amount across installments", () => {
      const contract = LoanContract.create(
        CustomerId.generate(),
        "Toyota Camry",
        Money.usd(60000),
        Money.usd(6000),
        12,
      );

      const totalInstallments = contract.installments.reduce((sum, inst) => sum.add(inst.amount), Money.usd(0));

      expect(totalInstallments).toEqualMoney(Money.usd(66000)); // 60k + 6k
    });
  });
});
```

### Testing Aggregate State Transitions

```typescript
describe("LoanContract - State Transitions", () => {
  it("should transition PENDING → ACTIVE → COMPLETED", () => {
    const contract = LoanContract.create(CustomerId.generate(), "Asset", Money.usd(10000), Money.usd(500), 12);

    expect(contract.status).toBe("PENDING");

    contract.activate();
    expect(contract.status).toBe("ACTIVE");

    contract.installments.forEach((inst) => contract.payInstallment(inst.id));
    expect(contract.status).toBe("COMPLETED");
  });

  it("should not allow invalid transitions", () => {
    const contract = LoanContract.create(CustomerId.generate(), "Asset", Money.usd(10000), Money.usd(500), 12);

    expect(() => contract.complete()).toThrow("Cannot complete contract in PENDING status");
  });
});
```

## Testing Value Objects

### Value Object Characteristics

**Value Object**: Defined by attributes, not identity. Two value objects with same attributes are equal.

**Characteristics:**

1. **Immutability**: Cannot be changed after creation
2. **Equality by value**: `Money(100, USD)` equals `Money(100, USD)`
3. **No identity**: No ID field
4. **Self-validation**: Constructor validates invariants

### Example: Money Value Object

```typescript
class Money {
  constructor(
    readonly amount: number,
    readonly currency: string,
  ) {
    if (amount < 0) {
      throw new Error("Money amount cannot be negative");
    }
    if (!currency || currency.length !== 3) {
      throw new Error("Currency must be 3-letter code");
    }
  }

  static usd(amount: number): Money {
    return new Money(amount, "USD");
  }

  add(other: Money): Money {
    if (this.currency !== other.currency) {
      throw new CurrencyMismatchError(this.currency, other.currency);
    }
    return new Money(this.amount + other.amount, this.currency);
  }

  multiply(multiplier: number): Money {
    if (multiplier < 0) {
      throw new Error("Cannot multiply by negative");
    }
    return new Money(this.amount * multiplier, this.currency);
  }

  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }
}

// GOOD: Test value object
describe("Money - Value Object", () => {
  describe("Creation and Validation", () => {
    it("should create valid money", () => {
      const money = new Money(100, "USD");

      expect(money.amount).toBe(100);
      expect(money.currency).toBe("USD");
    });

    it("should reject negative amount", () => {
      expect(() => new Money(-100, "USD")).toThrow("Money amount cannot be negative");
    });

    it("should reject invalid currency code", () => {
      expect(() => new Money(100, "US")).toThrow("Currency must be 3-letter code");
    });
  });

  describe("Equality", () => {
    it("should be equal when amount and currency match", () => {
      const money1 = new Money(100, "USD");
      const money2 = new Money(100, "USD");

      expect(money1.equals(money2)).toBe(true);
    });

    it("should not be equal when amounts differ", () => {
      const money1 = new Money(100, "USD");
      const money2 = new Money(50, "USD");

      expect(money1.equals(money2)).toBe(false);
    });

    it("should not be equal when currencies differ", () => {
      const money1 = new Money(100, "USD");
      const money2 = new Money(100, "EUR");

      expect(money1.equals(money2)).toBe(false);
    });
  });

  describe("Immutability", () => {
    it("should not mutate original when adding", () => {
      const original = new Money(100, "USD");
      const originalAmount = original.amount;

      const sum = original.add(new Money(50, "USD"));

      expect(original.amount).toBe(originalAmount); // Unchanged ✅
      expect(sum.amount).toBe(150); // New instance
    });
  });

  describe("Operations", () => {
    it("should add same currency", () => {
      const sum = Money.usd(100).add(Money.usd(50));

      expect(sum).toEqualMoney(Money.usd(150));
    });

    it("should throw on currency mismatch", () => {
      expect(() => Money.usd(100).add(new Money(50, "EUR"))).toThrow(CurrencyMismatchError);
    });
  });
});
```

### Example: HijriDate Value Object

```typescript
class HijriDate {
  constructor(
    readonly year: number,
    readonly month: number,
    readonly day: number,
  ) {
    if (month < 1 || month > 12) {
      throw new Error("Month must be between 1 and 12");
    }
    if (day < 1 || day > 30) {
      throw new Error("Day must be between 1 and 30");
    }
  }

  static of(year: number, month: number, day: number): HijriDate {
    return new HijriDate(year, month, day);
  }

  isAfter(other: HijriDate): boolean {
    if (this.year !== other.year) return this.year > other.year;
    if (this.month !== other.month) return this.month > other.month;
    return this.day > other.day;
  }

  equals(other: HijriDate): boolean {
    return this.year === other.year && this.month === other.month && this.day === other.day;
  }
}

describe("HijriDate - Value Object", () => {
  it("should validate month range", () => {
    expect(() => HijriDate.of(1445, 13, 1)).toThrow("Month must be between 1 and 12");
  });

  it("should compare dates correctly", () => {
    const earlier = HijriDate.of(1445, 1, 1);
    const later = HijriDate.of(1445, 6, 1);

    expect(later.isAfter(earlier)).toBe(true);
    expect(earlier.isAfter(later)).toBe(false);
  });

  it("should have value equality", () => {
    const date1 = HijriDate.of(1445, 1, 1);
    const date2 = HijriDate.of(1445, 1, 1);

    expect(date1.equals(date2)).toBe(true);
    expect(date1).not.toBe(date2); // Different references
  });
});
```

## Testing Domain Events

### What Are Domain Events?

**Domain Event**: Something that happened in the domain that domain experts care about.

**Examples**:

- `TaxCalculated`
- `LoanContractActivated`
- `TakafulClaimApproved`

### Testing Event Creation and Publishing

```typescript
// Domain Event
class TaxCalculated {
  readonly occurredAt: Date;

  constructor(
    readonly assessmentId: AssessmentId,
    readonly userId: UserId,
    readonly wealth: Money,
    readonly taxAmount: Money,
  ) {
    this.occurredAt = new Date();
  }

  toJSON() {
    return {
      eventType: "TaxCalculated",
      assessmentId: this.assessmentId.value,
      userId: this.userId.value,
      wealth: {
        amount: this.wealth.amount,
        currency: this.wealth.currency,
      },
      taxAmount: {
        amount: this.taxAmount.amount,
        currency: this.taxAmount.currency,
      },
      occurredAt: this.occurredAt.toISOString(),
    };
  }
}

// Aggregate emits events
class TaxAssessment {
  private _domainEvents: DomainEvent[] = [];

  calculate(): void {
    if (this.wealth.isLessThan(this.threshold)) {
      throw new Error("Wealth below threshold");
    }

    this._taxAmount = this.wealth.multiply(0.025);

    // Emit domain event
    this._domainEvents.push(new TaxCalculated(this.id, this.userId, this.wealth, this._taxAmount));
  }

  getDomainEvents(): readonly DomainEvent[] {
    return this._domainEvents;
  }

  clearDomainEvents(): void {
    this._domainEvents = [];
  }
}

// GOOD: Test event emission
describe("TaxAssessment - Domain Events", () => {
  it("should emit TaxCalculated event on calculation", () => {
    const assessment = new TaxAssessment(AssessmentId.generate(), UserId.generate(), Money.usd(1000), Money.usd(85));

    assessment.calculate();

    const events = assessment.getDomainEvents();
    expect(events).toHaveLength(1);
    expect(events[0]).toBeInstanceOf(TaxCalculated);
  });

  it("should include correct data in event", () => {
    const assessmentId = AssessmentId.generate();
    const userId = UserId.generate();
    const assessment = new TaxAssessment(assessmentId, userId, Money.usd(1000), Money.usd(85));

    assessment.calculate();

    const event = assessment.getDomainEvents()[0] as TaxCalculated;
    expect(event.assessmentId.equals(assessmentId)).toBe(true);
    expect(event.userId.equals(userId)).toBe(true);
    expect(event.taxAmount).toEqualMoney(Money.usd(25));
  });

  it("should not emit event when calculation fails", () => {
    const assessment = new TaxAssessment(
      AssessmentId.generate(),
      UserId.generate(),
      Money.usd(50), // Below threshold
      Money.usd(85),
    );

    expect(() => assessment.calculate()).toThrow("Wealth below threshold");

    const events = assessment.getDomainEvents();
    expect(events).toHaveLength(0); // No event emitted ✅
  });
});
```

### Testing Event Serialization

```typescript
describe("TaxCalculated - Serialization", () => {
  it("should serialize to JSON correctly", () => {
    const event = new TaxCalculated(
      AssessmentId.of("ASSESS-001"),
      UserId.of("USER-001"),
      Money.usd(1000),
      Money.usd(25),
    );

    const json = event.toJSON();

    expect(json).toMatchObject({
      eventType: "TaxCalculated",
      assessmentId: "ASSESS-001",
      userId: "USER-001",
      wealth: {
        amount: 1000,
        currency: "USD",
      },
      taxAmount: {
        amount: 25,
        currency: "USD",
      },
      occurredAt: expect.any(String),
    });
  });
});
```

## Testing Repositories

### Repository Responsibilities

**Repository**: Abstracts persistence, provides collection-like interface for aggregates.

**Key principle**: **Persistence ignorance**—domain model doesn't know about database.

### Testing Repository Interface

```typescript
// Repository interface (domain layer)
interface TaxAssessmentRepository {
  save(assessment: TaxAssessment): Promise<void>;
  findById(id: AssessmentId): Promise<TaxAssessment | undefined>;
  findByUserId(userId: UserId): Promise<TaxAssessment[]>;
}

// In-memory implementation (for unit tests)
class InMemoryTaxAssessmentRepository implements TaxAssessmentRepository {
  private assessments: Map<string, TaxAssessment> = new Map();

  async save(assessment: TaxAssessment): Promise<void> {
    this.assessments.set(assessment.id.value, assessment);
  }

  async findById(id: AssessmentId): Promise<TaxAssessment | undefined> {
    return this.assessments.get(id.value);
  }

  async findByUserId(userId: UserId): Promise<TaxAssessment[]> {
    return Array.from(this.assessments.values()).filter((a) => a.userId.equals(userId));
  }
}

// GOOD: Test repository behavior (unit test with in-memory)
describe("TaxAssessmentRepository - Unit", () => {
  let repository: TaxAssessmentRepository;

  beforeEach(() => {
    repository = new InMemoryTaxAssessmentRepository();
  });

  it("should save and retrieve assessment", async () => {
    const assessment = buildTaxAssessment();

    await repository.save(assessment);
    const retrieved = await repository.findById(assessment.id);

    expect(retrieved).toBeDefined();
    expect(retrieved!.id.equals(assessment.id)).toBe(true);
  });

  it("should return undefined when not found", async () => {
    const result = await repository.findById(AssessmentId.generate());

    expect(result).toBeUndefined();
  });

  it("should find assessments by user ID", async () => {
    const userId = UserId.generate();
    const assessment1 = buildTaxAssessment({ userId });
    const assessment2 = buildTaxAssessment({ userId });
    const otherAssessment = buildTaxAssessment(); // Different user

    await repository.save(assessment1);
    await repository.save(assessment2);
    await repository.save(otherAssessment);

    const userAssessments = await repository.findByUserId(userId);

    expect(userAssessments).toHaveLength(2);
    expect(userAssessments.every((a) => a.userId.equals(userId))).toBe(true);
  });
});

// Integration test with real database covered in ex-so-de-tedrdeve__09-integration-testing.md
```

## Testing Domain Services

### When to Use Domain Services

**Domain Service**: Business logic that doesn't naturally belong to an entity or value object.

**Use when:**

- Operation involves multiple aggregates
- Operation is a domain concept but not entity/value object
- Operation is stateless

### Example: Tax Distribution Service

```typescript
class TaxDistributionService {
  distribute(totalTax: Money, beneficiaries: Beneficiary[]): Distribution[] {
    this.validateBeneficiaries(beneficiaries);

    const totalWeight = beneficiaries.reduce((sum, b) => sum + b.weight, 0);

    return beneficiaries.map((beneficiary) => {
      const percentage = beneficiary.weight / totalWeight;
      const amount = totalTax.multiply(percentage);

      return new Distribution(beneficiary.id, beneficiary.name, amount, percentage);
    });
  }

  private validateBeneficiaries(beneficiaries: Beneficiary[]): void {
    if (beneficiaries.length === 0) {
      throw new Error("At least one beneficiary required");
    }

    const totalWeight = beneficiaries.reduce((sum, b) => sum + b.weight, 0);
    if (totalWeight <= 0) {
      throw new Error("Total weight must be positive");
    }
  }
}

// GOOD: Test domain service
describe("TaxDistributionService", () => {
  let service: TaxDistributionService;

  beforeEach(() => {
    service = new TaxDistributionService();
  });

  it("should distribute proportionally by weight", () => {
    const beneficiaries = [
      buildBeneficiary({ name: "Orphanage", weight: 0.5 }),
      buildBeneficiary({ name: "School", weight: 0.3 }),
      buildBeneficiary({ name: "Clinic", weight: 0.2 }),
    ];

    const distributions = service.distribute(Money.usd(1000), beneficiaries);

    expect(distributions).toHaveLength(3);
    expect(distributions[0].amount).toEqualMoney(Money.usd(500)); // 50%
    expect(distributions[1].amount).toEqualMoney(Money.usd(300)); // 30%
    expect(distributions[2].amount).toEqualMoney(Money.usd(200)); // 20%
  });

  it("should handle equal weights", () => {
    const beneficiaries = [
      buildBeneficiary({ weight: 1 }),
      buildBeneficiary({ weight: 1 }),
      buildBeneficiary({ weight: 1 }),
    ];

    const distributions = service.distribute(Money.usd(300), beneficiaries);

    distributions.forEach((dist) => {
      expect(dist.amount).toEqualMoney(Money.usd(100)); // Each gets 1/3
    });
  });

  it("should reject empty beneficiaries", () => {
    expect(() => service.distribute(Money.usd(1000), [])).toThrow("At least one beneficiary required");
  });

  it("should reject zero total weight", () => {
    const beneficiaries = [buildBeneficiary({ weight: 0 }), buildBeneficiary({ weight: 0 })];

    expect(() => service.distribute(Money.usd(1000), beneficiaries)).toThrow("Total weight must be positive");
  });
});
```

## Testing Factories

### Factory Pattern in DDD

**Factory**: Encapsulates complex aggregate creation logic.

```typescript
class LoanContractFactory {
  create(request: LoanApplicationRequest): LoanContract {
    this.validateRequest(request);

    const contract = LoanContract.create(
      request.customerId,
      request.assetDescription,
      request.assetPrice,
      this.calculateMarkup(request.assetPrice, request.termMonths),
      request.termMonths,
    );

    return contract;
  }

  private validateRequest(request: LoanApplicationRequest): void {
    if (request.termMonths < 1 || request.termMonths > 60) {
      throw new Error("Term must be between 1 and 60 months");
    }
  }

  private calculateMarkup(assetPrice: Money, termMonths: number): Money {
    const baseRate = 0.05; // 5%
    const timeAdjustment = (termMonths / 12) * 0.02; // +2% per year
    const totalRate = Math.min(baseRate + timeAdjustment, 0.1); // Max 10%

    return assetPrice.multiply(totalRate);
  }
}

describe("LoanContractFactory", () => {
  let factory: LoanContractFactory;

  beforeEach(() => {
    factory = new LoanContractFactory();
  });

  it("should create contract with calculated markup", () => {
    const request = buildLoanApplicationRequest({
      assetPrice: Money.usd(10000),
      termMonths: 12,
    });

    const contract = factory.create(request);

    // Markup for 12 months: 5% base + 2% time = 7%
    expect(contract.markup).toEqualMoney(Money.usd(700));
  });

  it("should cap markup at 10%", () => {
    const request = buildLoanApplicationRequest({
      assetPrice: Money.usd(10000),
      termMonths: 60, // 5 years
    });

    const contract = factory.create(request);

    // Would be 5% + (60/12 * 2%) = 15%, but capped at 10%
    expect(contract.markup).toEqualMoney(Money.usd(1000));
  });

  it("should reject invalid term", () => {
    const request = buildLoanApplicationRequest({
      termMonths: 61,
    });

    expect(() => factory.create(request)).toThrow("Term must be between 1 and 60 months");
  });
});
```

## Summary

TDD and DDD complement each other powerfully:

**Testing Aggregates:**

- Validate invariants in constructor/methods
- Test state transitions
- Verify business rules always hold
- Test that invalid operations throw errors

**Testing Value Objects:**

- Test validation in constructor
- Test equality by value (not reference)
- Verify immutability
- Test operations create new instances

**Testing Domain Events:**

- Verify events emitted on domain actions
- Test event data correctness
- Verify events not emitted on failures
- Test serialization for messaging

**Testing Repositories:**

- Use in-memory implementation for unit tests
- Test interface contract (save, find)
- Integration tests for real persistence (separate file)
- Maintain persistence ignorance in domain

**Testing Domain Services:**

- Test business logic involving multiple aggregates
- Verify coordination logic
- Test validation rules
- Keep services stateless

**Testing Factories:**

- Test complex creation logic
- Verify calculated values
- Test validation rules
- Ensure valid aggregates created

**Best Practices:**

1. Test behavior, not implementation
2. Use builders for complex domain objects
3. In-memory repositories for fast unit tests
4. Integration tests for real persistence
5. Domain events as test observability
6. Property-based testing for invariants

DDD tactical patterns create clear testing boundaries. Each pattern has specific testing concerns that align with its domain responsibility.

## Related Documentation

- **[07. Test Data Builders](./ex-so-de-tedrdeve__07-test-data-builders.md)** - Builders for aggregates and value objects
- **[09. Integration Testing](./ex-so-de-tedrdeve__09-integration-testing.md)** - Repository persistence testing
- **[11. TDD and Functional Programming](./ex-so-de-tedrdeve__11-tdd-and-functional-programming.md)** - Value objects as pure data
- **[14. Refactoring with Tests](./ex-so-de-tedrdeve__14-refactoring-with-tests.md)** - Refactoring domain models safely

## Related Principles

- [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)
- [Pure Functions Over Side Effects](../../../../../governance/principles/software-engineering/pure-functions.md)
