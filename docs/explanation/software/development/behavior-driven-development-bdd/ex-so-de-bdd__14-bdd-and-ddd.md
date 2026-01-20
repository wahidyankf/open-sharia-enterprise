# Behavior-Driven Development: BDD and DDD

## Overview

BDD (Behavior-Driven Development) and DDD (Domain-Driven Design) are highly complementary practices that strengthen each other when used together. DDD provides strategic design patterns (bounded contexts, ubiquitous language, domain model) that organize complex domains, while BDD provides tactical collaboration patterns (scenarios, examples, executable specifications) that verify domain behavior matches stakeholder expectations. Together, they create a powerful synergy: DDD shapes how you structure your system, BDD verifies each piece works correctly.

The connection between BDD and DDD centers on **ubiquitous language**—the shared vocabulary between domain experts and developers. DDD's ubiquitous language becomes BDD's Gherkin scenarios. When a Compliance scholar discusses "threshold threshold" or "Interest detection," these exact terms appear in both domain model code (DDD) and feature file scenarios (BDD). This linguistic alignment ensures everyone—business stakeholders, domain experts, developers, testers—speaks the same language, reducing miscommunication and ensuring implementation matches domain understanding.

For Islamic finance platforms, BDD+DDD integration is particularly powerful. DDD's bounded contexts (Tax Calculation, Permitted Certification, Loan Financing) map directly to BDD feature files. Each bounded context has its own ubiquitous language (Tax uses "threshold," "hawl," "taxable assets"; Loan uses "cost price," "profit markup," "Interest"), and BDD scenarios in each context use that context's terminology. Domain events (HawlCompleted, TaxCalculated, InterestDetected) become Then steps in scenarios. Aggregates (TaxAssessment, LoanContract) are created and tested through Given/When steps.

This document explores how BDD and DDD complement each other, practical integration patterns (features per bounded context, ubiquitous language in scenarios, testing aggregates and domain events), organizing BDD tests by domain architecture, and strategies for teams adopting both practices.

## Ubiquitous Language in BDD Scenarios

### DDD Ubiquitous Language Principle

**DDD Core Concept**: Use the same terminology everywhere—conversations, code, documentation, tests. If domain experts say "threshold," use `threshold` in code, not `minimumThreshold` or `taxThresholdAmount`.

**Benefits**:

- Eliminates translation errors (no mapping between business terms and technical terms)
- Enables domain experts to read code (or at least class names)
- Prevents linguistic drift (code vocabulary stays aligned with business)
- Reduces cognitive load (one vocabulary, not two)

### Ubiquitous Language in Gherkin

**BDD Natural Extension**: Gherkin scenarios use ubiquitous language, making them readable to domain experts.

**Example: Tax Calculation** (Islamic Finance Domain)

**Domain Expert Conversation** (Compliance Scholar):

> "Tax becomes obligatory when taxable wealth meets or exceeds the threshold threshold and has been owned for one complete hawl. The Tax rate is 2.5%, which represents one-fortieth of the wealth. Personal-use assets like one's primary residence are not taxable."

**DDD Domain Model** (Uses ubiquitous language):

```typescript
// domain/tax-calculation/tax-assessment.aggregate.ts
export class TaxAssessment {
  constructor(
    private readonly taxableWealth: TaxableWealth,
    private readonly thresholdThreshold: ThresholdThreshold,
    private readonly hawlPeriod: HawlPeriod,
  ) {}

  calculateTax(): TaxObligation {
    if (!this.meetsThreshold()) {
      return TaxObligation.notDue("Wealth below threshold threshold");
    }

    if (!this.hawlPeriod.isComplete()) {
      return TaxObligation.notDue("Hawl not complete");
    }

    const taxAmount = this.taxableWealth.amount * TaxRate.STANDARD; // 2.5%
    return TaxObligation.due(taxAmount);
  }

  private meetsThreshold(): boolean {
    return this.taxableWealth.amount >= this.thresholdThreshold.amount;
  }
}
```

**BDD Scenario** (Uses same ubiquitous language):

```gherkin
Feature: Tax Assessment

  Scenario: Tax obligatory when conditions met
    Given individual has taxable wealth of 10,000 USD
    And threshold threshold is 5,600 USD
    And hawl period of one lunar year is complete
    When Tax assessment is performed
    Then Tax should be obligatory
    And Tax amount should be 250 USD (2.5% of taxable wealth)

  Scenario: Personal residence excluded from taxable wealth
    Given individual owns:
      | Asset Type         | Value      | Taxable |
      | Primary Residence  | 300,000 USD| No        |
      | Investment Property| 100,000 USD| Yes       |
      | Cash Savings       | 20,000 USD | Yes       |
    When taxable wealth is calculated
    Then primary residence should be excluded
    And taxable wealth should be 120,000 USD
```

**Linguistic Alignment**:

- ✅ "taxable wealth" (not "taxable amount" or "wealth_to_tax")
- ✅ "threshold threshold" (not "minimum_wealth_limit")
- ✅ "hawl" (not "annual_period" or "one_year_duration")
- ✅ "Tax obligatory" (not "tax_required")

**Result**: Compliance scholar can read and validate scenarios without technical translation.

### Building Ubiquitous Language Through BDD

**BDD Process Enriches Ubiquitous Language**:

**Three Amigos Session** (Discovering terminology):

**Product Owner**: "Users need to know if they owe Tax."

**Developer**: "What triggers Tax?"

**Compliance Scholar** (Domain Expert): "Tax is obligatory when wealth meets threshold—the minimum threshold—and has been owned for hawl, which is one complete lunar year in the Hijri calendar."

**Developer**: "So we check if `wealth >= threshold` and `timeOwned >= oneYear`?"

**Compliance Scholar**: "Not quite. The threshold is specifically called threshold, and the time period is hawl. Also, the calendar is Hijri, not Gregorian, so one year is 354 days, not 365."

**Outcome**: Ubiquitous language captured

**Gherkin Scenario** (Documenting agreed language):

```gherkin
Scenario: Tax obligatory when wealth meets threshold for hawl
  Given individual owns 100 grams of gold
  And threshold threshold for gold is 85 grams
  And hawl period of 354 days has passed
  When Tax obligation is evaluated
  Then Tax should be obligatory
```

**Domain Model** (Implementing with ubiquitous language):

```typescript
export class ThresholdThreshold {
  constructor(
    public readonly amount: number,
    public readonly unit: string,
    public readonly assetType: AssetType,
  ) {}
}

export class HawlPeriod {
  private readonly HIJRI_YEAR_DAYS = 354;

  constructor(
    private readonly acquisitionDate: Date,
    private readonly currentDate: Date,
  ) {}

  isComplete(): boolean {
    const daysDifference = this.calculateDays(this.acquisitionDate, this.currentDate);
    return daysDifference >= this.HIJRI_YEAR_DAYS;
  }
}
```

**Result**: Term "threshold" and "hawl" used consistently across conversations, scenarios, and code.

## Features per Bounded Context

### DDD Bounded Contexts

**DDD Strategic Pattern**: Divide large domains into bounded contexts—cohesive subdomains with clear boundaries, each with its own ubiquitous language and model.

**Islamic Finance Platform Contexts**:

```
┌─────────────────────────────────────────────────────────┐
│                  Islamic Finance Platform                │
└─────────────────────────────────────────────────────────┘
           │
           ├─── Tax Calculation Context
           │    └─ Language: threshold, hawl, taxable assets, Tax rate
           │
           ├─── Permitted Certification Context
           │    └─ Language: permitted, forbidden, mashbooh, certification authority
           │
           ├─── Loan Financing Context
           │    └─ Language: cost price, profit markup, Interest, Loan contract
           │
           └─── Accounting Context
                └─ Language: journal entry, account, transaction, balance
```

### BDD Features Aligned with Bounded Contexts

**Pattern**: One feature file = One capability within one bounded context

**Directory Structure**:

```
features/
├── tax-calculation/           # Bounded Context: Tax Calculation
│   ├── README.md               # Context overview, ubiquitous language glossary
│   ├── gold-tax.feature
│   ├── silver-tax.feature
│   ├── cash-tax.feature
│   ├── agricultural-tax.feature
│   └── mixed-assets-tax.feature
│
├── permitted-certification/         # Bounded Context: Permitted Certification
│   ├── README.md
│   ├── ingredient-verification.feature
│   ├── certification-authority-validation.feature
│   ├── supply-chain-traceability.feature
│   └── certificate-lifecycle.feature
│
├── loan-financing/          # Bounded Context: Loan Financing
│   ├── README.md
│   ├── contract-creation.feature
│   ├── profit-calculation.feature
│   ├── interest-detection.feature
│   └── payment-schedule.feature
│
└── accounting/                  # Bounded Context: Accounting
    ├── README.md
    ├── transaction-recording.feature
    ├── journal-entry-creation.feature
    └── financial-reporting.feature
```

**Context README Example** (`tax-calculation/README.md`):

```markdown
# Tax Calculation Bounded Context

## Ubiquitous Language

- **Tax**: Mandatory charitable contribution (one of Five Pillars of Islam)
- **Threshold**: Minimum wealth threshold triggering Tax obligation (85g gold / 595g silver)
- **Hawl**: One complete lunar year (354 days in Hijri calendar)
- **Taxable Wealth**: Assets subject to Tax (excludes personal-use items)
- **Tax Rate**: 2.5% (one-fortieth) for most wealth types

## Business Rules

1. Tax obligatory when: `taxable_wealth >= threshold AND hawl_complete`
2. Personal residence excluded from taxable wealth
3. Different rates for agricultural produce (5% irrigated, 10% rain-fed)

## Features

- Gold Tax (`gold-tax.feature`)
- Silver Tax (`silver-tax.feature`)
- Cash Tax (`cash-tax.feature`)
```

**Benefits**:

- **Clear Ownership**: Each bounded context owned by specific team
- **Context Isolation**: Tax scenarios don't mix with Loan scenarios
- **Language Isolation**: Each context uses its own ubiquitous language
- **Scalability**: Add contexts without affecting others

### Cross-Context Scenarios

**When Features Span Contexts** (Integration scenarios):

**Scenario**: Tax calculation requires accounting transaction recording

```gherkin
# features/integration/tax-accounting-integration.feature
Feature: Tax Payment Recording in Accounting System

  # This scenario spans two bounded contexts:
  # - Tax Calculation (calculates amount)
  # - Accounting (records transaction)

  @integration @cross-context
  Scenario: Record Tax payment as accounting transaction
    # Tax Calculation Context
    Given individual has taxable wealth of 10,000 USD
    When Tax is calculated
    Then Tax amount is 250 USD

    # Accounting Context
    When Tax payment is processed
    Then journal entry should be created:
      | Account           | Debit  | Credit |
      | Tax Expense     | 250    |        |
      | Cash              |        | 250    |
    And transaction should be recorded with category "Tax"
```

**Pattern**: Integration scenarios explicitly span contexts, clearly marked with `@integration @cross-context` tags.

## Testing Domain Aggregates with BDD

### DDD Aggregates

**DDD Tactical Pattern**: Aggregate is a cluster of domain objects treated as a single unit for data changes. One entity is the Aggregate Root (controls access to other entities in the cluster).

**Example: TaxAssessment Aggregate**

```typescript
// domain/tax-calculation/tax-assessment.aggregate.ts
export class TaxAssessment {
  constructor(
    private readonly assessmentId: AssessmentId,
    private wealth: WealthPortfolio, // Entity
    private threshold: ThresholdThreshold, // Value Object
    private hawlPeriod: HawlPeriod, // Value Object
    private taxObligation?: TaxObligation, // Value Object
  ) {}

  // Aggregate root method (business rule enforcement)
  calculateTax(): void {
    if (!this.meetsThreshold()) {
      this.taxObligation = TaxObligation.notDue("Below threshold");
      return;
    }

    if (!this.hawlPeriod.isComplete()) {
      this.taxObligation = TaxObligation.notDue("Hawl incomplete");
      return;
    }

    const amount = this.wealth.calculateTaxableAmount() * TaxRate.STANDARD;
    this.taxObligation = TaxObligation.due(amount);

    // Domain event
    this.addDomainEvent(new TaxCalculated(this.assessmentId, amount));
  }

  private meetsThreshold(): boolean {
    return this.wealth.totalValue() >= this.threshold.amount;
  }
}
```

### BDD Scenarios for Aggregates

**Pattern**: Test aggregate behavior through BDD scenarios

```gherkin
Feature: Tax Assessment Aggregate

  # Test aggregate root business rules
  @aggregate @tax-assessment
  Scenario: Calculate Tax through aggregate
    Given TaxAssessment aggregate with:
      | Assessment ID | Individual      | Wealth    | Threshold     |
      | ZA-001        | alice@example   | 10,000 USD| 5,600 USD |
    And hawl period is complete
    When calculateTax method is invoked
    Then TaxObligation should be "due"
    And Tax amount should be 250 USD
    And TaxCalculated domain event should be raised

  # Test aggregate invariant enforcement
  @aggregate @invariant
  Scenario: Aggregate enforces threshold invariant
    Given TaxAssessment aggregate with wealth below threshold
    When calculateTax method is invoked
    Then TaxObligation should be "not due"
    And reason should be "Below threshold"
    And no domain events should be raised
```

**Step Definitions**:

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { TaxAssessment } from "../domain/tax-assessment.aggregate";
import { WealthPortfolio } from "../domain/wealth-portfolio";

const feature = loadFeature("./features/tax-assessment-aggregate.feature");

defineFeature(feature, (test) => {
  let aggregate: TaxAssessment;

  test("Calculate Tax through aggregate", ({ given, and, when, then }) => {
    given(/TaxAssessment aggregate with:/, (table) => {
      const row = table[0];
      aggregate = new TaxAssessment(
        new AssessmentId(row["Assessment ID"]),
        new WealthPortfolio([{ amount: 10000, currency: "USD" }]),
        new ThresholdThreshold(5600, "USD"),
        HawlPeriod.complete(),
      );
    });

    and("hawl period is complete", () => {
      // Already set up in aggregate construction
    });

    when("calculateTax method is invoked", () => {
      aggregate.calculateTax();
    });

    then(/TaxObligation should be "(.+)"/, (status: string) => {
      expect(aggregate.taxObligation?.status).toBe(status);
    });

    and("Tax amount should be {int} USD", (expectedAmount: number) => {
      expect(aggregate.taxObligation?.amount.value).toBe(expectedAmount);
    });

    and("TaxCalculated domain event should be raised", () => {
      const events = aggregate.getDomainEvents();
      const taxCalculatedEvent = events.find((e) => e instanceof TaxCalculated);
      expect(taxCalculatedEvent).toBeDefined();
    });
  });
});
```

## Testing Domain Events with BDD

### DDD Domain Events

**DDD Pattern**: Domain events represent something significant that happened in the domain. Events capture state changes and enable decoupling between bounded contexts.

**Example Domain Events** (Islamic Finance):

```typescript
// domain/tax-calculation/events/tax-calculated.event.ts
export class TaxCalculated extends DomainEvent {
  constructor(
    public readonly assessmentId: AssessmentId,
    public readonly amount: Money,
    public readonly dueDate: Date,
    public readonly occurredAt: Date = new Date(),
  ) {
    super();
  }
}

// domain/tax-calculation/events/hawl-completed.event.ts
export class HawlCompleted extends DomainEvent {
  constructor(
    public readonly individualId: IndividualId,
    public readonly completionDate: Date,
    public readonly occurredAt: Date = new Date(),
  ) {
    super();
  }
}

// domain/loan-financing/events/interest-detected.event.ts
export class InterestDetected extends DomainEvent {
  constructor(
    public readonly contractId: ContractId,
    public readonly interestType: InterestType,
    public readonly severity: Severity,
    public readonly occurredAt: Date = new Date(),
  ) {
    super();
  }
}
```

### BDD Scenarios for Domain Events

**Pattern**: Use Then steps to verify domain events are raised

```gherkin
Feature: Tax Calculation Domain Events

  @domain-events @tax-calculated
  Scenario: TaxCalculated event raised when Tax calculated
    Given individual has taxable wealth of 10,000 USD
    And threshold threshold is 5,600 USD
    And hawl is complete
    When Tax assessment is performed
    Then TaxCalculated event should be published
    And event should contain:
      | Field      | Value    |
      | amount     | 250 USD  |
      | dueDate    | today    |
    And event should be persisted to event store

  @domain-events @hawl-completed
  Scenario: HawlCompleted event raised when hawl period ends
    Given individual acquired wealth on 2025-01-20
    And current date is 2026-01-20 (354 days later)
    When hawl period is evaluated
    Then HawlCompleted event should be published
    And event should trigger Tax calculation workflow

  @domain-events @interest-detected @compliance
  Scenario: InterestDetected event raised for compliance violation
    Given Loan contract with time-based interest
    When Compliance compliance check is performed
    Then InterestDetected event should be published
    And event severity should be "CRITICAL"
    And event should trigger compliance officer notification
```

**Step Definitions** (Event verification):

```typescript
import { DomainEventBus } from "../infrastructure/domain-event-bus";

let eventBus: DomainEventBus;
let publishedEvents: DomainEvent[];

beforeEach(() => {
  publishedEvents = [];
  eventBus = new DomainEventBus();
  eventBus.subscribe((event) => {
    publishedEvents.push(event);
  });
});

then("TaxCalculated event should be published", () => {
  const taxCalculatedEvent = publishedEvents.find((e) => e instanceof TaxCalculated);
  expect(taxCalculatedEvent).toBeDefined();
});

then("event should contain:", (table) => {
  const taxEvent = publishedEvents.find((e) => e instanceof TaxCalculated) as TaxCalculated;

  table.forEach((row) => {
    const field = row["Field"];
    const expectedValue = row["Value"];

    if (field === "amount") {
      expect(taxEvent.amount.toString()).toBe(expectedValue);
    } else if (field === "dueDate") {
      if (expectedValue === "today") {
        expect(taxEvent.dueDate.toDateString()).toBe(new Date().toDateString());
      }
    }
  });
});

then("event should trigger Tax calculation workflow", async () => {
  // Verify event handler invoked
  const workflowTriggered = await workflowOrchestrator.wasTriggered("TaxCalculation");
  expect(workflowTriggered).toBe(true);
});
```

## Testing Value Objects with BDD

### DDD Value Objects

**DDD Pattern**: Value objects are immutable objects defined by their attributes (no identity). Examples: Money, ThresholdThreshold, HawlPeriod.

**Example Value Objects**:

```typescript
// domain/shared/money.value-object.ts
export class Money {
  constructor(
    public readonly amount: number,
    public readonly currency: Currency,
  ) {
    if (amount < 0) {
      throw new InvalidMoneyError("Amount cannot be negative");
    }
  }

  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }

  add(other: Money): Money {
    if (this.currency !== other.currency) {
      throw new CurrencyMismatchError();
    }
    return new Money(this.amount + other.amount, this.currency);
  }
}

// domain/tax-calculation/threshold-threshold.value-object.ts
export class ThresholdThreshold {
  constructor(
    public readonly amount: number,
    public readonly unit: string,
    public readonly assetType: AssetType,
  ) {
    if (amount <= 0) {
      throw new InvalidThresholdError("Threshold must be positive");
    }
  }

  meetsThreshold(wealth: Wealth): boolean {
    return wealth.amount >= this.amount && wealth.unit === this.unit;
  }
}
```

### BDD Scenarios for Value Objects

**Pattern**: Test value object behavior and invariants

```gherkin
Feature: Money Value Object

  @value-object @money
  Scenario: Create valid money value
    Given amount 100 and currency "USD"
    When Money value object is created
    Then Money should have amount 100
    And Money should have currency "USD"

  @value-object @money @invariant
  Scenario: Reject negative money amount
    Given amount -50 and currency "USD"
    When Money value object is created
    Then InvalidMoneyError should be thrown
    And error message should be "Amount cannot be negative"

  @value-object @money @equality
  Scenario: Money equality based on amount and currency
    Given Money A: 100 USD
    And Money B: 100 USD
    When equality is checked
    Then Money A should equal Money B

  @value-object @money @operations
  Scenario: Add money with same currency
    Given Money A: 100 USD
    And Money B: 50 USD
    When Money A is added to Money B
    Then result should be 150 USD

  @value-object @money @operations @error
  Scenario: Cannot add money with different currencies
    Given Money A: 100 USD
    And Money B: 50 EUR
    When Money A is added to Money B
    Then CurrencyMismatchError should be thrown
```

**Step Definitions**:

```typescript
test("Add money with same currency", ({ given, and, when, then }) => {
  let moneyA: Money;
  let moneyB: Money;
  let result: Money;

  given("Money A: {int} {word}", (amount: number, currency: string) => {
    moneyA = new Money(amount, currency as Currency);
  });

  and("Money B: {int} {word}", (amount: number, currency: string) => {
    moneyB = new Money(amount, currency as Currency);
  });

  when("Money A is added to Money B", () => {
    result = moneyA.add(moneyB);
  });

  then("result should be {int} {word}", (amount: number, currency: string) => {
    expect(result.amount).toBe(amount);
    expect(result.currency).toBe(currency);
  });
});
```

## Organizing BDD Tests by Domain Architecture

### Layered Architecture with BDD

**DDD Layers**:

```
┌─────────────────────────────────────────┐
│ Presentation Layer (API, UI)            │  ← E2E BDD Tests
└─────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────┐
│ Application Layer (Use Cases)           │  ← Integration BDD Tests
└─────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────┐
│ Domain Layer (Aggregates, Entities,     │  ← Unit-Level BDD Tests
│              Value Objects, Events)      │
└─────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────┐
│ Infrastructure Layer (Database, APIs)   │  ← (Mocked in BDD tests)
└─────────────────────────────────────────┘
```

**BDD Test Organization**:

```
features/
├── tax-calculation/
│   ├── domain/                          # Unit-level BDD (domain layer)
│   │   ├── tax-assessment.feature    # Aggregate behavior
│   │   ├── money.feature                # Value object behavior
│   │   └── domain-events.feature        # Event raising
│   │
│   ├── application/                     # Integration BDD (use cases)
│   │   ├── calculate-tax-use-case.feature
│   │   └── submit-tax-payment-use-case.feature
│   │
│   └── e2e/                             # E2E BDD (full stack)
│       └── tax-self-assessment-journey.feature
```

**Domain Layer BDD** (Fast, isolated):

```gherkin
# features/tax-calculation/domain/tax-assessment.feature
@unit @domain
Feature: TaxAssessment Aggregate

  Scenario: Calculate Tax on aggregate
    Given TaxAssessment with wealth 10,000 USD and threshold 5,600 USD
    When calculateTax is called
    Then Tax should be due
    And amount should be 250 USD
```

**Application Layer BDD** (Integration, use case orchestration):

```gherkin
# features/tax-calculation/application/calculate-tax-use-case.feature
@integration @use-case
Feature: Calculate Tax Use Case

  Scenario: Execute Tax calculation use case
    Given individual "alice@example.com" exists in database
    And individual has wealth 10,000 USD
    When CalculateTaxUseCase is executed for individual
    Then Tax calculation should be saved to database
    And TaxCalculated event should be published
    And individual should be notified via email
```

**E2E Layer BDD** (Full journey):

```gherkin
# features/tax-calculation/e2e/tax-self-assessment-journey.feature
@e2e @journey
Feature: Tax Self-Assessment Journey

  Scenario: User completes Tax self-assessment
    Given user navigates to /tax/assessment
    When user enters wealth details
    And clicks "Calculate Tax"
    Then user sees "Tax Due: 250 USD"
    And user receives email confirmation
```

## Islamic Finance Examples

### Example 1: Tax Calculation Bounded Context

**Ubiquitous Language in Scenarios**:

```gherkin
# features/tax-calculation/gold-tax.feature
Feature: Tax Calculation for Gold Wealth

  # Ubiquitous language: threshold, hawl, taxable, Tax rate

  Background:
    Given threshold threshold for gold is 85 grams
    And Tax rate for gold is 2.5%

  Scenario: Tax obligatory when gold meets threshold for hawl
    Given individual owns 100 grams of gold
    And gold has been owned for one complete hawl (354 days)
    When Tax obligation is evaluated
    Then Tax should be obligatory
    And Tax amount should be 2.5 grams of gold

  Scenario: Tax not obligatory when gold below threshold
    Given individual owns 50 grams of gold (below threshold)
    When Tax obligation is evaluated
    Then Tax should not be obligatory
    And individual should be informed "Wealth below threshold threshold"
```

**Domain Model Uses Same Language**:

```typescript
export class GoldTaxCalculator {
  private readonly THRESHOLD_GOLD_GRAMS = 85;
  private readonly TAX_RATE = 0.025;
  private readonly HAWL_DAYS = 354;

  calculateTax(goldWealth: GoldWealth, hawlPeriod: HawlPeriod): TaxObligation {
    if (!this.meetsThreshold(goldWealth)) {
      return TaxObligation.notDue("Wealth below threshold threshold");
    }

    if (!hawlPeriod.isComplete(this.HAWL_DAYS)) {
      return TaxObligation.notDue("Hawl not complete");
    }

    const taxAmount = goldWealth.amount * this.TAX_RATE;
    return TaxObligation.due(taxAmount, "grams");
  }

  private meetsThreshold(goldWealth: GoldWealth): boolean {
    return goldWealth.amount >= this.THRESHOLD_GOLD_GRAMS;
  }
}
```

### Example 2: Permitted Certification with Domain Events

**Feature with Domain Events**:

```gherkin
# features/permitted-certification/certification-lifecycle.feature
@permitted @domain-events
Feature: Permitted Certification Lifecycle

  Scenario: Issue permitted certification
    Given product "Organic Dates" passed all verifications
    And certification authority is "JAKIM"
    When permitted certificate is issued
    Then PermittedCertificateIssued event should be published
    And event should contain:
      | Field              | Value                |
      | product            | Organic Dates        |
      | certificationBody  | JAKIM                |
      | expiryDate         | 12 months from now   |
    And certificate PDF should be generated
    And supplier should be notified

  Scenario: Revoke expired certification
    Given product "Snack Mix" has expired certification (expired 30 days ago)
    When certification expiry check is performed
    Then PermittedCertificateRevoked event should be published
    And product status should be updated to "Certification Expired"
    And supplier should be alerted
```

**Domain Event Handler**:

```typescript
@EventHandler(PermittedCertificateIssued)
export class SendCertificateNotificationHandler {
  constructor(
    private readonly emailService: EmailService,
    private readonly pdfGenerator: PdfGenerator,
  ) {}

  async handle(event: PermittedCertificateIssued): Promise<void> {
    const certificatePdf = await this.pdfGenerator.generate(event.certificateId);

    await this.emailService.send({
      to: event.supplierEmail,
      subject: "Permitted Certificate Issued",
      body: `Your product "${event.productName}" has been certified permitted by ${event.certificationBody}.`,
      attachments: [certificatePdf],
    });
  }
}
```

### Example 3: Loan Financing Aggregate

**Testing Aggregate Invariants**:

```gherkin
# features/loan-financing/loan-contract-aggregate.feature
@aggregate @loan
Feature: Loan Contract Aggregate

  Scenario: Create valid Loan contract
    Given bank purchases asset for 100,000 USD (cost price)
    And bank discloses cost price to customer
    And profit markup is 15,000 USD (15%)
    When LoanContract aggregate is created
    Then contract should be valid
    And selling price should be 115,000 USD
    And ContractCreated event should be raised

  @invariant @interest
  Scenario: Aggregate rejects time-based interest (invariant violation)
    Given bank attempts to create contract with interest rate 5%
    When LoanContract aggregate is created
    Then InvalidContractError should be thrown
    And error should be "Interest prohibited: Time-based interest not allowed in Loan"
    And no domain events should be raised

  @invariant @disclosure
  Scenario: Aggregate enforces cost disclosure (invariant)
    Given bank purchases asset for 100,000 USD
    And cost price NOT disclosed to customer
    When LoanContract aggregate is created
    Then InvalidContractError should be thrown
    And error should be "Compliance requirement: Cost price must be disclosed to customer"
```

**Aggregate with Invariants**:

```typescript
export class LoanContract {
  constructor(
    private readonly contractId: ContractId,
    private readonly costPrice: Money,
    private readonly profitMarkup: Money,
    private readonly costDisclosedToCustomer: boolean,
  ) {
    this.validateInvariants();
  }

  private validateInvariants(): void {
    // Invariant: Cost must be disclosed (Compliance requirement)
    if (!this.costDisclosedToCustomer) {
      throw new InvalidContractError("Compliance requirement: Cost price must be disclosed to customer");
    }

    // Invariant: Profit must be fixed markup, not interest
    if (this.profitMarkup.isTimeBasedInterest()) {
      throw new InvalidContractError("Interest prohibited: Time-based interest not allowed in Loan");
    }
  }

  getSellingPrice(): Money {
    return this.costPrice.add(this.profitMarkup);
  }
}
```

## Summary

BDD and DDD are highly complementary practices that strengthen each other through shared ubiquitous language and domain-aligned testing.

**Ubiquitous Language**:

- **DDD Provides**: Shared vocabulary between domain experts and developers
- **BDD Uses**: Same vocabulary in Gherkin scenarios (threshold, hawl, Interest, permitted)
- **Result**: Domain experts can read and validate scenarios without translation

**Features per Bounded Context**:

- **DDD Provides**: Strategic boundaries dividing complex domains
- **BDD Organizes**: Feature files per bounded context (Tax, Permitted, Loan)
- **Result**: Clear ownership, context isolation, scalable organization

**Testing Domain Patterns**:

- **Aggregates**: BDD scenarios verify aggregate behavior and invariants
- **Domain Events**: Then steps verify events raised (TaxCalculated, InterestDetected)
- **Value Objects**: BDD tests verify immutability, equality, operations
- **Use Cases**: Integration BDD tests verify application layer orchestration

**Architectural Alignment**:

- **Domain Layer**: Unit-level BDD tests (fast, isolated)
- **Application Layer**: Integration BDD tests (use case orchestration)
- **Presentation Layer**: E2E BDD tests (full user journeys)

**Islamic Finance Benefits**:

- Compliance scholars validate BDD scenarios using domain language
- Bounded contexts mirror Islamic finance domains (Tax, Permitted, Loan)
- Domain events track compliance (InterestDetected, CertificationRevoked)
- Aggregates enforce Compliance invariants (cost disclosure, Interest prohibition)

Use BDD and DDD together to create domain-aligned, stakeholder-verified, well-tested software that accurately models complex business domains while remaining accessible to domain experts.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: BDD, DDD, Domain-Driven Design, Ubiquitous Language, Bounded Context, Aggregates, Domain Events, Value Objects, Islamic Finance, Tax, Permitted, Loan
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [DDD Introduction](../architecture/domain-driven-design-ddd/README.md) - Domain-Driven Design overview
  - [08. Feature Files and Organization](./ex-so-de-bdd__08-feature-files-and-organization.md) - Organizing by bounded context
  - [13. BDD and TDD](./ex-so-de-bdd__13-bdd-and-tdd.md) - TDD integration
- **Prerequisites**: Understanding of DDD concepts (bounded contexts, aggregates, domain events), BDD scenarios (Gherkin)
- **Next Steps**: Read [BDD in Nx Monorepo](./ex-so-de-bdd__15-bdd-in-nx-monorepo.md) for practical implementation patterns
- **Last Updated**: 2026-01-20
- **Status**: Active
