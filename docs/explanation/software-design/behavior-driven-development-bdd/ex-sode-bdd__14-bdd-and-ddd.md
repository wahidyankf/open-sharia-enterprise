# Behavior-Driven Development: BDD and DDD

## Overview

BDD (Behavior-Driven Development) and DDD (Domain-Driven Design) are highly complementary practices that strengthen each other when used together. DDD provides strategic design patterns (bounded contexts, ubiquitous language, domain model) that organize complex domains, while BDD provides tactical collaboration patterns (scenarios, examples, executable specifications) that verify domain behavior matches stakeholder expectations. Together, they create a powerful synergy: DDD shapes how you structure your system, BDD verifies each piece works correctly.

The connection between BDD and DDD centers on **ubiquitous language**—the shared vocabulary between domain experts and developers. DDD's ubiquitous language becomes BDD's Gherkin scenarios. When a Shariah scholar discusses "nisab threshold" or "Riba detection," these exact terms appear in both domain model code (DDD) and feature file scenarios (BDD). This linguistic alignment ensures everyone—business stakeholders, domain experts, developers, testers—speaks the same language, reducing miscommunication and ensuring implementation matches domain understanding.

For Islamic finance platforms, BDD+DDD integration is particularly powerful. DDD's bounded contexts (Zakat Calculation, Halal Certification, Murabaha Financing) map directly to BDD feature files. Each bounded context has its own ubiquitous language (Zakat uses "nisab," "hawl," "zakatable assets"; Murabaha uses "cost price," "profit markup," "Riba"), and BDD scenarios in each context use that context's terminology. Domain events (HawlCompleted, ZakatCalculated, RibaDetected) become Then steps in scenarios. Aggregates (ZakatAssessment, MurabahaContract) are created and tested through Given/When steps.

This document explores how BDD and DDD complement each other, practical integration patterns (features per bounded context, ubiquitous language in scenarios, testing aggregates and domain events), organizing BDD tests by domain architecture, and strategies for teams adopting both practices.

## Ubiquitous Language in BDD Scenarios

### DDD Ubiquitous Language Principle

**DDD Core Concept**: Use the same terminology everywhere—conversations, code, documentation, tests. If domain experts say "nisab," use `nisab` in code, not `minimumThreshold` or `zakatThresholdAmount`.

**Benefits**:

- Eliminates translation errors (no mapping between business terms and technical terms)
- Enables domain experts to read code (or at least class names)
- Prevents linguistic drift (code vocabulary stays aligned with business)
- Reduces cognitive load (one vocabulary, not two)

### Ubiquitous Language in Gherkin

**BDD Natural Extension**: Gherkin scenarios use ubiquitous language, making them readable to domain experts.

**Example: Zakat Calculation** (Islamic Finance Domain)

**Domain Expert Conversation** (Shariah Scholar):

> "Zakat becomes obligatory when zakatable wealth meets or exceeds the nisab threshold and has been owned for one complete hawl. The Zakat rate is 2.5%, which represents one-fortieth of the wealth. Personal-use assets like one's primary residence are not zakatable."

**DDD Domain Model** (Uses ubiquitous language):

```typescript
// domain/zakat-calculation/zakat-assessment.aggregate.ts
export class ZakatAssessment {
  constructor(
    private readonly zakatableWealth: ZakatableWealth,
    private readonly nisabThreshold: NisabThreshold,
    private readonly hawlPeriod: HawlPeriod,
  ) {}

  calculateZakat(): ZakatObligation {
    if (!this.meetsNisab()) {
      return ZakatObligation.notDue("Wealth below nisab threshold");
    }

    if (!this.hawlPeriod.isComplete()) {
      return ZakatObligation.notDue("Hawl not complete");
    }

    const zakatAmount = this.zakatableWealth.amount * ZakatRate.STANDARD; // 2.5%
    return ZakatObligation.due(zakatAmount);
  }

  private meetsNisab(): boolean {
    return this.zakatableWealth.amount >= this.nisabThreshold.amount;
  }
}
```

**BDD Scenario** (Uses same ubiquitous language):

```gherkin
Feature: Zakat Assessment

  Scenario: Zakat obligatory when conditions met
    Given individual has zakatable wealth of 10,000 USD
    And nisab threshold is 5,600 USD
    And hawl period of one lunar year is complete
    When Zakat assessment is performed
    Then Zakat should be obligatory
    And Zakat amount should be 250 USD (2.5% of zakatable wealth)

  Scenario: Personal residence excluded from zakatable wealth
    Given individual owns:
      | Asset Type         | Value      | Zakatable |
      | Primary Residence  | 300,000 USD| No        |
      | Investment Property| 100,000 USD| Yes       |
      | Cash Savings       | 20,000 USD | Yes       |
    When zakatable wealth is calculated
    Then primary residence should be excluded
    And zakatable wealth should be 120,000 USD
```

**Linguistic Alignment**:

- ✅ "zakatable wealth" (not "taxable amount" or "wealth_to_tax")
- ✅ "nisab threshold" (not "minimum_wealth_limit")
- ✅ "hawl" (not "annual_period" or "one_year_duration")
- ✅ "Zakat obligatory" (not "tax_required")

**Result**: Shariah scholar can read and validate scenarios without technical translation.

### Building Ubiquitous Language Through BDD

**BDD Process Enriches Ubiquitous Language**:

**Three Amigos Session** (Discovering terminology):

**Product Owner**: "Users need to know if they owe Zakat."

**Developer**: "What triggers Zakat?"

**Shariah Scholar** (Domain Expert): "Zakat is obligatory when wealth meets nisab—the minimum threshold—and has been owned for hawl, which is one complete lunar year in the Hijri calendar."

**Developer**: "So we check if `wealth >= threshold` and `timeOwned >= oneYear`?"

**Shariah Scholar**: "Not quite. The threshold is specifically called nisab, and the time period is hawl. Also, the calendar is Hijri, not Gregorian, so one year is 354 days, not 365."

**Outcome**: Ubiquitous language captured

**Gherkin Scenario** (Documenting agreed language):

```gherkin
Scenario: Zakat obligatory when wealth meets nisab for hawl
  Given individual owns 100 grams of gold
  And nisab threshold for gold is 85 grams
  And hawl period of 354 days has passed
  When Zakat obligation is evaluated
  Then Zakat should be obligatory
```

**Domain Model** (Implementing with ubiquitous language):

```typescript
export class NisabThreshold {
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

**Result**: Term "nisab" and "hawl" used consistently across conversations, scenarios, and code.

## Features per Bounded Context

### DDD Bounded Contexts

**DDD Strategic Pattern**: Divide large domains into bounded contexts—cohesive subdomains with clear boundaries, each with its own ubiquitous language and model.

**Islamic Finance Platform Contexts**:

```
┌─────────────────────────────────────────────────────────┐
│                  Islamic Finance Platform                │
└─────────────────────────────────────────────────────────┘
           │
           ├─── Zakat Calculation Context
           │    └─ Language: nisab, hawl, zakatable assets, Zakat rate
           │
           ├─── Halal Certification Context
           │    └─ Language: halal, haram, mashbooh, certification authority
           │
           ├─── Murabaha Financing Context
           │    └─ Language: cost price, profit markup, Riba, Murabaha contract
           │
           └─── Accounting Context
                └─ Language: journal entry, account, transaction, balance
```

### BDD Features Aligned with Bounded Contexts

**Pattern**: One feature file = One capability within one bounded context

**Directory Structure**:

```
features/
├── zakat-calculation/           # Bounded Context: Zakat Calculation
│   ├── README.md               # Context overview, ubiquitous language glossary
│   ├── gold-zakat.feature
│   ├── silver-zakat.feature
│   ├── cash-zakat.feature
│   ├── agricultural-zakat.feature
│   └── mixed-assets-zakat.feature
│
├── halal-certification/         # Bounded Context: Halal Certification
│   ├── README.md
│   ├── ingredient-verification.feature
│   ├── certification-authority-validation.feature
│   ├── supply-chain-traceability.feature
│   └── certificate-lifecycle.feature
│
├── murabaha-financing/          # Bounded Context: Murabaha Financing
│   ├── README.md
│   ├── contract-creation.feature
│   ├── profit-calculation.feature
│   ├── riba-detection.feature
│   └── payment-schedule.feature
│
└── accounting/                  # Bounded Context: Accounting
    ├── README.md
    ├── transaction-recording.feature
    ├── journal-entry-creation.feature
    └── financial-reporting.feature
```

**Context README Example** (`zakat-calculation/README.md`):

```markdown
# Zakat Calculation Bounded Context

## Ubiquitous Language

- **Zakat**: Mandatory charitable contribution (one of Five Pillars of Islam)
- **Nisab**: Minimum wealth threshold triggering Zakat obligation (85g gold / 595g silver)
- **Hawl**: One complete lunar year (354 days in Hijri calendar)
- **Zakatable Wealth**: Assets subject to Zakat (excludes personal-use items)
- **Zakat Rate**: 2.5% (one-fortieth) for most wealth types

## Business Rules

1. Zakat obligatory when: `zakatable_wealth >= nisab AND hawl_complete`
2. Personal residence excluded from zakatable wealth
3. Different rates for agricultural produce (5% irrigated, 10% rain-fed)

## Features

- Gold Zakat (`gold-zakat.feature`)
- Silver Zakat (`silver-zakat.feature`)
- Cash Zakat (`cash-zakat.feature`)
```

**Benefits**:

- **Clear Ownership**: Each bounded context owned by specific team
- **Context Isolation**: Zakat scenarios don't mix with Murabaha scenarios
- **Language Isolation**: Each context uses its own ubiquitous language
- **Scalability**: Add contexts without affecting others

### Cross-Context Scenarios

**When Features Span Contexts** (Integration scenarios):

**Scenario**: Zakat calculation requires accounting transaction recording

```gherkin
# features/integration/zakat-accounting-integration.feature
Feature: Zakat Payment Recording in Accounting System

  # This scenario spans two bounded contexts:
  # - Zakat Calculation (calculates amount)
  # - Accounting (records transaction)

  @integration @cross-context
  Scenario: Record Zakat payment as accounting transaction
    # Zakat Calculation Context
    Given individual has zakatable wealth of 10,000 USD
    When Zakat is calculated
    Then Zakat amount is 250 USD

    # Accounting Context
    When Zakat payment is processed
    Then journal entry should be created:
      | Account           | Debit  | Credit |
      | Zakat Expense     | 250    |        |
      | Cash              |        | 250    |
    And transaction should be recorded with category "Zakat"
```

**Pattern**: Integration scenarios explicitly span contexts, clearly marked with `@integration @cross-context` tags.

## Testing Domain Aggregates with BDD

### DDD Aggregates

**DDD Tactical Pattern**: Aggregate is a cluster of domain objects treated as a single unit for data changes. One entity is the Aggregate Root (controls access to other entities in the cluster).

**Example: ZakatAssessment Aggregate**

```typescript
// domain/zakat-calculation/zakat-assessment.aggregate.ts
export class ZakatAssessment {
  constructor(
    private readonly assessmentId: AssessmentId,
    private wealth: WealthPortfolio, // Entity
    private nisab: NisabThreshold, // Value Object
    private hawlPeriod: HawlPeriod, // Value Object
    private zakatObligation?: ZakatObligation, // Value Object
  ) {}

  // Aggregate root method (business rule enforcement)
  calculateZakat(): void {
    if (!this.meetsNisab()) {
      this.zakatObligation = ZakatObligation.notDue("Below nisab");
      return;
    }

    if (!this.hawlPeriod.isComplete()) {
      this.zakatObligation = ZakatObligation.notDue("Hawl incomplete");
      return;
    }

    const amount = this.wealth.calculateZakatableAmount() * ZakatRate.STANDARD;
    this.zakatObligation = ZakatObligation.due(amount);

    // Domain event
    this.addDomainEvent(new ZakatCalculated(this.assessmentId, amount));
  }

  private meetsNisab(): boolean {
    return this.wealth.totalValue() >= this.nisab.amount;
  }
}
```

### BDD Scenarios for Aggregates

**Pattern**: Test aggregate behavior through BDD scenarios

```gherkin
Feature: Zakat Assessment Aggregate

  # Test aggregate root business rules
  @aggregate @zakat-assessment
  Scenario: Calculate Zakat through aggregate
    Given ZakatAssessment aggregate with:
      | Assessment ID | Individual      | Wealth    | Nisab     |
      | ZA-001        | alice@example   | 10,000 USD| 5,600 USD |
    And hawl period is complete
    When calculateZakat method is invoked
    Then ZakatObligation should be "due"
    And Zakat amount should be 250 USD
    And ZakatCalculated domain event should be raised

  # Test aggregate invariant enforcement
  @aggregate @invariant
  Scenario: Aggregate enforces nisab invariant
    Given ZakatAssessment aggregate with wealth below nisab
    When calculateZakat method is invoked
    Then ZakatObligation should be "not due"
    And reason should be "Below nisab"
    And no domain events should be raised
```

**Step Definitions**:

```typescript
import { defineFeature, loadFeature } from "jest-cucumber";
import { ZakatAssessment } from "../domain/zakat-assessment.aggregate";
import { WealthPortfolio } from "../domain/wealth-portfolio";

const feature = loadFeature("./features/zakat-assessment-aggregate.feature");

defineFeature(feature, (test) => {
  let aggregate: ZakatAssessment;

  test("Calculate Zakat through aggregate", ({ given, and, when, then }) => {
    given(/ZakatAssessment aggregate with:/, (table) => {
      const row = table[0];
      aggregate = new ZakatAssessment(
        new AssessmentId(row["Assessment ID"]),
        new WealthPortfolio([{ amount: 10000, currency: "USD" }]),
        new NisabThreshold(5600, "USD"),
        HawlPeriod.complete(),
      );
    });

    and("hawl period is complete", () => {
      // Already set up in aggregate construction
    });

    when("calculateZakat method is invoked", () => {
      aggregate.calculateZakat();
    });

    then(/ZakatObligation should be "(.+)"/, (status: string) => {
      expect(aggregate.zakatObligation?.status).toBe(status);
    });

    and("Zakat amount should be {int} USD", (expectedAmount: number) => {
      expect(aggregate.zakatObligation?.amount.value).toBe(expectedAmount);
    });

    and("ZakatCalculated domain event should be raised", () => {
      const events = aggregate.getDomainEvents();
      const zakatCalculatedEvent = events.find((e) => e instanceof ZakatCalculated);
      expect(zakatCalculatedEvent).toBeDefined();
    });
  });
});
```

## Testing Domain Events with BDD

### DDD Domain Events

**DDD Pattern**: Domain events represent something significant that happened in the domain. Events capture state changes and enable decoupling between bounded contexts.

**Example Domain Events** (Islamic Finance):

```typescript
// domain/zakat-calculation/events/zakat-calculated.event.ts
export class ZakatCalculated extends DomainEvent {
  constructor(
    public readonly assessmentId: AssessmentId,
    public readonly amount: Money,
    public readonly dueDate: Date,
    public readonly occurredAt: Date = new Date(),
  ) {
    super();
  }
}

// domain/zakat-calculation/events/hawl-completed.event.ts
export class HawlCompleted extends DomainEvent {
  constructor(
    public readonly individualId: IndividualId,
    public readonly completionDate: Date,
    public readonly occurredAt: Date = new Date(),
  ) {
    super();
  }
}

// domain/murabaha-financing/events/riba-detected.event.ts
export class RibaDetected extends DomainEvent {
  constructor(
    public readonly contractId: ContractId,
    public readonly ribaType: RibaType,
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
Feature: Zakat Calculation Domain Events

  @domain-events @zakat-calculated
  Scenario: ZakatCalculated event raised when Zakat calculated
    Given individual has zakatable wealth of 10,000 USD
    And nisab threshold is 5,600 USD
    And hawl is complete
    When Zakat assessment is performed
    Then ZakatCalculated event should be published
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
    And event should trigger Zakat calculation workflow

  @domain-events @riba-detected @compliance
  Scenario: RibaDetected event raised for compliance violation
    Given Murabaha contract with time-based interest
    When Shariah compliance check is performed
    Then RibaDetected event should be published
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

then("ZakatCalculated event should be published", () => {
  const zakatCalculatedEvent = publishedEvents.find((e) => e instanceof ZakatCalculated);
  expect(zakatCalculatedEvent).toBeDefined();
});

then("event should contain:", (table) => {
  const zakatEvent = publishedEvents.find((e) => e instanceof ZakatCalculated) as ZakatCalculated;

  table.forEach((row) => {
    const field = row["Field"];
    const expectedValue = row["Value"];

    if (field === "amount") {
      expect(zakatEvent.amount.toString()).toBe(expectedValue);
    } else if (field === "dueDate") {
      if (expectedValue === "today") {
        expect(zakatEvent.dueDate.toDateString()).toBe(new Date().toDateString());
      }
    }
  });
});

then("event should trigger Zakat calculation workflow", async () => {
  // Verify event handler invoked
  const workflowTriggered = await workflowOrchestrator.wasTriggered("ZakatCalculation");
  expect(workflowTriggered).toBe(true);
});
```

## Testing Value Objects with BDD

### DDD Value Objects

**DDD Pattern**: Value objects are immutable objects defined by their attributes (no identity). Examples: Money, NisabThreshold, HawlPeriod.

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

// domain/zakat-calculation/nisab-threshold.value-object.ts
export class NisabThreshold {
  constructor(
    public readonly amount: number,
    public readonly unit: string,
    public readonly assetType: AssetType,
  ) {
    if (amount <= 0) {
      throw new InvalidNisabError("Nisab must be positive");
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
├── zakat-calculation/
│   ├── domain/                          # Unit-level BDD (domain layer)
│   │   ├── zakat-assessment.feature    # Aggregate behavior
│   │   ├── money.feature                # Value object behavior
│   │   └── domain-events.feature        # Event raising
│   │
│   ├── application/                     # Integration BDD (use cases)
│   │   ├── calculate-zakat-use-case.feature
│   │   └── submit-zakat-payment-use-case.feature
│   │
│   └── e2e/                             # E2E BDD (full stack)
│       └── zakat-self-assessment-journey.feature
```

**Domain Layer BDD** (Fast, isolated):

```gherkin
# features/zakat-calculation/domain/zakat-assessment.feature
@unit @domain
Feature: ZakatAssessment Aggregate

  Scenario: Calculate Zakat on aggregate
    Given ZakatAssessment with wealth 10,000 USD and nisab 5,600 USD
    When calculateZakat is called
    Then Zakat should be due
    And amount should be 250 USD
```

**Application Layer BDD** (Integration, use case orchestration):

```gherkin
# features/zakat-calculation/application/calculate-zakat-use-case.feature
@integration @use-case
Feature: Calculate Zakat Use Case

  Scenario: Execute Zakat calculation use case
    Given individual "alice@example.com" exists in database
    And individual has wealth 10,000 USD
    When CalculateZakatUseCase is executed for individual
    Then Zakat calculation should be saved to database
    And ZakatCalculated event should be published
    And individual should be notified via email
```

**E2E Layer BDD** (Full journey):

```gherkin
# features/zakat-calculation/e2e/zakat-self-assessment-journey.feature
@e2e @journey
Feature: Zakat Self-Assessment Journey

  Scenario: User completes Zakat self-assessment
    Given user navigates to /zakat/assessment
    When user enters wealth details
    And clicks "Calculate Zakat"
    Then user sees "Zakat Due: 250 USD"
    And user receives email confirmation
```

## Islamic Finance Examples

### Example 1: Zakat Calculation Bounded Context

**Ubiquitous Language in Scenarios**:

```gherkin
# features/zakat-calculation/gold-zakat.feature
Feature: Zakat Calculation for Gold Wealth

  # Ubiquitous language: nisab, hawl, zakatable, Zakat rate

  Background:
    Given nisab threshold for gold is 85 grams
    And Zakat rate for gold is 2.5%

  Scenario: Zakat obligatory when gold meets nisab for hawl
    Given individual owns 100 grams of gold
    And gold has been owned for one complete hawl (354 days)
    When Zakat obligation is evaluated
    Then Zakat should be obligatory
    And Zakat amount should be 2.5 grams of gold

  Scenario: Zakat not obligatory when gold below nisab
    Given individual owns 50 grams of gold (below nisab)
    When Zakat obligation is evaluated
    Then Zakat should not be obligatory
    And individual should be informed "Wealth below nisab threshold"
```

**Domain Model Uses Same Language**:

```typescript
export class GoldZakatCalculator {
  private readonly NISAB_GOLD_GRAMS = 85;
  private readonly ZAKAT_RATE = 0.025;
  private readonly HAWL_DAYS = 354;

  calculateZakat(goldWealth: GoldWealth, hawlPeriod: HawlPeriod): ZakatObligation {
    if (!this.meetsNisab(goldWealth)) {
      return ZakatObligation.notDue("Wealth below nisab threshold");
    }

    if (!hawlPeriod.isComplete(this.HAWL_DAYS)) {
      return ZakatObligation.notDue("Hawl not complete");
    }

    const zakatAmount = goldWealth.amount * this.ZAKAT_RATE;
    return ZakatObligation.due(zakatAmount, "grams");
  }

  private meetsNisab(goldWealth: GoldWealth): boolean {
    return goldWealth.amount >= this.NISAB_GOLD_GRAMS;
  }
}
```

### Example 2: Halal Certification with Domain Events

**Feature with Domain Events**:

```gherkin
# features/halal-certification/certification-lifecycle.feature
@halal @domain-events
Feature: Halal Certification Lifecycle

  Scenario: Issue halal certification
    Given product "Organic Dates" passed all verifications
    And certification authority is "JAKIM"
    When halal certificate is issued
    Then HalalCertificateIssued event should be published
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
    Then HalalCertificateRevoked event should be published
    And product status should be updated to "Certification Expired"
    And supplier should be alerted
```

**Domain Event Handler**:

```typescript
@EventHandler(HalalCertificateIssued)
export class SendCertificateNotificationHandler {
  constructor(
    private readonly emailService: EmailService,
    private readonly pdfGenerator: PdfGenerator,
  ) {}

  async handle(event: HalalCertificateIssued): Promise<void> {
    const certificatePdf = await this.pdfGenerator.generate(event.certificateId);

    await this.emailService.send({
      to: event.supplierEmail,
      subject: "Halal Certificate Issued",
      body: `Your product "${event.productName}" has been certified halal by ${event.certificationBody}.`,
      attachments: [certificatePdf],
    });
  }
}
```

### Example 3: Murabaha Financing Aggregate

**Testing Aggregate Invariants**:

```gherkin
# features/murabaha-financing/murabaha-contract-aggregate.feature
@aggregate @murabaha
Feature: Murabaha Contract Aggregate

  Scenario: Create valid Murabaha contract
    Given bank purchases asset for 100,000 USD (cost price)
    And bank discloses cost price to customer
    And profit markup is 15,000 USD (15%)
    When MurabahaContract aggregate is created
    Then contract should be valid
    And selling price should be 115,000 USD
    And ContractCreated event should be raised

  @invariant @riba
  Scenario: Aggregate rejects time-based interest (invariant violation)
    Given bank attempts to create contract with interest rate 5%
    When MurabahaContract aggregate is created
    Then InvalidContractError should be thrown
    And error should be "Riba prohibited: Time-based interest not allowed in Murabaha"
    And no domain events should be raised

  @invariant @disclosure
  Scenario: Aggregate enforces cost disclosure (invariant)
    Given bank purchases asset for 100,000 USD
    And cost price NOT disclosed to customer
    When MurabahaContract aggregate is created
    Then InvalidContractError should be thrown
    And error should be "Shariah requirement: Cost price must be disclosed to customer"
```

**Aggregate with Invariants**:

```typescript
export class MurabahaContract {
  constructor(
    private readonly contractId: ContractId,
    private readonly costPrice: Money,
    private readonly profitMarkup: Money,
    private readonly costDisclosedToCustomer: boolean,
  ) {
    this.validateInvariants();
  }

  private validateInvariants(): void {
    // Invariant: Cost must be disclosed (Shariah requirement)
    if (!this.costDisclosedToCustomer) {
      throw new InvalidContractError("Shariah requirement: Cost price must be disclosed to customer");
    }

    // Invariant: Profit must be fixed markup, not interest
    if (this.profitMarkup.isTimeBasedInterest()) {
      throw new InvalidContractError("Riba prohibited: Time-based interest not allowed in Murabaha");
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
- **BDD Uses**: Same vocabulary in Gherkin scenarios (nisab, hawl, Riba, halal)
- **Result**: Domain experts can read and validate scenarios without translation

**Features per Bounded Context**:

- **DDD Provides**: Strategic boundaries dividing complex domains
- **BDD Organizes**: Feature files per bounded context (Zakat, Halal, Murabaha)
- **Result**: Clear ownership, context isolation, scalable organization

**Testing Domain Patterns**:

- **Aggregates**: BDD scenarios verify aggregate behavior and invariants
- **Domain Events**: Then steps verify events raised (ZakatCalculated, RibaDetected)
- **Value Objects**: BDD tests verify immutability, equality, operations
- **Use Cases**: Integration BDD tests verify application layer orchestration

**Architectural Alignment**:

- **Domain Layer**: Unit-level BDD tests (fast, isolated)
- **Application Layer**: Integration BDD tests (use case orchestration)
- **Presentation Layer**: E2E BDD tests (full user journeys)

**Islamic Finance Benefits**:

- Shariah scholars validate BDD scenarios using domain language
- Bounded contexts mirror Islamic finance domains (Zakat, Halal, Murabaha)
- Domain events track compliance (RibaDetected, CertificationRevoked)
- Aggregates enforce Shariah invariants (cost disclosure, Riba prohibition)

Use BDD and DDD together to create domain-aligned, stakeholder-verified, well-tested software that accurately models complex business domains while remaining accessible to domain experts.

## Document Metadata

- **Category**: Explanation
- **Subcategory**: Software Design > Behavior-Driven Development
- **Tags**: BDD, DDD, Domain-Driven Design, Ubiquitous Language, Bounded Context, Aggregates, Domain Events, Value Objects, Islamic Finance, Zakat, Halal, Murabaha
- **Related Files**:
  - [README](./README.md) - BDD documentation overview
  - [DDD Introduction](../domain-driven-design-ddd/README.md) - Domain-Driven Design overview
  - [08. Feature Files and Organization](./ex-sode-bdd__08-feature-files-and-organization.md) - Organizing by bounded context
  - [13. BDD and TDD](./ex-sode-bdd__13-bdd-and-tdd.md) - TDD integration
- **Prerequisites**: Understanding of DDD concepts (bounded contexts, aggregates, domain events), BDD scenarios (Gherkin)
- **Next Steps**: Read [BDD in Nx Monorepo](./ex-sode-bdd__15-bdd-in-nx-monorepo.md) for practical implementation patterns
- **Last Updated**: 2026-01-20
- **Status**: Active
