---
title: Best Practices in Domain-Driven Design
tags:
  - explanation
  - software
  - architecture
  - domain-driven-design
  - ddd
  - best-practices
  - strategic-design
  - tactical-design
  - islamic-finance
  - zakat
  - murabaha
---

# Best Practices in Domain-Driven Design

> **Companion Document**: See [Antipatterns in Domain-Driven Design](./ex-so-ar-dodrdedd__20-antipatterns.md) for common mistakes to avoid.

## Overview

Domain-Driven Design (DDD) is a powerful approach for tackling complex business domains, but it requires careful application to achieve success. This guide explores proven best practices with examples from Islamic finance and Shariah-compliant business systems.

Success in DDD comes from balancing strategic design (understanding the business domain) with tactical patterns (implementing the code), while maintaining close collaboration with domain experts and avoiding over-engineering.

The benefits of following DDD best practices include:

- **Reduced Business Risk**: Accurate domain models minimize Shariah compliance violations and business logic errors
- **Faster Feature Development**: Clear bounded contexts and ubiquitous language accelerate development after initial investment
- **Better Communication**: Shared vocabulary eliminates translation errors between business and technical teams
- **Maintainable Code**: Rich domain models localize business logic, making changes easier and safer
- **Scalable Architecture**: Proper aggregate boundaries and eventual consistency enable system growth

## Table of Contents

- [Strategic Design Best Practices](#strategic-design-best-practices)
- [Tactical Design Best Practices](#tactical-design-best-practices)
- [Modeling Best Practices](#modeling-best-practices)
- [Collaboration Best Practices](#collaboration-best-practices)
- [Implementation Best Practices](#implementation-best-practices)
- [Islamic Finance Examples](#islamic-finance-examples)
- [Summary](#summary)

## Strategic Design Best Practices

### Practice 1: Start with Business Outcomes

**What**: Always begin DDD initiatives by understanding the business problems you're solving and the value you're delivering.

**Why**: DDD is expensive in terms of time and effort. Without clear business outcomes, you risk creating elegant architectures that solve the wrong problems.

**How**:

```markdown
## Business Outcome Example: Zakat Automation

**Current State**: Manual Zakat calculations take 2-3 days per client, error rate 15%
**Desired State**: Automated calculations in minutes, error rate <1%
**Business Value**: Process 10x more clients, reduce Shariah compliance risk
**Investment Justification**: $200k development cost vs $500k annual operational savings
```

**Islamic Finance Example**:

For a Murabaha financing system:

- **Business Outcome**: Reduce contract processing time from 5 days to 1 day
- **Value**: Handle 5x more financing requests with same staff
- **Risk Reduction**: Automated Shariah compliance checks reduce fatwa violations
- **Revenue Impact**: Faster processing = more transactions = higher revenue

**When to Apply**: Before any DDD project starts, during initial discovery workshops.

**Red Flags**:

- Starting with "we need microservices" instead of business problems
- Focusing on technical elegance without business metrics
- Unable to articulate ROI or business value

### Practice 2: Establish Ubiquitous Language Early

**What**: Create a shared vocabulary between developers and domain experts that is used consistently in code, conversations, and documentation.

**Why**: Miscommunication is the primary source of software defects in complex domains. A shared language eliminates translation errors.

**How**:

```typescript
// BAD: Developer terminology
class Transaction {
  type: string; // "donation"
  amount: number;
  taxDeductible: boolean;
}

// GOOD: Ubiquitous Language from Islamic finance
class ZakatPayment {
  nisab: Money; // Minimum wealth threshold
  zakatableAmount: Money; // 2.5% of qualifying assets
  hawl: HijriYear; // Lunar year completion
  recipient: ZakatRecipient; // One of eight categories
}
```

**Islamic Finance Example**:

**Ubiquitous Language Glossary for Murabaha Bounded Context**:

| Term     | Arabic | Meaning               | Code Representation          |
| -------- | ------ | --------------------- | ---------------------------- |
| Murabaha | مرابحة | Cost-plus financing   | `MurabahaContract` class     |
| Mithali  | مثلي   | Fungible commodity    | `FungibleAsset` value object |
| Qimiyy   | قيمي   | Non-fungible asset    | `UniqueAsset` value object   |
| Tamlik   | تمليك  | Transfer of ownership | `OwnershipTransfer` event    |
| Qabdh    | قبض    | Possession            | `PossessionTaken` event      |

**When to Apply**: During discovery workshops, event storming sessions, and continuously throughout development.

**Red Flags**:

- Developers saying "users" when domain experts say "Zakat payers"
- Code using "payment" when business uses "Sadaqah contribution"
- Translation layer between business language and code

### Practice 3: Use Bounded Contexts for Clear Boundaries

**What**: Divide the domain into distinct bounded contexts, each with its own model, language, and clear boundaries.

**Why**: Attempting to create one unified model for an entire enterprise leads to ambiguous terms, bloated models, and coordination bottlenecks.

**How**:

```
Islamic Finance Platform Bounded Contexts:

┌─────────────────────┐  ┌──────────────────────┐  ┌─────────────────────┐
│  Zakat Management   │  │  Waqf Administration │  │  Murabaha Financing │
│                     │  │                      │  │                     │
│  - ZakatCalculation │  │  - WaqfEndowment     │  │  - MurabahaContract │
│  - NisabThreshold   │  │  - BeneficiaryGrant  │  │  - AssetPurchase    │
│  - ZakatRecipient   │  │  - PropertyManagement│  │  - InstallmentPlan  │
└─────────────────────┘  └──────────────────────┘  └─────────────────────┘
         │                          │                         │
         └──────────────────────────┴─────────────────────────┘
                                    │
                          ┌─────────┴──────────┐
                          │  Identity & Access │
                          │                    │
                          │  - User            │
                          │  - Role            │
                          │  - Permission      │
                          └────────────────────┘
```

**Islamic Finance Example**:

The term "Asset" means different things in different contexts:

- **Zakat Management Context**: Asset = property subject to Zakat calculation (gold, cash, inventory)
- **Waqf Context**: Asset = endowed property generating income for beneficiaries
- **Murabaha Context**: Asset = commodity being sold with markup

Each bounded context has its own `Asset` model with different properties and behaviors.

**When to Apply**: During strategic design, when identifying subdomains and team boundaries.

**Red Flags**:

- One "Asset" entity shared across all contexts
- Constant debates about "what does X mean here?"
- Models becoming bloated with context-specific fields

### Practice 4: Align Bounded Contexts with Microservices

**What**: Create a one-to-one relationship between bounded contexts and microservices for maximum autonomy.

**Why**: This alignment gives each service clear boundaries, independent deployment, and team ownership.

**How**:

```
Microservice Architecture for Islamic Finance Platform:

┌─────────────────────────────────────────────────────────────┐
│                     API Gateway                             │
└─────────────────────────────────────────────────────────────┘
         │              │                 │
         ▼              ▼                 ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────────┐
│   Zakat     │  │    Waqf     │  │    Murabaha     │
│   Service   │  │   Service   │  │     Service     │
├─────────────┤  ├─────────────┤  ├─────────────────┤
│ Bounded     │  │ Bounded     │  │ Bounded         │
│ Context:    │  │ Context:    │  │ Context:        │
│ Zakat Mgmt  │  │ Waqf Admin  │  │ Murabaha Fin.   │
├─────────────┤  ├─────────────┤  ├─────────────────┤
│ Own DB      │  │ Own DB      │  │ Own DB          │
└─────────────┘  └─────────────┘  └─────────────────┘
```

**Islamic Finance Example**:

**Zakat Service** (autonomous):

- **Database**: Zakat-specific schema (calculations, recipients, distributions)
- **Team**: Zakat domain experts + developers
- **Deployment**: Independent release cycle
- **Integration**: Publishes `ZakatCalculated` event to event bus

**Waqf Service** (autonomous):

- **Database**: Waqf-specific schema (endowments, beneficiaries, grants)
- **Team**: Waqf domain experts + developers
- **Deployment**: Independent release cycle
- **Integration**: Subscribes to `DonationReceived` event

**When to Apply**: During system architecture design, microservices decomposition.

**Red Flags**:

- One database shared across multiple bounded contexts
- Changes in one bounded context requiring deployments in others
- Teams blocked waiting for other teams

### Practice 5: Use Event Storming for Discovery

**What**: Run collaborative workshops where domain experts and developers explore the domain by identifying domain events, commands, and aggregates.

**Why**: Event storming quickly reveals domain complexity, highlights bottlenecks, and builds shared understanding without getting bogged down in implementation details.

**How**:

```
Event Storming Session Output (Murabaha Financing):

Domain Events (Orange):
┌──────────────────┐  ┌─────────────────┐  ┌──────────────────┐
│ Client Requested │→ │ Asset Purchased │→ │ Contract Drafted │
│   Financing      │  │   by Bank       │  │                  │
└──────────────────┘  └─────────────────┘  └──────────────────┘
         ↓                     ↓                      ↓
Commands (Blue):      Commands (Blue):      Commands (Blue):
- Submit Request      - Purchase Asset      - Draft Contract
- Verify Eligibility  - Verify Possession   - Calculate Markup

Aggregates (Yellow):
- FinancingRequest    - AssetPurchase       - MurabahaContract

Policies (Purple):
                      "When Asset          "When Contract
                       Purchased,           Drafted,
                       Then Draft           Then Notify
                       Contract"            Client"
```

**Islamic Finance Example**:

**Event Storming for Zakat Calculation Bounded Context**:

**Step 1: Domain Events**

- `ZakatYearStarted`
- `AssetDeclared`
- `NisabThresholdMet`
- `HawlCompleted`
- `ZakatCalculated`
- `ZakatPaid`
- `ReceiptIssued`

**Step 2: Commands**

- `StartZakatYear`
- `DeclareAsset`
- `CalculateZakat`
- `PayZakat`

**Step 3: Aggregates**

- `ZakatCalculation` (root)
- `ZakatableAsset`
- `ZakatPayment`

**Step 4: Policies**

- "When Hawl Completed, Then Calculate Zakat"
- "When Zakat Calculated, Then Notify Payer"
- "When Zakat Paid, Then Issue Receipt"

**When to Apply**: Early in discovery phase, when entering new subdomains, when redesigning existing systems.

**Red Flags**:

- Skipping straight to entity modeling without understanding events
- Event storming session dominated by developers, not domain experts
- No follow-up to convert discoveries into code

## Tactical Design Best Practices

### Practice 6: Design Rich Domain Models

**What**: Create domain models with both data and behavior, where business logic lives in the domain objects themselves.

**Why**: Rich models encapsulate business rules, making them easier to understand, test, and maintain. They prevent business logic from scattering across services.

**How**:

```typescript
// BAD: Anemic model (data bag)
class ZakatCalculation {
  assets: Asset[];
  nisabThreshold: number;
  zakatAmount: number;
}

// Service contains all logic
class ZakatService {
  calculateZakat(calculation: ZakatCalculation): number {
    const totalAssets = calculation.assets.reduce((sum, asset) => sum + asset.value, 0);
    if (totalAssets < calculation.nisabThreshold) {
      return 0;
    }
    return totalAssets * 0.025;
  }
}

// GOOD: Rich domain model
class ZakatCalculation {
  private assets: Asset[];
  private nisabThreshold: Money;
  private hawlStartDate: HijriDate;

  addAsset(asset: Asset): void {
    if (!asset.isZakatable()) {
      throw new NonZakatableAssetError(asset);
    }
    this.assets.push(asset);
  }

  calculateZakat(currentDate: HijriDate): Money {
    this.verifyHawlCompleted(currentDate);

    const zakatableAmount = this.calculateZakatableAmount();

    if (zakatableAmount.isLessThan(this.nisabThreshold)) {
      return Money.zero();
    }

    return zakatableAmount.multiply(0.025);
  }

  private verifyHawlCompleted(currentDate: HijriDate): void {
    const daysPassed = currentDate.daysSince(this.hawlStartDate);
    if (daysPassed < 354) {
      // Lunar year
      throw new HawlNotCompletedError(daysPassed);
    }
  }

  private calculateZakatableAmount(): Money {
    return this.assets
      .filter((asset) => asset.isZakatable())
      .reduce((sum, asset) => sum.add(asset.zakatableValue()), Money.zero());
  }
}
```

**Islamic Finance Example**:

**Rich Murabaha Contract Aggregate**:

```typescript
class MurabahaContract {
  private status: ContractStatus;
  private assetPurchase: AssetPurchase;
  private markup: Percentage;
  private installmentPlan: InstallmentPlan;
  private possessionTransferred: boolean;

  // Business rule: Cannot draft contract before asset possession
  draftContract(markup: Percentage): void {
    if (!this.possessionTransferred) {
      throw new AssetNotPossessedError("Bank must take possession before drafting contract");
    }

    this.markup = markup;
    this.status = ContractStatus.Drafted;
    this.recordEvent(new ContractDrafted(this.id, markup));
  }

  // Business rule: Markup must be fixed, not variable
  calculateTotalPrice(): Money {
    if (this.markup.isVariable()) {
      throw new VariableMarkupNotAllowedError("Murabaha requires fixed markup");
    }

    const cost = this.assetPurchase.totalCost();
    return cost.add(cost.multiply(this.markup));
  }

  // Business rule: Asset must be Shariah-compliant
  private validateAssetCompliance(): void {
    if (this.assetPurchase.asset.isProhibited()) {
      throw new ProhibitedAssetError(`Cannot finance ${this.assetPurchase.asset.category}`);
    }
  }
}
```

**When to Apply**: Always, when modeling core domain entities and aggregates.

**Red Flags**:

- All business logic in service classes
- Domain objects with only getters/setters
- Comments explaining business rules instead of code expressing them

### Practice 7: Protect Aggregate Invariants

**What**: Use aggregates to enforce business invariants and maintain consistency boundaries. Only allow state changes through aggregate methods.

**Why**: Aggregates ensure that business rules cannot be violated, even in concurrent or distributed scenarios.

**How**:

```typescript
// Aggregate Root: WaqfEndowment
class WaqfEndowment {
  private readonly id: WaqfId;
  private beneficiaries: Beneficiary[];
  private totalDistributed: Money;
  private readonly principal: Money; // Cannot be touched
  private income: Money;

  // INVARIANT: Total distributed cannot exceed available income
  distributeToBeneficiaries(distributions: Distribution[]): void {
    const totalRequested = distributions.reduce((sum, d) => sum.add(d.amount), Money.zero());

    // Protect invariant
    if (totalRequested.isGreaterThan(this.income)) {
      throw new InsufficientIncomeError(`Requested: ${totalRequested}, Available: ${this.income}`);
    }

    // Invariant maintained
    distributions.forEach((d) => {
      this.distributeToBeneficiary(d);
      this.totalDistributed = this.totalDistributed.add(d.amount);
      this.income = this.income.subtract(d.amount);
    });

    this.recordEvent(new IncomeDistributed(this.id, distributions));
  }

  // INVARIANT: Principal must remain intact
  withdrawPrincipal(amount: Money): void {
    throw new PrincipalCannotBeWithdrawnError("Waqf principal is perpetual and cannot be withdrawn");
  }

  // INVARIANT: Beneficiaries must sum to 100%
  setBeneficiaries(beneficiaries: Beneficiary[]): void {
    const totalPercentage = beneficiaries.reduce((sum, b) => sum + b.percentage, 0);

    if (totalPercentage !== 100) {
      throw new InvalidBeneficiaryAllocationError(`Total must be 100%, got ${totalPercentage}%`);
    }

    this.beneficiaries = beneficiaries;
  }
}
```

**Islamic Finance Example**:

**Zakat Calculation Aggregate Invariants**:

```typescript
class ZakatCalculation {
  // INVARIANT: Hawl must be complete before calculation
  calculateZakat(): Money {
    if (!this.hawlCompleted) {
      throw new HawlNotCompletedError();
    }
    // ... calculation logic
  }

  // INVARIANT: Only zakatable assets count
  addAsset(asset: Asset): void {
    if (!asset.isZakatable()) {
      throw new NonZakatableAssetError();
    }
    this.assets.push(asset);
  }

  // INVARIANT: Nisab threshold must be met
  private verifyNisabMet(totalAssets: Money): void {
    if (totalAssets.isLessThan(this.nisabThreshold)) {
      throw new NisabNotMetError();
    }
  }
}
```

**When to Apply**: Always, when designing aggregates and defining consistency boundaries.

**Red Flags**:

- Direct field access from outside the aggregate
- Business rules validated in application services instead of aggregates
- Aggregates with no methods, only properties

### Practice 8: Use Value Objects for Immutable Concepts

**What**: Model domain concepts without identity as immutable value objects. Two value objects with the same attributes are considered equal.

**Why**: Value objects reduce bugs (immutability), make code more expressive, and encapsulate validation logic.

**How**:

```typescript
// Value Object: Money
class Money {
  private readonly amount: number;
  private readonly currency: Currency;

  private constructor(amount: number, currency: Currency) {
    if (amount < 0) {
      throw new NegativeAmountError();
    }
    this.amount = amount;
    this.currency = currency;
  }

  static of(amount: number, currency: Currency): Money {
    return new Money(amount, currency);
  }

  // Immutable operations return new instances
  add(other: Money): Money {
    this.ensureSameCurrency(other);
    return new Money(this.amount + other.amount, this.currency);
  }

  multiply(factor: number): Money {
    return new Money(this.amount * factor, this.currency);
  }

  // Value equality
  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency.equals(other.currency);
  }

  isGreaterThan(other: Money): boolean {
    this.ensureSameCurrency(other);
    return this.amount > other.amount;
  }

  private ensureSameCurrency(other: Money): void {
    if (!this.currency.equals(other.currency)) {
      throw new CurrencyMismatchError();
    }
  }
}

// Value Object: HijriDate
class HijriDate {
  private readonly year: number;
  private readonly month: number;
  private readonly day: number;

  private constructor(year: number, month: number, day: number) {
    this.validateDate(year, month, day);
    this.year = year;
    this.month = month;
    this.day = day;
  }

  static of(year: number, month: number, day: number): HijriDate {
    return new HijriDate(year, month, day);
  }

  daysSince(other: HijriDate): number {
    // Lunar year calculation (354 days)
    const yearsDiff = this.year - other.year;
    const daysDiff = this.dayOfYear() - other.dayOfYear();
    return yearsDiff * 354 + daysDiff;
  }

  addLunarYear(): HijriDate {
    return new HijriDate(this.year + 1, this.month, this.day);
  }

  private dayOfYear(): number {
    // Calculate day of lunar year
    return (this.month - 1) * 29.5 + this.day;
  }
}
```

**Islamic Finance Example**:

**Value Objects in Zakat Domain**:

```typescript
// Percentage (used for Zakat rate)
class Percentage {
  private readonly value: number;

  static of(value: number): Percentage {
    if (value < 0 || value > 100) {
      throw new InvalidPercentageError(value);
    }
    return new Percentage(value);
  }

  static zakatRate(): Percentage {
    return Percentage.of(2.5); // Fixed 2.5%
  }

  applyTo(amount: Money): Money {
    return amount.multiply(this.value / 100);
  }
}

// NisabThreshold (minimum wealth for Zakat)
class NisabThreshold {
  private readonly amount: Money;
  private readonly asOfDate: HijriDate;

  static goldNisab(goldPricePerGram: Money): NisabThreshold {
    const nisabGrams = 85; // 85 grams of gold
    return new NisabThreshold(goldPricePerGram.multiply(nisabGrams), HijriDate.today());
  }

  static silverNisab(silverPricePerGram: Money): NisabThreshold {
    const nisabGrams = 595; // 595 grams of silver
    return new NisabThreshold(silverPricePerGram.multiply(nisabGrams), HijriDate.today());
  }

  isMet(totalAssets: Money): boolean {
    return totalAssets.isGreaterThanOrEqual(this.amount);
  }
}
```

**When to Apply**: For domain concepts without identity: money, dates, percentages, addresses, ranges.

**Red Flags**:

- Primitive obsession (using `number` instead of `Money`)
- Mutable value objects
- Value objects with database IDs

### Practice 9: Use Domain Events for Decoupling

**What**: Model significant domain occurrences as immutable domain events. Publish events when state changes occur.

**Why**: Events enable eventual consistency, decouple bounded contexts, create audit trails, and enable event sourcing.

**How**:

```typescript
// Domain Event
interface DomainEvent {
  readonly eventId: string;
  readonly occurredOn: Date;
  readonly aggregateId: string;
}

class ZakatCalculated implements DomainEvent {
  readonly eventId: string;
  readonly occurredOn: Date;
  readonly aggregateId: string; // ZakatCalculationId

  readonly zakatableAmount: Money;
  readonly nisabThreshold: Money;
  readonly zakatDue: Money;
  readonly hawlYear: HijriYear;

  constructor(
    calculationId: string,
    zakatableAmount: Money,
    nisabThreshold: Money,
    zakatDue: Money,
    hawlYear: HijriYear,
  ) {
    this.eventId = uuid();
    this.occurredOn = new Date();
    this.aggregateId = calculationId;
    this.zakatableAmount = zakatableAmount;
    this.nisabThreshold = nisabThreshold;
    this.zakatDue = zakatDue;
    this.hawlYear = hawlYear;
  }
}

// Aggregate publishes events
class ZakatCalculation {
  private events: DomainEvent[] = [];

  calculateZakat(currentDate: HijriDate): Money {
    // ... business logic ...

    const zakatDue = this.performCalculation();

    // Record domain event
    this.recordEvent(new ZakatCalculated(this.id, this.zakatableAmount, this.nisabThreshold, zakatDue, this.hawlYear));

    return zakatDue;
  }

  private recordEvent(event: DomainEvent): void {
    this.events.push(event);
  }

  getUncommittedEvents(): DomainEvent[] {
    return [...this.events];
  }

  clearEvents(): void {
    this.events = [];
  }
}

// Event Handler in another bounded context
class NotificationService {
  @EventHandler(ZakatCalculated)
  async onZakatCalculated(event: ZakatCalculated): Promise<void> {
    const user = await this.userRepository.findById(event.aggregateId);

    await this.emailService.send({
      to: user.email,
      subject: "Your Zakat Calculation is Ready",
      body: `Your Zakat due: ${event.zakatDue.format()}`,
    });
  }
}
```

**Islamic Finance Example**:

**Murabaha Contract Events**:

```typescript
// Events in Murabaha bounded context
class AssetPurchased implements DomainEvent {
  constructor(
    public readonly contractId: string,
    public readonly asset: Asset,
    public readonly purchasePrice: Money,
    public readonly vendor: Vendor,
  ) {}
}

class PossessionTaken implements DomainEvent {
  constructor(
    public readonly contractId: string,
    public readonly possessionDate: Date,
    public readonly warehouseLocation: string,
  ) {}
}

class ContractDrafted implements DomainEvent {
  constructor(
    public readonly contractId: string,
    public readonly markup: Percentage,
    public readonly totalPrice: Money,
    public readonly installmentPlan: InstallmentPlan,
  ) {}
}

class ContractSigned implements DomainEvent {
  constructor(
    public readonly contractId: string,
    public readonly clientSignature: Signature,
    public readonly bankSignature: Signature,
    public readonly signedOn: Date,
  ) {}
}

// Event handler in Accounting bounded context
class AccountingService {
  @EventHandler(ContractSigned)
  async onContractSigned(event: ContractSigned): Promise<void> {
    // Create accounting entries
    await this.ledgerService.createJournalEntry({
      debit: { account: "Murabaha Receivables", amount: event.totalPrice },
      credit: { account: "Inventory", amount: event.purchasePrice },
      credit: { account: "Murabaha Revenue", amount: event.markup },
    });
  }
}
```

**When to Apply**: For significant state changes, cross-context communication, audit requirements.

**Red Flags**:

- Direct calls between bounded contexts
- No audit trail of state changes
- Tight coupling between aggregates

### Practice 10: Keep Aggregates Small

**What**: Design aggregates to be as small as possible while still protecting invariants. Favor consistency boundaries over navigational convenience.

**Why**: Large aggregates create contention, performance issues, and complex transaction boundaries. Small aggregates enable better scalability and concurrency.

**How**:

```typescript
// BAD: Large aggregate (one aggregate for entire Waqf)
class WaqfManagement {
  private endowments: WaqfEndowment[];
  private beneficiaries: Beneficiary[];
  private properties: Property[];
  private financialTransactions: Transaction[];
  private managers: Manager[];
  private auditRecords: AuditRecord[];

  // Too many responsibilities, too large
}

// GOOD: Small, focused aggregates
class WaqfEndowment {
  // Aggregate root
  private readonly id: WaqfId;
  private status: EndowmentStatus;
  private principal: Money;

  // Only references to other aggregates
  private beneficiaryIds: BeneficiaryId[];
  private propertyIds: PropertyId[];
}

class Beneficiary {
  // Separate aggregate
  private readonly id: BeneficiaryId;
  private name: string;
  private percentage: number;
  private status: BeneficiaryStatus;
}

class Property {
  // Separate aggregate
  private readonly id: PropertyId;
  private address: Address;
  private valuation: Money;
  private rentalIncome: Money;
}
```

**Islamic Finance Example**:

**Zakat Bounded Context - Small Aggregates**:

```typescript
// Aggregate 1: ZakatCalculation (focused on calculation)
class ZakatCalculation {
  private readonly id: ZakatCalculationId;
  private assets: ZakatableAsset[]; // Value objects, not entities
  private nisabThreshold: NisabThreshold;
  private hawlYear: HijriYear;

  // Reference to payer, not embedded
  private readonly payerId: PayerId;

  calculateZakat(): Money {
    // Focused responsibility
  }
}

// Aggregate 2: ZakatPayment (focused on payment)
class ZakatPayment {
  private readonly id: ZakatPaymentId;
  private amount: Money;
  private status: PaymentStatus;

  // References to other aggregates
  private readonly calculationId: ZakatCalculationId;
  private readonly recipientId: RecipientId;

  processPayment(): void {
    // Focused responsibility
  }
}

// Aggregate 3: ZakatRecipient (focused on recipient management)
class ZakatRecipient {
  private readonly id: RecipientId;
  private category: RecipientCategory; // One of eight categories
  private verificationStatus: VerificationStatus;

  verifyEligibility(): void {
    // Focused responsibility
  }
}
```

**When to Apply**: Always, when designing aggregates and defining consistency boundaries.

**Red Flags**:

- Aggregates with 10+ entity collections
- Performance issues due to aggregate loading
- Frequent concurrency conflicts

## Modeling Best Practices

### Practice 11: Distinguish Entities from Value Objects

**What**: Use entities for objects with identity and lifecycle. Use value objects for descriptive characteristics without identity.

**Why**: This distinction clarifies model semantics, reduces bugs (immutability), and improves performance (value object sharing).

**How**:

```typescript
// ENTITY: Has identity, mutable lifecycle
class ZakatPayer {
  private readonly id: PayerId; // Identity
  private name: string;
  private email: Email;
  private registrationDate: Date;

  // Same person even if name changes
  updateName(newName: string): void {
    this.name = newName;
  }

  equals(other: ZakatPayer): boolean {
    return this.id.equals(other.id); // Identity-based equality
  }
}

// VALUE OBJECT: No identity, immutable
class Email {
  private readonly value: string;

  private constructor(value: string) {
    if (!this.isValid(value)) {
      throw new InvalidEmailError(value);
    }
    this.value = value;
  }

  static of(value: string): Email {
    return new Email(value);
  }

  // Value equality
  equals(other: Email): boolean {
    return this.value === other.value;
  }

  // No setters - immutable
}
```

**Islamic Finance Example**:

**Entities vs Value Objects in Murabaha**:

```typescript
// ENTITY: MurabahaContract (has identity and lifecycle)
class MurabahaContract {
  private readonly id: ContractId; // Identity
  private status: ContractStatus; // Changes over time
  private signingDate?: Date; // Set later

  // Same contract even if status changes
  sign(signature: Signature): void {
    this.status = ContractStatus.Signed;
    this.signingDate = new Date();
  }
}

// VALUE OBJECT: InstallmentPlan (describes payment structure)
class InstallmentPlan {
  private readonly installments: Installment[];
  private readonly frequency: PaymentFrequency;

  // No identity - two plans with same installments are identical
  equals(other: InstallmentPlan): boolean {
    return this.installments.every((inst, index) => inst.equals(other.installments[index]));
  }

  // Immutable - returns new plan
  adjustForEarlyPayment(): InstallmentPlan {
    return new InstallmentPlan(/* adjusted installments */);
  }
}

// VALUE OBJECT: Markup (describes pricing)
class Markup {
  private readonly percentage: Percentage;
  private readonly basis: MarkupBasis; // Cost-plus, fixed amount

  applyTo(cost: Money): Money {
    return cost.add(cost.multiply(this.percentage));
  }
}
```

**When to Apply**: During modeling, when deciding whether an object needs an ID.

**Red Flags**:

- Value objects with database IDs
- Entities compared by value instead of ID
- Mutable value objects

### Practice 12: Model True Domain Invariants

**What**: Identify and enforce true business invariants that must always hold true, not just validation rules.

**Why**: True invariants represent core business rules that protect business integrity. They must never be violated.

**How**:

```typescript
// Not an invariant - just validation
class User {
  setEmail(email: string): void {
    if (!email.includes("@")) {
      throw new Error("Invalid email"); // Validation, not invariant
    }
    this.email = email;
  }
}

// TRUE INVARIANT - business rule
class WaqfEndowment {
  private principal: Money;
  private distributed: Money;

  // INVARIANT: Principal must never decrease (perpetuity)
  distributeIncome(amount: Money): void {
    const availableIncome = this.calculateIncome();

    if (amount.isGreaterThan(availableIncome)) {
      throw new InvariantViolationError("Cannot distribute more than income - principal must remain intact");
    }

    this.distributed = this.distributed.add(amount);
  }

  // This method should not exist - violates invariant
  withdrawPrincipal(amount: Money): void {
    throw new InvariantViolationError("Waqf principal is perpetual and cannot be withdrawn");
  }
}
```

**Islamic Finance Example**:

**True Invariants in Zakat Domain**:

```typescript
class ZakatCalculation {
  // INVARIANT: Zakat rate is always 2.5% (divine mandate)
  private static readonly ZAKAT_RATE = 0.025;

  calculateZakat(): Money {
    // Cannot be changed - it's Shariah law
    return this.zakatableAmount.multiply(ZakatCalculation.ZAKAT_RATE);
  }

  // INVARIANT: Hawl (lunar year) must be complete
  private verifyHawlCompleted(currentDate: HijriDate): void {
    const daysPassed = currentDate.daysSince(this.hawlStartDate);

    if (daysPassed < 354) {
      // Lunar year
      throw new InvariantViolationError(`Hawl not completed: ${daysPassed}/354 days`);
    }
  }

  // INVARIANT: Only zakatable assets count
  addAsset(asset: Asset): void {
    if (!asset.isZakatable()) {
      throw new InvariantViolationError(`${asset.type} is not zakatable`);
    }
    this.assets.push(asset);
  }
}

// INVARIANT: Murabaha markup must be fixed (not variable interest)
class MurabahaContract {
  setMarkup(markup: Markup): void {
    if (markup.isVariable()) {
      throw new InvariantViolationError("Murabaha requires fixed markup - variable rates prohibited (riba)");
    }
    this.markup = markup;
  }
}
```

**When to Apply**: During domain modeling, when identifying core business rules.

**Red Flags**:

- Treating all validation as invariants
- Invariants enforced in UI or application layer only
- Inconsistent enforcement of business rules

## Collaboration Best Practices

### Practice 13: Include Domain Experts Throughout

**What**: Maintain continuous collaboration with domain experts, not just during initial requirements gathering.

**Why**: Developers alone cannot understand complex domains. Domain experts provide the knowledge needed to model the domain accurately.

**How**:

```markdown
## Collaboration Cadence for Zakat Module

**Weekly Domain Sessions** (1 hour):

- Review: Code from previous week
- Model: New scenarios discovered
- Validate: Ubiquitous Language consistency
- Attendees: Shariah scholar, senior developer, product owner

**Event Storming Workshops** (4 hours, monthly):

- Explore: New subdomains
- Discover: Hidden complexity
- Map: Domain events and aggregates
- Attendees: Full team + domain experts

**Code Walkthroughs** (30 mins, bi-weekly):

- Expert: Reviews code for business logic correctness
- Developer: Explains implementation
- Outcome: Refinements to model
```

**Islamic Finance Example**:

**Domain Expert Involvement in Murabaha Implementation**:

**Week 1: Initial Modeling**

- **Domain Expert**: "The bank must take possession (qabdh) before selling to client"
- **Developer**: "So we need a `PossessionTaken` event before `ContractDrafted`?"
- **Expert**: "Yes, and possession can be constructive (not physical)"

**Week 3: Edge Case Discovery**

- **Developer**: "What if the asset is damaged while in bank's possession?"
- **Expert**: "Bank bears the risk until ownership transfers. We need to model this"
- **Outcome**: Added `AssetDamaged` event and `RiskBearing` aggregate

**Week 5: Validation**

- **Expert**: Reviews code for `MurabahaContract`
- **Expert**: "This allows variable markup - that's riba! Must be fixed"
- **Developer**: Adds invariant check for fixed markup

**When to Apply**: Throughout entire development lifecycle, not just at the beginning.

**Red Flags**:

- Developers making domain decisions alone
- Domain experts only involved in requirements phase
- Developers unable to explain domain concepts in business terms

### Practice 14: Create a Glossary of Domain Terms

**What**: Maintain a living document that defines all domain terms with their meanings, Arabic terms, and Shariah rulings.

**Why**: A glossary ensures consistent understanding across the team and serves as a reference for new team members.

**How**:

```markdown
# Zakat Management Domain Glossary

## Terms

### Nisab (نصاب)

**Definition**: Minimum threshold of wealth that makes Zakat obligatory
**Calculation**: 85 grams of gold OR 595 grams of silver
**Code**: `NisabThreshold` value object
**Shariah Ruling**: Must be maintained for full lunar year (hawl)

### Hawl (حول)

**Definition**: Lunar year (354 days) during which wealth is maintained
**Calculation**: From first day nisab is met until Zakat calculation
**Code**: `HijriYear` value object
**Shariah Ruling**: Wealth must remain above nisab for entire period

### Zakatable Assets (أموال زكوية)

**Definition**: Assets subject to Zakat calculation
**Categories**:

- Cash and savings
- Gold and silver
- Business inventory (عروض التجارة)
- Agricultural produce (زكاة الزروع)
  **Code**: `ZakatableAsset` value object with `AssetCategory` enum
  **Exclusions**: Personal use items, tools of trade

### Zakat Recipients (مستحقو الزكاة)

**Definition**: Eight categories eligible to receive Zakat
**Categories**:

1. Fuqara (الفقراء) - The poor
2. Masakin (المساكين) - The needy
3. Amilin (العاملون عليها) - Zakat administrators
4. Muallaf (المؤلفة قلوبهم) - New Muslims
5. Riqab (الرقاب) - Freeing slaves
6. Gharimin (الغارمون) - Those in debt
7. Fi Sabilillah (في سبيل الله) - In the path of Allah
8. Ibn Sabil (ابن السبيل) - Stranded travelers
   **Code**: `RecipientCategory` enum
   **Shariah Ruling**: Cannot give to non-eligible categories
```

**When to Apply**: From day one, updated continuously as understanding deepens.

**Red Flags**:

- No shared vocabulary between developers and experts
- Multiple terms for the same concept
- Terms used inconsistently across codebase

## Implementation Best Practices

### Practice 15: Apply Layered Architecture

**What**: Organize code into distinct layers: Presentation, Application, Domain, and Infrastructure. Keep domain layer isolated from external concerns.

**Why**: Layered architecture protects domain logic from framework dependencies, making it easier to test and maintain.

**How**:

```
Layered Architecture for Zakat Service:

┌─────────────────────────────────────────────────┐
│         Presentation Layer                      │
│  - REST Controllers                             │
│  - GraphQL Resolvers                            │
│  - DTOs (Data Transfer Objects)                 │
└─────────────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│         Application Layer                       │
│  - Use Cases / Command Handlers                 │
│  - Application Services                         │
│  - DTOs → Domain Model mapping                  │
└─────────────────────────────────────────────────┘
                     ↓
┌─────────────────────────────────────────────────┐
│         Domain Layer (Pure Business Logic)      │
│  - Aggregates (ZakatCalculation)                │
│  - Entities (ZakatPayer)                        │
│  - Value Objects (Money, HijriDate)             │
│  - Domain Events                                │
│  - Domain Services                              │
│  - Repository Interfaces                        │
└─────────────────────────────────────────────────┘
                     ↑
┌─────────────────────────────────────────────────┐
│         Infrastructure Layer                    │
│  - Repository Implementations                   │
│  - ORM Mappings (TypeORM, Prisma)               │
│  - External APIs                                │
│  - Message Brokers (RabbitMQ, Kafka)            │
└─────────────────────────────────────────────────┘
```

**Islamic Finance Example**:

**Zakat Calculation Use Case**:

```typescript
// DOMAIN LAYER (pure business logic)
class ZakatCalculation {
  calculateZakat(currentDate: HijriDate): Money {
    // Pure domain logic, no infrastructure
    this.verifyHawlCompleted(currentDate);
    const zakatableAmount = this.calculateZakatableAmount();
    return zakatableAmount.multiply(0.025);
  }
}

// APPLICATION LAYER (orchestration)
class CalculateZakatUseCase {
  constructor(
    private zakatRepository: ZakatRepository,
    private eventBus: EventBus,
  ) {}

  async execute(command: CalculateZakatCommand): Promise<ZakatCalculationResult> {
    // Load aggregate
    const calculation = await this.zakatRepository.findById(command.calculationId);

    // Execute domain logic
    const zakatDue = calculation.calculateZakat(HijriDate.today());

    // Save and publish events
    await this.zakatRepository.save(calculation);
    await this.eventBus.publishAll(calculation.getUncommittedEvents());

    // Return DTO
    return new ZakatCalculationResult(calculation.id, zakatDue);
  }
}

// PRESENTATION LAYER (HTTP API)
@Controller("/api/zakat")
class ZakatController {
  constructor(private calculateZakat: CalculateZakatUseCase) {}

  @Post("/calculate")
  async calculate(@Body() request: CalculateZakatRequest) {
    // Convert DTO to command
    const command = new CalculateZakatCommand(request.calculationId);

    // Execute use case
    const result = await this.calculateZakat.execute(command);

    // Return HTTP response
    return {
      calculationId: result.calculationId,
      zakatDue: result.zakatDue.toString(),
    };
  }
}

// INFRASTRUCTURE LAYER (persistence)
class PostgresZakatRepository implements ZakatRepository {
  async findById(id: string): Promise<ZakatCalculation> {
    const record = await this.db.query("SELECT * FROM zakat_calculations WHERE id = $1", [id]);
    return this.toDomain(record);
  }

  async save(calculation: ZakatCalculation): Promise<void> {
    const data = this.toDatabase(calculation);
    await this.db.query("INSERT INTO zakat_calculations (...) VALUES (...)", data);
  }
}
```

**When to Apply**: Always, as foundational architecture.

**Red Flags**:

- Domain layer importing infrastructure libraries
- Business logic in controllers
- Database entities used as domain models

### Practice 16: Use Eventual Consistency Between Aggregates

**What**: Accept eventual consistency between aggregates instead of trying to maintain strong consistency through large transactions.

**Why**: Eventual consistency enables scalability, avoids distributed transactions, and aligns with real-world business processes.

**How**:

```typescript
// BAD: Strong consistency across aggregates (large transaction)
class ZakatService {
  @Transactional() // Large transaction
  async processZakatPayment(paymentId: string): Promise<void> {
    const payment = await this.paymentRepo.findById(paymentId);
    const calculation = await this.calculationRepo.findById(payment.calculationId);
    const recipient = await this.recipientRepo.findById(payment.recipientId);

    // All in one transaction - tight coupling
    payment.process();
    calculation.markAsPaid();
    recipient.recordReceived(payment.amount);

    await this.paymentRepo.save(payment);
    await this.calculationRepo.save(calculation);
    await this.recipientRepo.save(recipient);
  }
}

// GOOD: Eventual consistency (events)
class ZakatPayment {
  process(): void {
    this.status = PaymentStatus.Processed;

    // Publish event - eventual consistency
    this.recordEvent(new ZakatPaymentProcessed(this.id, this.calculationId, this.recipientId, this.amount));
  }
}

// Separate bounded contexts react to event
@EventHandler(ZakatPaymentProcessed)
class CalculationEventHandler {
  async onPaymentProcessed(event: ZakatPaymentProcessed): Promise<void> {
    const calculation = await this.repo.findById(event.calculationId);
    calculation.markAsPaid();
    await this.repo.save(calculation);
  }
}

@EventHandler(ZakatPaymentProcessed)
class RecipientEventHandler {
  async onPaymentProcessed(event: ZakatPaymentProcessed): Promise<void> {
    const recipient = await this.repo.findById(event.recipientId);
    recipient.recordReceived(event.amount);
    await this.repo.save(recipient);
  }
}
```

**Islamic Finance Example**:

**Waqf Distribution Process (Eventual Consistency)**:

```typescript
// Step 1: Distribute income (immediate consistency within aggregate)
class WaqfEndowment {
  distributeIncome(distributions: Distribution[]): void {
    // Strong consistency within this aggregate
    distributions.forEach((d) => {
      this.income = this.income.subtract(d.amount);
      this.totalDistributed = this.totalDistributed.add(d.amount);
    });

    // Publish event for other contexts
    this.recordEvent(new IncomeDistributed(this.id, distributions));
  }
}

// Step 2: Update beneficiary records (eventual consistency)
@EventHandler(IncomeDistributed)
class BeneficiaryEventHandler {
  async onIncomeDistributed(event: IncomeDistributed): Promise<void> {
    for (const distribution of event.distributions) {
      const beneficiary = await this.repo.findById(distribution.beneficiaryId);

      // Eventually consistent
      beneficiary.recordPaymentReceived(distribution.amount);
      await this.repo.save(beneficiary);
    }
  }
}

// Step 3: Create accounting entries (eventual consistency)
@EventHandler(IncomeDistributed)
class AccountingEventHandler {
  async onIncomeDistributed(event: IncomeDistributed): Promise<void> {
    // Eventually consistent
    await this.ledger.createJournalEntry({
      debit: { account: "Beneficiary Distributions", amount: event.totalAmount },
      credit: { account: "Waqf Income", amount: event.totalAmount },
    });
  }
}
```

**When to Apply**: Between aggregates, across bounded contexts, in distributed systems.

**Red Flags**:

- Large transactions spanning multiple aggregates
- Distributed transactions (2PC)
- Inability to tolerate slight delays in consistency

## Islamic Finance Examples

### Example 1: Zakat Calculation Domain

**Bounded Context**: Zakat Management

**Ubiquitous Language**:

- Nisab (نصاب) - Minimum threshold
- Hawl (حول) - Lunar year
- Zakatable Assets (أموال زكوية) - Assets subject to Zakat
- Eight Categories (الأصناف الثمانية) - Eligible recipients

**Aggregates**:

```typescript
class ZakatCalculation {
  // Invariants
  private verifyHawlCompleted(): void {
    /* Must complete lunar year */
  }
  private verifyNisabMet(): void {
    /* Must meet threshold */
  }

  // Domain events
  calculateZakat(): void {
    this.recordEvent(new ZakatCalculated(/* ... */));
  }
}
```

**Best Practices Applied**:

- Rich domain model (calculation logic in aggregate)
- Value objects (Money, HijriDate, NisabThreshold)
- Domain events for cross-context communication
- Ubiquitous language from Shariah terminology

**Antipatterns Avoided**:

- Not anemic (logic lives in aggregate, not service)
- Not database-driven (domain-first design)
- Not missing experts (Shariah scholar involved)

## Summary

**Best Practices**:

| Category       | Practice                                    | Key Benefit                    |
| -------------- | ------------------------------------------- | ------------------------------ |
| Strategic      | Start with business outcomes                | Focus effort on value delivery |
| Strategic      | Establish ubiquitous language early         | Eliminate translation errors   |
| Strategic      | Use bounded contexts for clear boundaries   | Reduce coupling                |
| Strategic      | Align contexts with microservices           | Enable team autonomy           |
| Strategic      | Use event storming for discovery            | Reveal hidden complexity       |
| Tactical       | Design rich domain models                   | Centralize business logic      |
| Tactical       | Protect aggregate invariants                | Ensure business rules hold     |
| Tactical       | Use value objects for immutable concepts    | Reduce bugs via immutability   |
| Tactical       | Use domain events for decoupling            | Enable eventual consistency    |
| Tactical       | Keep aggregates small                       | Improve scalability            |
| Modeling       | Distinguish entities from value objects     | Clarify semantics              |
| Modeling       | Model true domain invariants                | Protect business integrity     |
| Collaboration  | Include domain experts throughout           | Accurate domain knowledge      |
| Collaboration  | Create glossary of domain terms             | Consistent understanding       |
| Implementation | Apply layered architecture                  | Isolate domain logic           |
| Implementation | Use eventual consistency between aggregates | Enable distributed systems     |

**Key Takeaway**: DDD is most successful when strategic design (understanding the business) and tactical patterns (implementing the code) work together, with continuous collaboration between developers and domain experts.

## References

- [Microsoft: Introduction to Domain-Driven Design](https://learn.microsoft.com/en-us/archive/msdn-magazine/2009/february/best-practice-an-introduction-to-domain-driven-design) - Best practices for DDD implementation
- [DDD Anti-Patterns (Alok Mishra)](https://alok-mishra.com/2021/11/03/ddd-anti-patterns/) - Common mistakes in DDD
- [Martin Fowler: Anemic Domain Model](https://martinfowler.com/bliki/AnemicDomainModel.html) - Classic antipattern explanation
- [Medium: Common DDD Mistakes](https://medium.com/navalia/the-most-common-domain-driven-design-mistake-6c3f90e0ec2b) - Practical antipatterns

## Related Documentation

- [ex-so-ar-dodrdedd\_\_01-introduction-and-philosophy.md](./ex-so-ar-dodrdedd__01-introduction-and-philosophy.md) - DDD introduction and core concepts
- [ex-so-ar-dodrdedd\_\_03-bounded-contexts.md](./ex-so-ar-dodrdedd__03-bounded-contexts.md) - Strategic design patterns
- [ex-so-ar-dodrdedd\_\_09-aggregates.md](./ex-so-ar-dodrdedd__09-aggregates.md) - Tactical design patterns
- [ex-so-ar-dodrdedd\_\_20-antipatterns.md](./ex-so-ar-dodrdedd__20-antipatterns.md) - Common mistakes to avoid
