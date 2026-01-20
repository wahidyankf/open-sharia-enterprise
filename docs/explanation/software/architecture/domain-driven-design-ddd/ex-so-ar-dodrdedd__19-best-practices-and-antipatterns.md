# Best Practices and Antipatterns in Domain-Driven Design

## Overview

Domain-Driven Design (DDD) is a powerful approach for tackling complex business domains, but it requires careful application to avoid common pitfalls. This guide explores proven best practices and frequent antipatterns, with examples from Islamic finance and Shariah-compliant business systems.

Success in DDD comes from balancing strategic design (understanding the business domain) with tactical patterns (implementing the code), while maintaining close collaboration with domain experts and avoiding over-engineering.

## Table of Contents

- [Strategic Design Best Practices](#strategic-design-best-practices)
- [Tactical Design Best Practices](#tactical-design-best-practices)
- [Modeling Best Practices](#modeling-best-practices)
- [Collaboration Best Practices](#collaboration-best-practices)
- [Implementation Best Practices](#implementation-best-practices)
- [Common Antipatterns](#common-antipatterns)
- [Strategic Antipatterns](#strategic-antipatterns)
- [Tactical Antipatterns](#tactical-antipatterns)
- [Organizational Antipatterns](#organizational-antipatterns)
- [Islamic Finance Examples](#islamic-finance-examples)

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

## Common Antipatterns

### Antipattern 1: Anemic Domain Model

**What**: Domain objects that are just data bags with getters/setters, with all business logic in service classes.

**Why It's Bad**: Violates encapsulation, scatters business logic, makes code harder to understand and maintain.

**Example**:

```typescript
// ANTIPATTERN: Anemic model
class ZakatCalculation {
  public assets: Asset[];
  public nisabThreshold: number;
  public zakatAmount: number;

  // Just getters/setters, no behavior
  getAssets() {
    return this.assets;
  }
  setAssets(assets: Asset[]) {
    this.assets = assets;
  }
}

class ZakatService {
  // All logic in service
  calculateZakat(calculation: ZakatCalculation): number {
    const total = calculation.getAssets().reduce((sum, a) => sum + a.value, 0);

    if (total < calculation.nisabThreshold) {
      return 0;
    }

    return total * 0.025;
  }
}

// CORRECT: Rich domain model
class ZakatCalculation {
  private assets: Asset[];
  private nisabThreshold: Money;

  calculateZakat(): Money {
    // Business logic lives in domain model
    const zakatableAmount = this.calculateZakatableAmount();

    if (!this.nisabThreshold.isMet(zakatableAmount)) {
      return Money.zero();
    }

    return zakatableAmount.multiply(0.025);
  }

  private calculateZakatableAmount(): Money {
    return this.assets.filter((a) => a.isZakatable()).reduce((sum, a) => sum.add(a.value()), Money.zero());
  }
}
```

**How to Fix**: Move business logic from services into domain objects. Services should orchestrate, not implement business rules.

**Warning Signs**:

- Classes with only getters/setters
- Fat service classes with hundreds of lines
- Business rules scattered across multiple services

### Antipattern 2: Premature Complexity

**What**: Applying full DDD patterns (aggregates, events, CQRS) to simple CRUD applications that don't need them.

**Why It's Bad**: Over-engineering simple problems wastes time, confuses developers, and creates unnecessary maintenance burden.

**Example**:

```typescript
// ANTIPATTERN: DDD for simple CRUD
// Simple user profile management doesn't need aggregates/events

class UserProfile {
  /* Complex aggregate root */
}
class UserProfileCreated implements DomainEvent {
  /* Unnecessary event */
}
class UserProfileRepository {
  /* Over-engineered repository */
}
class CreateUserProfileCommand {
  /* Overkill for simple update */
}

// CORRECT: Simple CRUD for simple problems
class UserProfileService {
  async updateProfile(userId: string, data: ProfileData): Promise<void> {
    await this.db.update("user_profiles", { userId }, data);
  }
}
```

**Islamic Finance Example**:

**When NOT to use DDD**:

- **Simple lookup tables**: Zakat recipient categories (just 8 fixed values)
- **Configuration**: Nisab threshold updates (simple CRUD)
- **Logging**: Audit trail of user actions (write-only, no business logic)

**When to use DDD**:

- **Complex calculations**: Zakat on business inventory (complex rules, formulas, exceptions)
- **Invariants**: Waqf principal preservation (critical business rule)
- **Workflows**: Murabaha contract lifecycle (multiple states, events, validations)

**How to Fix**: Start simple. Only introduce DDD patterns when complexity justifies them.

**Warning Signs**:

- More code for infrastructure than business logic
- Simple CRUD disguised as "aggregates"
- Team struggling to understand basic operations

### Antipattern 3: Ignoring Bounded Contexts

**What**: Attempting to create one unified model for the entire enterprise, forcing all contexts to share the same definitions.

**Why It's Bad**: Different contexts need different models. Forcing unification creates bloated, ambiguous models that serve no one well.

**Example**:

```typescript
// ANTIPATTERN: One "Asset" model for everything
class Asset {
  // Zakat-specific fields
  isZakatable: boolean;
  zakatRate: number;

  // Waqf-specific fields
  isWaqfProperty: boolean;
  rentalIncome: number;

  // Murabaha-specific fields
  isShariaCompliant: boolean;
  purchasePrice: number;
  markup: number;

  // Bloated, confused model
}

// CORRECT: Separate models per bounded context
// Zakat Context
class ZakatableAsset {
  value: Money;
  category: AssetCategory;
  isZakatable(): boolean {
    /* Zakat-specific logic */
  }
}

// Waqf Context
class WaqfProperty {
  valuation: Money;
  rentalIncome: Money;
  generateIncome(): Money {
    /* Waqf-specific logic */
  }
}

// Murabaha Context
class FinancedAsset {
  purchasePrice: Money;
  markup: Percentage;
  isShariaCompliant(): boolean {
    /* Murabaha-specific logic */
  }
}
```

**How to Fix**: Identify bounded contexts early. Allow each context to have its own model with its own vocabulary.

**Warning Signs**:

- Constant debates: "What does X mean here?"
- Models with 50+ fields, most unused in any given context
- Context-specific behavior polluting shared models

### Antipattern 4: Missing Ubiquitous Language

**What**: Developers use technical jargon while domain experts use business terms, with no shared vocabulary.

**Why It's Bad**: Communication gaps lead to misunderstandings, incorrect implementations, and bugs.

**Example**:

```typescript
// ANTIPATTERN: Technical jargon
class TransactionProcessor {
  processPayment(data: PaymentData): void {
    const entity = this.repo.findById(data.id);
    entity.amount = data.amount;
    entity.status = "processed";
    this.repo.save(entity);
  }
}

// Domain expert: "Where is the Zakat distribution logic?"
// Developer: "It's in the transaction processor"
// Expert: "What's a transaction processor?"

// CORRECT: Ubiquitous Language
class ZakatDistributor {
  distributeToRecipient(distribution: ZakatDistribution): void {
    const recipient = this.recipientRepo.findById(distribution.recipientId);

    recipient.receiveZakat(distribution.amount);
    this.recipientRepo.save(recipient);
  }
}

// Domain expert: "Where is the Zakat distribution logic?"
// Developer: "In the ZakatDistributor class"
// Expert: "Perfect, that matches our business process"
```

**How to Fix**: Create a glossary. Use business terms in code. Review code with domain experts.

**Warning Signs**:

- Code reviews where domain experts don't understand variable names
- Developers translating business terms into "technical" equivalents
- Documentation needed to explain what code does in business terms

### Antipattern 5: Not Involving Domain Experts

**What**: Developers implement domain logic based on assumptions, documentation, or limited initial requirements without ongoing expert collaboration.

**Why It's Bad**: Complex domains have subtle rules, exceptions, and edge cases that only experts understand. Missing these creates bugs and incorrect behavior.

**Example**:

```typescript
// ANTIPATTERN: Developer assumptions
class ZakatCalculator {
  calculateZakat(assets: Asset[]): number {
    // Developer assumption: "Zakat is 2.5% of everything"
    const total = assets.reduce((sum, a) => sum + a.value, 0);
    return total * 0.025;
  }
}

// Missing domain knowledge:
// - Not all assets are zakatable
// - Different asset types have different rates
// - Nisab threshold must be met
// - Hawl (lunar year) must be complete
// - Personal use items are excluded

// CORRECT: Domain expert collaboration
class ZakatCalculation {
  calculateZakat(currentDate: HijriDate): Money {
    // Domain expert: "First verify hawl is complete"
    this.verifyHawlCompleted(currentDate);

    // Domain expert: "Only zakatable assets count"
    const zakatableAssets = this.assets.filter((a) => a.isZakatable());

    // Domain expert: "Different categories have different rules"
    const total = this.calculateByCategory(zakatableAssets);

    // Domain expert: "Must meet nisab threshold"
    if (!this.nisabThreshold.isMet(total)) {
      return Money.zero();
    }

    // Domain expert: "Standard rate is 2.5%"
    return total.multiply(0.025);
  }
}
```

**How to Fix**: Schedule regular sessions with domain experts. Review code together. Ask "why" questions constantly.

**Warning Signs**:

- Bugs discovered in production that experts would have caught immediately
- Developers saying "I think this is how it works"
- Domain experts surprised by implementation details

## Strategic Antipatterns

### Antipattern 6: Database-Driven Design

**What**: Starting with database schema design instead of domain modeling. Letting database concerns drive domain model structure.

**Why It's Bad**: Database structure optimizes for storage, not business logic. This leads to anemic models and scattered business rules.

**Example**:

```typescript
// ANTIPATTERN: Database-first
// Start by creating tables
CREATE TABLE zakat_calculations (
  id UUID,
  user_id UUID,
  asset_ids UUID[], -- Array of foreign keys
  total_amount DECIMAL,
  zakat_amount DECIMAL,
  created_at TIMESTAMP
);

// Then create anemic models matching tables
class ZakatCalculation {
  id: string;
  userId: string;
  assetIds: string[];
  totalAmount: number;
  zakatAmount: number;
}

// CORRECT: Domain-first
// Start with domain model
class ZakatCalculation {
  private assets: ZakatableAsset[];
  private nisabThreshold: NisabThreshold;
  private hawlYear: HijriYear;

  calculateZakat(): Money {
    // Rich behavior
  }

  addAsset(asset: ZakatableAsset): void {
    // Enforces invariants
  }
}

// Then map to database (ORM handles this)
@Entity()
class ZakatCalculationEntity {
  @PrimaryColumn()
  id: string;

  @OneToMany(() => AssetEntity)
  assets: AssetEntity[];

  // Infrastructure concern, not domain
}
```

**How to Fix**: Start with domain modeling. Design database schema after domain model is clear.

**Warning Signs**:

- Database ERD created before domain model
- Domain objects matching database tables exactly
- Business logic in stored procedures

### Antipattern 7: Misusing Entities as Bounded Contexts

**What**: Creating a bounded context for each entity instead of identifying true business capabilities.

**Why It's Bad**: Bounded contexts should represent business capabilities (subdomains), not technical entity groupings. This creates artificial boundaries.

**Example**:

```typescript
// ANTIPATTERN: Entity-based contexts
// Wrong: Creating contexts per entity
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│   User       │  │   Asset      │  │   Payment    │
│   Context    │  │   Context    │  │   Context    │
└──────────────┘  └──────────────┘  └──────────────┘

// Results in:
// - No clear business capability
// - Artificial boundaries
// - Unclear ownership

// CORRECT: Business capability-based contexts
┌────────────────────────┐  ┌──────────────────────┐
│  Zakat Management      │  │  Waqf Administration │
│                        │  │                      │
│  Capabilities:         │  │  Capabilities:       │
│  - Calculate Zakat     │  │  - Manage Endowments │
│  - Distribute Funds    │  │  - Track Grants      │
│  - Track Recipients    │  │  - Report Income     │
│                        │  │                      │
│  Entities:             │  │  Entities:           │
│  - ZakatCalculation    │  │  - WaqfEndowment     │
│  - ZakatPayer          │  │  - Beneficiary       │
│  - ZakatRecipient      │  │  - Property          │
└────────────────────────┘  └──────────────────────┘
```

**How to Fix**: Identify business capabilities through domain discovery. Group related capabilities into bounded contexts.

**Warning Signs**:

- One entity per microservice
- Unclear what business problem a context solves
- Context names like "User Service" instead of "Identity Management"

## Tactical Antipatterns

### Antipattern 8: Using ID References Instead of Object References

**What**: Storing only IDs to related entities instead of actual object references within aggregate boundaries.

**Why It's Bad**: Makes code clumsy, loses type safety, and obscures relationships.

**Example**:

```typescript
// ANTIPATTERN: ID references everywhere
class ZakatCalculation {
  payerId: string; // Just ID
  recipientIds: string[]; // Just IDs

  async distributeZakat() {
    // Have to fetch separately
    const payer = await this.payerRepo.findById(this.payerId);
    const recipients = await Promise.all(this.recipientIds.map((id) => this.recipientRepo.findById(id)));
    // Lost aggregate boundaries
  }
}

// CORRECT: Object references within aggregate
class ZakatCalculation {
  private payer: ZakatPayer; // Entity reference
  private distributions: Distribution[]; // Value objects

  distributeZakat(): void {
    // Direct access, type-safe
    const zakatDue = this.calculateZakat();

    this.distributions.forEach((distribution) => {
      distribution.allocateFrom(zakatDue);
    });

    // Clear aggregate boundary
  }
}

// Use IDs only for cross-aggregate references
class ZakatPayment {
  private calculationId: ZakatCalculationId; // Different aggregate
  private recipientId: RecipientId; // Different aggregate
}
```

**How to Fix**: Use object references within aggregates. Use IDs only for cross-aggregate references.

**Warning Signs**:

- Constant repository lookups within business logic
- Lost type safety
- Unclear aggregate boundaries

### Antipattern 9: Confusing Entities and Value Objects

**What**: Making value objects into entities (with IDs) or treating entities as value objects (comparing by value).

**Why It's Bad**: Breaks semantics, creates identity confusion, and causes bugs.

**Example**:

```typescript
// ANTIPATTERN: Money as entity (wrong)
class Money {
  id: string; // Wrong - money has no identity
  amount: number;
  currency: string;
}

// ANTIPATTERN: ZakatPayer as value object (wrong)
class ZakatPayer {
  name: string;

  equals(other: ZakatPayer): boolean {
    return this.name === other.name; // Wrong - comparing by value
  }
}

// If person changes name, they're suddenly a different person?

// CORRECT: Value objects for concepts without identity
class Money {
  private readonly amount: number;
  private readonly currency: Currency;

  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency.equals(other.currency);
  }
}

// CORRECT: Entities for concepts with identity
class ZakatPayer {
  private readonly id: PayerId;
  private name: string;

  equals(other: ZakatPayer): boolean {
    return this.id.equals(other.id); // Identity-based
  }

  changeName(newName: string): void {
    this.name = newName; // Same person, different name
  }
}
```

**How to Fix**: Ask: "If all attributes are the same, are these the same thing?" (Value Object: yes, Entity: no)

**Warning Signs**:

- Value objects with database IDs
- Entities compared by all fields instead of ID
- Confusion about "sameness"

### Antipattern 10: Focusing Only on Tactical Patterns

**What**: Jumping straight to implementing aggregates, value objects, and repositories without understanding strategic design.

**Why It's Bad**: Tactical patterns are tools. Without strategic design (bounded contexts, ubiquitous language), you risk building the wrong thing correctly.

**Example**:

```typescript
// ANTIPATTERN: Perfect tactical implementation of wrong model
// Team spent weeks implementing perfect aggregates...
class Payment {
  // Beautifully crafted aggregate
  // Rich domain model
  // Proper invariants
  // Events published
}

// But missed that "Payment" means different things:
// - Zakat Payment (charitable, tax-deductible, specific recipients)
// - Murabaha Installment (financing, interest-free, asset-backed)
// - Waqf Donation (endowment, principal preserved, generates income)

// Should have been separate bounded contexts!

// CORRECT: Strategic design first
// 1. Identify bounded contexts through event storming
// 2. Define ubiquitous language per context
// 3. Map subdomains
// 4. THEN implement tactical patterns

Bounded Contexts:
- Zakat Management → ZakatPayment aggregate
- Murabaha Financing → InstallmentPayment aggregate
- Waqf Administration → WaqfDonation aggregate
```

**How to Fix**: Always start with strategic design. Tactical patterns come after bounded contexts are identified.

**Warning Signs**:

- Team knows every tactical pattern but unclear on business domain
- Perfect code structure but wrong model
- No discussion of bounded contexts or subdomains

## Organizational Antipatterns

### Antipattern 11: Doing DDD Without Business Buy-In

**What**: Developers decide to "do DDD" without explaining value to business stakeholders or getting their commitment.

**Why It's Bad**: DDD requires domain expert time, which costs money. Without business buy-in, experts won't be available and DDD fails.

**Example**:

```markdown
# ANTIPATTERN: Developer-driven DDD

**Developer**: "We're going to use DDD!"
**Manager**: "What's DDD?"
**Developer**: "Domain-Driven Design, it's a software pattern"
**Manager**: "Will it take longer?"
**Developer**: "Yes, but it's better architecture"
**Manager**: "Just ship the feature"

Result: No domain expert time allocated, DDD becomes just anemic models with fancy names.

# CORRECT: Business-driven DDD

**Developer**: "Zakat calculation has complex rules and frequent errors. Can we get Shariah scholar time?"
**Manager**: "How much time and what's the ROI?"
**Developer**: "4 hours/week for 3 months. ROI: 90% reduction in Shariah compliance bugs, faster feature development after initial investment"
**Manager**: "Approved. What do you need?"
**Developer**: "Weekly sessions with Sheikh Ahmad to model Zakat rules correctly"

Result: Business understands value, allocates resources, DDD succeeds.
```

**How to Fix**: Frame DDD in business terms (risk reduction, faster time-to-market, fewer bugs). Get explicit commitment for domain expert time.

**Warning Signs**:

- DDD as "technical decision" without business involvement
- Domain experts unavailable for collaboration
- DDD abandoned when deadlines approach

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

### Example 2: Murabaha Financing Domain

**Bounded Context**: Murabaha Financing

**Ubiquitous Language**:

- Murabaha (مرابحة) - Cost-plus financing
- Qabdh (قبض) - Possession
- Tamlik (تمليك) - Ownership transfer
- Fixed Markup - Not riba (interest)

**Aggregates**:

```typescript
class MurabahaContract {
  // Invariant: Bank must possess before selling
  draftContract(): void {
    if (!this.possessionTransferred) {
      throw new InvariantViolationError("Asset not possessed");
    }
  }

  // Invariant: Markup must be fixed
  setMarkup(markup: Markup): void {
    if (markup.isVariable()) {
      throw new InvariantViolationError("Riba prohibited");
    }
  }
}
```

**Best Practices Applied**:

- Event storming to discover workflow
- Bounded context aligned with microservice
- Domain events (AssetPurchased, PossessionTaken, ContractSigned)
- Eventual consistency with Accounting context

**Antipatterns Avoided**:

- Not premature complexity (complex domain justifies DDD)
- Not ignoring bounded contexts (separate from Zakat, Waqf)
- Not missing ubiquitous language (Shariah terms in code)

## Summary

**Best Practices**:

1. Start with business outcomes
2. Establish ubiquitous language early
3. Use bounded contexts for clear boundaries
4. Align contexts with microservices
5. Use event storming for discovery
6. Design rich domain models
7. Protect aggregate invariants
8. Use value objects for immutable concepts
9. Use domain events for decoupling
10. Keep aggregates small
11. Distinguish entities from value objects
12. Model true domain invariants
13. Include domain experts throughout
14. Create glossary of domain terms
15. Apply layered architecture
16. Use eventual consistency between aggregates

**Antipatterns to Avoid**:

1. Anemic domain model
2. Premature complexity
3. Ignoring bounded contexts
4. Missing ubiquitous language
5. Not involving domain experts
6. Database-driven design
7. Misusing entities as bounded contexts
8. Using ID references instead of object references
9. Confusing entities and value objects
10. Focusing only on tactical patterns
11. Doing DDD without business buy-in

**Key Takeaway**: DDD is most successful when strategic design (understanding the business) and tactical patterns (implementing the code) work together, with continuous collaboration between developers and domain experts.

## References

- [Microsoft: Introduction to Domain-Driven Design](https://learn.microsoft.com/en-us/archive/msdn-magazine/2009/february/best-practice-an-introduction-to-domain-driven-design) - Best practices for DDD implementation
- [DDD Anti-Patterns (Alok Mishra)](https://alok-mishra.com/2021/11/03/ddd-anti-patterns/) - Common mistakes in DDD
- [Martin Fowler: Anemic Domain Model](https://martinfowler.com/bliki/AnemicDomainModel.html) - Classic antipattern explanation
- [Medium: Common DDD Mistakes](https://medium.com/navalia/the-most-common-domain-driven-design-mistake-6c3f90e0ec2b) - Practical antipatterns

## Related Documentation

- [ex-so-ar-dodrdedd\_\_01-what-is-ddd.md](./ex-so-ar-dodrdedd__01-introduction-and-philosophy.md) - DDD introduction and core concepts
- [ex-so-ar-dodrdedd\_\_17-bounded-contexts.md](./ex-so-ar-dodrdedd__03-bounded-contexts.md) - Strategic design patterns
- [ex-so-ar-dodrdedd\_\_18-aggregates-entities-value-objects.md](./ex-so-ar-dodrdedd__09-aggregates.md) - Tactical design patterns
