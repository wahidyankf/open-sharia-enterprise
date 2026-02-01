# Antipatterns in Domain-Driven Design

> **Companion Document**: This guide focuses on common mistakes to avoid in DDD. For recommended practices, see [Best Practices in Domain-Driven Design](./ex-soen-ar-dodrdedd__19-best-practices.md).

## Overview

Domain-Driven Design (DDD) is a powerful approach, but its misapplication can lead to serious problems: over-engineered systems that are hard to maintain, anemic models that scatter business logic, or unified models that serve no context well. Understanding antipatterns is crucial for successful DDD implementation.

This guide explores frequent antipatterns across four categories: common mistakes that cross all areas, strategic design errors, tactical implementation problems, and organizational failures. Each antipattern includes Islamic finance examples to illustrate real-world consequences and provide clear guidance on what to avoid.

The cost of these antipatterns is significant: wasted development time, maintenance burden, business logic bugs, and failed DDD initiatives. By recognizing these patterns early, teams can avoid expensive mistakes and build systems that truly serve their business domains.

## Table of Contents

- [Common Antipatterns](#common-antipatterns)
- [Strategic Antipatterns](#strategic-antipatterns)
- [Tactical Antipatterns](#tactical-antipatterns)
- [Organizational Antipatterns](#organizational-antipatterns)
- [Islamic Finance Examples](#islamic-finance-examples)
- [Summary](#summary)

## Common Antipatterns

### Antipattern 1: Anemic Domain Model

**What**: Domain objects that are just data bags with getters/setters, with all business logic in service classes.

**Why It's Bad**: Violates encapsulation, scatters business logic, makes code harder to understand and maintain.

**Example**:

```typescript
// ANTIPATTERN: Anemic model
class TaxCalculation {
  public assets: Asset[];
  public thresholdThreshold: number;
  public taxAmount: number;

  // Just getters/setters, no behavior
  getAssets() {
    return this.assets;
  }
  setAssets(assets: Asset[]) {
    this.assets = assets;
  }
}

class TaxService {
  // All logic in service
  calculateTax(calculation: TaxCalculation): number {
    const total = calculation.getAssets().reduce((sum, a) => sum + a.value, 0);

    if (total < calculation.thresholdThreshold) {
      return 0;
    }

    return total * 0.025;
  }
}

// CORRECT: Rich domain model
class TaxCalculation {
  private assets: Asset[];
  private thresholdThreshold: Money;

  calculateTax(): Money {
    // Business logic lives in domain model
    const taxableAmount = this.calculateTaxableAmount();

    if (!this.thresholdThreshold.isMet(taxableAmount)) {
      return Money.zero();
    }

    return taxableAmount.multiply(0.025);
  }

  private calculateTaxableAmount(): Money {
    return this.assets.filter((a) => a.isTaxable()).reduce((sum, a) => sum.add(a.value()), Money.zero());
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

- **Simple lookup tables**: Tax recipient categories (just 8 fixed values)
- **Configuration**: Threshold threshold updates (simple CRUD)
- **Logging**: Audit trail of user actions (write-only, no business logic)

**When to use DDD**:

- **Complex calculations**: Tax on business inventory (complex rules, formulas, exceptions)
- **Invariants**: Donation principal preservation (critical business rule)
- **Workflows**: Loan contract lifecycle (multiple states, events, validations)

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
  // Tax-specific fields
  isTaxable: boolean;
  taxRate: number;

  // Donation-specific fields
  isDonationProperty: boolean;
  rentalIncome: number;

  // Loan-specific fields
  isComplianceCompliant: boolean;
  purchasePrice: number;
  markup: number;

  // Bloated, confused model
}

// CORRECT: Separate models per bounded context
// Tax Context
class TaxableAsset {
  value: Money;
  category: AssetCategory;
  isTaxable(): boolean {
    /* Tax-specific logic */
  }
}

// Donation Context
class DonationProperty {
  valuation: Money;
  rentalIncome: Money;
  generateIncome(): Money {
    /* Donation-specific logic */
  }
}

// Loan Context
class FinancedAsset {
  purchasePrice: Money;
  markup: Percentage;
  isComplianceCompliant(): boolean {
    /* Loan-specific logic */
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

// Domain expert: "Where is the Tax distribution logic?"
// Developer: "It's in the transaction processor"
// Expert: "What's a transaction processor?"

// CORRECT: Ubiquitous Language
class TaxDistributor {
  distributeToRecipient(distribution: TaxDistribution): void {
    const recipient = this.recipientRepo.findById(distribution.recipientId);

    recipient.receiveTax(distribution.amount);
    this.recipientRepo.save(recipient);
  }
}

// Domain expert: "Where is the Tax distribution logic?"
// Developer: "In the TaxDistributor class"
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
class TaxCalculator {
  calculateTax(assets: Asset[]): number {
    // Developer assumption: "Tax is 2.5% of everything"
    const total = assets.reduce((sum, a) => sum + a.value, 0);
    return total * 0.025;
  }
}

// Missing domain knowledge:
// - Not all assets are taxable
// - Different asset types have different rates
// - Threshold threshold must be met
// - Hawl (lunar year) must be complete
// - Personal use items are excluded

// CORRECT: Domain expert collaboration
class TaxCalculation {
  calculateTax(currentDate: HijriDate): Money {
    // Domain expert: "First verify hawl is complete"
    this.verifyHawlCompleted(currentDate);

    // Domain expert: "Only taxable assets count"
    const taxableAssets = this.assets.filter((a) => a.isTaxable());

    // Domain expert: "Different categories have different rules"
    const total = this.calculateByCategory(taxableAssets);

    // Domain expert: "Must meet threshold threshold"
    if (!this.thresholdThreshold.isMet(total)) {
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
CREATE TABLE tax_calculations (
  id UUID,
  user_id UUID,
  asset_ids UUID[], -- Array of foreign keys
  total_amount DECIMAL,
  tax_amount DECIMAL,
  created_at TIMESTAMP
);

// Then create anemic models matching tables
class TaxCalculation {
  id: string;
  userId: string;
  assetIds: string[];
  totalAmount: number;
  taxAmount: number;
}

// CORRECT: Domain-first
// Start with domain model
class TaxCalculation {
  private assets: TaxableAsset[];
  private thresholdThreshold: ThresholdThreshold;
  private hawlYear: HijriYear;

  calculateTax(): Money {
    // Rich behavior
  }

  addAsset(asset: TaxableAsset): void {
    // Enforces invariants
  }
}

// Then map to database (ORM handles this)
@Entity()
class TaxCalculationEntity {
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
│  Tax Management      │  │  Donation Administration │
│                        │  │                      │
│  Capabilities:         │  │  Capabilities:       │
│  - Calculate Tax     │  │  - Manage Endowments │
│  - Distribute Funds    │  │  - Track Grants      │
│  - Track Recipients    │  │  - Report Income     │
│                        │  │                      │
│  Entities:             │  │  Entities:           │
│  - TaxCalculation    │  │  - DonationEndowment     │
│  - TaxPayer          │  │  - Beneficiary       │
│  - TaxRecipient      │  │  - Property          │
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
class TaxCalculation {
  payerId: string; // Just ID
  recipientIds: string[]; // Just IDs

  async distributeTax() {
    // Have to fetch separately
    const payer = await this.payerRepo.findById(this.payerId);
    const recipients = await Promise.all(this.recipientIds.map((id) => this.recipientRepo.findById(id)));
    // Lost aggregate boundaries
  }
}

// CORRECT: Object references within aggregate
class TaxCalculation {
  private payer: TaxPayer; // Entity reference
  private distributions: Distribution[]; // Value objects

  distributeTax(): void {
    // Direct access, type-safe
    const taxDue = this.calculateTax();

    this.distributions.forEach((distribution) => {
      distribution.allocateFrom(taxDue);
    });

    // Clear aggregate boundary
  }
}

// Use IDs only for cross-aggregate references
class TaxPayment {
  private calculationId: TaxCalculationId; // Different aggregate
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

// ANTIPATTERN: TaxPayer as value object (wrong)
class TaxPayer {
  name: string;

  equals(other: TaxPayer): boolean {
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
class TaxPayer {
  private readonly id: PayerId;
  private name: string;

  equals(other: TaxPayer): boolean {
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
// - Tax Payment (charitable, tax-deductible, specific recipients)
// - Loan Installment (financing, interest-free, asset-backed)
// - Donation Donation (endowment, principal preserved, generates income)

// Should have been separate bounded contexts!

// CORRECT: Strategic design first
// 1. Identify bounded contexts through event storming
// 2. Define ubiquitous language per context
// 3. Map subdomains
// 4. THEN implement tactical patterns

Bounded Contexts:
- Tax Management → TaxPayment aggregate
- Loan Financing → InstallmentPayment aggregate
- Donation Administration → DonationDonation aggregate
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

**Developer**: "Tax calculation has complex rules and frequent errors. Can we get Compliance scholar time?"
**Manager**: "How much time and what's the ROI?"
**Developer**: "4 hours/week for 3 months. ROI: 90% reduction in Compliance compliance bugs, faster feature development after initial investment"
**Manager**: "Approved. What do you need?"
**Developer**: "Weekly sessions with Sheikh Ahmad to model Tax rules correctly"

Result: Business understands value, allocates resources, DDD succeeds.
```

**How to Fix**: Frame DDD in business terms (risk reduction, faster time-to-market, fewer bugs). Get explicit commitment for domain expert time.

**Warning Signs**:

- DDD as "technical decision" without business involvement
- Domain experts unavailable for collaboration
- DDD abandoned when deadlines approach

## Islamic Finance Examples

### Example 1: Tax Calculation Domain

**Bounded Context**: Tax Management

**Ubiquitous Language**:

- Threshold (نصاب) - Minimum threshold
- Hawl (حول) - Lunar year
- Taxable Assets (أموال زكوية) - Assets subject to Tax
- Eight Categories (الأصناف الثمانية) - Eligible recipients

**Aggregates**:

```typescript
class TaxCalculation {
  // Invariants
  private verifyHawlCompleted(): void {
    /* Must complete lunar year */
  }
  private verifyThresholdMet(): void {
    /* Must meet threshold */
  }

  // Domain events
  calculateTax(): void {
    this.recordEvent(new TaxCalculated(/* ... */));
  }
}
```

**Best Practices Applied**:

- Rich domain model (calculation logic in aggregate)
- Value objects (Money, HijriDate, ThresholdThreshold)
- Domain events for cross-context communication
- Ubiquitous language from Compliance terminology

**Antipatterns Avoided**:

- Not anemic (logic lives in aggregate, not service)
- Not database-driven (domain-first design)
- Not missing experts (Compliance scholar involved)

### Example 2: Loan Financing Domain

**Bounded Context**: Loan Financing

**Ubiquitous Language**:

- Loan (مرابحة) - Cost-plus financing
- Qabdh (قبض) - Possession
- Tamlik (تمليك) - Ownership transfer
- Fixed Markup - Not interest (interest)

**Aggregates**:

```typescript
class LoanContract {
  // Invariant: Bank must possess before selling
  draftContract(): void {
    if (!this.possessionTransferred) {
      throw new InvariantViolationError("Asset not possessed");
    }
  }

  // Invariant: Markup must be fixed
  setMarkup(markup: Markup): void {
    if (markup.isVariable()) {
      throw new InvariantViolationError("Interest prohibited");
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
- Not ignoring bounded contexts (separate from Tax, Donation)
- Not missing ubiquitous language (Compliance terms in code)

## Summary

**Antipatterns to Avoid**:

| Category      | Antipattern                              | Core Problem                  | Warning Sign                           |
| ------------- | ---------------------------------------- | ----------------------------- | -------------------------------------- |
| **Common**    | 1. Anemic Domain Model                   | Logic in services, not domain | Classes with only getters/setters      |
|               | 2. Premature Complexity                  | Over-engineering simple CRUD  | More infrastructure than business code |
|               | 3. Ignoring Bounded Contexts             | One model for all contexts    | Constant "what does X mean?" debates   |
|               | 4. Missing Ubiquitous Language           | Technical vs business terms   | Developers can't explain in domain     |
|               | 5. Not Involving Domain Experts          | Developer assumptions         | Production bugs experts would catch    |
| **Strategic** | 6. Database-Driven Design                | Schema drives domain model    | Database ERD before domain model       |
|               | 7. Misusing Entities as Bounded Contexts | Entity-per-service            | Unclear business capability            |
| **Tactical**  | 8. ID References Instead of Objects      | Lost aggregate boundaries     | Repository lookups in business logic   |
|               | 9. Confusing Entities and Value Objects  | Identity confusion            | Value objects with database IDs        |
|               | 10. Focusing Only on Tactical Patterns   | Perfect code, wrong model     | No bounded context discussion          |
| **Org**       | 11. Doing DDD Without Business Buy-In    | No expert time allocated      | DDD abandoned at deadlines             |

**Key Takeaway**: Avoiding these antipatterns requires vigilance across strategic design (understanding the business), tactical implementation (coding patterns), and organizational commitment (domain expert involvement). Success comes from recognizing warning signs early and correcting course before antipatterns become embedded in the system.

## References

- [Microsoft: Introduction to Domain-Driven Design](https://learn.microsoft.com/en-us/archive/msdn-magazine/2009/february/best-practice-an-introduction-to-domain-driven-design) - Best practices for DDD implementation
- [DDD Anti-Patterns (Alok Mishra)](https://alok-mishra.com/2021/11/03/ddd-anti-patterns/) - Common mistakes in DDD
- [Martin Fowler: Anemic Domain Model](https://martinfowler.com/bliki/AnemicDomainModel.html) - Classic antipattern explanation
- [Medium: Common DDD Mistakes](https://medium.com/navalia/the-most-common-domain-driven-design-mistake-6c3f90e0ec2b) - Practical antipatterns

## Related Documentation

- [Best Practices in Domain-Driven Design](./ex-soen-ar-dodrdedd__19-best-practices.md) - Recommended practices and patterns
- [Introduction and Philosophy](./ex-soen-ar-dodrdedd__01-introduction-and-philosophy.md) - DDD introduction and core concepts
- [Bounded Contexts](./ex-soen-ar-dodrdedd__03-bounded-contexts.md) - Strategic design patterns
- [Aggregates](./ex-soen-ar-dodrdedd__09-aggregates.md) - Tactical design patterns

## Related Principles

DDD antipatterns violate core software engineering principles:

- **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Anemic models hide business logic, making domain rules implicit
- **[Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)** - Over-engineering with unnecessary patterns adds complexity without value

See [Best Practices](./ex-soen-ar-dodrdedd__19-best-practices.md) for positive guidance and [Software Engineering Principles](../../../../../governance/principles/software-engineering/README.md).
