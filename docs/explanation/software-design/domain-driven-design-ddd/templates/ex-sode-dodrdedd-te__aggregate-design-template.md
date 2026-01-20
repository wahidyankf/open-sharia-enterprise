---
title: "Aggregate Design Template"
description: "Comprehensive template for documenting aggregate structure including invariants, commands, events, entities, value objects, and lifecycle"
tags: ["ddd", "template", "aggregate", "tactical-design", "domain-model"]
---

# Aggregate Design Template

This template provides a structured approach to designing and documenting aggregates, the consistency and transactional boundaries in your domain model.

## Purpose

Aggregate design documentation:

- **Clarifies boundaries**: What's inside vs outside the aggregate
- **Captures invariants**: Rules that must always be true
- **Documents behavior**: Commands and their outcomes
- **Guides implementation**: Clear specification for developers
- **Supports discussion**: Shared artifact for domain experts and developers
- **Enables review**: Validate design before implementation

## Template Structure

For each aggregate, document:

1. **Aggregate Overview**: Name, purpose, and bounded context
2. **Aggregate Root**: The entry point entity
3. **Internal Entities**: Entities inside the aggregate (if any)
4. **Value Objects**: Immutable values composing the aggregate
5. **Invariants**: Rules that must always hold
6. **Commands**: Operations that modify state
7. **Domain Events**: Events raised by the aggregate
8. **Lifecycle**: Creation, state transitions, and removal
9. **Size & Performance**: Practical considerations

## Complete Example: Zakat Assessment Aggregate

### 1. Aggregate Overview

**Name**: ZakatAssessment

**Purpose**: Manages the process of calculating a Muslim's annual Zakat obligation, including asset collection, hawl verification, nisab comparison, and liability determination.

**Bounded Context**: Zakat Management Context

**Responsibilities**:

- Collect taxpayer's zakatable assets
- Track hawl period for each asset category
- Calculate total zakatable wealth
- Determine Zakat liability based on nisab
- Maintain assessment status (Draft → Under Review → Finalized)

**Not Responsible For**:

- Nisab calculation (external service)
- Payment processing (separate aggregate)
- Beneficiary allocation (separate context)
- Asset valuation (external service for market prices)

### 2. Aggregate Root

**Entity**: `ZakatAssessment`

**Identity**: `AssessmentId` (UUID)

**Attributes**:

- `id: AssessmentId` - Unique identifier
- `taxpayerId: TaxpayerId` - Reference to taxpayer (external aggregate)
- `hawlStart: Date` - Start of lunar year
- `hawlEnd: Date` - End of lunar year
- `status: AssessmentStatus` - Current state (Draft | UnderReview | Finalized)
- `liability: ZakatLiability?` - Calculated liability (only present when finalized)
- `createdAt: DateTime` - Creation timestamp
- `finalizedAt: DateTime?` - Finalization timestamp

**References to External Aggregates**:

- `taxpayerId: TaxpayerId` - Taxpayer aggregate (stored by ID, not full object)

### 3. Internal Entities

**Entity**: `ZakatableAsset`

**Identity**: `AssetId` (UUID)

**Attributes**:

- `id: AssetId`
- `assessmentId: AssessmentId` - Parent assessment
- `category: AssetCategory` - Cash, Gold, Silver, Business, Investment
- `description: string` - Human-readable description
- `value: Money` - Current market value
- `hawlStartDate: Date` - When asset acquired/hawl started
- `isHawlComplete: boolean` - Whether full lunar year has passed

**Purpose**: Represents individual asset or asset category contributing to zakatable wealth.

**Why an Entity?**: Assets have identity and lifecycle (added, valued, removed from assessment).

**Entity**: `AssetValuation`

**Identity**: `ValuationId` (UUID)

**Attributes**:

- `id: ValuationId`
- `assetId: AssetId` - Parent asset
- `valuationDate: Date`
- `valuedAmount: Money`
- `source: ValuationSource` - Manual | MarketData | Appraisal
- `notes: string?`

**Purpose**: Track valuation history for assets (especially those with fluctuating values like investments).

**Why an Entity?**: Each valuation is a distinct historical record with its own identity.

### 4. Value Objects

**Value Object**: `AssessmentStatus`

**Attributes**:

- `value: 'Draft' | 'UnderReview' | 'Finalized'`

**Behavior**:

- `canTransitionTo(newStatus: AssessmentStatus): boolean`
- `equals(other: AssessmentStatus): boolean`

**Invariants**:

- Valid transitions: Draft → UnderReview → Finalized
- No backward transitions
- Once Finalized, cannot change

---

**Value Object**: `ZakatLiability`

**Attributes**:

- `amount: Money` - Zakat due (if obligated)
- `isObligated: boolean` - Whether Zakat is due
- `exemptionReason: string?` - Why exempt (e.g., "Below nisab", "Hawl not complete")

**Behavior**:

- `obligated(amount: Money): ZakatLiability` - Factory for obligated liability
- `exempt(reason: string): ZakatLiability` - Factory for exemption
- `equals(other: ZakatLiability): boolean`

---

**Value Object**: `AssetCategory`

**Attributes**:

- `value: 'Cash' | 'Gold' | 'Silver' | 'BusinessInventory' | 'Investment' | 'Receivables' | 'Livestock' | 'Agriculture'`

**Behavior**:

- `zakatRate(): Percentage` - Different categories may have different rates
- `requiresHawl(): boolean` - Agriculture doesn't require hawl
- `equals(other: AssetCategory): boolean`

---

**Value Object**: `HawlPeriod`

**Attributes**:

- `startDate: HijriDate` - Lunar calendar start
- `endDate: HijriDate` - Lunar calendar end

**Behavior**:

- `create(startDate: HijriDate): HawlPeriod`
- `isComplete(currentDate: HijriDate): boolean`
- `duration(): Duration`

### 5. Invariants

Invariants are rules that must always be true. The aggregate enforces these through its design.

**Invariant 1: Assessment Must Have Valid Hawl Period**

- Hawl end date must be exactly one lunar year after start date
- Hawl start date cannot be in the future
- Enforced: In factory method `ZakatAssessment.create()`

**Invariant 2: Assets Must Have Values**

- Each asset must have a current valuation
- Asset value cannot be negative
- Enforced: In `ZakatableAsset.create()` and `addAsset()` command

**Invariant 3: Only Draft Assessments Can Be Modified**

- Can only add/remove/update assets if status is Draft
- Once UnderReview or Finalized, assets are locked
- Enforced: In `addAsset()`, `removeAsset()`, `updateAsset()` commands

**Invariant 4: Finalization Requires Completed Hawl**

- Cannot finalize assessment before hawl period ends
- Enforced: In `finalize()` command

**Invariant 5: Finalized Assessment Must Have Liability**

- If status is Finalized, liability must be present (either obligated or exempt)
- Enforced: In `finalize()` command

**Invariant 6: Assets Belong to Assessment**

- All assets in the aggregate must reference the same assessment ID
- Enforced: In `addAsset()` command (sets parent ID)

**Invariant 7: Total Wealth Calculation Consistency**

- Total zakatable wealth equals sum of all asset values
- Gold/silver valued at current market prices
- Enforced: In `calculateZakatableWealth()` method

### 6. Commands

Commands are operations that modify the aggregate's state. Each returns `Result<Events, Error>`.

**Command**: `create`

**Static Factory Method**

**Parameters**:

- `taxpayerId: TaxpayerId`
- `hawlStart: HijriDate`

**Preconditions**:

- Hawl start date not in future
- Taxpayer ID must be valid

**Postconditions**:

- New aggregate created with Draft status
- No assets
- No liability

**Events Raised**:

- `ZakatAssessmentCreated`

**Example**:

```typescript
const result = ZakatAssessment.create(new TaxpayerId("taxpayer-123"), HijriDate.fromGregorian(new Date("2024-01-01")));

if (result.isSuccess()) {
  const assessment = result.value;
  // New draft assessment ready
}
```

---

**Command**: `addAsset`

**Parameters**:

- `category: AssetCategory`
- `description: string`
- `value: Money`
- `hawlStartDate: Date`

**Preconditions**:

- Assessment status must be Draft
- Value must be positive
- Hawl start date must be valid

**Postconditions**:

- New asset added to aggregate
- Asset ID assigned
- Asset references this assessment

**Events Raised**:

- `AssetAddedToAssessment`

**Errors**:

- `InvalidStateError` if status not Draft
- `ValidationError` if value negative or invalid dates

**Example**:

```typescript
const result = assessment.addAsset(
  AssetCategory.Cash,
  "Savings account balance",
  Money.usd(25000),
  new Date("2024-01-01"),
);

if (result.isSuccess()) {
  const events = result.value;
  // Asset added, events ready for publishing
}
```

---

**Command**: `removeAsset`

**Parameters**:

- `assetId: AssetId`

**Preconditions**:

- Assessment status must be Draft
- Asset with given ID must exist

**Postconditions**:

- Asset removed from aggregate

**Events Raised**:

- `AssetRemovedFromAssessment`

**Errors**:

- `InvalidStateError` if status not Draft
- `NotFoundError` if asset doesn't exist

---

**Command**: `updateAssetValue`

**Parameters**:

- `assetId: AssetId`
- `newValue: Money`
- `valuationSource: ValuationSource`

**Preconditions**:

- Assessment status must be Draft
- Asset must exist
- New value must be positive

**Postconditions**:

- Asset valuation updated
- New valuation record created

**Events Raised**:

- `AssetValueUpdated`

---

**Command**: `submitForReview`

**Parameters**: None

**Preconditions**:

- Assessment status must be Draft
- Must have at least one asset

**Postconditions**:

- Status changed to UnderReview
- Assets locked (no more modifications)

**Events Raised**:

- `AssessmentSubmittedForReview`

**Errors**:

- `InvalidStateError` if not Draft
- `ValidationError` if no assets

---

**Command**: `finalize`

**Parameters**:

- `nisab: Nisab` - Current nisab threshold
- `goldPrice: Money` - Current gold price per gram
- `silverPrice: Money` - Current silver price per gram

**Preconditions**:

- Assessment status must be UnderReview
- Hawl period must be complete
- All assets must have current valuations

**Postconditions**:

- Status changed to Finalized
- Liability calculated and set
- Finalization timestamp recorded

**Events Raised**:

- `ZakatAssessmentFinalized` (includes liability details)

**Business Logic**:

1. Calculate total zakatable wealth from all assets
2. Compare against nisab threshold
3. If below nisab: Create exempt liability
4. If above nisab: Calculate 2.5% of total wealth
5. Set liability and finalize status

**Errors**:

- `InvalidStateError` if not UnderReview
- `BusinessRuleViolation` if hawl not complete

**Example**:

```typescript
const result = assessment.finalize(
  currentNisab,
  Money.usd(60), // Gold price per gram
  Money.usd(0.7), // Silver price per gram
);

if (result.isSuccess()) {
  const events = result.value;
  // Assessment finalized, liability determined
  // Events: [ZakatAssessmentFinalized]
}
```

### 7. Domain Events

Domain events are facts about what happened to the aggregate. They are the aggregate's way of communicating with the outside world.

**Event**: `ZakatAssessmentCreated`

**Payload**:

- `assessmentId: AssessmentId`
- `taxpayerId: TaxpayerId`
- `hawlStart: Date`
- `hawlEnd: Date`
- `createdAt: DateTime`

**Raised By**: `create()` factory method

**Consumers**:

- Notification service (notify taxpayer)
- Analytics service (track assessment creation)

---

**Event**: `AssetAddedToAssessment`

**Payload**:

- `assessmentId: AssessmentId`
- `assetId: AssetId`
- `category: AssetCategory`
- `value: Money`
- `addedAt: DateTime`

**Raised By**: `addAsset()` command

**Consumers**:

- Audit log service

---

**Event**: `AssessmentSubmittedForReview`

**Payload**:

- `assessmentId: AssessmentId`
- `taxpayerId: TaxpayerId`
- `totalAssets: number`
- `submittedAt: DateTime`

**Raised By**: `submitForReview()` command

**Consumers**:

- Workflow service (assign to reviewer)
- Notification service (notify review team)

---

**Event**: `ZakatAssessmentFinalized`

**Payload**:

- `assessmentId: AssessmentId`
- `taxpayerId: TaxpayerId`
- `hawlPeriod: HawlPeriod`
- `totalWealth: Money`
- `nisabThreshold: Money`
- `liability: ZakatLiability`
- `finalizedAt: DateTime`

**Raised By**: `finalize()` command

**Consumers**:

- Payment service (create payment obligation if Zakat due)
- Accounting service (record Zakat liability)
- Notification service (notify taxpayer of obligation)
- Analytics service (compliance reporting)

---

**Event**: `AssetRemovedFromAssessment`

**Payload**:

- `assessmentId: AssessmentId`
- `assetId: AssetId`
- `removedAt: DateTime`

**Raised By**: `removeAsset()` command

---

**Event**: `AssetValueUpdated`

**Payload**:

- `assessmentId: AssessmentId`
- `assetId: AssetId`
- `previousValue: Money`
- `newValue: Money`
- `valuationSource: ValuationSource`
- `updatedAt: DateTime`

**Raised By**: `updateAssetValue()` command

### 8. Lifecycle

**Creation**:

```typescript
// Factory method creates new assessment
const result = ZakatAssessment.create(taxpayerId, hawlStart);

// Initial state: Draft, no assets, no liability
```

**State Transitions**:

```
┌──────┐  submitForReview()  ┌──────────────┐  finalize()  ┌───────────┐
│ Draft│ ───────────────────▶│ UnderReview  │ ───────────▶ │ Finalized │
└──────┘                     └──────────────┘              └───────────┘
   │                                                              │
   │ addAsset()                                                   │
   │ removeAsset()                                                │ (terminal state)
   │ updateAssetValue()                                           │
   │                                                              │
   └──────────────────────────────────────────────────────────────┘
             (No transitions out of Finalized)
```

**Persistence**:

- Aggregate root and all internal entities persisted together
- Repository ensures transactional consistency
- Events published after successful persistence

**Reconstitution**:

```typescript
// Load from database
const assessmentResult = await repository.findById(assessmentId);

// Aggregate rebuilt with all entities and value objects
const assessment = assessmentResult.value;
```

**Deletion**:

- Assessments are typically never deleted (audit trail)
- Can be archived after several years
- If deletion required, cascade to all internal entities

### 9. Size & Performance Considerations

**Aggregate Size**:

- **Assets**: Typically 5-20 assets per assessment (manageable)
- **Valuations**: 1-3 valuations per asset (reasonable history)
- **Total Size**: Small to medium aggregate

**Concurrency**:

- Low contention (one taxpayer, one assessment per year)
- Version locking sufficient (optimistic concurrency)

**Performance**:

- Loading: Single aggregate load is fast
- Calculation: Total wealth calculation is O(n) in number of assets (acceptable)
- Events: Small number of events per operation (1-2 typically)

**Scalability**:

- Assessments partitioned by taxpayer (natural sharding key)
- No cross-assessment transactions needed
- Can scale horizontally by taxpayer

**When Aggregate Might Grow Too Large**:

- If tracking daily valuations for volatile investments (hundreds of valuations)
- Solution: Separate Valuation aggregate, reference by ID
- If thousands of individual assets (e.g., large business inventory)
- Solution: Aggregate assets by category, maintain detail in separate read model

## Template for Your Aggregate

Use this structure to design your aggregates:

---

### 1. Aggregate Overview

**Name**: [AggregateName]

**Purpose**: [What this aggregate represents and manages]

**Bounded Context**: [Context name]

**Responsibilities**:

- [Responsibility 1]
- [Responsibility 2]

**Not Responsible For**:

- [What this aggregate doesn't handle]

---

### 2. Aggregate Root

**Entity**: `[RootEntityName]`

**Identity**: `[IdentityType]` ([How generated])

**Attributes**:

- `[attribute]: [Type]` - [Description]

**References to External Aggregates**:

- `[externalId]: [IdType]` - [What it references]

---

### 3. Internal Entities

**Entity**: `[EntityName]`

**Identity**: `[IdentityType]`

**Attributes**:

- `[attribute]: [Type]` - [Description]

**Purpose**: [Why this is an entity inside the aggregate]

**Why an Entity?**: [Justify identity and mutability]

---

### 4. Value Objects

**Value Object**: `[ValueObjectName]`

**Attributes**:

- `[attribute]: [Type]`

**Behavior**:

- `[method]([params]): [ReturnType]`

**Invariants**:

- [Invariant this value object enforces]

---

### 5. Invariants

**Invariant 1: [Name]**

- [Rule that must always be true]
- Enforced: [Where in code]

---

### 6. Commands

**Command**: `[commandName]`

**Parameters**:

- `[param]: [Type]`

**Preconditions**:

- [What must be true before executing]

**Postconditions**:

- [What is true after executing]

**Events Raised**:

- `[EventName]`

**Errors**:

- `[ErrorType]` [when it occurs]

**Example**:

```typescript
[Code example]
```

---

### 7. Domain Events

**Event**: `[EventName]`

**Payload**:

- `[field]: [Type]`

**Raised By**: `[command]`

**Consumers**:

- [Service or context that handles this event]

---

### 8. Lifecycle

**Creation**: [How aggregate is created]

**State Transitions**: [Diagram or description]

**Persistence**: [How it's saved]

**Reconstitution**: [How it's loaded]

**Deletion**: [How/when it's deleted]

---

### 9. Size & Performance Considerations

**Aggregate Size**: [Typical number of entities/values]

**Concurrency**: [Contention expectations]

**Performance**: [Load time, calculation complexity]

**Scalability**: [Partitioning strategy]

**When It Might Grow Too Large**: [Warning signs and solutions]

---

## Design Review Checklist

Use this checklist when reviewing aggregate designs:

### Boundaries

- [ ] Aggregate enforces true business invariants (not just data consistency)
- [ ] Aggregate is small (prefer smaller over larger)
- [ ] External aggregates referenced by ID, not embedded
- [ ] One aggregate per transaction (no multi-aggregate transactions)

### Consistency

- [ ] All invariants can be enforced within the aggregate
- [ ] Commands validate all preconditions before modifying state
- [ ] State transitions are explicit and controlled
- [ ] No invariants span multiple aggregates

### Behavior

- [ ] Commands are named in domain language
- [ ] Commands return Results (success/failure explicit)
- [ ] Domain events capture all significant state changes
- [ ] Business logic is in the aggregate, not services

### Identity

- [ ] Aggregate root has clear identity (ID)
- [ ] Internal entities have identity within aggregate scope
- [ ] Value objects are immutable and defined by attributes

### Performance

- [ ] Aggregate can be loaded in single query
- [ ] Calculations are efficient (no N+1 queries)
- [ ] Aggregate size won't grow unbounded
- [ ] Concurrency conflicts are rare

### Events

- [ ] Events are named in past tense
- [ ] Events contain all data consumers need
- [ ] Events are immutable
- [ ] Event payload doesn't expose internal aggregate structure

## See Also

- [Aggregates](../ex-sode-dodrdedd__09-aggregates.md) - Aggregate concepts and patterns
- [Entities](../ex-sode-dodrdedd__07-entities.md) - Entity design principles
- [Value Objects](../ex-sode-dodrdedd__08-value-objects.md) - Value object patterns
- [Domain Events](../ex-sode-dodrdedd__12-domain-events.md) - Event design and usage
- [Repositories](../ex-sode-dodrdedd__10-repositories.md) - Persisting aggregates
- [Factories](../ex-sode-dodrdedd__13-factories.md) - Creating aggregates
