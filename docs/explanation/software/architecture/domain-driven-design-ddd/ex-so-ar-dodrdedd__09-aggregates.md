# Aggregates

## What is an Aggregate?

An **Aggregate** is a cluster of domain objects (entities and value objects) that can be treated as a single unit for data changes. One entity within the aggregate is designated as the **Aggregate Root**, which is the only entry point for external interactions. All access to entities and value objects within the aggregate must go through the aggregate root.

**Key Characteristics:**

- **Consistency Boundary**: The aggregate defines transactional consistency boundaries - all objects within the aggregate are consistent or none are
- **Single Entry Point**: The aggregate root is the only entity that external objects can hold references to
- **Invariant Protection**: The aggregate root enforces business rules (invariants) that span multiple objects within the aggregate
- **Unit of Persistence**: Aggregates are saved and retrieved as complete units
- **Concurrency Boundary**: Optimistic locking typically applies at the aggregate level

## Why Aggregates Matter

### The Problem: Unrestricted Object Graphs

Without aggregates, domain models devolve into tangled graphs where any object can be modified from anywhere:

```typescript
// WITHOUT Aggregates: Dangerous unrestricted access
const order = orderRepository.findById(orderId);
const orderLine = order.orderLines[0];
orderLine.quantity = -100; // Violated invariant: negative quantity!
orderLine.price = Money.zero(); // Violated invariant: zero price!
// Order total now inconsistent with line items

orderRepository.save(order); // Persists invalid state
```

Problems:

- **Invariants easily violated**: No single point enforces rules
- **Inconsistent state**: Objects can be in invalid combinations
- **Concurrent modification chaos**: Multiple users editing different parts simultaneously
- **Performance issues**: Lazy loading entire object graphs

### The Solution: Aggregates

Aggregates solve these problems by:

**1. Enforcing Invariants**

All modifications go through the aggregate root, which validates business rules:

```typescript
// WITH Aggregates: Invariant protection
const order = orderRepository.findById(orderId);
order.changeLineQuantity(lineId, newQuantity); // Validates: quantity > 0
order.changeLinePrice(lineId, newPrice); // Validates: price > 0, recalculates total
order.recalculateTotal(); // Maintains consistency

orderRepository.save(order); // Persists valid state
```

**2. Defining Transactional Boundaries**

One aggregate = one transaction. Changes to multiple aggregates require eventual consistency.

**3. Managing Concurrency**

Version numbers on aggregate roots detect conflicting modifications.

**4. Optimizing Performance**

Load only the aggregate you need, not the entire domain graph.

## Aggregate Design Principles

### 1. Keep Aggregates Small

**Guideline**: An aggregate should be as small as possible while still protecting its invariants.

**Why?**

- Smaller aggregates reduce concurrency conflicts
- Faster loading and persistence
- Easier to understand and maintain
- Better scalability

**Anti-Pattern**: Large Aggregate

```typescript
// ANTI-PATTERN: Too large
class Customer {
  // Aggregate root
  customerId: CustomerId;
  name: string;
  email: Email;
  addresses: Address[]; // Hundreds of addresses
  orders: Order[]; // Thousands of orders
  supportTickets: SupportTicket[]; // Hundreds of tickets
  loyaltyPoints: LoyaltyAccount;
  preferences: CustomerPreferences;
}
```

Problems:

- Loading a customer loads thousands of orders
- Concurrent updates to address and orders conflict
- Large transaction scope
- Memory intensive

**Better Design**: Multiple Aggregates

```typescript
// BETTER: Separate aggregates
class Customer {
  // Aggregate root
  customerId: CustomerId;
  name: string;
  email: Email;
  defaultAddressId: AddressId; // Reference by ID
}

class Order {
  // Separate aggregate root
  orderId: OrderId;
  customerId: CustomerId; // Reference by ID
  orderLines: OrderLine[];
  total: Money;
}

class SupportTicket {
  // Separate aggregate root
  ticketId: TicketId;
  customerId: CustomerId; // Reference by ID
  messages: Message[];
}
```

Benefits:

- Load only what you need
- Concurrent operations on customer, orders, and tickets don't conflict
- Clear transactional boundaries

### 2. Reference Other Aggregates by ID Only

**Rule**: Aggregates should not hold direct object references to other aggregates. Use IDs instead.

**Why?**

- Prevents loading entire object graphs
- Makes aggregate boundaries explicit
- Simplifies persistence and serialization
- Reduces coupling

**Anti-Pattern**: Object Reference

```typescript
// ANTI-PATTERN: Direct object reference
class Order {
  orderId: OrderId;
  customer: Customer; // Direct reference
  product: Product; // Direct reference
}
```

**Better**: ID Reference

```typescript
// BETTER: Reference by ID
class Order {
  orderId: OrderId;
  customerId: CustomerId; // ID only
  productId: ProductId; // ID only
}

// Load customer separately when needed
const customer = customerRepository.findById(order.customerId);
```

### 3. One Repository Per Aggregate

**Rule**: Only create repositories for aggregate roots, not for entities within aggregates.

**Why?**

- Enforces access through aggregate root
- Prevents bypassing invariant enforcement
- Simplifies data access layer

```typescript
// GOOD: Repository for aggregate root
interface OrderRepository {
  findById(id: OrderId): Order;
  save(order: Order): void;
}

// BAD: No repository for internal entities
// interface OrderLineRepository { ... }  // Don't do this!
```

### 4. Protect Invariants Within Aggregate Boundaries

**Rule**: All business rules that must be immediately consistent should be within one aggregate.

**Why?**

- Aggregates define transactional consistency boundaries
- Rules spanning aggregates require eventual consistency

**Example: Order Invariants**

Invariants that MUST be immediately consistent (within Order aggregate):

- Order total equals sum of line item subtotals
- All line quantities are positive
- Order status transitions are valid (Draft → Submitted → Fulfilled)

Invariants that can be eventually consistent (across aggregates):

- Customer credit limit not exceeded (Order → Customer aggregates)
- Product inventory sufficient (Order → Inventory aggregates)

### 5. Use Eventual Consistency Between Aggregates

**Rule**: When business rules span aggregates, use domain events and eventual consistency.

**Why?**

- Allows aggregates to evolve independently
- Reduces contention and improves scalability
- Aligns with distributed system realities

**Example: Order Placement**

```typescript
class Order {
  submit(): void {
    // Validate internal invariants (immediate consistency)
    if (this.orderLines.length === 0) {
      throw new Error("Order must have at least one line");
    }

    this.status = OrderStatus.Submitted;

    // Publish event for cross-aggregate concerns (eventual consistency)
    this.addDomainEvent(new OrderSubmitted(this.orderId, this.customerId));
  }
}

// Separately, inventory aggregate listens and updates stock
class InventoryEventHandler {
  handleOrderSubmitted(event: OrderSubmitted): void {
    const order = orderRepository.findById(event.orderId);
    for (const line of order.orderLines) {
      const inventory = inventoryRepository.findById(line.productId);
      inventory.reserveStock(line.quantity);
      inventoryRepository.save(inventory);
    }
  }
}
```

## Aggregate Example: Zakat Assessment

Let's design a comprehensive aggregate for Islamic zakat calculation:

### Business Requirements

**Invariants** (must always be true):

- Assessment can only be finalized if hawl (lunar year) is complete
- Total wealth must meet or exceed nisab threshold to owe zakat
- All zakatable assets must have valid declarations
- Zakat amount equals wealth multiplied by correct rate
- Once finalized, assessment is immutable

**Entities**:

- `ZakatAssessment`: Aggregate root
- `WealthDeclaration`: Entity for each asset type declared

**Value Objects**:

- `AssessmentId`, `NisabAmount`, `ZakatRate`, `Money`, `HijriDate`

### Design

```typescript
// Aggregate Root
class ZakatAssessment {
  private readonly assessmentId: AssessmentId;
  private readonly wealthHolderId: WealthHolderId;
  private readonly assessmentPeriod: LunarYearPeriod;
  private wealthDeclarations: WealthDeclaration[] = [];
  private status: AssessmentStatus;
  private zakatAmount: Money | null = null;
  private finalizedAt: HijriDate | null = null;

  private constructor(assessmentId: AssessmentId, wealthHolderId: WealthHolderId, assessmentPeriod: LunarYearPeriod) {
    this.assessmentId = assessmentId;
    this.wealthHolderId = wealthHolderId;
    this.assessmentPeriod = assessmentPeriod;
    this.status = AssessmentStatus.Draft;
  }

  // Factory method
  static create(wealthHolderId: WealthHolderId, startDate: HijriDate): ZakatAssessment {
    const assessmentId = AssessmentId.generate();
    const period = LunarYearPeriod.fromStartDate(startDate);
    return new ZakatAssessment(assessmentId, wealthHolderId, period);
  }

  // Command: Add wealth declaration
  declareWealth(wealthType: WealthType, amount: Money, acquiredDate: HijriDate): void {
    // Enforce invariant: can only add declarations in Draft status
    if (this.status !== AssessmentStatus.Draft) {
      throw new Error("Cannot add declarations to finalized assessment");
    }

    // Enforce invariant: declaration must be within assessment period
    if (!this.assessmentPeriod.contains(acquiredDate)) {
      throw new Error("Wealth acquired date must be within assessment period");
    }

    const declaration = new WealthDeclaration(WealthDeclarationId.generate(), wealthType, amount, acquiredDate);

    this.wealthDeclarations.push(declaration);
  }

  // Command: Finalize assessment
  finalize(nisabThreshold: NisabAmount, zakatRate: ZakatRate): void {
    // Enforce invariant: hawl must be complete
    if (!this.assessmentPeriod.isComplete()) {
      throw new Error("Cannot finalize: hawl not yet complete");
    }

    // Enforce invariant: must have declarations
    if (this.wealthDeclarations.length === 0) {
      throw new Error("Cannot finalize: no wealth declared");
    }

    // Calculate total wealth
    const totalWealth = this.calculateTotalWealth();

    // Enforce invariant: must meet nisab threshold
    if (totalWealth.isLessThan(nisabThreshold.toMoney())) {
      this.zakatAmount = Money.zero();
      this.status = AssessmentStatus.ExemptBelowNisab;
    } else {
      // Calculate zakat owed
      this.zakatAmount = totalWealth.multiply(zakatRate.percentage);
      this.status = AssessmentStatus.Finalized;
    }

    this.finalizedAt = HijriDate.now();

    // Publish domain event
    this.addDomainEvent(
      new ZakatCalculated(this.assessmentId, this.wealthHolderId, this.zakatAmount, this.finalizedAt),
    );
  }

  // Query: Calculate total declared wealth
  private calculateTotalWealth(): Money {
    return this.wealthDeclarations.reduce((total, declaration) => total.add(declaration.amount), Money.zero());
  }

  // Query: Check if meets nisab
  meetsNisab(nisabThreshold: NisabAmount): boolean {
    return this.calculateTotalWealth().isGreaterThanOrEqual(nisabThreshold.toMoney());
  }

  // Getters (read-only access)
  get id(): AssessmentId {
    return this.assessmentId;
  }

  get totalWealth(): Money {
    return this.calculateTotalWealth();
  }

  get declarations(): readonly WealthDeclaration[] {
    return this.wealthDeclarations; // Return immutable view
  }
}

// Entity within aggregate
class WealthDeclaration {
  constructor(
    readonly id: WealthDeclarationId,
    readonly wealthType: WealthType,
    readonly amount: Money,
    readonly acquiredDate: HijriDate,
  ) {}
}

// Value Objects
class AssessmentId {
  private constructor(private readonly value: string) {}

  static generate(): AssessmentId {
    return new AssessmentId(uuidv4());
  }

  equals(other: AssessmentId): boolean {
    return this.value === other.value;
  }

  toString(): string {
    return this.value;
  }
}

class LunarYearPeriod {
  private constructor(
    readonly startDate: HijriDate,
    readonly endDate: HijriDate,
  ) {}

  static fromStartDate(start: HijriDate): LunarYearPeriod {
    const end = start.addLunarYears(1);
    return new LunarYearPeriod(start, end);
  }

  contains(date: HijriDate): boolean {
    return date.isAfterOrEqual(this.startDate) && date.isBeforeOrEqual(this.endDate);
  }

  isComplete(): boolean {
    return HijriDate.now().isAfterOrEqual(this.endDate);
  }
}

enum AssessmentStatus {
  Draft = "DRAFT",
  Finalized = "FINALIZED",
  ExemptBelowNisab = "EXEMPT_BELOW_NISAB",
}

// Domain Event
class ZakatCalculated {
  constructor(
    readonly assessmentId: AssessmentId,
    readonly wealthHolderId: WealthHolderId,
    readonly zakatAmount: Money,
    readonly calculatedAt: HijriDate,
  ) {}
}
```

### Analysis

**Aggregate Boundary**:

- `ZakatAssessment` (root) + `WealthDeclaration` (entities) + value objects
- Size: Small, focused on zakat calculation consistency

**Invariants Protected**:

1. Declarations only in Draft status ✓
2. Declarations within assessment period ✓
3. Hawl complete before finalization ✓
4. Must have declarations before finalization ✓
5. Zakat calculation correctness ✓

**External References by ID**:

- `wealthHolderId`: References external Wealth Holder aggregate

**Transactional Consistency**:

- All invariants enforced within one database transaction

**Eventual Consistency**:

- Integration with billing/payment systems via `ZakatCalculated` event

## Functional Programming Perspective

Aggregates work beautifully in FP with immutable data structures and pure functions:

```typescript
// FP-style aggregate with immutable updates
type ZakatAssessment = {
  readonly assessmentId: AssessmentId;
  readonly wealthHolderId: WealthHolderId;
  readonly assessmentPeriod: LunarYearPeriod;
  readonly declarations: ReadonlyArray<WealthDeclaration>;
  readonly status: AssessmentStatus;
  readonly zakatAmount: Money | null;
};

// Pure function to add declaration
function declareWealth(
  assessment: ZakatAssessment,
  wealthType: WealthType,
  amount: Money,
  acquiredDate: HijriDate,
): Result<ZakatAssessment, DomainError> {
  // Validate invariants
  if (assessment.status !== AssessmentStatus.Draft) {
    return Err(new Error("Cannot add declarations to finalized assessment"));
  }

  if (!assessment.assessmentPeriod.contains(acquiredDate)) {
    return Err(new Error("Declaration outside assessment period"));
  }

  // Return new assessment with added declaration
  const newDeclaration = {
    id: WealthDeclarationId.generate(),
    wealthType,
    amount,
    acquiredDate,
  };

  return Ok({
    ...assessment,
    declarations: [...assessment.declarations, newDeclaration],
  });
}

// Pure function to finalize
function finalize(
  assessment: ZakatAssessment,
  nisabThreshold: NisabAmount,
  zakatRate: ZakatRate,
): Result<[ZakatAssessment, ZakatCalculated], DomainError> {
  // Validate invariants
  if (!assessment.assessmentPeriod.isComplete()) {
    return Err(new Error("Hawl not complete"));
  }

  const totalWealth = calculateTotalWealth(assessment);
  const zakatAmount = totalWealth.isGreaterThanOrEqual(nisabThreshold.toMoney())
    ? totalWealth.multiply(zakatRate.percentage)
    : Money.zero();

  const newStatus = zakatAmount.isZero() ? AssessmentStatus.ExemptBelowNisab : AssessmentStatus.Finalized;

  const updatedAssessment = {
    ...assessment,
    zakatAmount,
    status: newStatus,
  };

  const event = new ZakatCalculated(assessment.assessmentId, assessment.wealthHolderId, zakatAmount, HijriDate.now());

  return Ok([updatedAssessment, event]);
}
```

**FP Benefits**:

- **Immutability**: Aggregate state never mutates
- **Pure Functions**: Commands return new aggregate + events
- **Testability**: No mocks needed, pure input/output testing
- **Time Travel**: Easy to replay state changes
- **Concurrency**: Immutable data = safe concurrent access

See [DDD and Functional Programming](./ex-so-ar-dodrdedd__14-ddd-and-functional-programming.md) for comprehensive FP patterns.

## Common Aggregate Patterns

### Pattern 1: Single Entity Aggregate

**When**: Entity has no closely related entities, only value objects.

**Example**: Halal Certification

```typescript
class HalalCertification {
  // Aggregate root (single entity)
  private readonly certificationId: CertificationId;
  private readonly productId: ProductId; // External reference
  private readonly authority: CertificationAuthority; // Value object
  private readonly expiryDate: ExpiryDate; // Value object
  private status: CertificationStatus;

  revoke(reason: string): void {
    if (this.status === CertificationStatus.Revoked) {
      throw new Error("Already revoked");
    }
    this.status = CertificationStatus.Revoked;
    this.addDomainEvent(new CertificationRevoked(this.certificationId, reason));
  }
}
```

### Pattern 2: Parent-Child Aggregate

**When**: Entity has child entities that make no sense outside the parent context.

**Example**: Order with Order Lines

```typescript
class Order {
  // Aggregate root
  private readonly orderId: OrderId;
  private orderLines: OrderLine[] = []; // Child entities
  private total: Money;

  addLine(productId: ProductId, quantity: number, price: Money): void {
    const line = new OrderLine(OrderLineId.generate(), productId, quantity, price);
    this.orderLines.push(line);
    this.recalculateTotal();
  }

  private recalculateTotal(): void {
    this.total = this.orderLines.reduce((sum, line) => sum.add(line.subtotal()), Money.zero());
  }
}

// Entity within aggregate (no identity outside Order)
class OrderLine {
  constructor(
    readonly id: OrderLineId,
    readonly productId: ProductId,
    readonly quantity: number,
    readonly price: Money,
  ) {}

  subtotal(): Money {
    return this.price.multiply(this.quantity);
  }
}
```

### Pattern 3: Aggregate with Lifecycle

**When**: Aggregate has distinct states with different allowed operations.

**Example**: Murabaha Contract (Islamic Financing)

```typescript
class MurabahaContract {
  // Aggregate root with lifecycle
  private status: ContractStatus;

  // Status transitions with invariant enforcement
  submit(): void {
    if (this.status !== ContractStatus.Draft) {
      throw new Error("Can only submit draft contracts");
    }
    this.validateCompleteness();
    this.status = ContractStatus.Submitted;
    this.addDomainEvent(new ContractSubmitted(this.contractId));
  }

  approve(): void {
    if (this.status !== ContractStatus.Submitted) {
      throw new Error("Can only approve submitted contracts");
    }
    this.validateRibaCompliance();
    this.status = ContractStatus.Approved;
    this.addDomainEvent(new ContractApproved(this.contractId));
  }

  execute(): void {
    if (this.status !== ContractStatus.Approved) {
      throw new Error("Can only execute approved contracts");
    }
    this.status = ContractStatus.Executed;
    this.addDomainEvent(new ContractExecuted(this.contractId));
  }
}

enum ContractStatus {
  Draft = "DRAFT",
  Submitted = "SUBMITTED",
  Approved = "APPROVED",
  Executed = "EXECUTED",
}
```

## Decision Tree: What Belongs in the Aggregate?

Use this decision tree to determine aggregate boundaries:

```
Does Entity/Value Object X belong in Aggregate A?

├─ Does X only make sense in the context of A?
│  └─ YES → X is part of aggregate A
│  └─ NO → Continue...
│
├─ Must X be immediately consistent with A?
│  └─ YES → X is part of aggregate A
│  └─ NO → X is a separate aggregate (eventual consistency OK)
│
├─ Would concurrent modifications to X and A cause conflicts?
│  └─ YES (frequently) → X is part of aggregate A
│  └─ NO (rarely) → X is a separate aggregate
│
└─ Is X reused across multiple aggregates?
   └─ YES → X is a separate aggregate
   └─ NO → X is part of aggregate A
```

**Example Decisions**:

- **Order Line in Order**: Only makes sense in Order context → Part of Order aggregate
- **Customer in Order**: Reused across many orders, eventual consistency OK → Separate aggregate, reference by ID
- **Money in Order**: Value object, immutable, part of Order state → Part of Order aggregate

See [Decision Trees and Best Practices](./ex-so-ar-dodrdedd__16-decision-trees-and-best-practices.md) for comprehensive decision guidance.

## Persistence Patterns

### Repository Interface

```typescript
interface ZakatAssessmentRepository {
  findById(id: AssessmentId): ZakatAssessment | null;
  save(assessment: ZakatAssessment): void;
  findByWealthHolder(holderId: WealthHolderId): ZakatAssessment[];
}
```

**Key Points**:

- Repository methods operate on aggregate roots only
- Save entire aggregate as atomic unit
- Load entire aggregate together

### Database Mapping

**Option 1: Single Table** (for small aggregates)

```sql
CREATE TABLE zakat_assessments (
  assessment_id UUID PRIMARY KEY,
  wealth_holder_id UUID NOT NULL,
  status VARCHAR(50) NOT NULL,
  zakat_amount DECIMAL(19,4),
  -- Other root fields
);

CREATE TABLE wealth_declarations (
  declaration_id UUID PRIMARY KEY,
  assessment_id UUID NOT NULL REFERENCES zakat_assessments(assessment_id),
  wealth_type VARCHAR(50) NOT NULL,
  amount DECIMAL(19,4) NOT NULL,
  acquired_date VARCHAR(10) NOT NULL,
  -- Other declaration fields
);
```

**Option 2: Document Store** (for complex aggregates)

```json
{
  "assessmentId": "uuid",
  "wealthHolderId": "uuid",
  "status": "DRAFT",
  "declarations": [
    {
      "id": "uuid",
      "wealthType": "CASH",
      "amount": { "value": 10000, "currency": "USD" },
      "acquiredDate": "1445-01-01"
    }
  ]
}
```

**Option 3: Event Sourcing** (for audit requirements)

Store all domain events, reconstruct aggregate by replaying events.

## Testing Aggregates

Aggregates are highly testable due to clear boundaries:

```typescript
describe("ZakatAssessment Aggregate", () => {
  it("should enforce hawl completion before finalization", () => {
    // Arrange
    const startDate = HijriDate.fromString("1444-01-01");
    const assessment = ZakatAssessment.create(wealthHolderId, startDate);
    assessment.declareWealth(WealthType.Cash, Money.usd(10000), startDate);

    // Act & Assert
    expect(() => {
      assessment.finalize(NisabAmount.goldStandard(), ZakatRate.standard());
    }).toThrow("hawl not yet complete");
  });

  it("should calculate zakat correctly when above nisab", () => {
    // Arrange
    const pastDate = HijriDate.fromString("1443-01-01");
    const assessment = ZakatAssessment.create(wealthHolderId, pastDate);
    assessment.declareWealth(WealthType.Cash, Money.usd(10000), pastDate);

    // Act
    assessment.finalize(
      NisabAmount.fromMoney(Money.usd(5000)),
      ZakatRate.standard(), // 2.5%
    );

    // Assert
    expect(assessment.zakatAmount.value).toBe(250); // 10000 * 0.025
    expect(assessment.status).toBe(AssessmentStatus.Finalized);
  });
});
```

## Common Mistakes

### 1. Anemic Aggregates

**Problem**: Aggregate roots with only getters/setters, no business logic.

```typescript
// ANTI-PATTERN: Anemic
class Order {
  getStatus(): OrderStatus {
    return this.status;
  }
  setStatus(status: OrderStatus): void {
    this.status = status; // No validation!
  }
}
```

**Solution**: Encapsulate behavior and enforce invariants.

### 2. Aggregates Too Large

**Problem**: Including too many entities, causing performance and concurrency issues.

**Solution**: Keep aggregates small, use eventual consistency between aggregates.

### 3. Breaking Encapsulation

**Problem**: Exposing internal entities for external modification.

```typescript
// ANTI-PATTERN
class Order {
  getOrderLines(): OrderLine[] {
    return this.orderLines; // Mutable array!
  }
}
// External code
order.getOrderLines()[0].quantity = -100; // Violated invariant!
```

**Solution**: Return immutable views or copies.

```typescript
class Order {
  get orderLines(): readonly OrderLine[] {
    return Object.freeze([...this.orderLines]);
  }
}
```

### 4. Persisting Aggregate Internals Separately

**Problem**: Saving entities within aggregate independently.

**Solution**: Save entire aggregate atomically via aggregate root repository.

## Summary

Aggregates are the cornerstone of tactical DDD:

- **Consistency Boundaries**: Define what must be immediately consistent
- **Invariant Protection**: Enforce business rules at a single entry point
- **Transactional Units**: One aggregate = one transaction
- **Concurrency Management**: Optimize version control and locking
- **Right-Sizing**: Keep aggregates as small as possible

Well-designed aggregates make domain models robust, testable, and scalable.

## Next Steps

- **[Entities](./ex-so-ar-dodrdedd__07-entities.md)** - Understand identity-based objects within aggregates
- **[Value Objects](./ex-so-ar-dodrdedd__08-value-objects.md)** - Immutable values within aggregates
- **[Repositories](./ex-so-ar-dodrdedd__10-repositories.md)** - Persistence abstraction for aggregates
- **[Domain Events](./ex-so-ar-dodrdedd__12-domain-events.md)** - Eventual consistency between aggregates
- **[Decision Trees and Best Practices](./ex-so-ar-dodrdedd__16-decision-trees-and-best-practices.md)** - Practical aggregate design guidance
- **[Aggregate Design Template](./templates/ex-so-ar-dodrdedd-te__aggregate-design-template.md)** - Document your aggregates

## References

- Eric Evans, "Domain-Driven Design" (2003) - Chapter on Aggregates
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013) - Chapter 10: Aggregates
- Vaughn Vernon, "Effective Aggregate Design" - Three-part series
- Martin Fowler, ["DDD Aggregate"](https://martinfowler.com/bliki/DDD_Aggregate.html)
