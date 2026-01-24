---
title: TypeScript Aggregate Template
description: Template for creating Domain-Driven Design aggregates in TypeScript with transactional boundaries, consistency enforcement, domain events, and child entity management
category: template
tags:
  - typescript
  - ddd
  - domain-model
  - aggregate
  - aggregate-root
  - consistency
  - transactions
  - domain-events
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.7
related:
  - entity-template.md
  - value-object-template.md
  - domain-event-template.md
  - ex-so-stla-ts__best-practices.md
  - ex-so-stla-ts__type-safety.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript Aggregate Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) aggregates in TypeScript. Aggregates are clusters of entities and value objects with a defined transactional boundary, enforcing invariants and consistency across related domain objects.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Aggregate Root Pattern](#aggregate-root-pattern)
4. [Child Entity Management](#child-entity-management)
5. [Invariant Enforcement](#invariant-enforcement)
6. [Transactional Boundaries](#transactional-boundaries)
7. [Domain Events](#domain-events)
8. [Complete Example: Donation Campaign](#complete-example-donation-campaign)
9. [TypeScript vs Java/Go Comparison](#typescript-vs-javago-comparison)
10. [Testing Aggregates](#testing-aggregates)
11. [Related Documentation](#related-documentation)

## Overview

Aggregates in TypeScript define **transactional consistency boundaries** around a cluster of entities and value objects. The aggregate root entity controls all access to child entities, ensuring invariants are maintained across the entire cluster.

**Key Characteristics**:

- **Single root entity**: One entity serves as the entry point for all operations
- **Transactional boundary**: All changes within aggregate are atomic
- **Invariant enforcement**: Root validates consistency rules across all children
- **External references**: Outside objects reference only the root's ID
- **Child entity management**: Arrays of child entities with encapsulated access
- **Domain events**: Array of pending events published after persistence

**TypeScript vs Java Comparison**:

```typescript
// TypeScript: Class with private arrays, branded types
type CampaignID = string & { readonly __brand: "CampaignID" };

class DonationCampaign {
  readonly id: CampaignID; // Immutable identity
  private name: string;
  private goal: Money;
  private readonly donations: Donation[] = []; // Child entities
  private readonly eventQueue: DomainEvent[] = []; // Pending events
  private readonly createdAt: Date;
  private updatedAt: Date;
  private version: number;

  private constructor(params: CampaignParams) {
    this.id = params.id;
    this.name = params.name;
    this.goal = params.goal;
    this.createdAt = params.createdAt;
    this.updatedAt = params.updatedAt;
    this.version = params.version;
  }

  // Factory function
  static create(
    id: CampaignID,
    name: string,
    goal: Money,
    createdBy: UserID,
  ): Result<DonationCampaign, ValidationError> {
    // Validation and instantiation
  }

  // Business method managing children and events
  receiveDonation(donationId: DonationID, amount: Money, donor: Donor, recordedBy: UserID): Result<void, DomainError> {
    // Validate invariants
    const donation = Donation.create(donationId, amount, donor, recordedBy);
    this.donations.push(donation.unwrap());

    // Register event
    this.eventQueue.push(
      DonationReceived.create({
        campaignId: this.id,
        donationId,
        amount,
        donorId: donor.id,
        recordedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }

  // Encapsulated access to children (immutable view)
  getDonations(): readonly Donation[] {
    return this.donations; // Returns readonly array
  }

  // Event retrieval and clearing
  getPendingEvents(): readonly DomainEvent[] {
    return this.eventQueue;
  }

  clearEvents(): void {
    this.eventQueue.length = 0;
  }
}
```

```java
// Java: Class with private lists
public class DonationCampaign {
    private final CampaignId id;
    private String name;
    private Money goal;
    private final List<Donation> donations = new ArrayList<>();
    private final List<DomainEvent> domainEvents = new ArrayList<>();
    private final Instant createdAt;
    private Instant updatedAt;
    private int version;

    private DonationCampaign(CampaignId id, String name, Money goal, UserId createdBy) {
        this.id = id;
        this.name = name;
        this.goal = goal;
        this.createdAt = Instant.now();
        this.updatedAt = Instant.now();
        this.version = 0;
    }

    public static Result<DonationCampaign, ValidationError> create(
        CampaignId id, String name, Money goal, UserId createdBy
    ) {
        // Validation and instantiation
    }

    public Result<Void, DomainError> receiveDonation(
        DonationId donationId, Money amount, Donor donor, UserId recordedBy
    ) {
        Donation donation = Donation.create(donationId, amount, donor, recordedBy);
        this.donations.add(donation);
        this.domainEvents.add(new DonationReceived(/* params */));
        this.markUpdated();
        return Result.ok(null);
    }

    public List<Donation> getDonations() {
        return Collections.unmodifiableList(donations);
    }
}
```

**Critical Differences**:

| Feature              | TypeScript                                   | Java                                   | Go                               |
| -------------------- | -------------------------------------------- | -------------------------------------- | -------------------------------- |
| Child collection     | `private readonly donations: Donation[]`     | `private final List<Donation>`         | `donations []Donation`           |
| Immutable view       | `readonly Donation[]` return type            | `Collections.unmodifiableList()`       | Return copy `[]*Donation`        |
| Events               | `private readonly eventQueue: DomainEvent[]` | `private final List<DomainEvent>`      | `eventQueue []DomainEvent`       |
| Factory              | Static method returning `Result<T, E>`       | Static method returning `Result<T, E>` | Function returning `(*T, error)` |
| Invariant validation | Throws or returns `Result<T, E>`             | Throws or returns `Result<T, E>`       | Returns `error`                  |

## Template Structure

### Basic Aggregate Root Template

```typescript
import { Result, Ok, Err } from "@open-sharia-enterprise/ts-result";
import { DomainError, ValidationError } from "../errors";
import { DomainEvent } from "../events";

// Branded type for aggregate ID
type AggregateID = string & { readonly __brand: "AggregateID" };

interface AggregateParams {
  id: AggregateID;
  // Core properties
  name: string;
  status: AggregateStatus;
  // Child entities (empty arrays initially)
  // Lifecycle timestamps
  createdAt: Date;
  updatedAt: Date;
  deletedAt?: Date;
  version: number;
}

class Aggregate {
  // Identity (immutable)
  readonly id: AggregateID;

  // Core mutable state
  private name: string;
  private status: AggregateStatus;

  // Child entities (encapsulated)
  private readonly children: ChildEntity[] = [];

  // Domain events (encapsulated)
  private readonly eventQueue: DomainEvent[] = [];

  // Lifecycle tracking
  private readonly createdAt: Date;
  private updatedAt: Date;
  private deletedAt?: Date;

  // Optimistic locking
  private version: number;

  // Private constructor - use factory
  private constructor(params: AggregateParams) {
    this.id = params.id;
    this.name = params.name;
    this.status = params.status;
    this.createdAt = params.createdAt;
    this.updatedAt = params.updatedAt;
    this.deletedAt = params.deletedAt;
    this.version = params.version;
  }

  // Factory function for new aggregates
  static create(id: AggregateID, name: string, createdBy: UserID): Result<Aggregate, ValidationError> {
    // Validation
    if (!name || name.trim().length === 0) {
      return Err({ type: "VALIDATION_ERROR", message: "Name is required" });
    }

    const now = new Date();
    return Ok(
      new Aggregate({
        id,
        name,
        status: AggregateStatus.ACTIVE,
        createdAt: now,
        updatedAt: now,
        version: 0,
      }),
    );
  }

  // Reconstitution factory for loading from database
  static reconstitute(params: AggregateParams): Aggregate {
    return new Aggregate(params);
  }

  // Business method managing child entities
  addChild(childId: ChildID, data: ChildData, addedBy: UserID): Result<void, DomainError> {
    // Validate invariants
    if (this.children.length >= 100) {
      return Err({ type: "DOMAIN_ERROR", message: "Maximum children exceeded" });
    }

    // Create child entity
    const childResult = ChildEntity.create(childId, data, this.id, addedBy);
    if (childResult.isErr()) {
      return Err(childResult.unwrapErr());
    }

    // Add to collection
    this.children.push(childResult.unwrap());

    // Register domain event
    this.registerEvent(
      ChildAdded.create({
        aggregateId: this.id,
        childId,
        addedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }

  // Business method updating child
  updateChild(childId: ChildID, newData: ChildData, updatedBy: UserID): Result<void, DomainError> {
    const child = this.children.find((c) => c.id === childId);
    if (!child) {
      return Err({ type: "NOT_FOUND", message: "Child not found" });
    }

    const updateResult = child.update(newData, updatedBy);
    if (updateResult.isErr()) {
      return Err(updateResult.unwrapErr());
    }

    this.registerEvent(
      ChildUpdated.create({
        aggregateId: this.id,
        childId,
        updatedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }

  // Business method removing child
  removeChild(childId: ChildID, removedBy: UserID): Result<void, DomainError> {
    const index = this.children.findIndex((c) => c.id === childId);
    if (index === -1) {
      return Err({ type: "NOT_FOUND", message: "Child not found" });
    }

    this.children.splice(index, 1);

    this.registerEvent(
      ChildRemoved.create({
        aggregateId: this.id,
        childId,
        removedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }

  // Encapsulated child access (immutable view)
  getChildren(): readonly ChildEntity[] {
    return this.children;
  }

  getChild(childId: ChildID): ChildEntity | undefined {
    return this.children.find((c) => c.id === childId);
  }

  // Domain event management
  private registerEvent(event: DomainEvent): void {
    this.eventQueue.push(event);
  }

  getPendingEvents(): readonly DomainEvent[] {
    return this.eventQueue;
  }

  clearEvents(): void {
    this.eventQueue.length = 0;
  }

  // Lifecycle management
  private markUpdated(): void {
    this.updatedAt = new Date();
  }

  softDelete(deletedBy: UserID): Result<void, DomainError> {
    if (this.deletedAt) {
      return Err({ type: "DOMAIN_ERROR", message: "Already deleted" });
    }

    this.deletedAt = new Date();
    this.registerEvent(
      AggregateDeleted.create({
        aggregateId: this.id,
        deletedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }

  // Getters
  getName(): string {
    return this.name;
  }

  getStatus(): AggregateStatus {
    return this.status;
  }

  getCreatedAt(): Date {
    return this.createdAt;
  }

  getUpdatedAt(): Date {
    return this.updatedAt;
  }

  getDeletedAt(): Date | undefined {
    return this.deletedAt;
  }

  getVersion(): number {
    return this.version;
  }

  isDeleted(): boolean {
    return this.deletedAt !== undefined;
  }
}

export { Aggregate, AggregateID, AggregateParams };
```

## Aggregate Root Pattern

### What Makes a Good Aggregate Root?

1. **Single Entry Point**: All external access goes through the root
2. **Invariant Guardian**: Root enforces all consistency rules
3. **Transaction Boundary**: Root defines what changes together
4. **Event Publisher**: Root registers all domain events

### Choosing Aggregate Boundaries

**Good Aggregate Boundary** (Small, cohesive):

```typescript
class Order {
  // Root entity
  readonly id: OrderID;
  private status: OrderStatus;

  // Child entities (part of same aggregate)
  private readonly items: OrderItem[] = [];

  // Value objects
  private shippingAddress: Address;
  private billingAddress: Address;

  // Reference to external aggregate (ID only)
  private customerId: CustomerID; // ← Reference, not embedded
}
```

**Bad Aggregate Boundary** (Too large):

```typescript
class Order {
  readonly id: OrderID;
  private readonly items: OrderItem[] = [];

  // ❌ BAD: Embedded entire Customer aggregate
  private customer: Customer; // Customer is separate aggregate!

  // ❌ BAD: Embedded Product aggregates
  private readonly products: Product[]; // Products are separate!
}
```

**Rule of Thumb**: If updating one object doesn't require updating another in the same transaction, they should be separate aggregates.

## Child Entity Management

### Adding Child Entities

```typescript
class DonationCampaign {
  private readonly donations: Donation[] = [];

  receiveDonation(donationId: DonationID, amount: Money, donor: Donor, recordedBy: UserID): Result<void, DomainError> {
    // Validate business rules
    if (this.isCompleted()) {
      return Err({ type: "DOMAIN_ERROR", message: "Campaign is completed" });
    }

    if (this.getTotalRaised().add(amount).isGreaterThan(this.goal)) {
      return Err({ type: "DOMAIN_ERROR", message: "Would exceed goal" });
    }

    // Create child entity through factory
    const donationResult = Donation.create(donationId, amount, donor, this.id, recordedBy);

    if (donationResult.isErr()) {
      return Err(donationResult.unwrapErr());
    }

    // Add to collection
    const donation = donationResult.unwrap();
    this.donations.push(donation);

    // Register domain event
    this.registerEvent(
      DonationReceived.create({
        campaignId: this.id,
        donationId,
        amount,
        donorId: donor.id,
        recordedBy,
        occurredAt: new Date(),
      }),
    );

    // Check if goal reached
    if (this.getTotalRaised().equals(this.goal)) {
      this.status = CampaignStatus.GOAL_REACHED;
      this.registerEvent(
        CampaignGoalReached.create({
          campaignId: this.id,
          goalAmount: this.goal,
          reachedAt: new Date(),
        }),
      );
    }

    this.markUpdated();
    return Ok(undefined);
  }

  // Calculate derived value from children
  getTotalRaised(): Money {
    return this.donations.reduce((total, donation) => total.add(donation.getAmount()).unwrap(), Money.zero("USD"));
  }

  // Provide immutable view of children
  getDonations(): readonly Donation[] {
    return this.donations;
  }

  getDonation(donationId: DonationID): Donation | undefined {
    return this.donations.find((d) => d.id === donationId);
  }
}
```

### Removing Child Entities

```typescript
class DonationCampaign {
  cancelDonation(donationId: DonationID, reason: string, cancelledBy: UserID): Result<void, DomainError> {
    const index = this.donations.findIndex((d) => d.id === donationId);

    if (index === -1) {
      return Err({ type: "NOT_FOUND", message: "Donation not found" });
    }

    const donation = this.donations[index];

    // Business rule validation
    if (donation.isProcessed()) {
      return Err({
        type: "DOMAIN_ERROR",
        message: "Cannot cancel processed donation",
      });
    }

    // Remove from collection
    this.donations.splice(index, 1);

    // Register event
    this.registerEvent(
      DonationCancelled.create({
        campaignId: this.id,
        donationId,
        reason,
        cancelledBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }
}
```

### Updating Child Entities

```typescript
class DonationCampaign {
  confirmDonation(donationId: DonationID, confirmedBy: UserID): Result<void, DomainError> {
    const donation = this.donations.find((d) => d.id === donationId);

    if (!donation) {
      return Err({ type: "NOT_FOUND", message: "Donation not found" });
    }

    // Update through child entity's business method
    const confirmResult = donation.confirm(confirmedBy);
    if (confirmResult.isErr()) {
      return Err(confirmResult.unwrapErr());
    }

    // Register aggregate-level event
    this.registerEvent(
      DonationConfirmed.create({
        campaignId: this.id,
        donationId,
        confirmedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }
}
```

## Invariant Enforcement

Aggregates are responsible for enforcing **consistency rules** that span multiple entities and value objects within the boundary.

### Examples of Invariants

```typescript
class DonationCampaign {
  // Invariant: Total donations cannot exceed goal
  receiveDonation(donationId: DonationID, amount: Money, donor: Donor, recordedBy: UserID): Result<void, DomainError> {
    const newTotal = this.getTotalRaised().add(amount);

    if (newTotal.isGreaterThan(this.goal)) {
      return Err({
        type: "INVARIANT_VIOLATION",
        message: `Donation would exceed goal. Goal: ${this.goal}, Current: ${this.getTotalRaised()}, Proposed: ${amount}`,
      });
    }

    // Proceed with adding donation
  }

  // Invariant: Campaign must have minimum number of donations before completion
  completeCampaign(completedBy: UserID): Result<void, DomainError> {
    const MINIMUM_DONATIONS = 5;

    if (this.donations.length < MINIMUM_DONATIONS) {
      return Err({
        type: "INVARIANT_VIOLATION",
        message: `Campaign requires at least ${MINIMUM_DONATIONS} donations before completion. Current: ${this.donations.length}`,
      });
    }

    if (!this.getTotalRaised().equals(this.goal)) {
      return Err({
        type: "INVARIANT_VIOLATION",
        message: "Goal must be reached before completion",
      });
    }

    this.status = CampaignStatus.COMPLETED;
    this.registerEvent(
      CampaignCompleted.create({
        campaignId: this.id,
        totalRaised: this.getTotalRaised(),
        donationCount: this.donations.length,
        completedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }

  // Invariant: Donation amounts must be valid for campaign currency
  private validateDonationCurrency(amount: Money): Result<void, DomainError> {
    if (amount.getCurrency() !== this.goal.getCurrency()) {
      return Err({
        type: "INVARIANT_VIOLATION",
        message: `Donation currency ${amount.getCurrency()} does not match campaign currency ${this.goal.getCurrency()}`,
      });
    }
    return Ok(undefined);
  }
}
```

### Validating Invariants Across Children

```typescript
class ZakatAccount {
  private readonly transactions: ZakatTransaction[] = [];
  private balance: Money;

  recordTransaction(
    transactionId: TransactionID,
    amount: Money,
    type: TransactionType,
    recordedBy: UserID,
  ): Result<void, DomainError> {
    // Calculate new balance
    const newBalance = type === "DEBIT" ? this.balance.subtract(amount) : this.balance.add(amount);

    // Invariant: Balance cannot be negative
    if (newBalance.isNegative()) {
      return Err({
        type: "INVARIANT_VIOLATION",
        message: `Transaction would result in negative balance. Current: ${this.balance}, Amount: ${amount}`,
      });
    }

    // Create transaction
    const transactionResult = ZakatTransaction.create(transactionId, amount, type, this.id, recordedBy);

    if (transactionResult.isErr()) {
      return Err(transactionResult.unwrapErr());
    }

    // Update state
    this.balance = newBalance;
    this.transactions.push(transactionResult.unwrap());

    // Register event
    this.registerEvent(
      TransactionRecorded.create({
        accountId: this.id,
        transactionId,
        amount,
        type,
        newBalance: this.balance,
        recordedBy,
        occurredAt: new Date(),
      }),
    );

    this.markUpdated();
    return Ok(undefined);
  }

  // Derived calculation validating child state
  getBalance(): Money {
    // Invariant check: Balance equals sum of transactions
    const calculated = this.transactions.reduce((sum, txn) => {
      return txn.getType() === "DEBIT" ? sum.subtract(txn.getAmount()).unwrap() : sum.add(txn.getAmount()).unwrap();
    }, Money.zero(this.balance.getCurrency()));

    if (!calculated.equals(this.balance)) {
      throw new Error("Balance invariant violated: stored balance does not match transactions");
    }

    return this.balance;
  }
}
```

## Transactional Boundaries

Aggregates define **what must change together** in a single database transaction.

### Single Aggregate Transaction

```typescript
// Application service
class DonationService {
  async receiveDonation(
    campaignId: CampaignID,
    donationId: DonationID,
    amount: Money,
    donor: Donor,
    recordedBy: UserID,
  ): Promise<Result<void, DomainError>> {
    // Begin transaction
    return this.unitOfWork.execute(async (txn) => {
      // Load aggregate
      const campaign = await this.campaignRepository.findById(txn, campaignId);
      if (!campaign) {
        return Err({ type: "NOT_FOUND", message: "Campaign not found" });
      }

      // Execute business logic (in-memory)
      const result = campaign.receiveDonation(donationId, amount, donor, recordedBy);
      if (result.isErr()) {
        return Err(result.unwrapErr());
      }

      // Save aggregate (persist all changes)
      await this.campaignRepository.save(txn, campaign);

      // Publish domain events after successful save
      await this.eventPublisher.publish(campaign.getPendingEvents());
      campaign.clearEvents();

      return Ok(undefined);
    });
  }
}
```

### Multiple Aggregate Consistency (Eventually Consistent)

When a business operation spans multiple aggregates, use **domain events** for eventual consistency instead of trying to update both in one transaction.

```typescript
// ❌ BAD: Updating two aggregates in one transaction
async transferFunds(
  fromAccountId: AccountID,
  toAccountId: AccountID,
  amount: Money
): Promise<Result<void, DomainError>> {
  return this.unitOfWork.execute(async (txn) => {
    // Load both aggregates
    const fromAccount = await this.accountRepo.findById(txn, fromAccountId);
    const toAccount = await this.accountRepo.findById(txn, toAccountId);

    // Update both (creates coupling and performance issues)
    fromAccount.debit(amount);
    toAccount.credit(amount);

    await this.accountRepo.save(txn, fromAccount);
    await this.accountRepo.save(txn, toAccount); // ❌ Two aggregate updates in one transaction
  });
}

// ✅ GOOD: Use events for cross-aggregate consistency
async transferFunds(
  fromAccountId: AccountID,
  toAccountId: AccountID,
  amount: Money,
  initiatedBy: UserID
): Promise<Result<void, DomainError>> {
  // Transaction 1: Debit source account
  const debitResult = await this.unitOfWork.execute(async (txn) => {
    const fromAccount = await this.accountRepo.findById(txn, fromAccountId);
    const result = fromAccount.debit(amount, initiatedBy);

    if (result.isErr()) {
      return Err(result.unwrapErr());
    }

    await this.accountRepo.save(txn, fromAccount);

    // Event: FundsDebited
    await this.eventPublisher.publish(fromAccount.getPendingEvents());
    fromAccount.clearEvents();

    return Ok(undefined);
  });

  if (debitResult.isErr()) {
    return Err(debitResult.unwrapErr());
  }

  // Event handler will trigger Transaction 2: Credit destination account
  return Ok(undefined);
}

// Event handler (separate transaction)
class FundsDebitedHandler {
  async handle(event: FundsDebited): Promise<void> {
    await this.unitOfWork.execute(async (txn) => {
      const toAccount = await this.accountRepo.findById(txn, event.toAccountId);
      toAccount.credit(event.amount, event.initiatedBy);
      await this.accountRepo.save(txn, toAccount);

      await this.eventPublisher.publish(toAccount.getPendingEvents());
      toAccount.clearEvents();
    });
  }
}
```

## Domain Events

Aggregates register domain events that represent significant business occurrences. Events are published after successful persistence.

### Event Registration Pattern

```typescript
class DonationCampaign {
  private readonly eventQueue: DomainEvent[] = [];

  receiveDonation(donationId: DonationID, amount: Money, donor: Donor, recordedBy: UserID): Result<void, DomainError> {
    // Business logic
    const donation = Donation.create(donationId, amount, donor, this.id, recordedBy);
    this.donations.push(donation.unwrap());

    // Register event
    this.registerEvent(
      DonationReceived.create({
        campaignId: this.id,
        donationId,
        amount,
        donorId: donor.id,
        donorName: donor.name,
        recordedBy,
        occurredAt: new Date(),
      }),
    );

    // Check for derived events
    if (this.getTotalRaised().equals(this.goal)) {
      this.status = CampaignStatus.GOAL_REACHED;
      this.registerEvent(
        CampaignGoalReached.create({
          campaignId: this.id,
          goalAmount: this.goal,
          totalDonations: this.donations.length,
          reachedAt: new Date(),
        }),
      );
    }

    this.markUpdated();
    return Ok(undefined);
  }

  private registerEvent(event: DomainEvent): void {
    this.eventQueue.push(event);
  }

  getPendingEvents(): readonly DomainEvent[] {
    return this.eventQueue;
  }

  clearEvents(): void {
    this.eventQueue.length = 0;
  }
}
```

### Event Publishing Flow

```typescript
// Application service coordinates event publishing
class CampaignService {
  async receiveDonation(
    campaignId: CampaignID,
    donationId: DonationID,
    amount: Money,
    donor: Donor,
    recordedBy: UserID,
  ): Promise<Result<void, DomainError>> {
    return this.unitOfWork.execute(async (txn) => {
      // 1. Load aggregate
      const campaign = await this.campaignRepo.findById(txn, campaignId);
      if (!campaign) {
        return Err({ type: "NOT_FOUND", message: "Campaign not found" });
      }

      // 2. Execute business method (registers events in-memory)
      const result = campaign.receiveDonation(donationId, amount, donor, recordedBy);
      if (result.isErr()) {
        return Err(result.unwrapErr());
      }

      // 3. Save aggregate (commit transaction)
      await this.campaignRepo.save(txn, campaign);

      // 4. Publish events AFTER successful save
      const events = campaign.getPendingEvents();
      await this.eventPublisher.publish(events);

      // 5. Clear event queue
      campaign.clearEvents();

      return Ok(undefined);
    });
  }
}
```

## Complete Example: Donation Campaign

Full implementation of a DonationCampaign aggregate with child entities, invariants, and domain events.

```typescript
import { Result, Ok, Err } from "@open-sharia-enterprise/ts-result";

// ========================================
// BRANDED TYPES
// ========================================

type CampaignID = string & { readonly __brand: "CampaignID" };
type DonationID = string & { readonly __brand: "DonationID" };
type DonorID = string & { readonly __brand: "DonorID" };
type UserID = string & { readonly __brand: "UserID" };

// ========================================
// ENUMS AND VALUE OBJECTS
// ========================================

enum CampaignStatus {
  DRAFT = "DRAFT",
  ACTIVE = "ACTIVE",
  GOAL_REACHED = "GOAL_REACHED",
  COMPLETED = "COMPLETED",
  CANCELLED = "CANCELLED",
}

class Money {
  private readonly amount: number;
  private readonly currency: string;

  private constructor(amount: number, currency: string) {
    this.amount = amount;
    this.currency = currency;
  }

  static create(amount: number, currency: string): Result<Money, ValidationError> {
    if (amount < 0) {
      return Err({ type: "VALIDATION_ERROR", message: "Amount cannot be negative" });
    }
    if (!currency || currency.length !== 3) {
      return Err({ type: "VALIDATION_ERROR", message: "Invalid currency code" });
    }
    return Ok(new Money(amount, currency));
  }

  static zero(currency: string): Money {
    return new Money(0, currency);
  }

  getAmount(): number {
    return this.amount;
  }

  getCurrency(): string {
    return this.currency;
  }

  add(other: Money): Result<Money, DomainError> {
    if (this.currency !== other.currency) {
      return Err({ type: "DOMAIN_ERROR", message: "Cannot add different currencies" });
    }
    return Ok(new Money(this.amount + other.amount, this.currency));
  }

  subtract(other: Money): Result<Money, DomainError> {
    if (this.currency !== other.currency) {
      return Err({ type: "DOMAIN_ERROR", message: "Cannot subtract different currencies" });
    }
    return Ok(new Money(this.amount - other.amount, this.currency));
  }

  isGreaterThan(other: Money): boolean {
    return this.amount > other.amount;
  }

  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }

  isNegative(): boolean {
    return this.amount < 0;
  }
}

class Donor {
  readonly id: DonorID;
  private name: string;
  private email: string;

  constructor(id: DonorID, name: string, email: string) {
    this.id = id;
    this.name = name;
    this.email = email;
  }

  getName(): string {
    return this.name;
  }

  getEmail(): string {
    return this.email;
  }
}

// ========================================
// CHILD ENTITY: DONATION
// ========================================

interface DonationParams {
  id: DonationID;
  amount: Money;
  donor: Donor;
  campaignId: CampaignID;
  status: DonationStatus;
  createdAt: Date;
  updatedAt: Date;
  confirmedAt?: Date;
  version: number;
}

enum DonationStatus {
  PENDING = "PENDING",
  CONFIRMED = "CONFIRMED",
  PROCESSED = "PROCESSED",
  CANCELLED = "CANCELLED",
}

class Donation {
  readonly id: DonationID;
  private amount: Money;
  private donor: Donor;
  private campaignId: CampaignID;
  private status: DonationStatus;
  private readonly createdAt: Date;
  private updatedAt: Date;
  private confirmedAt?: Date;
  private version: number;

  private constructor(params: DonationParams) {
    this.id = params.id;
    this.amount = params.amount;
    this.donor = params.donor;
    this.campaignId = params.campaignId;
    this.status = params.status;
    this.createdAt = params.createdAt;
    this.updatedAt = params.updatedAt;
    this.confirmedAt = params.confirmedAt;
    this.version = params.version;
  }

  static create(
    id: DonationID,
    amount: Money,
    donor: Donor,
    campaignId: CampaignID,
    createdBy: UserID,
  ): Result<Donation, ValidationError> {
    const now = new Date();
    return Ok(
      new Donation({
        id,
        amount,
        donor,
        campaignId,
        status: DonationStatus.PENDING,
        createdAt: now,
        updatedAt: now,
        version: 0,
      }),
    );
  }

  confirm(confirmedBy: UserID): Result<void, DomainError> {
    if (this.status !== DonationStatus.PENDING) {
      return Err({
        type: "DOMAIN_ERROR",
        message: `Cannot confirm donation in ${this.status} status`,
      });
    }

    this.status = DonationStatus.CONFIRMED;
    this.confirmedAt = new Date();
    this.markUpdated();
    return Ok(undefined);
  }

  process(processedBy: UserID): Result<void, DomainError> {
    if (this.status !== DonationStatus.CONFIRMED) {
      return Err({
        type: "DOMAIN_ERROR",
        message: "Can only process confirmed donations",
      });
    }

    this.status = DonationStatus.PROCESSED;
    this.markUpdated();
    return Ok(undefined);
  }

  cancel(reason: string, cancelledBy: UserID): Result<void, DomainError> {
    if (this.status === DonationStatus.PROCESSED) {
      return Err({
        type: "DOMAIN_ERROR",
        message: "Cannot cancel processed donation",
      });
    }

    this.status = DonationStatus.CANCELLED;
    this.markUpdated();
    return Ok(undefined);
  }

  private markUpdated(): void {
    this.updatedAt = new Date();
  }

  // Getters
  getAmount(): Money {
    return this.amount;
  }

  getDonor(): Donor {
    return this.donor;
  }

  getCampaignId(): CampaignID {
    return this.campaignId;
  }

  getStatus(): DonationStatus {
    return this.status;
  }

  isProcessed(): boolean {
    return this.status === DonationStatus.PROCESSED;
  }

  isConfirmed(): boolean {
    return this.status === DonationStatus.CONFIRMED;
  }
}

// ========================================
// DOMAIN EVENTS
// ========================================

interface DomainEvent {
  eventId: string;
  occurredAt: Date;
  eventType: string;
}

interface DonationReceivedData {
  campaignId: CampaignID;
  donationId: DonationID;
  amount: Money;
  donorId: DonorID;
  donorName: string;
  recordedBy: UserID;
  occurredAt: Date;
}

class DonationReceived implements DomainEvent {
  readonly eventId: string;
  readonly campaignId: CampaignID;
  readonly donationId: DonationID;
  readonly amount: Money;
  readonly donorId: DonorID;
  readonly donorName: string;
  readonly recordedBy: UserID;
  readonly occurredAt: Date;
  readonly eventType = "DonationReceived";

  private constructor(data: DonationReceivedData) {
    this.eventId = crypto.randomUUID();
    this.campaignId = data.campaignId;
    this.donationId = data.donationId;
    this.amount = data.amount;
    this.donorId = data.donorId;
    this.donorName = data.donorName;
    this.recordedBy = data.recordedBy;
    this.occurredAt = data.occurredAt;
  }

  static create(data: DonationReceivedData): DonationReceived {
    return new DonationReceived(data);
  }
}

interface CampaignGoalReachedData {
  campaignId: CampaignID;
  goalAmount: Money;
  totalDonations: number;
  reachedAt: Date;
}

class CampaignGoalReached implements DomainEvent {
  readonly eventId: string;
  readonly campaignId: CampaignID;
  readonly goalAmount: Money;
  readonly totalDonations: number;
  readonly occurredAt: Date;
  readonly eventType = "CampaignGoalReached";

  private constructor(data: CampaignGoalReachedData) {
    this.eventId = crypto.randomUUID();
    this.campaignId = data.campaignId;
    this.goalAmount = data.goalAmount;
    this.totalDonations = data.totalDonations;
    this.occurredAt = data.reachedAt;
  }

  static create(data: CampaignGoalReachedData): CampaignGoalReached {
    return new CampaignGoalReached(data);
  }
}

// ========================================
// AGGREGATE ROOT: DONATION CAMPAIGN
// ========================================

interface DonationCampaignParams {
  id: CampaignID;
  name: string;
  description: string;
  goal: Money;
  status: CampaignStatus;
  createdAt: Date;
  updatedAt: Date;
  deletedAt?: Date;
  version: number;
}

class DonationCampaign {
  // Identity
  readonly id: CampaignID;

  // Core state
  private name: string;
  private description: string;
  private goal: Money;
  private status: CampaignStatus;

  // Child entities
  private readonly donations: Donation[] = [];

  // Domain events
  private readonly eventQueue: DomainEvent[] = [];

  // Lifecycle
  private readonly createdAt: Date;
  private updatedAt: Date;
  private deletedAt?: Date;
  private version: number;

  private constructor(params: DonationCampaignParams) {
    this.id = params.id;
    this.name = params.name;
    this.description = params.description;
    this.goal = params.goal;
    this.status = params.status;
    this.createdAt = params.createdAt;
    this.updatedAt = params.updatedAt;
    this.deletedAt = params.deletedAt;
    this.version = params.version;
  }

  // Factory for new campaigns
  static create(
    id: CampaignID,
    name: string,
    description: string,
    goal: Money,
    createdBy: UserID,
  ): Result<DonationCampaign, ValidationError> {
    if (!name || name.trim().length === 0) {
      return Err({ type: "VALIDATION_ERROR", message: "Campaign name is required" });
    }

    if (!description || description.trim().length === 0) {
      return Err({ type: "VALIDATION_ERROR", message: "Campaign description is required" });
    }

    if (goal.getAmount() <= 0) {
      return Err({ type: "VALIDATION_ERROR", message: "Goal must be positive" });
    }

    const now = new Date();
    return Ok(
      new DonationCampaign({
        id,
        name,
        description,
        goal,
        status: CampaignStatus.DRAFT,
        createdAt: now,
        updatedAt: now,
        version: 0,
      }),
    );
  }

  // Reconstitution factory
  static reconstitute(params: DonationCampaignParams): DonationCampaign {
    return new DonationCampaign(params);
  }

  // ========================================
  // BUSINESS METHODS
  // ========================================

  activate(activatedBy: UserID): Result<void, DomainError> {
    if (this.status !== CampaignStatus.DRAFT) {
      return Err({
        type: "DOMAIN_ERROR",
        message: "Only draft campaigns can be activated",
      });
    }

    this.status = CampaignStatus.ACTIVE;
    this.markUpdated();
    return Ok(undefined);
  }

  receiveDonation(donationId: DonationID, amount: Money, donor: Donor, recordedBy: UserID): Result<void, DomainError> {
    // Invariant: Campaign must be active
    if (this.status !== CampaignStatus.ACTIVE) {
      return Err({
        type: "DOMAIN_ERROR",
        message: `Cannot receive donations for campaign in ${this.status} status`,
      });
    }

    // Invariant: Currency must match goal
    if (amount.getCurrency() !== this.goal.getCurrency()) {
      return Err({
        type: "DOMAIN_ERROR",
        message: `Donation currency ${amount.getCurrency()} does not match campaign currency ${this.goal.getCurrency()}`,
      });
    }

    // Invariant: Total raised cannot exceed goal
    const newTotal = this.getTotalRaised().add(amount);
    if (newTotal.isErr() || newTotal.unwrap().isGreaterThan(this.goal)) {
      return Err({
        type: "DOMAIN_ERROR",
        message: "Donation would exceed campaign goal",
      });
    }

    // Create child entity
    const donationResult = Donation.create(donationId, amount, donor, this.id, recordedBy);
    if (donationResult.isErr()) {
      return Err(donationResult.unwrapErr());
    }

    // Add to collection
    this.donations.push(donationResult.unwrap());

    // Register event
    this.registerEvent(
      DonationReceived.create({
        campaignId: this.id,
        donationId,
        amount,
        donorId: donor.id,
        donorName: donor.getName(),
        recordedBy,
        occurredAt: new Date(),
      }),
    );

    // Check if goal reached
    const totalRaised = this.getTotalRaised();
    if (totalRaised.equals(this.goal)) {
      this.status = CampaignStatus.GOAL_REACHED;
      this.registerEvent(
        CampaignGoalReached.create({
          campaignId: this.id,
          goalAmount: this.goal,
          totalDonations: this.donations.length,
          reachedAt: new Date(),
        }),
      );
    }

    this.markUpdated();
    return Ok(undefined);
  }

  confirmDonation(donationId: DonationID, confirmedBy: UserID): Result<void, DomainError> {
    const donation = this.donations.find((d) => d.id === donationId);
    if (!donation) {
      return Err({ type: "NOT_FOUND", message: "Donation not found" });
    }

    const confirmResult = donation.confirm(confirmedBy);
    if (confirmResult.isErr()) {
      return Err(confirmResult.unwrapErr());
    }

    this.markUpdated();
    return Ok(undefined);
  }

  cancelDonation(donationId: DonationID, reason: string, cancelledBy: UserID): Result<void, DomainError> {
    const index = this.donations.findIndex((d) => d.id === donationId);
    if (index === -1) {
      return Err({ type: "NOT_FOUND", message: "Donation not found" });
    }

    const donation = this.donations[index];

    // Business rule: Cannot cancel processed donations
    if (donation.isProcessed()) {
      return Err({
        type: "DOMAIN_ERROR",
        message: "Cannot cancel processed donation",
      });
    }

    // Remove from collection
    this.donations.splice(index, 1);

    // If goal was reached, revert status
    if (this.status === CampaignStatus.GOAL_REACHED) {
      const totalRaised = this.getTotalRaised();
      if (totalRaised.isGreaterThan(this.goal) === false) {
        this.status = CampaignStatus.ACTIVE;
      }
    }

    this.markUpdated();
    return Ok(undefined);
  }

  completeCampaign(completedBy: UserID): Result<void, DomainError> {
    const MINIMUM_DONATIONS = 1;

    // Invariant: Must have minimum donations
    if (this.donations.length < MINIMUM_DONATIONS) {
      return Err({
        type: "DOMAIN_ERROR",
        message: `Campaign requires at least ${MINIMUM_DONATIONS} donation(s)`,
      });
    }

    // Invariant: Goal must be reached
    if (!this.getTotalRaised().equals(this.goal)) {
      return Err({
        type: "DOMAIN_ERROR",
        message: "Goal must be reached before completion",
      });
    }

    this.status = CampaignStatus.COMPLETED;
    this.markUpdated();
    return Ok(undefined);
  }

  // ========================================
  // CALCULATED VALUES
  // ========================================

  getTotalRaised(): Money {
    return this.donations.reduce(
      (total, donation) => total.add(donation.getAmount()).unwrap(),
      Money.zero(this.goal.getCurrency()),
    );
  }

  getDonationCount(): number {
    return this.donations.length;
  }

  getConfirmedDonationCount(): number {
    return this.donations.filter((d) => d.isConfirmed()).length;
  }

  // ========================================
  // CHILD ACCESS (IMMUTABLE VIEWS)
  // ========================================

  getDonations(): readonly Donation[] {
    return this.donations;
  }

  getDonation(donationId: DonationID): Donation | undefined {
    return this.donations.find((d) => d.id === donationId);
  }

  // ========================================
  // EVENT MANAGEMENT
  // ========================================

  private registerEvent(event: DomainEvent): void {
    this.eventQueue.push(event);
  }

  getPendingEvents(): readonly DomainEvent[] {
    return this.eventQueue;
  }

  clearEvents(): void {
    this.eventQueue.length = 0;
  }

  // ========================================
  // LIFECYCLE
  // ========================================

  private markUpdated(): void {
    this.updatedAt = new Date();
  }

  softDelete(deletedBy: UserID): Result<void, DomainError> {
    if (this.deletedAt) {
      return Err({ type: "DOMAIN_ERROR", message: "Campaign already deleted" });
    }

    this.deletedAt = new Date();
    this.markUpdated();
    return Ok(undefined);
  }

  // ========================================
  // GETTERS
  // ========================================

  getName(): string {
    return this.name;
  }

  getDescription(): string {
    return this.description;
  }

  getGoal(): Money {
    return this.goal;
  }

  getStatus(): CampaignStatus {
    return this.status;
  }

  getCreatedAt(): Date {
    return this.createdAt;
  }

  getUpdatedAt(): Date {
    return this.updatedAt;
  }

  getDeletedAt(): Date | undefined {
    return this.deletedAt;
  }

  getVersion(): number {
    return this.version;
  }

  isActive(): boolean {
    return this.status === CampaignStatus.ACTIVE;
  }

  isCompleted(): boolean {
    return this.status === CampaignStatus.COMPLETED;
  }

  isDeleted(): boolean {
    return this.deletedAt !== undefined;
  }
}

export {
  DonationCampaign,
  DonationCampaignParams,
  CampaignID,
  CampaignStatus,
  Donation,
  DonationID,
  DonationStatus,
  Donor,
  DonorID,
  Money,
  DonationReceived,
  CampaignGoalReached,
};
```

## TypeScript vs Java/Go Comparison

### Child Entity Storage

**TypeScript**:

```typescript
private readonly donations: Donation[] = [];

// Add child
this.donations.push(donation);

// Remove child
this.donations.splice(index, 1);

// Immutable view
getDonations(): readonly Donation[] {
  return this.donations;
}
```

**Java**:

```java
private final List<Donation> donations = new ArrayList<>();

// Add child
donations.add(donation);

// Remove child
donations.remove(index);

// Immutable view
public List<Donation> getDonations() {
    return Collections.unmodifiableList(donations);
}
```

**Go**:

```go
type Campaign struct {
  donations []Donation // Slice of structs
}

// Add child
c.donations = append(c.donations, donation)

// Remove child
c.donations = append(c.donations[:i], c.donations[i+1:]...)

// Immutable view (return copy)
func (c *Campaign) GetDonations() []*Donation {
  copied := make([]*Donation, len(c.donations))
  for i := range c.donations {
    copied[i] = &c.donations[i]
  }
  return copied
}
```

### Event Management

**TypeScript**:

```typescript
private readonly eventQueue: DomainEvent[] = [];

private registerEvent(event: DomainEvent): void {
  this.eventQueue.push(event);
}

getPendingEvents(): readonly DomainEvent[] {
  return this.eventQueue;
}

clearEvents(): void {
  this.eventQueue.length = 0;
}
```

**Java**:

```java
private final List<DomainEvent> domainEvents = new ArrayList<>();

private void registerEvent(DomainEvent event) {
    domainEvents.add(event);
}

public List<DomainEvent> getPendingEvents() {
    return Collections.unmodifiableList(domainEvents);
}

public void clearEvents() {
    domainEvents.clear();
}
```

**Go**:

```go
type Campaign struct {
  eventQueue []DomainEvent
}

func (c *Campaign) registerEvent(event DomainEvent) {
  c.eventQueue = append(c.eventQueue, event)
}

func (c *Campaign) GetPendingEvents() []DomainEvent {
  return c.eventQueue
}

func (c *Campaign) ClearEvents() {
  c.eventQueue = c.eventQueue[:0]
}
```

### Invariant Validation

**TypeScript**:

```typescript
receiveDonation(
  donationId: DonationID,
  amount: Money,
  donor: Donor,
  recordedBy: UserID
): Result<void, DomainError> {
  if (this.status !== CampaignStatus.ACTIVE) {
    return Err({ type: "DOMAIN_ERROR", message: "Campaign not active" });
  }

  const newTotal = this.getTotalRaised().add(amount);
  if (newTotal.unwrap().isGreaterThan(this.goal)) {
    return Err({ type: "DOMAIN_ERROR", message: "Would exceed goal" });
  }

  // Proceed with business logic
}
```

**Java**:

```java
public Result<Void, DomainError> receiveDonation(
    DonationId donationId,
    Money amount,
    Donor donor,
    UserId recordedBy
) {
    if (this.status != CampaignStatus.ACTIVE) {
        return Result.err(new DomainError("Campaign not active"));
    }

    Money newTotal = this.getTotalRaised().add(amount);
    if (newTotal.isGreaterThan(this.goal)) {
        return Result.err(new DomainError("Would exceed goal"));
    }

    // Proceed
}
```

**Go**:

```go
func (c *Campaign) ReceiveDonation(
  donationId DonationID,
  amount Money,
  donor Donor,
  recordedBy UserID,
) error {
  if c.status != CampaignStatusActive {
    return errors.New("campaign not active")
  }

  newTotal, err := c.GetTotalRaised().Add(amount)
  if err != nil {
    return err
  }
  if newTotal.IsGreaterThan(c.goal) {
    return errors.New("would exceed goal")
  }

  // Proceed
  return nil
}
```

## Testing Aggregates

### Unit Testing Business Logic

```typescript
import { describe, it, expect } from "vitest";

describe("DonationCampaign", () => {
  describe("receiveDonation", () => {
    it("should add donation and register event when valid", () => {
      // Arrange
      const campaignId = "campaign-1" as CampaignID;
      const goal = Money.create(10000, "USD").unwrap();
      const campaign = DonationCampaign.create(
        campaignId,
        "Test Campaign",
        "Description",
        goal,
        "user-1" as UserID,
      ).unwrap();

      campaign.activate("user-1" as UserID);

      const donationId = "donation-1" as DonationID;
      const amount = Money.create(1000, "USD").unwrap();
      const donor = new Donor("donor-1" as DonorID, "John Doe", "john@example.com");

      // Act
      const result = campaign.receiveDonation(donationId, amount, donor, "user-1" as UserID);

      // Assert
      expect(result.isOk()).toBe(true);
      expect(campaign.getDonations()).toHaveLength(1);
      expect(campaign.getTotalRaised().equals(amount)).toBe(true);

      const events = campaign.getPendingEvents();
      expect(events).toHaveLength(1);
      expect(events[0].eventType).toBe("DonationReceived");
    });

    it("should reject donation exceeding goal", () => {
      // Arrange
      const campaignId = "campaign-1" as CampaignID;
      const goal = Money.create(1000, "USD").unwrap();
      const campaign = DonationCampaign.create(
        campaignId,
        "Test Campaign",
        "Description",
        goal,
        "user-1" as UserID,
      ).unwrap();

      campaign.activate("user-1" as UserID);

      const amount = Money.create(2000, "USD").unwrap();
      const donor = new Donor("donor-1" as DonorID, "John Doe", "john@example.com");

      // Act
      const result = campaign.receiveDonation("donation-1" as DonationID, amount, donor, "user-1" as UserID);

      // Assert
      expect(result.isErr()).toBe(true);
      expect(result.unwrapErr().message).toContain("exceed");
    });

    it("should transition to GOAL_REACHED when goal met", () => {
      // Arrange
      const campaignId = "campaign-1" as CampaignID;
      const goal = Money.create(1000, "USD").unwrap();
      const campaign = DonationCampaign.create(
        campaignId,
        "Test Campaign",
        "Description",
        goal,
        "user-1" as UserID,
      ).unwrap();

      campaign.activate("user-1" as UserID);

      const amount = Money.create(1000, "USD").unwrap();
      const donor = new Donor("donor-1" as DonorID, "John Doe", "john@example.com");

      // Act
      campaign.receiveDonation("donation-1" as DonationID, amount, donor, "user-1" as UserID);

      // Assert
      expect(campaign.getStatus()).toBe(CampaignStatus.GOAL_REACHED);

      const events = campaign.getPendingEvents();
      expect(events).toHaveLength(2);
      expect(events[1].eventType).toBe("CampaignGoalReached");
    });
  });

  describe("invariants", () => {
    it("should enforce minimum donation count for completion", () => {
      // Arrange
      const campaignId = "campaign-1" as CampaignID;
      const goal = Money.create(1000, "USD").unwrap();
      const campaign = DonationCampaign.create(
        campaignId,
        "Test Campaign",
        "Description",
        goal,
        "user-1" as UserID,
      ).unwrap();

      // Act
      const result = campaign.completeCampaign("user-1" as UserID);

      // Assert
      expect(result.isErr()).toBe(true);
      expect(result.unwrapErr().message).toContain("minimum");
    });
  });
});
```

### Integration Testing with Repository

```typescript
import { describe, it, expect, beforeEach } from "vitest";

describe("DonationCampaign Integration", () => {
  let repository: DonationCampaignRepository;
  let unitOfWork: UnitOfWork;

  beforeEach(async () => {
    // Setup test database
    repository = new SQLDonationCampaignRepository(testDB);
    unitOfWork = new UnitOfWork(testDB);
  });

  it("should persist aggregate with child entities and events", async () => {
    // Arrange
    const campaignId = "campaign-1" as CampaignID;
    const goal = Money.create(10000, "USD").unwrap();
    const campaign = DonationCampaign.create(
      campaignId,
      "Test Campaign",
      "Description",
      goal,
      "user-1" as UserID,
    ).unwrap();

    campaign.activate("user-1" as UserID);

    const donation1 = Money.create(1000, "USD").unwrap();
    const donor1 = new Donor("donor-1" as DonorID, "John Doe", "john@example.com");
    campaign.receiveDonation("donation-1" as DonationID, donation1, donor1, "user-1" as UserID);

    // Act
    await unitOfWork.execute(async (txn) => {
      await repository.save(txn, campaign);
    });

    // Reload from database
    const reloaded = await unitOfWork.execute(async (txn) => {
      return await repository.findById(txn, campaignId);
    });

    // Assert
    expect(reloaded).toBeDefined();
    expect(reloaded!.getName()).toBe("Test Campaign");
    expect(reloaded!.getDonations()).toHaveLength(1);
    expect(reloaded!.getTotalRaised().getAmount()).toBe(1000);
  });
});
```

## Related Documentation

- **[Entity Template](./entity-template.md)** - Creating entities with identity and lifecycle
- **[Value Object Template](./value-object-template.md)** - Immutable value objects
- **[Domain Event Template](./domain-event-template.md)** - Domain events for communication
- **[Repository Template](./repository-template.md)** - Persisting aggregates
- **[Service Layer Template](./service-layer-template.md)** - Orchestrating aggregates
- **[TypeScript Best Practices](../ex-so-stla-ts__best-practices.md)** - TypeScript coding standards
- **[TypeScript Type Safety](../ex-so-stla-ts__type-safety.md)** - Leveraging TypeScript's type system

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
