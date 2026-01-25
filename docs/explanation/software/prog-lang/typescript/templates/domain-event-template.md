---
title: TypeScript Domain Event Template
description: Template for creating Domain-Driven Design domain events in TypeScript with immutable design, event handlers, publishing patterns, and event versioning
category: template
tags:
  - typescript
  - ddd
  - domain-events
  - event-driven
  - event-sourcing
  - immutability
  - cqrs
  - messaging
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.9
related:
  - aggregate-template.md
  - entity-template.md
  - value-object-template.md
  - ex-so-stla-ts__domain-driven-design.md
  - ex-so-stla-ts__concurrency-and-parallelism.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript Domain Event Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) domain events in TypeScript. Domain events represent **facts that have already happened** in the business domain and are fundamental to event-driven architectures, CQRS, and event sourcing patterns.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Event Interface](#event-interface)
4. [Immutable Design](#immutable-design)
5. [Naming Convention](#naming-convention)
6. [Event Versioning](#event-versioning)
7. [Event Handlers](#event-handlers)
8. [Event Publishing](#event-publishing)
9. [Event Sourcing](#event-sourcing)
10. [Complete Example: Zakat Processing](#complete-example-zakat-processing)
11. [TypeScript vs Java/Go Comparison](#typescript-vs-javago-comparison)
12. [Testing Domain Events](#testing-domain-events)
13. [Related Documentation](#related-documentation)

## Overview

Domain events in TypeScript represent **immutable facts** about significant business occurrences. Events communicate what has happened in the domain, enabling loose coupling between aggregates, supporting audit trails, and forming the foundation for event-driven architectures.

**Key Characteristics**:

- **Immutable**: Once created, events never change (all fields readonly)
- **Past-tense naming**: Events represent facts that happened (ZakatPaymentProcessed, not ProcessZakatPayment)
- **Rich with domain meaning**: Events carry business-relevant information
- **Temporal**: Events include timestamp of occurrence
- **Causality tracking**: Events record who/what triggered them
- **No behavior**: Events are pure data structures with no business logic
- **Versioned**: Events support schema evolution with version field

**TypeScript vs Java Comparison**:

```typescript
// TypeScript: Class with readonly fields
class ZakatPaymentProcessed implements DomainEvent {
  readonly eventId: string;
  readonly aggregateId: ZakatID;
  readonly amount: Money;
  readonly payerId: DonorID;
  readonly processedBy: UserID;
  readonly occurredAt: Date;
  readonly version: number;
  readonly eventType = "ZakatPaymentProcessed";

  private constructor(data: ZakatPaymentProcessedData) {
    this.eventId = crypto.randomUUID();
    this.aggregateId = data.aggregateId;
    this.amount = data.amount;
    this.payerId = data.payerId;
    this.processedBy = data.processedBy;
    this.occurredAt = data.occurredAt;
    this.version = 1;
  }

  static create(data: ZakatPaymentProcessedData): ZakatPaymentProcessed {
    return new ZakatPaymentProcessed(data);
  }
}
```

```java
// Java: Class with private final fields
public final class ZakatPaymentProcessed implements DomainEvent {
    private final UUID eventId;
    private final ZakatId aggregateId;
    private final Money amount;
    private final DonorId payerId;
    private final UserId processedBy;
    private final Instant occurredAt;
    private final int version;

    private ZakatPaymentProcessed(Builder builder) {
        this.eventId = UUID.randomUUID();
        this.aggregateId = requireNonNull(builder.aggregateId);
        this.amount = requireNonNull(builder.amount);
        // ... other fields
        this.version = 1;
    }

    public static Builder builder() {
        return new Builder();
    }

    // Only getters
    public UUID eventId() { return eventId; }
    public ZakatId aggregateId() { return aggregateId; }
}
```

**Critical Differences**:

| Feature      | TypeScript                           | Java                          | Go                          |
| ------------ | ------------------------------------ | ----------------------------- | --------------------------- |
| Immutability | `readonly` keyword                   | `private final`               | Struct with exported fields |
| Constructor  | Private constructor + static factory | Private constructor + builder | Direct struct literal       |
| Field access | Public readonly fields               | Private fields + getters      | Public fields (exported)    |
| Type safety  | Branded types for IDs                | Wrapper classes for IDs       | Type aliases                |
| Event ID     | `crypto.randomUUID()`                | `UUID.randomUUID()`           | `uuid.New()`                |

## Template Structure

### Basic Domain Event Template

```typescript
// Domain event interface (all events implement this)
interface DomainEvent {
  eventId: string;
  occurredAt: Date;
  eventType: string;
  version: number;
}

// Event-specific data interface
interface EventNameData {
  aggregateId: AggregateID;
  // Business-relevant fields
  field1: ValueType;
  field2: ValueType;
  // Causality tracking
  triggeredBy: UserID;
  occurredAt: Date;
}

// Event implementation
class EventName implements DomainEvent {
  // Standard event fields
  readonly eventId: string;
  readonly occurredAt: Date;
  readonly eventType = "EventName"; // Literal type for event type
  readonly version: number;

  // Event-specific fields (readonly)
  readonly aggregateId: AggregateID;
  readonly field1: ValueType;
  readonly field2: ValueType;
  readonly triggeredBy: UserID;

  // Private constructor - enforce factory usage
  private constructor(data: EventNameData) {
    this.eventId = crypto.randomUUID();
    this.aggregateId = data.aggregateId;
    this.field1 = data.field1;
    this.field2 = data.field2;
    this.triggeredBy = data.triggeredBy;
    this.occurredAt = data.occurredAt;
    this.version = 1; // Current schema version
  }

  // Factory function
  static create(data: EventNameData): EventName {
    return new EventName(data);
  }

  // Optional: Serialization for persistence
  toJSON(): Record<string, unknown> {
    return {
      eventId: this.eventId,
      eventType: this.eventType,
      version: this.version,
      aggregateId: this.aggregateId,
      field1: this.field1,
      field2: this.field2,
      triggeredBy: this.triggeredBy,
      occurredAt: this.occurredAt.toISOString(),
    };
  }

  // Optional: Deserialization from persistence
  static fromJSON(json: Record<string, unknown>): EventName {
    return new EventName({
      aggregateId: json.aggregateId as AggregateID,
      field1: json.field1 as ValueType,
      field2: json.field2 as ValueType,
      triggeredBy: json.triggeredBy as UserID,
      occurredAt: new Date(json.occurredAt as string),
    });
  }
}

export { EventName, EventNameData };
```

## Event Interface

Define a common interface that all domain events implement:

```typescript
// Base domain event interface
interface DomainEvent {
  // Unique event identifier
  eventId: string;

  // When the event occurred
  occurredAt: Date;

  // Event type discriminator (for deserialization)
  eventType: string;

  // Schema version (for evolution)
  version: number;
}

// Type guard for domain events
function isDomainEvent(obj: unknown): obj is DomainEvent {
  return (
    typeof obj === "object" &&
    obj !== null &&
    "eventId" in obj &&
    "occurredAt" in obj &&
    "eventType" in obj &&
    "version" in obj
  );
}

// Union type for all domain events
type AllDomainEvents = DonationReceived | CampaignGoalReached | ZakatPaymentProcessed | ZakatCalculated;

export { DomainEvent, AllDomainEvents, isDomainEvent };
```

## Immutable Design

Events MUST be immutable - once created, they never change.

### Using readonly Keyword

```typescript
class DonationReceived implements DomainEvent {
  // All fields are readonly
  readonly eventId: string;
  readonly campaignId: CampaignID;
  readonly donationId: DonationID;
  readonly amount: Money;
  readonly donorId: DonorID;
  readonly donorName: string;
  readonly recordedBy: UserID;
  readonly occurredAt: Date;
  readonly eventType = "DonationReceived";
  readonly version: number;

  private constructor(data: DonationReceivedData) {
    this.eventId = crypto.randomUUID();
    this.campaignId = data.campaignId;
    this.donationId = data.donationId;
    this.amount = data.amount; // Money is already immutable value object
    this.donorId = data.donorId;
    this.donorName = data.donorName;
    this.recordedBy = data.recordedBy;
    this.occurredAt = data.occurredAt;
    this.version = 1;
  }

  static create(data: DonationReceivedData): DonationReceived {
    return new DonationReceived(data);
  }
}
```

### Freezing Objects (Optional Defense)

```typescript
class DonationReceived implements DomainEvent {
  // ... fields ...

  static create(data: DonationReceivedData): DonationReceived {
    const event = new DonationReceived(data);
    return Object.freeze(event); // Deep immutability guarantee
  }
}
```

### Immutable Value Objects in Events

```typescript
class DonationReceived implements DomainEvent {
  readonly eventId: string;
  readonly amount: Money; // Money is immutable value object
  readonly metadata: EventMetadata; // Immutable metadata object

  private constructor(data: DonationReceivedData) {
    this.eventId = crypto.randomUUID();
    this.amount = data.amount; // Already immutable
    // Create immutable metadata
    this.metadata = Object.freeze({
      correlationId: data.correlationId,
      causationId: data.causationId,
      userId: data.recordedBy,
    });
  }
}
```

## Naming Convention

Domain events MUST use **past-tense naming** to indicate facts that have already happened.

### Correct Event Names

```typescript
// ✅ GOOD: Past tense verbs
class DonationReceived implements DomainEvent {}
class CampaignGoalReached implements DomainEvent {}
class ZakatPaymentProcessed implements DomainEvent {}
class ZakatCalculated implements DomainEvent {}
class UserRegistered implements DomainEvent {}
class OrderPlaced implements DomainEvent {}
class InvoiceIssued implements DomainEvent {}
class PaymentCompleted implements DomainEvent {}
```

### Incorrect Event Names

```typescript
// ❌ BAD: Present tense or imperative
class ReceiveDonation implements DomainEvent {} // Should be DonationReceived
class ProcessPayment implements DomainEvent {} // Should be PaymentProcessed
class CalculateZakat implements DomainEvent {} // Should be ZakatCalculated
class RegisterUser implements DomainEvent {} // Should be UserRegistered

// ❌ BAD: Nouns without verb
class Donation implements DomainEvent {} // Should be DonationReceived
class Payment implements DomainEvent {} // Should be PaymentProcessed
```

### Business-Meaningful Names

```typescript
// ✅ GOOD: Names reflect business events
class CampaignGoalReached implements DomainEvent {}
class DonorRecognitionThresholdExceeded implements DomainEvent {}
class ZakatEligibilityDetermined implements DomainEvent {}

// ❌ BAD: Technical names without business context
class DataUpdated implements DomainEvent {}
class RecordSaved implements DomainEvent {}
class StateChanged implements DomainEvent {}
```

## Event Versioning

Events must support schema evolution for long-lived systems.

### Version Field

```typescript
class DonationReceived implements DomainEvent {
  readonly eventId: string;
  readonly version: number; // Schema version

  private constructor(data: DonationReceivedData) {
    this.eventId = crypto.randomUUID();
    this.version = 2; // Current schema version
    // ... other fields
  }
}
```

### Handling Version Evolution

```typescript
// Version 1 (original)
interface DonationReceivedDataV1 {
  campaignId: CampaignID;
  donationId: DonationID;
  amount: number; // Just a number
  donorId: DonorID;
  occurredAt: Date;
}

// Version 2 (enhanced with Money value object)
interface DonationReceivedDataV2 {
  campaignId: CampaignID;
  donationId: DonationID;
  amount: Money; // Now a value object
  donorId: DonorID;
  donorName: string; // New field
  occurredAt: Date;
}

class DonationReceived implements DomainEvent {
  readonly eventId: string;
  readonly version: number;
  readonly amount: Money;
  readonly donorName: string;

  private constructor(data: DonationReceivedDataV2) {
    this.eventId = crypto.randomUUID();
    this.version = 2;
    this.amount = data.amount;
    this.donorName = data.donorName;
    // ... other fields
  }

  static create(data: DonationReceivedDataV2): DonationReceived {
    return new DonationReceived(data);
  }

  // Deserialization with version handling
  static fromJSON(json: Record<string, unknown>): DonationReceived {
    const version = json.version as number;

    if (version === 1) {
      // Migrate from V1 to V2
      return new DonationReceived({
        campaignId: json.campaignId as CampaignID,
        donationId: json.donationId as DonationID,
        amount: Money.create(json.amount as number, "USD").unwrap(), // Convert number to Money
        donorId: json.donorId as DonorID,
        donorName: "Unknown Donor", // Default value for missing field
        occurredAt: new Date(json.occurredAt as string),
      });
    }

    // V2 (current version)
    return new DonationReceived({
      campaignId: json.campaignId as CampaignID,
      donationId: json.donationId as DonationID,
      amount: Money.fromJSON(json.amount as Record<string, unknown>),
      donorId: json.donorId as DonorID,
      donorName: json.donorName as string,
      occurredAt: new Date(json.occurredAt as string),
    });
  }
}
```

## Event Handlers

Event handlers respond to domain events, implementing side effects or triggering downstream processes.

### Handler Interface

```typescript
interface EventHandler<T extends DomainEvent> {
  handle(event: T): Promise<void>;
}

// Type-safe handler registration
type EventHandlerMap = {
  [K in AllDomainEvents["eventType"]]: EventHandler<Extract<AllDomainEvents, { eventType: K }>>[];
};
```

### Implementing Handlers

```typescript
class DonationReceivedHandler implements EventHandler<DonationReceived> {
  constructor(
    private readonly notificationService: NotificationService,
    private readonly analyticsService: AnalyticsService,
    private readonly logger: Logger,
  ) {}

  async handle(event: DonationReceived): Promise<void> {
    this.logger.info("Handling DonationReceived event", {
      eventId: event.eventId,
      donationId: event.donationId,
    });

    try {
      // Send thank you email
      await this.notificationService.sendThankYouEmail({
        donorEmail: event.donorEmail,
        amount: event.amount,
        campaignName: event.campaignName,
      });

      // Track analytics
      await this.analyticsService.trackDonation({
        donationId: event.donationId,
        amount: event.amount,
        timestamp: event.occurredAt,
      });

      this.logger.info("DonationReceived event handled successfully");
    } catch (error) {
      this.logger.error("Failed to handle DonationReceived event", error);
      throw error; // Rethrow for retry mechanism
    }
  }
}
```

### Handler Registration

```typescript
class EventBus {
  private handlers: Map<string, EventHandler<DomainEvent>[]> = new Map();

  register<T extends DomainEvent>(eventType: string, handler: EventHandler<T>): void {
    const existing = this.handlers.get(eventType) || [];
    this.handlers.set(eventType, [...existing, handler as EventHandler<DomainEvent>]);
  }

  async publish(event: DomainEvent): Promise<void> {
    const handlers = this.handlers.get(event.eventType) || [];

    // Execute handlers in parallel
    await Promise.all(handlers.map((handler) => handler.handle(event)));
  }
}

// Usage
const eventBus = new EventBus();
eventBus.register("DonationReceived", new DonationReceivedHandler(/* dependencies */));
eventBus.register("DonationReceived", new DonationAnalyticsHandler(/* dependencies */));
```

## Event Publishing

Events are published after successful aggregate persistence.

### Publishing Pattern

```typescript
// Application service
class DonationService {
  constructor(
    private readonly campaignRepo: CampaignRepository,
    private readonly eventPublisher: EventPublisher,
    private readonly unitOfWork: UnitOfWork,
    private readonly logger: Logger,
  ) {}

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

      // 2. Execute business logic (registers events)
      const result = campaign.receiveDonation(donationId, amount, donor, recordedBy);
      if (result.isErr()) {
        return Err(result.unwrapErr());
      }

      // 3. Save aggregate
      await this.campaignRepo.save(txn, campaign);

      // 4. Publish events AFTER successful save
      const events = campaign.getPendingEvents();
      this.logger.info(`Publishing ${events.length} events`);

      await this.eventPublisher.publish(events);

      // 5. Clear event queue
      campaign.clearEvents();

      return Ok(undefined);
    });
  }
}
```

### Event Publisher Interface

```typescript
interface EventPublisher {
  publish(events: readonly DomainEvent[]): Promise<void>;
  publish(event: DomainEvent): Promise<void>;
}

class InMemoryEventPublisher implements EventPublisher {
  constructor(private readonly eventBus: EventBus) {}

  async publish(eventsOrEvent: readonly DomainEvent[] | DomainEvent): Promise<void> {
    const events = Array.isArray(eventsOrEvent) ? eventsOrEvent : [eventsOrEvent];

    for (const event of events) {
      await this.eventBus.publish(event);
    }
  }
}

class MessageQueueEventPublisher implements EventPublisher {
  constructor(
    private readonly messageQueue: MessageQueue,
    private readonly logger: Logger,
  ) {}

  async publish(eventsOrEvent: readonly DomainEvent[] | DomainEvent): Promise<void> {
    const events = Array.isArray(eventsOrEvent) ? eventsOrEvent : [eventsOrEvent];

    for (const event of events) {
      await this.messageQueue.send({
        topic: event.eventType,
        payload: event.toJSON(),
        metadata: {
          eventId: event.eventId,
          occurredAt: event.occurredAt,
          version: event.version,
        },
      });

      this.logger.info(`Published event ${event.eventType}`, { eventId: event.eventId });
    }
  }
}
```

## Event Sourcing

Event sourcing stores events as the source of truth, reconstructing state from event history.

### Event Store Interface

```typescript
interface EventStore {
  append(aggregateId: string, events: DomainEvent[]): Promise<void>;
  getEvents(aggregateId: string): Promise<DomainEvent[]>;
  getEventsSince(aggregateId: string, version: number): Promise<DomainEvent[]>;
}
```

### Storing Events

```typescript
class PostgresEventStore implements EventStore {
  constructor(private readonly db: Database) {}

  async append(aggregateId: string, events: DomainEvent[]): Promise<void> {
    const queries = events.map((event, index) => ({
      text: `
        INSERT INTO events (event_id, aggregate_id, event_type, version, payload, occurred_at)
        VALUES ($1, $2, $3, $4, $5, $6)
      `,
      values: [
        event.eventId,
        aggregateId,
        event.eventType,
        event.version,
        JSON.stringify(event.toJSON()),
        event.occurredAt,
      ],
    }));

    await this.db.transaction(async (txn) => {
      for (const query of queries) {
        await txn.query(query);
      }
    });
  }

  async getEvents(aggregateId: string): Promise<DomainEvent[]> {
    const result = await this.db.query("SELECT * FROM events WHERE aggregate_id = $1 ORDER BY occurred_at ASC", [
      aggregateId,
    ]);

    return result.rows.map((row) => this.deserializeEvent(row));
  }

  private deserializeEvent(row: EventRow): DomainEvent {
    const payload = JSON.parse(row.payload);

    switch (row.event_type) {
      case "DonationReceived":
        return DonationReceived.fromJSON(payload);
      case "CampaignGoalReached":
        return CampaignGoalReached.fromJSON(payload);
      default:
        throw new Error(`Unknown event type: ${row.event_type}`);
    }
  }
}
```

### Reconstructing Aggregates from Events

```typescript
class DonationCampaign {
  // ... aggregate fields ...

  // Reconstitute from events
  static fromEvents(events: DomainEvent[]): DonationCampaign {
    if (events.length === 0) {
      throw new Error("Cannot reconstitute aggregate from empty event stream");
    }

    // Create empty aggregate
    const campaign = new DonationCampaign({ /* initial state */ });

    // Apply each event in sequence
    for (const event of events) {
      campaign.applyEvent(event);
    }

    return campaign;
  }

  private applyEvent(event: DomainEvent): void {
    switch (event.eventType) {
      case "CampaignCreated":
        this.applyCandidate campaign(event as CampaignCreated);
        break;
      case "DonationReceived":
        this.applyDonationReceived(event as DonationReceived);
        break;
      case "CampaignGoalReached":
        this.applyCampaignGoalReached(event as CampaignGoalReached);
        break;
      default:
        throw new Error(`Unknown event type: ${event.eventType}`);
    }
  }

  private applyCampaignCreated(event: CampaignCreated): void {
    this.id = event.campaignId;
    this.name = event.name;
    this.goal = event.goal;
    this.status = CampaignStatus.DRAFT;
    this.createdAt = event.occurredAt;
    this.updatedAt = event.occurredAt;
  }

  private applyDonationReceived(event: DonationReceived): void {
    const donation = Donation.reconstitute({
      id: event.donationId,
      amount: event.amount,
      donor: event.donor,
      campaignId: this.id,
      status: DonationStatus.PENDING,
      createdAt: event.occurredAt,
      updatedAt: event.occurredAt,
      version: 0
    });

    this.donations.push(donation);
    this.updatedAt = event.occurredAt;
  }

  private applyCampaignGoalReached(event: CampaignGoalReached): void {
    this.status = CampaignStatus.GOAL_REACHED;
    this.updatedAt = event.occurredAt;
  }
}
```

## Complete Example: Zakat Processing

Full implementation of Zakat-related domain events.

```typescript
import { Result, Ok, Err } from "@open-sharia-enterprise/ts-result";

// ========================================
// BRANDED TYPES
// ========================================

type ZakatID = string & { readonly __brand: "ZakatID" };
type DonorID = string & { readonly __brand: "DonorID" };
type UserID = string & { readonly __brand: "UserID" };
type AccountID = string & { readonly __brand: "AccountID" };

// ========================================
// VALUE OBJECTS
// ========================================

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
    return Ok(new Money(amount, currency));
  }

  getAmount(): number {
    return this.amount;
  }

  getCurrency(): string {
    return this.currency;
  }

  toJSON(): Record<string, unknown> {
    return { amount: this.amount, currency: this.currency };
  }

  static fromJSON(json: Record<string, unknown>): Money {
    return new Money(json.amount as number, json.currency as string);
  }
}

// ========================================
// DOMAIN EVENT INTERFACE
// ========================================

interface DomainEvent {
  eventId: string;
  occurredAt: Date;
  eventType: string;
  version: number;
}

// ========================================
// EVENT 1: ZAKAT CALCULATED
// ========================================

interface ZakatCalculatedData {
  zakatId: ZakatID;
  accountId: AccountID;
  zakatAmount: Money;
  nisabThreshold: Money;
  zakatableAssets: Money;
  calculationMethod: string;
  calculatedBy: UserID;
  occurredAt: Date;
}

class ZakatCalculated implements DomainEvent {
  readonly eventId: string;
  readonly zakatId: ZakatID;
  readonly accountId: AccountID;
  readonly zakatAmount: Money;
  readonly nisabThreshold: Money;
  readonly zakatableAssets: Money;
  readonly calculationMethod: string;
  readonly calculatedBy: UserID;
  readonly occurredAt: Date;
  readonly eventType = "ZakatCalculated";
  readonly version: number;

  private constructor(data: ZakatCalculatedData) {
    this.eventId = crypto.randomUUID();
    this.zakatId = data.zakatId;
    this.accountId = data.accountId;
    this.zakatAmount = data.zakatAmount;
    this.nisabThreshold = data.nisabThreshold;
    this.zakatableAssets = data.zakatableAssets;
    this.calculationMethod = data.calculationMethod;
    this.calculatedBy = data.calculatedBy;
    this.occurredAt = data.occurredAt;
    this.version = 1;
  }

  static create(data: ZakatCalculatedData): ZakatCalculated {
    return new ZakatCalculated(data);
  }

  toJSON(): Record<string, unknown> {
    return {
      eventId: this.eventId,
      eventType: this.eventType,
      version: this.version,
      zakatId: this.zakatId,
      accountId: this.accountId,
      zakatAmount: this.zakatAmount.toJSON(),
      nisabThreshold: this.nisabThreshold.toJSON(),
      zakatableAssets: this.zakatableAssets.toJSON(),
      calculationMethod: this.calculationMethod,
      calculatedBy: this.calculatedBy,
      occurredAt: this.occurredAt.toISOString(),
    };
  }

  static fromJSON(json: Record<string, unknown>): ZakatCalculated {
    return new ZakatCalculated({
      zakatId: json.zakatId as ZakatID,
      accountId: json.accountId as AccountID,
      zakatAmount: Money.fromJSON(json.zakatAmount as Record<string, unknown>),
      nisabThreshold: Money.fromJSON(json.nisabThreshold as Record<string, unknown>),
      zakatableAssets: Money.fromJSON(json.zakatableAssets as Record<string, unknown>),
      calculationMethod: json.calculationMethod as string,
      calculatedBy: json.calculatedBy as UserID,
      occurredAt: new Date(json.occurredAt as string),
    });
  }
}

// ========================================
// EVENT 2: ZAKAT PAYMENT PROCESSED
// ========================================

interface ZakatPaymentProcessedData {
  zakatId: ZakatID;
  paymentId: string;
  amount: Money;
  payerId: DonorID;
  paymentMethod: string;
  transactionReference: string;
  processedBy: UserID;
  occurredAt: Date;
}

class ZakatPaymentProcessed implements DomainEvent {
  readonly eventId: string;
  readonly zakatId: ZakatID;
  readonly paymentId: string;
  readonly amount: Money;
  readonly payerId: DonorID;
  readonly paymentMethod: string;
  readonly transactionReference: string;
  readonly processedBy: UserID;
  readonly occurredAt: Date;
  readonly eventType = "ZakatPaymentProcessed";
  readonly version: number;

  private constructor(data: ZakatPaymentProcessedData) {
    this.eventId = crypto.randomUUID();
    this.zakatId = data.zakatId;
    this.paymentId = data.paymentId;
    this.amount = data.amount;
    this.payerId = data.payerId;
    this.paymentMethod = data.paymentMethod;
    this.transactionReference = data.transactionReference;
    this.processedBy = data.processedBy;
    this.occurredAt = data.occurredAt;
    this.version = 1;
  }

  static create(data: ZakatPaymentProcessedData): ZakatPaymentProcessed {
    return new ZakatPaymentProcessed(data);
  }

  toJSON(): Record<string, unknown> {
    return {
      eventId: this.eventId,
      eventType: this.eventType,
      version: this.version,
      zakatId: this.zakatId,
      paymentId: this.paymentId,
      amount: this.amount.toJSON(),
      payerId: this.payerId,
      paymentMethod: this.paymentMethod,
      transactionReference: this.transactionReference,
      processedBy: this.processedBy,
      occurredAt: this.occurredAt.toISOString(),
    };
  }

  static fromJSON(json: Record<string, unknown>): ZakatPaymentProcessed {
    return new ZakatPaymentProcessed({
      zakatId: json.zakatId as ZakatID,
      paymentId: json.paymentId as string,
      amount: Money.fromJSON(json.amount as Record<string, unknown>),
      payerId: json.payerId as DonorID,
      paymentMethod: json.paymentMethod as string,
      transactionReference: json.transactionReference as string,
      processedBy: json.processedBy as UserID,
      occurredAt: new Date(json.occurredAt as string),
    });
  }
}

// ========================================
// EVENT 3: ZAKAT DISTRIBUTION COMPLETED
// ========================================

interface ZakatDistributionCompletedData {
  zakatId: ZakatID;
  distributionId: string;
  totalAmount: Money;
  recipientCount: number;
  distributions: Array<{
    recipientId: string;
    amount: Money;
    category: string;
  }>;
  completedBy: UserID;
  occurredAt: Date;
}

class ZakatDistributionCompleted implements DomainEvent {
  readonly eventId: string;
  readonly zakatId: ZakatID;
  readonly distributionId: string;
  readonly totalAmount: Money;
  readonly recipientCount: number;
  readonly distributions: ReadonlyArray<{
    recipientId: string;
    amount: Money;
    category: string;
  }>;
  readonly completedBy: UserID;
  readonly occurredAt: Date;
  readonly eventType = "ZakatDistributionCompleted";
  readonly version: number;

  private constructor(data: ZakatDistributionCompletedData) {
    this.eventId = crypto.randomUUID();
    this.zakatId = data.zakatId;
    this.distributionId = data.distributionId;
    this.totalAmount = data.totalAmount;
    this.recipientCount = data.recipientCount;
    this.distributions = Object.freeze([...data.distributions]);
    this.completedBy = data.completedBy;
    this.occurredAt = data.occurredAt;
    this.version = 1;
  }

  static create(data: ZakatDistributionCompletedData): ZakatDistributionCompleted {
    return new ZakatDistributionCompleted(data);
  }

  toJSON(): Record<string, unknown> {
    return {
      eventId: this.eventId,
      eventType: this.eventType,
      version: this.version,
      zakatId: this.zakatId,
      distributionId: this.distributionId,
      totalAmount: this.totalAmount.toJSON(),
      recipientCount: this.recipientCount,
      distributions: this.distributions.map((d) => ({
        recipientId: d.recipientId,
        amount: d.amount.toJSON(),
        category: d.category,
      })),
      completedBy: this.completedBy,
      occurredAt: this.occurredAt.toISOString(),
    };
  }

  static fromJSON(json: Record<string, unknown>): ZakatDistributionCompleted {
    const distributions = (json.distributions as Array<Record<string, unknown>>).map((d) => ({
      recipientId: d.recipientId as string,
      amount: Money.fromJSON(d.amount as Record<string, unknown>),
      category: d.category as string,
    }));

    return new ZakatDistributionCompleted({
      zakatId: json.zakatId as ZakatID,
      distributionId: json.distributionId as string,
      totalAmount: Money.fromJSON(json.totalAmount as Record<string, unknown>),
      recipientCount: json.recipientCount as number,
      distributions,
      completedBy: json.completedBy as UserID,
      occurredAt: new Date(json.occurredAt as string),
    });
  }
}

// ========================================
// EVENT HANDLER EXAMPLE
// ========================================

interface EventHandler<T extends DomainEvent> {
  handle(event: T): Promise<void>;
}

class ZakatCalculatedHandler implements EventHandler<ZakatCalculated> {
  constructor(
    private readonly notificationService: NotificationService,
    private readonly logger: Logger,
  ) {}

  async handle(event: ZakatCalculated): Promise<void> {
    this.logger.info("Handling ZakatCalculated event", {
      eventId: event.eventId,
      zakatId: event.zakatId,
      amount: event.zakatAmount.getAmount(),
    });

    try {
      // Notify user of calculated Zakat
      await this.notificationService.notifyZakatCalculated({
        userId: event.calculatedBy,
        zakatAmount: event.zakatAmount,
        nisabThreshold: event.nisabThreshold,
      });

      this.logger.info("ZakatCalculated notification sent successfully");
    } catch (error) {
      this.logger.error("Failed to send ZakatCalculated notification", error);
      throw error;
    }
  }
}

class ZakatPaymentProcessedHandler implements EventHandler<ZakatPaymentProcessed> {
  constructor(
    private readonly receiptService: ReceiptService,
    private readonly analyticsService: AnalyticsService,
    private readonly logger: Logger,
  ) {}

  async handle(event: ZakatPaymentProcessed): Promise<void> {
    this.logger.info("Handling ZakatPaymentProcessed event", {
      eventId: event.eventId,
      paymentId: event.paymentId,
    });

    try {
      // Generate receipt
      await this.receiptService.generateZakatReceipt({
        paymentId: event.paymentId,
        payerId: event.payerId,
        amount: event.amount,
        transactionReference: event.transactionReference,
      });

      // Track analytics
      await this.analyticsService.trackZakatPayment({
        zakatId: event.zakatId,
        amount: event.amount,
        paymentMethod: event.paymentMethod,
        timestamp: event.occurredAt,
      });

      this.logger.info("ZakatPaymentProcessed handled successfully");
    } catch (error) {
      this.logger.error("Failed to handle ZakatPaymentProcessed", error);
      throw error;
    }
  }
}

// ========================================
// EXPORTS
// ========================================

export {
  DomainEvent,
  ZakatCalculated,
  ZakatCalculatedData,
  ZakatPaymentProcessed,
  ZakatPaymentProcessedData,
  ZakatDistributionCompleted,
  ZakatDistributionCompletedData,
  ZakatCalculatedHandler,
  ZakatPaymentProcessedHandler,
  EventHandler,
};
```

## TypeScript vs Java/Go Comparison

### Event Creation

**TypeScript**:

```typescript
const event = ZakatCalculated.create({
  zakatId: "zakat-123" as ZakatID,
  accountId: "account-456" as AccountID,
  zakatAmount: Money.create(250, "USD").unwrap(),
  nisabThreshold: Money.create(1000, "USD").unwrap(),
  zakatableAssets: Money.create(10000, "USD").unwrap(),
  calculationMethod: "Standard",
  calculatedBy: "user-789" as UserID,
  occurredAt: new Date(),
});
```

**Java**:

```java
ZakatCalculated event = ZakatCalculated.builder()
    .zakatId(new ZakatId("zakat-123"))
    .accountId(new AccountId("account-456"))
    .zakatAmount(Money.of(250, Currency.USD))
    .nisabThreshold(Money.of(1000, Currency.USD))
    .zakatableAssets(Money.of(10000, Currency.USD))
    .calculationMethod("Standard")
    .calculatedBy(new UserId("user-789"))
    .occurredAt(Instant.now())
    .build();
```

**Go**:

```go
event := ZakatCalculated{
  EventID:          uuid.New().String(),
  ZakatID:          "zakat-123",
  AccountID:        "account-456",
  ZakatAmount:      Money{Amount: 250, Currency: "USD"},
  NisabThreshold:   Money{Amount: 1000, Currency: "USD"},
  ZakatableAssets:  Money{Amount: 10000, Currency: "USD"},
  CalculationMethod: "Standard",
  CalculatedBy:     "user-789",
  OccurredAt:       time.Now(),
  EventType:        "ZakatCalculated",
  Version:          1,
}
```

### Event Serialization

**TypeScript**:

```typescript
const json = event.toJSON();
const restored = ZakatCalculated.fromJSON(json);
```

**Java**:

```java
String json = objectMapper.writeValueAsString(event);
ZakatCalculated restored = objectMapper.readValue(json, ZakatCalculated.class);
```

**Go**:

```go
json, _ := json.Marshal(event)
var restored ZakatCalculated
json.Unmarshal(json, &restored)
```

## Testing Domain Events

### Unit Testing Events

```typescript
import { describe, it, expect } from "vitest";

describe("ZakatCalculated", () => {
  it("should create event with all required fields", () => {
    // Arrange
    const data: ZakatCalculatedData = {
      zakatId: "zakat-123" as ZakatID,
      accountId: "account-456" as AccountID,
      zakatAmount: Money.create(250, "USD").unwrap(),
      nisabThreshold: Money.create(1000, "USD").unwrap(),
      zakatableAssets: Money.create(10000, "USD").unwrap(),
      calculationMethod: "Standard",
      calculatedBy: "user-789" as UserID,
      occurredAt: new Date(),
    };

    // Act
    const event = ZakatCalculated.create(data);

    // Assert
    expect(event.eventId).toBeDefined();
    expect(event.eventType).toBe("ZakatCalculated");
    expect(event.version).toBe(1);
    expect(event.zakatId).toBe(data.zakatId);
    expect(event.zakatAmount).toEqual(data.zakatAmount);
  });

  it("should be immutable", () => {
    // Arrange
    const event = ZakatCalculated.create({
      zakatId: "zakat-123" as ZakatID,
      accountId: "account-456" as AccountID,
      zakatAmount: Money.create(250, "USD").unwrap(),
      nisabThreshold: Money.create(1000, "USD").unwrap(),
      zakatableAssets: Money.create(10000, "USD").unwrap(),
      calculationMethod: "Standard",
      calculatedBy: "user-789" as UserID,
      occurredAt: new Date(),
    });

    // Act & Assert
    expect(() => {
      // @ts-expect-error - Testing immutability
      event.zakatAmount = Money.create(500, "USD").unwrap();
    }).toThrow();
  });

  it("should serialize and deserialize correctly", () => {
    // Arrange
    const original = ZakatCalculated.create({
      zakatId: "zakat-123" as ZakatID,
      accountId: "account-456" as AccountID,
      zakatAmount: Money.create(250, "USD").unwrap(),
      nisabThreshold: Money.create(1000, "USD").unwrap(),
      zakatableAssets: Money.create(10000, "USD").unwrap(),
      calculationMethod: "Standard",
      calculatedBy: "user-789" as UserID,
      occurredAt: new Date(),
    });

    // Act
    const json = original.toJSON();
    const restored = ZakatCalculated.fromJSON(json);

    // Assert
    expect(restored.zakatId).toBe(original.zakatId);
    expect(restored.zakatAmount.getAmount()).toBe(original.zakatAmount.getAmount());
    expect(restored.eventType).toBe(original.eventType);
  });
});
```

### Testing Event Handlers

```typescript
import { describe, it, expect, vi } from "vitest";

describe("ZakatCalculatedHandler", () => {
  it("should send notification when handling event", async () => {
    // Arrange
    const mockNotificationService = {
      notifyZakatCalculated: vi.fn().mockResolvedValue(undefined),
    };
    const mockLogger = {
      info: vi.fn(),
      error: vi.fn(),
    };

    const handler = new ZakatCalculatedHandler(
      mockNotificationService as unknown as NotificationService,
      mockLogger as unknown as Logger,
    );

    const event = ZakatCalculated.create({
      zakatId: "zakat-123" as ZakatID,
      accountId: "account-456" as AccountID,
      zakatAmount: Money.create(250, "USD").unwrap(),
      nisabThreshold: Money.create(1000, "USD").unwrap(),
      zakatableAssets: Money.create(10000, "USD").unwrap(),
      calculationMethod: "Standard",
      calculatedBy: "user-789" as UserID,
      occurredAt: new Date(),
    });

    // Act
    await handler.handle(event);

    // Assert
    expect(mockNotificationService.notifyZakatCalculated).toHaveBeenCalledWith({
      userId: event.calculatedBy,
      zakatAmount: event.zakatAmount,
      nisabThreshold: event.nisabThreshold,
    });

    expect(mockLogger.info).toHaveBeenCalledWith("ZakatCalculated notification sent successfully");
  });
});
```

## Related Documentation

- **[Aggregate Template](./aggregate-template.md)** - Creating aggregates that emit domain events
- **[Entity Template](./entity-template.md)** - Entities within aggregates
- **[Value Object Template](./value-object-template.md)** - Immutable value objects in events
- **[Service Layer Template](./service-layer-template.md)** - Publishing events from services
- **[TypeScript Best Practices](../ex-so-stla-ts__best-practices.md)** - TypeScript coding standards
- **[TypeScript Concurrency and Parallelism](../ex-so-stla-ts__concurrency-and-parallelism.md)** - Async/await patterns for event handlers

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
