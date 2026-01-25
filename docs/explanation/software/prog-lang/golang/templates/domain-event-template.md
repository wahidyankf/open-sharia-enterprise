---
title: Go Domain Event Template
description: Template for creating Domain-Driven Design domain events in Go with immutable design, event handlers, publishing patterns, and event sourcing
category: template
tags:
  - golang
  - ddd
  - domain-events
  - event-driven
  - event-sourcing
  - immutability
  - cqrs
  - messaging
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
related:
  - aggregate-template.md
  - entity-template.md
  - ex-so-prla-go__domain-driven-design.md
  - ex-so-prla-go__concurrency.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
---

# Go Domain Event Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) domain events in Go. Domain events represent **facts that have already happened** in the business domain and are fundamental to event-driven architectures, CQRS, and event sourcing patterns.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Event Interface](#event-interface)
4. [Immutable Design](#immutable-design)
5. [Naming Convention](#naming-convention)
6. [Event Handlers](#event-handlers)
7. [Event Publishing](#event-publishing)
8. [Event Sourcing](#event-sourcing)
9. [Complete Example: Zakat Processing](#complete-example-zakat-processing)
10. [Before/After Comparison](#beforeafter-comparison)
11. [Usage Guidelines](#usage-guidelines)
12. [Testing Domain Events](#testing-domain-events)
13. [Related Documentation](#related-documentation)

## Overview

Domain events in Go represent **immutable facts** about significant business occurrences. Events communicate what has happened in the domain, enabling loose coupling between aggregates, supporting audit trails, and forming the foundation for event-driven architectures.

**Key Characteristics**:

- **Immutable**: Once created, events never change (all fields set at creation)
- **Past-tense naming**: Events represent facts that happened (ZakatPaymentProcessed, not ProcessZakatPayment)
- **Rich with domain meaning**: Events carry business-relevant information
- **Temporal**: Events include timestamp of occurrence
- **Causality tracking**: Events record who/what triggered them
- **No behavior**: Events are pure data structures with no business logic

**Go vs Java Differences**:

```go
// Go: Struct with all exported fields, no setters
type ZakatPaymentProcessed struct {
 EventID     EventID     // Unique event identifier
 AggregateID ZakatID     // Which aggregate this event relates to
 Amount      Money       // Event payload (domain data)
 PayerID     DonorID
 ProcessedBy UserID
 OccurredAt  time.Time   // When event occurred
 Version     int         // Event schema version
}

// No constructor needed - direct struct literal creation
event := ZakatPaymentProcessed{
 EventID:     NewEventID(),
 AggregateID: zakatID,
 Amount:      amount,
 PayerID:     payerID,
 ProcessedBy: userID,
 OccurredAt:  time.Now(),
 Version:     1,
}

// Interface implementation via methods
func (e ZakatPaymentProcessed) GetEventID() EventID         { return e.EventID }
func (e ZakatPaymentProcessed) GetAggregateID() interface{} { return e.AggregateID }
func (e ZakatPaymentProcessed) GetOccurredAt() time.Time    { return e.OccurredAt }
func (e ZakatPaymentProcessed) EventType() string           { return "ZakatPaymentProcessed" }
```

```java
// Java: Class with private final fields, builder pattern
public final class ZakatPaymentProcessed implements DomainEvent {
    private final EventId eventId;
    private final ZakatId aggregateId;
    private final Money amount;
    private final DonorId payerId;
    private final UserId processedBy;
    private final Instant occurredAt;
    private final int version;

    // Private constructor
    private ZakatPaymentProcessed(Builder builder) {
        this.eventId = requireNonNull(builder.eventId);
        this.aggregateId = requireNonNull(builder.aggregateId);
        // ... validation and assignment
    }

    // Builder pattern for construction
    public static Builder builder() {
        return new Builder();
    }

    // Only getters, no setters
    public EventId eventId() { return eventId; }
    public ZakatId aggregateId() { return aggregateId; }
    // ...
}
```

**Critical Differences**:

- Go uses **struct literals** for immutable construction (no builder needed)
- Go events have **exported fields** (no getters needed)
- Go has **value receiver methods** for interface implementation
- Go uses **time.Time** instead of Java's Instant
- Go uses **interface{}** for polymorphic aggregate IDs
- Go struct tags enable JSON/DB serialization without boilerplate

## Template Structure

```go
package event

import (
 "time"

 "github.com/google/uuid"
)

// ========================================
// Event Identity Types
// ========================================

// EventID uniquely identifies a domain event.
type EventID struct {
 value string
}

// NewEventID generates a new unique event ID.
func NewEventID() EventID {
 return EventID{value: uuid.New().String()}
}

// String returns the string representation of the event ID.
func (id EventID) String() string {
 return id.value
}

// ========================================
// Domain Event Interface
// ========================================

// DomainEvent is the base interface for all domain events.
//
// All domain events must:
//   - Have a unique EventID
//   - Reference an AggregateID (which aggregate this event relates to)
//   - Record when the event occurred (OccurredAt)
//   - Specify the event type (EventType)
//
// Domain events are immutable - once created, they cannot be modified.
//
// Example:
//
// event := ZakatPaymentProcessed{
//     EventID:     NewEventID(),
//     AggregateID: zakatID,
//     Amount:      money.NewUSD(100, 0),
//     OccurredAt:  time.Now(),
//     Version:     1,
// }
type DomainEvent interface {
 // GetEventID returns the unique identifier for this event.
 GetEventID() EventID

 // GetAggregateID returns the ID of the aggregate that generated this event.
 // Returns interface{} to support different aggregate ID types.
 GetAggregateID() interface{}

 // GetOccurredAt returns when this event occurred.
 GetOccurredAt() time.Time

 // EventType returns the type name of this event (e.g., "ZakatPaymentProcessed").
 // Used for event routing, serialization, and deserialization.
 EventType() string
}

// ========================================
// Event Template Structure
// ========================================

// EventName represents [brief description of what happened].
//
// This event is published when [business condition occurs].
//
// Aggregate: [AggregateType]
//
// Triggered By:
//   - [Business operation that triggers this event]
//   - [Another triggering condition]
//
// Event Payload:
//   - [Key business data field 1]
//   - [Key business data field 2]
//
// Handlers:
//   - [Handler 1 description]
//   - [Handler 2 description]
//
// Downstream Effects:
//   - [Effect on other aggregates]
//   - [External system notifications]
//
// Example:
//
// event := EventName{
//     EventID:        NewEventID(),
//     AggregateID:    aggregateID,
//     BusinessField1: value1,
//     BusinessField2: value2,
//     PerformedBy:    userID,
//     OccurredAt:     time.Now(),
//     Version:        1,
// }
type EventName struct {
 // ========================================
 // Standard Event Fields (Required)
 // ========================================

 EventID     EventID      // Unique identifier for this event
 AggregateID AggregateID  // Which aggregate generated this event
 OccurredAt  time.Time    // When this event occurred
 Version     int          // Event schema version (for evolution)

 // ========================================
 // Event-Specific Payload
 // ========================================

 // BusinessField1 represents [business meaning].
 BusinessField1 string

 // BusinessField2 represents [business meaning].
 BusinessField2 Money

 // RelatedEntityID references [related entity].
 RelatedEntityID EntityID

 // ========================================
 // Causality Fields (Who/What triggered this)
 // ========================================

 // PerformedBy is the user who triggered this event.
 PerformedBy UserID

 // TriggeredBy is the original event that caused this (optional).
 // Nil if this is a root event.
 TriggeredBy *EventID

 // ========================================
 // Context/Metadata (Optional)
 // ========================================

 // Reason provides human-readable explanation for this event.
 Reason string

 // Metadata stores additional context (e.g., IP address, user agent).
 Metadata map[string]string
}

// ========================================
// Interface Implementation
// ========================================

// GetEventID implements DomainEvent.
func (e EventName) GetEventID() EventID {
 return e.EventID
}

// GetAggregateID implements DomainEvent.
func (e EventName) GetAggregateID() interface{} {
 return e.AggregateID
}

// GetOccurredAt implements DomainEvent.
func (e EventName) GetOccurredAt() time.Time {
 return e.OccurredAt
}

// EventType implements DomainEvent.
func (e EventName) EventType() string {
 return "EventName"
}

// ========================================
// Helper Methods (Optional)
// ========================================

// IsTriggeredEvent returns true if this event was triggered by another event.
func (e EventName) IsTriggeredEvent() bool {
 return e.TriggeredBy != nil
}

// WithMetadata returns a copy of the event with additional metadata.
// This does NOT mutate the original event - it creates a new one.
func (e EventName) WithMetadata(key, value string) EventName {
 // Copy event
 updated := e

 // Initialize metadata if needed
 if updated.Metadata == nil {
  updated.Metadata = make(map[string]string)
 }

 // Add new metadata
 updated.Metadata[key] = value

 return updated
}
```

## Event Interface

The `DomainEvent` interface provides a common contract for all domain events:

```go
// DomainEvent is the base interface for all domain events.
type DomainEvent interface {
 GetEventID() EventID        // Unique event identifier
 GetAggregateID() interface{} // Which aggregate produced this event
 GetOccurredAt() time.Time   // When the event occurred
 EventType() string          // Event type name (for routing/serialization)
}
```

**Key Design Decisions**:

1. **EventID uniqueness**: Every event instance has a unique identifier for tracing and deduplication
2. **AggregateID as interface{}**: Supports different aggregate ID types (ZakatID, DonationID, etc.)
3. **OccurredAt timestamp**: Captures temporal ordering and causality
4. **EventType string**: Enables type-based routing and deserialization

**Interface Implementation**:

```go
type ZakatPaymentProcessed struct {
 EventID     EventID
 AggregateID ZakatID
 Amount      Money
 OccurredAt  time.Time
 Version     int
}

// Value receiver methods implement DomainEvent
func (e ZakatPaymentProcessed) GetEventID() EventID         { return e.EventID }
func (e ZakatPaymentProcessed) GetAggregateID() interface{} { return e.AggregateID }
func (e ZakatPaymentProcessed) GetOccurredAt() time.Time    { return e.OccurredAt }
func (e ZakatPaymentProcessed) EventType() string           { return "ZakatPaymentProcessed" }
```

**Why Value Receivers?**

Events are immutable data structures. Value receivers:

- Prevent accidental mutation
- Work with both pointer and value event instances
- Clearly communicate immutability intent

## Immutable Design

**CRITICAL**: Domain events MUST be immutable once created. They represent historical facts that cannot change.

### ✅ Correct: Immutable Event Creation

```go
// All fields set at creation via struct literal
event := ZakatPaymentProcessed{
 EventID:     NewEventID(),
 AggregateID: zakatID,
 Amount:      money.NewUSD(100, 0),
 PayerID:     donorID,
 ProcessedBy: userID,
 OccurredAt:  time.Now(),
 Version:     1,
}

// Event can be passed around safely
publishEvent(event)         // Copy semantics
storeEvent(event)           // Original unchanged
auditLog.Record(event)      // Safe to share
```

### ❌ Wrong: Mutable Event

```go
// NEVER DO THIS - no setters allowed!
type ZakatPaymentProcessed struct {
 EventID     EventID
 AggregateID ZakatID
 Amount      Money
 // ...
}

// WRONG - mutation method violates immutability
func (e *ZakatPaymentProcessed) SetAmount(amount Money) {
 e.Amount = amount  // Historical fact cannot change!
}

// WRONG - pointer receiver on interface method
func (e *ZakatPaymentProcessed) GetEventID() EventID {
 return e.EventID
}
```

### Immutability Best Practices

```go
// ✅ Use value types for event fields when possible
type DonationReceived struct {
 EventID     EventID
 Amount      Money       // Money is a value type (immutable)
 DonorID     DonorID     // DonorID is a value type
 OccurredAt  time.Time   // time.Time is value type
}

// ✅ Use slices/maps carefully - document that they should not be modified
type CampaignGoalReached struct {
 EventID        EventID
 TotalDonations Money
 Contributors   []DonorID  // Document: Do not modify after event creation
}

// ✅ Create new events instead of modifying existing ones
func enrichEvent(original DonationReceived, metadata map[string]string) DonationReceived {
 // Create new event with additional data (not mutation!)
 return DonationReceived{
  EventID:    original.EventID,
  Amount:     original.Amount,
  DonorID:    original.DonorID,
  OccurredAt: original.OccurredAt,
  Metadata:   metadata,  // New field
 }
}
```

### Why Immutability Matters

1. **Historical accuracy**: Events represent facts - facts don't change
2. **Concurrency safety**: Immutable events can be shared across goroutines without locks
3. **Event sourcing**: Replaying events must produce consistent results
4. **Debugging**: Event stream is reliable audit trail
5. **Cache safety**: Events can be safely cached without invalidation concerns

## Naming Convention

**CRITICAL**: Domain events MUST use **past-tense naming** to represent facts that have already occurred.

### ✅ Correct: Past-Tense Event Names

```go
// ✅ Zakat domain events
type ZakatPaymentProcessed struct { /* ... */ }    // Past tense
type ZakatObligationCalculated struct { /* ... */ } // Past tense
type ZakatDistributionCompleted struct { /* ... */ } // Past tense

// ✅ Donation domain events
type DonationReceived struct { /* ... */ }         // Past tense
type DonationRefunded struct { /* ... */ }         // Past tense
type DonationAllocated struct { /* ... */ }        // Past tense

// ✅ Loan domain events
type LoanApproved struct { /* ... */ }             // Past tense
type LoanDisbursed struct { /* ... */ }            // Past tense
type LoanRepaymentReceived struct { /* ... */ }    // Past tense
```

### ❌ Wrong: Present/Imperative Event Names

```go
// ❌ WRONG - Sounds like command, not event
type ProcessZakatPayment struct { /* ... */ }      // Command (imperative)
type ReceiveDonation struct { /* ... */ }          // Command (imperative)
type ApproveLoan struct { /* ... */ }              // Command (imperative)

// ❌ WRONG - Present tense (ongoing action)
type ProcessingZakatPayment struct { /* ... */ }   // Present progressive
type ReceivingDonation struct { /* ... */ }        // Present progressive
```

### Naming Convention Rules

1. **Always past tense**: Events represent completed actions
2. **Domain language**: Use ubiquitous language from business domain
3. **Specific not generic**: "ZakatPaymentProcessed" not "EntityUpdated"
4. **Action-oriented**: Describe what happened, not current state
5. **Positive framing**: What succeeded, not what failed (use specific failure events)

**Examples by Domain**:

```go
// Zakat Processing
ZakatObligationCalculated      // Calculation completed
ZakatPaymentProcessed          // Payment processed
ZakatReceiptGenerated          // Receipt created
ZakatDistributionInitiated     // Distribution started

// Donation Management
DonationReceived               // Donation accepted
DonationRecurringScheduled     // Recurring setup completed
DonationCampaignLaunched       // Campaign started
CampaignGoalReached            // Target achieved

// Islamic Finance
LoanApplicationSubmitted       // Application received
LoanApproved                   // Approval granted
LoanDisbursed                  // Funds transferred
RepaymentScheduleGenerated     // Schedule created
LoanFullyRepaid                // Loan closed
```

## Event Handlers

Event handlers respond to domain events asynchronously, enabling loose coupling between aggregates and bounded contexts.

### Handler Interface

```go
package event

import "context"

// EventHandler processes domain events asynchronously.
//
// Handlers should be idempotent - processing the same event multiple times
// should have the same effect as processing it once.
//
// Example:
//
// type SendReceiptHandler struct {
//     emailService EmailService
// }
//
// func (h *SendReceiptHandler) Handle(ctx context.Context, event DomainEvent) error {
//     zakatProcessed, ok := event.(ZakatPaymentProcessed)
//     if !ok {
//         return nil // Not interested in this event type
//     }
//
//     return h.emailService.SendReceipt(ctx, zakatProcessed.PayerID, zakatProcessed.Amount)
// }
//
// func (h *SendReceiptHandler) EventTypes() []string {
//     return []string{"ZakatPaymentProcessed"}
// }
type EventHandler interface {
 // Handle processes a domain event.
 // Returns error if processing fails (enables retry).
 Handle(ctx context.Context, event DomainEvent) error

 // EventTypes returns the event types this handler is interested in.
 // Used for event routing and subscription management.
 EventTypes() []string
}

// EventHandlerFunc is a function adapter for EventHandler interface.
type EventHandlerFunc func(ctx context.Context, event DomainEvent) error

// Handle implements EventHandler.
func (f EventHandlerFunc) Handle(ctx context.Context, event DomainEvent) error {
 return f(ctx, event)
}

// EventTypes implements EventHandler (matches all types by default).
func (f EventHandlerFunc) EventTypes() []string {
 return []string{"*"} // Wildcard - handle all events
}
```

### Handler Registration

```go
package event

import (
 "context"
 "fmt"
 "sync"
)

// EventBus manages event handler registration and event dispatching.
type EventBus struct {
 handlers map[string][]EventHandler // Event type -> handlers
 mu       sync.RWMutex
}

// NewEventBus creates a new event bus.
func NewEventBus() *EventBus {
 return &EventBus{
  handlers: make(map[string][]EventHandler),
 }
}

// Register adds an event handler for specific event types.
func (bus *EventBus) Register(handler EventHandler) {
 bus.mu.Lock()
 defer bus.mu.Unlock()

 for _, eventType := range handler.EventTypes() {
  bus.handlers[eventType] = append(bus.handlers[eventType], handler)
 }
}

// Dispatch sends an event to all registered handlers.
func (bus *EventBus) Dispatch(ctx context.Context, event DomainEvent) error {
 bus.mu.RLock()
 eventType := event.EventType()
 handlers := append([]EventHandler{}, bus.handlers[eventType]...)
 wildcardHandlers := append([]EventHandler{}, bus.handlers["*"]...)
 bus.mu.RUnlock()

 // Combine specific and wildcard handlers
 allHandlers := append(handlers, wildcardHandlers...)

 // Execute handlers (sequential for simplicity, can be parallel)
 var errs []error
 for _, handler := range allHandlers {
  if err := handler.Handle(ctx, event); err != nil {
   errs = append(errs, fmt.Errorf("handler failed for %s: %w", eventType, err))
  }
 }

 if len(errs) > 0 {
  return fmt.Errorf("event dispatch errors: %v", errs)
 }

 return nil
}
```

### Example Handlers

```go
package handler

import (
 "context"
 "fmt"

 "yourapp/domain/event"
 "yourapp/domain/model"
 "yourapp/service"
)

// SendZakatReceiptHandler sends email receipt when zakat payment is processed.
type SendZakatReceiptHandler struct {
 emailService service.EmailService
 receiptRepo  model.ReceiptRepository
}

// NewSendZakatReceiptHandler creates a new receipt sender handler.
func NewSendZakatReceiptHandler(
 emailService service.EmailService,
 receiptRepo model.ReceiptRepository,
) *SendZakatReceiptHandler {
 return &SendZakatReceiptHandler{
  emailService: emailService,
  receiptRepo:  receiptRepo,
 }
}

// Handle processes ZakatPaymentProcessed event.
func (h *SendZakatReceiptHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 // Type assertion to specific event
 zakatProcessed, ok := evt.(event.ZakatPaymentProcessed)
 if !ok {
  return nil // Not interested in this event
 }

 // Idempotency check - don't send receipt twice
 existing, err := h.receiptRepo.FindByEventID(ctx, zakatProcessed.EventID)
 if err != nil {
  return fmt.Errorf("check existing receipt: %w", err)
 }
 if existing != nil {
  return nil // Receipt already sent
 }

 // Generate receipt
 receipt, err := model.GenerateZakatReceipt(zakatProcessed)
 if err != nil {
  return fmt.Errorf("generate receipt: %w", err)
 }

 // Send email
 if err := h.emailService.SendReceipt(ctx, zakatProcessed.PayerID, receipt); err != nil {
  return fmt.Errorf("send email: %w", err)
 }

 // Record receipt sent
 if err := h.receiptRepo.Save(ctx, receipt); err != nil {
  return fmt.Errorf("save receipt: %w", err)
 }

 return nil
}

// EventTypes declares which events this handler processes.
func (h *SendZakatReceiptHandler) EventTypes() []string {
 return []string{"ZakatPaymentProcessed"}
}

// UpdateDonationStatisticsHandler updates campaign statistics when donation received.
type UpdateDonationStatisticsHandler struct {
 statsRepo model.CampaignStatisticsRepository
}

// Handle processes DonationReceived event.
func (h *UpdateDonationStatisticsHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 donationReceived, ok := evt.(event.DonationReceived)
 if !ok {
  return nil
 }

 // Update campaign statistics (idempotent aggregate operation)
 stats, err := h.statsRepo.FindByCampaignID(ctx, donationReceived.CampaignID)
 if err != nil {
  return fmt.Errorf("find statistics: %w", err)
 }

 // Idempotency - check if event already processed
 if stats.HasProcessedEvent(donationReceived.EventID) {
  return nil
 }

 // Update statistics
 if err := stats.RecordDonation(donationReceived.Amount, donationReceived.EventID); err != nil {
  return fmt.Errorf("record donation: %w", err)
 }

 // Persist updated statistics
 if err := h.statsRepo.Save(ctx, stats); err != nil {
  return fmt.Errorf("save statistics: %w", err)
 }

 return nil
}

// EventTypes declares event interest.
func (h *UpdateDonationStatisticsHandler) EventTypes() []string {
 return []string{"DonationReceived"}
}
```

### Async Processing Pattern

```go
package event

import (
 "context"
 "log"
 "sync"
)

// AsyncEventBus processes events asynchronously via goroutines.
type AsyncEventBus struct {
 *EventBus
 wg sync.WaitGroup
}

// NewAsyncEventBus creates an async event bus.
func NewAsyncEventBus() *AsyncEventBus {
 return &AsyncEventBus{
  EventBus: NewEventBus(),
 }
}

// DispatchAsync sends event to handlers asynchronously.
// Returns immediately without waiting for handlers to complete.
func (bus *AsyncEventBus) DispatchAsync(ctx context.Context, event DomainEvent) {
 bus.mu.RLock()
 eventType := event.EventType()
 handlers := append([]EventHandler{}, bus.handlers[eventType]...)
 wildcardHandlers := append([]EventHandler{}, bus.handlers["*"]...)
 bus.mu.RUnlock()

 allHandlers := append(handlers, wildcardHandlers...)

 // Process each handler in separate goroutine
 for _, handler := range allHandlers {
  bus.wg.Add(1)
  go func(h EventHandler) {
   defer bus.wg.Done()

   if err := h.Handle(ctx, event); err != nil {
    // Log error (consider dead-letter queue for retries)
    log.Printf("ERROR: Handler failed for event %s: %v", eventType, err)
   }
  }(handler)
 }
}

// Wait waits for all async handlers to complete.
// Call this before application shutdown.
func (bus *AsyncEventBus) Wait() {
 bus.wg.Wait()
}
```

## Event Publishing

Event publishing sends domain events to external consumers (other aggregates, bounded contexts, or external systems).

### Publisher Interface

```go
package event

import "context"

// EventPublisher publishes domain events to external consumers.
type EventPublisher interface {
 // Publish sends a single event to consumers.
 Publish(ctx context.Context, event DomainEvent) error

 // PublishBatch sends multiple events atomically.
 // Either all events are published or none are.
 PublishBatch(ctx context.Context, events []DomainEvent) error
}
```

### In-Memory Publisher (Development/Testing)

```go
package event

import (
 "context"
 "sync"
)

// InMemoryPublisher publishes events to registered handlers (same process).
type InMemoryPublisher struct {
 bus *EventBus
 mu  sync.Mutex
}

// NewInMemoryPublisher creates an in-memory event publisher.
func NewInMemoryPublisher(bus *EventBus) *InMemoryPublisher {
 return &InMemoryPublisher{
  bus: bus,
 }
}

// Publish dispatches event to registered handlers.
func (p *InMemoryPublisher) Publish(ctx context.Context, event DomainEvent) error {
 p.mu.Lock()
 defer p.mu.Unlock()

 return p.bus.Dispatch(ctx, event)
}

// PublishBatch dispatches multiple events.
func (p *InMemoryPublisher) PublishBatch(ctx context.Context, events []DomainEvent) error {
 p.mu.Lock()
 defer p.mu.Unlock()

 for _, event := range events {
  if err := p.bus.Dispatch(ctx, event); err != nil {
   return err
  }
 }

 return nil
}
```

### Message Queue Publisher (Production)

```go
package event

import (
 "context"
 "encoding/json"
 "fmt"

 "github.com/nats-io/nats.go"
)

// NATSPublisher publishes events to NATS messaging system.
type NATSPublisher struct {
 conn *nats.Conn
}

// NewNATSPublisher creates a NATS event publisher.
func NewNATSPublisher(natsURL string) (*NATSPublisher, error) {
 conn, err := nats.Connect(natsURL)
 if err != nil {
  return nil, fmt.Errorf("connect to NATS: %w", err)
 }

 return &NATSPublisher{
  conn: conn,
 }, nil
}

// Publish sends event to NATS subject.
func (p *NATSPublisher) Publish(ctx context.Context, event DomainEvent) error {
 // Serialize event to JSON
 data, err := json.Marshal(event)
 if err != nil {
  return fmt.Errorf("marshal event: %w", err)
 }

 // Publish to subject: events.<EventType>
 subject := fmt.Sprintf("events.%s", event.EventType())
 if err := p.conn.Publish(subject, data); err != nil {
  return fmt.Errorf("publish to NATS: %w", err)
 }

 return nil
}

// PublishBatch sends multiple events.
func (p *NATSPublisher) PublishBatch(ctx context.Context, events []DomainEvent) error {
 for _, event := range events {
  if err := p.Publish(ctx, event); err != nil {
   return err
  }
 }
 return nil
}

// Close closes the NATS connection.
func (p *NATSPublisher) Close() error {
 p.conn.Close()
 return nil
}
```

### Kafka Publisher (High Throughput)

```go
package event

import (
 "context"
 "encoding/json"
 "fmt"

 "github.com/segmentio/kafka-go"
)

// KafkaPublisher publishes events to Apache Kafka.
type KafkaPublisher struct {
 writer *kafka.Writer
}

// NewKafkaPublisher creates a Kafka event publisher.
func NewKafkaPublisher(brokers []string, topic string) *KafkaPublisher {
 return &KafkaPublisher{
  writer: &kafka.Writer{
   Addr:     kafka.TCP(brokers...),
   Topic:    topic,
   Balancer: &kafka.LeastBytes{},
  },
 }
}

// Publish sends event to Kafka topic.
func (p *KafkaPublisher) Publish(ctx context.Context, event DomainEvent) error {
 // Serialize event
 value, err := json.Marshal(event)
 if err != nil {
  return fmt.Errorf("marshal event: %w", err)
 }

 // Use aggregate ID as partition key for ordering
 key, err := json.Marshal(event.GetAggregateID())
 if err != nil {
  return fmt.Errorf("marshal aggregate ID: %w", err)
 }

 // Write message
 err = p.writer.WriteMessages(ctx, kafka.Message{
  Key:   key,
  Value: value,
  Headers: []kafka.Header{
   {Key: "event-type", Value: []byte(event.EventType())},
   {Key: "event-id", Value: []byte(event.GetEventID().String())},
  },
 })

 if err != nil {
  return fmt.Errorf("write to Kafka: %w", err)
 }

 return nil
}

// PublishBatch sends multiple events to Kafka.
func (p *KafkaPublisher) PublishBatch(ctx context.Context, events []DomainEvent) error {
 messages := make([]kafka.Message, len(events))

 for i, event := range events {
  value, err := json.Marshal(event)
  if err != nil {
   return fmt.Errorf("marshal event %d: %w", i, err)
  }

  key, err := json.Marshal(event.GetAggregateID())
  if err != nil {
   return fmt.Errorf("marshal aggregate ID %d: %w", i, err)
  }

  messages[i] = kafka.Message{
   Key:   key,
   Value: value,
   Headers: []kafka.Header{
    {Key: "event-type", Value: []byte(event.EventType())},
    {Key: "event-id", Value: []byte(event.GetEventID().String())},
   },
  }
 }

 if err := p.writer.WriteMessages(ctx, messages...); err != nil {
  return fmt.Errorf("write batch to Kafka: %w", err)
 }

 return nil
}

// Close closes the Kafka writer.
func (p *KafkaPublisher) Close() error {
 return p.writer.Close()
}
```

## Event Sourcing

Event sourcing stores domain events as the primary source of truth, reconstructing aggregate state by replaying events.

### Event Store Interface

```go
package event

import "context"

// EventStore persists and retrieves domain events.
type EventStore interface {
 // Append adds new events to the stream for an aggregate.
 Append(ctx context.Context, aggregateID interface{}, events []DomainEvent) error

 // Load retrieves all events for an aggregate.
 Load(ctx context.Context, aggregateID interface{}) ([]DomainEvent, error)

 // LoadFromVersion retrieves events starting from a specific version.
 LoadFromVersion(ctx context.Context, aggregateID interface{}, version int) ([]DomainEvent, error)

 // LoadAll retrieves all events across all aggregates (for projections).
 LoadAll(ctx context.Context) ([]DomainEvent, error)
}
```

### In-Memory Event Store

```go
package event

import (
 "context"
 "fmt"
 "sync"
)

// InMemoryEventStore stores events in memory (testing/development only).
type InMemoryEventStore struct {
 streams map[string][]DomainEvent // aggregateID -> events
 mu      sync.RWMutex
}

// NewInMemoryEventStore creates a new in-memory event store.
func NewInMemoryEventStore() *InMemoryEventStore {
 return &InMemoryEventStore{
  streams: make(map[string][]DomainEvent),
 }
}

// Append adds events to aggregate stream.
func (s *InMemoryEventStore) Append(ctx context.Context, aggregateID interface{}, events []DomainEvent) error {
 s.mu.Lock()
 defer s.mu.Unlock()

 key := fmt.Sprintf("%v", aggregateID)
 s.streams[key] = append(s.streams[key], events...)

 return nil
}

// Load retrieves all events for an aggregate.
func (s *InMemoryEventStore) Load(ctx context.Context, aggregateID interface{}) ([]DomainEvent, error) {
 s.mu.RLock()
 defer s.mu.RUnlock()

 key := fmt.Sprintf("%v", aggregateID)
 events, ok := s.streams[key]
 if !ok {
  return []DomainEvent{}, nil
 }

 // Return copy to prevent external modification
 result := make([]DomainEvent, len(events))
 copy(result, events)

 return result, nil
}

// LoadFromVersion retrieves events starting from version.
func (s *InMemoryEventStore) LoadFromVersion(ctx context.Context, aggregateID interface{}, version int) ([]DomainEvent, error) {
 allEvents, err := s.Load(ctx, aggregateID)
 if err != nil {
  return nil, err
 }

 if version >= len(allEvents) {
  return []DomainEvent{}, nil
 }

 return allEvents[version:], nil
}

// LoadAll retrieves all events across all aggregates.
func (s *InMemoryEventStore) LoadAll(ctx context.Context) ([]DomainEvent, error) {
 s.mu.RLock()
 defer s.mu.RUnlock()

 var all []DomainEvent
 for _, events := range s.streams {
  all = append(all, events...)
 }

 return all, nil
}
```

### Event Sourced Aggregate Pattern

```go
package model

import (
 "context"
 "fmt"

 "yourapp/domain/event"
)

// ZakatAccount is an event-sourced aggregate.
type ZakatAccount struct {
 id       ZakatAccountID
 balance  Money
 payments []ZakatPaymentRecord
 version  int
}

// NewZakatAccount creates a new zakat account (initial state).
func NewZakatAccount(id ZakatAccountID) *ZakatAccount {
 return &ZakatAccount{
  id:       id,
  balance:  Money{},
  payments: []ZakatPaymentRecord{},
  version:  0,
 }
}

// ReconstituteFromEvents rebuilds aggregate state from event stream.
func ReconstituteFromEvents(id ZakatAccountID, events []event.DomainEvent) (*ZakatAccount, error) {
 account := NewZakatAccount(id)

 for _, evt := range events {
  if err := account.Apply(evt); err != nil {
   return nil, fmt.Errorf("apply event: %w", err)
  }
  account.version++
 }

 return account, nil
}

// Apply applies a single event to update aggregate state.
func (z *ZakatAccount) Apply(evt event.DomainEvent) error {
 switch e := evt.(type) {
 case event.ZakatPaymentProcessed:
  return z.applyZakatPaymentProcessed(e)

 case event.ZakatRefundIssued:
  return z.applyZakatRefundIssued(e)

 default:
  return fmt.Errorf("unknown event type: %s", evt.EventType())
 }
}

// applyZakatPaymentProcessed updates state from payment event.
func (z *ZakatAccount) applyZakatPaymentProcessed(e event.ZakatPaymentProcessed) error {
 z.balance = z.balance.Add(e.Amount)
 z.payments = append(z.payments, ZakatPaymentRecord{
  PaymentID:   e.PaymentID,
  Amount:      e.Amount,
  ProcessedAt: e.OccurredAt,
 })
 return nil
}

// applyZakatRefundIssued updates state from refund event.
func (z *ZakatAccount) applyZakatRefundIssued(e event.ZakatRefundIssued) error {
 z.balance = z.balance.Subtract(e.Amount)
 return nil
}
```

### Event Sourced Repository

```go
package repository

import (
 "context"
 "fmt"

 "yourapp/domain/event"
 "yourapp/domain/model"
)

// EventSourcedZakatAccountRepository loads/saves zakat accounts via events.
type EventSourcedZakatAccountRepository struct {
 eventStore event.EventStore
 publisher  event.EventPublisher
}

// NewEventSourcedZakatAccountRepository creates event-sourced repository.
func NewEventSourcedZakatAccountRepository(
 eventStore event.EventStore,
 publisher event.EventPublisher,
) *EventSourcedZakatAccountRepository {
 return &EventSourcedZakatAccountRepository{
  eventStore: eventStore,
  publisher:  publisher,
 }
}

// Load reconstitutes aggregate from event stream.
func (r *EventSourcedZakatAccountRepository) Load(ctx context.Context, id model.ZakatAccountID) (*model.ZakatAccount, error) {
 // Load events from store
 events, err := r.eventStore.Load(ctx, id)
 if err != nil {
  return nil, fmt.Errorf("load events: %w", err)
 }

 if len(events) == 0 {
  return nil, fmt.Errorf("zakat account not found: %v", id)
 }

 // Reconstitute from events
 account, err := model.ReconstituteFromEvents(id, events)
 if err != nil {
  return nil, fmt.Errorf("reconstitute: %w", err)
 }

 return account, nil
}

// Save appends new events to event stream.
func (r *EventSourcedZakatAccountRepository) Save(ctx context.Context, account *model.ZakatAccount) error {
 // Get uncommitted events from aggregate
 events := account.UncommittedEvents()

 if len(events) == 0 {
  return nil // Nothing to save
 }

 // Append to event store
 if err := r.eventStore.Append(ctx, account.ID(), events); err != nil {
  return fmt.Errorf("append events: %w", err)
 }

 // Publish events
 if err := r.publisher.PublishBatch(ctx, events); err != nil {
  return fmt.Errorf("publish events: %w", err)
 }

 // Mark events as committed
 account.MarkEventsCommitted()

 return nil
}
```

### Projection Pattern (Read Models)

```go
package projection

import (
 "context"
 "fmt"

 "yourapp/domain/event"
)

// ZakatAccountBalanceProjection maintains current balance read model.
type ZakatAccountBalanceProjection struct {
 balances map[string]Money // accountID -> current balance
}

// NewZakatAccountBalanceProjection creates a new projection.
func NewZakatAccountBalanceProjection() *ZakatAccountBalanceProjection {
 return &ZakatAccountBalanceProjection{
  balances: make(map[string]Money),
 }
}

// Handle processes events to update projection.
func (p *ZakatAccountBalanceProjection) Handle(ctx context.Context, evt event.DomainEvent) error {
 switch e := evt.(type) {
 case event.ZakatPaymentProcessed:
  return p.handlePaymentProcessed(e)

 case event.ZakatRefundIssued:
  return p.handleRefundIssued(e)

 default:
  return nil // Not interested in other events
 }
}

// handlePaymentProcessed updates balance from payment.
func (p *ZakatAccountBalanceProjection) handlePaymentProcessed(e event.ZakatPaymentProcessed) error {
 accountID := fmt.Sprintf("%v", e.AccountID)
 current := p.balances[accountID]
 p.balances[accountID] = current.Add(e.Amount)
 return nil
}

// handleRefundIssued updates balance from refund.
func (p *ZakatAccountBalanceProjection) handleRefundIssued(e event.ZakatRefundIssued) error {
 accountID := fmt.Sprintf("%v", e.AccountID)
 current := p.balances[accountID]
 p.balances[accountID] = current.Subtract(e.Amount)
 return nil
}

// GetBalance retrieves current balance from projection.
func (p *ZakatAccountBalanceProjection) GetBalance(accountID string) Money {
 return p.balances[accountID]
}

// EventTypes declares event interest.
func (p *ZakatAccountBalanceProjection) EventTypes() []string {
 return []string{"ZakatPaymentProcessed", "ZakatRefundIssued"}
}
```

## Complete Example: Zakat Processing

Complete working example showing domain events in a zakat payment processing context.

### Domain Events

```go
package event

import "time"

// ZakatPaymentProcessed represents a completed zakat payment.
type ZakatPaymentProcessed struct {
 EventID       EventID
 AggregateID   ZakatAccountID
 PaymentID     PaymentID
 Amount        Money
 PayerID       DonorID
 PaymentMethod PaymentMethod
 ProcessedBy   UserID
 OccurredAt    time.Time
 Version       int
}

func (e ZakatPaymentProcessed) GetEventID() EventID         { return e.EventID }
func (e ZakatPaymentProcessed) GetAggregateID() interface{} { return e.AggregateID }
func (e ZakatPaymentProcessed) GetOccurredAt() time.Time    { return e.OccurredAt }
func (e ZakatPaymentProcessed) EventType() string           { return "ZakatPaymentProcessed" }

// ZakatReceiptGenerated represents a generated receipt.
type ZakatReceiptGenerated struct {
 EventID     EventID
 AggregateID ReceiptID
 PaymentID   PaymentID
 ReceiptNo   string
 GeneratedBy UserID
 OccurredAt  time.Time
 Version     int
}

func (e ZakatReceiptGenerated) GetEventID() EventID         { return e.EventID }
func (e ZakatReceiptGenerated) GetAggregateID() interface{} { return e.AggregateID }
func (e ZakatReceiptGenerated) GetOccurredAt() time.Time    { return e.OccurredAt }
func (e ZakatReceiptGenerated) EventType() string           { return "ZakatReceiptGenerated" }

// ZakatDistributionCompleted represents completed distribution.
type ZakatDistributionCompleted struct {
 EventID       EventID
 AggregateID   DistributionID
 TotalAmount   Money
 Recipients    []RecipientID
 DistributedBy UserID
 OccurredAt    time.Time
 Version       int
}

func (e ZakatDistributionCompleted) GetEventID() EventID         { return e.EventID }
func (e ZakatDistributionCompleted) GetAggregateID() interface{} { return e.AggregateID }
func (e ZakatDistributionCompleted) GetOccurredAt() time.Time    { return e.OccurredAt }
func (e ZakatDistributionCompleted) EventType() string           { return "ZakatDistributionCompleted" }
```

### Aggregate with Events

```go
package model

import (
 "fmt"
 "time"

 "yourapp/domain/event"
)

// ZakatAccount represents a zakat account aggregate.
type ZakatAccount struct {
 id             ZakatAccountID
 ownerID        DonorID
 balance        Money
 paymentHistory []PaymentRecord
 uncommitted    []event.DomainEvent // Events not yet persisted
 version        int
}

// ProcessPayment records a zakat payment.
func (z *ZakatAccount) ProcessPayment(
 paymentID PaymentID,
 amount Money,
 method PaymentMethod,
 processedBy UserID,
) error {
 // Validate
 if amount.IsNegative() {
  return fmt.Errorf("payment amount must be positive")
 }

 // Create event
 evt := event.ZakatPaymentProcessed{
  EventID:       event.NewEventID(),
  AggregateID:   z.id,
  PaymentID:     paymentID,
  Amount:        amount,
  PayerID:       z.ownerID,
  PaymentMethod: method,
  ProcessedBy:   processedBy,
  OccurredAt:    time.Now(),
  Version:       1,
 }

 // Apply event to update state
 if err := z.Apply(evt); err != nil {
  return err
 }

 // Record uncommitted event
 z.registerEvent(evt)

 return nil
}

// Apply updates state from event (idempotent).
func (z *ZakatAccount) Apply(evt event.DomainEvent) error {
 switch e := evt.(type) {
 case event.ZakatPaymentProcessed:
  z.balance = z.balance.Add(e.Amount)
  z.paymentHistory = append(z.paymentHistory, PaymentRecord{
   PaymentID:   e.PaymentID,
   Amount:      e.Amount,
   ProcessedAt: e.OccurredAt,
  })
  return nil

 default:
  return fmt.Errorf("unknown event: %s", evt.EventType())
 }
}

// UncommittedEvents returns events not yet persisted.
func (z *ZakatAccount) UncommittedEvents() []event.DomainEvent {
 return z.uncommitted
}

// MarkEventsCommitted clears uncommitted events after persistence.
func (z *ZakatAccount) MarkEventsCommitted() {
 z.uncommitted = []event.DomainEvent{}
}

// registerEvent adds event to uncommitted list.
func (z *ZakatAccount) registerEvent(evt event.DomainEvent) {
 z.uncommitted = append(z.uncommitted, evt)
}
```

### Event Handlers

```go
package handler

import (
 "context"
 "fmt"

 "yourapp/domain/event"
 "yourapp/service"
)

// GenerateReceiptHandler generates receipt when payment processed.
type GenerateReceiptHandler struct {
 receiptService service.ReceiptService
}

func (h *GenerateReceiptHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 payment, ok := evt.(event.ZakatPaymentProcessed)
 if !ok {
  return nil
 }

 // Generate receipt
 receipt, err := h.receiptService.GenerateZakatReceipt(ctx, payment.PaymentID)
 if err != nil {
  return fmt.Errorf("generate receipt: %w", err)
 }

 // Receipt generation publishes ZakatReceiptGenerated event
 _ = receipt

 return nil
}

func (h *GenerateReceiptHandler) EventTypes() []string {
 return []string{"ZakatPaymentProcessed"}
}

// SendEmailNotificationHandler sends email when payment processed.
type SendEmailNotificationHandler struct {
 emailService service.EmailService
}

func (h *SendEmailNotificationHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 payment, ok := evt.(event.ZakatPaymentProcessed)
 if !ok {
  return nil
 }

 return h.emailService.SendPaymentConfirmation(ctx, payment.PayerID, payment.Amount)
}

func (h *SendEmailNotificationHandler) EventTypes() []string {
 return []string{"ZakatPaymentProcessed"}
}
```

### Application Service

```go
package application

import (
 "context"
 "fmt"

 "yourapp/domain/event"
 "yourapp/domain/model"
 "yourapp/domain/repository"
)

// ZakatPaymentService handles zakat payment use cases.
type ZakatPaymentService struct {
 accountRepo repository.ZakatAccountRepository
 publisher   event.EventPublisher
}

// ProcessPayment processes a zakat payment.
func (s *ZakatPaymentService) ProcessPayment(
 ctx context.Context,
 accountID model.ZakatAccountID,
 paymentID model.PaymentID,
 amount model.Money,
 method model.PaymentMethod,
 processedBy model.UserID,
) error {
 // Load aggregate
 account, err := s.accountRepo.Load(ctx, accountID)
 if err != nil {
  return fmt.Errorf("load account: %w", err)
 }

 // Execute business logic (produces event)
 if err := account.ProcessPayment(paymentID, amount, method, processedBy); err != nil {
  return fmt.Errorf("process payment: %w", err)
 }

 // Persist aggregate (saves events)
 if err := s.accountRepo.Save(ctx, account); err != nil {
  return fmt.Errorf("save account: %w", err)
 }

 // Publish events (triggers handlers)
 events := account.UncommittedEvents()
 if err := s.publisher.PublishBatch(ctx, events); err != nil {
  return fmt.Errorf("publish events: %w", err)
 }

 account.MarkEventsCommitted()

 return nil
}
```

## Before/After Comparison

### ❌ Before: Direct Coupling Without Events

```go
package service

import (
 "context"
 "fmt"
)

// PaymentService tightly couples all side effects.
type PaymentService struct {
 accountRepo    AccountRepository
 receiptService ReceiptService
 emailService   EmailService
 smsService     SMSService
 analyticsAPI   AnalyticsAPI
}

// ProcessPayment has too many responsibilities.
func (s *PaymentService) ProcessPayment(ctx context.Context, payment Payment) error {
 // Update account
 account, err := s.accountRepo.FindByID(ctx, payment.AccountID)
 if err != nil {
  return err
 }

 account.Balance += payment.Amount
 account.LastPaymentDate = payment.Date

 if err := s.accountRepo.Save(ctx, account); err != nil {
  return err
 }

 // Generate receipt (tight coupling!)
 receipt, err := s.receiptService.Generate(ctx, payment)
 if err != nil {
  return err // Rollback needed?
 }

 // Send email (blocking!)
 if err := s.emailService.SendConfirmation(ctx, account.Email, receipt); err != nil {
  // What if email fails? Rollback receipt?
  return err
 }

 // Send SMS (another point of failure!)
 if err := s.smsService.SendNotification(ctx, account.Phone); err != nil {
  // Email sent but SMS failed - inconsistent state!
  return err
 }

 // Track analytics (external API call!)
 if err := s.analyticsAPI.TrackPayment(ctx, payment); err != nil {
  // All above succeeded but analytics failed - what now?
  return err
 }

 // PROBLEMS:
 // - Service knows about ALL side effects
 // - All operations are blocking and sequential
 // - Failure in any step breaks entire flow
 // - Hard to add new side effects without modifying service
 // - Difficult to test in isolation
 // - No audit trail of what happened

 return nil
}
```

### ✅ After: Event-Driven Decoupling

```go
package service

import (
 "context"
 "fmt"
 "time"

 "yourapp/domain/event"
 "yourapp/domain/model"
)

// PaymentService focused on domain logic only.
type PaymentService struct {
 accountRepo repository.AccountRepository
 eventBus    *event.EventBus
}

// ProcessPayment executes domain logic and publishes event.
func (s *PaymentService) ProcessPayment(ctx context.Context, payment Payment) error {
 // Load aggregate
 account, err := s.accountRepo.FindByID(ctx, payment.AccountID)
 if err != nil {
  return fmt.Errorf("load account: %w", err)
 }

 // Execute domain logic
 if err := account.RecordPayment(payment); err != nil {
  return fmt.Errorf("record payment: %w", err)
 }

 // Persist aggregate
 if err := s.accountRepo.Save(ctx, account); err != nil {
  return fmt.Errorf("save account: %w", err)
 }

 // Publish event (non-blocking, fire and forget)
 evt := event.ZakatPaymentProcessed{
  EventID:     event.NewEventID(),
  AggregateID: account.ID(),
  PaymentID:   payment.ID,
  Amount:      payment.Amount,
  PayerID:     payment.PayerID,
  OccurredAt:  time.Now(),
  Version:     1,
 }

 s.eventBus.DispatchAsync(ctx, evt)

 // BENEFITS:
 // - Service only knows domain logic
 // - Event published asynchronously
 // - Handlers can fail independently without affecting payment
 // - Easy to add new handlers without changing service
 // - Event stream provides complete audit trail
 // - Each handler tested in isolation

 return nil
}

// Handlers execute asynchronously, decoupled from payment processing

type ReceiptHandler struct{ receiptService ReceiptService }
func (h *ReceiptHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 // Generate receipt independently
}

type EmailHandler struct{ emailService EmailService }
func (h *EmailHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 // Send email independently
}

type SMSHandler struct{ smsService SMSService }
func (h *SMSHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 // Send SMS independently
}

type AnalyticsHandler struct{ analyticsAPI AnalyticsAPI }
func (h *AnalyticsHandler) Handle(ctx context.Context, evt event.DomainEvent) error {
 // Track analytics independently
}
```

### Key Improvements

| Aspect            | Before (Tight Coupling)         | After (Event-Driven)                 |
| ----------------- | ------------------------------- | ------------------------------------ |
| **Coupling**      | Service knows all side effects  | Service only knows domain logic      |
| **Execution**     | Sequential, blocking            | Async, non-blocking                  |
| **Failure**       | Any failure breaks flow         | Failures isolated to handlers        |
| **Extensibility** | Modify service for new features | Add new handlers independently       |
| **Testing**       | Must mock all dependencies      | Test service and handlers separately |
| **Audit**         | No record of what happened      | Complete event stream                |
| **Scalability**   | All work in critical path       | Side effects processed async         |

## Usage Guidelines

### When to Use Domain Events

**✅ Use domain events when:**

- **Decoupling aggregates**: One aggregate needs to know about changes in another
- **Cross-boundary communication**: Bounded contexts need to exchange information
- **Audit requirements**: Need complete history of what happened
- **Async processing**: Side effects can execute asynchronously (emails, notifications)
- **Event sourcing**: Domain state derived from event stream
- **CQRS**: Separate read and write models
- **Integration**: External systems need notifications

**❌ Don't use domain events when:**

- **Synchronous validation**: Must validate before proceeding (use domain services)
- **Simple CRUD**: No business logic, just data persistence
- **Performance critical**: Event overhead not justified
- **Local state changes**: Changes within single aggregate not interesting to others

### Event Design Principles

1. **Immutable by design**: Events never change after creation
2. **Past tense naming**: Events represent facts that happened
3. **Rich domain meaning**: Event name from ubiquitous language
4. **Complete information**: Event contains all data needed by consumers
5. **Causality tracking**: Record who/what triggered event
6. **Versioning support**: Include schema version for evolution
7. **Idempotent handlers**: Processing same event multiple times safe

### Performance Considerations

```go
// ✅ Batch event publishing for efficiency
func (s *Service) ProcessBatch(ctx context.Context, items []Item) error {
 events := make([]event.DomainEvent, 0, len(items))

 for _, item := range items {
  evt := createEvent(item)
  events = append(events, evt)
 }

 // Publish all events in single batch
 return s.publisher.PublishBatch(ctx, events)
}

// ✅ Use async event bus for non-blocking dispatch
asyncBus := event.NewAsyncEventBus()
asyncBus.DispatchAsync(ctx, event) // Returns immediately

// ✅ Use projections for read-heavy queries
balance := projection.GetBalance(accountID) // Fast lookup, no event replay
```

## Testing Domain Events

### Testing Event Creation

```go
package model_test

import (
 "testing"
 "time"

 "github.com/stretchr/testify/assert"
 "github.com/stretchr/testify/require"

 "yourapp/domain/event"
 "yourapp/domain/model"
)

func TestZakatAccount_ProcessPayment_CreatesEvent(t *testing.T) {
 // Arrange
 accountID := model.NewZakatAccountID()
 account := model.NewZakatAccount(accountID, donorID)

 paymentID := model.NewPaymentID()
 amount := model.NewMoney(100, 0, "USD")

 // Act
 err := account.ProcessPayment(paymentID, amount, model.CreditCard, userID)

 // Assert
 require.NoError(t, err)

 events := account.UncommittedEvents()
 require.Len(t, events, 1, "should create one event")

 evt, ok := events[0].(event.ZakatPaymentProcessed)
 require.True(t, ok, "should be ZakatPaymentProcessed event")

 assert.Equal(t, accountID, evt.AggregateID)
 assert.Equal(t, paymentID, evt.PaymentID)
 assert.Equal(t, amount, evt.Amount)
 assert.Equal(t, "ZakatPaymentProcessed", evt.EventType())
}

func TestZakatPaymentProcessed_Immutability(t *testing.T) {
 // Create event
 original := event.ZakatPaymentProcessed{
  EventID:     event.NewEventID(),
  AggregateID: accountID,
  Amount:      model.NewMoney(100, 0, "USD"),
  OccurredAt:  time.Now(),
  Version:     1,
 }

 // Attempt to use value - should create copy
 evt := original
 evt.Amount = model.NewMoney(200, 0, "USD") // Modifies copy

 // Original unchanged
 assert.NotEqual(t, original.Amount, evt.Amount, "original should be unchanged")
}
```

### Testing Event Handlers

```go
package handler_test

import (
 "context"
 "testing"

 "github.com/stretchr/testify/assert"
 "github.com/stretchr/testify/mock"
 "github.com/stretchr/testify/require"

 "yourapp/domain/event"
 "yourapp/handler"
)

// MockEmailService for testing
type MockEmailService struct {
 mock.Mock
}

func (m *MockEmailService) SendPaymentConfirmation(ctx context.Context, payerID model.DonorID, amount model.Money) error {
 args := m.Called(ctx, payerID, amount)
 return args.Error(0)
}

func TestSendEmailNotificationHandler_Success(t *testing.T) {
 // Arrange
 mockEmail := new(MockEmailService)
 handler := &handler.SendEmailNotificationHandler{
  emailService: mockEmail,
 }

 evt := event.ZakatPaymentProcessed{
  EventID:     event.NewEventID(),
  AggregateID: accountID,
  Amount:      model.NewMoney(100, 0, "USD"),
  PayerID:     donorID,
 }

 mockEmail.On("SendPaymentConfirmation", mock.Anything, evt.PayerID, evt.Amount).
  Return(nil)

 // Act
 err := handler.Handle(context.Background(), evt)

 // Assert
 require.NoError(t, err)
 mockEmail.AssertExpectations(t)
}

func TestSendEmailNotificationHandler_IgnoresOtherEvents(t *testing.T) {
 // Arrange
 mockEmail := new(MockEmailService)
 handler := &handler.SendEmailNotificationHandler{
  emailService: mockEmail,
 }

 evt := event.ZakatReceiptGenerated{
  EventID:     event.NewEventID(),
  AggregateID: receiptID,
 }

 // Act
 err := handler.Handle(context.Background(), evt)

 // Assert
 require.NoError(t, err)
 mockEmail.AssertNotCalled(t, "SendPaymentConfirmation")
}
```

### Testing Event Sourcing

```go
package model_test

import (
 "testing"

 "github.com/stretchr/testify/assert"
 "github.com/stretchr/testify/require"

 "yourapp/domain/event"
 "yourapp/domain/model"
)

func TestReconstituteFromEvents(t *testing.T) {
 // Arrange - event stream
 accountID := model.NewZakatAccountID()
 events := []event.DomainEvent{
  event.ZakatPaymentProcessed{
   EventID:     event.NewEventID(),
   AggregateID: accountID,
   Amount:      model.NewMoney(100, 0, "USD"),
  },
  event.ZakatPaymentProcessed{
   EventID:     event.NewEventID(),
   AggregateID: accountID,
   Amount:      model.NewMoney(50, 0, "USD"),
  },
 }

 // Act - reconstitute from events
 account, err := model.ReconstituteFromEvents(accountID, events)

 // Assert
 require.NoError(t, err)
 assert.Equal(t, accountID, account.ID())
 assert.Equal(t, model.NewMoney(150, 0, "USD"), account.Balance())
 assert.Len(t, account.PaymentHistory(), 2)
}

func TestEventReplay_Idempotency(t *testing.T) {
 // Same event stream replayed twice should produce same state
 accountID := model.NewZakatAccountID()
 events := []event.DomainEvent{
  event.ZakatPaymentProcessed{
   EventID:     event.NewEventID(),
   AggregateID: accountID,
   Amount:      model.NewMoney(100, 0, "USD"),
  },
 }

 account1, _ := model.ReconstituteFromEvents(accountID, events)
 account2, _ := model.ReconstituteFromEvents(accountID, events)

 assert.Equal(t, account1.Balance(), account2.Balance(), "replays should be deterministic")
}
```

## Related Documentation

### DDD Templates

- [Aggregate Template](./aggregate-template.md) - Transactional boundaries and invariant enforcement
- [Entity Template](./entity-template.md) - Entities with identity and lifecycle
- [Value Object Template](./value-object-template.md) - Immutable value objects

### Golang Best Practices

- [Go Best Practices](../ex-so-prla-go__best-practices.md) - Go-specific coding standards
- [Go Idioms](../ex-so-prla-go__idioms.md) - Idiomatic Go patterns
- [Go Concurrency and Parallelism](../ex-so-prla-go__concurrency-and-parallelism.md) - Goroutines and channels for async event processing

### Domain-Driven Design

- [DDD in Go](../ex-so-prla-go__domain-driven-design.md) - Complete DDD patterns in Go

### Related Concepts

- [Repository Pattern](./repository-template.md) - Aggregate persistence

### Conventions & Principles

- [Simplicity Over Complexity Principle](../../../../../../governance/principles/general/simplicity-over-complexity.md) - Event design follows simplicity
- [Explicit Over Implicit Principle](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md) - Events make domain changes explicit

---

**Template Version**: 1.0
**Go Compatibility**: 1.18+ (generics optional), 1.21+ (recommended), 1.22+, 1.23+, 1.24+, 1.25+
**Last Updated**: 2025-01-23
