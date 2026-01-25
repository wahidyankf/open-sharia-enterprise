---
title: Java Domain Event Template
description: Template for creating domain events in Java with event sourcing patterns and immutability
category: template
tags:
  - java
  - ddd
  - domain-events
  - event-sourcing
  - immutability
related:
  - ex-so-prla-ja-te__aggregate-template.md
  - ex-so-prla-ja-te__entity-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2026-01-21
---

# Java Domain Event Template

This template provides a standardized structure for creating domain events in Java. Domain events capture significant business occurrences and enable loose coupling between aggregates.

## Template Structure

```java
package com.openshariaenterprise.{domain}.events;

import java.time.Instant;
import java.util.Objects;
import java.util.UUID;

/**
 * {EventName} represents {description of what happened in the domain}.
 *
 * <p>Event Characteristics:
 * <ul>
 *   <li>Immutable - Event data cannot be changed after creation</li>
 *   <li>Past tense naming - Describes what happened, not what will happen</li>
 *   <li>Rich in context - Contains all relevant information</li>
 *   <li>Self-contained - Can be understood without external queries</li>
 * </ul>
 *
 * <p>Event Metadata:
 * <ul>
 *   <li>Event ID - Unique identifier for this event</li>
 *   <li>Occurred At - When the event happened</li>
 *   <li>Aggregate ID - Which aggregate produced this event</li>
 *   <li>Aggregate Type - Type of aggregate that produced this event</li>
 *   <li>Event Version - Schema version for evolution</li>
 * </ul>
 *
 * <p>Use Cases:
 * <ul>
 *   <li>Notify other aggregates of state changes</li>
 *   <li>Trigger side effects (notifications, analytics)</li>
 *   <li>Build read models via event sourcing</li>
 *   <li>Audit trail and compliance</li>
 * </ul>
 *
 * @see RelatedAggregate
 * @see DomainEvent
 */
public final class EventName implements DomainEvent {

    // ========================================
    // Event Metadata (Standard for all events)
    // ========================================

    private final UUID eventId;
    private final Instant occurredAt;
    private final AggregateId aggregateId;
    private final String aggregateType;
    private final int eventVersion;

    // ========================================
    // Event Payload (Domain-specific data)
    // ========================================

    private final String businessField1;
    private final Money businessField2;
    private final ValueObject businessField3;
    private final UserId performedBy;

    // ========================================
    // Constructor (Private - Use Factory Methods)
    // ========================================

    private EventName(
        UUID eventId,
        Instant occurredAt,
        AggregateId aggregateId,
        String aggregateType,
        int eventVersion,
        String businessField1,
        Money businessField2,
        ValueObject businessField3,
        UserId performedBy
    ) {
        // Validate metadata
        this.eventId = Objects.requireNonNull(eventId, "Event ID must not be null");
        this.occurredAt = Objects.requireNonNull(occurredAt, "Occurred at must not be null");
        this.aggregateId = Objects.requireNonNull(aggregateId, "Aggregate ID must not be null");
        this.aggregateType = Objects.requireNonNull(aggregateType, "Aggregate type must not be null");
        this.eventVersion = eventVersion;

        // Validate payload
        this.businessField1 = Objects.requireNonNull(businessField1, "Business field 1 must not be null");
        this.businessField2 = Objects.requireNonNull(businessField2, "Business field 2 must not be null");
        this.businessField3 = Objects.requireNonNull(businessField3, "Business field 3 must not be null");
        this.performedBy = Objects.requireNonNull(performedBy, "Performed by must not be null");
    }

    // ========================================
    // Factory Methods
    // ========================================

    /**
     * Creates a new {EventName} with the specified data.
     *
     * @param aggregateId the ID of the aggregate that produced this event
     * @param businessField1 first business field
     * @param businessField2 second business field
     * @param businessField3 third business field
     * @param performedBy user who performed the action
     * @return newly created event
     */
    public static EventName create(
        AggregateId aggregateId,
        String businessField1,
        Money businessField2,
        ValueObject businessField3,
        UserId performedBy
    ) {
        return new EventName(
            UUID.randomUUID(),
            Instant.now(),
            aggregateId,
            "AggregateName",
            1, // Current event schema version
            businessField1,
            businessField2,
            businessField3,
            performedBy
        );
    }

    /**
     * Reconstitutes an event from persistence/event store.
     *
     * @param eventId stored event ID
     * @param occurredAt stored timestamp
     * @param aggregateId stored aggregate ID
     * @param aggregateType stored aggregate type
     * @param eventVersion stored event version
     * @param businessField1 stored field 1
     * @param businessField2 stored field 2
     * @param businessField3 stored field 3
     * @param performedBy stored user
     * @return reconstituted event
     */
    public static EventName reconstitute(
        UUID eventId,
        Instant occurredAt,
        AggregateId aggregateId,
        String aggregateType,
        int eventVersion,
        String businessField1,
        Money businessField2,
        ValueObject businessField3,
        UserId performedBy
    ) {
        return new EventName(
            eventId,
            occurredAt,
            aggregateId,
            aggregateType,
            eventVersion,
            businessField1,
            businessField2,
            businessField3,
            performedBy
        );
    }

    // ========================================
    // DomainEvent Interface Implementation
    // ========================================

    @Override
    public UUID getEventId() {
        return eventId;
    }

    @Override
    public Instant getOccurredAt() {
        return occurredAt;
    }

    @Override
    public AggregateId getAggregateId() {
        return aggregateId;
    }

    @Override
    public String getAggregateType() {
        return aggregateType;
    }

    @Override
    public int getEventVersion() {
        return eventVersion;
    }

    @Override
    public String getEventType() {
        return this.getClass().getSimpleName();
    }

    // ========================================
    // Payload Accessors
    // ========================================

    public String getBusinessField1() {
        return businessField1;
    }

    public Money getBusinessField2() {
        return businessField2;
    }

    public ValueObject getBusinessField3() {
        return businessField3;
    }

    public UserId getPerformedBy() {
        return performedBy;
    }

    // ========================================
    // Equality & Hash Code (Identity-based on eventId)
    // ========================================

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EventName that = (EventName) o;
        return Objects.equals(eventId, that.eventId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(eventId);
    }

    @Override
    public String toString() {
        return "EventName{" +
               "eventId=" + eventId +
               ", occurredAt=" + occurredAt +
               ", aggregateId=" + aggregateId +
               ", businessField1='" + businessField1 + '\'' +
               ", businessField2=" + businessField2 +
               ", performedBy=" + performedBy +
               '}';
    }
}

/**
 * Base interface for all domain events.
 */
interface DomainEvent {
    UUID getEventId();
    Instant getOccurredAt();
    AggregateId getAggregateId();
    String getAggregateType();
    String getEventType();
    int getEventVersion();
}
```

## Financial Domain Example: ZakatPaymentProcessed Event

```java
package com.openshariaenterprise.zakat.events;

import java.time.Instant;
import java.util.Objects;
import java.util.UUID;

/**
 * ZakatPaymentProcessed event indicates that a Zakat payment has been successfully processed.
 *
 * <p>This event is published when:
 * <ul>
 *   <li>A Zakat payment is completed successfully</li>
 *   <li>The account balance has been updated</li>
 *   <li>The payment has been recorded in the payment history</li>
 * </ul>
 *
 * <p>Event Handlers:
 * <ul>
 *   <li>Receipt Generator - Creates PDF receipt</li>
 *   <li>Notification Service - Sends confirmation email/SMS</li>
 *   <li>Analytics Service - Records payment statistics</li>
 *   <li>Audit Service - Logs payment for compliance</li>
 * </ul>
 *
 * @see ZakatAccount
 * @see ZakatPayment
 */
public final class ZakatPaymentProcessed implements DomainEvent {

    // Event Metadata
    private final UUID eventId;
    private final Instant occurredAt;
    private final ZakatAccountId accountId;
    private final String aggregateType;
    private final int eventVersion;

    // Event Payload
    private final ZakatPaymentId paymentId;
    private final Money paymentAmount;
    private final Money previousBalance;
    private final Money newBalance;
    private final String recipient;
    private final PaymentMethod paymentMethod;
    private final UserId processedBy;

    // Constructor
    private ZakatPaymentProcessed(
        UUID eventId,
        Instant occurredAt,
        ZakatAccountId accountId,
        String aggregateType,
        int eventVersion,
        ZakatPaymentId paymentId,
        Money paymentAmount,
        Money previousBalance,
        Money newBalance,
        String recipient,
        PaymentMethod paymentMethod,
        UserId processedBy
    ) {
        this.eventId = Objects.requireNonNull(eventId);
        this.occurredAt = Objects.requireNonNull(occurredAt);
        this.accountId = Objects.requireNonNull(accountId);
        this.aggregateType = Objects.requireNonNull(aggregateType);
        this.eventVersion = eventVersion;

        this.paymentId = Objects.requireNonNull(paymentId);
        this.paymentAmount = Objects.requireNonNull(paymentAmount);
        this.previousBalance = Objects.requireNonNull(previousBalance);
        this.newBalance = Objects.requireNonNull(newBalance);
        this.recipient = Objects.requireNonNull(recipient);
        this.paymentMethod = Objects.requireNonNull(paymentMethod);
        this.processedBy = Objects.requireNonNull(processedBy);
    }

    // Factory Method
    public static ZakatPaymentProcessed create(
        ZakatAccountId accountId,
        ZakatPaymentId paymentId,
        Money paymentAmount,
        Money previousBalance,
        Money newBalance,
        String recipient,
        PaymentMethod paymentMethod,
        UserId processedBy
    ) {
        return new ZakatPaymentProcessed(
            UUID.randomUUID(),
            Instant.now(),
            accountId,
            "ZakatAccount",
            1,
            paymentId,
            paymentAmount,
            previousBalance,
            newBalance,
            recipient,
            paymentMethod,
            processedBy
        );
    }

    // Reconstitution from Event Store
    public static ZakatPaymentProcessed reconstitute(
        UUID eventId,
        Instant occurredAt,
        ZakatAccountId accountId,
        String aggregateType,
        int eventVersion,
        ZakatPaymentId paymentId,
        Money paymentAmount,
        Money previousBalance,
        Money newBalance,
        String recipient,
        PaymentMethod paymentMethod,
        UserId processedBy
    ) {
        return new ZakatPaymentProcessed(
            eventId,
            occurredAt,
            accountId,
            aggregateType,
            eventVersion,
            paymentId,
            paymentAmount,
            previousBalance,
            newBalance,
            recipient,
            paymentMethod,
            processedBy
        );
    }

    // DomainEvent Interface

    @Override
    public UUID getEventId() {
        return eventId;
    }

    @Override
    public Instant getOccurredAt() {
        return occurredAt;
    }

    @Override
    public ZakatAccountId getAggregateId() {
        return accountId;
    }

    @Override
    public String getAggregateType() {
        return aggregateType;
    }

    @Override
    public String getEventType() {
        return "ZakatPaymentProcessed";
    }

    @Override
    public int getEventVersion() {
        return eventVersion;
    }

    // Payload Accessors

    public ZakatPaymentId getPaymentId() {
        return paymentId;
    }

    public Money getPaymentAmount() {
        return paymentAmount;
    }

    public Money getPreviousBalance() {
        return previousBalance;
    }

    public Money getNewBalance() {
        return newBalance;
    }

    public String getRecipient() {
        return recipient;
    }

    public PaymentMethod getPaymentMethod() {
        return paymentMethod;
    }

    public UserId getProcessedBy() {
        return processedBy;
    }

    // Derived Information

    public boolean isSignificantAmount() {
        return paymentAmount.isGreaterThan(Money.of(1000, paymentAmount.getCurrencyCode()));
    }

    public boolean requiresAdditionalReview() {
        return paymentAmount.isGreaterThan(Money.of(10000, paymentAmount.getCurrencyCode()));
    }

    // Equality

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ZakatPaymentProcessed that = (ZakatPaymentProcessed) o;
        return Objects.equals(eventId, that.eventId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(eventId);
    }

    @Override
    public String toString() {
        return "ZakatPaymentProcessed{" +
               "eventId=" + eventId +
               ", occurredAt=" + occurredAt +
               ", accountId=" + accountId +
               ", paymentId=" + paymentId +
               ", paymentAmount=" + paymentAmount +
               ", recipient='" + recipient + '\'' +
               ", paymentMethod=" + paymentMethod +
               '}';
    }
}
```

## Additional Financial Events

### DonationReceived Event

```java
public final class DonationReceived implements DomainEvent {
    private final UUID eventId;
    private final Instant occurredAt;
    private final CampaignId campaignId;

    private final DonationId donationId;
    private final DonorId donorId;
    private final Money amount;
    private final String donorMessage;
    private final boolean isAnonymous;

    public static DonationReceived create(
        CampaignId campaignId,
        DonationId donationId,
        DonorId donorId,
        Money amount,
        String donorMessage,
        boolean isAnonymous
    ) {
        return new DonationReceived(
            UUID.randomUUID(),
            Instant.now(),
            campaignId,
            donationId,
            donorId,
            amount,
            donorMessage,
            isAnonymous
        );
    }

    // ... implementation similar to above
}
```

### CampaignGoalReached Event

```java
public final class CampaignGoalReached implements DomainEvent {
    private final UUID eventId;
    private final Instant occurredAt;
    private final CampaignId campaignId;

    private final Money goalAmount;
    private final Money totalRaised;
    private final int totalDonors;
    private final Instant goalReachedAt;

    public static CampaignGoalReached create(
        CampaignId campaignId,
        Money goalAmount,
        Money totalRaised,
        int totalDonors
    ) {
        return new CampaignGoalReached(
            UUID.randomUUID(),
            Instant.now(),
            campaignId,
            goalAmount,
            totalRaised,
            totalDonors,
            Instant.now()
        );
    }

    // ... implementation
}
```

## Event Handler Pattern

```java
/**
 * Event handler interface for domain events.
 */
@FunctionalInterface
public interface DomainEventHandler<T extends DomainEvent> {
    void handle(T event);
}

/**
 * Example: Receipt generation handler for Zakat payments.
 */
public class ZakatPaymentReceiptHandler implements DomainEventHandler<ZakatPaymentProcessed> {

    private final ReceiptGenerator receiptGenerator;
    private final ReceiptRepository receiptRepository;

    public ZakatPaymentReceiptHandler(
        ReceiptGenerator receiptGenerator,
        ReceiptRepository receiptRepository
    ) {
        this.receiptGenerator = receiptGenerator;
        this.receiptRepository = receiptRepository;
    }

    @Override
    public void handle(ZakatPaymentProcessed event) {
        // Generate PDF receipt
        var receipt = receiptGenerator.generateZakatReceipt(
            event.getPaymentId(),
            event.getPaymentAmount(),
            event.getRecipient(),
            event.getOccurredAt()
        );

        // Store receipt
        receiptRepository.save(receipt);

        // Log success
        log.info("Generated receipt for payment: {}", event.getPaymentId());
    }
}
```

## Event Publisher Pattern

```java
/**
 * Domain event publisher for publishing events to handlers.
 */
public class DomainEventPublisher {

    private final Map<Class<? extends DomainEvent>, List<DomainEventHandler<?>>> handlers;

    public DomainEventPublisher() {
        this.handlers = new ConcurrentHashMap<>();
    }

    public <T extends DomainEvent> void subscribe(
        Class<T> eventType,
        DomainEventHandler<T> handler
    ) {
        handlers.computeIfAbsent(eventType, k -> new ArrayList<>())
                .add(handler);
    }

    @SuppressWarnings("unchecked")
    public void publish(DomainEvent event) {
        var eventHandlers = handlers.get(event.getClass());

        if (eventHandlers != null) {
            for (var handler : eventHandlers) {
                try {
                    ((DomainEventHandler<DomainEvent>) handler).handle(event);
                } catch (Exception e) {
                    log.error("Error handling event: " + event.getEventId(), e);
                    // Consider: dead letter queue, retry logic
                }
            }
        }
    }
}
```

## Usage Guidelines

1. **Past Tense Naming**: Event names describe what happened (e.g., "PaymentProcessed", not "ProcessPayment")
2. **Immutability**: All fields final, no setters
3. **Rich Context**: Include all relevant information, avoid requiring additional queries
4. **Event Versioning**: Track schema version for evolution
5. **Event Sourcing**: Can be used to rebuild aggregate state
6. **Idempotency**: Handlers should handle duplicate events gracefully
7. **Async Handling**: Consider asynchronous event processing for scalability
8. **Error Handling**: Implement retry logic and dead letter queues

## Related Templates

- [Aggregate Template](./ex-so-prla-ja-te__aggregate-template.md)
- [Entity Template](./ex-so-prla-ja-te__entity-template.md)
- [Value Object Template](./ex-so-prla-ja-te__value-object-template.md)

## See Also

- [Domain Events Pattern](https://martinfowler.com/eaaDev/DomainEvent.html)
- [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
