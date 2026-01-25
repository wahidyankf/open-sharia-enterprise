---
title: Java Entity Template
description: Template for creating Domain-Driven Design entities in Java with identity, lifecycle, and business logic
category: template
tags:
  - java
  - ddd
  - domain-model
  - entity
  - aggregate
related:
  - ex-so-ar-dodrdedd-te__aggregate-design-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2026-01-21
---

# Java Entity Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) entities in Java. Entities have identity, lifecycle, and business logic while maintaining invariants.

## Template Structure

```java
package com.openshariaenterprise.{domain}.model;

import java.time.LocalDateTime;
import java.util.*;

/**
 * {EntityName} entity represents {brief description of what this entity models}.
 *
 * <p>Key Characteristics:
 * <ul>
 *   <li>Identity: Defined by {identity field(s)}</li>
 *   <li>Lifecycle: {Created -> Updated -> Archived/Deleted}</li>
 *   <li>Invariants: {List key business rules that must always hold}</li>
 * </ul>
 *
 * <p>Responsibilities:
 * <ul>
 *   <li>{Primary responsibility 1}</li>
 *   <li>{Primary responsibility 2}</li>
 *   <li>{Primary responsibility 3}</li>
 * </ul>
 *
 * @see RelatedValueObject
 * @see RelatedAggregate
 */
public class EntityName {

    // ========================================
    // Identity
    // ========================================

    private final EntityId id;

    // ========================================
    // Essential State
    // ========================================

    private String name;
    private ValueObject valueObject;
    private EntityStatus status;

    // ========================================
    // Audit Fields
    // ========================================

    private final LocalDateTime createdAt;
    private final UserId createdBy;
    private LocalDateTime updatedAt;
    private UserId updatedBy;
    private int version;

    // ========================================
    // Associations
    // ========================================

    private final Set<RelatedEntity> relatedEntities;

    // ========================================
    // Constructor (Private - Use Factory Methods)
    // ========================================

    private EntityName(
        EntityId id,
        String name,
        ValueObject valueObject,
        UserId createdBy
    ) {
        // Validate invariants
        Objects.requireNonNull(id, "Entity ID must not be null");
        Objects.requireNonNull(name, "Name must not be null");
        validateName(name);
        Objects.requireNonNull(valueObject, "Value object must not be null");
        Objects.requireNonNull(createdBy, "Created by must not be null");

        // Initialize identity
        this.id = id;

        // Initialize state
        this.name = name;
        this.valueObject = valueObject;
        this.status = EntityStatus.ACTIVE;

        // Initialize audit fields
        this.createdAt = LocalDateTime.now();
        this.createdBy = createdBy;
        this.updatedAt = this.createdAt;
        this.updatedBy = createdBy;
        this.version = 0;

        // Initialize associations
        this.relatedEntities = new HashSet<>();
    }

    // ========================================
    // Factory Methods
    // ========================================

    /**
     * Creates a new {EntityName} with the given parameters.
     *
     * @param id the unique identifier
     * @param name the entity name
     * @param valueObject associated value object
     * @param createdBy user creating the entity
     * @return newly created entity
     * @throws IllegalArgumentException if any invariant is violated
     */
    public static EntityName create(
        EntityId id,
        String name,
        ValueObject valueObject,
        UserId createdBy
    ) {
        return new EntityName(id, name, valueObject, createdBy);
    }

    /**
     * Reconstitutes an entity from persistence.
     *
     * @param id the unique identifier
     * @param name the entity name
     * @param valueObject associated value object
     * @param status the entity status
     * @param createdAt creation timestamp
     * @param createdBy creation user
     * @param updatedAt last update timestamp
     * @param updatedBy last update user
     * @param version optimistic locking version
     * @return reconstituted entity
     */
    public static EntityName reconstitute(
        EntityId id,
        String name,
        ValueObject valueObject,
        EntityStatus status,
        LocalDateTime createdAt,
        UserId createdBy,
        LocalDateTime updatedAt,
        UserId updatedBy,
        int version
    ) {
        var entity = new EntityName(id, name, valueObject, createdBy);
        entity.status = status;
        entity.updatedAt = updatedAt;
        entity.updatedBy = updatedBy;
        entity.version = version;
        return entity;
    }

    // ========================================
    // Business Logic
    // ========================================

    /**
     * Updates the entity name.
     *
     * @param newName the new name
     * @param updatedBy user performing the update
     * @throws IllegalArgumentException if name is invalid
     * @throws IllegalStateException if entity is not in updatable state
     */
    public void updateName(String newName, UserId updatedBy) {
        validateUpdatable();
        validateName(newName);

        this.name = newName;
        markUpdated(updatedBy);
    }

    /**
     * Changes entity status with state transition validation.
     *
     * @param newStatus the target status
     * @param updatedBy user performing the status change
     * @throws IllegalStateException if transition is not allowed
     */
    public void changeStatus(EntityStatus newStatus, UserId updatedBy) {
        Objects.requireNonNull(newStatus, "Status must not be null");
        Objects.requireNonNull(updatedBy, "Updated by must not be null");

        if (!canTransitionTo(newStatus)) {
            throw new IllegalStateException(
                String.format("Cannot transition from %s to %s", this.status, newStatus)
            );
        }

        this.status = newStatus;
        markUpdated(updatedBy);
    }

    /**
     * Adds a related entity to this entity's collection.
     *
     * @param relatedEntity the entity to add
     * @param updatedBy user performing the operation
     * @throws IllegalArgumentException if related entity is null or invalid
     * @throws IllegalStateException if entity is not in updatable state
     */
    public void addRelatedEntity(RelatedEntity relatedEntity, UserId updatedBy) {
        validateUpdatable();
        Objects.requireNonNull(relatedEntity, "Related entity must not be null");

        if (relatedEntities.contains(relatedEntity)) {
            throw new IllegalArgumentException("Related entity already exists");
        }

        relatedEntities.add(relatedEntity);
        markUpdated(updatedBy);
    }

    /**
     * Removes a related entity from this entity's collection.
     *
     * @param relatedEntity the entity to remove
     * @param updatedBy user performing the operation
     * @return true if entity was removed, false if not found
     */
    public boolean removeRelatedEntity(RelatedEntity relatedEntity, UserId updatedBy) {
        validateUpdatable();
        Objects.requireNonNull(relatedEntity, "Related entity must not be null");

        boolean removed = relatedEntities.remove(relatedEntity);
        if (removed) {
            markUpdated(updatedBy);
        }
        return removed;
    }

    /**
     * Archives this entity (soft delete).
     *
     * @param archivedBy user performing the archival
     * @throws IllegalStateException if entity cannot be archived
     */
    public void archive(UserId archivedBy) {
        if (status == EntityStatus.ARCHIVED) {
            throw new IllegalStateException("Entity is already archived");
        }

        if (!canBeArchived()) {
            throw new IllegalStateException("Entity cannot be archived in current state");
        }

        this.status = EntityStatus.ARCHIVED;
        markUpdated(archivedBy);
    }

    // ========================================
    // Validation & Invariants
    // ========================================

    private static void validateName(String name) {
        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("Name must not be blank");
        }

        if (name.length() < 3 || name.length() > 100) {
            throw new IllegalArgumentException("Name must be between 3 and 100 characters");
        }
    }

    private void validateUpdatable() {
        if (status == EntityStatus.ARCHIVED || status == EntityStatus.DELETED) {
            throw new IllegalStateException("Cannot update entity in " + status + " state");
        }
    }

    private boolean canTransitionTo(EntityStatus targetStatus) {
        return switch (this.status) {
            case ACTIVE -> targetStatus == EntityStatus.SUSPENDED ||
                          targetStatus == EntityStatus.ARCHIVED;
            case SUSPENDED -> targetStatus == EntityStatus.ACTIVE ||
                             targetStatus == EntityStatus.ARCHIVED;
            case ARCHIVED -> targetStatus == EntityStatus.ACTIVE;
            case DELETED -> false;
        };
    }

    private boolean canBeArchived() {
        return relatedEntities.isEmpty() ||
               relatedEntities.stream().allMatch(RelatedEntity::isArchived);
    }

    // ========================================
    // State Modification Helpers
    // ========================================

    private void markUpdated(UserId updatedBy) {
        this.updatedAt = LocalDateTime.now();
        this.updatedBy = updatedBy;
        this.version++;
    }

    // ========================================
    // Query Methods
    // ========================================

    public EntityId getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public ValueObject getValueObject() {
        return valueObject;
    }

    public EntityStatus getStatus() {
        return status;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public UserId getCreatedBy() {
        return createdBy;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public UserId getUpdatedBy() {
        return updatedBy;
    }

    public int getVersion() {
        return version;
    }

    public Set<RelatedEntity> getRelatedEntities() {
        return Collections.unmodifiableSet(relatedEntities);
    }

    public boolean isActive() {
        return status == EntityStatus.ACTIVE;
    }

    public boolean isArchived() {
        return status == EntityStatus.ARCHIVED;
    }

    // ========================================
    // Equality & Hash Code (Identity-based)
    // ========================================

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EntityName that = (EntityName) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "EntityName{" +
               "id=" + id +
               ", name='" + name + '\'' +
               ", status=" + status +
               ", version=" + version +
               '}';
    }
}

/**
 * Entity status enumeration.
 */
enum EntityStatus {
    ACTIVE,
    SUSPENDED,
    ARCHIVED,
    DELETED
}
```

## Financial Domain Example: Zakat Account Entity

```java
package com.openshariaenterprise.zakat.model;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * ZakatAccount entity represents an account for tracking Zakatable wealth and calculating Zakat obligations.
 *
 * <p>Key Characteristics:
 * <ul>
 *   <li>Identity: Defined by account number</li>
 *   <li>Lifecycle: Opened -> Active -> Closed</li>
 *   <li>Invariants:
 *     <ul>
 *       <li>Balance must not be negative</li>
 *       <li>Nisab threshold must be positive</li>
 *       <li>Haul period must be one lunar year (354-355 days)</li>
 *       <li>Zakat rate must be 2.5% for wealth</li>
 *     </ul>
 *   </li>
 * </ul>
 *
 * <p>Responsibilities:
 * <ul>
 *   <li>Track wealth subject to Zakat</li>
 *   <li>Calculate Zakat obligations when conditions are met</li>
 *   <li>Maintain payment history</li>
 *   <li>Enforce Islamic finance rules</li>
 * </ul>
 */
public class ZakatAccount {

    private static final BigDecimal ZAKAT_RATE = new BigDecimal("0.025"); // 2.5%
    private static final int HAUL_DAYS = 354; // One lunar year

    // Identity
    private final ZakatAccountId id;

    // Essential State
    private final AccountHolderId holderId;
    private Money balance;
    private Money nisabThreshold;
    private ZakatAccountStatus status;

    // Haul Tracking
    private LocalDate haulStartDate;
    private LocalDate lastZakatPaymentDate;

    // Audit Fields
    private final LocalDateTime openedAt;
    private final UserId openedBy;
    private LocalDateTime updatedAt;
    private UserId updatedBy;
    private int version;

    // Payments
    private final List<ZakatPayment> payments;

    // Private Constructor
    private ZakatAccount(
        ZakatAccountId id,
        AccountHolderId holderId,
        Money initialBalance,
        Money nisabThreshold,
        UserId openedBy
    ) {
        // Validate
        Objects.requireNonNull(id, "Account ID must not be null");
        Objects.requireNonNull(holderId, "Holder ID must not be null");
        Objects.requireNonNull(initialBalance, "Initial balance must not be null");
        Objects.requireNonNull(nisabThreshold, "Nisab threshold must not be null");
        Objects.requireNonNull(openedBy, "Opened by must not be null");

        validateBalance(initialBalance);
        validateNisab(nisabThreshold);

        // Initialize
        this.id = id;
        this.holderId = holderId;
        this.balance = initialBalance;
        this.nisabThreshold = nisabThreshold;
        this.status = ZakatAccountStatus.ACTIVE;

        // Haul tracking
        this.haulStartDate = LocalDate.now();
        this.lastZakatPaymentDate = null;

        // Audit
        this.openedAt = LocalDateTime.now();
        this.openedBy = openedBy;
        this.updatedAt = this.openedAt;
        this.updatedBy = openedBy;
        this.version = 0;

        // Payments
        this.payments = new ArrayList<>();
    }

    // Factory Method
    public static ZakatAccount open(
        ZakatAccountId id,
        AccountHolderId holderId,
        Money initialBalance,
        Money nisabThreshold,
        UserId openedBy
    ) {
        return new ZakatAccount(id, holderId, initialBalance, nisabThreshold, openedBy);
    }

    // Business Logic

    /**
     * Calculates Zakat due for this account.
     *
     * @return ZakatCalculation result
     */
    public ZakatCalculation calculateZakat() {
        validateActive();

        // Check if balance meets nisab
        if (balance.isLessThan(nisabThreshold)) {
            return ZakatCalculation.notEligible("Balance below nisab threshold");
        }

        // Check if haul (one lunar year) has passed
        if (!hasCompletedHaul()) {
            return ZakatCalculation.notEligible("Haul period not completed");
        }

        // Calculate 2.5% of wealth
        var zakatAmount = balance.multiply(ZAKAT_RATE);

        return ZakatCalculation.eligible(zakatAmount, balance, nisabThreshold);
    }

    /**
     * Records a Zakat payment.
     *
     * @param payment the Zakat payment to record
     * @param recordedBy user recording the payment
     * @throws IllegalArgumentException if payment is invalid
     * @throws IllegalStateException if account is not active
     */
    public void recordPayment(ZakatPayment payment, UserId recordedBy) {
        validateActive();
        Objects.requireNonNull(payment, "Payment must not be null");
        Objects.requireNonNull(recordedBy, "Recorded by must not be null");

        // Validate payment amount doesn't exceed balance
        if (payment.getAmount().isGreaterThan(balance)) {
            throw new IllegalArgumentException("Payment amount exceeds account balance");
        }

        // Record payment
        payments.add(payment);

        // Update balance
        this.balance = balance.subtract(payment.getAmount());

        // Reset haul tracking
        this.lastZakatPaymentDate = LocalDate.now();
        this.haulStartDate = LocalDate.now();

        markUpdated(recordedBy);
    }

    /**
     * Deposits funds into the account.
     *
     * @param amount amount to deposit
     * @param depositedBy user making the deposit
     */
    public void deposit(Money amount, UserId depositedBy) {
        validateActive();
        Objects.requireNonNull(amount, "Amount must not be null");
        Objects.requireNonNull(depositedBy, "Deposited by must not be null");

        if (amount.isNegativeOrZero()) {
            throw new IllegalArgumentException("Deposit amount must be positive");
        }

        this.balance = balance.add(amount);
        markUpdated(depositedBy);
    }

    /**
     * Withdraws funds from the account.
     *
     * @param amount amount to withdraw
     * @param withdrawnBy user making the withdrawal
     */
    public void withdraw(Money amount, UserId withdrawnBy) {
        validateActive();
        Objects.requireNonNull(amount, "Amount must not be null");
        Objects.requireNonNull(withdrawnBy, "Withdrawn by must not be null");

        if (amount.isNegativeOrZero()) {
            throw new IllegalArgumentException("Withdrawal amount must be positive");
        }

        var newBalance = balance.subtract(amount);
        if (newBalance.isNegative()) {
            throw new IllegalArgumentException("Insufficient balance");
        }

        this.balance = newBalance;
        markUpdated(withdrawnBy);
    }

    /**
     * Closes the account.
     *
     * @param closedBy user closing the account
     */
    public void close(UserId closedBy) {
        validateActive();

        if (balance.isPositive()) {
            throw new IllegalStateException("Cannot close account with positive balance");
        }

        this.status = ZakatAccountStatus.CLOSED;
        markUpdated(closedBy);
    }

    // Validation

    private static void validateBalance(Money balance) {
        if (balance.isNegative()) {
            throw new IllegalArgumentException("Balance must not be negative");
        }
    }

    private static void validateNisab(Money nisab) {
        if (nisab.isNegativeOrZero()) {
            throw new IllegalArgumentException("Nisab threshold must be positive");
        }
    }

    private void validateActive() {
        if (status != ZakatAccountStatus.ACTIVE) {
            throw new IllegalStateException("Account is not active");
        }
    }

    private boolean hasCompletedHaul() {
        var daysSinceHaulStart = LocalDate.now().toEpochDay() - haulStartDate.toEpochDay();
        return daysSinceHaulStart >= HAUL_DAYS;
    }

    private void markUpdated(UserId updatedBy) {
        this.updatedAt = LocalDateTime.now();
        this.updatedBy = updatedBy;
        this.version++;
    }

    // Getters

    public ZakatAccountId getId() {
        return id;
    }

    public AccountHolderId getHolderId() {
        return holderId;
    }

    public Money getBalance() {
        return balance;
    }

    public Money getNisabThreshold() {
        return nisabThreshold;
    }

    public ZakatAccountStatus getStatus() {
        return status;
    }

    public LocalDate getHaulStartDate() {
        return haulStartDate;
    }

    public Optional<LocalDate> getLastZakatPaymentDate() {
        return Optional.ofNullable(lastZakatPaymentDate);
    }

    public List<ZakatPayment> getPayments() {
        return Collections.unmodifiableList(payments);
    }

    public boolean isActive() {
        return status == ZakatAccountStatus.ACTIVE;
    }

    public boolean isEligibleForZakat() {
        return balance.isGreaterThanOrEqualTo(nisabThreshold) && hasCompletedHaul();
    }

    // Equality (Identity-based)

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ZakatAccount that = (ZakatAccount) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "ZakatAccount{" +
               "id=" + id +
               ", balance=" + balance +
               ", status=" + status +
               ", eligibleForZakat=" + isEligibleForZakat() +
               '}';
    }
}

enum ZakatAccountStatus {
    ACTIVE,
    SUSPENDED,
    CLOSED
}
```

## Usage Guidelines

1. **Identity**: Always use strongly-typed ID value objects, never primitives
2. **Immutability**: Use `final` for identity and reference fields
3. **Invariants**: Validate all invariants in constructors and business methods
4. **Encapsulation**: Keep state private, expose behavior through methods
5. **Factory Methods**: Use static factory methods instead of public constructors
6. **State Changes**: All mutations should go through business methods
7. **Equality**: Implement identity-based equality using ID only
8. **Version**: Use version field for optimistic locking

## Related Templates

- [Value Object Template](./ex-so-prla-ja-te__value-object-template.md)
- [Aggregate Template](./ex-so-prla-ja-te__aggregate-template.md)
- [Domain Event Template](./ex-so-prla-ja-te__domain-event-template.md)

## See Also

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
