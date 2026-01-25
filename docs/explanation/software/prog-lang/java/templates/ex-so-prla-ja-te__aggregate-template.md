---
title: Java Aggregate Template
description: Template for creating Domain-Driven Design aggregates in Java with transactional boundaries and consistency enforcement
category: template
tags:
  - java
  - ddd
  - domain-model
  - aggregate
  - aggregate-root
  - consistency
related:
  - ex-so-prla-ja-te__entity-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2026-01-21
---

# Java Aggregate Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) aggregates in Java. Aggregates are clusters of entities and value objects with a defined transactional boundary, enforcing invariants and consistency.

## Template Structure

```java
package com.openshariaenterprise.{domain}.model;

import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * {AggregateName} aggregate represents {brief description of the aggregate}.
 *
 * <p>Aggregate Root: {RootEntityName}
 *
 * <p>Transactional Boundary:
 * <ul>
 *   <li>Root Entity: {RootEntity}</li>
 *   <li>Child Entities: {ChildEntity1}, {ChildEntity2}</li>
 *   <li>Value Objects: {ValueObject1}, {ValueObject2}</li>
 * </ul>
 *
 * <p>Invariants (Enforced across the aggregate):
 * <ul>
 *   <li>{Invariant 1 that spans multiple entities}</li>
 *   <li>{Invariant 2 that involves relationships}</li>
 *   <li>{Invariant 3 about aggregate-level consistency}</li>
 * </ul>
 *
 * <p>Responsibilities:
 * <ul>
 *   <li>{Main business operation 1}</li>
 *   <li>{Main business operation 2}</li>
 *   <li>Enforce all aggregate-level invariants</li>
 *   <li>Coordinate state changes across child entities</li>
 * </ul>
 *
 * <p>External Access Rules:
 * <ul>
 *   <li>All modifications must go through the aggregate root</li>
 *   <li>External references should only hold root's ID</li>
 *   <li>Child entities cannot be directly modified from outside</li>
 * </ul>
 *
 * @see ChildEntity
 * @see RelatedValueObject
 */
public class AggregateName {

    // ========================================
    // Aggregate Root Identity
    // ========================================

    private final AggregateId id;

    // ========================================
    // Root Entity State
    // ========================================

    private String rootProperty;
    private ValueObject rootValueObject;
    private AggregateStatus status;

    // ========================================
    // Child Entities (Managed by Root)
    // ========================================

    private final List<ChildEntity> childEntities;
    private final Map<ChildEntityId, ChildEntity> childEntityIndex;

    // ========================================
    // Domain Events
    // ========================================

    private final List<DomainEvent> domainEvents;

    // ========================================
    // Audit Fields
    // ========================================

    private final LocalDateTime createdAt;
    private final UserId createdBy;
    private LocalDateTime updatedAt;
    private UserId updatedBy;
    private int version;

    // ========================================
    // Constructor (Private - Use Factory Methods)
    // ========================================

    private AggregateName(
        AggregateId id,
        String rootProperty,
        ValueObject rootValueObject,
        UserId createdBy
    ) {
        // Validate invariants
        Objects.requireNonNull(id, "Aggregate ID must not be null");
        Objects.requireNonNull(rootProperty, "Root property must not be null");
        validateRootProperty(rootProperty);
        Objects.requireNonNull(rootValueObject, "Root value object must not be null");
        Objects.requireNonNull(createdBy, "Created by must not be null");

        // Initialize root
        this.id = id;
        this.rootProperty = rootProperty;
        this.rootValueObject = rootValueObject;
        this.status = AggregateStatus.ACTIVE;

        // Initialize child collections
        this.childEntities = new ArrayList<>();
        this.childEntityIndex = new HashMap<>();

        // Initialize domain events
        this.domainEvents = new ArrayList<>();

        // Initialize audit
        this.createdAt = LocalDateTime.now();
        this.createdBy = createdBy;
        this.updatedAt = this.createdAt;
        this.updatedBy = createdBy;
        this.version = 0;

        // Register creation event
        registerEvent(new AggregateCreated(id, createdAt, createdBy));
    }

    // ========================================
    // Factory Methods
    // ========================================

    /**
     * Creates a new aggregate with the given parameters.
     *
     * @param id the aggregate identifier
     * @param rootProperty the root property
     * @param rootValueObject the root value object
     * @param createdBy user creating the aggregate
     * @return newly created aggregate
     * @throws IllegalArgumentException if any invariant is violated
     */
    public static AggregateName create(
        AggregateId id,
        String rootProperty,
        ValueObject rootValueObject,
        UserId createdBy
    ) {
        return new AggregateName(id, rootProperty, rootValueObject, createdBy);
    }

    /**
     * Reconstitutes an aggregate from persistence.
     *
     * @param id aggregate identifier
     * @param rootProperty root property
     * @param rootValueObject root value object
     * @param status aggregate status
     * @param childEntities list of child entities
     * @param createdAt creation timestamp
     * @param createdBy creation user
     * @param updatedAt last update timestamp
     * @param updatedBy last update user
     * @param version optimistic locking version
     * @return reconstituted aggregate
     */
    public static AggregateName reconstitute(
        AggregateId id,
        String rootProperty,
        ValueObject rootValueObject,
        AggregateStatus status,
        List<ChildEntity> childEntities,
        LocalDateTime createdAt,
        UserId createdBy,
        LocalDateTime updatedAt,
        UserId updatedBy,
        int version
    ) {
        var aggregate = new AggregateName(id, rootProperty, rootValueObject, createdBy);
        aggregate.status = status;
        aggregate.childEntities.addAll(childEntities);
        childEntities.forEach(child -> aggregate.childEntityIndex.put(child.getId(), child));
        aggregate.updatedAt = updatedAt;
        aggregate.updatedBy = updatedBy;
        aggregate.version = version;
        aggregate.domainEvents.clear(); // Don't republish events on reconstitution
        return aggregate;
    }

    // ========================================
    // Root-Level Business Operations
    // ========================================

    /**
     * Updates the root property with validation.
     *
     * @param newProperty new value for root property
     * @param updatedBy user performing the update
     * @throws IllegalArgumentException if property is invalid
     * @throws IllegalStateException if aggregate is not in updatable state
     */
    public void updateRootProperty(String newProperty, UserId updatedBy) {
        validateActive();
        validateRootProperty(newProperty);

        this.rootProperty = newProperty;
        markUpdated(updatedBy);

        registerEvent(new AggregateUpdated(id, LocalDateTime.now(), updatedBy));
    }

    /**
     * Changes aggregate status with validation.
     *
     * @param newStatus target status
     * @param updatedBy user performing the status change
     * @throws IllegalStateException if transition is not allowed
     */
    public void changeStatus(AggregateStatus newStatus, UserId updatedBy) {
        Objects.requireNonNull(newStatus, "Status must not be null");
        Objects.requireNonNull(updatedBy, "Updated by must not be null");

        if (!canTransitionTo(newStatus)) {
            throw new IllegalStateException(
                String.format("Cannot transition from %s to %s", this.status, newStatus)
            );
        }

        var oldStatus = this.status;
        this.status = newStatus;
        markUpdated(updatedBy);

        registerEvent(new AggregateStatusChanged(id, oldStatus, newStatus, LocalDateTime.now(), updatedBy));
    }

    // ========================================
    // Child Entity Management (Through Root)
    // ========================================

    /**
     * Adds a child entity to the aggregate.
     *
     * @param childEntity the child entity to add
     * @param addedBy user performing the operation
     * @throws IllegalArgumentException if child is invalid or violates invariants
     * @throws IllegalStateException if aggregate is not in valid state
     */
    public void addChildEntity(ChildEntity childEntity, UserId addedBy) {
        validateActive();
        Objects.requireNonNull(childEntity, "Child entity must not be null");

        // Check for duplicates
        if (childEntityIndex.containsKey(childEntity.getId())) {
            throw new IllegalArgumentException("Child entity already exists: " + childEntity.getId());
        }

        // Validate aggregate-level invariants
        validateChildEntityAddition(childEntity);

        // Add child
        childEntities.add(childEntity);
        childEntityIndex.put(childEntity.getId(), childEntity);

        markUpdated(addedBy);
        registerEvent(new ChildEntityAdded(id, childEntity.getId(), LocalDateTime.now(), addedBy));
    }

    /**
     * Removes a child entity from the aggregate.
     *
     * @param childEntityId the ID of child entity to remove
     * @param removedBy user performing the operation
     * @throws IllegalArgumentException if child is not found
     * @throws IllegalStateException if removal violates invariants
     */
    public void removeChildEntity(ChildEntityId childEntityId, UserId removedBy) {
        validateActive();
        Objects.requireNonNull(childEntityId, "Child entity ID must not be null");

        var childEntity = childEntityIndex.get(childEntityId);
        if (childEntity == null) {
            throw new IllegalArgumentException("Child entity not found: " + childEntityId);
        }

        // Validate aggregate-level invariants
        validateChildEntityRemoval(childEntity);

        // Remove child
        childEntities.remove(childEntity);
        childEntityIndex.remove(childEntityId);

        markUpdated(removedBy);
        registerEvent(new ChildEntityRemoved(id, childEntityId, LocalDateTime.now(), removedBy));
    }

    /**
     * Updates a child entity through the aggregate root.
     *
     * @param childEntityId the ID of child to update
     * @param updateOperation the update operation to perform
     * @param updatedBy user performing the operation
     * @throws IllegalArgumentException if child is not found
     * @throws IllegalStateException if update violates invariants
     */
    public void updateChildEntity(
        ChildEntityId childEntityId,
        ChildEntityUpdateOperation updateOperation,
        UserId updatedBy
    ) {
        validateActive();
        Objects.requireNonNull(childEntityId, "Child entity ID must not be null");
        Objects.requireNonNull(updateOperation, "Update operation must not be null");

        var childEntity = childEntityIndex.get(childEntityId);
        if (childEntity == null) {
            throw new IllegalArgumentException("Child entity not found: " + childEntityId);
        }

        // Apply update
        updateOperation.apply(childEntity);

        // Validate aggregate-level invariants after update
        validateAggregateInvariants();

        markUpdated(updatedBy);
        registerEvent(new ChildEntityUpdated(id, childEntityId, LocalDateTime.now(), updatedBy));
    }

    // ========================================
    // Aggregate-Level Business Operations
    // ========================================

    /**
     * Performs a complex operation across multiple child entities.
     *
     * @param operationParams parameters for the operation
     * @param performedBy user performing the operation
     * @return result of the operation
     * @throws IllegalStateException if operation cannot be performed
     */
    public OperationResult performComplexOperation(
        OperationParams operationParams,
        UserId performedBy
    ) {
        validateActive();
        Objects.requireNonNull(operationParams, "Operation params must not be null");

        // Validate preconditions
        if (!canPerformOperation(operationParams)) {
            throw new IllegalStateException("Cannot perform operation in current state");
        }

        // Perform operation across child entities
        var affectedChildren = childEntities.stream()
            .filter(operationParams.getFilter())
            .collect(Collectors.toList());

        for (var child : affectedChildren) {
            operationParams.getTransformation().apply(child);
        }

        // Validate aggregate invariants after operation
        validateAggregateInvariants();

        markUpdated(performedBy);

        var result = new OperationResult(affectedChildren.size(), LocalDateTime.now());
        registerEvent(new ComplexOperationPerformed(id, result, LocalDateTime.now(), performedBy));

        return result;
    }

    // ========================================
    // Validation & Invariants
    // ========================================

    private static void validateRootProperty(String property) {
        if (property == null || property.isBlank()) {
            throw new IllegalArgumentException("Root property must not be blank");
        }
    }

    private void validateActive() {
        if (status != AggregateStatus.ACTIVE) {
            throw new IllegalStateException("Aggregate is not active: " + status);
        }
    }

    private boolean canTransitionTo(AggregateStatus targetStatus) {
        return switch (this.status) {
            case ACTIVE -> targetStatus == AggregateStatus.SUSPENDED ||
                          targetStatus == AggregateStatus.COMPLETED;
            case SUSPENDED -> targetStatus == AggregateStatus.ACTIVE ||
                             targetStatus == AggregateStatus.COMPLETED;
            case COMPLETED -> false;
        };
    }

    private void validateChildEntityAddition(ChildEntity childEntity) {
        // Example: Validate maximum number of children
        if (childEntities.size() >= 100) {
            throw new IllegalStateException("Cannot add more than 100 child entities");
        }

        // Example: Validate business rules
        if (!childEntity.isValid()) {
            throw new IllegalArgumentException("Child entity is not valid");
        }
    }

    private void validateChildEntityRemoval(ChildEntity childEntity) {
        // Example: Validate minimum number of children
        if (childEntities.size() <= 1) {
            throw new IllegalStateException("Cannot remove last child entity");
        }
    }

    private void validateAggregateInvariants() {
        // Example invariant: Total of child values must not exceed root limit
        var totalValue = childEntities.stream()
            .mapToInt(ChildEntity::getValue)
            .sum();

        if (totalValue > rootValueObject.getMaxLimit()) {
            throw new IllegalStateException("Total child values exceed root limit");
        }

        // Example invariant: At least one active child required
        var hasActiveChild = childEntities.stream()
            .anyMatch(ChildEntity::isActive);

        if (!hasActiveChild) {
            throw new IllegalStateException("At least one active child entity is required");
        }
    }

    private boolean canPerformOperation(OperationParams params) {
        return status == AggregateStatus.ACTIVE &&
               !childEntities.isEmpty() &&
               params.isValid();
    }

    // ========================================
    // Domain Events
    // ========================================

    private void registerEvent(DomainEvent event) {
        domainEvents.add(event);
    }

    /**
     * Gets and clears all uncommitted domain events.
     *
     * @return list of domain events to be published
     */
    public List<DomainEvent> getAndClearDomainEvents() {
        var events = new ArrayList<>(domainEvents);
        domainEvents.clear();
        return events;
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

    public AggregateId getId() {
        return id;
    }

    public String getRootProperty() {
        return rootProperty;
    }

    public ValueObject getRootValueObject() {
        return rootValueObject;
    }

    public AggregateStatus getStatus() {
        return status;
    }

    public List<ChildEntity> getChildEntities() {
        return Collections.unmodifiableList(childEntities);
    }

    public Optional<ChildEntity> findChildEntity(ChildEntityId childId) {
        return Optional.ofNullable(childEntityIndex.get(childId));
    }

    public List<ChildEntity> findChildEntitiesBy(Predicate<ChildEntity> predicate) {
        return childEntities.stream()
            .filter(predicate)
            .collect(Collectors.toUnmodifiableList());
    }

    public int getChildEntityCount() {
        return childEntities.size();
    }

    public boolean hasChildEntity(ChildEntityId childId) {
        return childEntityIndex.containsKey(childId);
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

    public boolean isActive() {
        return status == AggregateStatus.ACTIVE;
    }

    // ========================================
    // Equality & Hash Code (Identity-based)
    // ========================================

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AggregateName that = (AggregateName) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "AggregateName{" +
               "id=" + id +
               ", rootProperty='" + rootProperty + '\'' +
               ", status=" + status +
               ", childCount=" + childEntities.size() +
               ", version=" + version +
               '}';
    }
}

/**
 * Aggregate status enumeration.
 */
enum AggregateStatus {
    ACTIVE,
    SUSPENDED,
    COMPLETED
}

/**
 * Functional interface for child entity update operations.
 */
@FunctionalInterface
interface ChildEntityUpdateOperation {
    void apply(ChildEntity childEntity);
}
```

## Financial Domain Example: Donation Campaign Aggregate

```java
package com.openshariaenterprise.donation.model;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * DonationCampaign aggregate represents a fundraising campaign with donations and beneficiaries.
 *
 * <p>Aggregate Root: DonationCampaign
 *
 * <p>Transactional Boundary:
 * <ul>
 *   <li>Root: DonationCampaign</li>
 *   <li>Child Entities: Donation, Beneficiary</li>
 *   <li>Value Objects: Money, DateRange, CampaignGoal</li>
 * </ul>
 *
 * <p>Invariants:
 * <ul>
 *   <li>Total donations must not exceed campaign goal</li>
 *   <li>Campaign must have at least one beneficiary</li>
 *   <li>Cannot accept donations before start date or after end date</li>
 *   <li>Cannot close campaign with pending donations</li>
 * </ul>
 */
public class DonationCampaign {

    private final CampaignId id;
    private String name;
    private String description;
    private CampaignGoal goal;
    private DateRange campaignPeriod;
    private CampaignStatus status;

    // Child entities
    private final List<Donation> donations;
    private final Map<DonationId, Donation> donationIndex;
    private final List<Beneficiary> beneficiaries;

    // Domain events
    private final List<DomainEvent> domainEvents;

    // Audit
    private final LocalDateTime createdAt;
    private final UserId createdBy;
    private LocalDateTime updatedAt;
    private UserId updatedBy;
    private int version;

    // Constructor
    private DonationCampaign(
        CampaignId id,
        String name,
        String description,
        CampaignGoal goal,
        DateRange campaignPeriod,
        List<Beneficiary> initialBeneficiaries,
        UserId createdBy
    ) {
        // Validate
        Objects.requireNonNull(id);
        validateName(name);
        Objects.requireNonNull(goal);
        Objects.requireNonNull(campaignPeriod);
        validateBeneficiaries(initialBeneficiaries);
        Objects.requireNonNull(createdBy);

        // Initialize root
        this.id = id;
        this.name = name;
        this.description = description;
        this.goal = goal;
        this.campaignPeriod = campaignPeriod;
        this.status = CampaignStatus.DRAFT;

        // Initialize children
        this.donations = new ArrayList<>();
        this.donationIndex = new HashMap<>();
        this.beneficiaries = new ArrayList<>(initialBeneficiaries);

        // Initialize events
        this.domainEvents = new ArrayList<>();

        // Initialize audit
        this.createdAt = LocalDateTime.now();
        this.createdBy = createdBy;
        this.updatedAt = this.createdAt;
        this.updatedBy = createdBy;
        this.version = 0;

        registerEvent(new CampaignCreated(id, name, goal, createdAt));
    }

    // Factory
    public static DonationCampaign create(
        CampaignId id,
        String name,
        String description,
        CampaignGoal goal,
        DateRange campaignPeriod,
        List<Beneficiary> initialBeneficiaries,
        UserId createdBy
    ) {
        return new DonationCampaign(id, name, description, goal, campaignPeriod, initialBeneficiaries, createdBy);
    }

    // Business Operations

    /**
     * Activates the campaign, making it available for donations.
     *
     * @param activatedBy user activating the campaign
     * @throws IllegalStateException if campaign cannot be activated
     */
    public void activate(UserId activatedBy) {
        if (status != CampaignStatus.DRAFT) {
            throw new IllegalStateException("Only draft campaigns can be activated");
        }

        if (!campaignPeriod.isCurrentlyActive()) {
            throw new IllegalStateException("Campaign period has not started");
        }

        this.status = CampaignStatus.ACTIVE;
        markUpdated(activatedBy);

        registerEvent(new CampaignActivated(id, LocalDateTime.now()));
    }

    /**
     * Records a donation to this campaign.
     *
     * @param donor the donor making the donation
     * @param amount donation amount
     * @param donatedBy user recording the donation
     * @return the created Donation entity
     * @throws IllegalStateException if campaign cannot accept donations
     */
    public Donation receiveDonation(Donor donor, Money amount, UserId donatedBy) {
        // Validate state
        if (status != CampaignStatus.ACTIVE) {
            throw new IllegalStateException("Campaign is not active");
        }

        if (!campaignPeriod.isCurrentlyActive()) {
            throw new IllegalStateException("Campaign period has ended");
        }

        // Validate amount
        Objects.requireNonNull(amount);
        if (amount.isNegativeOrZero()) {
            throw new IllegalArgumentException("Donation amount must be positive");
        }

        // Check if adding this donation would exceed goal
        var totalAfterDonation = calculateTotalDonations().add(amount);
        if (totalAfterDonation.isGreaterThan(goal.getTargetAmount())) {
            throw new IllegalStateException("Donation would exceed campaign goal");
        }

        // Create donation
        var donation = Donation.create(
            DonationId.generate(),
            donor,
            amount,
            LocalDateTime.now()
        );

        // Add to aggregate
        donations.add(donation);
        donationIndex.put(donation.getId(), donation);

        markUpdated(donatedBy);

        registerEvent(new DonationReceived(id, donation.getId(), amount, LocalDateTime.now()));

        // Check if goal reached
        if (isGoalReached()) {
            registerEvent(new CampaignGoalReached(id, goal.getTargetAmount(), LocalDateTime.now()));
        }

        return donation;
    }

    /**
     * Closes the campaign, preventing further donations.
     *
     * @param closedBy user closing the campaign
     * @throws IllegalStateException if campaign cannot be closed
     */
    public void close(UserId closedBy) {
        if (status != CampaignStatus.ACTIVE) {
            throw new IllegalStateException("Only active campaigns can be closed");
        }

        // Check for pending donations
        var hasPendingDonations = donations.stream()
            .anyMatch(d -> d.getStatus() == DonationStatus.PENDING);

        if (hasPendingDonations) {
            throw new IllegalStateException("Cannot close campaign with pending donations");
        }

        this.status = CampaignStatus.CLOSED;
        markUpdated(closedBy);

        registerEvent(new CampaignClosed(id, calculateTotalDonations(), LocalDateTime.now()));
    }

    /**
     * Adds a beneficiary to the campaign.
     *
     * @param beneficiary the beneficiary to add
     * @param addedBy user adding the beneficiary
     */
    public void addBeneficiary(Beneficiary beneficiary, UserId addedBy) {
        validateNotClosed();
        Objects.requireNonNull(beneficiary);

        if (beneficiaries.contains(beneficiary)) {
            throw new IllegalArgumentException("Beneficiary already exists");
        }

        beneficiaries.add(beneficiary);
        markUpdated(addedBy);

        registerEvent(new BeneficiaryAdded(id, beneficiary.getId(), LocalDateTime.now()));
    }

    // Calculations

    /**
     * Calculates total donations received.
     *
     * @return total donation amount
     */
    public Money calculateTotalDonations() {
        return donations.stream()
            .filter(d -> d.getStatus() == DonationStatus.COMPLETED)
            .map(Donation::getAmount)
            .reduce(Money.zero(goal.getTargetAmount().getCurrencyCode()), Money::add);
    }

    /**
     * Calculates fundraising progress as a percentage.
     *
     * @return progress percentage (0-100)
     */
    public BigDecimal calculateProgress() {
        var total = calculateTotalDonations();
        var target = goal.getTargetAmount();

        if (target.isZero()) {
            return BigDecimal.ZERO;
        }

        return total.getAmount()
            .divide(target.getAmount(), 4, java.math.RoundingMode.HALF_UP)
            .multiply(new BigDecimal("100"));
    }

    /**
     * Checks if campaign goal has been reached.
     *
     * @return true if total donations >= goal
     */
    public boolean isGoalReached() {
        return calculateTotalDonations().isGreaterThanOrEqualTo(goal.getTargetAmount());
    }

    // Validation

    private static void validateName(String name) {
        if (name == null || name.isBlank()) {
            throw new IllegalArgumentException("Campaign name must not be blank");
        }
    }

    private static void validateBeneficiaries(List<Beneficiary> beneficiaries) {
        if (beneficiaries == null || beneficiaries.isEmpty()) {
            throw new IllegalArgumentException("Campaign must have at least one beneficiary");
        }
    }

    private void validateNotClosed() {
        if (status == CampaignStatus.CLOSED) {
            throw new IllegalStateException("Campaign is closed");
        }
    }

    // Domain Events

    private void registerEvent(DomainEvent event) {
        domainEvents.add(event);
    }

    public List<DomainEvent> getAndClearDomainEvents() {
        var events = new ArrayList<>(domainEvents);
        domainEvents.clear();
        return events;
    }

    // State Modification

    private void markUpdated(UserId updatedBy) {
        this.updatedAt = LocalDateTime.now();
        this.updatedBy = updatedBy;
        this.version++;
    }

    // Getters

    public CampaignId getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public CampaignGoal getGoal() {
        return goal;
    }

    public CampaignStatus getStatus() {
        return status;
    }

    public List<Donation> getDonations() {
        return Collections.unmodifiableList(donations);
    }

    public List<Beneficiary> getBeneficiaries() {
        return Collections.unmodifiableList(beneficiaries);
    }

    public boolean isActive() {
        return status == CampaignStatus.ACTIVE;
    }

    // Equality

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DonationCampaign that = (DonationCampaign) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}

enum CampaignStatus {
    DRAFT,
    ACTIVE,
    CLOSED
}
```

## Usage Guidelines

1. **Single Root**: Only one entity serves as the aggregate root
2. **External References**: External objects should only hold references to the root's ID
3. **Transactional Boundary**: All changes within the aggregate are atomic
4. **Invariant Enforcement**: Root enforces all aggregate-wide invariants
5. **Child Access**: All modifications to child entities go through the root
6. **Size Limits**: Keep aggregates small; split into multiple aggregates if growing too large
7. **Domain Events**: Use events to communicate state changes to other aggregates
8. **Consistency**: Strong consistency within aggregate, eventual consistency between aggregates

## Related Templates

- [Entity Template](./ex-so-prla-ja-te__entity-template.md)
- [Value Object Template](./ex-so-prla-ja-te__value-object-template.md)
- [Domain Event Template](./ex-so-prla-ja-te__domain-event-template.md)

## See Also

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
