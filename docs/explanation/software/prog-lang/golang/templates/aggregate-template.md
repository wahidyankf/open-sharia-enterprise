---
title: Go Aggregate Template
description: Template for creating Domain-Driven Design aggregates in Go with transactional boundaries, consistency enforcement, and domain events
category: template
tags:
  - golang
  - ddd
  - domain-model
  - aggregate
  - aggregate-root
  - consistency
  - transactions
  - domain-events
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
related:
  - entity-template.md
  - value-object-template.md
  - ex-so-prla-go__best-practices.md
  - ex-so-prla-go__idioms.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
---

# Go Aggregate Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) aggregates in Go. Aggregates are clusters of entities and value objects with a defined transactional boundary, enforcing invariants and consistency across related domain objects.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Aggregate Root Pattern](#aggregate-root-pattern)
4. [Child Entity Management](#child-entity-management)
5. [Invariant Enforcement](#invariant-enforcement)
6. [Transactional Boundaries](#transactional-boundaries)
7. [Domain Events](#domain-events)
8. [Complete Example: Donation Campaign](#complete-example-donation-campaign)
9. [Before/After Comparison](#beforeafter-comparison)
10. [Usage Guidelines](#usage-guidelines)
11. [Testing Aggregates](#testing-aggregates)
12. [Related Documentation](#related-documentation)

## Overview

Aggregates in Go define **transactional consistency boundaries** around a cluster of entities and value objects. The aggregate root entity controls all access to child entities, ensuring invariants are maintained across the entire cluster.

**Key Characteristics**:

- **Single root entity**: One entity serves as the entry point for all operations
- **Transactional boundary**: All changes within aggregate are atomic
- **Invariant enforcement**: Root validates consistency rules across all children
- **External references**: Outside objects reference only the root's ID
- **Child entity management**: Slices of structs with pointer receivers
- **Domain events**: Slice of pending events published after persistence

**Go vs Java Differences**:

```go
// Go: Slices for child entities, unexported fields
type Campaign struct {
 id         CampaignID       // Unexported identity
 name       string
 donations  []Donation       // Slice of child entities
 eventQueue []DomainEvent    // Pending events
 version    int
}

// Factory function
func NewCampaign(id CampaignID, name string, createdBy UserID) (*Campaign, error) {
 // Returns pointer for mutability
}

// Business methods use pointer receivers
func (c *Campaign) ReceiveDonation(amount Money, donor Donor, recordedBy UserID) error {
 // Validates invariants, updates state, registers events
 c.donations = append(c.donations, donation)
 c.registerEvent(DonationReceived{...})
 c.markUpdated(recordedBy)
 return nil
}
```

```java
// Java: Lists for child entities, private fields
public class Campaign {
    private final CampaignId id; // final identity
    private String name;
    private List<Donation> donations; // List of children
    private List<DomainEvent> domainEvents; // Pending events
    private int version;

    // Private constructor
    private Campaign(CampaignId id, String name, UserId createdBy) {
        // ...
    }

    // Factory method
    public static Campaign create(CampaignId id, String name, UserId createdBy) {
        return new Campaign(id, name, createdBy);
    }

    // Business methods
    public void receiveDonation(Money amount, Donor donor, UserId recordedBy) {
        // Validates invariants, updates state, registers events
    }
}
```

**Critical Differences**:

- Go uses **slices** for child entity collections instead of Java Lists
- Go has **no `final` keyword** - identity immutability by convention
- Go uses **pointer receivers** for aggregate root methods
- Go uses **unexported fields** for encapsulation
- Go aggregates use **nil pointers** for optional fields (DeletedAt)
- Go uses **simple append** for event registration

## Template Structure

```go
package model

import (
 "errors"
 "fmt"
 "time"
)

// AggregateName represents [brief description of the aggregate].
//
// Aggregate Root: [RootEntityName]
//
// Transactional Boundary:
//   - Root Entity: [RootEntity]
//   - Child Entities: [ChildEntity1], [ChildEntity2]
//   - Value Objects: [ValueObject1], [ValueObject2]
//
// Invariants (Enforced across the aggregate):
//   - [Invariant 1 that spans multiple entities]
//   - [Invariant 2 that involves relationships]
//   - [Invariant 3 about aggregate-level consistency]
//
// Responsibilities:
//   - [Main business operation 1]
//   - [Main business operation 2]
//   - Enforce all aggregate-level invariants
//   - Coordinate state changes across child entities
//
// External Access Rules:
//   - All modifications must go through the aggregate root
//   - External references should only hold root's ID
//   - Child entities cannot be directly modified from outside
//
// Example:
//
// agg, err := NewAggregateName(id, "property", valueObject, createdBy)
// if err != nil {
//   return err
// }
// err = agg.AddChildEntity(child, addedBy)
// err = agg.PerformComplexOperation(params, performedBy)
type AggregateName struct {
 // ========================================
 // Aggregate Root Identity
 // ========================================

 id AggregateID

 // ========================================
 // Root Entity State
 // ========================================

 rootProperty   string
 rootValueObject ValueObject
 status         AggregateStatus

 // ========================================
 // Child Entities (Managed by Root)
 // ========================================

 childEntities []ChildEntity              // Slice of children
 childIndex    map[ChildEntityID]*ChildEntity // Fast lookup by ID

 // ========================================
 // Domain Events (Pending Publication)
 // ========================================

 eventQueue []DomainEvent

 // ========================================
 // Audit Fields
 // ========================================

 createdAt time.Time
 createdBy UserID
 updatedAt time.Time
 updatedBy UserID
 deletedAt *time.Time // nil = not deleted
 version   int        // Optimistic locking

 // ========================================
 // Aggregate-Level State
 // ========================================

 aggregateMetrics AggregateMetrics // Example: cached calculations
}

// ========================================
// Factory Functions (Constructors)
// ========================================

// NewAggregateName creates a new aggregate with validation.
//
// Returns error if any invariant is violated.
func NewAggregateName(
 id AggregateID,
 rootProperty string,
 rootValueObject ValueObject,
 createdBy UserID,
) (*AggregateName, error) {
 // Validate inputs
 if id == (AggregateID{}) {
  return nil, errors.New("aggregate ID required")
 }

 if err := validateRootProperty(rootProperty); err != nil {
  return nil, fmt.Errorf("invalid root property: %w", err)
 }

 if err := rootValueObject.Validate(); err != nil {
  return nil, fmt.Errorf("invalid root value object: %w", err)
 }

 if createdBy == (UserID{}) {
  return nil, errors.New("created by user ID required")
 }

 now := time.Now()

 agg := &AggregateName{
  // Identity
  id: id,

  // Root state
  rootProperty:    rootProperty,
  rootValueObject: rootValueObject,
  status:          StatusActive,

  // Child collections
  childEntities: []ChildEntity{},
  childIndex:    make(map[ChildEntityID]*ChildEntity),

  // Events
  eventQueue: []DomainEvent{},

  // Audit
  createdAt: now,
  createdBy: createdBy,
  updatedAt: now,
  updatedBy: createdBy,
  deletedAt: nil,
  version:   0,

  // Metrics
  aggregateMetrics: AggregateMetrics{},
 }

 // Register creation event
 agg.registerEvent(AggregateCreated{
  AggregateID: id,
  OccurredAt:  now,
  PerformedBy: createdBy,
 })

 return agg, nil
}

// ReconstituteAggregateName reconstitutes an aggregate from persistence.
//
// Bypasses validation for data already in database.
// Use only when loading from trusted persistence layer.
func ReconstituteAggregateName(
 id AggregateID,
 rootProperty string,
 rootValueObject ValueObject,
 status AggregateStatus,
 childEntities []ChildEntity,
 createdAt time.Time,
 createdBy UserID,
 updatedAt time.Time,
 updatedBy UserID,
 deletedAt *time.Time,
 version int,
) *AggregateName {
 // Build child index
 childIndex := make(map[ChildEntityID]*ChildEntity)
 for i := range childEntities {
  childIndex[childEntities[i].ID()] = &childEntities[i]
 }

 return &AggregateName{
  id:              id,
  rootProperty:    rootProperty,
  rootValueObject: rootValueObject,
  status:          status,
  childEntities:   childEntities,
  childIndex:      childIndex,
  eventQueue:      []DomainEvent{}, // Don't republish on reconstitution
  createdAt:       createdAt,
  createdBy:       createdBy,
  updatedAt:       updatedAt,
  updatedBy:       updatedBy,
  deletedAt:       deletedAt,
  version:         version,
  aggregateMetrics: calculateMetrics(childEntities),
 }
}

// ========================================
// Root-Level Business Operations
// ========================================

// UpdateRootProperty updates the root property with validation.
//
// Returns error if property is invalid or aggregate is not updatable.
func (a *AggregateName) UpdateRootProperty(newProperty string, updatedBy UserID) error {
 if err := a.validateUpdatable(); err != nil {
  return err
 }

 if err := validateRootProperty(newProperty); err != nil {
  return fmt.Errorf("invalid property: %w", err)
 }

 if updatedBy == (UserID{}) {
  return errors.New("updated by user ID required")
 }

 a.rootProperty = newProperty
 a.markUpdated(updatedBy)

 a.registerEvent(AggregateUpdated{
  AggregateID: a.id,
  OccurredAt:  time.Now(),
  PerformedBy: updatedBy,
 })

 return nil
}

// ChangeStatus changes aggregate status with state transition validation.
//
// Returns error if transition is not allowed.
func (a *AggregateName) ChangeStatus(newStatus AggregateStatus, updatedBy UserID) error {
 if updatedBy == (UserID{}) {
  return errors.New("updated by user ID required")
 }

 if !a.canTransitionTo(newStatus) {
  return fmt.Errorf("cannot transition from %s to %s", a.status, newStatus)
 }

 oldStatus := a.status
 a.status = newStatus
 a.markUpdated(updatedBy)

 a.registerEvent(AggregateStatusChanged{
  AggregateID: a.id,
  OldStatus:   oldStatus,
  NewStatus:   newStatus,
  OccurredAt:  time.Now(),
  PerformedBy: updatedBy,
 })

 return nil
}

// ========================================
// Child Entity Management (Through Root)
// ========================================

// AddChildEntity adds a child entity to the aggregate.
//
// Returns error if child is invalid or violates invariants.
func (a *AggregateName) AddChildEntity(child ChildEntity, addedBy UserID) error {
 if err := a.validateUpdatable(); err != nil {
  return err
 }

 if addedBy == (UserID{}) {
  return errors.New("added by user ID required")
 }

 // Check for duplicates
 if _, exists := a.childIndex[child.ID()]; exists {
  return fmt.Errorf("child entity already exists: %v", child.ID())
 }

 // Validate child
 if err := child.Validate(); err != nil {
  return fmt.Errorf("invalid child entity: %w", err)
 }

 // Validate aggregate-level invariants BEFORE adding
 if err := a.validateChildEntityAddition(child); err != nil {
  return err
 }

 // Add child
 a.childEntities = append(a.childEntities, child)
 a.childIndex[child.ID()] = &a.childEntities[len(a.childEntities)-1]

 // Recalculate metrics
 a.recalculateMetrics()

 a.markUpdated(addedBy)

 a.registerEvent(ChildEntityAdded{
  AggregateID:   a.id,
  ChildEntityID: child.ID(),
  OccurredAt:    time.Now(),
  PerformedBy:   addedBy,
 })

 return nil
}

// RemoveChildEntity removes a child entity from the aggregate.
//
// Returns error if child is not found or removal violates invariants.
func (a *AggregateName) RemoveChildEntity(childID ChildEntityID, removedBy UserID) error {
 if err := a.validateUpdatable(); err != nil {
  return err
 }

 if childID == (ChildEntityID{}) {
  return errors.New("child entity ID required")
 }

 if removedBy == (UserID{}) {
  return errors.New("removed by user ID required")
 }

 // Find child
 childPtr, exists := a.childIndex[childID]
 if !exists {
  return fmt.Errorf("child entity not found: %v", childID)
 }

 // Validate aggregate-level invariants BEFORE removing
 if err := a.validateChildEntityRemoval(*childPtr); err != nil {
  return err
 }

 // Remove from slice
 for i := range a.childEntities {
  if a.childEntities[i].ID() == childID {
   // Remove by replacing with last element and truncating
   a.childEntities[i] = a.childEntities[len(a.childEntities)-1]
   a.childEntities = a.childEntities[:len(a.childEntities)-1]
   break
  }
 }

 // Remove from index
 delete(a.childIndex, childID)

 // Recalculate metrics
 a.recalculateMetrics()

 a.markUpdated(removedBy)

 a.registerEvent(ChildEntityRemoved{
  AggregateID:   a.id,
  ChildEntityID: childID,
  OccurredAt:    time.Now(),
  PerformedBy:   removedBy,
 })

 return nil
}

// UpdateChildEntity updates a child entity through the aggregate root.
//
// Returns error if child is not found or update violates invariants.
func (a *AggregateName) UpdateChildEntity(
 childID ChildEntityID,
 updateFn func(*ChildEntity) error,
 updatedBy UserID,
) error {
 if err := a.validateUpdatable(); err != nil {
  return err
 }

 if childID == (ChildEntityID{}) {
  return errors.New("child entity ID required")
 }

 if updateFn == nil {
  return errors.New("update function required")
 }

 if updatedBy == (UserID{}) {
  return errors.New("updated by user ID required")
 }

 // Find child
 childPtr, exists := a.childIndex[childID]
 if !exists {
  return fmt.Errorf("child entity not found: %v", childID)
 }

 // Apply update
 if err := updateFn(childPtr); err != nil {
  return fmt.Errorf("update failed: %w", err)
 }

 // Validate aggregate-level invariants AFTER update
 if err := a.validateAggregateInvariants(); err != nil {
  return fmt.Errorf("invariant violation: %w", err)
 }

 // Recalculate metrics
 a.recalculateMetrics()

 a.markUpdated(updatedBy)

 a.registerEvent(ChildEntityUpdated{
  AggregateID:   a.id,
  ChildEntityID: childID,
  OccurredAt:    time.Now(),
  PerformedBy:   updatedBy,
 })

 return nil
}

// FindChildEntity finds a child entity by ID.
//
// Returns nil if not found.
func (a *AggregateName) FindChildEntity(childID ChildEntityID) *ChildEntity {
 return a.childIndex[childID]
}

// FindChildEntitiesBy finds child entities matching a predicate.
func (a *AggregateName) FindChildEntitiesBy(predicate func(ChildEntity) bool) []ChildEntity {
 var result []ChildEntity
 for _, child := range a.childEntities {
  if predicate(child) {
   result = append(result, child)
  }
 }
 return result
}

// ========================================
// Aggregate-Level Business Operations
// ========================================

// PerformComplexOperation performs a complex operation across multiple child entities.
//
// Returns result of the operation.
func (a *AggregateName) PerformComplexOperation(
 params OperationParams,
 performedBy UserID,
) (OperationResult, error) {
 if err := a.validateUpdatable(); err != nil {
  return OperationResult{}, err
 }

 if err := params.Validate(); err != nil {
  return OperationResult{}, fmt.Errorf("invalid params: %w", err)
 }

 if performedBy == (UserID{}) {
  return OperationResult{}, errors.New("performed by user ID required")
 }

 // Validate preconditions
 if !a.canPerformOperation(params) {
  return OperationResult{}, errors.New("cannot perform operation in current state")
 }

 // Find affected children
 affectedChildren := a.FindChildEntitiesBy(params.Filter)

 // Apply transformation to each affected child
 for i := range affectedChildren {
  if err := params.Transform(&affectedChildren[i]); err != nil {
   return OperationResult{}, fmt.Errorf("transform failed: %w", err)
  }
 }

 // Validate aggregate invariants after operation
 if err := a.validateAggregateInvariants(); err != nil {
  return OperationResult{}, fmt.Errorf("invariant violation: %w", err)
 }

 // Recalculate metrics
 a.recalculateMetrics()

 a.markUpdated(performedBy)

 result := OperationResult{
  AffectedCount: len(affectedChildren),
  CompletedAt:   time.Now(),
 }

 a.registerEvent(ComplexOperationPerformed{
  AggregateID: a.id,
  Result:      result,
  OccurredAt:  time.Now(),
  PerformedBy: performedBy,
 })

 return result, nil
}

// ========================================
// Validation & Invariants
// ========================================

func validateRootProperty(property string) error {
 if property == "" {
  return errors.New("must not be empty")
 }

 if len(property) < 3 || len(property) > 100 {
  return errors.New("must be between 3 and 100 characters")
 }

 return nil
}

func (a *AggregateName) validateUpdatable() error {
 if a.deletedAt != nil {
  return errors.New("aggregate is deleted")
 }

 if a.status != StatusActive {
  return fmt.Errorf("aggregate is not active: %s", a.status)
 }

 return nil
}

func (a *AggregateName) canTransitionTo(target AggregateStatus) bool {
 switch a.status {
 case StatusActive:
  return target == StatusSuspended || target == StatusCompleted
 case StatusSuspended:
  return target == StatusActive || target == StatusCompleted
 case StatusCompleted:
  return false // Terminal state
 default:
  return false
 }
}

func (a *AggregateName) validateChildEntityAddition(child ChildEntity) error {
 // Example: Validate maximum number of children
 if len(a.childEntities) >= 100 {
  return errors.New("cannot add more than 100 child entities")
 }

 // Example: Validate business rules
 if !child.IsValid() {
  return errors.New("child entity is not valid")
 }

 return nil
}

func (a *AggregateName) validateChildEntityRemoval(child ChildEntity) error {
 // Example: Validate minimum number of children
 if len(a.childEntities) <= 1 {
  return errors.New("cannot remove last child entity")
 }

 return nil
}

func (a *AggregateName) validateAggregateInvariants() error {
 // Example invariant: Total of child values must not exceed root limit
 totalValue := 0
 for _, child := range a.childEntities {
  totalValue += child.Value()
 }

 if totalValue > a.rootValueObject.MaxLimit() {
  return errors.New("total child values exceed root limit")
 }

 // Example invariant: At least one active child required
 hasActiveChild := false
 for _, child := range a.childEntities {
  if child.IsActive() {
   hasActiveChild = true
   break
  }
 }

 if !hasActiveChild {
  return errors.New("at least one active child entity is required")
 }

 return nil
}

func (a *AggregateName) canPerformOperation(params OperationParams) bool {
 return a.status == StatusActive &&
  len(a.childEntities) > 0 &&
  params.IsValid()
}

// ========================================
// Domain Events
// ========================================

func (a *AggregateName) registerEvent(event DomainEvent) {
 a.eventQueue = append(a.eventQueue, event)
}

// GetPendingEvents returns all uncommitted domain events.
//
// Events should be published AFTER aggregate is successfully persisted.
func (a *AggregateName) GetPendingEvents() []DomainEvent {
 // Defensive copy
 events := make([]DomainEvent, len(a.eventQueue))
 copy(events, a.eventQueue)
 return events
}

// ClearPendingEvents clears all uncommitted domain events.
//
// Should be called AFTER events are successfully published.
func (a *AggregateName) ClearPendingEvents() {
 a.eventQueue = []DomainEvent{}
}

// ========================================
// State Modification Helpers
// ========================================

func (a *AggregateName) markUpdated(updatedBy UserID) {
 a.updatedAt = time.Now()
 a.updatedBy = updatedBy
 a.version++
}

func (a *AggregateName) recalculateMetrics() {
 a.aggregateMetrics = calculateMetrics(a.childEntities)
}

func calculateMetrics(children []ChildEntity) AggregateMetrics {
 totalValue := 0
 activeCount := 0

 for _, child := range children {
  totalValue += child.Value()
  if child.IsActive() {
   activeCount++
  }
 }

 return AggregateMetrics{
  TotalValue:  totalValue,
  ActiveCount: activeCount,
  TotalCount:  len(children),
 }
}

// ========================================
// Query Methods (Getters)
// ========================================

// ID returns the aggregate's unique identifier.
func (a *AggregateName) ID() AggregateID {
 return a.id
}

// RootProperty returns the root property value.
func (a *AggregateName) RootProperty() string {
 return a.rootProperty
}

// RootValueObject returns the root value object.
func (a *AggregateName) RootValueObject() ValueObject {
 return a.rootValueObject
}

// Status returns the aggregate's current status.
func (a *AggregateName) Status() AggregateStatus {
 return a.status
}

// ChildEntities returns a copy of all child entities.
func (a *AggregateName) ChildEntities() []ChildEntity {
 // Defensive copy
 result := make([]ChildEntity, len(a.childEntities))
 copy(result, a.childEntities)
 return result
}

// ChildEntityCount returns the number of child entities.
func (a *AggregateName) ChildEntityCount() int {
 return len(a.childEntities)
}

// HasChildEntity checks if a child entity exists.
func (a *AggregateName) HasChildEntity(childID ChildEntityID) bool {
 _, exists := a.childIndex[childID]
 return exists
}

// CreatedAt returns when the aggregate was created.
func (a *AggregateName) CreatedAt() time.Time {
 return a.createdAt
}

// CreatedBy returns who created the aggregate.
func (a *AggregateName) CreatedBy() UserID {
 return a.createdBy
}

// UpdatedAt returns when the aggregate was last updated.
func (a *AggregateName) UpdatedAt() time.Time {
 return a.updatedAt
}

// UpdatedBy returns who last updated the aggregate.
func (a *AggregateName) UpdatedBy() UserID {
 return a.updatedBy
}

// DeletedAt returns when the aggregate was deleted (nil if not deleted).
func (a *AggregateName) DeletedAt() *time.Time {
 return a.deletedAt
}

// Version returns the aggregate's version for optimistic locking.
func (a *AggregateName) Version() int {
 return a.version
}

// Metrics returns calculated aggregate metrics.
func (a *AggregateName) Metrics() AggregateMetrics {
 return a.aggregateMetrics
}

// IsActive returns true if the aggregate is active.
func (a *AggregateName) IsActive() bool {
 return a.status == StatusActive && a.deletedAt == nil
}

// IsDeleted returns true if the aggregate is soft-deleted.
func (a *AggregateName) IsDeleted() bool {
 return a.deletedAt != nil
}

// ========================================
// Equality (Identity-Based)
// ========================================

// Equals compares this aggregate with another for identity equality.
//
// Aggregates are equal if their IDs match (state doesn't matter).
func (a *AggregateName) Equals(other *AggregateName) bool {
 if a == nil || other == nil {
  return a == other
 }
 return a.id == other.id
}

// ========================================
// String Representation
// ========================================

// String returns a string representation of the aggregate.
func (a *AggregateName) String() string {
 return fmt.Sprintf("AggregateName{id=%v, status=%s, childCount=%d, version=%d, deleted=%t}",
  a.id, a.status, len(a.childEntities), a.version, a.deletedAt != nil)
}

// ========================================
// Supporting Types
// ========================================

// AggregateStatus represents the aggregate's lifecycle status.
type AggregateStatus string

const (
 StatusActive    AggregateStatus = "ACTIVE"
 StatusSuspended AggregateStatus = "SUSPENDED"
 StatusCompleted AggregateStatus = "COMPLETED"
)

func (s AggregateStatus) String() string {
 return string(s)
}

// AggregateMetrics holds calculated aggregate-level metrics.
type AggregateMetrics struct {
 TotalValue  int
 ActiveCount int
 TotalCount  int
}

// OperationParams holds parameters for complex operations.
type OperationParams struct {
 Filter    func(ChildEntity) bool
 Transform func(*ChildEntity) error
}

func (p OperationParams) Validate() error {
 if p.Filter == nil {
  return errors.New("filter function required")
 }
 if p.Transform == nil {
  return errors.New("transform function required")
 }
 return nil
}

func (p OperationParams) IsValid() bool {
 return p.Filter != nil && p.Transform != nil
}

// OperationResult holds the result of a complex operation.
type OperationResult struct {
 AffectedCount int
 CompletedAt   time.Time
}

// DomainEvent represents a domain event that occurred on the aggregate.
type DomainEvent interface {
 AggregateID() AggregateID
 OccurredAt() time.Time
 EventType() string
}

// AggregateCreated event
type AggregateCreated struct {
 AggregateID AggregateID
 OccurredAt  time.Time
 PerformedBy UserID
}

func (e AggregateCreated) EventType() string { return "AggregateCreated" }

// AggregateUpdated event
type AggregateUpdated struct {
 AggregateID AggregateID
 OccurredAt  time.Time
 PerformedBy UserID
}

func (e AggregateUpdated) EventType() string { return "AggregateUpdated" }

// AggregateStatusChanged event
type AggregateStatusChanged struct {
 AggregateID AggregateID
 OldStatus   AggregateStatus
 NewStatus   AggregateStatus
 OccurredAt  time.Time
 PerformedBy UserID
}

func (e AggregateStatusChanged) EventType() string { return "AggregateStatusChanged" }

// ChildEntityAdded event
type ChildEntityAdded struct {
 AggregateID   AggregateID
 ChildEntityID ChildEntityID
 OccurredAt    time.Time
 PerformedBy   UserID
}

func (e ChildEntityAdded) EventType() string { return "ChildEntityAdded" }

// ChildEntityRemoved event
type ChildEntityRemoved struct {
 AggregateID   AggregateID
 ChildEntityID ChildEntityID
 OccurredAt    time.Time
 PerformedBy   UserID
}

func (e ChildEntityRemoved) EventType() string { return "ChildEntityRemoved" }

// ChildEntityUpdated event
type ChildEntityUpdated struct {
 AggregateID   AggregateID
 ChildEntityID ChildEntityID
 OccurredAt    time.Time
 PerformedBy   UserID
}

func (e ChildEntityUpdated) EventType() string { return "ChildEntityUpdated" }

// ComplexOperationPerformed event
type ComplexOperationPerformed struct {
 AggregateID AggregateID
 Result      OperationResult
 OccurredAt  time.Time
 PerformedBy UserID
}

func (e ComplexOperationPerformed) EventType() string { return "ComplexOperationPerformed" }
```

## Aggregate Root Pattern

The **aggregate root** is the only entry point for accessing and modifying entities within the aggregate boundary.

### Root Entity Characteristics

```go
// CORRECT: Root entity with identity, child management, invariant enforcement
type Campaign struct {
 // Identity (never changes)
 id CampaignID

 // Root state
 name        string
 description string
 goal        CampaignGoal

 // Child entities managed by root
 donations     []Donation              // Slice of children
 donationIndex map[DonationID]*Donation // Fast lookup

 // Audit
 createdAt time.Time
 version   int
}

// Factory function enforces invariants at creation
func NewCampaign(id CampaignID, name string, goal CampaignGoal, createdBy UserID) (*Campaign, error) {
 if id == (CampaignID{}) {
  return nil, errors.New("campaign ID required")
 }

 if name == "" {
  return nil, errors.New("name required")
 }

 if err := goal.Validate(); err != nil {
  return nil, fmt.Errorf("invalid goal: %w", err)
 }

 now := time.Now()

 return &Campaign{
  id:            id,
  name:          name,
  goal:          goal,
  donations:     []Donation{},
  donationIndex: make(map[DonationID]*Donation),
  createdAt:     now,
  version:       0,
 }, nil
}

// All modifications go through root methods
func (c *Campaign) ReceiveDonation(amount Money, donor Donor, recordedBy UserID) (*Donation, error) {
 // Validate aggregate-level invariants
 if !c.canAcceptDonations() {
  return nil, errors.New("campaign cannot accept donations")
 }

 // Check donation won't exceed goal
 totalAfter, err := c.TotalDonations().Add(amount)
 if err != nil {
  return nil, err
 }

 exceedsGoal, _ := totalAfter.IsGreaterThan(c.goal.TargetAmount())
 if exceedsGoal {
  return nil, errors.New("donation would exceed campaign goal")
 }

 // Create child entity
 donation, err := NewDonation(DonationID{}, donor, amount, time.Now())
 if err != nil {
  return nil, err
 }

 // Add to aggregate
 c.donations = append(c.donations, *donation)
 c.donationIndex[donation.ID()] = &c.donations[len(c.donations)-1]

 c.markUpdated(recordedBy)

 return donation, nil
}

// INCORRECT: Exposing mutable child collection (breaks encapsulation)
func (c *Campaign) Donations() []Donation {
 return c.donations // Caller can modify!
}

// CORRECT: Return defensive copy
func (c *Campaign) Donations() []Donation {
 result := make([]Donation, len(c.donations))
 copy(result, c.donations)
 return result
}
```

### Identity Immutability

```go
// CORRECT: ID field is unexported and has no setter
type Campaign struct {
 id CampaignID // Cannot be changed externally
 // ...
}

// Getter provides read-only access
func (c *Campaign) ID() CampaignID {
 return c.id
}

// INCORRECT: Exported ID field (can be modified)
type MutableCampaign struct {
 ID CampaignID // Can be changed: campaign.ID = newID
}

// INCORRECT: ID setter method (breaks identity)
func (c *Campaign) SetID(id CampaignID) {
 c.id = id // Identity should never change!
}
```

## Child Entity Management

Child entities are managed through **slices** in Go, with a **map index** for fast lookups.

### Child Entity Collection Pattern

```go
type Campaign struct {
 donations     []Donation              // Slice maintains order
 donationIndex map[DonationID]*Donation // Map enables O(1) lookup
}

// Add: Append to slice, update index
func (c *Campaign) ReceiveDonation(amount Money, donor Donor, recordedBy UserID) error {
 donation, err := NewDonation(DonationID{}, donor, amount, time.Now())
 if err != nil {
  return err
 }

 // Add to slice
 c.donations = append(c.donations, *donation)

 // Update index with pointer to last element
 c.donationIndex[donation.ID()] = &c.donations[len(c.donations)-1]

 c.markUpdated(recordedBy)

 return nil
}

// Remove: Delete from index, remove from slice
func (c *Campaign) CancelDonation(donationID DonationID, cancelledBy UserID) error {
 // Check existence
 if _, exists := c.donationIndex[donationID]; !exists {
  return fmt.Errorf("donation not found: %v", donationID)
 }

 // Remove from index
 delete(c.donationIndex, donationID)

 // Remove from slice (unordered removal for efficiency)
 for i := range c.donations {
  if c.donations[i].ID() == donationID {
   // Replace with last element and truncate
   c.donations[i] = c.donations[len(c.donations)-1]
   c.donations = c.donations[:len(c.donations)-1]

   // Update index for moved element
   if i < len(c.donations) {
    c.donationIndex[c.donations[i].ID()] = &c.donations[i]
   }
   break
  }
 }

 c.markUpdated(cancelledBy)

 return nil
}

// Find: Use index for O(1) lookup
func (c *Campaign) FindDonation(donationID DonationID) *Donation {
 return c.donationIndex[donationID]
}

// Query: Filter children by predicate
func (c *Campaign) FindDonationsBy(predicate func(Donation) bool) []Donation {
 var result []Donation
 for _, donation := range c.donations {
  if predicate(donation) {
   result = append(result, donation)
  }
 }
 return result
}
```

### Update Through Root Pattern

```go
// CORRECT: Update child through aggregate root
func (c *Campaign) ConfirmDonation(donationID DonationID, confirmedBy UserID) error {
 // Find child
 donation := c.donationIndex[donationID]
 if donation == nil {
  return fmt.Errorf("donation not found: %v", donationID)
 }

 // Update child state
 if err := donation.Confirm(); err != nil {
  return err
 }

 // Validate aggregate invariants after update
 if err := c.validateInvariants(); err != nil {
  return err
 }

 c.markUpdated(confirmedBy)

 return nil
}

// INCORRECT: Direct child modification (bypasses invariants)
donation := campaign.FindDonation(id)
donation.Confirm() // Aggregate doesn't know about change!

// INCORRECT: Exposing child for modification
func (c *Campaign) GetDonationForUpdate(id DonationID) *Donation {
 return c.donationIndex[id] // Caller can modify without root validation!
}
```

### Defensive Copying

```go
// CORRECT: Return defensive copy of child collection
func (c *Campaign) Donations() []Donation {
 result := make([]Donation, len(c.donations))
 copy(result, c.donations)
 return result
}

// INCORRECT: Return reference to internal slice
func (c *Campaign) DonationsUnsafe() []Donation {
 return c.donations // Caller can append/modify!
}

// CORRECT: Return individual child by value
func (c *Campaign) GetDonation(id DonationID) (Donation, bool) {
 donation := c.donationIndex[id]
 if donation == nil {
  return Donation{}, false
 }
 return *donation, true // Return copy, not pointer
}

// INCORRECT: Return pointer to child (allows external mutation)
func (c *Campaign) GetDonationPtr(id DonationID) *Donation {
 return c.donationIndex[id] // Breaks encapsulation!
}
```

## Invariant Enforcement

Aggregates enforce **consistency rules** that span multiple entities within the boundary.

### Aggregate-Level Invariants

```go
type Campaign struct {
 id          CampaignID
 goal        CampaignGoal
 donations   []Donation
 status      CampaignStatus
 startDate   time.Time
 endDate     time.Time
}

// Invariant 1: Total donations must not exceed goal
func (c *Campaign) validateTotalDonations() error {
 total := c.calculateTotalDonations()

 exceedsGoal, _ := total.IsGreaterThan(c.goal.TargetAmount())
 if exceedsGoal {
  return errors.New("total donations exceed campaign goal")
 }

 return nil
}

// Invariant 2: Campaign must have at least one beneficiary
func (c *Campaign) validateBeneficiaries() error {
 if len(c.beneficiaries) == 0 {
  return errors.New("campaign must have at least one beneficiary")
 }

 return nil
}

// Invariant 3: Cannot accept donations outside campaign period
func (c *Campaign) validateDonationPeriod() error {
 now := time.Now()

 if now.Before(c.startDate) {
  return errors.New("campaign has not started")
 }

 if now.After(c.endDate) {
  return errors.New("campaign has ended")
 }

 return nil
}

// Invariant 4: All donations must be in campaign currency
func (c *Campaign) validateDonationCurrency(donation Donation) error {
 goalCurrency := c.goal.TargetAmount().Currency()
 donationCurrency := donation.Amount().Currency()

 if goalCurrency != donationCurrency {
  return fmt.Errorf("donation currency %s does not match campaign currency %s",
   donationCurrency, goalCurrency)
 }

 return nil
}

// Validate all invariants
func (c *Campaign) validateInvariants() error {
 if err := c.validateTotalDonations(); err != nil {
  return err
 }

 if err := c.validateBeneficiaries(); err != nil {
  return err
 }

 return nil
}
```

### Validation Timing

```go
// BEFORE adding child entity
func (c *Campaign) ReceiveDonation(amount Money, donor Donor, recordedBy UserID) error {
 // Validate BEFORE adding
 if err := c.validateDonationPeriod(); err != nil {
  return err // Reject before state change
 }

 donation, err := NewDonation(DonationID{}, donor, amount, time.Now())
 if err != nil {
  return err
 }

 // Validate currency BEFORE adding
 if err := c.validateDonationCurrency(*donation); err != nil {
  return err
 }

 // Check if adding would violate total limit
 totalAfter, _ := c.calculateTotalDonations().Add(amount)
 if totalAfter.IsGreaterThan(c.goal.TargetAmount()) {
  return errors.New("donation would exceed goal")
 }

 // NOW safe to add
 c.donations = append(c.donations, *donation)
 c.donationIndex[donation.ID()] = &c.donations[len(c.donations)-1]

 return nil
}

// AFTER updating child entity
func (c *Campaign) UpdateDonationAmount(donationID DonationID, newAmount Money, updatedBy UserID) error {
 donation := c.donationIndex[donationID]
 if donation == nil {
  return errors.New("donation not found")
 }

 // Update child
 oldAmount := donation.Amount()
 donation.amount = newAmount

 // Validate AFTER update
 if err := c.validateInvariants(); err != nil {
  // Rollback on invariant violation
  donation.amount = oldAmount
  return err
 }

 c.markUpdated(updatedBy)

 return nil
}
```

## Transactional Boundaries

Aggregates define **transactional consistency boundaries**. All changes within an aggregate should be committed atomically.

### One Aggregate Per Transaction

```go
// CORRECT: Load and save single aggregate
func (s *CampaignService) ReceiveDonation(
 ctx context.Context,
 campaignID CampaignID,
 amount Money,
 donor Donor,
 recordedBy UserID,
) error {
 // Start transaction
 tx, err := s.db.BeginTx(ctx, nil)
 if err != nil {
  return err
 }
 defer tx.Rollback()

 // Load aggregate
 campaign, err := s.repo.FindByID(ctx, tx, campaignID)
 if err != nil {
  return err
 }

 // Perform business operation
 donation, err := campaign.ReceiveDonation(amount, donor, recordedBy)
 if err != nil {
  return err
 }

 // Save aggregate (entire cluster)
 if err := s.repo.Save(ctx, tx, campaign); err != nil {
  return err
 }

 // Publish domain events AFTER persistence
 events := campaign.GetPendingEvents()
 if err := s.eventPublisher.Publish(ctx, events); err != nil {
  return err
 }

 campaign.ClearPendingEvents()

 // Commit transaction
 return tx.Commit()
}

// INCORRECT: Modify multiple aggregates in same transaction (tight coupling)
func (s *CampaignService) TransferDonation(
 ctx context.Context,
 fromCampaignID CampaignID,
 toCampaignID CampaignID,
 amount Money,
) error {
 tx, _ := s.db.BeginTx(ctx, nil)
 defer tx.Rollback()

 // DON'T: Modify two aggregates in one transaction
 fromCampaign, _ := s.repo.FindByID(ctx, tx, fromCampaignID)
 toCampaign, _ := s.repo.FindByID(ctx, tx, toCampaignID)

 fromCampaign.WithdrawDonation(amount) // Modifies aggregate 1
 toCampaign.ReceiveDonation(amount)    // Modifies aggregate 2

 // This creates tight coupling between aggregates!
 s.repo.Save(ctx, tx, fromCampaign)
 s.repo.Save(ctx, tx, toCampaign)

 return tx.Commit()
}

// CORRECT: Use domain events for cross-aggregate coordination
func (s *CampaignService) TransferDonation(
 ctx context.Context,
 fromCampaignID CampaignID,
 toCampaignID CampaignID,
 amount Money,
) error {
 // Transaction 1: Withdraw from source
 {
  tx, _ := s.db.BeginTx(ctx, nil)
  defer tx.Rollback()

  fromCampaign, _ := s.repo.FindByID(ctx, tx, fromCampaignID)
  fromCampaign.WithdrawDonation(amount)
  s.repo.Save(ctx, tx, fromCampaign)

  // Publish DonationWithdrawn event
  events := fromCampaign.GetPendingEvents()
  s.eventPublisher.Publish(ctx, events)

  tx.Commit()
 }

 // Event handler will trigger second transaction for destination campaign
 // This is eventual consistency between aggregates

 return nil
}
```

### Repository Pattern Integration

```go
// Repository saves/loads entire aggregate atomically
type CampaignRepository struct {
 db *sql.DB
}

// Save persists aggregate root and all children in single transaction
func (r *CampaignRepository) Save(ctx context.Context, tx *sql.Tx, campaign *Campaign) error {
 // Save root entity
 _, err := tx.ExecContext(ctx, `
  INSERT INTO campaigns (id, name, goal, status, version)
  VALUES (?, ?, ?, ?, ?)
  ON DUPLICATE KEY UPDATE
   name = VALUES(name),
   goal = VALUES(goal),
   status = VALUES(status),
   version = VALUES(version)
 `, campaign.ID().Value(), campaign.Name(), campaign.Goal().Value(), campaign.Status(), campaign.Version())

 if err != nil {
  return err
 }

 // Delete existing children (for simplicity, could use upsert)
 _, err = tx.ExecContext(ctx, "DELETE FROM donations WHERE campaign_id = ?", campaign.ID().Value())
 if err != nil {
  return err
 }

 // Insert all children
 for _, donation := range campaign.Donations() {
  _, err = tx.ExecContext(ctx, `
   INSERT INTO donations (id, campaign_id, donor_id, amount, status)
   VALUES (?, ?, ?, ?, ?)
  `, donation.ID().Value(), campaign.ID().Value(), donation.DonorID().Value(), donation.Amount().Amount(), donation.Status())

  if err != nil {
   return err
  }
 }

 return nil
}

// FindByID loads aggregate root and all children
func (r *CampaignRepository) FindByID(ctx context.Context, tx *sql.Tx, id CampaignID) (*Campaign, error) {
 // Load root
 var name, goal, status string
 var version int

 err := tx.QueryRowContext(ctx,
  "SELECT name, goal, status, version FROM campaigns WHERE id = ?",
  id.Value(),
 ).Scan(&name, &goal, &status, &version)

 if err != nil {
  return nil, err
 }

 // Load children
 rows, err := tx.QueryContext(ctx,
  "SELECT id, donor_id, amount, status FROM donations WHERE campaign_id = ?",
  id.Value(),
 )
 if err != nil {
  return nil, err
 }
 defer rows.Close()

 var donations []Donation
 for rows.Next() {
  var donationID, donorID, amount, donationStatus string
  if err := rows.Scan(&donationID, &donorID, &amount, &donationStatus); err != nil {
   return nil, err
  }

  donation := ReconstituteDonation(...)
  donations = append(donations, donation)
 }

 // Reconstitute aggregate
 return ReconstituteCampaign(id, name, goal, status, donations, version), nil
}
```

## Domain Events

Aggregates register **domain events** when important state changes occur. Events are published AFTER the aggregate is successfully persisted.

### Event Registration Pattern

```go
type Campaign struct {
 id         CampaignID
 eventQueue []DomainEvent // Pending events
}

// Register event during business operation
func (c *Campaign) ReceiveDonation(amount Money, donor Donor, recordedBy UserID) error {
 // Perform operation
 donation, err := NewDonation(DonationID{}, donor, amount, time.Now())
 if err != nil {
  return err
 }

 c.donations = append(c.donations, *donation)
 c.donationIndex[donation.ID()] = &c.donations[len(c.donations)-1]

 // Register event
 c.registerEvent(DonationReceived{
  CampaignID:  c.id,
  DonationID:  donation.ID(),
  Amount:      amount,
  Donor:       donor,
  OccurredAt:  time.Now(),
  RecordedBy:  recordedBy,
 })

 // Check if goal reached
 if c.isGoalReached() {
  c.registerEvent(CampaignGoalReached{
   CampaignID:   c.id,
   TotalRaised:  c.calculateTotalDonations(),
   TargetAmount: c.goal.TargetAmount(),
   OccurredAt:   time.Now(),
  })
 }

 return nil
}

// Register event to queue
func (c *Campaign) registerEvent(event DomainEvent) {
 c.eventQueue = append(c.eventQueue, event)
}

// Get pending events (called AFTER persistence)
func (c *Campaign) GetPendingEvents() []DomainEvent {
 // Defensive copy
 events := make([]DomainEvent, len(c.eventQueue))
 copy(events, c.eventQueue)
 return events
}

// Clear events after publishing
func (c *Campaign) ClearPendingEvents() {
 c.eventQueue = []DomainEvent{}
}
```

### Domain Event Types

```go
// DomainEvent interface
type DomainEvent interface {
 AggregateID() interface{}
 OccurredAt() time.Time
 EventType() string
}

// CampaignCreated event
type CampaignCreated struct {
 CampaignID  CampaignID
 Name        string
 Goal        Money
 OccurredAt  time.Time
 CreatedBy   UserID
}

func (e CampaignCreated) AggregateID() interface{} { return e.CampaignID }
func (e CampaignCreated) OccurredAt() time.Time    { return e.OccurredAt }
func (e CampaignCreated) EventType() string        { return "CampaignCreated" }

// DonationReceived event
type DonationReceived struct {
 CampaignID CampaignID
 DonationID DonationID
 Amount     Money
 Donor      Donor
 OccurredAt time.Time
 RecordedBy UserID
}

func (e DonationReceived) AggregateID() interface{} { return e.CampaignID }
func (e DonationReceived) OccurredAt() time.Time    { return e.OccurredAt }
func (e DonationReceived) EventType() string        { return "DonationReceived" }

// CampaignGoalReached event
type CampaignGoalReached struct {
 CampaignID   CampaignID
 TotalRaised  Money
 TargetAmount Money
 OccurredAt   time.Time
}

func (e CampaignGoalReached) AggregateID() interface{} { return e.CampaignID }
func (e CampaignGoalReached) OccurredAt() time.Time    { return e.OccurredAt }
func (e CampaignGoalReached) EventType() string        { return "CampaignGoalReached" }

// CampaignClosed event
type CampaignClosed struct {
 CampaignID  CampaignID
 FinalAmount Money
 OccurredAt  time.Time
 ClosedBy    UserID
}

func (e CampaignClosed) AggregateID() interface{} { return e.CampaignID }
func (e CampaignClosed) OccurredAt() time.Time    { return e.OccurredAt }
func (e CampaignClosed) EventType() string        { return "CampaignClosed" }
```

### Event Publishing Pattern

```go
// Application service coordinates persistence and event publishing
type CampaignService struct {
 repo           CampaignRepository
 eventPublisher EventPublisher
 db             *sql.DB
}

func (s *CampaignService) ReceiveDonation(
 ctx context.Context,
 campaignID CampaignID,
 amount Money,
 donor Donor,
 recordedBy UserID,
) error {
 // Start transaction
 tx, err := s.db.BeginTx(ctx, nil)
 if err != nil {
  return err
 }
 defer tx.Rollback()

 // Load aggregate
 campaign, err := s.repo.FindByID(ctx, tx, campaignID)
 if err != nil {
  return err
 }

 // Perform business operation (registers events)
 if err := campaign.ReceiveDonation(amount, donor, recordedBy); err != nil {
  return err
 }

 // Save aggregate
 if err := s.repo.Save(ctx, tx, campaign); err != nil {
  return err
 }

 // Commit transaction BEFORE publishing events
 if err := tx.Commit(); err != nil {
  return err
 }

 // Publish events AFTER successful persistence
 events := campaign.GetPendingEvents()
 if err := s.eventPublisher.PublishBatch(ctx, events); err != nil {
  // Log error but don't fail - events can be republished
  log.Printf("failed to publish events: %v", err)
 }

 // Clear events after publishing
 campaign.ClearPendingEvents()

 return nil
}

// EventPublisher interface
type EventPublisher interface {
 Publish(ctx context.Context, event DomainEvent) error
 PublishBatch(ctx context.Context, events []DomainEvent) error
}
```

## Complete Example: Donation Campaign

Complete implementation of a donation campaign aggregate with donors, donations, and goals:

```go
package model

import (
 "errors"
 "fmt"
 "time"
)

// DonationCampaign aggregate represents a fundraising campaign with donations and beneficiaries.
//
// Aggregate Root: DonationCampaign
//
// Transactional Boundary:
//   - Root: DonationCampaign
//   - Child Entities: Donation, Beneficiary
//   - Value Objects: Money, CampaignGoal, DateRange
//
// Invariants:
//   - Total donations must not exceed campaign goal
//   - Campaign must have at least one beneficiary
//   - Cannot accept donations before start date or after end date
//   - Cannot close campaign with pending donations
//   - All donations must be in campaign currency
//
// Responsibilities:
//   - Track donations from multiple donors
//   - Enforce donation limits and campaign period
//   - Calculate fundraising progress
//   - Manage campaign lifecycle (draft -> active -> closed)
//
// External Access Rules:
//   - All donations must go through ReceiveDonation method
//   - External systems reference campaign by ID only
//   - Beneficiaries can only be added through AddBeneficiary method
type DonationCampaign struct {
 // ========================================
 // Identity
 // ========================================

 id CampaignID

 // ========================================
 // Root State
 // ========================================

 name        string
 description string
 goal        CampaignGoal
 period      DateRange
 status      CampaignStatus

 // ========================================
 // Child Entities
 // ========================================

 donations     []Donation
 donationIndex map[DonationID]*Donation
 beneficiaries []Beneficiary

 // ========================================
 // Domain Events
 // ========================================

 eventQueue []DomainEvent

 // ========================================
 // Audit
 // ========================================

 createdAt time.Time
 createdBy UserID
 updatedAt time.Time
 updatedBy UserID
 version   int
}

// ========================================
// Factory Functions
// ========================================

// NewDonationCampaign creates a new donation campaign with validation.
func NewDonationCampaign(
 id CampaignID,
 name string,
 description string,
 goal CampaignGoal,
 period DateRange,
 initialBeneficiaries []Beneficiary,
 createdBy UserID,
) (*DonationCampaign, error) {
 // Validate inputs
 if id == (CampaignID{}) {
  return nil, errors.New("campaign ID required")
 }

 if err := validateCampaignName(name); err != nil {
  return nil, fmt.Errorf("invalid name: %w", err)
 }

 if err := goal.Validate(); err != nil {
  return nil, fmt.Errorf("invalid goal: %w", err)
 }

 if err := period.Validate(); err != nil {
  return nil, fmt.Errorf("invalid period: %w", err)
 }

 if err := validateBeneficiaries(initialBeneficiaries); err != nil {
  return nil, fmt.Errorf("invalid beneficiaries: %w", err)
 }

 if createdBy == (UserID{}) {
  return nil, errors.New("created by user ID required")
 }

 now := time.Now()

 campaign := &DonationCampaign{
  id:            id,
  name:          name,
  description:   description,
  goal:          goal,
  period:        period,
  status:        StatusDraft,
  donations:     []Donation{},
  donationIndex: make(map[DonationID]*Donation),
  beneficiaries: append([]Beneficiary{}, initialBeneficiaries...),
  eventQueue:    []DomainEvent{},
  createdAt:     now,
  createdBy:     createdBy,
  updatedAt:     now,
  updatedBy:     createdBy,
  version:       0,
 }

 // Register creation event
 campaign.registerEvent(CampaignCreated{
  CampaignID: id,
  Name:       name,
  Goal:       goal.TargetAmount(),
  OccurredAt: now,
  CreatedBy:  createdBy,
 })

 return campaign, nil
}

// ReconstituteDonationCampaign reconstitutes from persistence.
func ReconstituteDonationCampaign(
 id CampaignID,
 name string,
 description string,
 goal CampaignGoal,
 period DateRange,
 status CampaignStatus,
 donations []Donation,
 beneficiaries []Beneficiary,
 createdAt time.Time,
 createdBy UserID,
 updatedAt time.Time,
 updatedBy UserID,
 version int,
) *DonationCampaign {
 // Build donation index
 donationIndex := make(map[DonationID]*Donation)
 for i := range donations {
  donationIndex[donations[i].ID()] = &donations[i]
 }

 return &DonationCampaign{
  id:            id,
  name:          name,
  description:   description,
  goal:          goal,
  period:        period,
  status:        status,
  donations:     donations,
  donationIndex: donationIndex,
  beneficiaries: beneficiaries,
  eventQueue:    []DomainEvent{},
  createdAt:     createdAt,
  createdBy:     createdBy,
  updatedAt:     updatedAt,
  updatedBy:     updatedBy,
  version:       version,
 }
}

// ========================================
// Business Operations
// ========================================

// Activate activates the campaign, making it available for donations.
func (c *DonationCampaign) Activate(activatedBy UserID) error {
 if c.status != StatusDraft {
  return errors.New("only draft campaigns can be activated")
 }

 if activatedBy == (UserID{}) {
  return errors.New("activated by user ID required")
 }

 // Validate campaign period
 if !c.period.ContainsDate(time.Now()) {
  return errors.New("campaign period has not started")
 }

 c.status = StatusActive
 c.markUpdated(activatedBy)

 c.registerEvent(CampaignActivated{
  CampaignID: c.id,
  OccurredAt: time.Now(),
  ActivatedBy: activatedBy,
 })

 return nil
}

// ReceiveDonation records a donation to this campaign.
//
// Returns the created Donation entity.
func (c *DonationCampaign) ReceiveDonation(
 donor Donor,
 amount Money,
 recordedBy UserID,
) (*Donation, error) {
 // Validate state
 if c.status != StatusActive {
  return nil, errors.New("campaign is not active")
 }

 if recordedBy == (UserID{}) {
  return nil, errors.New("recorded by user ID required")
 }

 // Validate campaign period
 now := time.Now()
 if !c.period.ContainsDate(now) {
  return nil, errors.New("campaign period has ended")
 }

 // Validate amount
 if err := amount.Validate(); err != nil {
  return nil, fmt.Errorf("invalid amount: %w", err)
 }

 if amount.IsNegativeOrZero() {
  return nil, errors.New("donation amount must be positive")
 }

 // Validate currency matches campaign
 if amount.Currency() != c.goal.TargetAmount().Currency() {
  return nil, fmt.Errorf("donation currency %s does not match campaign currency %s",
   amount.Currency(), c.goal.TargetAmount().Currency())
 }

 // Check if adding this donation would exceed goal
 totalAfter, err := c.calculateTotalDonations().Add(amount)
 if err != nil {
  return nil, err
 }

 exceedsGoal, _ := totalAfter.IsGreaterThan(c.goal.TargetAmount())
 if exceedsGoal {
  return nil, errors.New("donation would exceed campaign goal")
 }

 // Create donation
 donation, err := NewDonation(
  DonationID{},
  donor,
  amount,
  StatusPending,
  now,
 )
 if err != nil {
  return nil, err
 }

 // Add to aggregate
 c.donations = append(c.donations, *donation)
 c.donationIndex[donation.ID()] = &c.donations[len(c.donations)-1]

 c.markUpdated(recordedBy)

 // Register event
 c.registerEvent(DonationReceived{
  CampaignID: c.id,
  DonationID: donation.ID(),
  Amount:     amount,
  Donor:      donor,
  OccurredAt: now,
  RecordedBy: recordedBy,
 })

 // Check if goal reached
 if c.isGoalReached() {
  c.registerEvent(CampaignGoalReached{
   CampaignID:   c.id,
   TotalRaised:  c.calculateTotalDonations(),
   TargetAmount: c.goal.TargetAmount(),
   OccurredAt:   now,
  })
 }

 return donation, nil
}

// ConfirmDonation confirms a pending donation.
func (c *DonationCampaign) ConfirmDonation(donationID DonationID, confirmedBy UserID) error {
 if confirmedBy == (UserID{}) {
  return errors.New("confirmed by user ID required")
 }

 donation := c.donationIndex[donationID]
 if donation == nil {
  return fmt.Errorf("donation not found: %v", donationID)
 }

 if donation.Status() != StatusPending {
  return fmt.Errorf("donation is not pending: %s", donation.Status())
 }

 // Confirm donation
 donation.status = StatusCompleted

 c.markUpdated(confirmedBy)

 c.registerEvent(DonationConfirmed{
  CampaignID: c.id,
  DonationID: donationID,
  OccurredAt: time.Now(),
 })

 return nil
}

// Close closes the campaign, preventing further donations.
func (c *DonationCampaign) Close(closedBy UserID) error {
 if c.status != StatusActive {
  return errors.New("only active campaigns can be closed")
 }

 if closedBy == (UserID{}) {
  return errors.New("closed by user ID required")
 }

 // Check for pending donations
 hasPending := false
 for _, donation := range c.donations {
  if donation.Status() == StatusPending {
   hasPending = true
   break
  }
 }

 if hasPending {
  return errors.New("cannot close campaign with pending donations")
 }

 c.status = StatusClosed
 c.markUpdated(closedBy)

 c.registerEvent(CampaignClosed{
  CampaignID:  c.id,
  FinalAmount: c.calculateTotalDonations(),
  OccurredAt:  time.Now(),
  ClosedBy:    closedBy,
 })

 return nil
}

// AddBeneficiary adds a beneficiary to the campaign.
func (c *DonationCampaign) AddBeneficiary(beneficiary Beneficiary, addedBy UserID) error {
 if c.status == StatusClosed {
  return errors.New("cannot add beneficiary to closed campaign")
 }

 if addedBy == (UserID{}) {
  return errors.New("added by user ID required")
 }

 if err := beneficiary.Validate(); err != nil {
  return fmt.Errorf("invalid beneficiary: %w", err)
 }

 // Check for duplicates
 for _, existing := range c.beneficiaries {
  if existing.ID() == beneficiary.ID() {
   return errors.New("beneficiary already exists")
  }
 }

 c.beneficiaries = append(c.beneficiaries, beneficiary)
 c.markUpdated(addedBy)

 c.registerEvent(BeneficiaryAdded{
  CampaignID:    c.id,
  BeneficiaryID: beneficiary.ID(),
  OccurredAt:    time.Now(),
 })

 return nil
}

// ========================================
// Calculations
// ========================================

// calculateTotalDonations calculates total confirmed donations.
func (c *DonationCampaign) calculateTotalDonations() Money {
 total := MustZero(c.goal.TargetAmount().Currency())

 for _, donation := range c.donations {
  if donation.Status() == StatusCompleted {
   total, _ = total.Add(donation.Amount())
  }
 }

 return total
}

// CalculateProgress calculates fundraising progress as a percentage (0-100).
func (c *DonationCampaign) CalculateProgress() float64 {
 total := c.calculateTotalDonations()
 target := c.goal.TargetAmount()

 if target.IsZero() {
  return 0.0
 }

 percentage := float64(total.Amount()) / float64(target.Amount()) * 100.0

 return percentage
}

// isGoalReached checks if campaign goal has been reached.
func (c *DonationCampaign) isGoalReached() bool {
 total := c.calculateTotalDonations()
 reached, _ := total.IsGreaterThanOrEqual(c.goal.TargetAmount())
 return reached
}

// ========================================
// Validation
// ========================================

func validateCampaignName(name string) error {
 if name == "" {
  return errors.New("must not be empty")
 }

 if len(name) < 3 || len(name) > 200 {
  return errors.New("must be between 3 and 200 characters")
 }

 return nil
}

func validateBeneficiaries(beneficiaries []Beneficiary) error {
 if len(beneficiaries) == 0 {
  return errors.New("at least one beneficiary required")
 }

 for _, beneficiary := range beneficiaries {
  if err := beneficiary.Validate(); err != nil {
   return fmt.Errorf("invalid beneficiary: %w", err)
  }
 }

 return nil
}

// ========================================
// Domain Events
// ========================================

func (c *DonationCampaign) registerEvent(event DomainEvent) {
 c.eventQueue = append(c.eventQueue, event)
}

// GetPendingEvents returns all uncommitted domain events.
func (c *DonationCampaign) GetPendingEvents() []DomainEvent {
 events := make([]DomainEvent, len(c.eventQueue))
 copy(events, c.eventQueue)
 return events
}

// ClearPendingEvents clears all uncommitted domain events.
func (c *DonationCampaign) ClearPendingEvents() {
 c.eventQueue = []DomainEvent{}
}

// ========================================
// State Modification
// ========================================

func (c *DonationCampaign) markUpdated(updatedBy UserID) {
 c.updatedAt = time.Now()
 c.updatedBy = updatedBy
 c.version++
}

// ========================================
// Getters
// ========================================

// ID returns the campaign's unique identifier.
func (c *DonationCampaign) ID() CampaignID {
 return c.id
}

// Name returns the campaign name.
func (c *DonationCampaign) Name() string {
 return c.name
}

// Description returns the campaign description.
func (c *DonationCampaign) Description() string {
 return c.description
}

// Goal returns the campaign goal.
func (c *DonationCampaign) Goal() CampaignGoal {
 return c.goal
}

// Period returns the campaign period.
func (c *DonationCampaign) Period() DateRange {
 return c.period
}

// Status returns the campaign status.
func (c *DonationCampaign) Status() CampaignStatus {
 return c.status
}

// Donations returns a copy of all donations.
func (c *DonationCampaign) Donations() []Donation {
 result := make([]Donation, len(c.donations))
 copy(result, c.donations)
 return result
}

// Beneficiaries returns a copy of all beneficiaries.
func (c *DonationCampaign) Beneficiaries() []Beneficiary {
 result := make([]Beneficiary, len(c.beneficiaries))
 copy(result, c.beneficiaries)
 return result
}

// TotalDonations returns the total confirmed donation amount.
func (c *DonationCampaign) TotalDonations() Money {
 return c.calculateTotalDonations()
}

// IsActive returns true if the campaign is active.
func (c *DonationCampaign) IsActive() bool {
 return c.status == StatusActive
}

// IsGoalReached returns true if the campaign goal has been reached.
func (c *DonationCampaign) IsGoalReached() bool {
 return c.isGoalReached()
}

// CreatedAt returns when the campaign was created.
func (c *DonationCampaign) CreatedAt() time.Time {
 return c.createdAt
}

// Version returns the campaign version for optimistic locking.
func (c *DonationCampaign) Version() int {
 return c.version
}

// ========================================
// Equality
// ========================================

// Equals compares campaigns for identity equality.
func (c *DonationCampaign) Equals(other *DonationCampaign) bool {
 if c == nil || other == nil {
  return c == other
 }
 return c.id == other.id
}

// ========================================
// String Representation
// ========================================

// String returns a string representation of the campaign.
func (c *DonationCampaign) String() string {
 return fmt.Sprintf("DonationCampaign{id=%v, name=%q, status=%s, donations=%d, goal=%s, version=%d}",
  c.id, c.name, c.status, len(c.donations), c.goal.TargetAmount(), c.version)
}

// ========================================
// Supporting Types
// ========================================

// CampaignStatus represents campaign lifecycle status.
type CampaignStatus string

const (
 StatusDraft  CampaignStatus = "DRAFT"
 StatusActive CampaignStatus = "ACTIVE"
 StatusClosed CampaignStatus = "CLOSED"
)

func (s CampaignStatus) String() string {
 return string(s)
}

// DonationStatus represents donation lifecycle status.
type DonationStatus string

const (
 StatusPending   DonationStatus = "PENDING"
 StatusCompleted DonationStatus = "COMPLETED"
 StatusCancelled DonationStatus = "CANCELLED"
)

func (s DonationStatus) String() string {
 return string(s)
}

// Donation child entity
type Donation struct {
 id        DonationID
 donor     Donor
 amount    Money
 status    DonationStatus
 createdAt time.Time
}

// NewDonation creates a new donation.
func NewDonation(id DonationID, donor Donor, amount Money, status DonationStatus, createdAt time.Time) (*Donation, error) {
 if id == (DonationID{}) {
  return nil, errors.New("donation ID required")
 }

 if err := donor.Validate(); err != nil {
  return nil, fmt.Errorf("invalid donor: %w", err)
 }

 if err := amount.Validate(); err != nil {
  return nil, fmt.Errorf("invalid amount: %w", err)
 }

 return &Donation{
  id:        id,
  donor:     donor,
  amount:    amount,
  status:    status,
  createdAt: createdAt,
 }, nil
}

func (d *Donation) ID() DonationID       { return d.id }
func (d *Donation) Donor() Donor         { return d.donor }
func (d *Donation) Amount() Money        { return d.amount }
func (d *Donation) Status() DonationStatus { return d.status }
func (d *Donation) CreatedAt() time.Time { return d.createdAt }

func (d *Donation) Validate() error {
 if d.id == (DonationID{}) {
  return errors.New("ID required")
 }
 return nil
}

func (d *Donation) IsValid() bool {
 return d.Validate() == nil
}

// Beneficiary child entity
type Beneficiary struct {
 id          BeneficiaryID
 name        string
 description string
}

func NewBeneficiary(id BeneficiaryID, name string, description string) (*Beneficiary, error) {
 if id == (BeneficiaryID{}) {
  return nil, errors.New("beneficiary ID required")
 }

 if name == "" {
  return nil, errors.New("name required")
 }

 return &Beneficiary{
  id:          id,
  name:        name,
  description: description,
 }, nil
}

func (b *Beneficiary) ID() BeneficiaryID { return b.id }
func (b *Beneficiary) Name() string      { return b.name }
func (b *Beneficiary) Description() string { return b.description }

func (b *Beneficiary) Validate() error {
 if b.id == (BeneficiaryID{}) {
  return errors.New("ID required")
 }
 if b.name == "" {
  return errors.New("name required")
 }
 return nil
}

// Campaign-specific domain events
type CampaignActivated struct {
 CampaignID  CampaignID
 OccurredAt  time.Time
 ActivatedBy UserID
}

func (e CampaignActivated) EventType() string { return "CampaignActivated" }

type DonationConfirmed struct {
 CampaignID CampaignID
 DonationID DonationID
 OccurredAt time.Time
}

func (e DonationConfirmed) EventType() string { return "DonationConfirmed" }

type BeneficiaryAdded struct {
 CampaignID    CampaignID
 BeneficiaryID BeneficiaryID
 OccurredAt    time.Time
}

func (e BeneficiaryAdded) EventType() string { return "BeneficiaryAdded" }
```

## Before/After Comparison

### Before: Anemic Domain Model (Anti-Pattern)

```go
// INCORRECT: Anemic domain model - no behavior, just data
type Campaign struct {
 ID          CampaignID    // Exported fields
 Name        string
 Goal        Money
 Donations   []Donation    // Directly accessible
 Status      string
}

// Business logic in service layer (not in domain)
type CampaignService struct {
 repo CampaignRepository
}

func (s *CampaignService) ReceiveDonation(
 campaignID CampaignID,
 amount Money,
 donor Donor,
) error {
 // Service does all the work
 campaign, _ := s.repo.FindByID(campaignID)

 // Check invariants in service (should be in aggregate!)
 if campaign.Status != "ACTIVE" {
  return errors.New("not active")
 }

 total := Money{}
 for _, d := range campaign.Donations {
  total, _ = total.Add(d.Amount)
 }

 if total.Add(amount).IsGreaterThan(campaign.Goal) {
  return errors.New("exceeds goal")
 }

 // Directly modify campaign (no encapsulation!)
 campaign.Donations = append(campaign.Donations, Donation{
  Amount: amount,
  Donor:  donor,
 })

 s.repo.Save(campaign)

 return nil
}

// Problems:
// - No encapsulation (fields are public)
// - Invariants enforced in service, not domain
// - No domain events
// - Campaign is just a data container
// - Business logic scattered across services
```

### After: Rich Domain Model (Correct)

```go
// CORRECT: Rich domain model - behavior + data
type Campaign struct {
 // Unexported fields (encapsulation)
 id          CampaignID
 name        string
 goal        Money
 donations   []Donation
 status      CampaignStatus
 eventQueue  []DomainEvent
}

// Business logic in aggregate root
func (c *Campaign) ReceiveDonation(
 amount Money,
 donor Donor,
 recordedBy UserID,
) (*Donation, error) {
 // Aggregate validates invariants
 if c.status != StatusActive {
  return nil, errors.New("campaign is not active")
 }

 totalAfter, _ := c.calculateTotalDonations().Add(amount)
 if totalAfter.IsGreaterThan(c.goal.TargetAmount()) {
  return nil, errors.New("donation would exceed goal")
 }

 // Aggregate controls child entity creation
 donation, err := NewDonation(DonationID{}, donor, amount, time.Now())
 if err != nil {
  return nil, err
 }

 // Aggregate manages child collection
 c.donations = append(c.donations, *donation)

 // Aggregate registers domain events
 c.registerEvent(DonationReceived{
  CampaignID: c.id,
  DonationID: donation.ID(),
  Amount:     amount,
  OccurredAt: time.Now(),
 })

 c.markUpdated(recordedBy)

 return donation, nil
}

// Service is thin - just coordinates persistence
type CampaignService struct {
 repo CampaignRepository
}

func (s *CampaignService) ReceiveDonation(
 ctx context.Context,
 campaignID CampaignID,
 amount Money,
 donor Donor,
 recordedBy UserID,
) error {
 tx, _ := s.db.BeginTx(ctx, nil)
 defer tx.Rollback()

 // Load aggregate
 campaign, _ := s.repo.FindByID(ctx, tx, campaignID)

 // Delegate to aggregate (business logic in domain!)
 _, err := campaign.ReceiveDonation(amount, donor, recordedBy)
 if err != nil {
  return err
 }

 // Save aggregate
 s.repo.Save(ctx, tx, campaign)

 tx.Commit()

 // Publish events
 events := campaign.GetPendingEvents()
 s.eventPublisher.PublishBatch(ctx, events)
 campaign.ClearPendingEvents()

 return nil
}

// Benefits:
// - Encapsulation (private fields, controlled access)
// - Invariants enforced in aggregate
// - Domain events registered
// - Rich behavior in domain model
// - Thin service layer (just orchestration)
```

## Usage Guidelines

### Design Checklist

- [ ] **Single root**: Only one entity serves as aggregate root
- [ ] **Small boundary**: Keep aggregates small (5-10 entities max)
- [ ] **External references**: Outside objects reference only root's ID
- [ ] **Transactional consistency**: All changes atomic within aggregate
- [ ] **Invariant enforcement**: Root validates all aggregate-level rules
- [ ] **Child access control**: All modifications go through root
- [ ] **Domain events**: Important state changes publish events
- [ ] **Repository pattern**: One repository per aggregate
- [ ] **Optimistic locking**: Use version field for concurrency
- [ ] **Defensive copying**: Return copies of child collections

### Size Guidelines

```go
// CORRECT: Small aggregate (single campaign)
type Campaign struct {
 id        CampaignID
 donations []Donation     // 10-100 donations
 beneficiaries []Beneficiary // 1-5 beneficiaries
}

// INCORRECT: Large aggregate (entire system!)
type DonationSystem struct {
 campaigns []Campaign      // 1000s of campaigns - TOO BIG!
 donors    []Donor         // 1000s of donors - TOO BIG!
 payments  []Payment       // 1000s of payments - TOO BIG!
}

// Split into separate aggregates:
// - Campaign aggregate (with its donations)
// - Donor aggregate (with donor history)
// - Payment aggregate (with payment details)
```

### Consistency Boundaries

```go
// CORRECT: Strong consistency within aggregate
func (c *Campaign) ReceiveDonation(amount Money, donor Donor) error {
 // All checks happen atomically
 if c.status != StatusActive {
  return errors.New("not active")
 }

 if c.totalDonations().Add(amount).IsGreaterThan(c.goal) {
  return errors.New("exceeds goal")
 }

 c.donations = append(c.donations, donation)
 // Consistent: Either all changes happen or none
 return nil
}

// CORRECT: Eventual consistency between aggregates
func (s *CampaignService) TransferDonation(
 fromCampaignID CampaignID,
 toCampaignID CampaignID,
 amount Money,
) error {
 // Transaction 1: Withdraw from source
 {
  campaign, _ := s.repo.FindByID(fromCampaignID)
  campaign.WithdrawDonation(amount)
  s.repo.Save(campaign)

  // Publish event
  s.eventPublisher.Publish(DonationWithdrawn{...})
 }

 // Transaction 2: Event handler adds to destination
 // Eventual consistency is OK between aggregates!

 return nil
}
```

## Testing Aggregates

### Basic Aggregate Tests

```go
package model_test

import (
 "testing"
 "time"

 "myapp/model"
)

func TestDonationCampaign_NewDonationCampaign(t *testing.T) {
 tests := []struct {
  name          string
  campaignID    model.CampaignID
  campaignName  string
  goal          model.CampaignGoal
  period        model.DateRange
  beneficiaries []model.Beneficiary
  createdBy     model.UserID
  wantErr       bool
  expectedError string
 }{
  {
   "valid campaign",
   mustCampaignID("CAMP-001"),
   "Relief Fund",
   mustCampaignGoal(mustMoney(100000, "USD")),
   mustDateRange(time.Now(), time.Now().AddDate(0, 1, 0)),
   []model.Beneficiary{mustBeneficiary("BEN-001", "Orphanage")},
   mustUserID("USER-001"),
   false,
   "",
  },
  {
   "missing campaign ID",
   model.CampaignID{},
   "Relief Fund",
   mustCampaignGoal(mustMoney(100000, "USD")),
   mustDateRange(time.Now(), time.Now().AddDate(0, 1, 0)),
   []model.Beneficiary{mustBeneficiary("BEN-001", "Orphanage")},
   mustUserID("USER-001"),
   true,
   "campaign ID required",
  },
  {
   "empty name",
   mustCampaignID("CAMP-002"),
   "",
   mustCampaignGoal(mustMoney(100000, "USD")),
   mustDateRange(time.Now(), time.Now().AddDate(0, 1, 0)),
   []model.Beneficiary{mustBeneficiary("BEN-001", "Orphanage")},
   mustUserID("USER-001"),
   true,
   "invalid name",
  },
  {
   "no beneficiaries",
   mustCampaignID("CAMP-003"),
   "Relief Fund",
   mustCampaignGoal(mustMoney(100000, "USD")),
   mustDateRange(time.Now(), time.Now().AddDate(0, 1, 0)),
   []model.Beneficiary{},
   mustUserID("USER-001"),
   true,
   "at least one beneficiary required",
  },
 }

 for _, tt := range tests {
  t.Run(tt.name, func(t *testing.T) {
   got, err := model.NewDonationCampaign(
    tt.campaignID,
    tt.campaignName,
    "Description",
    tt.goal,
    tt.period,
    tt.beneficiaries,
    tt.createdBy,
   )

   if (err != nil) != tt.wantErr {
    t.Errorf("NewDonationCampaign() error = %v, wantErr %v", err, tt.wantErr)
    return
   }

   if tt.wantErr && err != nil {
    if !contains(err.Error(), tt.expectedError) {
     t.Errorf("error = %v, want substring %v", err.Error(), tt.expectedError)
    }
    return
   }

   // Verify initial state
   if got.ID() != tt.campaignID {
    t.Errorf("ID() = %v, want %v", got.ID(), tt.campaignID)
   }

   if got.Status() != model.StatusDraft {
    t.Errorf("Status() = %v, want %v", got.Status(), model.StatusDraft)
   }

   if got.Version() != 0 {
    t.Errorf("Version() = %v, want 0", got.Version())
   }

   // Verify event registered
   events := got.GetPendingEvents()
   if len(events) != 1 {
    t.Errorf("expected 1 event, got %d", len(events))
   }
  })
 }
}

func TestDonationCampaign_ReceiveDonation(t *testing.T) {
 // Setup
 goal := mustCampaignGoal(mustMoney(100000, "USD"))
 period := mustDateRange(time.Now().AddDate(0, 0, -1), time.Now().AddDate(0, 1, 0))
 beneficiaries := []model.Beneficiary{mustBeneficiary("BEN-001", "Orphanage")}

 campaign, err := model.NewDonationCampaign(
  mustCampaignID("CAMP-001"),
  "Relief Fund",
  "Description",
  goal,
  period,
  beneficiaries,
  mustUserID("USER-001"),
 )
 if err != nil {
  t.Fatalf("setup failed: %v", err)
 }

 // Activate campaign
 campaign.Activate(mustUserID("USER-001"))

 donor := mustDonor("DONOR-001", "John Doe")
 recordedBy := mustUserID("USER-002")

 tests := []struct {
  name      string
  amount    model.Money
  wantErr   bool
  wantCount int
 }{
  {"valid donation", mustMoney(10000, "USD"), false, 1},
  {"second donation", mustMoney(20000, "USD"), false, 2},
  {"exceeds goal", mustMoney(80000, "USD"), true, 2}, // 10k + 20k + 80k > 100k
  {"wrong currency", mustMoney(5000, "EUR"), true, 2},
  {"negative amount", mustMoney(-1000, "USD"), true, 2},
 }

 for _, tt := range tests {
  t.Run(tt.name, func(t *testing.T) {
   // Clone campaign for test isolation
   testCampaign := cloneCampaign(campaign)
   initialVersion := testCampaign.Version()

   donation, err := testCampaign.ReceiveDonation(donor, tt.amount, recordedBy)

   if (err != nil) != tt.wantErr {
    t.Errorf("ReceiveDonation() error = %v, wantErr %v", err, tt.wantErr)
    return
   }

   if tt.wantErr {
    // Version should not increment on error
    if testCampaign.Version() != initialVersion {
     t.Errorf("version incremented on error")
    }
    return
   }

   // Verify donation added
   if donation == nil {
    t.Error("expected donation, got nil")
   }

   if len(testCampaign.Donations()) != tt.wantCount {
    t.Errorf("donation count = %d, want %d", len(testCampaign.Donations()), tt.wantCount)
   }

   // Verify version incremented
   if testCampaign.Version() != initialVersion+1 {
    t.Errorf("version = %d, want %d", testCampaign.Version(), initialVersion+1)
   }

   // Verify event registered
   events := testCampaign.GetPendingEvents()
   foundEvent := false
   for _, event := range events {
    if event.EventType() == "DonationReceived" {
     foundEvent = true
     break
    }
   }
   if !foundEvent {
    t.Error("DonationReceived event not registered")
   }

   // Update campaign for next iteration
   campaign = testCampaign
  })
 }
}

func TestDonationCampaign_InvariantEnforcement(t *testing.T) {
 goal := mustCampaignGoal(mustMoney(50000, "USD"))
 period := mustDateRange(time.Now().AddDate(0, 0, -1), time.Now().AddDate(0, 1, 0))
 beneficiaries := []model.Beneficiary{mustBeneficiary("BEN-001", "Orphanage")}

 campaign, _ := model.NewDonationCampaign(
  mustCampaignID("CAMP-001"),
  "Relief Fund",
  "Description",
  goal,
  period,
  beneficiaries,
  mustUserID("USER-001"),
 )

 campaign.Activate(mustUserID("USER-001"))

 donor := mustDonor("DONOR-001", "John Doe")
 recordedBy := mustUserID("USER-002")

 // Add donations up to goal
 campaign.ReceiveDonation(donor, mustMoney(30000, "USD"), recordedBy)
 campaign.ReceiveDonation(donor, mustMoney(20000, "USD"), recordedBy)

 // Try to exceed goal (should fail)
 _, err := campaign.ReceiveDonation(donor, mustMoney(1000, "USD"), recordedBy)
 if err == nil {
  t.Error("expected error when exceeding goal, got nil")
 }

 if !contains(err.Error(), "exceed") {
  t.Errorf("error = %v, want 'exceed' in message", err)
 }

 // Verify total hasn't changed
 total := campaign.TotalDonations()
 if total.Amount() != 50000 {
  t.Errorf("total = %d, want 50000", total.Amount())
 }
}

func TestDonationCampaign_Equals(t *testing.T) {
 id1 := mustCampaignID("CAMP-001")
 id2 := mustCampaignID("CAMP-002")

 goal := mustCampaignGoal(mustMoney(100000, "USD"))
 period := mustDateRange(time.Now(), time.Now().AddDate(0, 1, 0))
 beneficiaries := []model.Beneficiary{mustBeneficiary("BEN-001", "Orphanage")}

 campaign1, _ := model.NewDonationCampaign(id1, "Fund 1", "Desc", goal, period, beneficiaries, mustUserID("USER-001"))
 campaign2, _ := model.NewDonationCampaign(id1, "Fund 2", "Desc", goal, period, beneficiaries, mustUserID("USER-002")) // Same ID
 campaign3, _ := model.NewDonationCampaign(id2, "Fund 1", "Desc", goal, period, beneficiaries, mustUserID("USER-001")) // Different ID

 // Same ID = equal (even with different state)
 if !campaign1.Equals(campaign2) {
  t.Error("campaigns with same ID should be equal")
 }

 // Different ID = not equal
 if campaign1.Equals(campaign3) {
  t.Error("campaigns with different IDs should not be equal")
 }

 // Nil handling
 if campaign1.Equals(nil) {
  t.Error("non-nil campaign should not equal nil")
 }
}

// Helper functions
func mustCampaignID(s string) model.CampaignID {
 id, _ := model.NewCampaignID(s)
 return id
}

func mustUserID(s string) model.UserID {
 id, _ := model.NewUserID(s)
 return id
}

func mustMoney(amount int64, currency string) model.Money {
 return model.MustNewMoney(amount, currency)
}

func mustCampaignGoal(target model.Money) model.CampaignGoal {
 goal, _ := model.NewCampaignGoal(target)
 return goal
}

func mustDateRange(start, end time.Time) model.DateRange {
 dr, _ := model.NewDateRange(start, end)
 return dr
}

func mustBeneficiary(id, name string) model.Beneficiary {
 ben, _ := model.NewBeneficiary(model.BeneficiaryID{}, name, "Description")
 return *ben
}

func mustDonor(id, name string) model.Donor {
 donor, _ := model.NewDonor(model.DonorID{}, name, "email@example.com")
 return donor
}

func cloneCampaign(c *model.DonationCampaign) *model.DonationCampaign {
 return model.ReconstituteDonationCampaign(
  c.ID(),
  c.Name(),
  c.Description(),
  c.Goal(),
  c.Period(),
  c.Status(),
  c.Donations(),
  c.Beneficiaries(),
  c.CreatedAt(),
  c.CreatedBy(),
  c.UpdatedAt(),
  c.UpdatedBy(),
  c.Version(),
 )
}

func contains(s, substr string) bool {
 return len(s) >= len(substr) && (s == substr || len(s) > len(substr) && (s[:len(substr)] == substr || s[len(s)-len(substr):] == substr))
}
```

## Related Documentation

**Go Language Documentation**:

- [Best Practices](../ex-so-prla-go__best-practices.md) - Go coding standards
- [Idioms](../ex-so-prla-go__idioms.md) - Go-specific patterns
- [Interfaces and Composition](../ex-so-prla-go__interfaces-and-composition.md) - Interface design

**Template Documentation**:

- [Entity Template](./entity-template.md) - Single entity with identity
- [Value Object Template](./value-object-template.md) - Immutable value objects

**DDD Concepts**:

- Repository pattern (persistence of aggregates)
- Domain events (cross-aggregate communication)
- Bounded contexts (aggregate boundaries)

**Principles**:

- [Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)
- [Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2025-01-23
**Go Version**: 1.18+
