---
title: TypeScript Entity Template
description: Template for creating Domain-Driven Design entities in TypeScript with branded types, lifecycle management, and business logic
category: template
tags:
  - typescript
  - ddd
  - domain-model
  - entity
  - aggregate
  - identity
  - lifecycle
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.9
related:
  - value-object-template.md
  - aggregate-template.md
  - ex-so-stla-ts__best-practices.md
  - ex-so-stla-ts__type-safety.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
---

# TypeScript Entity Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) entities in TypeScript. Entities have identity, lifecycle, and business logic while maintaining domain invariants through TypeScript's type system.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Identity with Branded Types](#identity-with-branded-types)
4. [Factory Functions](#factory-functions)
5. [Lifecycle Management](#lifecycle-management)
6. [Version Tracking](#version-tracking)
7. [Business Logic](#business-logic)
8. [Complete Example: Donation Entity](#complete-example-donation-entity)
9. [TypeScript vs Java/Go Comparison](#typescript-vs-javago-comparison)
10. [Testing Entities](#testing-entities)
11. [Related Documentation](#related-documentation)

## Overview

Entities in TypeScript use **branded types** for identity, **readonly properties** for immutability, and **factory functions** for controlled instantiation. TypeScript's type system provides compile-time safety that Java lacks and more flexibility than Go's struct-based approach.

**Key Characteristics**:

- **Identity-based equality**: Two entities are equal if their IDs match
- **Mutable state**: State changes through business methods (not via direct property mutation)
- **Lifecycle tracking**: createdAt, updatedAt, deletedAt timestamps
- **Version tracking**: Optimistic locking support with version field
- **Business methods**: Domain logic encapsulated in entity methods
- **Factory functions**: Constructor functions with validation
- **Branded types**: Type-safe identity preventing mix-ups

**TypeScript vs Java Comparison**:

```typescript
// TypeScript: Class with branded types and readonly
type DonationID = string & { readonly __brand: "DonationID" };

class Donation {
  // Readonly identity (cannot be reassigned)
  readonly id: DonationID;

  // Mutable state (private fields with public accessors)
  private amount: Money;
  private status: DonationStatus;
  private readonly createdAt: Date;
  private updatedAt: Date;
  private deletedAt?: Date;
  private version: number;

  // Private constructor - use factory function
  private constructor(params: DonationParams) {
    this.id = params.id;
    this.amount = params.amount;
    this.status = params.status;
    this.createdAt = params.createdAt;
    this.updatedAt = params.updatedAt;
    this.version = params.version;
  }

  // Factory function
  static create(id: DonationID, amount: Money, createdBy: UserID): Result<Donation, ValidationError> {
    // Validation and instantiation
  }

  // Business methods
  confirmDonation(confirmedBy: UserID): Result<void, DomainError> {
    this.status = DonationStatus.CONFIRMED;
    this.markUpdated(confirmedBy);
    return Ok(undefined);
  }
}
```

```java
// Java: Class with final fields and builder
public class Donation {
    private final DonationId id; // final identity
    private Money amount;        // mutable state
    private DonationStatus status;
    private final LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;
    private int version;

    // Private constructor
    private Donation(Builder builder) {
        this.id = builder.id;
        this.amount = builder.amount;
        // ...
    }

    // Factory method
    public static Donation create(
        DonationId id,
        Money amount,
        UserId createdBy
    ) {
        return new Builder(id, amount, createdBy).build();
    }

    // Business methods
    public void confirmDonation(UserId confirmedBy) {
        this.status = DonationStatus.CONFIRMED;
        this.markUpdated(confirmedBy);
    }
}
```

**Critical Differences**:

- TypeScript uses **branded types** (`type DonationID = string & { __brand }`) instead of Java's class-based IDs
- TypeScript uses **readonly** for identity immutability instead of Java's `final`
- TypeScript uses **Result<T, E>** monad pattern instead of throwing exceptions
- TypeScript uses **private constructor + static factory** instead of builder pattern
- TypeScript entities use **class-based approach** (not Go's struct + factory function pattern)
- TypeScript uses **Date** instead of Java's LocalDateTime

## Template Structure

````typescript
import { Result, Ok, Err } from "./result";
import { ValidationError, DomainError } from "./errors";

// ========================================
// Branded Type for Identity
// ========================================

/**
 * EntityID branded type ensures type safety for entity identifiers.
 *
 * Prevents accidentally mixing entity IDs with plain strings.
 */
export type EntityID = string & { readonly __brand: "EntityID" };

/**
 * Creates a typed EntityID from a string.
 *
 * @param value - String value to brand as EntityID
 * @returns Branded EntityID
 */
export function EntityID(value: string): EntityID {
  return value as EntityID;
}

// ========================================
// Entity Status Enum
// ========================================

/**
 * EntityStatus represents the lifecycle state of an entity.
 */
export enum EntityStatus {
  ACTIVE = "ACTIVE",
  SUSPENDED = "SUSPENDED",
  ARCHIVED = "ARCHIVED",
}

// ========================================
// Entity Class
// ========================================

/**
 * EntityName represents [brief description of what this entity models].
 *
 * Characteristics:
 * - Identity: Defined by ID field (equality based on ID only)
 * - Mutable: State can change through business methods
 * - Lifecycle: Created → Updated → Deleted (soft delete)
 * - Version tracking: Optimistic locking support
 *
 * Invariants:
 * - [Invariant 1]
 * - [Invariant 2]
 * - [Invariant 3]
 *
 * Responsibilities:
 * - [Primary responsibility 1]
 * - [Primary responsibility 2]
 * - [Primary responsibility 3]
 *
 * @example
 * ```typescript
 * const entity = EntityName.create(id, "name", valueObject, createdBy);
 * if (entity.isErr()) {
 *   return entity.error;
 * }
 * const result = entity.value.updateName("newName", updatedBy);
 * ```
 */
export class EntityName {
  // ========================================
  // Identity (readonly - never changes after creation)
  // ========================================

  /**
   * Unique identifier for this entity.
   * Readonly ensures identity cannot change after instantiation.
   */
  readonly id: EntityID;

  // ========================================
  // Essential State (private - modify via business methods)
  // ========================================

  private name: string;
  private valueObject: ValueObject;
  private status: EntityStatus;

  // ========================================
  // Audit Fields (lifecycle tracking)
  // ========================================

  private readonly createdAt: Date;
  private readonly createdBy: UserID;
  private updatedAt: Date;
  private updatedBy: UserID;
  private deletedAt?: Date; // undefined = not deleted
  private version: number; // Optimistic locking

  // ========================================
  // Associations (relationships to other entities)
  // ========================================

  private relatedEntities: readonly RelatedEntityID[];

  // ========================================
  // Private Constructor (use factory function)
  // ========================================

  /**
   * Private constructor prevents direct instantiation.
   * Use EntityName.create() or EntityName.reconstitute() instead.
   *
   * @param params - Entity parameters
   */
  private constructor(params: EntityNameParams) {
    this.id = params.id;
    this.name = params.name;
    this.valueObject = params.valueObject;
    this.status = params.status;
    this.createdAt = params.createdAt;
    this.createdBy = params.createdBy;
    this.updatedAt = params.updatedAt;
    this.updatedBy = params.updatedBy;
    this.deletedAt = params.deletedAt;
    this.version = params.version;
    this.relatedEntities = params.relatedEntities ?? [];
  }

  // ========================================
  // Factory Functions (Constructors)
  // ========================================

  /**
   * Creates a new EntityName with validation.
   *
   * Validates all invariants before creating instance.
   * Returns Result monad to handle errors functionally.
   *
   * @param id - Entity ID
   * @param name - Entity name
   * @param valueObject - Associated value object
   * @param createdBy - User who created the entity
   * @returns Result containing entity or validation error
   *
   * @example
   * ```typescript
   * const result = EntityName.create(id, "John Doe", email, userId);
   * if (result.isErr()) {
   *   console.error("Validation failed:", result.error);
   *   return;
   * }
   * const entity = result.value;
   * ```
   */
  static create(
    id: EntityID,
    name: string,
    valueObject: ValueObject,
    createdBy: UserID,
  ): Result<EntityName, ValidationError> {
    // Validate all invariants
    const nameValidation = this.validateName(name);
    if (nameValidation.isErr()) {
      return Err(new ValidationError(`Invalid name: ${nameValidation.error}`));
    }

    const valueObjectValidation = valueObject.validate();
    if (valueObjectValidation.isErr()) {
      return Err(new ValidationError(`Invalid value object: ${valueObjectValidation.error}`));
    }

    const now = new Date();

    return Ok(
      new EntityName({
        id,
        name,
        valueObject,
        status: EntityStatus.ACTIVE,
        createdAt: now,
        createdBy,
        updatedAt: now,
        updatedBy: createdBy,
        deletedAt: undefined,
        version: 0,
        relatedEntities: [],
      }),
    );
  }

  /**
   * Reconstitutes an entity from persistence.
   *
   * Bypasses validation for data already in database.
   * Use ONLY when loading from trusted persistence layer.
   *
   * @param params - Complete entity state from database
   * @returns Reconstituted entity
   *
   * @example
   * ```typescript
   * const entity = EntityName.reconstitute({
   *   id: EntityID("123"),
   *   name: "John Doe",
   *   // ... all other fields from database
   * });
   * ```
   */
  static reconstitute(params: EntityNameParams): EntityName {
    return new EntityName(params);
  }

  // ========================================
  // Validation (Private Static Methods)
  // ========================================

  private static validateName(name: string): Result<void, string> {
    if (!name || name.trim().length === 0) {
      return Err("Name must not be empty");
    }

    if (name.length < 3 || name.length > 100) {
      return Err("Name must be between 3 and 100 characters");
    }

    return Ok(undefined);
  }

  private validateUpdatable(): Result<void, DomainError> {
    if (this.deletedAt !== undefined) {
      return Err(new DomainError("Entity is deleted"));
    }

    if (this.status === EntityStatus.ARCHIVED) {
      return Err(new DomainError("Entity is archived"));
    }

    return Ok(undefined);
  }

  private canTransitionTo(newStatus: EntityStatus): boolean {
    switch (this.status) {
      case EntityStatus.ACTIVE:
        return newStatus === EntityStatus.SUSPENDED || newStatus === EntityStatus.ARCHIVED;
      case EntityStatus.SUSPENDED:
        return newStatus === EntityStatus.ACTIVE || newStatus === EntityStatus.ARCHIVED;
      case EntityStatus.ARCHIVED:
        return newStatus === EntityStatus.ACTIVE;
      default:
        return false;
    }
  }

  // ========================================
  // Business Logic (Public Methods)
  // ========================================

  /**
   * Updates the entity name with validation.
   *
   * Validates new name and ensures entity is updatable.
   * Increments version for optimistic locking.
   *
   * @param newName - New name value
   * @param updatedBy - User performing the update
   * @returns Result indicating success or domain error
   *
   * @example
   * ```typescript
   * const result = entity.updateName("Jane Doe", userId);
   * if (result.isErr()) {
   *   console.error("Update failed:", result.error);
   * }
   * ```
   */
  updateName(newName: string, updatedBy: UserID): Result<void, DomainError> {
    const updatableCheck = this.validateUpdatable();
    if (updatableCheck.isErr()) {
      return updatableCheck;
    }

    const nameValidation = EntityName.validateName(newName);
    if (nameValidation.isErr()) {
      return Err(new DomainError(`Invalid name: ${nameValidation.error}`));
    }

    this.name = newName;
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  /**
   * Changes entity status with state transition validation.
   *
   * @param newStatus - Target status
   * @param updatedBy - User performing the change
   * @returns Result indicating success or domain error
   */
  changeStatus(newStatus: EntityStatus, updatedBy: UserID): Result<void, DomainError> {
    if (!this.canTransitionTo(newStatus)) {
      return Err(new DomainError(`Cannot transition from ${this.status} to ${newStatus}`));
    }

    this.status = newStatus;
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  /**
   * Adds a related entity to this entity's collection.
   *
   * @param relatedID - Related entity ID
   * @param updatedBy - User performing the addition
   * @returns Result indicating success or domain error
   */
  addRelatedEntity(relatedID: RelatedEntityID, updatedBy: UserID): Result<void, DomainError> {
    const updatableCheck = this.validateUpdatable();
    if (updatableCheck.isErr()) {
      return updatableCheck;
    }

    // Check for duplicates
    if (this.relatedEntities.includes(relatedID)) {
      return Err(new DomainError("Related entity already exists"));
    }

    this.relatedEntities = [...this.relatedEntities, relatedID];
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  /**
   * Removes a related entity from this entity's collection.
   *
   * @param relatedID - Related entity ID to remove
   * @param updatedBy - User performing the removal
   * @returns Result with boolean indicating if entity was removed
   */
  removeRelatedEntity(relatedID: RelatedEntityID, updatedBy: UserID): Result<boolean, DomainError> {
    const updatableCheck = this.validateUpdatable();
    if (updatableCheck.isErr()) {
      return Err(updatableCheck.error);
    }

    const index = this.relatedEntities.indexOf(relatedID);
    if (index === -1) {
      return Ok(false);
    }

    this.relatedEntities = this.relatedEntities.filter((id) => id !== relatedID);
    this.markUpdated(updatedBy);

    return Ok(true);
  }

  /**
   * Soft-deletes this entity.
   *
   * Sets deletedAt timestamp instead of removing from database.
   *
   * @param deletedBy - User performing the deletion
   * @returns Result indicating success or domain error
   */
  delete(deletedBy: UserID): Result<void, DomainError> {
    if (this.deletedAt !== undefined) {
      return Err(new DomainError("Entity already deleted"));
    }

    this.deletedAt = new Date();
    this.status = EntityStatus.ARCHIVED;
    this.markUpdated(deletedBy);

    return Ok(undefined);
  }

  /**
   * Restores a soft-deleted entity.
   *
   * @param restoredBy - User performing the restoration
   * @returns Result indicating success or domain error
   */
  restore(restoredBy: UserID): Result<void, DomainError> {
    if (this.deletedAt === undefined) {
      return Err(new DomainError("Entity is not deleted"));
    }

    this.deletedAt = undefined;
    this.status = EntityStatus.ACTIVE;
    this.markUpdated(restoredBy);

    return Ok(undefined);
  }

  // ========================================
  // State Modification Helpers (Private)
  // ========================================

  private markUpdated(updatedBy: UserID): void {
    this.updatedAt = new Date();
    this.updatedBy = updatedBy;
    this.version++;
  }

  // ========================================
  // Query Methods (Getters)
  // ========================================

  /**
   * Returns the entity's unique identifier.
   */
  getId(): EntityID {
    return this.id;
  }

  /**
   * Returns the entity's name.
   */
  getName(): string {
    return this.name;
  }

  /**
   * Returns the entity's value object.
   */
  getValueObject(): ValueObject {
    return this.valueObject;
  }

  /**
   * Returns the entity's current status.
   */
  getStatus(): EntityStatus {
    return this.status;
  }

  /**
   * Returns when the entity was created.
   */
  getCreatedAt(): Date {
    return new Date(this.createdAt); // Defensive copy
  }

  /**
   * Returns who created the entity.
   */
  getCreatedBy(): UserID {
    return this.createdBy;
  }

  /**
   * Returns when the entity was last updated.
   */
  getUpdatedAt(): Date {
    return new Date(this.updatedAt); // Defensive copy
  }

  /**
   * Returns who last updated the entity.
   */
  getUpdatedBy(): UserID {
    return this.updatedBy;
  }

  /**
   * Returns when the entity was deleted (undefined if not deleted).
   */
  getDeletedAt(): Date | undefined {
    return this.deletedAt ? new Date(this.deletedAt) : undefined; // Defensive copy
  }

  /**
   * Returns the entity's version for optimistic locking.
   */
  getVersion(): number {
    return this.version;
  }

  /**
   * Returns a copy of related entity IDs.
   */
  getRelatedEntities(): readonly RelatedEntityID[] {
    return [...this.relatedEntities]; // Defensive copy
  }

  /**
   * Returns true if the entity is active.
   */
  isActive(): boolean {
    return this.status === EntityStatus.ACTIVE && this.deletedAt === undefined;
  }

  /**
   * Returns true if the entity is soft-deleted.
   */
  isDeleted(): boolean {
    return this.deletedAt !== undefined;
  }

  // ========================================
  // Equality (Identity-Based)
  // ========================================

  /**
   * Compares this entity with another for identity equality.
   *
   * Entities are equal if their IDs match (state doesn't matter).
   *
   * @param other - Entity to compare with
   * @returns true if entities have same ID
   *
   * @example
   * ```typescript
   * if (entity1.equals(entity2)) {
   *   console.log("Same entity");
   * }
   * ```
   */
  equals(other: EntityName): boolean {
    if (!(other instanceof EntityName)) {
      return false;
    }
    return this.id === other.id;
  }

  // ========================================
  // String Representation
  // ========================================

  /**
   * Returns a string representation of the entity.
   *
   * @returns String describing entity state
   */
  toString(): string {
    return `EntityName{id=${this.id}, name="${this.name}", status=${this.status}, version=${this.version}, deleted=${this.deletedAt !== undefined}}`;
  }

  /**
   * Returns JSON representation for serialization.
   *
   * @returns Plain object with all properties
   */
  toJSON(): EntityNameJSON {
    return {
      id: this.id,
      name: this.name,
      valueObject: this.valueObject.toJSON(),
      status: this.status,
      createdAt: this.createdAt.toISOString(),
      createdBy: this.createdBy,
      updatedAt: this.updatedAt.toISOString(),
      updatedBy: this.updatedBy,
      deletedAt: this.deletedAt?.toISOString(),
      version: this.version,
      relatedEntities: [...this.relatedEntities],
    };
  }
}

// ========================================
// Supporting Types
// ========================================

/**
 * Parameters for EntityName constructor.
 */
interface EntityNameParams {
  id: EntityID;
  name: string;
  valueObject: ValueObject;
  status: EntityStatus;
  createdAt: Date;
  createdBy: UserID;
  updatedAt: Date;
  updatedBy: UserID;
  deletedAt?: Date;
  version: number;
  relatedEntities?: readonly RelatedEntityID[];
}

/**
 * JSON representation of EntityName.
 */
interface EntityNameJSON {
  id: EntityID;
  name: string;
  valueObject: ValueObjectJSON;
  status: EntityStatus;
  createdAt: string;
  createdBy: UserID;
  updatedAt: string;
  updatedBy: UserID;
  deletedAt?: string;
  version: number;
  relatedEntities: RelatedEntityID[];
}
````

## Identity with Branded Types

TypeScript uses **branded types** to prevent mixing different ID types at compile time. This is safer than plain strings and more ergonomic than class-based IDs.

### Branded Type Pattern

```typescript
// CORRECT: Branded type prevents ID mix-ups
export type UserID = string & { readonly __brand: "UserID" };
export type OrderID = string & { readonly __brand: "OrderID" };

// Factory functions for creating branded types
export function UserID(value: string): UserID {
  return value as UserID;
}

export function OrderID(value: string): OrderID {
  return value as OrderID;
}

// Type safety at compile time
const userId = UserID("user-123");
const orderId = OrderID("order-456");

function getUser(id: UserID): User {
  // ...
}

getUser(userId); // ✅ OK
getUser(orderId); // ❌ Type error: OrderID is not assignable to UserID
getUser("user-123"); // ❌ Type error: string is not assignable to UserID
```

### Identity Field Pattern

```typescript
// CORRECT: Readonly identity field
class User {
  // Cannot be reassigned after construction
  readonly id: UserID;

  private constructor(params: UserParams) {
    this.id = params.id;
  }

  static create(id: UserID, name: string): Result<User, ValidationError> {
    // Factory validation
    return Ok(new User({ id, name /* ... */ }));
  }

  // No setter for ID!
  getId(): UserID {
    return this.id;
  }
}

// INCORRECT: Mutable ID (can be changed)
class BadUser {
  id: UserID; // Not readonly!

  setId(newId: UserID): void {
    this.id = newId; // Identity should never change!
  }
}
```

### Identity-Based Equality

```typescript
// CORRECT: Equality based on ID only
class User {
  readonly id: UserID;
  private name: string;

  equals(other: User): boolean {
    return this.id === other.id;
  }
}

// Usage: Same ID = equal entities, even with different state
const user1 = User.create(UserID("123"), "John");
const user2 = User.create(UserID("123"), "Jane");

user1.value.equals(user2.value); // true - same ID

// INCORRECT: Equality based on all fields (this is for value objects!)
class BadUser {
  readonly id: UserID;
  private name: string;

  equals(other: BadUser): boolean {
    return this.id === other.id && this.name === other.name; // Don't do this for entities!
  }
}
```

## Factory Functions

TypeScript entities use **static factory methods** combined with **private constructors** to enforce validation and controlled instantiation.

### Basic Factory Pattern

```typescript
// Pattern 1: Result-returning factory (preferred)
class User {
  private constructor(params: UserParams) {
    this.id = params.id;
    this.name = params.name;
    this.email = params.email;
    this.createdAt = params.createdAt;
    this.updatedAt = params.updatedAt;
    this.version = params.version;
  }

  static create(id: UserID, name: string, email: Email): Result<User, ValidationError> {
    // Validate all inputs
    if (!name || name.trim().length === 0) {
      return Err(new ValidationError("Name required"));
    }

    const emailValidation = email.validate();
    if (emailValidation.isErr()) {
      return Err(new ValidationError(`Invalid email: ${emailValidation.error}`));
    }

    const now = new Date();

    return Ok(
      new User({
        id,
        name,
        email,
        status: UserStatus.ACTIVE,
        createdAt: now,
        updatedAt: now,
        version: 0,
      }),
    );
  }

  // Pattern 2: Throwing factory (for guaranteed valid input)
  static mustCreate(id: UserID, name: string, email: Email): User {
    const result = User.create(id, name, email);
    if (result.isErr()) {
      throw new Error(`Invalid user: ${result.error}`);
    }
    return result.value;
  }
}

// Usage: Result return for runtime validation
const userResult = User.create(userId, userName, email);
if (userResult.isErr()) {
  console.error("Creation failed:", userResult.error);
  return;
}
const user = userResult.value;

// Usage: Throwing for compile-time constants
const adminUser = User.mustCreate(adminId, "Admin", adminEmail);
```

### Reconstitution Pattern

```typescript
// Reconstitute loads entity from persistence without validation
class User {
  private constructor(params: UserParams) {
    this.id = params.id;
    this.name = params.name;
    // ...
  }

  static create(id: UserID, name: string, email: Email): Result<User, ValidationError> {
    // Validates inputs
    return Ok(new User({ id, name, email /* ... */ }));
  }

  /**
   * Reconstitutes entity from persistence without validation.
   *
   * Use ONLY when loading from trusted persistence layer.
   * Data is already in database, no need to revalidate.
   */
  static reconstitute(params: UserParams): User {
    return new User(params);
  }
}

// Repository uses reconstitution
class UserRepository {
  async findById(id: UserID): Promise<User | null> {
    const row = await this.db.query("SELECT * FROM users WHERE id = $1", [id]);

    if (!row) {
      return null;
    }

    return User.reconstitute({
      id: UserID(row.id),
      name: row.name,
      email: Email(row.email), // Safe: already validated on insert
      status: row.status as UserStatus,
      createdAt: new Date(row.created_at),
      updatedAt: new Date(row.updated_at),
      deletedAt: row.deleted_at ? new Date(row.deleted_at) : undefined,
      version: row.version,
    });
  }
}
```

## Lifecycle Management

Entities track their lifecycle through audit fields: createdAt, updatedAt, deletedAt. TypeScript's `Date` type provides built-in timestamp support.

### Audit Fields Pattern

```typescript
class Order {
  readonly id: OrderID;

  // Business state
  private items: OrderItem[];
  private total: Money;
  private status: OrderStatus;

  // Audit fields
  private readonly createdAt: Date;
  private readonly createdBy: UserID;
  private updatedAt: Date;
  private updatedBy: UserID;
  private deletedAt?: Date; // undefined = not deleted
  private version: number;

  private constructor(params: OrderParams) {
    this.id = params.id;
    this.items = params.items;
    this.total = params.total;
    this.status = params.status;
    this.createdAt = params.createdAt;
    this.createdBy = params.createdBy;
    this.updatedAt = params.updatedAt;
    this.updatedBy = params.updatedBy;
    this.deletedAt = params.deletedAt;
    this.version = params.version;
  }

  static create(id: OrderID, items: OrderItem[], createdBy: UserID): Result<Order, ValidationError> {
    if (items.length === 0) {
      return Err(new ValidationError("Order must have at least one item"));
    }

    const now = new Date();
    const total = calculateTotal(items);

    return Ok(
      new Order({
        id,
        items,
        total,
        status: OrderStatus.PENDING,
        createdAt: now,
        createdBy,
        updatedAt: now,
        updatedBy: createdBy,
        deletedAt: undefined,
        version: 0,
      }),
    );
  }

  // Business methods track who made changes
  addItem(item: OrderItem, updatedBy: UserID): Result<void, DomainError> {
    if (this.deletedAt !== undefined) {
      return Err(new DomainError("Cannot modify deleted order"));
    }

    this.items = [...this.items, item];
    this.total = calculateTotal(this.items);
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  private markUpdated(updatedBy: UserID): void {
    this.updatedAt = new Date();
    this.updatedBy = updatedBy;
    this.version++;
  }
}
```

### Soft Delete Pattern

```typescript
class Order {
  private deletedAt?: Date;

  // Soft delete: Set deletedAt instead of removing from database
  delete(deletedBy: UserID): Result<void, DomainError> {
    if (this.deletedAt !== undefined) {
      return Err(new DomainError("Order already deleted"));
    }

    if (this.status === OrderStatus.SHIPPED) {
      return Err(new DomainError("Cannot delete shipped order"));
    }

    this.deletedAt = new Date();
    this.status = OrderStatus.CANCELLED;
    this.markUpdated(deletedBy);

    return Ok(undefined);
  }

  // Restore: Clear deletedAt
  restore(restoredBy: UserID): Result<void, DomainError> {
    if (this.deletedAt === undefined) {
      return Err(new DomainError("Order is not deleted"));
    }

    this.deletedAt = undefined;
    this.status = OrderStatus.PENDING;
    this.markUpdated(restoredBy);

    return Ok(undefined);
  }

  // Query methods
  isDeleted(): boolean {
    return this.deletedAt !== undefined;
  }

  getDeletedAt(): Date | undefined {
    return this.deletedAt ? new Date(this.deletedAt) : undefined;
  }
}
```

### Lifecycle Queries

```typescript
// Repository with lifecycle filtering
class OrderRepository {
  async findActive(): Promise<Order[]> {
    const rows = await this.db.query("SELECT * FROM orders WHERE deleted_at IS NULL ORDER BY created_at DESC");

    return rows.map((row) => Order.reconstitute(this.mapRowToParams(row)));
  }

  async findById(id: OrderID, includeDeleted = false): Promise<Order | null> {
    let query = "SELECT * FROM orders WHERE id = $1";
    if (!includeDeleted) {
      query += " AND deleted_at IS NULL";
    }

    const row = await this.db.query(query, [id]);
    if (!row) {
      return null;
    }

    return Order.reconstitute(this.mapRowToParams(row));
  }
}
```

## Version Tracking

Entities use a **version field** for optimistic locking to prevent concurrent modification conflicts. TypeScript's type system ensures version is always a number.

### Version Field Pattern

```typescript
class Account {
  readonly id: AccountID;
  private balance: Money;
  private version: number; // Optimistic locking version

  private constructor(params: AccountParams) {
    this.id = params.id;
    this.balance = params.balance;
    this.version = params.version;
  }

  deposit(amount: Money, updatedBy: UserID): Result<void, DomainError> {
    const newBalanceResult = this.balance.add(amount);
    if (newBalanceResult.isErr()) {
      return Err(new DomainError(`Cannot add balance: ${newBalanceResult.error}`));
    }

    this.balance = newBalanceResult.value;
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  private markUpdated(updatedBy: UserID): void {
    this.updatedAt = new Date();
    this.updatedBy = updatedBy;
    this.version++; // Increment on every modification
  }

  getVersion(): number {
    return this.version;
  }
}
```

### Repository with Optimistic Locking

```typescript
class AccountRepository {
  async save(account: Account): Promise<Result<void, OptimisticLockError>> {
    const result = await this.db.query(
      `UPDATE accounts
       SET balance = $1, updated_at = $2, updated_by = $3, version = $4
       WHERE id = $5 AND version = $6`,
      [
        account.getBalance().getAmount(),
        account.getUpdatedAt(),
        account.getUpdatedBy(),
        account.getVersion(), // New version
        account.getId(),
        account.getVersion() - 1, // Expected version
      ],
    );

    if (result.rowCount === 0) {
      return Err(new OptimisticLockError("Account was modified by another transaction"));
    }

    return Ok(undefined);
  }
}
```

### Conflict Resolution

```typescript
// Retry pattern for optimistic locking conflicts
async function depositWithRetry(
  repo: AccountRepository,
  accountId: AccountID,
  amount: Money,
  userId: UserID,
  maxRetries = 3,
): Promise<Result<void, DomainError>> {
  for (let i = 0; i < maxRetries; i++) {
    // Load current state
    const account = await repo.findById(accountId);
    if (!account) {
      return Err(new DomainError("Account not found"));
    }

    // Apply business logic
    const depositResult = account.deposit(amount, userId);
    if (depositResult.isErr()) {
      return depositResult;
    }

    // Try to save
    const saveResult = await repo.save(account);
    if (saveResult.isOk()) {
      return Ok(undefined); // Success!
    }

    // Check if it's an optimistic locking conflict
    if (!(saveResult.error instanceof OptimisticLockError)) {
      return Err(new DomainError(saveResult.error.message));
    }

    // Retry after brief delay
    await delay(100);
  }

  return Err(new DomainError("Max retries exceeded for optimistic locking"));
}

function delay(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
```

## Business Logic

Entity business logic encapsulates domain rules and state transitions using TypeScript's type system for safety.

### State Transition Pattern

```typescript
enum OrderStatus {
  PENDING = "PENDING",
  CONFIRMED = "CONFIRMED",
  SHIPPED = "SHIPPED",
  DELIVERED = "DELIVERED",
  CANCELLED = "CANCELLED",
}

class Order {
  readonly id: OrderID;
  private status: OrderStatus;

  // State transition with validation
  confirm(confirmedBy: UserID): Result<void, DomainError> {
    const updatableCheck = this.validateUpdatable();
    if (updatableCheck.isErr()) {
      return updatableCheck;
    }

    if (!this.canTransitionTo(OrderStatus.CONFIRMED)) {
      return Err(new DomainError(`Cannot confirm order in ${this.status} state`));
    }

    this.status = OrderStatus.CONFIRMED;
    this.markUpdated(confirmedBy);

    return Ok(undefined);
  }

  ship(shippedBy: UserID, trackingNumber: string): Result<void, DomainError> {
    const updatableCheck = this.validateUpdatable();
    if (updatableCheck.isErr()) {
      return updatableCheck;
    }

    if (!this.canTransitionTo(OrderStatus.SHIPPED)) {
      return Err(new DomainError(`Cannot ship order in ${this.status} state`));
    }

    if (!trackingNumber || trackingNumber.trim().length === 0) {
      return Err(new DomainError("Tracking number required"));
    }

    this.status = OrderStatus.SHIPPED;
    this.trackingNumber = trackingNumber;
    this.markUpdated(shippedBy);

    return Ok(undefined);
  }

  cancel(cancelledBy: UserID, reason: string): Result<void, DomainError> {
    if (!this.canTransitionTo(OrderStatus.CANCELLED)) {
      return Err(new DomainError(`Cannot cancel order in ${this.status} state`));
    }

    if (!reason || reason.trim().length === 0) {
      return Err(new DomainError("Cancellation reason required"));
    }

    this.status = OrderStatus.CANCELLED;
    this.cancellationReason = reason;
    this.markUpdated(cancelledBy);

    return Ok(undefined);
  }

  // State transition rules
  private canTransitionTo(target: OrderStatus): boolean {
    switch (this.status) {
      case OrderStatus.PENDING:
        return target === OrderStatus.CONFIRMED || target === OrderStatus.CANCELLED;
      case OrderStatus.CONFIRMED:
        return target === OrderStatus.SHIPPED || target === OrderStatus.CANCELLED;
      case OrderStatus.SHIPPED:
        return target === OrderStatus.DELIVERED;
      case OrderStatus.DELIVERED:
        return false; // Terminal state
      case OrderStatus.CANCELLED:
        return false; // Terminal state
      default:
        return false;
    }
  }

  private validateUpdatable(): Result<void, DomainError> {
    if (this.deletedAt !== undefined) {
      return Err(new DomainError("Order is deleted"));
    }
    return Ok(undefined);
  }
}
```

### Domain Validation

```typescript
class Product {
  readonly id: ProductID;
  private name: string;
  private price: Money;
  private stock: number;

  // Business method with validation
  updatePrice(newPrice: Money, updatedBy: UserID): Result<void, DomainError> {
    const updatableCheck = this.validateUpdatable();
    if (updatableCheck.isErr()) {
      return updatableCheck;
    }

    // Domain rule: Price cannot be negative
    if (newPrice.isNegative()) {
      return Err(new DomainError("Price must not be negative"));
    }

    // Domain rule: Price must be in valid range
    const minPrice = Money.create(100, "USD"); // $1.00 minimum
    const maxPrice = Money.create(100000000, "USD"); // $1M maximum

    if (minPrice.isOk() && newPrice.isLessThan(minPrice.value)) {
      return Err(new DomainError("Price below minimum allowed"));
    }

    if (maxPrice.isOk() && newPrice.isGreaterThan(maxPrice.value)) {
      return Err(new DomainError("Price exceeds maximum allowed"));
    }

    this.price = newPrice;
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  // Business method with complex validation
  decreaseStock(quantity: number, updatedBy: UserID): Result<void, DomainError> {
    const updatableCheck = this.validateUpdatable();
    if (updatableCheck.isErr()) {
      return updatableCheck;
    }

    // Domain rule: Quantity must be positive
    if (quantity <= 0) {
      return Err(new DomainError("Quantity must be positive"));
    }

    // Domain rule: Cannot decrease below zero
    if (this.stock < quantity) {
      return Err(new DomainError(`Insufficient stock: have ${this.stock}, need ${quantity}`));
    }

    this.stock -= quantity;
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  private validateUpdatable(): Result<void, DomainError> {
    if (this.deletedAt !== undefined) {
      return Err(new DomainError("Product is deleted"));
    }
    return Ok(undefined);
  }
}
```

## Complete Example: Donation Entity

Complete implementation with identity, lifecycle, versioning, and business logic in Islamic finance domain:

````typescript
import { Result, Ok, Err } from "../shared/result";
import { ValidationError, DomainError } from "../shared/errors";
import { Money } from "../value-objects/money";
import { DonorInfo } from "../value-objects/donor-info";

// ========================================
// Branded Types
// ========================================

export type DonationID = string & { readonly __brand: "DonationID" };
export type CampaignID = string & { readonly __brand: "CampaignID" };
export type UserID = string & { readonly __brand: "UserID" };

export function DonationID(value: string): DonationID {
  return value as DonationID;
}

export function CampaignID(value: string): CampaignID {
  return value as CampaignID;
}

export function UserID(value: string): UserID {
  return value as UserID;
}

// ========================================
// Enums
// ========================================

/**
 * DonationStatus represents the lifecycle state of a donation.
 */
export enum DonationStatus {
  PENDING = "PENDING",
  CONFIRMED = "CONFIRMED",
  DISBURSED = "DISBURSED",
  CANCELLED = "CANCELLED",
}

/**
 * DonationType categorizes the Islamic nature of the donation.
 */
export enum DonationType {
  ZAKAT = "ZAKAT", // Obligatory charity (2.5% of wealth)
  SADAQAH = "SADAQAH", // Voluntary charity
  WAQF = "WAQF", // Endowment
  QURBANI = "QURBANI", // Sacrificial offering
}

// ========================================
// Donation Entity
// ========================================

/**
 * Donation entity represents a charitable contribution in Islamic finance.
 *
 * Characteristics:
 * - Identity: Defined by donation ID
 * - Lifecycle: Pending → Confirmed → Disbursed or Cancelled
 * - Mutable: Status and metadata change over time
 * - Version tracking: Optimistic locking support
 *
 * Invariants:
 * - Amount must be positive
 * - Currency must match campaign currency
 * - Cannot modify after disbursement
 * - Cancelled donations cannot be reactivated
 *
 * Responsibilities:
 * - Track donation lifecycle
 * - Enforce Islamic charity rules
 * - Maintain audit trail
 * - Validate business rules
 *
 * @example
 * ```typescript
 * const donation = Donation.create(
 *   id,
 *   campaignId,
 *   amount,
 *   donorInfo,
 *   DonationType.ZAKAT,
 *   createdBy
 * );
 * if (donation.isOk()) {
 *   const confirmResult = donation.value.confirm(userId);
 * }
 * ```
 */
export class Donation {
  // ========================================
  // Identity
  // ========================================

  readonly id: DonationID;
  private readonly campaignId: CampaignID;

  // ========================================
  // Essential State
  // ========================================

  private amount: Money;
  private donorInfo: DonorInfo;
  private type: DonationType;
  private status: DonationStatus;

  // ========================================
  // Metadata
  // ========================================

  private isAnonymous: boolean;
  private message?: string;

  // ========================================
  // Disbursement Tracking
  // ========================================

  private confirmedAt?: Date;
  private confirmedBy?: UserID;
  private disbursedAt?: Date;
  private disbursedBy?: UserID;
  private disbursementReference?: string;

  // ========================================
  // Audit Fields
  // ========================================

  private readonly createdAt: Date;
  private readonly createdBy: UserID;
  private updatedAt: Date;
  private updatedBy: UserID;
  private deletedAt?: Date;
  private version: number;

  // ========================================
  // Private Constructor
  // ========================================

  private constructor(params: DonationParams) {
    this.id = params.id;
    this.campaignId = params.campaignId;
    this.amount = params.amount;
    this.donorInfo = params.donorInfo;
    this.type = params.type;
    this.status = params.status;
    this.isAnonymous = params.isAnonymous;
    this.message = params.message;
    this.confirmedAt = params.confirmedAt;
    this.confirmedBy = params.confirmedBy;
    this.disbursedAt = params.disbursedAt;
    this.disbursedBy = params.disbursedBy;
    this.disbursementReference = params.disbursementReference;
    this.createdAt = params.createdAt;
    this.createdBy = params.createdBy;
    this.updatedAt = params.updatedAt;
    this.updatedBy = params.updatedBy;
    this.deletedAt = params.deletedAt;
    this.version = params.version;
  }

  // ========================================
  // Factory Functions
  // ========================================

  /**
   * Creates a new donation with validation.
   */
  static create(
    id: DonationID,
    campaignId: CampaignID,
    amount: Money,
    donorInfo: DonorInfo,
    type: DonationType,
    createdBy: UserID,
    isAnonymous = false,
    message?: string,
  ): Result<Donation, ValidationError> {
    // Validate amount
    if (amount.isNegativeOrZero()) {
      return Err(new ValidationError("Donation amount must be positive"));
    }

    // Validate donor info
    const donorValidation = donorInfo.validate();
    if (donorValidation.isErr()) {
      return Err(new ValidationError(`Invalid donor info: ${donorValidation.error}`));
    }

    // Validate message length
    if (message && message.length > 500) {
      return Err(new ValidationError("Message must not exceed 500 characters"));
    }

    const now = new Date();

    return Ok(
      new Donation({
        id,
        campaignId,
        amount,
        donorInfo,
        type,
        status: DonationStatus.PENDING,
        isAnonymous,
        message,
        confirmedAt: undefined,
        confirmedBy: undefined,
        disbursedAt: undefined,
        disbursedBy: undefined,
        disbursementReference: undefined,
        createdAt: now,
        createdBy,
        updatedAt: now,
        updatedBy: createdBy,
        deletedAt: undefined,
        version: 0,
      }),
    );
  }

  /**
   * Reconstitutes donation from persistence.
   */
  static reconstitute(params: DonationParams): Donation {
    return new Donation(params);
  }

  // ========================================
  // Business Logic
  // ========================================

  /**
   * Confirms a pending donation.
   *
   * Validates that donation is in correct state and marks it as confirmed.
   */
  confirm(confirmedBy: UserID): Result<void, DomainError> {
    if (this.status !== DonationStatus.PENDING) {
      return Err(new DomainError(`Cannot confirm donation in ${this.status} state`));
    }

    if (this.deletedAt !== undefined) {
      return Err(new DomainError("Cannot confirm deleted donation"));
    }

    this.status = DonationStatus.CONFIRMED;
    this.confirmedAt = new Date();
    this.confirmedBy = confirmedBy;
    this.markUpdated(confirmedBy);

    return Ok(undefined);
  }

  /**
   * Disburses a confirmed donation.
   *
   * Records disbursement with reference number.
   */
  disburse(disbursedBy: UserID, reference: string): Result<void, DomainError> {
    if (this.status !== DonationStatus.CONFIRMED) {
      return Err(new DomainError(`Cannot disburse donation in ${this.status} state`));
    }

    if (!reference || reference.trim().length === 0) {
      return Err(new DomainError("Disbursement reference required"));
    }

    this.status = DonationStatus.DISBURSED;
    this.disbursedAt = new Date();
    this.disbursedBy = disbursedBy;
    this.disbursementReference = reference;
    this.markUpdated(disbursedBy);

    return Ok(undefined);
  }

  /**
   * Cancels a donation.
   *
   * Only pending donations can be cancelled.
   */
  cancel(cancelledBy: UserID, reason: string): Result<void, DomainError> {
    if (this.status !== DonationStatus.PENDING) {
      return Err(new DomainError("Only pending donations can be cancelled"));
    }

    if (!reason || reason.trim().length === 0) {
      return Err(new DomainError("Cancellation reason required"));
    }

    this.status = DonationStatus.CANCELLED;
    this.message = `Cancelled: ${reason}`;
    this.markUpdated(cancelledBy);

    return Ok(undefined);
  }

  /**
   * Updates donation message.
   *
   * Can only update message before disbursement.
   */
  updateMessage(newMessage: string, updatedBy: UserID): Result<void, DomainError> {
    if (this.status === DonationStatus.DISBURSED) {
      return Err(new DomainError("Cannot update message after disbursement"));
    }

    if (this.status === DonationStatus.CANCELLED) {
      return Err(new DomainError("Cannot update message of cancelled donation"));
    }

    if (newMessage.length > 500) {
      return Err(new DomainError("Message must not exceed 500 characters"));
    }

    this.message = newMessage;
    this.markUpdated(updatedBy);

    return Ok(undefined);
  }

  /**
   * Checks if donation is eligible for tax receipt.
   *
   * Only confirmed and disbursed donations are eligible.
   */
  isEligibleForTaxReceipt(): boolean {
    return this.status === DonationStatus.CONFIRMED || this.status === DonationStatus.DISBURSED;
  }

  /**
   * Checks if donation is Zakat.
   */
  isZakat(): boolean {
    return this.type === DonationType.ZAKAT;
  }

  /**
   * Checks if donation is Waqf (endowment).
   */
  isWaqf(): boolean {
    return this.type === DonationType.WAQF;
  }

  // ========================================
  // State Modification Helpers
  // ========================================

  private markUpdated(updatedBy: UserID): void {
    this.updatedAt = new Date();
    this.updatedBy = updatedBy;
    this.version++;
  }

  // ========================================
  // Getters
  // ========================================

  getId(): DonationID {
    return this.id;
  }

  getCampaignId(): CampaignID {
    return this.campaignId;
  }

  getAmount(): Money {
    return this.amount;
  }

  getDonorInfo(): DonorInfo {
    return this.donorInfo;
  }

  getType(): DonationType {
    return this.type;
  }

  getStatus(): DonationStatus {
    return this.status;
  }

  getIsAnonymous(): boolean {
    return this.isAnonymous;
  }

  getMessage(): string | undefined {
    return this.message;
  }

  getConfirmedAt(): Date | undefined {
    return this.confirmedAt ? new Date(this.confirmedAt) : undefined;
  }

  getConfirmedBy(): UserID | undefined {
    return this.confirmedBy;
  }

  getDisbursedAt(): Date | undefined {
    return this.disbursedAt ? new Date(this.disbursedAt) : undefined;
  }

  getDisbursedBy(): UserID | undefined {
    return this.disbursedBy;
  }

  getDisbursementReference(): string | undefined {
    return this.disbursementReference;
  }

  getCreatedAt(): Date {
    return new Date(this.createdAt);
  }

  getCreatedBy(): UserID {
    return this.createdBy;
  }

  getUpdatedAt(): Date {
    return new Date(this.updatedAt);
  }

  getUpdatedBy(): UserID {
    return this.updatedBy;
  }

  getDeletedAt(): Date | undefined {
    return this.deletedAt ? new Date(this.deletedAt) : undefined;
  }

  getVersion(): number {
    return this.version;
  }

  isPending(): boolean {
    return this.status === DonationStatus.PENDING;
  }

  isConfirmed(): boolean {
    return this.status === DonationStatus.CONFIRMED;
  }

  isDisbursed(): boolean {
    return this.status === DonationStatus.DISBURSED;
  }

  isCancelled(): boolean {
    return this.status === DonationStatus.CANCELLED;
  }

  isDeleted(): boolean {
    return this.deletedAt !== undefined;
  }

  // ========================================
  // Equality
  // ========================================

  equals(other: Donation): boolean {
    if (!(other instanceof Donation)) {
      return false;
    }
    return this.id === other.id;
  }

  // ========================================
  // String Representation
  // ========================================

  toString(): string {
    return `Donation{id=${this.id}, amount=${this.amount.toString()}, type=${this.type}, status=${this.status}, version=${this.version}}`;
  }

  toJSON(): DonationJSON {
    return {
      id: this.id,
      campaignId: this.campaignId,
      amount: this.amount.toJSON(),
      donorInfo: this.donorInfo.toJSON(),
      type: this.type,
      status: this.status,
      isAnonymous: this.isAnonymous,
      message: this.message,
      confirmedAt: this.confirmedAt?.toISOString(),
      confirmedBy: this.confirmedBy,
      disbursedAt: this.disbursedAt?.toISOString(),
      disbursedBy: this.disbursedBy,
      disbursementReference: this.disbursementReference,
      createdAt: this.createdAt.toISOString(),
      createdBy: this.createdBy,
      updatedAt: this.updatedAt.toISOString(),
      updatedBy: this.updatedBy,
      deletedAt: this.deletedAt?.toISOString(),
      version: this.version,
    };
  }
}

// ========================================
// Supporting Types
// ========================================

interface DonationParams {
  id: DonationID;
  campaignId: CampaignID;
  amount: Money;
  donorInfo: DonorInfo;
  type: DonationType;
  status: DonationStatus;
  isAnonymous: boolean;
  message?: string;
  confirmedAt?: Date;
  confirmedBy?: UserID;
  disbursedAt?: Date;
  disbursedBy?: UserID;
  disbursementReference?: string;
  createdAt: Date;
  createdBy: UserID;
  updatedAt: Date;
  updatedBy: UserID;
  deletedAt?: Date;
  version: number;
}

interface DonationJSON {
  id: DonationID;
  campaignId: CampaignID;
  amount: MoneyJSON;
  donorInfo: DonorInfoJSON;
  type: DonationType;
  status: DonationStatus;
  isAnonymous: boolean;
  message?: string;
  confirmedAt?: string;
  confirmedBy?: UserID;
  disbursedAt?: string;
  disbursedBy?: UserID;
  disbursementReference?: string;
  createdAt: string;
  createdBy: UserID;
  updatedAt: string;
  updatedBy: UserID;
  deletedAt?: string;
  version: number;
}
````

## TypeScript vs Java/Go Comparison

Understanding the differences between TypeScript entities and Java/Go implementations is crucial for proper domain modeling.

### Key Differences

| Aspect               | TypeScript                           | Java                              | Go                                |
| -------------------- | ------------------------------------ | --------------------------------- | --------------------------------- |
| **Identity**         | Branded types (`type ID = string &`) | Class-based ID (`class UserId`)   | Struct with unexported field      |
| **Immutability**     | `readonly` keyword                   | `final` keyword                   | Convention (no keyword)           |
| **Error Handling**   | `Result<T, E>` monad                 | Exceptions                        | Multiple return values `(T, err)` |
| **Factory**          | Static method + private constructor  | Builder pattern or factory method | Factory function                  |
| **Null Safety**      | `undefined` vs `null`, strict mode   | `@Nullable` / `Optional<T>`       | Pointers, `nil`                   |
| **Date/Time**        | `Date` object                        | `LocalDateTime`                   | `time.Time`                       |
| **Collections**      | Arrays with spread operator          | `List<T>`, `ArrayList<T>`         | Slices                            |
| **Access Modifiers** | `private`, `readonly`                | `private`, `final`, `public`      | Unexported (lowercase)            |

### Side-by-Side Comparison

```typescript
// TypeScript: Class with branded types
type UserID = string & { readonly __brand: "UserID" };

class User {
  readonly id: UserID; // Readonly identity
  private email: Email; // Private mutable state
  private readonly createdAt: Date; // Readonly lifecycle
  private updatedAt: Date;
  private version: number;

  private constructor(params: UserParams) {
    this.id = params.id;
    this.email = params.email;
    this.createdAt = params.createdAt;
    this.updatedAt = params.updatedAt;
    this.version = params.version;
  }

  static create(id: UserID, email: Email): Result<User, ValidationError> {
    // Returns Result monad
    return Ok(new User({ id, email /* ... */ }));
  }

  updateEmail(email: Email): Result<void, DomainError> {
    this.email = email;
    this.markUpdated();
    return Ok(undefined);
  }

  equals(other: User): boolean {
    return this.id === other.id; // ID comparison
  }
}
```

```java
// Java: Class with builder and final
public class User {
    private final UserId id;       // final identity
    private Email email;           // mutable state
    private final LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private int version;

    private User(Builder builder) {
        this.id = builder.id;
        this.email = builder.email;
        this.createdAt = builder.createdAt;
        this.updatedAt = builder.updatedAt;
        this.version = builder.version;
    }

    public static User create(UserId id, Email email) {
        // Throws exception on validation failure
        return new Builder(id, email).build();
    }

    public void updateEmail(Email email) {
        this.email = email;
        this.markUpdated();
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof User)) return false;
        User other = (User) obj;
        return this.id.equals(other.id);
    }
}
```

```go
// Go: Struct with unexported fields
type UserID struct{ value string }

type User struct {
 id        UserID    // Unexported (immutable by convention)
 email     Email     // Unexported
 createdAt time.Time // Unexported
 updatedAt time.Time
 version   int
}

// Factory function
func NewUser(id UserID, email Email) (*User, error) {
 // Returns (value, error) tuple
 return &User{
  id:        id,
  email:     email,
  createdAt: time.Now(),
  updatedAt: time.Now(),
  version:   0,
 }, nil
}

// Pointer receiver for mutations
func (u *User) UpdateEmail(email Email) error {
 u.email = email
 u.markUpdated()
 return nil
}

// Equality method
func (u *User) Equals(other *User) bool {
 return u.id == other.id // ID comparison
}
```

**Critical Differences**:

- TypeScript uses **branded types** for type-safe IDs at compile time
- TypeScript uses **Result monad** for functional error handling
- TypeScript uses **readonly** for immutability instead of Go's convention or Java's `final`
- TypeScript has **strict null checks** (`strictNullChecks: true` in tsconfig)
- TypeScript entities use **class-based OOP** (not Go's struct-based approach)
- TypeScript uses **Date** for timestamps (simpler than Java's `LocalDateTime`)

## Testing Entities

### Basic Entity Tests

```typescript
import { describe, it, expect } from "vitest";
import { Donation, DonationID, CampaignID, UserID, DonationType } from "./donation";
import { Money } from "../value-objects/money";
import { DonorInfo } from "../value-objects/donor-info";

describe("Donation Entity", () => {
  describe("create", () => {
    it("should create valid donation", () => {
      // Arrange
      const id = DonationID("DON-001");
      const campaignId = CampaignID("CAMP-001");
      const amount = Money.create(10000, "USD");
      const donorInfo = DonorInfo.create("John Doe", "john@example.com");
      const createdBy = UserID("USER-001");

      // Act
      const result = Donation.create(id, campaignId, amount.value, donorInfo.value, DonationType.ZAKAT, createdBy);

      // Assert
      expect(result.isOk()).toBe(true);
      expect(result.value.getId()).toBe(id);
      expect(result.value.getAmount().equals(amount.value)).toBe(true);
      expect(result.value.isPending()).toBe(true);
      expect(result.value.getVersion()).toBe(0);
    });

    it("should reject negative amount", () => {
      // Arrange
      const id = DonationID("DON-002");
      const campaignId = CampaignID("CAMP-001");
      const amount = Money.create(-10000, "USD");
      const donorInfo = DonorInfo.create("John Doe", "john@example.com");
      const createdBy = UserID("USER-001");

      // Act
      const result = Donation.create(id, campaignId, amount.value, donorInfo.value, DonationType.ZAKAT, createdBy);

      // Assert
      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("must be positive");
    });

    it("should reject message exceeding 500 characters", () => {
      // Arrange
      const id = DonationID("DON-003");
      const campaignId = CampaignID("CAMP-001");
      const amount = Money.create(10000, "USD");
      const donorInfo = DonorInfo.create("John Doe", "john@example.com");
      const createdBy = UserID("USER-001");
      const longMessage = "a".repeat(501);

      // Act
      const result = Donation.create(
        id,
        campaignId,
        amount.value,
        donorInfo.value,
        DonationType.ZAKAT,
        createdBy,
        false,
        longMessage,
      );

      // Assert
      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("500 characters");
    });
  });

  describe("confirm", () => {
    it("should confirm pending donation", () => {
      // Arrange
      const donation = createValidDonation();
      const confirmedBy = UserID("USER-002");
      const initialVersion = donation.getVersion();

      // Act
      const result = donation.confirm(confirmedBy);

      // Assert
      expect(result.isOk()).toBe(true);
      expect(donation.isConfirmed()).toBe(true);
      expect(donation.getConfirmedBy()).toBe(confirmedBy);
      expect(donation.getVersion()).toBe(initialVersion + 1);
    });

    it("should reject confirming non-pending donation", () => {
      // Arrange
      const donation = createValidDonation();
      const userId = UserID("USER-002");

      donation.confirm(userId); // First confirmation
      const initialVersion = donation.getVersion();

      // Act
      const result = donation.confirm(userId); // Second confirmation

      // Assert
      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("Cannot confirm");
      expect(donation.getVersion()).toBe(initialVersion); // Version not incremented on error
    });
  });

  describe("disburse", () => {
    it("should disburse confirmed donation", () => {
      // Arrange
      const donation = createValidDonation();
      const userId = UserID("USER-002");
      donation.confirm(userId);

      const reference = "DISB-2024-001";
      const initialVersion = donation.getVersion();

      // Act
      const result = donation.disburse(userId, reference);

      // Assert
      expect(result.isOk()).toBe(true);
      expect(donation.isDisbursed()).toBe(true);
      expect(donation.getDisbursementReference()).toBe(reference);
      expect(donation.getVersion()).toBe(initialVersion + 1);
    });

    it("should reject disbursing without reference", () => {
      // Arrange
      const donation = createValidDonation();
      const userId = UserID("USER-002");
      donation.confirm(userId);

      // Act
      const result = donation.disburse(userId, "");

      // Assert
      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("reference required");
    });

    it("should reject disbursing pending donation", () => {
      // Arrange
      const donation = createValidDonation();
      const userId = UserID("USER-002");

      // Act
      const result = donation.disburse(userId, "REF-001");

      // Assert
      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("Cannot disburse");
    });
  });

  describe("equals", () => {
    it("should be equal with same ID", () => {
      // Arrange
      const id = DonationID("DON-001");
      const donation1 = createDonationWithId(id);
      const donation2 = createDonationWithId(id); // Different state, same ID

      // Act & Assert
      expect(donation1.equals(donation2)).toBe(true);
    });

    it("should not be equal with different IDs", () => {
      // Arrange
      const donation1 = createDonationWithId(DonationID("DON-001"));
      const donation2 = createDonationWithId(DonationID("DON-002"));

      // Act & Assert
      expect(donation1.equals(donation2)).toBe(false);
    });
  });

  describe("isEligibleForTaxReceipt", () => {
    it("should be eligible when confirmed", () => {
      // Arrange
      const donation = createValidDonation();
      donation.confirm(UserID("USER-001"));

      // Act & Assert
      expect(donation.isEligibleForTaxReceipt()).toBe(true);
    });

    it("should be eligible when disbursed", () => {
      // Arrange
      const donation = createValidDonation();
      const userId = UserID("USER-001");
      donation.confirm(userId);
      donation.disburse(userId, "REF-001");

      // Act & Assert
      expect(donation.isEligibleForTaxReceipt()).toBe(true);
    });

    it("should not be eligible when pending", () => {
      // Arrange
      const donation = createValidDonation();

      // Act & Assert
      expect(donation.isEligibleForTaxReceipt()).toBe(false);
    });

    it("should not be eligible when cancelled", () => {
      // Arrange
      const donation = createValidDonation();
      donation.cancel(UserID("USER-001"), "Test cancellation");

      // Act & Assert
      expect(donation.isEligibleForTaxReceipt()).toBe(false);
    });
  });
});

// ========================================
// Test Helpers
// ========================================

function createValidDonation(): Donation {
  const id = DonationID("DON-TEST-001");
  const campaignId = CampaignID("CAMP-001");
  const amount = Money.create(10000, "USD").value;
  const donorInfo = DonorInfo.create("John Doe", "john@example.com").value;
  const createdBy = UserID("USER-001");

  const result = Donation.create(id, campaignId, amount, donorInfo, DonationType.ZAKAT, createdBy);

  if (result.isErr()) {
    throw new Error(`Failed to create test donation: ${result.error}`);
  }

  return result.value;
}

function createDonationWithId(id: DonationID): Donation {
  const campaignId = CampaignID("CAMP-001");
  const amount = Money.create(10000, "USD").value;
  const donorInfo = DonorInfo.create("John Doe", "john@example.com").value;
  const createdBy = UserID("USER-001");

  const result = Donation.create(id, campaignId, amount, donorInfo, DonationType.ZAKAT, createdBy);

  if (result.isErr()) {
    throw new Error(`Failed to create test donation: ${result.error}`);
  }

  return result.value;
}
```

## Related Documentation

**TypeScript Language Documentation**:

- [TypeScript Best Practices](../ex-so-stla-ts__best-practices.md) - TypeScript coding standards
- [Type Safety in TypeScript](../ex-so-stla-ts__type-safety.md) - Advanced type patterns
- [Error Handling with Result](../ex-so-stla-ts__error-handling.md) - Result monad pattern

**Template Documentation**:

- [Value Object Template](./value-object-template.md) - Immutable value objects in TypeScript
- [Aggregate Template](./aggregate-template.md) - Aggregate root pattern

**DDD Concepts**:

- Aggregate pattern (combines entities and value objects)
- Repository pattern (persistence)
- Domain events (entity state changes)

**Principles**:

- [Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)
- [Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
