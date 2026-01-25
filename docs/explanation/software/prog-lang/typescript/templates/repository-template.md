---
title: TypeScript Repository Template
description: Template for creating Domain-Driven Design repositories in TypeScript with TypeORM/Prisma integration, transaction management, and comprehensive testing patterns
category: template
tags:
  - typescript
  - ddd
  - repository
  - persistence
  - database
  - typeorm
  - prisma
  - transactions
  - specification
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
  - service-layer-template.md
  - ex-so-prla-ts__domain-driven-design.md
  - ex-so-prla-ts__best-practices.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript Repository Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) repositories in TypeScript. Repositories provide a clean abstraction over data persistence, allowing domain models to remain independent of database implementation details using TypeORM or Prisma.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Repository Interface](#repository-interface)
4. [TypeORM Implementation](#typeorm-implementation)
5. [Prisma Implementation](#prisma-implementation)
6. [Business Queries](#business-queries)
7. [Specification Pattern](#specification-pattern)
8. [Transaction Support](#transaction-support)
9. [Complete Example: Donation Repository](#complete-example-donation-repository)
10. [Before/After Comparison](#beforeafter-comparison)
11. [Usage Guidelines](#usage-guidelines)
12. [Testing Repositories](#testing-repositories)
13. [Related Documentation](#related-documentation)

## Overview

Repositories in TypeScript serve as the **boundary between domain models and persistence infrastructure**. They provide a collection-like interface for accessing and storing aggregates, hiding database complexity from business logic.

**Key Characteristics**:

- **Interface-first design**: Domain layer defines interface, infrastructure implements
- **Aggregate-focused**: One repository per aggregate root (not per entity)
- **Promise-based operations**: All repository methods return Promises
- **Business query methods**: Domain-specific queries beyond basic CRUD
- **Result type error handling**: Explicit error handling with Result type
- **Reconstitution**: Load aggregates from persistence using factory functions

**TypeScript vs Java/Go Comparison**:

```typescript
// TypeScript: Interface in domain package, implementation in infrastructure
// domain/repositories/DonationRepository.ts

export interface DonationRepository {
  save(donation: Donation): Promise<Result<void, RepositoryError>>;
  findById(id: DonationID): Promise<Result<Donation, RepositoryError>>;
  findByDonor(donorId: DonorID): Promise<Result<Donation[], RepositoryError>>;
  delete(id: DonationID): Promise<Result<void, RepositoryError>>;
}

// infrastructure/persistence/typeorm/TypeORMDonationRepository.ts

export class TypeORMDonationRepository implements DonationRepository {
  constructor(private readonly entityManager: EntityManager) {}

  async save(donation: Donation): Promise<Result<void, RepositoryError>> {
    // Implementation using TypeORM
  }
}
```

```java
// Java: Interface in domain, implementation in infrastructure
package com.example.domain.repositories;

public interface DonationRepository {
    void save(Donation donation);
    Optional<Donation> findById(DonationId id);
    List<Donation> findByDonor(DonorId donorId);
    void delete(DonationId id);
}

// JPA implementation (infrastructure layer)
package com.example.infrastructure.persistence;

@Repository
public class JpaDonationRepository implements DonationRepository {
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public void save(Donation donation) {
        entityManager.persist(donation);
    }
}
```

**Critical Differences**:

- TypeScript uses **async/await with Promises** instead of synchronous/blocking calls
- TypeScript returns **Result type** instead of throwing exceptions
- TypeScript uses **ORM frameworks (TypeORM/Prisma)** instead of direct SQL
- TypeScript repositories use **reconstitution functions** to load aggregates
- TypeScript has **dependency injection via constructor injection** (manual or with frameworks like NestJS)
- TypeScript uses **parameterized queries** for SQL safety

## Template Structure

```typescript
// domain/repositories/AggregateRepository.ts
import { Result } from "@/shared/result";

/**
 * AggregateRepository provides persistence operations for AggregateRoot entities.
 *
 * Responsibilities:
 *   - Save and retrieve aggregates
 *   - Execute domain-specific queries
 *   - Hide persistence implementation details
 *   - Maintain aggregate boundaries
 *
 * Transactional Operations:
 *   - All methods are Promise-based for async operations
 *   - Use transaction manager for atomic operations
 *
 * Error Handling:
 *   - Returns Result<T, RepositoryError>
 *   - NotFoundError when aggregate doesn't exist
 *   - OptimisticLockError when version mismatch occurs
 *   - Returns infrastructure errors wrapped with context
 *
 * @example
 * const repo = new TypeORMAggregateRepository(entityManager);
 * const result = await repo.findById(id);
 * if (result.isErr()) {
 *   return result;
 * }
 * const aggregate = result.value;
 * await repo.save(aggregate);
 */
export interface AggregateRepository {
  // ========================================
  // Standard CRUD Operations
  // ========================================

  /**
   * Save creates or updates an aggregate.
   *
   * Uses optimistic locking based on version field.
   * Returns OptimisticLockError if version mismatch.
   */
  save(aggregate: AggregateRoot): Promise<Result<void, RepositoryError>>;

  /**
   * FindByID retrieves an aggregate by its ID.
   *
   * Returns NotFoundError if aggregate doesn't exist.
   * Excludes soft-deleted aggregates by default.
   */
  findById(id: AggregateID): Promise<Result<AggregateRoot, RepositoryError>>;

  /**
   * FindByIDIncludingDeleted retrieves an aggregate including soft-deleted.
   *
   * Returns NotFoundError if aggregate doesn't exist.
   */
  findByIdIncludingDeleted(id: AggregateID): Promise<Result<AggregateRoot, RepositoryError>>;

  /**
   * Delete removes an aggregate (hard delete).
   *
   * Returns NotFoundError if aggregate doesn't exist.
   * Consider using soft delete via aggregate method instead.
   */
  delete(id: AggregateID): Promise<Result<void, RepositoryError>>;

  // ========================================
  // Business Query Methods
  // ========================================

  /**
   * FindByOwner retrieves all aggregates for a specific owner.
   *
   * Returns empty array if no aggregates found (not an error).
   * Excludes soft-deleted aggregates.
   */
  findByOwner(ownerId: OwnerID): Promise<Result<AggregateRoot[], RepositoryError>>;

  /**
   * FindByStatus retrieves aggregates with specific status.
   *
   * Returns empty array if no aggregates found.
   */
  findByStatus(status: AggregateStatus): Promise<Result<AggregateRoot[], RepositoryError>>;

  /**
   * FindPendingApproval retrieves aggregates pending approval.
   *
   * Business-specific query method.
   */
  findPendingApproval(): Promise<Result<AggregateRoot[], RepositoryError>>;

  // ========================================
  // Specification Pattern
  // ========================================

  /**
   * FindBySpec retrieves aggregates matching a specification.
   *
   * Uses Specification pattern for complex queries.
   */
  findBySpec(spec: Specification<AggregateRoot>): Promise<Result<AggregateRoot[], RepositoryError>>;

  // ========================================
  // Transaction Support
  // ========================================

  /**
   * WithTransaction executes operations within a transaction.
   *
   * All repository operations within the callback use the same transaction.
   * Transaction is automatically committed on success, rolled back on error.
   */
  withTransaction<T>(
    callback: (repo: AggregateRepository) => Promise<Result<T, RepositoryError>>,
  ): Promise<Result<T, RepositoryError>>;
}
```

## Repository Interface

Define the repository interface in the domain layer:

```typescript
// domain/repositories/DonationRepository.ts
import { Result } from "@/shared/result";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { DonorID } from "@/domain/value-objects/DonorID";
import { RecipientID } from "@/domain/value-objects/RecipientID";
import { DonationStatus } from "@/domain/value-objects/DonationStatus";
import { DateRange } from "@/domain/value-objects/DateRange";

export interface DonationRepository {
  // ========================================
  // Standard CRUD Operations
  // ========================================

  save(donation: Donation): Promise<Result<void, RepositoryError>>;

  findById(id: DonationID): Promise<Result<Donation, RepositoryError>>;

  findByIdIncludingDeleted(id: DonationID): Promise<Result<Donation, RepositoryError>>;

  delete(id: DonationID): Promise<Result<void, RepositoryError>>;

  // ========================================
  // Business Query Methods
  // ========================================

  /**
   * Find all donations made by a specific donor.
   */
  findByDonor(donorId: DonorID): Promise<Result<Donation[], RepositoryError>>;

  /**
   * Find all donations to a specific recipient.
   */
  findByRecipient(recipientId: RecipientID): Promise<Result<Donation[], RepositoryError>>;

  /**
   * Find donations with a specific status.
   */
  findByStatus(status: DonationStatus): Promise<Result<Donation[], RepositoryError>>;

  /**
   * Find donations within a date range.
   */
  findByDateRange(range: DateRange): Promise<Result<Donation[], RepositoryError>>;

  /**
   * Find donations pending verification.
   */
  findPendingVerification(): Promise<Result<Donation[], RepositoryError>>;

  /**
   * Find total donation amount by donor.
   */
  getTotalAmountByDonor(donorId: DonorID): Promise<Result<Money, RepositoryError>>;

  // ========================================
  // Transaction Support
  // ========================================

  withTransaction<T>(
    callback: (repo: DonationRepository) => Promise<Result<T, RepositoryError>>,
  ): Promise<Result<T, RepositoryError>>;
}

// ========================================
// Repository Errors
// ========================================

export type RepositoryError = NotFoundError | OptimisticLockError | DatabaseError;

export class NotFoundError extends Error {
  constructor(public readonly aggregateId: string) {
    super(`Aggregate not found: ${aggregateId}`);
    this.name = "NotFoundError";
  }
}

export class OptimisticLockError extends Error {
  constructor(
    public readonly aggregateId: string,
    public readonly expectedVersion: number,
    public readonly actualVersion: number,
  ) {
    super(`Optimistic lock conflict for ${aggregateId}: expected version ${expectedVersion}, got ${actualVersion}`);
    this.name = "OptimisticLockError";
  }
}

export class DatabaseError extends Error {
  constructor(
    message: string,
    public readonly cause?: Error,
  ) {
    super(message);
    this.name = "DatabaseError";
  }
}
```

## TypeORM Implementation

Implement the repository using TypeORM:

```typescript
// infrastructure/persistence/typeorm/TypeORMDonationRepository.ts
import { EntityManager, Repository } from "typeorm";
import { Result, Ok, Err } from "@/shared/result";
import {
  DonationRepository,
  RepositoryError,
  NotFoundError,
  OptimisticLockError,
  DatabaseError,
} from "@/domain/repositories/DonationRepository";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { DonationEntity } from "./entities/DonationEntity";
import { DonationMapper } from "./mappers/DonationMapper";

export class TypeORMDonationRepository implements DonationRepository {
  private readonly repository: Repository<DonationEntity>;

  constructor(private readonly entityManager: EntityManager) {
    this.repository = entityManager.getRepository(DonationEntity);
  }

  // ========================================
  // Standard CRUD Operations
  // ========================================

  async save(donation: Donation): Promise<Result<void, RepositoryError>> {
    try {
      const entity = DonationMapper.toEntity(donation);

      // Check for optimistic locking
      const existing = await this.repository.findOne({
        where: { id: entity.id },
      });

      if (existing && existing.version !== entity.version - 1) {
        return Err(new OptimisticLockError(entity.id, entity.version - 1, existing.version));
      }

      await this.repository.save(entity);
      return Ok(undefined);
    } catch (error) {
      return Err(new DatabaseError("Failed to save donation", error as Error));
    }
  }

  async findById(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    try {
      const entity = await this.repository.findOne({
        where: {
          id: id.value,
          deletedAt: null, // Exclude soft-deleted
        },
        relations: ["donor", "recipient"],
      });

      if (!entity) {
        return Err(new NotFoundError(id.value));
      }

      const donation = DonationMapper.toDomain(entity);
      return Ok(donation);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donation", error as Error));
    }
  }

  async findByIdIncludingDeleted(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    try {
      const entity = await this.repository.findOne({
        where: { id: id.value },
        relations: ["donor", "recipient"],
      });

      if (!entity) {
        return Err(new NotFoundError(id.value));
      }

      const donation = DonationMapper.toDomain(entity);
      return Ok(donation);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donation", error as Error));
    }
  }

  async delete(id: DonationID): Promise<Result<void, RepositoryError>> {
    try {
      const result = await this.repository.delete({ id: id.value });

      if (result.affected === 0) {
        return Err(new NotFoundError(id.value));
      }

      return Ok(undefined);
    } catch (error) {
      return Err(new DatabaseError("Failed to delete donation", error as Error));
    }
  }

  // ========================================
  // Business Query Methods
  // ========================================

  async findByDonor(donorId: DonorID): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          donorId: donorId.value,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by donor", error as Error));
    }
  }

  async findByRecipient(recipientId: RecipientID): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          recipientId: recipientId.value,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by recipient", error as Error));
    }
  }

  async findByStatus(status: DonationStatus): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          status: status.value,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by status", error as Error));
    }
  }

  async findByDateRange(range: DateRange): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository
        .createQueryBuilder("donation")
        .where("donation.createdAt >= :start", { start: range.start })
        .andWhere("donation.createdAt <= :end", { end: range.end })
        .andWhere("donation.deletedAt IS NULL")
        .leftJoinAndSelect("donation.donor", "donor")
        .leftJoinAndSelect("donation.recipient", "recipient")
        .getMany();

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by date range", error as Error));
    }
  }

  async findPendingVerification(): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          status: "PENDING_VERIFICATION",
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
        order: { createdAt: "ASC" },
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find pending verification donations", error as Error));
    }
  }

  async getTotalAmountByDonor(donorId: DonorID): Promise<Result<Money, RepositoryError>> {
    try {
      const result = await this.repository
        .createQueryBuilder("donation")
        .select("SUM(donation.amountCents)", "total")
        .where("donation.donorId = :donorId", { donorId: donorId.value })
        .andWhere("donation.status = :status", { status: "CONFIRMED" })
        .andWhere("donation.deletedAt IS NULL")
        .getRawOne();

      const totalCents = parseInt(result?.total || "0", 10);
      const money = Money.fromCents(totalCents, "USD").unwrap(); // Assuming USD
      return Ok(money);
    } catch (error) {
      return Err(new DatabaseError("Failed to get total amount by donor", error as Error));
    }
  }

  // ========================================
  // Transaction Support
  // ========================================

  async withTransaction<T>(
    callback: (repo: DonationRepository) => Promise<Result<T, RepositoryError>>,
  ): Promise<Result<T, RepositoryError>> {
    return this.entityManager.transaction(async (transactionalEntityManager) => {
      const transactionalRepo = new TypeORMDonationRepository(transactionalEntityManager);
      return callback(transactionalRepo);
    });
  }
}
```

### TypeORM Entity Definition

```typescript
// infrastructure/persistence/typeorm/entities/DonationEntity.ts
import {
  Entity,
  PrimaryColumn,
  Column,
  CreateDateColumn,
  UpdateDateColumn,
  DeleteDateColumn,
  VersionColumn,
  ManyToOne,
  JoinColumn,
} from "typeorm";

@Entity("donations")
export class DonationEntity {
  @PrimaryColumn("uuid")
  id!: string;

  @Column("uuid")
  donorId!: string;

  @Column("uuid")
  recipientId!: string;

  @Column("bigint")
  amountCents!: number; // Store money in smallest unit (cents)

  @Column("varchar", { length: 3 })
  currency!: string; // ISO 4217 currency code

  @Column("varchar", { length: 50 })
  status!: string;

  @Column("text", { nullable: true })
  notes?: string;

  @CreateDateColumn()
  createdAt!: Date;

  @Column("uuid")
  createdBy!: string;

  @UpdateDateColumn()
  updatedAt!: Date;

  @Column("uuid")
  updatedBy!: string;

  @DeleteDateColumn({ nullable: true })
  deletedAt?: Date;

  @Column("uuid", { nullable: true })
  deletedBy?: string;

  @VersionColumn()
  version!: number;

  // Relations (optional, for query optimization)
  @ManyToOne(() => UserEntity, { nullable: true })
  @JoinColumn({ name: "donorId" })
  donor?: UserEntity;

  @ManyToOne(() => OrganizationEntity, { nullable: true })
  @JoinColumn({ name: "recipientId" })
  recipient?: OrganizationEntity;
}
```

### Domain-Entity Mapper

```typescript
// infrastructure/persistence/typeorm/mappers/DonationMapper.ts
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { DonationEntity } from "../entities/DonationEntity";
import { Money } from "@/domain/value-objects/Money";
import { DonorID } from "@/domain/value-objects/DonorID";
import { RecipientID } from "@/domain/value-objects/RecipientID";
import { DonationStatus } from "@/domain/value-objects/DonationStatus";
import { UserID } from "@/domain/value-objects/UserID";

export class DonationMapper {
  /**
   * Convert domain aggregate to database entity.
   */
  static toEntity(donation: Donation): DonationEntity {
    const entity = new DonationEntity();
    entity.id = donation.id.value;
    entity.donorId = donation.getDonorId().value;
    entity.recipientId = donation.getRecipientId().value;
    entity.amountCents = donation.getAmount().toCents();
    entity.currency = donation.getAmount().getCurrency();
    entity.status = donation.getStatus().value;
    entity.notes = donation.getNotes();
    entity.createdAt = donation.getCreatedAt();
    entity.createdBy = donation.getCreatedBy().value;
    entity.updatedAt = donation.getUpdatedAt();
    entity.updatedBy = donation.getUpdatedBy().value;
    entity.deletedAt = donation.getDeletedAt();
    entity.deletedBy = donation.getDeletedBy()?.value;
    entity.version = donation.getVersion();

    return entity;
  }

  /**
   * Convert database entity to domain aggregate.
   */
  static toDomain(entity: DonationEntity): Donation {
    const id = DonationID.create(entity.id).unwrap();
    const donorId = DonorID.create(entity.donorId).unwrap();
    const recipientId = RecipientID.create(entity.recipientId).unwrap();
    const amount = Money.fromCents(entity.amountCents, entity.currency).unwrap();
    const status = DonationStatus.create(entity.status).unwrap();
    const createdBy = UserID.create(entity.createdBy).unwrap();
    const updatedBy = UserID.create(entity.updatedBy).unwrap();

    // Use reconstitution method (assumes factory method exists)
    return Donation.reconstitute({
      id,
      donorId,
      recipientId,
      amount,
      status,
      notes: entity.notes,
      createdAt: entity.createdAt,
      createdBy,
      updatedAt: entity.updatedAt,
      updatedBy,
      deletedAt: entity.deletedAt,
      deletedBy: entity.deletedBy ? UserID.create(entity.deletedBy).unwrap() : undefined,
      version: entity.version,
    });
  }
}
```

## Prisma Implementation

Implement the repository using Prisma:

```typescript
// infrastructure/persistence/prisma/PrismaDonationRepository.ts
import { PrismaClient } from "@prisma/client";
import { Result, Ok, Err } from "@/shared/result";
import {
  DonationRepository,
  RepositoryError,
  NotFoundError,
  OptimisticLockError,
  DatabaseError,
} from "@/domain/repositories/DonationRepository";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { DonationMapper } from "./mappers/DonationMapper";

export class PrismaDonationRepository implements DonationRepository {
  constructor(private readonly prisma: PrismaClient) {}

  // ========================================
  // Standard CRUD Operations
  // ========================================

  async save(donation: Donation): Promise<Result<void, RepositoryError>> {
    try {
      const data = DonationMapper.toPrisma(donation);

      // Optimistic locking check
      const existing = await this.prisma.donation.findUnique({
        where: { id: data.id },
      });

      if (existing && existing.version !== data.version - 1) {
        return Err(new OptimisticLockError(data.id, data.version - 1, existing.version));
      }

      await this.prisma.donation.upsert({
        where: { id: data.id },
        update: data,
        create: data,
      });

      return Ok(undefined);
    } catch (error) {
      return Err(new DatabaseError("Failed to save donation", error as Error));
    }
  }

  async findById(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    try {
      const record = await this.prisma.donation.findFirst({
        where: {
          id: id.value,
          deletedAt: null,
        },
        include: {
          donor: true,
          recipient: true,
        },
      });

      if (!record) {
        return Err(new NotFoundError(id.value));
      }

      const donation = DonationMapper.toDomain(record);
      return Ok(donation);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donation", error as Error));
    }
  }

  async findByIdIncludingDeleted(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    try {
      const record = await this.prisma.donation.findUnique({
        where: { id: id.value },
        include: {
          donor: true,
          recipient: true,
        },
      });

      if (!record) {
        return Err(new NotFoundError(id.value));
      }

      const donation = DonationMapper.toDomain(record);
      return Ok(donation);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donation", error as Error));
    }
  }

  async delete(id: DonationID): Promise<Result<void, RepositoryError>> {
    try {
      await this.prisma.donation.delete({
        where: { id: id.value },
      });

      return Ok(undefined);
    } catch (error) {
      if (error.code === "P2025") {
        // Prisma error code for "Record not found"
        return Err(new NotFoundError(id.value));
      }
      return Err(new DatabaseError("Failed to delete donation", error as Error));
    }
  }

  // ========================================
  // Business Query Methods
  // ========================================

  async findByDonor(donorId: DonorID): Promise<Result<Donation[], RepositoryError>> {
    try {
      const records = await this.prisma.donation.findMany({
        where: {
          donorId: donorId.value,
          deletedAt: null,
        },
        include: {
          donor: true,
          recipient: true,
        },
      });

      const donations = records.map((r) => DonationMapper.toDomain(r));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by donor", error as Error));
    }
  }

  async findByRecipient(recipientId: RecipientID): Promise<Result<Donation[], RepositoryError>> {
    try {
      const records = await this.prisma.donation.findMany({
        where: {
          recipientId: recipientId.value,
          deletedAt: null,
        },
        include: {
          donor: true,
          recipient: true,
        },
      });

      const donations = records.map((r) => DonationMapper.toDomain(r));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by recipient", error as Error));
    }
  }

  async findByStatus(status: DonationStatus): Promise<Result<Donation[], RepositoryError>> {
    try {
      const records = await this.prisma.donation.findMany({
        where: {
          status: status.value,
          deletedAt: null,
        },
        include: {
          donor: true,
          recipient: true,
        },
      });

      const donations = records.map((r) => DonationMapper.toDomain(r));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by status", error as Error));
    }
  }

  async findByDateRange(range: DateRange): Promise<Result<Donation[], RepositoryError>> {
    try {
      const records = await this.prisma.donation.findMany({
        where: {
          createdAt: {
            gte: range.start,
            lte: range.end,
          },
          deletedAt: null,
        },
        include: {
          donor: true,
          recipient: true,
        },
      });

      const donations = records.map((r) => DonationMapper.toDomain(r));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by date range", error as Error));
    }
  }

  async findPendingVerification(): Promise<Result<Donation[], RepositoryError>> {
    try {
      const records = await this.prisma.donation.findMany({
        where: {
          status: "PENDING_VERIFICATION",
          deletedAt: null,
        },
        include: {
          donor: true,
          recipient: true,
        },
        orderBy: {
          createdAt: "asc",
        },
      });

      const donations = records.map((r) => DonationMapper.toDomain(r));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find pending verification donations", error as Error));
    }
  }

  async getTotalAmountByDonor(donorId: DonorID): Promise<Result<Money, RepositoryError>> {
    try {
      const result = await this.prisma.donation.aggregate({
        where: {
          donorId: donorId.value,
          status: "CONFIRMED",
          deletedAt: null,
        },
        _sum: {
          amountCents: true,
        },
      });

      const totalCents = result._sum.amountCents || 0;
      const money = Money.fromCents(totalCents, "USD").unwrap();
      return Ok(money);
    } catch (error) {
      return Err(new DatabaseError("Failed to get total amount by donor", error as Error));
    }
  }

  // ========================================
  // Transaction Support
  // ========================================

  async withTransaction<T>(
    callback: (repo: DonationRepository) => Promise<Result<T, RepositoryError>>,
  ): Promise<Result<T, RepositoryError>> {
    return this.prisma.$transaction(async (tx) => {
      const transactionalRepo = new PrismaDonationRepository(tx as PrismaClient);
      return callback(transactionalRepo);
    });
  }
}
```

### Prisma Schema

```prisma
// schema.prisma
generator client {
  provider = "prisma-client-js"
}

datasource db {
  provider = "postgresql"
  url      = env("DATABASE_URL")
}

model Donation {
  id           String   @id @default(uuid())
  donorId      String
  recipientId  String
  amountCents  BigInt   // Store money in smallest unit
  currency     String   @db.VarChar(3)
  status       String   @db.VarChar(50)
  notes        String?  @db.Text
  createdAt    DateTime @default(now())
  createdBy    String
  updatedAt    DateTime @updatedAt
  updatedBy    String
  deletedAt    DateTime?
  deletedBy    String?
  version      Int      @default(1)

  donor        User         @relation("DonorDonations", fields: [donorId], references: [id])
  recipient    Organization @relation("RecipientDonations", fields: [recipientId], references: [id])

  @@index([donorId])
  @@index([recipientId])
  @@index([status])
  @@index([createdAt])
  @@map("donations")
}
```

## Business Queries

Add domain-specific query methods beyond basic CRUD:

```typescript
// Additional business query examples

/**
 * Find donations exceeding a threshold amount.
 */
async findLargeDonations(
  minimumAmount: Money
): Promise<Result<Donation[], RepositoryError>> {
  try {
    const entities = await this.repository
      .createQueryBuilder("donation")
      .where("donation.amountCents >= :minAmount", {
        minAmount: minimumAmount.toCents(),
      })
      .andWhere("donation.deletedAt IS NULL")
      .leftJoinAndSelect("donation.donor", "donor")
      .leftJoinAndSelect("donation.recipient", "recipient")
      .orderBy("donation.amountCents", "DESC")
      .getMany();

    const donations = entities.map((e) => DonationMapper.toDomain(e));
    return Ok(donations);
  } catch (error) {
    return Err(
      new DatabaseError("Failed to find large donations", error as Error)
    );
  }
}

/**
 * Find donations requiring follow-up (e.g., tax receipt).
 */
async findRequiringFollowUp(): Promise<Result<Donation[], RepositoryError>> {
  try {
    const entities = await this.repository
      .createQueryBuilder("donation")
      .where("donation.status IN (:...statuses)", {
        statuses: ["CONFIRMED", "AWAITING_RECEIPT"],
      })
      .andWhere("donation.amountCents >= :threshold", {
        threshold: 10000, // $100.00 in cents
      })
      .andWhere("donation.deletedAt IS NULL")
      .leftJoinAndSelect("donation.donor", "donor")
      .leftJoinAndSelect("donation.recipient", "recipient")
      .orderBy("donation.createdAt", "ASC")
      .getMany();

    const donations = entities.map((e) => DonationMapper.toDomain(e));
    return Ok(donations);
  } catch (error) {
    return Err(
      new DatabaseError(
        "Failed to find donations requiring follow-up",
        error as Error
      )
    );
  }
}
```

## Specification Pattern

Implement the Specification pattern for complex queries:

```typescript
// domain/specifications/Specification.ts
export interface Specification<T> {
  isSatisfiedBy(candidate: T): boolean;
  and(other: Specification<T>): Specification<T>;
  or(other: Specification<T>): Specification<T>;
  not(): Specification<T>;
}

// domain/specifications/DonationSpecification.ts
import { Specification } from "./Specification";
import { Donation } from "@/domain/aggregates/Donation";

export abstract class DonationSpecification implements Specification<Donation> {
  abstract isSatisfiedBy(donation: Donation): boolean;

  and(other: DonationSpecification): DonationSpecification {
    return new AndSpecification(this, other);
  }

  or(other: DonationSpecification): DonationSpecification {
    return new OrSpecification(this, other);
  }

  not(): DonationSpecification {
    return new NotSpecification(this);
  }

  /**
   * Convert specification to database query criteria.
   * Override in concrete specifications.
   */
  abstract toQueryCriteria(): Record<string, any>;
}

// Concrete specifications

export class DonationByStatusSpecification extends DonationSpecification {
  constructor(private readonly status: DonationStatus) {
    super();
  }

  isSatisfiedBy(donation: Donation): boolean {
    return donation.getStatus().equals(this.status);
  }

  toQueryCriteria(): Record<string, any> {
    return { status: this.status.value };
  }
}

export class DonationByDonorSpecification extends DonationSpecification {
  constructor(private readonly donorId: DonorID) {
    super();
  }

  isSatisfiedBy(donation: Donation): boolean {
    return donation.getDonorId().equals(this.donorId);
  }

  toQueryCriteria(): Record<string, any> {
    return { donorId: this.donorId.value };
  }
}

export class LargeDonationSpecification extends DonationSpecification {
  constructor(private readonly minimumAmount: Money) {
    super();
  }

  isSatisfiedBy(donation: Donation): boolean {
    return donation.getAmount().isGreaterThanOrEqual(this.minimumAmount);
  }

  toQueryCriteria(): Record<string, any> {
    return {
      amountCents: { gte: this.minimumAmount.toCents() },
    };
  }
}

// Composite specifications

class AndSpecification extends DonationSpecification {
  constructor(
    private readonly left: DonationSpecification,
    private readonly right: DonationSpecification
  ) {
    super();
  }

  isSatisfiedBy(donation: Donation): boolean {
    return (
      this.left.isSatisfiedBy(donation) && this.right.isSatisfiedBy(donation)
    );
  }

  toQueryCriteria(): Record<string, any> {
    return {
      AND: [this.left.toQueryCriteria(), this.right.toQueryCriteria()],
    };
  }
}

class OrSpecification extends DonationSpecification {
  constructor(
    private readonly left: DonationSpecification,
    private readonly right: DonationSpecification
  ) {
    super();
  }

  isSatisfiedBy(donation: Donation): boolean {
    return (
      this.left.isSatisfiedBy(donation) || this.right.isSatisfiedBy(donation)
    );
  }

  toQueryCriteria(): Record<string, any> {
    return {
      OR: [this.left.toQueryCriteria(), this.right.toQueryCriteria()],
    };
  }
}

class NotSpecification extends DonationSpecification {
  constructor(private readonly spec: DonationSpecification) {
    super();
  }

  isSatisfiedBy(donation: Donation): boolean {
    return !this.spec.isSatisfiedBy(donation);
  }

  toQueryCriteria(): Record<string, any> {
    return {
      NOT: this.spec.toQueryCriteria(),
    };
  }
}

// Repository implementation with specification support

async findBySpec(
  spec: DonationSpecification
): Promise<Result<Donation[], RepositoryError>> {
  try {
    const criteria = spec.toQueryCriteria();

    const records = await this.prisma.donation.findMany({
      where: {
        ...criteria,
        deletedAt: null,
      },
      include: {
        donor: true,
        recipient: true,
      },
    });

    const donations = records.map((r) => DonationMapper.toDomain(r));
    return Ok(donations);
  } catch (error) {
    return Err(
      new DatabaseError(
        "Failed to find donations by specification",
        error as Error
      )
    );
  }
}

// Usage example

const spec = new DonationByStatusSpecification(
  DonationStatus.create("CONFIRMED").unwrap()
)
  .and(new LargeDonationSpecification(Money.fromCents(10000, "USD").unwrap()))
  .and(new DonationByDonorSpecification(donorId));

const result = await donationRepository.findBySpec(spec);
```

## Transaction Support

Implement transaction management for multi-aggregate operations:

```typescript
// Example: Process donation with transaction

async processDonation(
  donationId: DonationID,
  processedBy: UserID
): Promise<Result<void, ApplicationError>> {
  return this.donationRepository.withTransaction(async (txRepo) => {
    // 1. Load donation
    const donationResult = await txRepo.findById(donationId);
    if (donationResult.isErr()) {
      return Err(donationResult.error);
    }
    const donation = donationResult.value;

    // 2. Process donation (domain logic)
    const processResult = donation.process(processedBy);
    if (processResult.isErr()) {
      return Err(processResult.error);
    }

    // 3. Save updated donation
    const saveResult = await txRepo.save(donation);
    if (saveResult.isErr()) {
      return Err(saveResult.error);
    }

    // 4. Update donor statistics (different aggregate)
    const donorResult = await this.donorRepository.findById(
      donation.getDonorId()
    );
    if (donorResult.isErr()) {
      return Err(donorResult.error);
    }
    const donor = donorResult.value;

    donor.incrementDonationCount();
    await this.donorRepository.save(donor);

    return Ok(undefined);
  });
}
```

## Complete Example: Donation Repository

Full implementation with TypeORM:

```typescript
// Complete TypeORMDonationRepository with all features

import { EntityManager, Repository } from "typeorm";
import { Result, Ok, Err } from "@/shared/result";
import {
  DonationRepository,
  RepositoryError,
  NotFoundError,
  OptimisticLockError,
  DatabaseError,
} from "@/domain/repositories/DonationRepository";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { DonationEntity } from "./entities/DonationEntity";
import { DonationMapper } from "./mappers/DonationMapper";
import { DonationSpecification } from "@/domain/specifications/DonationSpecification";

export class TypeORMDonationRepository implements DonationRepository {
  private readonly repository: Repository<DonationEntity>;

  constructor(private readonly entityManager: EntityManager) {
    this.repository = entityManager.getRepository(DonationEntity);
  }

  async save(donation: Donation): Promise<Result<void, RepositoryError>> {
    try {
      const entity = DonationMapper.toEntity(donation);

      const existing = await this.repository.findOne({
        where: { id: entity.id },
      });

      if (existing && existing.version !== entity.version - 1) {
        return Err(new OptimisticLockError(entity.id, entity.version - 1, existing.version));
      }

      await this.repository.save(entity);
      return Ok(undefined);
    } catch (error) {
      return Err(new DatabaseError("Failed to save donation", error as Error));
    }
  }

  async findById(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    try {
      const entity = await this.repository.findOne({
        where: {
          id: id.value,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      if (!entity) {
        return Err(new NotFoundError(id.value));
      }

      const donation = DonationMapper.toDomain(entity);
      return Ok(donation);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donation", error as Error));
    }
  }

  async findByIdIncludingDeleted(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    try {
      const entity = await this.repository.findOne({
        where: { id: id.value },
        relations: ["donor", "recipient"],
      });

      if (!entity) {
        return Err(new NotFoundError(id.value));
      }

      const donation = DonationMapper.toDomain(entity);
      return Ok(donation);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donation", error as Error));
    }
  }

  async delete(id: DonationID): Promise<Result<void, RepositoryError>> {
    try {
      const result = await this.repository.delete({ id: id.value });

      if (result.affected === 0) {
        return Err(new NotFoundError(id.value));
      }

      return Ok(undefined);
    } catch (error) {
      return Err(new DatabaseError("Failed to delete donation", error as Error));
    }
  }

  async findByDonor(donorId: DonorID): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          donorId: donorId.value,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by donor", error as Error));
    }
  }

  async findByRecipient(recipientId: RecipientID): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          recipientId: recipientId.value,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by recipient", error as Error));
    }
  }

  async findByStatus(status: DonationStatus): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          status: status.value,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by status", error as Error));
    }
  }

  async findByDateRange(range: DateRange): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository
        .createQueryBuilder("donation")
        .where("donation.createdAt >= :start", { start: range.start })
        .andWhere("donation.createdAt <= :end", { end: range.end })
        .andWhere("donation.deletedAt IS NULL")
        .leftJoinAndSelect("donation.donor", "donor")
        .leftJoinAndSelect("donation.recipient", "recipient")
        .getMany();

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by date range", error as Error));
    }
  }

  async findPendingVerification(): Promise<Result<Donation[], RepositoryError>> {
    try {
      const entities = await this.repository.find({
        where: {
          status: "PENDING_VERIFICATION",
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
        order: { createdAt: "ASC" },
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find pending verification donations", error as Error));
    }
  }

  async getTotalAmountByDonor(donorId: DonorID): Promise<Result<Money, RepositoryError>> {
    try {
      const result = await this.repository
        .createQueryBuilder("donation")
        .select("SUM(donation.amountCents)", "total")
        .where("donation.donorId = :donorId", { donorId: donorId.value })
        .andWhere("donation.status = :status", { status: "CONFIRMED" })
        .andWhere("donation.deletedAt IS NULL")
        .getRawOne();

      const totalCents = parseInt(result?.total || "0", 10);
      const money = Money.fromCents(totalCents, "USD").unwrap();
      return Ok(money);
    } catch (error) {
      return Err(new DatabaseError("Failed to get total amount by donor", error as Error));
    }
  }

  async findBySpec(spec: DonationSpecification): Promise<Result<Donation[], RepositoryError>> {
    try {
      const criteria = spec.toQueryCriteria();

      const entities = await this.repository.find({
        where: {
          ...criteria,
          deletedAt: null,
        },
        relations: ["donor", "recipient"],
      });

      const donations = entities.map((e) => DonationMapper.toDomain(e));
      return Ok(donations);
    } catch (error) {
      return Err(new DatabaseError("Failed to find donations by specification", error as Error));
    }
  }

  async withTransaction<T>(
    callback: (repo: DonationRepository) => Promise<Result<T, RepositoryError>>,
  ): Promise<Result<T, RepositoryError>> {
    return this.entityManager.transaction(async (transactionalEntityManager) => {
      const transactionalRepo = new TypeORMDonationRepository(transactionalEntityManager);
      return callback(transactionalRepo);
    });
  }
}
```

## Before/After Comparison

**Before (Data Access Scattered in Services)**:

```typescript
// ❌ Bad: Domain logic mixed with database queries

class DonationService {
  constructor(private readonly entityManager: EntityManager) {}

  async processDonation(donationId: string, userId: string): Promise<void> {
    // Database query directly in service
    const donation = await this.entityManager.getRepository(DonationEntity).findOne({ where: { id: donationId } });

    if (!donation) {
      throw new Error("Donation not found");
    }

    // Domain logic mixed with persistence
    if (donation.status !== "PENDING") {
      throw new Error("Can only process pending donations");
    }

    donation.status = "PROCESSED";
    donation.processedBy = userId;
    donation.processedAt = new Date();

    await this.entityManager.save(donation);
  }
}
```

**After (Repository Abstraction)**:

```typescript
// ✅ Good: Clean separation, domain logic in aggregate

class DonationService {
  constructor(private readonly donationRepository: DonationRepository) {}

  async processDonation(donationId: DonationID, userId: UserID): Promise<Result<void, ApplicationError>> {
    // Load aggregate via repository
    const donationResult = await this.donationRepository.findById(donationId);
    if (donationResult.isErr()) {
      return Err(donationResult.error);
    }
    const donation = donationResult.value;

    // Domain logic in aggregate
    const processResult = donation.process(userId);
    if (processResult.isErr()) {
      return Err(processResult.error);
    }

    // Save via repository
    return this.donationRepository.save(donation);
  }
}
```

**Benefits**:

- Domain logic stays in domain objects
- Database implementation hidden from service
- Easier to test (mock repository interface)
- Can swap TypeORM for Prisma without changing service
- Type-safe domain operations

## Usage Guidelines

### 1. One Repository Per Aggregate

Create repositories for aggregate roots only, not for every entity:

```typescript
// ✅ Good: Repository for aggregate root
interface DonationRepository {
  save(donation: Donation): Promise<Result<void, RepositoryError>>;
}

// ❌ Bad: Repositories for child entities
interface DonationItemRepository {
  save(item: DonationItem): Promise<void>; // Child entity
}
```

### 2. Return Domain Objects

Repositories should return domain objects, not database entities:

```typescript
// ✅ Good: Returns domain aggregate
async findById(id: DonationID): Promise<Result<Donation, RepositoryError>> {
  const entity = await this.repository.findOne({ where: { id: id.value } });
  return Ok(DonationMapper.toDomain(entity));
}

// ❌ Bad: Returns database entity
async findById(id: string): Promise<DonationEntity> {
  return this.repository.findOne({ where: { id } });
}
```

### 3. Use Business Methods

Add domain-specific query methods beyond basic CRUD:

```typescript
// ✅ Good: Business-focused methods
interface DonationRepository {
  findPendingVerification(): Promise<Result<Donation[], RepositoryError>>;
  getTotalAmountByDonor(donorId: DonorID): Promise<Result<Money, RepositoryError>>;
  findRequiringFollowUp(): Promise<Result<Donation[], RepositoryError>>;
}

// ❌ Avoid: Generic query builders in domain
interface DonationRepository {
  query(sql: string): Promise<any[]>; // Too generic
}
```

### 4. Handle Optimistic Locking

Implement version checking for concurrent updates:

```typescript
async save(donation: Donation): Promise<Result<void, RepositoryError>> {
  const entity = DonationMapper.toEntity(donation);

  // Check version
  const existing = await this.repository.findOne({ where: { id: entity.id } });
  if (existing && existing.version !== entity.version - 1) {
    return Err(
      new OptimisticLockError(entity.id, entity.version - 1, existing.version)
    );
  }

  await this.repository.save(entity);
  return Ok(undefined);
}
```

### 5. Use Transactions for Multi-Aggregate Operations

Wrap operations affecting multiple aggregates in transactions:

```typescript
async transferFunds(
  fromAccount: AccountID,
  toAccount: AccountID,
  amount: Money
): Promise<Result<void, ApplicationError>> {
  return this.accountRepository.withTransaction(async (txRepo) => {
    const fromResult = await txRepo.findById(fromAccount);
    if (fromResult.isErr()) return Err(fromResult.error);

    const toResult = await txRepo.findById(toAccount);
    if (toResult.isErr()) return Err(toResult.error);

    const withdrawResult = fromResult.value.withdraw(amount);
    if (withdrawResult.isErr()) return Err(withdrawResult.error);

    const depositResult = toResult.value.deposit(amount);
    if (depositResult.isErr()) return Err(depositResult.error);

    await txRepo.save(fromResult.value);
    await txRepo.save(toResult.value);

    return Ok(undefined);
  });
}
```

## Testing Repositories

### Unit Testing with Mocks

```typescript
// tests/unit/services/DonationService.test.ts
import { describe, it, expect, vi } from "vitest";
import { DonationService } from "@/application/services/DonationService";
import { DonationRepository } from "@/domain/repositories/DonationRepository";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { Ok, Err } from "@/shared/result";

describe("DonationService", () => {
  it("should process donation successfully", async () => {
    // Mock repository
    const mockRepository: DonationRepository = {
      findById: vi.fn().mockResolvedValue(
        Ok(
          Donation.reconstitute({
            id: DonationID.create("donation-1").unwrap(),
            status: DonationStatus.create("PENDING").unwrap(),
            // ... other fields
          }),
        ),
      ),
      save: vi.fn().mockResolvedValue(Ok(undefined)),
      // ... other methods
    };

    const service = new DonationService(mockRepository);

    const result = await service.processDonation(
      DonationID.create("donation-1").unwrap(),
      UserID.create("user-1").unwrap(),
    );

    expect(result.isOk()).toBe(true);
    expect(mockRepository.save).toHaveBeenCalledTimes(1);
  });

  it("should return error when donation not found", async () => {
    const mockRepository: DonationRepository = {
      findById: vi.fn().mockResolvedValue(Err(new NotFoundError("donation-1"))),
      // ... other methods
    };

    const service = new DonationService(mockRepository);

    const result = await service.processDonation(
      DonationID.create("donation-1").unwrap(),
      UserID.create("user-1").unwrap(),
    );

    expect(result.isErr()).toBe(true);
    expect(result.error).toBeInstanceOf(NotFoundError);
  });
});
```

### Integration Testing with Real Database

```typescript
// tests/integration/repositories/TypeORMDonationRepository.test.ts
import { describe, it, expect, beforeAll, afterAll, beforeEach } from "vitest";
import { DataSource } from "typeorm";
import { TypeORMDonationRepository } from "@/infrastructure/persistence/typeorm/TypeORMDonationRepository";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { Money } from "@/domain/value-objects/Money";

describe("TypeORMDonationRepository Integration", () => {
  let dataSource: DataSource;
  let repository: TypeORMDonationRepository;

  beforeAll(async () => {
    // Setup test database
    dataSource = new DataSource({
      type: "postgres",
      host: "localhost",
      port: 5432,
      username: "test",
      password: "test",
      database: "test_db",
      entities: [DonationEntity],
      synchronize: true,
    });

    await dataSource.initialize();
    repository = new TypeORMDonationRepository(dataSource.manager);
  });

  afterAll(async () => {
    await dataSource.destroy();
  });

  beforeEach(async () => {
    // Clear database before each test
    await dataSource.manager.delete(DonationEntity, {});
  });

  it("should save and retrieve donation", async () => {
    // Create donation
    const donation = Donation.create(
      DonationID.create("donation-1").unwrap(),
      DonorID.create("donor-1").unwrap(),
      RecipientID.create("recipient-1").unwrap(),
      Money.fromCents(10000, "USD").unwrap(),
      UserID.create("user-1").unwrap(),
    ).unwrap();

    // Save
    const saveResult = await repository.save(donation);
    expect(saveResult.isOk()).toBe(true);

    // Retrieve
    const findResult = await repository.findById(donation.id);
    expect(findResult.isOk()).toBe(true);

    const retrieved = findResult.value;
    expect(retrieved.id.equals(donation.id)).toBe(true);
    expect(retrieved.getAmount().equals(donation.getAmount())).toBe(true);
  });

  it("should detect optimistic lock conflicts", async () => {
    // Create and save donation
    const donation = Donation.create(
      DonationID.create("donation-1").unwrap(),
      DonorID.create("donor-1").unwrap(),
      RecipientID.create("recipient-1").unwrap(),
      Money.fromCents(10000, "USD").unwrap(),
      UserID.create("user-1").unwrap(),
    ).unwrap();

    await repository.save(donation);

    // Load twice
    const donation1 = (await repository.findById(donation.id)).unwrap();
    const donation2 = (await repository.findById(donation.id)).unwrap();

    // Update first instance
    donation1.confirm(UserID.create("user-1").unwrap());
    await repository.save(donation1);

    // Try to update second instance (should fail)
    donation2.confirm(UserID.create("user-2").unwrap());
    const result = await repository.save(donation2);

    expect(result.isErr()).toBe(true);
    expect(result.error).toBeInstanceOf(OptimisticLockError);
  });

  it("should find donations by donor", async () => {
    const donorId = DonorID.create("donor-1").unwrap();

    // Create multiple donations
    const donation1 = Donation.create(
      DonationID.create("donation-1").unwrap(),
      donorId,
      RecipientID.create("recipient-1").unwrap(),
      Money.fromCents(10000, "USD").unwrap(),
      UserID.create("user-1").unwrap(),
    ).unwrap();

    const donation2 = Donation.create(
      DonationID.create("donation-2").unwrap(),
      donorId,
      RecipientID.create("recipient-2").unwrap(),
      Money.fromCents(20000, "USD").unwrap(),
      UserID.create("user-1").unwrap(),
    ).unwrap();

    await repository.save(donation1);
    await repository.save(donation2);

    // Find by donor
    const result = await repository.findByDonor(donorId);
    expect(result.isOk()).toBe(true);
    expect(result.value).toHaveLength(2);
  });

  it("should handle transactions correctly", async () => {
    const donation = Donation.create(
      DonationID.create("donation-1").unwrap(),
      DonorID.create("donor-1").unwrap(),
      RecipientID.create("recipient-1").unwrap(),
      Money.fromCents(10000, "USD").unwrap(),
      UserID.create("user-1").unwrap(),
    ).unwrap();

    // Transaction that succeeds
    const result1 = await repository.withTransaction(async (txRepo) => {
      await txRepo.save(donation);
      return Ok(undefined);
    });

    expect(result1.isOk()).toBe(true);

    const found1 = await repository.findById(donation.id);
    expect(found1.isOk()).toBe(true);

    // Transaction that fails (rollback)
    const result2 = await repository.withTransaction(async (txRepo) => {
      const donation2 = Donation.create(
        DonationID.create("donation-2").unwrap(),
        DonorID.create("donor-2").unwrap(),
        RecipientID.create("recipient-2").unwrap(),
        Money.fromCents(20000, "USD").unwrap(),
        UserID.create("user-1").unwrap(),
      ).unwrap();

      await txRepo.save(donation2);

      // Simulate error
      return Err(new DatabaseError("Simulated error"));
    });

    expect(result2.isErr()).toBe(true);

    // donation-2 should not be saved
    const found2 = await repository.findById(DonationID.create("donation-2").unwrap());
    expect(found2.isErr()).toBe(true);
  });
});
```

### Testing with In-Memory Repository

```typescript
// tests/helpers/InMemoryDonationRepository.ts
import { Result, Ok, Err } from "@/shared/result";
import {
  DonationRepository,
  RepositoryError,
  NotFoundError,
  OptimisticLockError,
} from "@/domain/repositories/DonationRepository";
import { Donation, DonationID } from "@/domain/aggregates/Donation";

export class InMemoryDonationRepository implements DonationRepository {
  private donations: Map<string, Donation> = new Map();

  async save(donation: Donation): Promise<Result<void, RepositoryError>> {
    const existing = this.donations.get(donation.id.value);

    if (existing && existing.getVersion() !== donation.getVersion() - 1) {
      return Err(new OptimisticLockError(donation.id.value, donation.getVersion() - 1, existing.getVersion()));
    }

    this.donations.set(donation.id.value, donation);
    return Ok(undefined);
  }

  async findById(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    const donation = this.donations.get(id.value);

    if (!donation || donation.getDeletedAt()) {
      return Err(new NotFoundError(id.value));
    }

    return Ok(donation);
  }

  async findByIdIncludingDeleted(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    const donation = this.donations.get(id.value);

    if (!donation) {
      return Err(new NotFoundError(id.value));
    }

    return Ok(donation);
  }

  async delete(id: DonationID): Promise<Result<void, RepositoryError>> {
    if (!this.donations.has(id.value)) {
      return Err(new NotFoundError(id.value));
    }

    this.donations.delete(id.value);
    return Ok(undefined);
  }

  async findByDonor(donorId: DonorID): Promise<Result<Donation[], RepositoryError>> {
    const donations = Array.from(this.donations.values()).filter(
      (d) => d.getDonorId().equals(donorId) && !d.getDeletedAt(),
    );

    return Ok(donations);
  }

  // ... implement other methods

  async withTransaction<T>(
    callback: (repo: DonationRepository) => Promise<Result<T, RepositoryError>>,
  ): Promise<Result<T, RepositoryError>> {
    // In-memory implementation: just call callback with self
    return callback(this);
  }

  // Test helpers
  clear(): void {
    this.donations.clear();
  }

  size(): number {
    return this.donations.size;
  }
}
```

## Related Documentation

- [Entity Template](./entity-template.md) - Creating domain entities
- [Aggregate Template](./aggregate-template.md) - Aggregate root patterns
- [Service Layer Template](./service-layer-template.md) - Application services using repositories
- [TypeScript Domain-Driven Design](../ex-so-prla-ts__domain-driven-design.md) - DDD principles
- [TypeScript Best Practices](../ex-so-prla-ts__best-practices.md) - TypeScript coding standards

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
**TypeORM Version**: 0.3+
**Prisma Version**: 5.0+
