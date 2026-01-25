---
title: TypeScript Service Layer Template
description: Template for creating application service layer in TypeScript with orchestration, transaction management, event publishing, and dependency injection using NestJS or manual DI
category: template
tags:
  - typescript
  - ddd
  - service-layer
  - application-services
  - orchestration
  - transactions
  - domain-events
  - dependency-injection
  - error-handling
  - nestjs
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.9
related:
  - repository-template.md
  - domain-event-template.md
  - aggregate-template.md
  - ex-so-prla-ts__domain-driven-design.md
  - ex-so-prla-ts__best-practices.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript Service Layer Template

This template provides a standardized structure for creating application service layers in TypeScript. The service layer orchestrates domain logic, coordinates repositories, manages transactions, and publishes domain events to implement business use cases.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Orchestration Layer](#orchestration-layer)
4. [Transaction Management](#transaction-management)
5. [Event Publishing](#event-publishing)
6. [Dependency Injection](#dependency-injection)
7. [Error Handling](#error-handling)
8. [Complete Example: ProcessDonationService](#complete-example-processdonationservice)
9. [NestJS Integration](#nestjs-integration)
10. [Before/After Comparison](#beforeafter-comparison)
11. [Usage Guidelines](#usage-guidelines)
12. [Testing Services](#testing-services)
13. [Related Documentation](#related-documentation)

## Overview

The service layer (also called **application services**) sits between the presentation layer and the domain layer. Services orchestrate business use cases by coordinating domain objects, repositories, and external services without containing business logic themselves.

**Key Characteristics**:

- **Orchestration focused**: Coordinates domain logic, doesn't contain it
- **Use case implementation**: One method per use case
- **Transaction boundaries**: Manages transaction lifecycle
- **Event publishing**: Publishes domain events after successful transactions
- **Thin layer**: Minimal logic, delegates to domain objects
- **Result type error handling**: Explicit error handling with Result<T, E>

**TypeScript vs Java/Go Comparison**:

```typescript
// TypeScript: Constructor injection with explicit dependencies
export class ProcessDonationService {
  constructor(
    private readonly donationRepository: DonationRepository,
    private readonly accountRepository: AccountRepository,
    private readonly eventPublisher: EventPublisher,
    private readonly logger: Logger,
  ) {}

  // Use case method returning Result type
  async execute(request: ProcessDonationRequest): Promise<Result<ProcessDonationResponse, ApplicationError>> {
    // Orchestration logic
  }
}
```

```java
// Java: Class with @Service annotation and dependency injection
@Service
public class ProcessDonationService {
    private final DonationRepository donationRepository;
    private final AccountRepository accountRepository;
    private final EventPublisher eventPublisher;
    private final Logger logger;

    @Autowired
    public ProcessDonationService(
        DonationRepository donationRepository,
        AccountRepository accountRepository,
        EventPublisher eventPublisher,
        Logger logger
    ) {
        this.donationRepository = donationRepository;
        this.accountRepository = accountRepository;
        this.eventPublisher = eventPublisher;
        this.logger = logger;
    }

    @Transactional
    public ProcessDonationResponse execute(ProcessDonationRequest request) {
        // Orchestration logic
    }
}
```

```go
// Go: Constructor function with explicit dependencies
type ProcessDonationService struct {
    donationRepo    domain.DonationRepository
    accountRepo     domain.AccountRepository
    eventPublisher  event.EventPublisher
    logger          Logger
}

func NewProcessDonationService(
    donationRepo domain.DonationRepository,
    accountRepo domain.AccountRepository,
    eventPublisher event.EventPublisher,
    logger Logger,
) *ProcessDonationService {
    return &ProcessDonationService{
        donationRepo:   donationRepo,
        accountRepo:    accountRepo,
        eventPublisher: eventPublisher,
        logger:         logger,
    }
}

// Use case method with context.Context
func (s *ProcessDonationService) Execute(
    ctx context.Context,
    request ProcessDonationRequest,
) (*ProcessDonationResponse, error) {
    // Orchestration logic
}
```

**Critical Differences**:

- TypeScript uses **constructor injection** (manual or with NestJS/InversifyJS)
- TypeScript uses **async/await** for asynchronous operations
- TypeScript returns **Result type** instead of throwing exceptions
- TypeScript has **no automatic transaction management** like Java's @Transactional
- TypeScript services manage **transactions explicitly** via repository methods
- TypeScript uses **class-based services** (not struct-based like Go)

## Template Structure

```typescript
// application/services/ProcessDonationService.ts
import { Result, Ok, Err } from "@/shared/result";
import { Injectable } from "@nestjs/common"; // Optional: NestJS DI
import { DonationRepository } from "@/domain/repositories/DonationRepository";
import { AccountRepository } from "@/domain/repositories/AccountRepository";
import { EventPublisher } from "@/infrastructure/events/EventPublisher";
import { Logger } from "@/infrastructure/logging/Logger";

/**
 * ProcessDonationService orchestrates donation processing use cases.
 *
 * Responsibilities:
 *   - Load aggregates from repositories
 *   - Coordinate domain operations
 *   - Manage transaction boundaries
 *   - Publish domain events
 *   - Handle cross-aggregate consistency
 *
 * This service does NOT contain business logic. Business rules belong in
 * domain entities and value objects.
 *
 * @example
 * const service = new ProcessDonationService(
 *   donationRepo,
 *   accountRepo,
 *   eventPublisher,
 *   logger
 * );
 *
 * const result = await service.execute({
 *   donationId: "donation-123",
 *   processedBy: "user-456",
 * });
 *
 * if (result.isErr()) {
 *   logger.error(result.error);
 *   return;
 * }
 *
 * const response = result.value;
 */
@Injectable() // Optional: NestJS decorator
export class ProcessDonationService {
  constructor(
    private readonly donationRepository: DonationRepository,
    private readonly accountRepository: AccountRepository,
    private readonly eventPublisher: EventPublisher,
    private readonly logger: Logger,
  ) {}

  /**
   * Execute processes a donation.
   *
   * Transaction boundary: All operations within this method execute atomically.
   * Domain events are published after successful transaction commit.
   */
  async execute(request: ProcessDonationRequest): Promise<Result<ProcessDonationResponse, ApplicationError>> {
    this.logger.info("Processing donation", { request });

    // Use transaction for atomic operations
    return this.donationRepository.withTransaction(async (txRepo) => {
      // 1. Load donation aggregate
      const donationResult = await txRepo.findById(request.donationId);
      if (donationResult.isErr()) {
        return Err(new DonationNotFoundError(request.donationId));
      }
      const donation = donationResult.value;

      // 2. Execute domain operation
      const processResult = donation.process(request.processedBy);
      if (processResult.isErr()) {
        return Err(new DonationProcessingError(processResult.error));
      }

      // 3. Save aggregate
      const saveResult = await txRepo.save(donation);
      if (saveResult.isErr()) {
        return Err(new PersistenceError(saveResult.error));
      }

      // 4. Publish domain events (after successful save)
      await this.publishEvents(donation.getDomainEvents());

      // 5. Build response
      const response: ProcessDonationResponse = {
        donationId: donation.id,
        status: donation.getStatus(),
        processedAt: new Date(),
      };

      this.logger.info("Donation processed successfully", { response });
      return Ok(response);
    });
  }

  private async publishEvents(events: DomainEvent[]): Promise<void> {
    for (const event of events) {
      await this.eventPublisher.publish(event);
    }
  }
}

// ========================================
// Request/Response DTOs
// ========================================

export interface ProcessDonationRequest {
  donationId: DonationID;
  processedBy: UserID;
}

export interface ProcessDonationResponse {
  donationId: DonationID;
  status: DonationStatus;
  processedAt: Date;
}

// ========================================
// Application Errors
// ========================================

export type ApplicationError = DonationNotFoundError | DonationProcessingError | PersistenceError;

export class DonationNotFoundError extends Error {
  constructor(public readonly donationId: DonationID) {
    super(`Donation not found: ${donationId.value}`);
    this.name = "DonationNotFoundError";
  }
}

export class DonationProcessingError extends Error {
  constructor(public readonly cause: Error) {
    super(`Failed to process donation: ${cause.message}`);
    this.name = "DonationProcessingError";
  }
}

export class PersistenceError extends Error {
  constructor(public readonly cause: Error) {
    super(`Failed to persist changes: ${cause.message}`);
    this.name = "PersistenceError";
  }
}
```

## Orchestration Layer

Services coordinate domain operations without implementing business logic:

```typescript
// ========================================
// Good Example: Orchestration Only
// ========================================

export class TransferFundsService {
  constructor(
    private readonly accountRepository: AccountRepository,
    private readonly eventPublisher: EventPublisher,
  ) {}

  async execute(request: TransferFundsRequest): Promise<Result<TransferFundsResponse, ApplicationError>> {
    return this.accountRepository.withTransaction(async (txRepo) => {
      // 1. Load both accounts
      const fromAccountResult = await txRepo.findById(request.fromAccountId);
      if (fromAccountResult.isErr()) {
        return Err(new AccountNotFoundError(request.fromAccountId));
      }

      const toAccountResult = await txRepo.findById(request.toAccountId);
      if (toAccountResult.isErr()) {
        return Err(new AccountNotFoundError(request.toAccountId));
      }

      const fromAccount = fromAccountResult.value;
      const toAccount = toAccountResult.value;

      // 2. Execute domain operations (business logic in domain objects)
      const withdrawResult = fromAccount.withdraw(request.amount, request.initiatedBy);
      if (withdrawResult.isErr()) {
        return Err(new InsufficientFundsError(withdrawResult.error));
      }

      const depositResult = toAccount.deposit(request.amount, request.initiatedBy);
      if (depositResult.isErr()) {
        return Err(new DepositFailedError(depositResult.error));
      }

      // 3. Save both aggregates
      await txRepo.save(fromAccount);
      await txRepo.save(toAccount);

      // 4. Publish events
      await this.publishEvents([...fromAccount.getDomainEvents(), ...toAccount.getDomainEvents()]);

      // 5. Return response
      return Ok({
        fromAccountId: fromAccount.id,
        toAccountId: toAccount.id,
        amount: request.amount,
        transferredAt: new Date(),
      });
    });
  }
}

// ========================================
// Bad Example: Business Logic in Service
// ========================================

export class TransferFundsServiceBad {
  async execute(request: TransferFundsRequest): Promise<void> {
    const fromAccount = await this.accountRepository.findById(request.fromAccountId);
    const toAccount = await this.accountRepository.findById(request.toAccountId);

    // ❌ BAD: Business logic in service layer
    if (fromAccount.balance.toCents() < request.amount.toCents()) {
      throw new Error("Insufficient funds");
    }

    // ❌ BAD: Direct property manipulation
    fromAccount.balance = fromAccount.balance.subtract(request.amount);
    toAccount.balance = toAccount.balance.add(request.amount);

    await this.accountRepository.save(fromAccount);
    await this.accountRepository.save(toAccount);
  }
}
```

**Key Principle**: Services **orchestrate**, domain objects **execute**.

## Transaction Management

Manage transactions explicitly using repository transaction methods:

```typescript
// ========================================
// Transaction Pattern with Repository
// ========================================

export class CreateDonationService {
  constructor(
    private readonly donationRepository: DonationRepository,
    private readonly accountRepository: AccountRepository,
    private readonly eventPublisher: EventPublisher,
  ) {}

  async execute(request: CreateDonationRequest): Promise<Result<CreateDonationResponse, ApplicationError>> {
    // Start transaction via repository
    return this.donationRepository.withTransaction(async (donationTxRepo) => {
      // 1. Create donation aggregate
      const donationResult = Donation.create(
        DonationID.generate(),
        request.donorId,
        request.recipientId,
        request.amount,
        request.createdBy,
      );

      if (donationResult.isErr()) {
        return Err(new DonationCreationError(donationResult.error));
      }

      const donation = donationResult.value;

      // 2. Save donation (within transaction)
      const saveDonationResult = await donationTxRepo.save(donation);
      if (saveDonationResult.isErr()) {
        return Err(new PersistenceError(saveDonationResult.error));
      }

      // 3. Update donor account (different aggregate, same transaction)
      // Note: Need transaction-aware account repository
      const accountResult = await this.accountRepository.findById(request.donorId);
      if (accountResult.isErr()) {
        return Err(new AccountNotFoundError(request.donorId));
      }

      const account = accountResult.value;
      account.recordDonation(donation.id, request.amount);

      const saveAccountResult = await this.accountRepository.save(account);
      if (saveAccountResult.isErr()) {
        // Transaction will rollback automatically
        return Err(new PersistenceError(saveAccountResult.error));
      }

      // 4. Publish events (after successful commit)
      await this.publishEvents([...donation.getDomainEvents(), ...account.getDomainEvents()]);

      return Ok({
        donationId: donation.id,
        createdAt: donation.getCreatedAt(),
      });
    });
  }
}

// ========================================
// Manual Transaction Management (Advanced)
// ========================================

export class ComplexTransactionService {
  constructor(
    private readonly dataSource: DataSource, // TypeORM DataSource
  ) {}

  async execute(request: ComplexRequest): Promise<Result<ComplexResponse, ApplicationError>> {
    const queryRunner = this.dataSource.createQueryRunner();
    await queryRunner.connect();
    await queryRunner.startTransaction();

    try {
      // Create transaction-aware repositories
      const donationRepo = new TypeORMDonationRepository(queryRunner.manager);
      const accountRepo = new TypeORMAccountRepository(queryRunner.manager);

      // Perform operations...
      const donation = await donationRepo.findById(request.donationId);
      // ... business operations

      // Commit transaction
      await queryRunner.commitTransaction();

      return Ok({ success: true });
    } catch (error) {
      // Rollback on error
      await queryRunner.rollbackTransaction();
      return Err(new TransactionError(error));
    } finally {
      await queryRunner.release();
    }
  }
}
```

## Event Publishing

Publish domain events after successful transactions:

```typescript
// ========================================
// Event Publisher Interface
// ========================================

export interface EventPublisher {
  publish(event: DomainEvent): Promise<void>;
  publishAll(events: DomainEvent[]): Promise<void>;
}

// ========================================
// In-Memory Event Publisher (Simple)
// ========================================

export class InMemoryEventPublisher implements EventPublisher {
  private handlers: Map<string, Array<(event: DomainEvent) => Promise<void>>> = new Map();

  async publish(event: DomainEvent): Promise<void> {
    const handlers = this.handlers.get(event.eventType) || [];
    await Promise.all(handlers.map((handler) => handler(event)));
  }

  async publishAll(events: DomainEvent[]): Promise<void> {
    await Promise.all(events.map((event) => this.publish(event)));
  }

  subscribe(eventType: string, handler: (event: DomainEvent) => Promise<void>): void {
    if (!this.handlers.has(eventType)) {
      this.handlers.set(eventType, []);
    }
    this.handlers.get(eventType)!.push(handler);
  }
}

// ========================================
// Service with Event Publishing
// ========================================

export class ConfirmDonationService {
  constructor(
    private readonly donationRepository: DonationRepository,
    private readonly eventPublisher: EventPublisher,
    private readonly logger: Logger,
  ) {}

  async execute(request: ConfirmDonationRequest): Promise<Result<ConfirmDonationResponse, ApplicationError>> {
    return this.donationRepository.withTransaction(async (txRepo) => {
      // 1. Load and confirm donation
      const donationResult = await txRepo.findById(request.donationId);
      if (donationResult.isErr()) {
        return Err(new DonationNotFoundError(request.donationId));
      }

      const donation = donationResult.value;
      const confirmResult = donation.confirm(request.confirmedBy);
      if (confirmResult.isErr()) {
        return Err(new DonationConfirmationError(confirmResult.error));
      }

      // 2. Save donation
      await txRepo.save(donation);

      // 3. Collect and publish domain events
      const events = donation.getDomainEvents();

      this.logger.info("Publishing domain events", {
        count: events.length,
        types: events.map((e) => e.eventType),
      });

      // Publish after transaction commit
      await this.eventPublisher.publishAll(events);

      // Clear events from aggregate (prevent duplicate publishing)
      donation.clearDomainEvents();

      return Ok({
        donationId: donation.id,
        status: donation.getStatus(),
        confirmedAt: new Date(),
      });
    });
  }
}

// ========================================
// Event Handlers (Subscribers)
// ========================================

export class SendDonationReceiptHandler {
  constructor(
    private readonly emailService: EmailService,
    private readonly logger: Logger,
  ) {}

  async handle(event: DonationConfirmedEvent): Promise<void> {
    this.logger.info("Sending donation receipt", {
      donationId: event.aggregateId,
    });

    try {
      await this.emailService.sendDonationReceipt({
        donorId: event.donorId,
        donationId: event.aggregateId,
        amount: event.amount,
        confirmedAt: event.occurredAt,
      });

      this.logger.info("Donation receipt sent successfully");
    } catch (error) {
      this.logger.error("Failed to send donation receipt", { error });
      // Note: Event handler failures don't rollback the transaction
    }
  }
}

// Register event handlers
const eventPublisher = new InMemoryEventPublisher();
const receiptHandler = new SendDonationReceiptHandler(emailService, logger);

eventPublisher.subscribe("DonationConfirmed", async (event) => {
  await receiptHandler.handle(event as DonationConfirmedEvent);
});
```

## Dependency Injection

### Manual Dependency Injection

```typescript
// ========================================
// Service Factory (Manual DI)
// ========================================

export class ServiceFactory {
  constructor(
    private readonly dataSource: DataSource,
    private readonly eventPublisher: EventPublisher,
    private readonly logger: Logger,
  ) {}

  createProcessDonationService(): ProcessDonationService {
    const donationRepository = new TypeORMDonationRepository(this.dataSource.manager);
    const accountRepository = new TypeORMAccountRepository(this.dataSource.manager);

    return new ProcessDonationService(donationRepository, accountRepository, this.eventPublisher, this.logger);
  }

  createTransferFundsService(): TransferFundsService {
    const accountRepository = new TypeORMAccountRepository(this.dataSource.manager);

    return new TransferFundsService(accountRepository, this.eventPublisher, this.logger);
  }
}

// Usage
const factory = new ServiceFactory(dataSource, eventPublisher, logger);
const processDonationService = factory.createProcessDonationService();

const result = await processDonationService.execute(request);
```

### NestJS Dependency Injection

```typescript
// ========================================
// NestJS Module Setup
// ========================================

import { Module } from "@nestjs/common";
import { TypeOrmModule } from "@nestjs/typeorm";

@Module({
  imports: [TypeOrmModule.forFeature([DonationEntity, AccountEntity])],
  providers: [
    // Repositories
    {
      provide: "DonationRepository",
      useClass: TypeORMDonationRepository,
    },
    {
      provide: "AccountRepository",
      useClass: TypeORMAccountRepository,
    },
    // Event Publisher
    {
      provide: "EventPublisher",
      useClass: InMemoryEventPublisher,
    },
    // Services
    ProcessDonationService,
    TransferFundsService,
    ConfirmDonationService,
  ],
  exports: [ProcessDonationService, TransferFundsService, ConfirmDonationService],
})
export class DonationModule {}

// ========================================
// Service with NestJS DI
// ========================================

import { Injectable, Inject } from "@nestjs/common";

@Injectable()
export class ProcessDonationService {
  constructor(
    @Inject("DonationRepository")
    private readonly donationRepository: DonationRepository,
    @Inject("AccountRepository")
    private readonly accountRepository: AccountRepository,
    @Inject("EventPublisher")
    private readonly eventPublisher: EventPublisher,
    private readonly logger: Logger,
  ) {}

  // ... service methods
}
```

## Error Handling

Use Result type for explicit error handling:

```typescript
// ========================================
// Result Type Definition
// ========================================

export type Result<T, E> = Ok<T> | Err<E>;

export class Ok<T> {
  constructor(public readonly value: T) {}

  isOk(): this is Ok<T> {
    return true;
  }

  isErr(): this is Err<never> {
    return false;
  }

  unwrap(): T {
    return this.value;
  }

  unwrapOr(defaultValue: T): T {
    return this.value;
  }
}

export class Err<E> {
  constructor(public readonly error: E) {}

  isOk(): this is Ok<never> {
    return false;
  }

  isErr(): this is Err<E> {
    return true;
  }

  unwrap(): never {
    throw new Error(`Cannot unwrap Err: ${this.error}`);
  }

  unwrapOr<T>(defaultValue: T): T {
    return defaultValue;
  }
}

// ========================================
// Service Error Handling
// ========================================

export class ProcessDonationService {
  async execute(request: ProcessDonationRequest): Promise<Result<ProcessDonationResponse, ApplicationError>> {
    // Load donation
    const donationResult = await this.donationRepository.findById(request.donationId);

    // Handle repository error
    if (donationResult.isErr()) {
      this.logger.error("Failed to load donation", {
        donationId: request.donationId,
        error: donationResult.error,
      });

      return Err(new DonationNotFoundError(request.donationId));
    }

    const donation = donationResult.value;

    // Handle domain error
    const processResult = donation.process(request.processedBy);
    if (processResult.isErr()) {
      this.logger.error("Domain operation failed", {
        error: processResult.error,
      });

      return Err(new DonationProcessingError(processResult.error));
    }

    // Handle persistence error
    const saveResult = await this.donationRepository.save(donation);
    if (saveResult.isErr()) {
      this.logger.error("Failed to save donation", {
        error: saveResult.error,
      });

      return Err(new PersistenceError(saveResult.error));
    }

    // Success
    return Ok({
      donationId: donation.id,
      status: donation.getStatus(),
      processedAt: new Date(),
    });
  }
}

// ========================================
// Controller Error Mapping
// ========================================

export class DonationController {
  constructor(private readonly processDonationService: ProcessDonationService) {}

  async processDonation(req: Request, res: Response): Promise<void> {
    const result = await this.processDonationService.execute({
      donationId: DonationID.create(req.params.id).unwrap(),
      processedBy: UserID.create(req.user.id).unwrap(),
    });

    if (result.isErr()) {
      // Map application errors to HTTP status codes
      if (result.error instanceof DonationNotFoundError) {
        res.status(404).json({ error: "Donation not found" });
        return;
      }

      if (result.error instanceof DonationProcessingError) {
        res.status(422).json({ error: result.error.message });
        return;
      }

      if (result.error instanceof PersistenceError) {
        res.status(500).json({ error: "Internal server error" });
        return;
      }

      // Default
      res.status(500).json({ error: "Unknown error" });
      return;
    }

    // Success
    res.status(200).json(result.value);
  }
}
```

## Complete Example: ProcessDonationService

Full service implementation with all patterns:

```typescript
// ========================================
// application/services/ProcessDonationService.ts
// ========================================

import { Injectable, Inject } from "@nestjs/common";
import { Result, Ok, Err } from "@/shared/result";
import { DonationRepository } from "@/domain/repositories/DonationRepository";
import { AccountRepository } from "@/domain/repositories/AccountRepository";
import { EventPublisher } from "@/infrastructure/events/EventPublisher";
import { Logger } from "@/infrastructure/logging/Logger";
import { DonationID } from "@/domain/value-objects/DonationID";
import { UserID } from "@/domain/value-objects/UserID";
import { DonationStatus } from "@/domain/value-objects/DonationStatus";

/**
 * ProcessDonationService orchestrates donation processing workflow.
 *
 * Use Case: Process a pending donation
 *
 * Steps:
 *   1. Load donation aggregate
 *   2. Execute donation.process() domain operation
 *   3. Load donor account aggregate
 *   4. Update account donation statistics
 *   5. Save both aggregates
 *   6. Publish domain events
 *
 * Transaction Boundary: All operations are atomic
 *
 * @example
 * const result = await service.execute({
 *   donationId: DonationID.create("donation-123").unwrap(),
 *   processedBy: UserID.create("user-456").unwrap(),
 * });
 *
 * if (result.isOk()) {
 *   console.log("Donation processed:", result.value);
 * } else {
 *   console.error("Processing failed:", result.error);
 * }
 */
@Injectable()
export class ProcessDonationService {
  constructor(
    @Inject("DonationRepository")
    private readonly donationRepository: DonationRepository,
    @Inject("AccountRepository")
    private readonly accountRepository: AccountRepository,
    @Inject("EventPublisher")
    private readonly eventPublisher: EventPublisher,
    private readonly logger: Logger,
  ) {}

  /**
   * Execute processes a donation.
   */
  async execute(request: ProcessDonationRequest): Promise<Result<ProcessDonationResponse, ProcessDonationError>> {
    this.logger.info("Processing donation", {
      donationId: request.donationId.value,
      processedBy: request.processedBy.value,
    });

    // Start transaction
    return this.donationRepository.withTransaction(async (donationTxRepo) => {
      // ========================================
      // Step 1: Load Donation
      // ========================================

      const donationResult = await donationTxRepo.findById(request.donationId);
      if (donationResult.isErr()) {
        this.logger.error("Donation not found", {
          donationId: request.donationId.value,
        });
        return Err(new DonationNotFoundError(request.donationId));
      }

      const donation = donationResult.value;

      // ========================================
      // Step 2: Execute Domain Operation
      // ========================================

      const processResult = donation.process(request.processedBy);
      if (processResult.isErr()) {
        this.logger.error("Domain operation failed", {
          error: processResult.error.message,
        });
        return Err(new DomainOperationError(processResult.error));
      }

      // ========================================
      // Step 3: Load Donor Account
      // ========================================

      const accountResult = await this.accountRepository.findById(donation.getDonorId());
      if (accountResult.isErr()) {
        this.logger.error("Donor account not found", {
          donorId: donation.getDonorId().value,
        });
        return Err(new AccountNotFoundError(donation.getDonorId()));
      }

      const account = accountResult.value;

      // ========================================
      // Step 4: Update Account Statistics
      // ========================================

      account.recordProcessedDonation(donation.id, donation.getAmount());

      // ========================================
      // Step 5: Save Aggregates
      // ========================================

      const saveDonationResult = await donationTxRepo.save(donation);
      if (saveDonationResult.isErr()) {
        this.logger.error("Failed to save donation", {
          error: saveDonationResult.error,
        });
        return Err(new PersistenceError(saveDonationResult.error));
      }

      const saveAccountResult = await this.accountRepository.save(account);
      if (saveAccountResult.isErr()) {
        this.logger.error("Failed to save account", {
          error: saveAccountResult.error,
        });
        return Err(new PersistenceError(saveAccountResult.error));
      }

      // ========================================
      // Step 6: Publish Domain Events
      // ========================================

      const allEvents = [...donation.getDomainEvents(), ...account.getDomainEvents()];

      this.logger.info("Publishing domain events", {
        count: allEvents.length,
        types: allEvents.map((e) => e.eventType),
      });

      await this.eventPublisher.publishAll(allEvents);

      // Clear events from aggregates
      donation.clearDomainEvents();
      account.clearDomainEvents();

      // ========================================
      // Step 7: Build Response
      // ========================================

      const response: ProcessDonationResponse = {
        donationId: donation.id,
        status: donation.getStatus(),
        amount: donation.getAmount(),
        processedAt: new Date(),
        accountBalance: account.getBalance(),
      };

      this.logger.info("Donation processed successfully", {
        donationId: donation.id.value,
        status: donation.getStatus().value,
      });

      return Ok(response);
    });
  }
}

// ========================================
// Request/Response DTOs
// ========================================

export interface ProcessDonationRequest {
  donationId: DonationID;
  processedBy: UserID;
}

export interface ProcessDonationResponse {
  donationId: DonationID;
  status: DonationStatus;
  amount: Money;
  processedAt: Date;
  accountBalance: Money;
}

// ========================================
// Application Errors
// ========================================

export type ProcessDonationError =
  | DonationNotFoundError
  | AccountNotFoundError
  | DomainOperationError
  | PersistenceError;

export class DonationNotFoundError extends Error {
  constructor(public readonly donationId: DonationID) {
    super(`Donation not found: ${donationId.value}`);
    this.name = "DonationNotFoundError";
  }
}

export class AccountNotFoundError extends Error {
  constructor(public readonly accountId: AccountID) {
    super(`Account not found: ${accountId.value}`);
    this.name = "AccountNotFoundError";
  }
}

export class DomainOperationError extends Error {
  constructor(public readonly cause: Error) {
    super(`Domain operation failed: ${cause.message}`);
    this.name = "DomainOperationError";
  }
}

export class PersistenceError extends Error {
  constructor(public readonly cause: Error) {
    super(`Failed to persist changes: ${cause.message}`);
    this.name = "PersistenceError";
  }
}
```

## NestJS Integration

Complete NestJS module with controllers and services:

```typescript
// ========================================
// app/modules/donation/donation.module.ts
// ========================================

import { Module } from "@nestjs/common";
import { TypeOrmModule } from "@nestjs/typeorm";
import { DonationEntity } from "@/infrastructure/persistence/typeorm/entities/DonationEntity";
import { AccountEntity } from "@/infrastructure/persistence/typeorm/entities/AccountEntity";
import { TypeORMDonationRepository } from "@/infrastructure/persistence/typeorm/TypeORMDonationRepository";
import { TypeORMAccountRepository } from "@/infrastructure/persistence/typeorm/TypeORMAccountRepository";
import { InMemoryEventPublisher } from "@/infrastructure/events/InMemoryEventPublisher";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { CreateDonationService } from "@/application/services/CreateDonationService";
import { DonationController } from "@/presentation/controllers/DonationController";

@Module({
  imports: [TypeOrmModule.forFeature([DonationEntity, AccountEntity])],
  providers: [
    // Repositories
    {
      provide: "DonationRepository",
      useClass: TypeORMDonationRepository,
    },
    {
      provide: "AccountRepository",
      useClass: TypeORMAccountRepository,
    },
    // Event Publisher
    {
      provide: "EventPublisher",
      useClass: InMemoryEventPublisher,
    },
    // Application Services
    ProcessDonationService,
    CreateDonationService,
  ],
  controllers: [DonationController],
  exports: [ProcessDonationService, CreateDonationService],
})
export class DonationModule {}

// ========================================
// presentation/controllers/DonationController.ts
// ========================================

import { Controller, Post, Param, Body, HttpStatus, HttpException } from "@nestjs/common";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { CreateDonationService } from "@/application/services/CreateDonationService";
import { DonationID } from "@/domain/value-objects/DonationID";
import { UserID } from "@/domain/value-objects/UserID";

@Controller("donations")
export class DonationController {
  constructor(
    private readonly processDonationService: ProcessDonationService,
    private readonly createDonationService: CreateDonationService,
  ) {}

  @Post(":id/process")
  async processDonation(@Param("id") id: string, @Body("processedBy") processedBy: string) {
    const result = await this.processDonationService.execute({
      donationId: DonationID.create(id).unwrap(),
      processedBy: UserID.create(processedBy).unwrap(),
    });

    if (result.isErr()) {
      throw new HttpException(result.error.message, this.mapErrorToStatus(result.error));
    }

    return result.value;
  }

  @Post()
  async createDonation(@Body() body: CreateDonationDTO) {
    const result = await this.createDonationService.execute({
      donorId: DonorID.create(body.donorId).unwrap(),
      recipientId: RecipientID.create(body.recipientId).unwrap(),
      amount: Money.fromCents(body.amountCents, body.currency).unwrap(),
      createdBy: UserID.create(body.createdBy).unwrap(),
    });

    if (result.isErr()) {
      throw new HttpException(result.error.message, this.mapErrorToStatus(result.error));
    }

    return result.value;
  }

  private mapErrorToStatus(error: Error): number {
    if (error.name === "DonationNotFoundError") {
      return HttpStatus.NOT_FOUND;
    }
    if (error.name === "DomainOperationError") {
      return HttpStatus.UNPROCESSABLE_ENTITY;
    }
    if (error.name === "PersistenceError") {
      return HttpStatus.INTERNAL_SERVER_ERROR;
    }
    return HttpStatus.BAD_REQUEST;
  }
}

interface CreateDonationDTO {
  donorId: string;
  recipientId: string;
  amountCents: number;
  currency: string;
  createdBy: string;
}
```

## Before/After Comparison

**Before (Anemic Domain Model)**:

```typescript
// ❌ Bad: Service contains business logic

export class DonationService {
  constructor(private readonly entityManager: EntityManager) {}

  async processDonation(donationId: string, userId: string): Promise<void> {
    // Load donation entity (anemic)
    const donation = await this.entityManager.getRepository(DonationEntity).findOne({ where: { id: donationId } });

    if (!donation) {
      throw new Error("Donation not found");
    }

    // ❌ Business logic in service
    if (donation.status !== "PENDING") {
      throw new Error("Can only process pending donations");
    }

    if (donation.amountCents < 100) {
      throw new Error("Amount too small to process");
    }

    // ❌ Direct property manipulation
    donation.status = "PROCESSED";
    donation.processedBy = userId;
    donation.processedAt = new Date();

    await this.entityManager.save(donation);

    // ❌ External concerns mixed in
    await this.sendEmail(donation.donorEmail, "Your donation was processed");
  }

  private async sendEmail(to: string, message: string): Promise<void> {
    // Email logic in service
  }
}
```

**After (Rich Domain Model)**:

```typescript
// ✅ Good: Service orchestrates, domain executes

export class ProcessDonationService {
  constructor(
    private readonly donationRepository: DonationRepository,
    private readonly eventPublisher: EventPublisher,
  ) {}

  async execute(request: ProcessDonationRequest): Promise<Result<ProcessDonationResponse, ApplicationError>> {
    return this.donationRepository.withTransaction(async (txRepo) => {
      // Load aggregate
      const donationResult = await txRepo.findById(request.donationId);
      if (donationResult.isErr()) {
        return Err(new DonationNotFoundError(request.donationId));
      }

      const donation = donationResult.value;

      // ✅ Business logic in domain
      const processResult = donation.process(request.processedBy);
      if (processResult.isErr()) {
        return Err(new DomainOperationError(processResult.error));
      }

      // Save aggregate
      await txRepo.save(donation);

      // ✅ Events for side effects
      await this.eventPublisher.publishAll(donation.getDomainEvents());

      return Ok({
        donationId: donation.id,
        status: donation.getStatus(),
        processedAt: new Date(),
      });
    });
  }
}

// ✅ Email handled by event subscriber (decoupled)
export class SendDonationEmailHandler {
  async handle(event: DonationProcessedEvent): Promise<void> {
    await this.emailService.send({
      to: event.donorEmail,
      subject: "Donation Processed",
      body: "Your donation was processed successfully",
    });
  }
}
```

**Benefits**:

- Business logic in domain objects (single source of truth)
- Service focuses on orchestration
- Side effects handled via events (decoupled)
- Easier to test (mock repository interface)
- Type-safe domain operations

## Usage Guidelines

### 1. One Method Per Use Case

Each service method represents one business use case:

```typescript
// ✅ Good: One method per use case
export class DonationService {
  async processDonation(request: ProcessDonationRequest): Promise<Result<ProcessDonationResponse, ApplicationError>> {
    // Implementation
  }

  async cancelDonation(request: CancelDonationRequest): Promise<Result<CancelDonationResponse, ApplicationError>> {
    // Implementation
  }
}

// ❌ Bad: Generic CRUD methods
export class DonationService {
  async update(id: string, data: any): Promise<void> {
    // Too generic, unclear intent
  }
}
```

### 2. Keep Services Thin

Services should orchestrate, not implement:

```typescript
// ✅ Good: Thin orchestration
async execute(
  request: ProcessDonationRequest
): Promise<Result<ProcessDonationResponse, ApplicationError>> {
  const donation = await this.donationRepository.findById(request.donationId);
  const result = donation.process(request.processedBy); // Domain logic
  await this.donationRepository.save(donation);
  return Ok({ donationId: donation.id });
}

// ❌ Bad: Business logic in service
async execute(request: ProcessDonationRequest): Promise<void> {
  const donation = await this.donationRepository.findById(request.donationId);

  // ❌ Business logic belongs in domain
  if (donation.status !== "PENDING") {
    throw new Error("Invalid status");
  }

  donation.status = "PROCESSED";
  donation.processedAt = new Date();

  await this.donationRepository.save(donation);
}
```

### 3. Use Transactions for Multi-Aggregate Operations

Wrap operations affecting multiple aggregates:

```typescript
// ✅ Good: Transaction wraps multi-aggregate operation
async transferFunds(
  request: TransferFundsRequest
): Promise<Result<TransferFundsResponse, ApplicationError>> {
  return this.accountRepository.withTransaction(async (txRepo) => {
    const fromAccount = await txRepo.findById(request.fromAccountId);
    const toAccount = await txRepo.findById(request.toAccountId);

    fromAccount.withdraw(request.amount);
    toAccount.deposit(request.amount);

    await txRepo.save(fromAccount);
    await txRepo.save(toAccount);

    return Ok({ success: true });
  });
}
```

### 4. Publish Events After Successful Transactions

Publish domain events only after successful commit:

```typescript
async execute(
  request: ProcessDonationRequest
): Promise<Result<ProcessDonationResponse, ApplicationError>> {
  return this.donationRepository.withTransaction(async (txRepo) => {
    // ... business operations

    await txRepo.save(donation);

    // ✅ Publish events after successful save
    await this.eventPublisher.publishAll(donation.getDomainEvents());

    return Ok(response);
  });
}
```

### 5. Return Result Type

Use Result type for explicit error handling:

```typescript
// ✅ Good: Result type
async execute(
  request: ProcessDonationRequest
): Promise<Result<ProcessDonationResponse, ApplicationError>> {
  const result = await this.donationRepository.findById(request.donationId);

  if (result.isErr()) {
    return Err(new DonationNotFoundError(request.donationId));
  }

  // ...
  return Ok(response);
}

// ❌ Bad: Throwing exceptions
async execute(request: ProcessDonationRequest): Promise<ProcessDonationResponse> {
  const donation = await this.donationRepository.findById(request.donationId);

  if (!donation) {
    throw new Error("Donation not found"); // Hidden error path
  }

  // ...
  return response;
}
```

## Testing Services

### Unit Testing with Mocks

```typescript
// tests/unit/services/ProcessDonationService.test.ts
import { describe, it, expect, vi, beforeEach } from "vitest";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { DonationRepository } from "@/domain/repositories/DonationRepository";
import { AccountRepository } from "@/domain/repositories/AccountRepository";
import { EventPublisher } from "@/infrastructure/events/EventPublisher";
import { Logger } from "@/infrastructure/logging/Logger";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { Ok, Err } from "@/shared/result";

describe("ProcessDonationService", () => {
  let service: ProcessDonationService;
  let mockDonationRepository: jest.Mocked<DonationRepository>;
  let mockAccountRepository: jest.Mocked<AccountRepository>;
  let mockEventPublisher: jest.Mocked<EventPublisher>;
  let mockLogger: jest.Mocked<Logger>;

  beforeEach(() => {
    mockDonationRepository = {
      findById: vi.fn(),
      save: vi.fn(),
      withTransaction: vi.fn((callback) => callback(mockDonationRepository)),
    } as any;

    mockAccountRepository = {
      findById: vi.fn(),
      save: vi.fn(),
    } as any;

    mockEventPublisher = {
      publishAll: vi.fn(),
    } as any;

    mockLogger = {
      info: vi.fn(),
      error: vi.fn(),
    } as any;

    service = new ProcessDonationService(mockDonationRepository, mockAccountRepository, mockEventPublisher, mockLogger);
  });

  it("should process donation successfully", async () => {
    // Arrange
    const donationId = DonationID.create("donation-1").unwrap();
    const processedBy = UserID.create("user-1").unwrap();

    const mockDonation = {
      id: donationId,
      process: vi.fn().mockReturnValue(Ok(undefined)),
      getDonorId: vi.fn().mockReturnValue(DonorID.create("donor-1").unwrap()),
      getStatus: vi.fn().mockReturnValue(DonationStatus.create("PROCESSED").unwrap()),
      getAmount: vi.fn().mockReturnValue(Money.fromCents(10000, "USD").unwrap()),
      getDomainEvents: vi.fn().mockReturnValue([]),
      clearDomainEvents: vi.fn(),
    };

    const mockAccount = {
      recordProcessedDonation: vi.fn(),
      getBalance: vi.fn().mockReturnValue(Money.fromCents(100000, "USD").unwrap()),
      getDomainEvents: vi.fn().mockReturnValue([]),
      clearDomainEvents: vi.fn(),
    };

    mockDonationRepository.findById.mockResolvedValue(Ok(mockDonation as any));
    mockDonationRepository.save.mockResolvedValue(Ok(undefined));
    mockAccountRepository.findById.mockResolvedValue(Ok(mockAccount as any));
    mockAccountRepository.save.mockResolvedValue(Ok(undefined));

    // Act
    const result = await service.execute({
      donationId,
      processedBy,
    });

    // Assert
    expect(result.isOk()).toBe(true);
    expect(mockDonation.process).toHaveBeenCalledWith(processedBy);
    expect(mockDonationRepository.save).toHaveBeenCalledWith(mockDonation);
    expect(mockAccountRepository.save).toHaveBeenCalledWith(mockAccount);
    expect(mockEventPublisher.publishAll).toHaveBeenCalled();
  });

  it("should return error when donation not found", async () => {
    // Arrange
    const donationId = DonationID.create("donation-1").unwrap();
    const processedBy = UserID.create("user-1").unwrap();

    mockDonationRepository.findById.mockResolvedValue(Err(new NotFoundError(donationId.value)));

    // Act
    const result = await service.execute({
      donationId,
      processedBy,
    });

    // Assert
    expect(result.isErr()).toBe(true);
    expect(result.error).toBeInstanceOf(DonationNotFoundError);
    expect(mockDonationRepository.save).not.toHaveBeenCalled();
    expect(mockEventPublisher.publishAll).not.toHaveBeenCalled();
  });

  it("should return error when domain operation fails", async () => {
    // Arrange
    const donationId = DonationID.create("donation-1").unwrap();
    const processedBy = UserID.create("user-1").unwrap();

    const mockDonation = {
      id: donationId,
      process: vi.fn().mockReturnValue(Err(new Error("Cannot process completed donation"))),
      getDonorId: vi.fn().mockReturnValue(DonorID.create("donor-1").unwrap()),
    };

    mockDonationRepository.findById.mockResolvedValue(Ok(mockDonation as any));

    // Act
    const result = await service.execute({
      donationId,
      processedBy,
    });

    // Assert
    expect(result.isErr()).toBe(true);
    expect(result.error).toBeInstanceOf(DomainOperationError);
    expect(mockDonationRepository.save).not.toHaveBeenCalled();
  });
});
```

### Integration Testing

```typescript
// tests/integration/services/ProcessDonationService.integration.test.ts
import { describe, it, expect, beforeAll, afterAll, beforeEach } from "vitest";
import { DataSource } from "typeorm";
import { ProcessDonationService } from "@/application/services/ProcessDonationService";
import { TypeORMDonationRepository } from "@/infrastructure/persistence/typeorm/TypeORMDonationRepository";
import { TypeORMAccountRepository } from "@/infrastructure/persistence/typeorm/TypeORMAccountRepository";
import { InMemoryEventPublisher } from "@/infrastructure/events/InMemoryEventPublisher";
import { Donation, DonationID } from "@/domain/aggregates/Donation";

describe("ProcessDonationService Integration", () => {
  let dataSource: DataSource;
  let service: ProcessDonationService;
  let donationRepository: TypeORMDonationRepository;
  let accountRepository: TypeORMAccountRepository;

  beforeAll(async () => {
    dataSource = new DataSource({
      type: "postgres",
      host: "localhost",
      port: 5432,
      username: "test",
      password: "test",
      database: "test_db",
      entities: [DonationEntity, AccountEntity],
      synchronize: true,
    });

    await dataSource.initialize();

    donationRepository = new TypeORMDonationRepository(dataSource.manager);
    accountRepository = new TypeORMAccountRepository(dataSource.manager);

    service = new ProcessDonationService(donationRepository, accountRepository, new InMemoryEventPublisher(), console);
  });

  afterAll(async () => {
    await dataSource.destroy();
  });

  beforeEach(async () => {
    await dataSource.manager.delete(DonationEntity, {});
    await dataSource.manager.delete(AccountEntity, {});
  });

  it("should process donation end-to-end", async () => {
    // Create test data
    const donorId = DonorID.create("donor-1").unwrap();
    const account = Account.create(donorId, Money.fromCents(100000, "USD").unwrap()).unwrap();
    await accountRepository.save(account);

    const donation = Donation.create(
      DonationID.create("donation-1").unwrap(),
      donorId,
      RecipientID.create("recipient-1").unwrap(),
      Money.fromCents(10000, "USD").unwrap(),
      UserID.create("user-1").unwrap(),
    ).unwrap();
    await donationRepository.save(donation);

    // Execute service
    const result = await service.execute({
      donationId: donation.id,
      processedBy: UserID.create("user-2").unwrap(),
    });

    // Assert
    expect(result.isOk()).toBe(true);

    const processedDonation = (await donationRepository.findById(donation.id)).unwrap();
    expect(processedDonation.getStatus().value).toBe("PROCESSED");

    const updatedAccount = (await accountRepository.findById(donorId)).unwrap();
    // Assert account statistics updated
  });
});
```

## Related Documentation

- [Repository Template](./repository-template.md) - Repository pattern implementation
- [Aggregate Template](./aggregate-template.md) - Domain aggregate patterns
- [Domain Event Template](./domain-event-template.md) - Event publishing patterns
- [TypeScript Domain-Driven Design](../ex-so-prla-ts__domain-driven-design.md) - DDD principles
- [TypeScript Best Practices](../ex-so-prla-ts__best-practices.md) - TypeScript coding standards

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
**NestJS Version**: 10.0+ (optional)
