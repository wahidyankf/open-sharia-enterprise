---
title: TypeScript Project Structure Template
description: Standard TypeScript project layout following hexagonal architecture and clean code principles with clear separation of domain, application, and infrastructure layers
category: template
tags:
  - typescript
  - project-structure
  - hexagonal-architecture
  - clean-architecture
  - ddd
  - organization
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.9
related:
  - entity-template.md
  - repository-template.md
  - service-layer-template.md
  - ex-so-prla-ts__best-practices.md
  - ex-so-prla-ts__domain-driven-design.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-24
updated: 2026-01-24
---

# TypeScript Project Structure Template

Standard TypeScript project layout following hexagonal architecture and Domain-Driven Design principles.

## Directory Structure

```
donation-service/
├── src/
│   ├── domain/                      # Pure domain layer (no external dependencies)
│   │   ├── aggregates/              # Aggregate roots
│   │   │   ├── Donation.ts
│   │   │   ├── Account.ts
│   │   │   └── index.ts
│   │   ├── entities/                # Entities (part of aggregates)
│   │   │   ├── DonationItem.ts
│   │   │   └── index.ts
│   │   ├── value-objects/           # Value objects
│   │   │   ├── Money.ts
│   │   │   ├── DonationID.ts
│   │   │   ├── Email.ts
│   │   │   └── index.ts
│   │   ├── events/                  # Domain events
│   │   │   ├── DomainEvent.ts
│   │   │   ├── DonationCreatedEvent.ts
│   │   │   └── index.ts
│   │   ├── specifications/          # Specification pattern
│   │   │   ├── DonationSpecification.ts
│   │   │   └── index.ts
│   │   ├── repositories/            # Repository interfaces (domain contracts)
│   │   │   ├── DonationRepository.ts
│   │   │   ├── AccountRepository.ts
│   │   │   └── index.ts
│   │   └── services/                # Domain services (business logic)
│   │       ├── ZakatCalculationService.ts
│   │       └── index.ts
│   ├── application/                 # Application layer (orchestration)
│   │   ├── services/                # Application services (use cases)
│   │   │   ├── ProcessDonationService.ts
│   │   │   ├── CreateDonationService.ts
│   │   │   └── index.ts
│   │   ├── dto/                     # Data Transfer Objects
│   │   │   ├── ProcessDonationRequest.ts
│   │   │   ├── ProcessDonationResponse.ts
│   │   │   └── index.ts
│   │   └── errors/                  # Application-specific errors
│   │       ├── ApplicationError.ts
│   │       ├── DonationNotFoundError.ts
│   │       └── index.ts
│   ├── infrastructure/              # Infrastructure layer (external concerns)
│   │   ├── persistence/             # Database implementations
│   │   │   ├── typeorm/
│   │   │   │   ├── entities/
│   │   │   │   │   ├── DonationEntity.ts
│   │   │   │   │   └── index.ts
│   │   │   │   ├── mappers/
│   │   │   │   │   ├── DonationMapper.ts
│   │   │   │   │   └── index.ts
│   │   │   │   ├── repositories/
│   │   │   │   │   ├── TypeORMDonationRepository.ts
│   │   │   │   │   └── index.ts
│   │   │   │   ├── migrations/
│   │   │   │   │   └── 1234567890-CreateDonationTable.ts
│   │   │   │   └── data-source.ts
│   │   │   └── prisma/
│   │   │       ├── schema.prisma
│   │   │       └── PrismaDonationRepository.ts
│   │   ├── http/                    # HTTP server and controllers
│   │   │   ├── controllers/
│   │   │   │   ├── DonationController.ts
│   │   │   │   └── index.ts
│   │   │   ├── middleware/
│   │   │   │   ├── errorHandler.ts
│   │   │   │   ├── authentication.ts
│   │   │   │   └── index.ts
│   │   │   ├── routes/
│   │   │   │   ├── donationRoutes.ts
│   │   │   │   └── index.ts
│   │   │   └── server.ts
│   │   ├── events/                  # Event infrastructure
│   │   │   ├── EventPublisher.ts
│   │   │   ├── InMemoryEventPublisher.ts
│   │   │   └── index.ts
│   │   ├── logging/                 # Logging infrastructure
│   │   │   ├── Logger.ts
│   │   │   ├── WinstonLogger.ts
│   │   │   └── index.ts
│   │   └── config/                  # Configuration management
│   │       ├── AppConfig.ts
│   │       ├── DatabaseConfig.ts
│   │       └── index.ts
│   ├── shared/                      # Shared utilities (cross-cutting)
│   │   ├── result/                  # Result type
│   │   │   ├── Result.ts
│   │   │   ├── Ok.ts
│   │   │   ├── Err.ts
│   │   │   └── index.ts
│   │   ├── utils/                   # Utility functions
│   │   │   ├── uuid.ts
│   │   │   ├── date.ts
│   │   │   └── index.ts
│   │   └── types/                   # Common types
│   │       ├── branded-types.ts
│   │       └── index.ts
│   └── index.ts                     # Application entry point
├── tests/                           # Tests mirror src/ structure
│   ├── unit/
│   │   ├── domain/
│   │   │   ├── aggregates/
│   │   │   │   └── Donation.test.ts
│   │   │   └── value-objects/
│   │   │       └── Money.test.ts
│   │   ├── application/
│   │   │   └── services/
│   │   │       └── ProcessDonationService.test.ts
│   │   └── infrastructure/
│   │       └── http/
│   │           └── DonationController.test.ts
│   ├── integration/
│   │   ├── repositories/
│   │   │   └── DonationRepository.integration.test.ts
│   │   └── http/
│   │       └── DonationAPI.integration.test.ts
│   ├── e2e/
│   │   └── donation-workflow.e2e.test.ts
│   ├── helpers/                     # Test utilities
│   │   ├── InMemoryDonationRepository.ts
│   │   ├── fixtures.ts
│   │   └── index.ts
│   └── setup.ts                     # Global test setup
├── tsconfig.json                    # Base TypeScript configuration
├── tsconfig.build.json              # Production build config
├── tsconfig.test.json               # Test configuration
├── eslint.config.js                 # ESLint 9.x flat config
├── jest.config.js                   # Jest configuration
├── vitest.config.ts                 # Vitest configuration (alternative)
├── .prettierrc.json                 # Prettier config
├── .prettierignore
├── .gitignore
├── .dockerignore
├── Dockerfile                       # Multi-stage Docker build
├── docker-compose.yml               # Local development environment
├── package.json
├── package-lock.json
└── README.md
```

## Key Directories Explained

### `src/domain/`

**Pure domain layer** - No external dependencies, only business logic.

**Characteristics**:

- No imports from infrastructure or application layers
- No database, HTTP, or framework dependencies
- Pure TypeScript/JavaScript code
- Business rules and domain models

**Example**:

```typescript
// src/domain/aggregates/Donation.ts
import { Result, Ok, Err } from "@/shared/result";
import { DonationID } from "@/domain/value-objects/DonationID";
import { Money } from "@/domain/value-objects/Money";
import { DonationStatus } from "@/domain/value-objects/DonationStatus";

export class Donation {
  private constructor(
    public readonly id: DonationID,
    private amount: Money,
    private status: DonationStatus,
    private readonly createdAt: Date,
    private updatedAt: Date,
    private version: number,
  ) {}

  static create(id: DonationID, amount: Money, createdBy: UserID): Result<Donation, ValidationError> {
    // Pure domain logic, no external dependencies
    return Ok(new Donation(id, amount, DonationStatus.create("PENDING").unwrap(), new Date(), new Date(), 1));
  }

  process(processedBy: UserID): Result<void, DomainError> {
    // Business rules
    if (!this.status.equals(DonationStatus.create("PENDING").unwrap())) {
      return Err(new DomainError("Cannot process non-pending donation"));
    }

    this.status = DonationStatus.create("PROCESSED").unwrap();
    this.updatedAt = new Date();
    this.version++;

    return Ok(undefined);
  }
}
```

### `src/application/`

**Application layer** - Orchestrates domain logic, implements use cases.

**Characteristics**:

- Depends on domain layer
- No dependency on infrastructure details
- Defines application service interfaces
- Implements use case workflows

**Example**:

```typescript
// src/application/services/ProcessDonationService.ts
import { Result, Ok, Err } from "@/shared/result";
import { DonationRepository } from "@/domain/repositories/DonationRepository";
import { EventPublisher } from "@/infrastructure/events/EventPublisher";

export class ProcessDonationService {
  constructor(
    private readonly donationRepository: DonationRepository,
    private readonly eventPublisher: EventPublisher,
  ) {}

  async execute(request: ProcessDonationRequest): Promise<Result<ProcessDonationResponse, ApplicationError>> {
    return this.donationRepository.withTransaction(async (txRepo) => {
      const donation = await txRepo.findById(request.donationId);
      const result = donation.process(request.processedBy);
      await txRepo.save(donation);
      await this.eventPublisher.publishAll(donation.getDomainEvents());

      return Ok({ donationId: donation.id });
    });
  }
}
```

### `src/infrastructure/`

**Infrastructure layer** - External system adapters.

**Characteristics**:

- Database implementations (TypeORM, Prisma)
- HTTP server and controllers
- Event publishers
- Logging
- External service integrations

**Example**:

```typescript
// src/infrastructure/persistence/typeorm/repositories/TypeORMDonationRepository.ts
import { Repository, EntityManager } from "typeorm";
import { DonationRepository } from "@/domain/repositories/DonationRepository";
import { Donation } from "@/domain/aggregates/Donation";
import { DonationEntity } from "../entities/DonationEntity";
import { DonationMapper } from "../mappers/DonationMapper";

export class TypeORMDonationRepository implements DonationRepository {
  private readonly repository: Repository<DonationEntity>;

  constructor(entityManager: EntityManager) {
    this.repository = entityManager.getRepository(DonationEntity);
  }

  async save(donation: Donation): Promise<Result<void, RepositoryError>> {
    const entity = DonationMapper.toEntity(donation);
    await this.repository.save(entity);
    return Ok(undefined);
  }

  async findById(id: DonationID): Promise<Result<Donation, RepositoryError>> {
    const entity = await this.repository.findOne({ where: { id: id.value } });
    if (!entity) return Err(new NotFoundError(id.value));
    return Ok(DonationMapper.toDomain(entity));
  }
}
```

### `src/shared/`

**Shared utilities** - Cross-cutting concerns used across layers.

**Characteristics**:

- Result type (Ok/Err)
- UUID generation
- Date utilities
- Common types

**Example**:

```typescript
// src/shared/result/Result.ts
export type Result<T, E> = Ok<T> | Err<E>;

export class Ok<T> {
  constructor(public readonly value: T) {}

  isOk(): this is Ok<T> {
    return true;
  }

  isErr(): this is Err<never> {
    return false;
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
}
```

### `tests/`

**Tests mirror src/ structure** - Unit, integration, and E2E tests.

**Characteristics**:

- Unit tests: Test individual components in isolation
- Integration tests: Test component interactions with real dependencies
- E2E tests: Test complete workflows
- Test helpers: Shared test utilities and fixtures

**Example**:

```typescript
// tests/unit/domain/aggregates/Donation.test.ts
import { describe, it, expect } from "vitest";
import { Donation, DonationID } from "@/domain/aggregates/Donation";
import { Money } from "@/domain/value-objects/Money";

describe("Donation", () => {
  describe("create", () => {
    it("should create a donation with pending status", () => {
      const id = DonationID.create("donation-1").unwrap();
      const amount = Money.fromCents(10000, "USD").unwrap();
      const createdBy = UserID.create("user-1").unwrap();

      const result = Donation.create(id, amount, createdBy);

      expect(result.isOk()).toBe(true);
      const donation = result.unwrap();
      expect(donation.getStatus().value).toBe("PENDING");
    });
  });

  describe("process", () => {
    it("should process pending donation", () => {
      const donation = createPendingDonation();
      const processedBy = UserID.create("user-2").unwrap();

      const result = donation.process(processedBy);

      expect(result.isOk()).toBe(true);
      expect(donation.getStatus().value).toBe("PROCESSED");
    });

    it("should fail to process non-pending donation", () => {
      const donation = createProcessedDonation();
      const processedBy = UserID.create("user-2").unwrap();

      const result = donation.process(processedBy);

      expect(result.isErr()).toBe(true);
    });
  });
});
```

## Best Practices

### 1. Follow Hexagonal Architecture

**Domain layer** (core) → **Application layer** (use cases) → **Infrastructure layer** (adapters)

Dependencies flow inward:

```
Infrastructure → Application → Domain
```

**Domain layer**:

- ✅ Can depend on: Nothing (pure business logic)
- ❌ Cannot depend on: Application, Infrastructure

**Application layer**:

- ✅ Can depend on: Domain
- ❌ Cannot depend on: Infrastructure (only interfaces)

**Infrastructure layer**:

- ✅ Can depend on: Domain, Application
- ✅ Implements: Domain interfaces (repositories)

### 2. Use Barrel Exports

Create `index.ts` files for cleaner imports:

```typescript
// src/domain/value-objects/index.ts
export * from "./Money";
export * from "./DonationID";
export * from "./Email";

// Usage
import { Money, DonationID, Email } from "@/domain/value-objects";
```

### 3. Separate Tests by Type

```
tests/
├── unit/        # Fast, isolated tests (no external dependencies)
├── integration/ # Tests with real database, HTTP
└── e2e/         # Full workflow tests
```

### 4. Keep Domain Pure

Domain layer should have zero external dependencies:

```typescript
// ✅ Good: Pure domain code
export class Money {
  private constructor(
    private readonly amount: number,
    private readonly currency: string,
  ) {}

  add(other: Money): Result<Money, DomainError> {
    // Pure logic, no external calls
  }
}

// ❌ Bad: Domain with infrastructure dependency
import { EntityManager } from "typeorm"; // WRONG!

export class Money {
  async save(entityManager: EntityManager): Promise<void> {
    // Domain should not know about persistence
  }
}
```

### 5. Use Dependency Injection

Pass dependencies through constructors:

```typescript
// ✅ Good: Explicit dependencies
export class ProcessDonationService {
  constructor(
    private readonly donationRepository: DonationRepository,
    private readonly eventPublisher: EventPublisher,
  ) {}
}

// ❌ Bad: Hidden dependencies
export class ProcessDonationService {
  execute() {
    const repo = new TypeORMDonationRepository(); // Hidden dependency
  }
}
```

### 6. Co-locate Tests with Source

Alternative structure (tests next to source):

```
src/
├── domain/
│   ├── aggregates/
│   │   ├── Donation.ts
│   │   └── Donation.test.ts    # Test next to implementation
│   └── value-objects/
│       ├── Money.ts
│       └── Money.test.ts
```

Both approaches are valid. Choose one and be consistent.

## Common Mistakes

### ❌ Mistake 1: Domain Depends on Infrastructure

```typescript
// WRONG: Domain importing from infrastructure
import { TypeORMDonationRepository } from "@/infrastructure/persistence";

export class Donation {
  async save(): Promise<void> {
    const repo = new TypeORMDonationRepository();
    await repo.save(this);
  }
}
```

**Fix**: Keep domain pure, use repository interfaces.

### ❌ Mistake 2: Deep Nesting

```typescript
// WRONG: Too much nesting
src / infrastructure / persistence / database / repositories / typeorm / implementations / DonationRepository.ts;
```

**Fix**: Keep directory structure flat and meaningful.

### ❌ Mistake 3: Mixing Concerns in Single Directory

```typescript
// WRONG: Different concerns in same directory
src/models/
├── Donation.ts          # Domain model
├── DonationEntity.ts    # Database entity
├── DonationDTO.ts       # Data transfer object
└── DonationController.ts # HTTP controller
```

**Fix**: Separate by layer and concern.

## Related Documentation

- [Entity Template](./entity-template.md) - Creating domain entities
- [Repository Template](./repository-template.md) - Repository pattern
- [Service Layer Template](./service-layer-template.md) - Application services
- [TypeScript Best Practices](../ex-so-prla-ts__best-practices.md) - Coding standards
- [TypeScript Domain-Driven Design](../ex-so-prla-ts__domain-driven-design.md) - DDD principles

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
