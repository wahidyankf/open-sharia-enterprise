# Repositories

## What is a Repository?

A **Repository** is a persistence abstraction that provides collection-like access to aggregate roots. It encapsulates the logic required to access data sources and provides a clean separation between the domain model and data access infrastructure. Repositories make aggregates appear as if they exist in an in-memory collection.

**Key Characteristics:**

- **Aggregate Root Persistence**: Repositories exist only for aggregate roots, not internal entities
- **Collection Interface**: Provides collection-like operations (find, save, delete)
- **Domain-Centric**: Interface defined in domain layer, implementation in infrastructure
- **Persistence Ignorance**: Domain model unaware of database details (SQL, NoSQL, files, etc.)
- **Consistency Boundary**: Saves and loads entire aggregates atomically

**Example**: `TaxAssessmentRepository` provides methods like `findById()` and `save()` without exposing whether data is stored in PostgreSQL, MongoDB, or in-memory.

## Why Repositories Matter

### The Problem: Data Access Leaking into Domain

Without repositories, domain code becomes polluted with persistence concerns:

```typescript
// WITHOUT Repositories: Persistence logic in domain
class TaxCalculationService {
  async calculateTax(wealthHolderId: string): Promise<Money> {
    // Database queries directly in domain service
    const sql = `
      SELECT * FROM tax_assessments
      WHERE wealth_holder_id = $1 AND status = 'DRAFT'
    `;
    const result = await db.query(sql, [wealthHolderId]);

    if (result.rows.length === 0) {
      throw new Error("No assessment found");
    }

    // Manual object construction from database rows
    const assessment = new TaxAssessment(
      result.rows[0].id,
      result.rows[0].wealth_holder_id,
      // ... many more fields
    );

    // Load related declarations
    const declSql = `
      SELECT * FROM wealth_declarations
      WHERE assessment_id = $1
    `;
    const declResult = await db.query(declSql, [assessment.id]);

    // ... more database-specific code

    return assessment.finalizedTax;
  }
}
```

**Problems:**

- **Tight Coupling**: Domain logic coupled to specific database (SQL syntax, column names)
- **Testing Difficulty**: Cannot test without real database
- **Duplication**: Database access logic repeated across services
- **Violates SRP**: Service handles both business logic and data access
- **No Abstraction**: Switching databases requires rewriting domain code

### The Solution: Repositories

Repositories solve these problems through clean abstraction:

```typescript
// WITH Repositories: Clean separation
interface TaxAssessmentRepository {
  findById(id: AssessmentId): Promise<TaxAssessment | null>;
  findDraftByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment | null>;
  save(assessment: TaxAssessment): Promise<void>;
  delete(id: AssessmentId): Promise<void>;
}

class TaxCalculationService {
  constructor(private assessmentRepository: TaxAssessmentRepository) {}

  async calculateTax(wealthHolderId: WealthHolderId): Promise<Money> {
    // Clean, domain-focused code
    const assessment = await this.assessmentRepository.findDraftByWealthHolder(wealthHolderId);

    if (!assessment) {
      throw new Error("No draft assessment found");
    }

    assessment.finalize(ThresholdAmount.goldStandard(), TaxRate.standard());

    await this.assessmentRepository.save(assessment);

    return assessment.finalizedTax!;
  }
}
```

**Benefits:**

- **Persistence Ignorance**: Domain code unaware of database technology
- **Easy Testing**: Mock repository for unit tests
- **Centralized Data Access**: Database logic in one place
- **Single Responsibility**: Services focus on business logic
- **Flexible Implementation**: Swap databases without changing domain

## Repository Design Principles

### 1. One Repository Per Aggregate Root

**Rule:** Create repositories only for aggregate roots, never for entities within aggregates.

**Why?**

- Enforces access through aggregate root
- Prevents bypassing invariant enforcement
- Maintains aggregate consistency

**Anti-Pattern: Repositories for Internal Entities**

```typescript
// ANTI-PATTERN: Repository for internal entity
interface WealthDeclarationRepository {
  // WRONG! WealthDeclaration is inside TaxAssessment aggregate
  findById(id: WealthDeclarationId): Promise<WealthDeclaration>;
  save(declaration: WealthDeclaration): Promise<void>;
}

// Allows bypassing aggregate root
const declaration = await wealthDeclarationRepo.findById(declarationId);
declaration.amount = Money.usd(-1000); // Violates invariants!
await wealthDeclarationRepo.save(declaration); // Persists invalid state
```

**Correct: Repository for Aggregate Root Only**

```typescript
// CORRECT: Repository for aggregate root
interface TaxAssessmentRepository {
  findById(id: AssessmentId): Promise<TaxAssessment | null>;
  save(assessment: TaxAssessment): Promise<void>;
}

// Access declarations through aggregate root
const assessment = await assessmentRepo.findById(assessmentId);
assessment.addDeclaration(WealthType.Cash, Money.usd(1000)); // Invariants enforced
await assessmentRepo.save(assessment); // Entire aggregate saved
```

### 2. Collection-Oriented Interface

**Rule:** Repository interface should resemble an in-memory collection.

**Collection-Oriented Methods:**

```typescript
interface TaxAssessmentRepository {
  // Retrieve
  findById(id: AssessmentId): Promise<TaxAssessment | null>;
  findAll(): Promise<TaxAssessment[]>;

  // Add/Update
  save(assessment: TaxAssessment): Promise<void>; // Insert or update

  // Remove
  delete(id: AssessmentId): Promise<void>;

  // Query
  findByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment[]>;
  findFinalized(startDate: HijriDate, endDate: HijriDate): Promise<TaxAssessment[]>;
}
```

**Benefits:**

- Familiar, intuitive interface
- Hides persistence details
- Encourages thinking in terms of objects, not tables

**Alternative: Persistence-Oriented Interface**

Some prefer explicit `add()` and `update()` instead of `save()`:

```typescript
interface TaxAssessmentRepository {
  add(assessment: TaxAssessment): Promise<void>; // Insert only
  update(assessment: TaxAssessment): Promise<void>; // Update only
  remove(id: AssessmentId): Promise<void>;
}
```

**Trade-off:** More explicit but requires caller to track new vs. existing aggregates.

### 3. Domain Interface, Infrastructure Implementation

**Rule:** Repository interface defined in domain layer, implementation in infrastructure layer.

**Layered Architecture:**

```
┌─────────────────────────────────────┐
│   Application Layer                 │
│   - Use Cases                       │
│   - Application Services            │
└─────────────────────────────────────┘
            ↓ uses
┌─────────────────────────────────────┐
│   Domain Layer                      │
│   - Aggregates                      │
│   - Entities                        │
│   - Value Objects                   │
│   - Repository Interfaces ← HERE    │
└─────────────────────────────────────┘
            ↑ implemented by
┌─────────────────────────────────────┐
│   Infrastructure Layer              │
│   - Repository Implementations      │
│   - Database Access                 │
│   - ORM Mappings                    │
└─────────────────────────────────────┘
```

**Domain Layer (Interface):**

```typescript
// domain/repositories/TaxAssessmentRepository.ts
export interface TaxAssessmentRepository {
  findById(id: AssessmentId): Promise<TaxAssessment | null>;
  save(assessment: TaxAssessment): Promise<void>;
  findByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment[]>;
}
```

**Infrastructure Layer (Implementation):**

```typescript
// infrastructure/repositories/PostgresTaxAssessmentRepository.ts
import { TaxAssessmentRepository } from "../../domain/repositories/TaxAssessmentRepository";

export class PostgresTaxAssessmentRepository implements TaxAssessmentRepository {
  constructor(private db: DatabaseConnection) {}

  async findById(id: AssessmentId): Promise<TaxAssessment | null> {
    const row = await this.db.query("SELECT * FROM tax_assessments WHERE id = $1", [id.toString()]);

    if (!row) return null;

    return this.toDomain(row);
  }

  async save(assessment: TaxAssessment): Promise<void> {
    const data = this.toPersistence(assessment);
    await this.db.query("INSERT INTO tax_assessments (...) VALUES (...) ON CONFLICT (id) DO UPDATE ...", data);
  }

  async findByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment[]> {
    const rows = await this.db.query("SELECT * FROM tax_assessments WHERE wealth_holder_id = $1", [
      holderId.toString(),
    ]);

    return rows.map((row) => this.toDomain(row));
  }

  private toDomain(row: any): TaxAssessment {
    // Map database row to domain aggregate
    // ...
  }

  private toPersistence(assessment: TaxAssessment): any {
    // Map domain aggregate to database row
    // ...
  }
}
```

**Benefits:**

- Domain independent of infrastructure
- Multiple implementations possible (Postgres, MongoDB, InMemory)
- Easy to test domain with mock repositories

### 4. Return Domain Objects, Not DTOs

**Rule:** Repository methods return domain objects (aggregates, entities, value objects), never DTOs or database rows.

**Anti-Pattern: Returning DTOs**

```typescript
// ANTI-PATTERN: Returning database DTOs
interface TaxAssessmentRepository {
  findById(id: string): Promise<TaxAssessmentDTO>; // Wrong! DTO, not domain object
}

type TaxAssessmentDTO = {
  id: string;
  wealth_holder_id: string; // Database column name
  status: string;
  tax_amount: number;
  // ... flat database structure
};
```

**Correct: Returning Domain Objects**

```typescript
// CORRECT: Returning domain aggregates
interface TaxAssessmentRepository {
  findById(id: AssessmentId): Promise<TaxAssessment | null>; // Domain aggregate
}

// TaxAssessment is rich domain model with behavior
class TaxAssessment {
  constructor(
    readonly id: AssessmentId,
    readonly wealthHolderId: WealthHolderId,
    private declarations: WealthDeclaration[],
    private status: AssessmentStatus,
    private taxAmount: Money | null,
  ) {}

  finalize(threshold: ThresholdAmount, rate: TaxRate): void {
    // Rich behavior
  }
}
```

### 5. Query Methods for Common Use Cases

**Rule:** Provide query methods for frequent domain queries, not just `findById()`.

**Common Query Patterns:**

```typescript
interface TaxAssessmentRepository {
  // Basic CRUD
  findById(id: AssessmentId): Promise<TaxAssessment | null>;
  save(assessment: TaxAssessment): Promise<void>;
  delete(id: AssessmentId): Promise<void>;

  // Domain queries
  findDraftByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment | null>;
  findFinalizedInPeriod(startDate: HijriDate, endDate: HijriDate): Promise<TaxAssessment[]>;
  findExemptBelowThreshold(year: number): Promise<TaxAssessment[]>;

  // Existence checks
  existsForWealthHolder(holderId: WealthHolderId, year: number): Promise<boolean>;

  // Counting
  countFinalizedInYear(year: number): Promise<number>;
}
```

**Benefits:**

- Expressive domain-specific queries
- Optimized database queries (vs. loading all and filtering in memory)
- Clear use cases visible in interface

**Avoid:** Generic query builders in repository interface (too infrastructure-focused).

## Functional Programming Perspective

In FP, repositories become pure functions that return I/O actions:

```typescript
// FP-style repository: pure functions returning I/O effects
type TaxAssessmentRepository = {
  findById: (id: AssessmentId) => IO<Option<TaxAssessment>>;
  save: (assessment: TaxAssessment) => IO<void>;
  findByWealthHolder: (holderId: WealthHolderId) => IO<TaxAssessment[]>;
};

// IO represents deferred side effect
type IO<A> = () => Promise<A>;

// Option represents nullable result
type Option<A> = { type: "Some"; value: A } | { type: "None" };

// Implementation
function createPostgresTaxAssessmentRepository(db: Database): TaxAssessmentRepository {
  return {
    findById: (id: AssessmentId) => async () => {
      const row = await db.query("SELECT * FROM tax_assessments WHERE id = $1", [id.toString()]);
      return row ? { type: "Some", value: toDomain(row) } : { type: "None" };
    },

    save: (assessment: TaxAssessment) => async () => {
      const data = toPersistence(assessment);
      await db.query("INSERT INTO ... ON CONFLICT ...", data);
    },

    findByWealthHolder: (holderId: WealthHolderId) => async () => {
      const rows = await db.query("SELECT * FROM tax_assessments WHERE wealth_holder_id = $1", [holderId.toString()]);
      return rows.map(toDomain);
    },
  };
}

// Usage: IO actions are composable
async function calculateTaxUseCase(wealthHolderId: WealthHolderId, repo: TaxAssessmentRepository): Promise<Money> {
  // Execute I/O action
  const assessments = await repo.findByWealthHolder(wealthHolderId)();

  const draftAssessment = assessments.find((a) => a.status === AssessmentStatus.Draft);

  if (!draftAssessment) {
    throw new Error("No draft assessment");
  }

  const finalized = finalizeAssessment(draftAssessment, ThresholdAmount.goldStandard(), TaxRate.standard());

  await repo.save(finalized)();

  return finalized.taxAmount!;
}
```

**FP Benefits:**

- Pure functions (no hidden side effects)
- Composable I/O operations
- Explicit effects (IO type makes side effects visible)
- Easy to test (functions, not classes)

See [DDD and Functional Programming](./ex-so-ar-dodrdedd__14-ddd-and-functional-programming.md) for comprehensive FP patterns.

## Repository Implementation Patterns

### Pattern 1: In-Memory Repository (Testing)

```typescript
export class InMemoryTaxAssessmentRepository implements TaxAssessmentRepository {
  private assessments: Map<string, TaxAssessment> = new Map();

  async findById(id: AssessmentId): Promise<TaxAssessment | null> {
    return this.assessments.get(id.toString()) ?? null;
  }

  async save(assessment: TaxAssessment): Promise<void> {
    // Clone to simulate persistence (prevent in-memory mutation affecting stored version)
    const clone = this.clone(assessment);
    this.assessments.set(assessment.id.toString(), clone);
  }

  async delete(id: AssessmentId): Promise<void> {
    this.assessments.delete(id.toString());
  }

  async findByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment[]> {
    return Array.from(this.assessments.values()).filter((a) => a.wealthHolderId.equals(holderId));
  }

  // Test helper
  clear(): void {
    this.assessments.clear();
  }

  private clone(assessment: TaxAssessment): TaxAssessment {
    // Deep clone implementation
    return JSON.parse(JSON.stringify(assessment));
  }
}
```

**Use Case:** Unit testing without real database.

### Pattern 2: SQL Repository (Relational Database)

```typescript
export class PostgresTaxAssessmentRepository implements TaxAssessmentRepository {
  constructor(private db: DatabaseConnection) {}

  async findById(id: AssessmentId): Promise<TaxAssessment | null> {
    const assessmentRow = await this.db.queryOne(
      `
      SELECT * FROM tax_assessments
      WHERE id = $1
    `,
      [id.toString()],
    );

    if (!assessmentRow) return null;

    // Load related declarations
    const declarationRows = await this.db.query(
      `
      SELECT * FROM wealth_declarations
      WHERE assessment_id = $1
    `,
      [id.toString()],
    );

    return this.toDomain(assessmentRow, declarationRows);
  }

  async save(assessment: TaxAssessment): Promise<void> {
    await this.db.transaction(async (tx) => {
      // Upsert assessment
      await tx.query(
        `
        INSERT INTO tax_assessments (id, wealth_holder_id, status, tax_amount, ...)
        VALUES ($1, $2, $3, $4, ...)
        ON CONFLICT (id) DO UPDATE SET
          wealth_holder_id = EXCLUDED.wealth_holder_id,
          status = EXCLUDED.status,
          tax_amount = EXCLUDED.tax_amount,
          ...
      `,
        [
          assessment.id.toString(),
          assessment.wealthHolderId.toString(),
          assessment.status,
          assessment.taxAmount?.amount ?? null,
          // ...
        ],
      );

      // Delete existing declarations
      await tx.query("DELETE FROM wealth_declarations WHERE assessment_id = $1", [assessment.id.toString()]);

      // Insert current declarations
      for (const declaration of assessment.declarations) {
        await tx.query(
          `
          INSERT INTO wealth_declarations (id, assessment_id, wealth_type, amount, ...)
          VALUES ($1, $2, $3, $4, ...)
        `,
          [declaration.id.toString(), assessment.id.toString(), declaration.wealthType, declaration.amount.amount],
        );
      }
    });
  }

  private toDomain(assessmentRow: any, declarationRows: any[]): TaxAssessment {
    const declarations = declarationRows.map(
      (row) => new WealthDeclaration(WealthDeclarationId.from(row.id), row.wealth_type, Money.usd(row.amount)),
    );

    return new TaxAssessment(
      AssessmentId.from(assessmentRow.id),
      WealthHolderId.from(assessmentRow.wealth_holder_id),
      declarations,
      assessmentRow.status,
      assessmentRow.tax_amount ? Money.usd(assessmentRow.tax_amount) : null,
    );
  }
}
```

**Key Points:**

- Transactional saves (aggregate + internal entities)
- Loading eager (all declarations loaded with assessment)
- Mapping between database schema and domain model

### Pattern 3: Document Store Repository (NoSQL)

```typescript
export class MongoTaxAssessmentRepository implements TaxAssessmentRepository {
  constructor(private collection: Collection) {}

  async findById(id: AssessmentId): Promise<TaxAssessment | null> {
    const doc = await this.collection.findOne({ _id: id.toString() });

    if (!doc) return null;

    return this.toDomain(doc);
  }

  async save(assessment: TaxAssessment): Promise<void> {
    const doc = this.toPersistence(assessment);

    await this.collection.replaceOne({ _id: assessment.id.toString() }, doc, { upsert: true });
  }

  async findByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment[]> {
    const docs = await this.collection.find({ wealthHolderId: holderId.toString() }).toArray();

    return docs.map((doc) => this.toDomain(doc));
  }

  private toDomain(doc: any): TaxAssessment {
    const declarations = doc.declarations.map(
      (d: any) =>
        new WealthDeclaration(
          WealthDeclarationId.from(d.id),
          d.wealthType,
          new Money(d.amount.value, d.amount.currency),
        ),
    );

    return new TaxAssessment(
      AssessmentId.from(doc._id),
      WealthHolderId.from(doc.wealthHolderId),
      declarations,
      doc.status,
      doc.taxAmount ? new Money(doc.taxAmount.value, doc.taxAmount.currency) : null,
    );
  }

  private toPersistence(assessment: TaxAssessment): any {
    return {
      _id: assessment.id.toString(),
      wealthHolderId: assessment.wealthHolderId.toString(),
      status: assessment.status,
      taxAmount: assessment.taxAmount
        ? {
            value: assessment.taxAmount.amount,
            currency: assessment.taxAmount.currency,
          }
        : null,
      declarations: assessment.declarations.map((d) => ({
        id: d.id.toString(),
        wealthType: d.wealthType,
        amount: {
          value: d.amount.amount,
          currency: d.amount.currency,
        },
      })),
    };
  }
}
```

**Benefits:**

- Aggregate naturally maps to document
- No joins needed
- Atomic aggregate saves

### Pattern 4: Event Sourcing Repository

```typescript
export class EventSourcedTaxAssessmentRepository implements TaxAssessmentRepository {
  constructor(private eventStore: EventStore) {}

  async findById(id: AssessmentId): Promise<TaxAssessment | null> {
    // Load all events for aggregate
    const events = await this.eventStore.getEvents(id.toString());

    if (events.length === 0) return null;

    // Reconstruct aggregate by replaying events
    return this.replayEvents(events);
  }

  async save(assessment: TaxAssessment): Promise<void> {
    // Extract domain events
    const events = assessment.popDomainEvents();

    // Append events to event store
    await this.eventStore.appendEvents(assessment.id.toString(), events);
  }

  private replayEvents(events: DomainEvent[]): TaxAssessment {
    // Start with initial state
    let assessment: TaxAssessment | null = null;

    for (const event of events) {
      if (event instanceof AssessmentCreated) {
        assessment = TaxAssessment.create(event.wealthHolderId);
      } else if (event instanceof WealthDeclared && assessment) {
        assessment.declareWealth(event.wealthType, event.amount, event.acquiredDate);
      } else if (event instanceof TaxCalculated && assessment) {
        // Apply finalization
      }
      // ... handle other events
    }

    return assessment!;
  }
}
```

**Benefits:**

- Complete audit trail
- Temporal queries (state at any point in time)
- Event-driven architecture integration

**Drawbacks:**

- More complex
- Slower reads (event replay)
- Requires snapshots for large event streams

## Testing Repositories

### Unit Testing Domain Logic (Mock Repository)

```typescript
describe("TaxCalculationService", () => {
  let service: TaxCalculationService;
  let mockRepo: TaxAssessmentRepository;

  beforeEach(() => {
    // Mock repository
    mockRepo = {
      findById: jest.fn(),
      save: jest.fn(),
      findByWealthHolder: jest.fn(),
    } as any;

    service = new TaxCalculationService(mockRepo);
  });

  it("should finalize assessment when called", async () => {
    // Arrange
    const assessment = TaxAssessment.create(wealthHolderId);
    assessment.addDeclaration(WealthType.Cash, Money.usd(10000));

    mockRepo.findByWealthHolder = jest.fn().mockResolvedValue([assessment]);

    // Act
    await service.calculateTax(wealthHolderId);

    // Assert
    expect(mockRepo.save).toHaveBeenCalledWith(
      expect.objectContaining({
        status: AssessmentStatus.Finalized,
      }),
    );
  });
});
```

### Integration Testing Repository Implementation

```typescript
describe("PostgresTaxAssessmentRepository", () => {
  let repo: PostgresTaxAssessmentRepository;
  let db: DatabaseConnection;

  beforeEach(async () => {
    db = await createTestDatabase();
    repo = new PostgresTaxAssessmentRepository(db);
  });

  afterEach(async () => {
    await db.close();
  });

  it("should save and retrieve assessment with declarations", async () => {
    // Arrange
    const assessment = TaxAssessment.create(wealthHolderId);
    assessment.addDeclaration(WealthType.Cash, Money.usd(10000));
    assessment.addDeclaration(WealthType.Gold, Money.usd(5000));

    // Act
    await repo.save(assessment);
    const retrieved = await repo.findById(assessment.id);

    // Assert
    expect(retrieved).not.toBeNull();
    expect(retrieved!.declarations).toHaveLength(2);
    expect(retrieved!.declarations[0].wealthType).toBe(WealthType.Cash);
  });

  it("should update existing assessment on save", async () => {
    // Arrange
    const assessment = TaxAssessment.create(wealthHolderId);
    await repo.save(assessment);

    // Act
    assessment.addDeclaration(WealthType.Cash, Money.usd(1000));
    await repo.save(assessment);

    const retrieved = await repo.findById(assessment.id);

    // Assert
    expect(retrieved!.declarations).toHaveLength(1);
  });
});
```

## Common Mistakes

### 1. Repositories for Non-Aggregate Roots

**Problem:** Creating repositories for entities inside aggregates.

```typescript
// ANTI-PATTERN
interface WealthDeclarationRepository {
  // WealthDeclaration is inside TaxAssessment aggregate
  save(declaration: WealthDeclaration): Promise<void>;
}
```

**Solution:** Repository only for aggregate root.

### 2. Anemic Repository Interface

**Problem:** Only `findById()` and `save()`, forcing clients to load and filter in memory.

```typescript
// ANTI-PATTERN: Too minimal
interface TaxAssessmentRepository {
  findById(id: AssessmentId): Promise<TaxAssessment>;
  save(assessment: TaxAssessment): Promise<void>;
}

// Forces inefficient in-memory filtering
const allAssessments = await Promise.all(ids.map((id) => repo.findById(id)));
const drafts = allAssessments.filter((a) => a.status === AssessmentStatus.Draft);
```

**Solution:** Add domain-specific query methods.

```typescript
// CORRECT
interface TaxAssessmentRepository {
  findById(id: AssessmentId): Promise<TaxAssessment | null>;
  save(assessment: TaxAssessment): Promise<void>;
  findDraftsByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment[]>; // Optimized query
}
```

### 3. Leaking Database Concepts into Interface

**Problem:** Exposing SQL, pagination tokens, or database-specific features.

```typescript
// ANTI-PATTERN: Database concepts in interface
interface TaxAssessmentRepository {
  executeQuery(sql: string, params: any[]): Promise<any>; // SQL leak!
  findWithPagination(offset: number, limit: number): Promise<{ rows: any[]; hasMore: boolean }>; // Database pagination
}
```

**Solution:** Domain-centric interface.

```typescript
// CORRECT
interface TaxAssessmentRepository {
  findByWealthHolder(holderId: WealthHolderId): Promise<TaxAssessment[]>;
  findFinalized(year: number): Promise<TaxAssessment[]>;
}
```

### 4. Not Saving Entire Aggregate

**Problem:** Saving only root entity, not internal entities.

```typescript
// ANTI-PATTERN: Incomplete save
async save(assessment: TaxAssessment): Promise<void> {
  await this.db.query("INSERT INTO tax_assessments (...) VALUES (...)", [
    /*...*/
  ]);
  // Forgot to save declarations!
}
```

**Solution:** Save entire aggregate atomically.

### 5. Returning DTOs Instead of Domain Objects

**Problem:** Returning database-specific data structures.

```typescript
// ANTI-PATTERN
async findById(id: string): Promise<AssessmentRow> {
  // Returns database row, not domain object
}
```

**Solution:** Map to domain objects.

```typescript
// CORRECT
async findById(id: AssessmentId): Promise<TaxAssessment | null> {
  const row = await this.db.query(/*...*/);
  return this.toDomain(row);
}
```

## Summary

Repositories provide clean persistence abstraction:

- **Aggregate-Centric**: One repository per aggregate root
- **Collection Interface**: Resemble in-memory collections
- **Domain-Defined**: Interface in domain, implementation in infrastructure
- **Persistence Ignorance**: Domain unaware of database details
- **Atomic Saves**: Entire aggregate saved/loaded together

**Benefits:**

- Clean separation of concerns
- Easy testing with mocks
- Flexible implementations (SQL, NoSQL, In-Memory)
- Domain focus maintained

## Next Steps

- **[Aggregates](./ex-so-ar-dodrdedd__09-aggregates.md)** - Understand what repositories persist
- **[Entities](./ex-so-ar-dodrdedd__07-entities.md)** - Objects repositories work with
- **[Domain Services](./ex-so-ar-dodrdedd__11-domain-services.md)** - Services using repositories
- **[Layered Architecture](./ex-so-ar-dodrdedd__15-layered-architecture.md)** - Where repositories fit
- **[DDD and Functional Programming](./ex-so-ar-dodrdedd__14-ddd-and-functional-programming.md)** - FP-style repositories

## References

- Eric Evans, "Domain-Driven Design" (2003) - Chapter on Repositories
- Vaughn Vernon, "Implementing Domain-Driven Design" (2013) - Chapter 12: Repositories
- Martin Fowler, ["Repository Pattern"](https://martinfowler.com/eaaCatalog/repository.html)
- Martin Fowler, ["Data Mapper"](https://martinfowler.com/eaaCatalog/dataMapper.html)
