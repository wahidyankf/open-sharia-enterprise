# Integration Test Template

## Metadata

- **Parent Directory**: [Test-Driven Development (TDD)](../README.md)
- **Main Topic**: [Test-Driven Development (TDD)](../README.md)
- **Related Templates**:
  - [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md)
  - [Test Data Builder Template](./ex-so-de-tedrdeve-te__test-data-builder-template.md)
  - [CI/CD Pipeline Template](./ex-so-de-tedrdeve-te__ci-cd-pipeline-template.md)
- **Use Case**: Testing component interactions with real dependencies (database, external services)
- **Template Size**: ~20 KB
- **Complexity**: Intermediate to Advanced

## Overview

This template provides a standardized structure for writing integration tests that verify how multiple components work together with real infrastructure dependencies like databases, message queues, and external services. Integration tests are slower than unit tests but provide higher confidence in system behavior.

## Core Principles

Integration testing aligns with software engineering principles:

- **[Reproducibility First](../../../../../../governance/principles/software-engineering/reproducibility.md)** - Integration tests use test containers and isolated environments to ensure reproducible test execution across different systems.
- **[Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)** - Tests explicitly verify component interactions and infrastructure behavior with real dependencies.

## When to Use Integration Tests

**Use Integration Tests When**:

- Testing repository implementations with real databases
- Verifying API endpoints with database operations
- Testing message queue producers/consumers
- Validating external service integrations
- Testing transaction boundaries and rollback behavior
- Verifying data persistence and retrieval
- Testing database migrations and schema changes

**Don't Use Integration Tests For**:

- Pure business logic (use unit tests)
- Simple calculations or transformations
- Value object behavior
- Complex conditional logic (use unit tests for speed)

## Template Structure

```typescript
// File: [component-name].integration.spec.ts
import { Test, TestingModule } from "@nestjs/testing";
import { DatabaseContainer, StartedTestContainer } from "testcontainers";
import { DataSource } from "typeorm";
import { ComponentUnderTest } from "./component-under-test";

describe("[ComponentUnderTest] Integration", () => {
  let container: StartedTestContainer;
  let dataSource: DataSource;
  let component: ComponentUnderTest;

  // Start infrastructure before all tests
  beforeAll(async () => {
    // Start test containers (database, redis, etc.)
    container = await new DatabaseContainer("postgres:16-alpine")
      .withDatabase("test_db")
      .withUsername("test_user")
      .withPassword("test_password")
      .start();

    // Initialize database connection
    dataSource = await createTestDataSource(container);
    await dataSource.initialize();
    await dataSource.runMigrations();
  });

  // Clean up infrastructure after all tests
  afterAll(async () => {
    await dataSource.destroy();
    await container.stop();
  });

  // Clean database state before each test
  beforeEach(async () => {
    await cleanDatabase(dataSource);
    component = new ComponentUnderTest(dataSource);
  });

  describe("[Feature Name]", () => {
    it("should persist and retrieve data correctly", async () => {
      // Arrange
      const data = createTestData();

      // Act
      await component.save(data);
      const retrieved = await component.findById(data.id);

      // Assert
      expect(retrieved).toEqual(data);
    });

    it("should handle transaction rollback on error", async () => {
      // Arrange
      const invalidData = createInvalidData();

      // Act & Assert
      await expect(component.save(invalidData)).rejects.toThrow();

      // Verify rollback
      const count = await component.count();
      expect(count).toBe(0);
    });
  });
});
```

## Islamic Finance Example: Tax Repository Integration Tests

### Repository Under Test

```typescript
// File: tax-repository.ts
import { Repository, DataSource } from "typeorm";
import { TaxAssessment } from "./tax-assessment.entity";
import { Money } from "./money";

export class TaxRepository {
  private repository: Repository<TaxAssessment>;

  constructor(private readonly dataSource: DataSource) {
    this.repository = dataSource.getRepository(TaxAssessment);
  }

  async save(assessment: TaxAssessment): Promise<TaxAssessment> {
    return await this.repository.save(assessment);
  }

  async findById(id: string): Promise<TaxAssessment | null> {
    return await this.repository.findOne({ where: { id } });
  }

  async findByDonorId(donorId: string): Promise<TaxAssessment[]> {
    return await this.repository.find({
      where: { donorId },
      order: { assessmentDate: "DESC" },
    });
  }

  async findPendingAssessments(): Promise<TaxAssessment[]> {
    return await this.repository.find({
      where: { status: "PENDING" },
      order: { assessmentDate: "ASC" },
    });
  }

  async updateStatus(id: string, status: string): Promise<void> {
    await this.repository.update(id, { status });
  }

  async calculateTotalTaxCollected(startDate: Date, endDate: Date): Promise<Money> {
    const result = await this.repository
      .createQueryBuilder("assessment")
      .select("SUM(assessment.taxAmount)", "total")
      .addSelect("assessment.currency", "currency")
      .where("assessment.status = :status", { status: "PAID" })
      .andWhere("assessment.paymentDate BETWEEN :startDate AND :endDate", {
        startDate,
        endDate,
      })
      .groupBy("assessment.currency")
      .getRawMany();

    // For simplicity, return first currency found or zero
    if (result.length === 0) {
      return Money.zero("USD");
    }

    return Money.fromAmount(parseFloat(result[0].total), result[0].currency);
  }
}
```

### Entity Definition

```typescript
// File: tax-assessment.entity.ts
import { Entity, Column, PrimaryColumn, CreateDateColumn } from "typeorm";

@Entity("tax_assessments")
export class TaxAssessment {
  @PrimaryColumn("uuid")
  id: string;

  @Column()
  donorId: string;

  @Column("decimal", { precision: 10, scale: 2 })
  wealthAmount: number;

  @Column("decimal", { precision: 10, scale: 2 })
  thresholdAmount: number;

  @Column("decimal", { precision: 10, scale: 2 })
  taxAmount: number;

  @Column()
  currency: string;

  @Column()
  status: string; // PENDING, PAID, CANCELLED

  @CreateDateColumn()
  assessmentDate: Date;

  @Column({ nullable: true })
  paymentDate?: Date;

  @Column({ nullable: true })
  notes?: string;
}
```

### Complete Integration Test Suite

```typescript
// File: tax-repository.integration.spec.ts
import { DataSource } from "typeorm";
import { PostgreSqlContainer, StartedPostgreSqlContainer } from "@testcontainers/postgresql";
import { TaxRepository } from "./tax-repository";
import { TaxAssessment } from "./tax-assessment.entity";
import { Money } from "./money";
import { v4 as uuidv4 } from "uuid";

describe("TaxRepository Integration", () => {
  let container: StartedPostgreSqlContainer;
  let dataSource: DataSource;
  let repository: TaxRepository;

  // Start PostgreSQL container before all tests
  beforeAll(async () => {
    container = await new PostgreSqlContainer("postgres:16-alpine")
      .withDatabase("tax_test")
      .withUsername("test_user")
      .withPassword("test_password")
      .start();

    // Create DataSource with container connection
    dataSource = new DataSource({
      type: "postgres",
      host: container.getHost(),
      port: container.getPort(),
      username: container.getUsername(),
      password: container.getPassword(),
      database: container.getDatabase(),
      entities: [TaxAssessment],
      synchronize: true, // Auto-create schema for tests
      logging: false,
    });

    await dataSource.initialize();
  }, 60000); // Timeout: 60 seconds for container startup

  // Stop container after all tests
  afterAll(async () => {
    if (dataSource) {
      await dataSource.destroy();
    }
    if (container) {
      await container.stop();
    }
  });

  // Create fresh repository before each test
  beforeEach(async () => {
    // Clean all data
    await dataSource.getRepository(TaxAssessment).clear();
    repository = new TaxRepository(dataSource);
  });

  describe("save", () => {
    it("should persist TaxAssessment to database", async () => {
      // Arrange
      const assessment = createTestAssessment({
        id: uuidv4(),
        donorId: "donor-001",
        wealthAmount: 10000,
        thresholdAmount: 2000,
        taxAmount: 250,
        currency: "USD",
        status: "PENDING",
      });

      // Act
      const saved = await repository.save(assessment);

      // Assert
      expect(saved).toBeDefined();
      expect(saved.id).toBe(assessment.id);

      // Verify persistence
      const retrieved = await repository.findById(assessment.id);
      expect(retrieved).toBeDefined();
      expect(retrieved?.donorId).toBe("donor-001");
      expect(retrieved?.taxAmount).toBe(250);
    });

    it("should handle multiple assessments for same donor", async () => {
      // Arrange
      const donorId = "donor-002";
      const assessment1 = createTestAssessment({
        id: uuidv4(),
        donorId,
        taxAmount: 250,
        assessmentDate: new Date("2024-01-15"),
      });
      const assessment2 = createTestAssessment({
        id: uuidv4(),
        donorId,
        taxAmount: 300,
        assessmentDate: new Date("2024-06-15"),
      });

      // Act
      await repository.save(assessment1);
      await repository.save(assessment2);

      // Assert
      const assessments = await repository.findByDonorId(donorId);
      expect(assessments).toHaveLength(2);
      expect(assessments[0].taxAmount).toBe(300); // Most recent first
      expect(assessments[1].taxAmount).toBe(250);
    });

    it("should enforce unique id constraint", async () => {
      // Arrange
      const id = uuidv4();
      const assessment1 = createTestAssessment({ id, taxAmount: 250 });
      const assessment2 = createTestAssessment({ id, taxAmount: 300 });

      // Act
      await repository.save(assessment1);

      // Assert
      await expect(repository.save(assessment2)).rejects.toThrow();
    });
  });

  describe("findById", () => {
    it("should retrieve assessment by id", async () => {
      // Arrange
      const assessment = createTestAssessment({
        id: uuidv4(),
        donorId: "donor-003",
        taxAmount: 500,
      });
      await repository.save(assessment);

      // Act
      const retrieved = await repository.findById(assessment.id);

      // Assert
      expect(retrieved).toBeDefined();
      expect(retrieved?.id).toBe(assessment.id);
      expect(retrieved?.donorId).toBe("donor-003");
      expect(retrieved?.taxAmount).toBe(500);
    });

    it("should return null for non-existent id", async () => {
      // Arrange
      const nonExistentId = uuidv4();

      // Act
      const retrieved = await repository.findById(nonExistentId);

      // Assert
      expect(retrieved).toBeNull();
    });
  });

  describe("findByDonorId", () => {
    it("should retrieve all assessments for donor ordered by date descending", async () => {
      // Arrange
      const donorId = "donor-004";
      const dates = [new Date("2024-01-01"), new Date("2024-04-01"), new Date("2024-07-01"), new Date("2024-10-01")];

      for (const date of dates) {
        const assessment = createTestAssessment({
          id: uuidv4(),
          donorId,
          assessmentDate: date,
        });
        await repository.save(assessment);
      }

      // Act
      const assessments = await repository.findByDonorId(donorId);

      // Assert
      expect(assessments).toHaveLength(4);
      expect(assessments[0].assessmentDate).toEqual(dates[3]); // Most recent
      expect(assessments[1].assessmentDate).toEqual(dates[2]);
      expect(assessments[2].assessmentDate).toEqual(dates[1]);
      expect(assessments[3].assessmentDate).toEqual(dates[0]); // Oldest
    });

    it("should return empty array for donor with no assessments", async () => {
      // Arrange
      const donorId = "donor-999";

      // Act
      const assessments = await repository.findByDonorId(donorId);

      // Assert
      expect(assessments).toHaveLength(0);
    });

    it("should not return assessments for other donors", async () => {
      // Arrange
      await repository.save(createTestAssessment({ id: uuidv4(), donorId: "donor-005" }));
      await repository.save(createTestAssessment({ id: uuidv4(), donorId: "donor-006" }));

      // Act
      const assessments = await repository.findByDonorId("donor-005");

      // Assert
      expect(assessments).toHaveLength(1);
      expect(assessments[0].donorId).toBe("donor-005");
    });
  });

  describe("findPendingAssessments", () => {
    it("should retrieve only pending assessments", async () => {
      // Arrange
      await repository.save(createTestAssessment({ id: uuidv4(), status: "PENDING", donorId: "donor-007" }));
      await repository.save(createTestAssessment({ id: uuidv4(), status: "PAID", donorId: "donor-008" }));
      await repository.save(createTestAssessment({ id: uuidv4(), status: "PENDING", donorId: "donor-009" }));
      await repository.save(createTestAssessment({ id: uuidv4(), status: "CANCELLED", donorId: "donor-010" }));

      // Act
      const pending = await repository.findPendingAssessments();

      // Assert
      expect(pending).toHaveLength(2);
      expect(pending.every((a) => a.status === "PENDING")).toBe(true);
    });

    it("should order pending assessments by date ascending (oldest first)", async () => {
      // Arrange
      const dates = [new Date("2024-03-01"), new Date("2024-01-01"), new Date("2024-02-01")];

      for (const date of dates) {
        await repository.save(
          createTestAssessment({
            id: uuidv4(),
            status: "PENDING",
            assessmentDate: date,
          }),
        );
      }

      // Act
      const pending = await repository.findPendingAssessments();

      // Assert
      expect(pending[0].assessmentDate).toEqual(dates[1]); // Oldest
      expect(pending[1].assessmentDate).toEqual(dates[2]);
      expect(pending[2].assessmentDate).toEqual(dates[0]); // Newest
    });
  });

  describe("updateStatus", () => {
    it("should update assessment status", async () => {
      // Arrange
      const assessment = createTestAssessment({
        id: uuidv4(),
        status: "PENDING",
      });
      await repository.save(assessment);

      // Act
      await repository.updateStatus(assessment.id, "PAID");

      // Assert
      const updated = await repository.findById(assessment.id);
      expect(updated?.status).toBe("PAID");
    });

    it("should not affect other fields when updating status", async () => {
      // Arrange
      const assessment = createTestAssessment({
        id: uuidv4(),
        donorId: "donor-011",
        taxAmount: 750,
        status: "PENDING",
      });
      await repository.save(assessment);

      // Act
      await repository.updateStatus(assessment.id, "CANCELLED");

      // Assert
      const updated = await repository.findById(assessment.id);
      expect(updated?.status).toBe("CANCELLED");
      expect(updated?.donorId).toBe("donor-011"); // Unchanged
      expect(updated?.taxAmount).toBe(750); // Unchanged
    });
  });

  describe("calculateTotalTaxCollected", () => {
    it("should sum paid tax amounts within date range", async () => {
      // Arrange
      const startDate = new Date("2024-01-01");
      const endDate = new Date("2024-12-31");

      // Paid assessments within range
      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PAID",
          taxAmount: 250,
          currency: "USD",
          paymentDate: new Date("2024-03-15"),
        }),
      );
      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PAID",
          taxAmount: 500,
          currency: "USD",
          paymentDate: new Date("2024-06-15"),
        }),
      );
      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PAID",
          taxAmount: 300,
          currency: "USD",
          paymentDate: new Date("2024-09-15"),
        }),
      );

      // Act
      const total = await repository.calculateTotalTaxCollected(startDate, endDate);

      // Assert
      expect(total.amount).toBe(1050); // 250 + 500 + 300
      expect(total.currency).toBe("USD");
    });

    it("should exclude pending assessments from total", async () => {
      // Arrange
      const startDate = new Date("2024-01-01");
      const endDate = new Date("2024-12-31");

      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PAID",
          taxAmount: 250,
          paymentDate: new Date("2024-03-15"),
        }),
      );
      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PENDING",
          taxAmount: 500,
          assessmentDate: new Date("2024-03-15"),
        }),
      );

      // Act
      const total = await repository.calculateTotalTaxCollected(startDate, endDate);

      // Assert
      expect(total.amount).toBe(250); // Only paid amount
    });

    it("should exclude payments outside date range", async () => {
      // Arrange
      const startDate = new Date("2024-04-01");
      const endDate = new Date("2024-06-30");

      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PAID",
          taxAmount: 100,
          paymentDate: new Date("2024-03-15"), // Before range
        }),
      );
      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PAID",
          taxAmount: 250,
          paymentDate: new Date("2024-05-15"), // Within range
        }),
      );
      await repository.save(
        createTestAssessment({
          id: uuidv4(),
          status: "PAID",
          taxAmount: 300,
          paymentDate: new Date("2024-07-15"), // After range
        }),
      );

      // Act
      const total = await repository.calculateTotalTaxCollected(startDate, endDate);

      // Assert
      expect(total.amount).toBe(250); // Only payment within range
    });

    it("should return zero when no payments found", async () => {
      // Arrange
      const startDate = new Date("2024-01-01");
      const endDate = new Date("2024-12-31");

      // Act
      const total = await repository.calculateTotalTaxCollected(startDate, endDate);

      // Assert
      expect(total.amount).toBe(0);
      expect(total.currency).toBe("USD");
    });
  });

  describe("transaction behavior", () => {
    it("should rollback on error during save", async () => {
      // Arrange
      const assessment = createTestAssessment({
        id: uuidv4(),
        donorId: "donor-012",
      });

      // Act & Assert
      // Simulate error by providing invalid data
      const invalidAssessment = { ...assessment, id: null as any };

      await expect(repository.save(invalidAssessment)).rejects.toThrow();

      // Verify rollback - no data persisted
      const count = await dataSource.getRepository(TaxAssessment).count();
      expect(count).toBe(0);
    });
  });
});

// Test Helper Functions

function createTestAssessment(overrides: Partial<TaxAssessment> = {}): TaxAssessment {
  const assessment = new TaxAssessment();
  assessment.id = overrides.id || uuidv4();
  assessment.donorId = overrides.donorId || "donor-default";
  assessment.wealthAmount = overrides.wealthAmount ?? 10000;
  assessment.thresholdAmount = overrides.thresholdAmount ?? 2000;
  assessment.taxAmount = overrides.taxAmount ?? 250;
  assessment.currency = overrides.currency || "USD";
  assessment.status = overrides.status || "PENDING";
  assessment.assessmentDate = overrides.assessmentDate || new Date();
  assessment.paymentDate = overrides.paymentDate;
  assessment.notes = overrides.notes;
  return assessment;
}
```

## Additional Example: Permitted Certification Repository

```typescript
// File: permitted-certification-repository.integration.spec.ts
import { DataSource } from "typeorm";
import { PostgreSqlContainer, StartedPostgreSqlContainer } from "@testcontainers/postgresql";
import { PermittedCertificationRepository } from "./permitted-certification-repository";
import { PermittedCertification } from "./permitted-certification.entity";
import { v4 as uuidv4 } from "uuid";

describe("PermittedCertificationRepository Integration", () => {
  let container: StartedPostgreSqlContainer;
  let dataSource: DataSource;
  let repository: PermittedCertificationRepository;

  beforeAll(async () => {
    container = await new PostgreSqlContainer("postgres:16-alpine").start();

    dataSource = new DataSource({
      type: "postgres",
      host: container.getHost(),
      port: container.getPort(),
      username: container.getUsername(),
      password: container.getPassword(),
      database: container.getDatabase(),
      entities: [PermittedCertification],
      synchronize: true,
      logging: false,
    });

    await dataSource.initialize();
  }, 60000);

  afterAll(async () => {
    await dataSource?.destroy();
    await container?.stop();
  });

  beforeEach(async () => {
    await dataSource.getRepository(PermittedCertification).clear();
    repository = new PermittedCertificationRepository(dataSource);
  });

  describe("findActiveCertifications", () => {
    it("should retrieve only active certifications", async () => {
      // Arrange
      const active1 = createCertification({
        id: uuidv4(),
        status: "ACTIVE",
        expiryDate: new Date("2025-12-31"),
      });
      const active2 = createCertification({
        id: uuidv4(),
        status: "ACTIVE",
        expiryDate: new Date("2025-06-30"),
      });
      const expired = createCertification({
        id: uuidv4(),
        status: "EXPIRED",
        expiryDate: new Date("2023-12-31"),
      });

      await repository.save(active1);
      await repository.save(active2);
      await repository.save(expired);

      // Act
      const activeCerts = await repository.findActiveCertifications();

      // Assert
      expect(activeCerts).toHaveLength(2);
      expect(activeCerts.every((c) => c.status === "ACTIVE")).toBe(true);
    });
  });

  describe("findExpiringCertifications", () => {
    it("should find certifications expiring within specified days", async () => {
      // Arrange
      const today = new Date();
      const in15Days = new Date(today.getTime() + 15 * 24 * 60 * 60 * 1000);
      const in45Days = new Date(today.getTime() + 45 * 24 * 60 * 60 * 1000);
      const in90Days = new Date(today.getTime() + 90 * 24 * 60 * 60 * 1000);

      await repository.save(createCertification({ id: uuidv4(), expiryDate: in15Days }));
      await repository.save(createCertification({ id: uuidv4(), expiryDate: in45Days }));
      await repository.save(createCertification({ id: uuidv4(), expiryDate: in90Days }));

      // Act
      const expiring = await repository.findExpiringCertifications(30);

      // Assert
      expect(expiring).toHaveLength(1); // Only the one expiring in 15 days
    });
  });
});

function createCertification(overrides: Partial<PermittedCertification> = {}): PermittedCertification {
  const cert = new PermittedCertification();
  cert.id = overrides.id || uuidv4();
  cert.productId = overrides.productId || "product-001";
  cert.certificationBody = overrides.certificationBody || "JAKIM";
  cert.status = overrides.status || "ACTIVE";
  cert.issueDate = overrides.issueDate || new Date();
  cert.expiryDate = overrides.expiryDate || new Date("2025-12-31");
  return cert;
}
```

## Best Practices for Integration Tests

### 1. Use Test Containers

```typescript
// ✅ GOOD - Use test containers for real database
beforeAll(async () => {
  container = await new PostgreSqlContainer("postgres:16-alpine").withDatabase("test_db").start();
});

// ❌ BAD - Using in-memory database with different behavior
beforeAll(async () => {
  dataSource = new DataSource({
    type: "sqlite",
    database: ":memory:",
  });
});
```

### 2. Clean Database Between Tests

```typescript
// ✅ GOOD - Clean state before each test
beforeEach(async () => {
  await dataSource.getRepository(TaxAssessment).clear();
  // Or use transactions and rollback
});

// ❌ BAD - Tests depend on execution order
it("test 1", async () => {
  await repository.save(data1);
});

it("test 2", async () => {
  // Assumes data1 exists from previous test
  const all = await repository.findAll();
  expect(all).toHaveLength(1);
});
```

### 3. Test Real Database Constraints

```typescript
describe("database constraints", () => {
  it("should enforce unique constraint on certification number", async () => {
    const cert1 = createCertification({ certificationNumber: "CERT-001" });
    const cert2 = createCertification({ certificationNumber: "CERT-001" });

    await repository.save(cert1);

    await expect(repository.save(cert2)).rejects.toThrow();
  });

  it("should enforce foreign key constraint", async () => {
    const assessment = createTestAssessment({
      donorId: "non-existent-donor",
    });

    await expect(repository.save(assessment)).rejects.toThrow();
  });
});
```

### 4. Test Transaction Behavior

```typescript
describe("transactions", () => {
  it("should rollback all changes on error", async () => {
    const queryRunner = dataSource.createQueryRunner();
    await queryRunner.connect();
    await queryRunner.startTransaction();

    try {
      // Arrange
      const assessment1 = createTestAssessment({ id: uuidv4() });
      const assessment2 = createTestAssessment({ id: null as any }); // Invalid

      // Act
      await queryRunner.manager.save(assessment1);
      await queryRunner.manager.save(assessment2); // Will throw

      await queryRunner.commitTransaction();
    } catch (error) {
      await queryRunner.rollbackTransaction();
    } finally {
      await queryRunner.release();
    }

    // Assert - no data persisted
    const count = await repository.count();
    expect(count).toBe(0);
  });
});
```

### 5. Test Query Performance

```typescript
describe("performance", () => {
  it("should use index for donor lookup", async () => {
    // Arrange - create many assessments
    const donorId = "donor-performance";
    for (let i = 0; i < 1000; i++) {
      await repository.save(createTestAssessment({ id: uuidv4(), donorId: `donor-${i}` }));
    }
    await repository.save(createTestAssessment({ id: uuidv4(), donorId }));

    // Act
    const startTime = Date.now();
    const assessments = await repository.findByDonorId(donorId);
    const duration = Date.now() - startTime;

    // Assert
    expect(assessments).toHaveLength(1);
    expect(duration).toBeLessThan(100); // Should be fast with index
  });
});
```

## Test Container Configuration Examples

### PostgreSQL with Custom Configuration

```typescript
beforeAll(async () => {
  container = await new PostgreSqlContainer("postgres:16-alpine")
    .withDatabase("tax_test")
    .withUsername("test_user")
    .withPassword("test_password")
    .withExposedPorts(5432)
    .withEnvironment({ POSTGRES_INITDB_ARGS: "--encoding=UTF8" })
    .start();
});
```

### Multiple Containers (Database + Redis)

```typescript
beforeAll(async () => {
  // Start PostgreSQL
  postgresContainer = await new PostgreSqlContainer("postgres:16-alpine").start();

  // Start Redis
  redisContainer = await new GenericContainer("redis:7-alpine").withExposedPorts(6379).start();

  // Initialize connections
  dataSource = await createDataSource(postgresContainer);
  redisClient = createRedisClient(redisContainer);
});
```

## Checklist for Integration Tests

- [ ] Test file name includes `.integration.spec.ts` suffix
- [ ] Test containers used for infrastructure dependencies
- [ ] Database initialized before all tests
- [ ] Database cleaned before each test
- [ ] Containers stopped after all tests
- [ ] Real database constraints tested (unique, foreign key, etc.)
- [ ] Transaction behavior verified
- [ ] Error handling and rollback tested
- [ ] Query performance validated with realistic data volumes
- [ ] Tests can run in isolation
- [ ] Tests can run in parallel (separate database per suite)
- [ ] Proper timeout configured for container startup
- [ ] Connection properly closed to avoid leaks

## Related Templates

- [Unit Test Template](./ex-so-de-tedrdeve-te__unit-test-template.md) - Testing isolated units
- [Test Data Builder Template](./ex-so-de-tedrdeve-te__test-data-builder-template.md) - Creating test data
- [CI/CD Pipeline Template](./ex-so-de-tedrdeve-te__ci-cd-pipeline-template.md) - Running tests in pipeline

## Related Principles

Integration testing demonstrates alignment with:

- **[Reproducibility First](../../../../../../governance/principles/software-engineering/reproducibility.md)** - Test containers ensure identical infrastructure across all test environments.

See [Software Engineering Principles](../../../../../../governance/principles/software-engineering/README.md) for comprehensive documentation.

## Summary

**Key Takeaways**:

1. **Test Containers**: Use real infrastructure dependencies, not mocks
2. **Clean State**: Reset database before each test for isolation
3. **Real Constraints**: Test actual database constraints and behavior
4. **Transaction Testing**: Verify rollback and commit behavior
5. **Performance**: Test with realistic data volumes
6. **Proper Cleanup**: Always stop containers and close connections
7. **Longer Timeouts**: Account for container startup time

Integration tests provide confidence that your application works correctly with real infrastructure while maintaining test isolation and reproducibility.
