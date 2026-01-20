# Test-Driven Development: Integration Testing

## Overview

Integration tests verify that multiple components work together correctly. Unlike unit tests that isolate individual pieces, integration tests exercise real collaborations—database persistence, API communication, message queue interactions, and cross-boundary contracts.

Integration tests sit in the middle of the testing pyramid. They're slower than unit tests but faster than end-to-end tests. They catch bugs that unit tests miss (integration failures, incorrect assumptions about dependencies) while remaining more stable and debuggable than full system tests.

This document covers when to write integration tests, how to structure them, testing with real dependencies (databases, APIs), test containers, contract testing for bounded contexts, and best practices for maintaining a healthy integration test suite.

## Integration Tests vs Unit Tests

### Key Differences

| Aspect           | Unit Tests                       | Integration Tests                     |
| ---------------- | -------------------------------- | ------------------------------------- |
| **Scope**        | Single component in isolation    | Multiple components working together  |
| **Dependencies** | Mocked/stubbed                   | Real (database, API, file system)     |
| **Speed**        | Milliseconds (1-10ms)            | Seconds (100ms-5s)                    |
| **Environment**  | None required                    | Test database, containers, test APIs  |
| **Failures**     | Pinpoint exact function/class    | Indicate integration problem          |
| **Maintenance**  | Low (only when behavior changes) | Medium (schema changes, API changes)  |
| **Flakiness**    | Very low (deterministic)         | Low to medium (environmental issues)  |
| **What to test** | Logic, algorithms, edge cases    | Persistence, communication, contracts |

### Example: Unit vs Integration Test

```typescript
// UNIT TEST: ZakatCalculator logic in isolation
describe("ZakatCalculator - Unit", () => {
  it("should calculate 2.5% of wealth", () => {
    const calculator = new ZakatCalculator();
    const wealth = Money.usd(1000);

    const zakat = calculator.calculate(wealth);

    expect(zakat).toEqualMoney(Money.usd(25));
  });
  // Fast: <1ms, no dependencies ✅
});

// INTEGRATION TEST: ZakatRepository with real database
describe("ZakatRepository - Integration", () => {
  let repository: ZakatRepository;
  let database: TestDatabase;

  beforeEach(async () => {
    database = await TestDatabase.create();
    await database.migrate();
    repository = new ZakatRepository(database);
  });

  afterEach(async () => {
    await database.cleanup();
  });

  it("should save and retrieve zakat assessment", async () => {
    const assessment = buildZakatAssessment();

    await repository.save(assessment);
    const retrieved = await repository.findById(assessment.id);

    expect(retrieved).toBeDefined();
    expect(retrieved!.id.equals(assessment.id)).toBe(true);
  });
  // Slower: ~100-500ms, uses real database ✅
});
```

## When to Write Integration Tests

### Use Integration Tests For

1. **Database Persistence**
   - Repository save/retrieve operations
   - Query correctness (joins, filters, aggregations)
   - Transaction boundaries
   - Schema compatibility

   **Example**: Verify `MurabahaContractRepository` correctly saves and loads contracts with all related installments.

2. **External API Integration**
   - Third-party service calls
   - HTTP client configuration
   - Response parsing
   - Error handling (timeouts, 500 errors)

   **Example**: Test Gold Price API integration returns valid nisab threshold.

3. **Message Queue Communication**
   - Event publishing
   - Event consumption
   - Message serialization/deserialization
   - Dead letter queue handling

   **Example**: Verify `ZakatCalculated` event is published and consumed correctly.

4. **Cross-Boundary Contracts**
   - Bounded context integration (DDD)
   - API contracts between services
   - Shared data formats

   **Example**: Test contract between Zakat Assessment context and Notification context.

5. **File System Operations**
   - File reading/writing
   - CSV/JSON parsing
   - Path resolution

   **Example**: Test Halal certification document upload and retrieval.

### Don't Write Integration Tests For

- Pure business logic (use unit tests)
- UI rendering (use component tests or E2E)
- Full user workflows (use E2E tests)
- Simple getters/setters (no test needed)

## Database Integration Testing

### Test Database Setup

**Option 1: In-Memory Database (SQLite)**

```typescript
// Fast, isolated, no Docker required
class TestDatabase {
  static async create(): Promise<TestDatabase> {
    const db = new Database(":memory:"); // SQLite in-memory
    return new TestDatabase(db);
  }

  async migrate(): Promise<void> {
    await this.db.exec(`
      CREATE TABLE zakat_assessments (
        id TEXT PRIMARY KEY,
        wealth_amount REAL NOT NULL,
        wealth_currency TEXT NOT NULL,
        nisab_amount REAL NOT NULL,
        calculated_at TEXT NOT NULL
      )
    `);
  }

  async cleanup(): Promise<void> {
    await this.db.close();
  }
}

// Usage
describe("ZakatRepository with in-memory DB", () => {
  let repository: ZakatRepository;
  let database: TestDatabase;

  beforeEach(async () => {
    database = await TestDatabase.create();
    await database.migrate();
    repository = new ZakatRepository(database);
  });

  afterEach(async () => {
    await database.cleanup();
  });

  it("should save assessment", async () => {
    const assessment = buildZakatAssessment();
    await repository.save(assessment);

    const count = await database.query("SELECT COUNT(*) FROM zakat_assessments");
    expect(count).toBe(1);
  });
});
```

**Pros**: Fast (in-memory), no Docker, isolated per test
**Cons**: SQLite syntax differs from PostgreSQL/MySQL (may miss production bugs)

**Option 2: Test Containers (Real Database)**

```typescript
import { PostgreSqlContainer } from "testcontainers";

class TestDatabase {
  static async create(): Promise<TestDatabase> {
    const container = await new PostgreSqlContainer("postgres:16")
      .withDatabase("zakat_test")
      .withUsername("test")
      .withPassword("test")
      .start();

    const connectionString = container.getConnectionUri();
    const db = new DatabaseClient(connectionString);
    await db.connect();

    return new TestDatabase(db, container);
  }

  async migrate(): Promise<void> {
    await this.db.migrate(); // Run production migrations
  }

  async cleanup(): Promise<void> {
    await this.db.close();
    await this.container.stop();
  }
}

// Usage (same as in-memory)
describe("ZakatRepository with PostgreSQL", () => {
  let repository: ZakatRepository;
  let database: TestDatabase;

  beforeEach(async () => {
    database = await TestDatabase.create();
    await database.migrate();
    repository = new ZakatRepository(database);
  });

  afterEach(async () => {
    await database.cleanup();
  });

  it("should save assessment with PostgreSQL-specific types", async () => {
    const assessment = buildZakatAssessment();
    await repository.save(assessment);

    const retrieved = await repository.findById(assessment.id);
    expect(retrieved).toBeDefined();
  });
});
```

**Pros**: Real database (catches production bugs), uses actual migrations
**Cons**: Slower (~2-5s startup), requires Docker

### Repository Testing Pattern

```typescript
// GOOD: Comprehensive repository integration test
describe("MurabahaContractRepository Integration", () => {
  let repository: MurabahaContractRepository;
  let database: TestDatabase;

  beforeEach(async () => {
    database = await TestDatabase.create();
    await database.migrate();
    repository = new MurabahaContractRepository(database);
  });

  afterEach(async () => {
    await database.cleanup();
  });

  describe("save", () => {
    it("should persist contract with all properties", async () => {
      const contract = buildMurabahaContract()
        .withAssetPrice(Money.usd(50000))
        .withMarkup(Money.usd(2500))
        .withTermMonths(12)
        .build();

      await repository.save(contract);

      const retrieved = await repository.findById(contract.id);
      expect(retrieved).toBeDefined();
      expect(retrieved!.assetPrice).toEqualMoney(contract.assetPrice);
      expect(retrieved!.markup).toEqualMoney(contract.markup);
      expect(retrieved!.termMonths).toBe(12);
    });

    it("should persist installments as child entities", async () => {
      const contract = buildMurabahaContract()
        .withInstallments([
          buildInstallment({ dueDate: new Date("2024-02-01"), amount: Money.usd(4375) }),
          buildInstallment({ dueDate: new Date("2024-03-01"), amount: Money.usd(4375) }),
        ])
        .build();

      await repository.save(contract);

      const retrieved = await repository.findById(contract.id);
      expect(retrieved!.installments).toHaveLength(2);
      expect(retrieved!.installments[0].amount).toEqualMoney(Money.usd(4375));
    });

    it("should throw on duplicate ID", async () => {
      const contract = buildMurabahaContract();
      await repository.save(contract);

      await expect(repository.save(contract)).rejects.toThrow("Contract already exists");
    });
  });

  describe("findById", () => {
    it("should return undefined when not found", async () => {
      const result = await repository.findById(ContractId.generate());

      expect(result).toBeUndefined();
    });

    it("should retrieve saved contract", async () => {
      const contract = buildMurabahaContract();
      await repository.save(contract);

      const retrieved = await repository.findById(contract.id);

      expect(retrieved).toBeDefined();
      expect(retrieved!.id.equals(contract.id)).toBe(true);
    });
  });

  describe("findByStatus", () => {
    it("should filter contracts by status", async () => {
      await repository.save(buildMurabahaContract().withStatus("ACTIVE").build());
      await repository.save(buildMurabahaContract().withStatus("ACTIVE").build());
      await repository.save(buildMurabahaContract().withStatus("COMPLETED").build());

      const active = await repository.findByStatus("ACTIVE");

      expect(active).toHaveLength(2);
      active.forEach((contract) => {
        expect(contract.status).toBe("ACTIVE");
      });
    });

    it("should return empty array when no matches", async () => {
      const pending = await repository.findByStatus("PENDING");

      expect(pending).toEqual([]);
    });
  });

  describe("update", () => {
    it("should update contract status", async () => {
      const contract = buildMurabahaContract().withStatus("PENDING").build();
      await repository.save(contract);

      contract.activate(); // Domain method changes status
      await repository.update(contract);

      const updated = await repository.findById(contract.id);
      expect(updated!.status).toBe("ACTIVE");
    });
  });

  describe("transactions", () => {
    it("should rollback on error", async () => {
      const contract = buildMurabahaContract();

      await expect(async () => {
        await repository.saveWithTransaction(async (tx) => {
          await tx.save(contract);
          throw new Error("Simulated error");
        });
      }).rejects.toThrow();

      const retrieved = await repository.findById(contract.id);
      expect(retrieved).toBeUndefined(); // Rolled back ✅
    });

    it("should commit on success", async () => {
      const contract = buildMurabahaContract();

      await repository.saveWithTransaction(async (tx) => {
        await tx.save(contract);
      });

      const retrieved = await repository.findById(contract.id);
      expect(retrieved).toBeDefined(); // Committed ✅
    });
  });
});
```

## API Integration Testing

### Testing HTTP Clients

```typescript
// GOOD: Test real HTTP integration with test server
describe("GoldPriceService Integration", () => {
  let service: GoldPriceService;
  let testServer: TestHttpServer;

  beforeEach(async () => {
    testServer = await TestHttpServer.start();
    service = new GoldPriceService(testServer.url);
  });

  afterEach(async () => {
    await testServer.stop();
  });

  it("should fetch current gold price", async () => {
    testServer.mock({
      method: "GET",
      path: "/api/gold/price",
      response: {
        status: 200,
        body: {
          pricePerGram: 65.5,
          currency: "USD",
          timestamp: "2024-01-15T10:00:00Z",
        },
      },
    });

    const price = await service.getCurrentPrice();

    expect(price.pricePerGram).toBeCloseTo(65.5, 2);
    expect(price.currency).toBe("USD");
  });

  it("should handle API errors gracefully", async () => {
    testServer.mock({
      method: "GET",
      path: "/api/gold/price",
      response: {
        status: 500,
        body: { error: "Internal Server Error" },
      },
    });

    await expect(service.getCurrentPrice()).rejects.toThrow("Failed to fetch gold price");
  });

  it("should handle network timeouts", async () => {
    testServer.mock({
      method: "GET",
      path: "/api/gold/price",
      delay: 5000, // 5 second delay
    });

    await expect(service.getCurrentPrice()).rejects.toThrow("Request timeout");
  });

  it("should parse response correctly", async () => {
    testServer.mock({
      method: "GET",
      path: "/api/gold/price",
      response: {
        status: 200,
        body: {
          pricePerGram: 65.5,
          currency: "USD",
          timestamp: "2024-01-15T10:00:00Z",
        },
      },
    });

    const price = await service.getCurrentPrice();

    expect(price).toBeInstanceOf(GoldPrice);
    expect(price.timestamp).toBeInstanceOf(Date);
  });
});
```

### Testing with Real External APIs (Contract Tests)

```typescript
// GOOD: Contract test against real API (run less frequently)
describe("GoldPriceService Contract Test", () => {
  let service: GoldPriceService;

  beforeEach(() => {
    service = new GoldPriceService(process.env.GOLD_API_URL!);
  });

  it("should respect API contract", async () => {
    const price = await service.getCurrentPrice();

    // Verify response structure
    expect(price).toHaveProperty("pricePerGram");
    expect(price).toHaveProperty("currency");
    expect(price).toHaveProperty("timestamp");

    // Verify types
    expect(typeof price.pricePerGram).toBe("number");
    expect(price.pricePerGram).toBeGreaterThan(0);
    expect(price.currency).toBe("USD");
    expect(price.timestamp).toBeInstanceOf(Date);
  });
});

// Run contract tests separately
// jest --testMatch="**/*.contract.spec.ts"
```

## Message Queue Integration Testing

### Testing Event Publishing

```typescript
// GOOD: Test event publishing to real message queue
describe("ZakatEventPublisher Integration", () => {
  let publisher: ZakatEventPublisher;
  let messageQueue: TestMessageQueue;

  beforeEach(async () => {
    messageQueue = await TestMessageQueue.create();
    publisher = new ZakatEventPublisher(messageQueue);
  });

  afterEach(async () => {
    await messageQueue.cleanup();
  });

  it("should publish ZakatCalculated event", async () => {
    const event = new ZakatCalculated(AssessmentId.generate(), Money.usd(1000), Money.usd(25), new Date());

    await publisher.publish(event);

    const messages = await messageQueue.getMessages("zakat.calculated");
    expect(messages).toHaveLength(1);
    expect(messages[0].eventType).toBe("ZakatCalculated");
    expect(messages[0].payload.zakatAmount.amount).toBe(25);
  });

  it("should retry on transient failures", async () => {
    messageQueue.simulateFailure(2); // Fail first 2 attempts

    const event = new ZakatCalculated(AssessmentId.generate(), Money.usd(1000), Money.usd(25), new Date());

    await publisher.publish(event);

    const messages = await messageQueue.getMessages("zakat.calculated");
    expect(messages).toHaveLength(1); // Eventually succeeds ✅
    expect(messageQueue.getAttemptCount()).toBe(3); // 2 failures + 1 success
  });
});
```

### Testing Event Consumption

```typescript
// GOOD: Test event consumer
describe("ZakatNotificationHandler Integration", () => {
  let handler: ZakatNotificationHandler;
  let messageQueue: TestMessageQueue;
  let notificationService: FakeNotificationService;

  beforeEach(async () => {
    messageQueue = await TestMessageQueue.create();
    notificationService = new FakeNotificationService();
    handler = new ZakatNotificationHandler(notificationService);
  });

  afterEach(async () => {
    await messageQueue.cleanup();
  });

  it("should send notification on ZakatCalculated event", async () => {
    const event = {
      eventType: "ZakatCalculated",
      payload: {
        assessmentId: "ASSESS-001",
        zakatAmount: { amount: 25, currency: "USD" },
      },
    };

    await handler.handle(event);

    const sentNotifications = notificationService.getSentNotifications();
    expect(sentNotifications).toHaveLength(1);
    expect(sentNotifications[0].subject).toContain("Zakat Calculated");
    expect(sentNotifications[0].body).toContain("$25.00");
  });

  it("should send to dead letter queue on persistent failure", async () => {
    notificationService.alwaysFail();

    const event = {
      eventType: "ZakatCalculated",
      payload: {
        assessmentId: "ASSESS-001",
        zakatAmount: { amount: 25, currency: "USD" },
      },
    };

    await handler.handle(event);

    const dlqMessages = await messageQueue.getMessages("dead-letter-queue");
    expect(dlqMessages).toHaveLength(1);
  });
});
```

## Contract Testing for Bounded Contexts

### Consumer-Driven Contracts

**Contract testing** verifies that services can communicate without running full integration tests.

```typescript
// Consumer side: Zakat Assessment Context
describe("Zakat to Notification Contract - Consumer", () => {
  it("should define expected ZakatCalculated event", () => {
    const expectedContract = {
      eventType: "ZakatCalculated",
      payload: {
        assessmentId: expect.any(String),
        userId: expect.any(String),
        zakatAmount: {
          amount: expect.any(Number),
          currency: expect.stringMatching(/^[A-Z]{3}$/),
        },
        calculatedAt: expect.any(String), // ISO 8601 date
      },
    };

    const actualEvent = new ZakatCalculated(
      AssessmentId.of("ASSESS-001"),
      UserId.of("USER-001"),
      Money.usd(25),
      new Date("2024-01-15T10:00:00Z"),
    );

    expect(actualEvent.toJSON()).toMatchObject(expectedContract);
  });
});

// Provider side: Notification Context
describe("Zakat to Notification Contract - Provider", () => {
  it("should consume ZakatCalculated event", () => {
    const event = {
      eventType: "ZakatCalculated",
      payload: {
        assessmentId: "ASSESS-001",
        userId: "USER-001",
        zakatAmount: {
          amount: 25,
          currency: "USD",
        },
        calculatedAt: "2024-01-15T10:00:00Z",
      },
    };

    // Verify Notification context can parse this event
    const handler = new ZakatNotificationHandler();
    expect(() => handler.handle(event)).not.toThrow();
  });
});
```

### Pact Testing (Advanced)

```typescript
import { Pact } from "@pact-foundation/pact";

// Consumer: Zakat Assessment Service
describe("Zakat Pact with Gold Price API", () => {
  const provider = new Pact({
    consumer: "ZakatAssessmentService",
    provider: "GoldPriceAPI",
    port: 1234,
  });

  beforeAll(() => provider.setup());
  afterEach(() => provider.verify());
  afterAll(() => provider.finalize());

  it("should get current gold price", async () => {
    await provider.addInteraction({
      state: "gold price is available",
      uponReceiving: "a request for current gold price",
      withRequest: {
        method: "GET",
        path: "/api/gold/price",
      },
      willRespondWith: {
        status: 200,
        headers: { "Content-Type": "application/json" },
        body: {
          pricePerGram: 65.5,
          currency: "USD",
          timestamp: "2024-01-15T10:00:00Z",
        },
      },
    });

    const service = new GoldPriceService("http://localhost:1234");
    const price = await service.getCurrentPrice();

    expect(price.pricePerGram).toBeCloseTo(65.5, 2);
  });
});
```

## Test Data Management

### Test Data Builders for Integration Tests

```typescript
// GOOD: Use builders for complex integration test data
describe("TakafulClaimProcessing Integration", () => {
  let repository: TakafulClaimRepository;
  let database: TestDatabase;

  beforeEach(async () => {
    database = await TestDatabase.create();
    await database.migrate();
    repository = new TakafulClaimRepository(database);
  });

  it("should process claim with full workflow", async () => {
    // Arrange: Build complex test data
    const policy = buildTakafulPolicy().withCoverageAmount(Money.usd(100000)).withStatus("ACTIVE").build();

    const claim = buildTakafulClaim()
      .forPolicy(policy)
      .withAmount(Money.usd(5000))
      .withReason("Medical emergency")
      .withSupportingDocuments([buildDocument({ type: "MEDICAL_REPORT" }), buildDocument({ type: "INVOICE" })])
      .build();

    // Act: Save and process
    await repository.save(claim);
    await repository.approve(claim.id);

    // Assert: Verify state
    const processed = await repository.findById(claim.id);
    expect(processed!.status).toBe("APPROVED");
  });
});
```

### Database Fixtures

```typescript
// GOOD: Fixture functions for common test scenarios
class TakafulTestFixtures {
  static async createActivePolicy(db: TestDatabase): Promise<TakafulPolicy> {
    const policy = buildTakafulPolicy().withStatus("ACTIVE").withCoverageAmount(Money.usd(100000)).build();

    await db.insert("takaful_policies", policy);
    return policy;
  }

  static async createPendingClaim(db: TestDatabase, policy: TakafulPolicy): Promise<TakafulClaim> {
    const claim = buildTakafulClaim().forPolicy(policy).withStatus("PENDING").build();

    await db.insert("takaful_claims", claim);
    return claim;
  }
}

// Usage
describe("TakafulClaimService Integration", () => {
  it("should approve claim", async () => {
    const policy = await TakafulTestFixtures.createActivePolicy(database);
    const claim = await TakafulTestFixtures.createPendingClaim(database, policy);

    await service.approve(claim.id);

    const approved = await repository.findById(claim.id);
    expect(approved!.status).toBe("APPROVED");
  });
});
```

## Integration Test Organization

### File Structure

```
src/
  zakat/
    domain/
      ZakatCalculator.ts
      ZakatCalculator.spec.ts          # Unit tests
    infrastructure/
      ZakatRepository.ts
      ZakatRepository.integration.spec.ts  # Integration tests
    application/
      ZakatAssessmentService.ts
      ZakatAssessmentService.spec.ts   # Unit tests
      ZakatAssessmentService.integration.spec.ts
```

### Naming Convention

- Unit tests: `*.spec.ts`
- Integration tests: `*.integration.spec.ts`
- Contract tests: `*.contract.spec.ts`
- E2E tests: `*.e2e.spec.ts`

### Running Integration Tests Separately

```json
// package.json
{
  "scripts": {
    "test": "jest",
    "test:unit": "jest --testPathIgnorePatterns=integration",
    "test:integration": "jest --testMatch='**/*.integration.spec.ts'",
    "test:contract": "jest --testMatch='**/*.contract.spec.ts'"
  }
}
```

## Summary

Integration tests verify component collaborations with real dependencies:

**When to Use Integration Tests:**

1. Database persistence (repositories)
2. External API integration
3. Message queue communication
4. Cross-boundary contracts (bounded contexts)
5. File system operations

**Key Patterns:**

- **Test Database**: In-memory (fast) or Test Containers (realistic)
- **Repository Testing**: Save, retrieve, query, transactions
- **API Testing**: Mock servers for fast tests, contract tests for real APIs
- **Message Queues**: Test publishing, consumption, dead letter queues
- **Contract Testing**: Consumer-driven contracts for bounded contexts

**Best Practices:**

- Use test data builders for complex scenarios
- Create fixture functions for common setups
- Run integration tests separately from unit tests
- Keep integration tests focused (test one integration per test)
- Use Test Containers for production-like databases
- Clean up resources in `afterEach`

**Integration Test Characteristics:**

- Slower than unit tests (100ms-5s)
- Use real dependencies (database, message queue)
- Test communication and persistence
- Complement unit tests (different failure modes)
- 20-30% of test suite (testing pyramid)

Integration tests catch bugs that unit tests miss while remaining faster and more debuggable than E2E tests. They're the middle layer of a robust testing strategy.

## Related Documentation

- **[03. Test Types and Testing Pyramid](./ex-sode-tedrdeve__03-test-types-and-pyramid.md)** - Where integration tests fit
- **[04. Unit Testing Fundamentals](./ex-sode-tedrdeve__04-unit-testing-fundamentals.md)** - Contrast with unit tests
- **[05. Test Doubles](./ex-sode-tedrdeve__05-test-doubles.md)** - When to use real vs fake dependencies
- **[07. Test Data Builders](./ex-sode-tedrdeve__07-test-data-builders.md)** - Building complex test data
- **[10. End-to-End Testing](./ex-sode-tedrdeve__10-end-to-end-testing.md)** - Next level up in pyramid
- **[12. TDD and DDD](./ex-sode-tedrdeve__12-tdd-and-ddd.md)** - Testing repositories and bounded contexts
