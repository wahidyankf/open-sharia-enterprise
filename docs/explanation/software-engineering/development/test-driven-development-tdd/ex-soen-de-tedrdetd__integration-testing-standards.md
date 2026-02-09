---
title: "Integration Testing Standards"
description: OSE Platform standards for repository, database, and API integration testing
category: explanation
subcategory: development
tags:
  - tdd
  - integration-testing
  - test-containers
principles:
  - automation-over-manual
  - reproducibility
created: 2026-02-09
updated: 2026-02-09
---

# Integration Testing Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding TDD By Example](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/development/test-driven-development-tdd/by-example/) before using these standards.

## Purpose

OSE Platform standards for integration testing with databases and external services.

## REQUIRED: Use Test Containers

**REQUIRED**: Use Testcontainers for database integration tests.

**PROHIBITED**: In-memory databases (H2, SQLite) for PostgreSQL testing.

**Java Example**:

```java
@Testcontainers
class DonationRepositoryIntegrationTest {
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16")
        .withDatabaseName("test_db")
        .withUsername("test")
        .withPassword("test");

    private DonationRepository repository;

    @BeforeEach
    void setUp() {
        repository = new PostgresDonationRepository(
            createDataSource(postgres)
        );
    }

    @Test
    void shouldPersistDonation() {
        // Arrange
        Donation donation = Donation.create(
            DonationId.generate(),
            CampaignId.of("CAMPAIGN-001"),
            Money.usd(500)
        );

        // Act
        repository.save(donation);
        Optional<Donation> retrieved = repository.findById(donation.getId());

        // Assert
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get().getAmount())
            .isEqualTo(Money.usd(500));
    }
}
```

## Separate Unit and Integration Tests

**REQUIRED**: Separate integration tests from unit tests.

**Directory Structure:**

```
src/
  test/
    unit/
      DonationServiceTest.java          # Fast unit tests
    integration/
      DonationRepositoryIntegrationTest.java  # Database tests
```

**Naming Convention:**

- Integration tests: `*IntegrationTest.java`, `*.integration.spec.ts`

## Transaction Management

**REQUIRED**: Rollback transactions after each integration test.

```java
@SpringBootTest
@Transactional
class ZakatAssessmentRepositoryIntegrationTest {
    @Autowired
    private ZakatAssessmentRepository repository;

    @Test
    void shouldSaveAssessment() {
        ZakatAssessment assessment = buildAssessment();

        repository.save(assessment);

        // Transaction automatically rolled back after test
    }
}
```

## OSE Platform Examples

### Repository Integration Test

```typescript
describe("CampaignRepository (Integration)", () => {
  let container: StartedPostgreSqlContainer;
  let repository: CampaignRepository;

  beforeAll(async () => {
    container = await new PostgreSqlContainer().withDatabase("test_db").start();

    repository = new PostgresCampaignRepository(createDataSource(container));
  });

  afterAll(async () => {
    await container.stop();
  });

  it("should persist campaign with goal", async () => {
    // Arrange
    const campaign = Campaign.create(CampaignId.generate(), "Build School", Money.usd(50000));

    // Act
    await repository.save(campaign);
    const retrieved = await repository.findById(campaign.id);

    // Assert
    expect(retrieved).toBeDefined();
    expect(retrieved!.goal.equals(Money.usd(50000))).toBe(true);
  });
});
```

### API Integration Test

```java
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class ZakatApiIntegrationTest {
    @Autowired
    private TestRestTemplate restTemplate;

    @Test
    void shouldCalculateZakat() {
        // Arrange
        ZakatRequest request = new ZakatRequest(
            BigDecimal.valueOf(100000),
            "USD"
        );

        // Act
        ResponseEntity<ZakatResponse> response = restTemplate.postForEntity(
            "/api/zakat/calculate",
            request,
            ZakatResponse.class
        );

        // Assert
        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.OK);
        assertThat(response.getBody().getZakatAmount())
            .isEqualTo(BigDecimal.valueOf(2500));
    }
}
```

---

**Last Updated**: 2026-02-09
