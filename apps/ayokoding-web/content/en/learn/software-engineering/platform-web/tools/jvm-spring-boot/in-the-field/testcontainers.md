---
title: "Testcontainers"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000062
description: "Manual Docker setup → Testcontainers for real database integration testing"
tags: ["spring-boot", "in-the-field", "production", "testing", "testcontainers"]
---

## Why Testcontainers Matters

Testcontainers eliminates production/test database differences by running real PostgreSQL in Docker during tests. In production systems where H2 in-memory database differs from PostgreSQL (UUID types, JSONB, array types), Testcontainers catches 90% of database-specific bugs before production—testing against identical database engine used in production.

**Solution**: Testcontainers with Spring Boot @DynamicPropertySource.

```java
@SpringBootTest
@Testcontainers  // => Manages Docker containers lifecycle
class DonationServicePostgresTest {

    @Container  // => Starts PostgreSQL container before tests
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15")
        .withDatabaseName("zakat_test")
        .withUsername("test")
        .withPassword("test");

    @DynamicPropertySource  // => Override Spring datasource properties
    static void registerPgProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
    }

    @Autowired
    private DonationService donationService;

    @Test
    void shouldHandlePostgresSpecificTypes() {
        // => Test PostgreSQL UUID, JSONB, array types
        // => Real PostgreSQL behavior, not H2 simulation
    }
}
```

**Benefits over H2**:

- ✅ Real PostgreSQL (not simulation)
- ✅ Tests database-specific features (JSONB, arrays, window functions)
- ✅ Catches SQL dialect differences before production
- ✅ Identical to production database version

**Trade-offs**: Testcontainers adds 5-10s startup (Docker). Acceptable for integration tests, not unit tests.

## Next Steps

- [Spring Boot Test](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/spring-boot-test) - Integration testing patterns
- [Test Slices](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/test-slices) - Faster slice testing
