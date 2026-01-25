---
title: Java Integration Test Template
description: Template for creating integration tests in Java using Spring Boot Test, TestContainers, and REST Assured
category: template
tags:
  - java
  - testing
  - integration-test
  - spring-boot
  - testcontainers
  - rest-assured
  - database
related:
  - ex-so-prla-ja__test-driven-development.md
  - ex-so-de-tedrdeve-te__integration-test-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
  - automation-over-manual
last_updated: 2026-01-21
---

# Java Integration Test Template

This template provides a standardized structure for writing integration tests in Java using Spring Boot Test, TestContainers for infrastructure dependencies, and REST Assured for API testing.

## Template Structure

```java
package com.openshariaenterprise.{domain}.integration;

import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import static io.restassured.RestAssured.*;
import static org.assertj.core.api.Assertions.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Integration tests for {@link FeatureUnderTest}.
 *
 * <p>Test Coverage:
 * <ul>
 *   <li>End-to-end API workflows</li>
 *   <li>Database persistence and retrieval</li>
 *   <li>External service integration</li>
 *   <li>Transaction management</li>
 *   <li>Security and authorization</li>
 * </ul>
 *
 * @see FeatureUnderTest
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DisplayName("Feature Integration Tests")
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class FeatureIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired
    private EntityRepository repository;

    @Autowired
    private TestDataFactory testDataFactory;

    // TestContainers
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine")
        .withDatabaseName("testdb")
        .withUsername("test")
        .withPassword("test")
        .withInitScript("db/init-test-schema.sql");

    @Container
    static GenericContainer<?> redis = new GenericContainer<>("redis:7-alpine")
        .withExposedPorts(6379);

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
        registry.add("spring.redis.host", redis::getHost);
        registry.add("spring.redis.port", redis::getFirstMappedPort);
    }

    @BeforeAll
    static void setUpAll() {
        // One-time setup for all tests
    }

    @BeforeEach
    void setUp() {
        RestAssured.port = port;
        RestAssured.basePath = "/api/v1";

        // Clean database before each test
        repository.deleteAll();

        // Seed test data
        testDataFactory.createDefaultTestData();
    }

    @AfterEach
    void tearDown() {
        // Clean up after each test
        repository.deleteAll();
    }

    @AfterAll
    static void tearDownAll() {
        // One-time cleanup
    }

    // ========================================
    // Create Operations
    // ========================================

    @Nested
    @DisplayName("Create Operations")
    class CreateOperationTests {

        @Test
        @Order(1)
        @DisplayName("Should create new entity successfully")
        void testCreateEntity() {
            // Arrange
            var requestBody = """
                {
                    "name": "Test Entity",
                    "amount": 1000.00,
                    "currency": "USD",
                    "status": "ACTIVE"
                }
                """;

            // Act & Assert
            given()
                .contentType(ContentType.JSON)
                .body(requestBody)
            .when()
                .post("/entities")
            .then()
                .statusCode(201)
                .header("Location", notNullValue())
                .body("id", notNullValue())
                .body("name", equalTo("Test Entity"))
                .body("amount", equalTo(1000.00f))
                .body("currency", equalTo("USD"))
                .body("status", equalTo("ACTIVE"))
                .body("createdAt", notNullValue());

            // Verify persistence
            var entities = repository.findAll();
            assertThat(entities)
                .hasSize(1)
                .first()
                .satisfies(entity -> {
                    assertThat(entity.getName()).isEqualTo("Test Entity");
                    assertThat(entity.getAmount()).isEqualByComparingTo(new BigDecimal("1000.00"));
                });
        }

        @Test
        @DisplayName("Should reject creation with invalid data")
        void testCreateWithInvalidData() {
            // Arrange
            var invalidRequestBody = """
                {
                    "name": "",
                    "amount": -100.00,
                    "currency": "INVALID"
                }
                """;

            // Act & Assert
            given()
                .contentType(ContentType.JSON)
                .body(invalidRequestBody)
            .when()
                .post("/entities")
            .then()
                .statusCode(400)
                .body("errors", hasSize(greaterThan(0)))
                .body("errors[0].field", notNullValue())
                .body("errors[0].message", notNullValue());
        }

        @Test
        @DisplayName("Should reject duplicate creation")
        void testCreateDuplicate() {
            // Arrange
            var existingEntity = testDataFactory.createEntity("existing-id");
            repository.save(existingEntity);

            var duplicateRequest = """
                {
                    "id": "existing-id",
                    "name": "Duplicate Entity"
                }
                """;

            // Act & Assert
            given()
                .contentType(ContentType.JSON)
                .body(duplicateRequest)
            .when()
                .post("/entities")
            .then()
                .statusCode(409)
                .body("message", containsString("already exists"));
        }
    }

    // ========================================
    // Read Operations
    // ========================================

    @Nested
    @DisplayName("Read Operations")
    class ReadOperationTests {

        @Test
        @DisplayName("Should retrieve entity by ID")
        void testGetEntityById() {
            // Arrange
            var entity = testDataFactory.createEntity("test-id");
            repository.save(entity);

            // Act & Assert
            given()
            .when()
                .get("/entities/{id}", "test-id")
            .then()
                .statusCode(200)
                .body("id", equalTo("test-id"))
                .body("name", notNullValue())
                .body("amount", notNullValue())
                .body("createdAt", notNullValue());
        }

        @Test
        @DisplayName("Should return 404 for non-existent entity")
        void testGetNonExistentEntity() {
            // Act & Assert
            given()
            .when()
                .get("/entities/{id}", "non-existent-id")
            .then()
                .statusCode(404)
                .body("message", containsString("not found"));
        }

        @Test
        @DisplayName("Should list all entities with pagination")
        void testListEntitiesWithPagination() {
            // Arrange
            var entities = testDataFactory.createMultipleEntities(25);
            repository.saveAll(entities);

            // Act & Assert - First page
            given()
                .queryParam("page", 0)
                .queryParam("size", 10)
            .when()
                .get("/entities")
            .then()
                .statusCode(200)
                .body("content", hasSize(10))
                .body("totalElements", equalTo(25))
                .body("totalPages", equalTo(3))
                .body("number", equalTo(0));

            // Act & Assert - Second page
            given()
                .queryParam("page", 1)
                .queryParam("size", 10)
            .when()
                .get("/entities")
            .then()
                .statusCode(200)
                .body("content", hasSize(10))
                .body("number", equalTo(1));
        }

        @Test
        @DisplayName("Should filter entities by status")
        void testFilterByStatus() {
            // Arrange
            var activeEntities = testDataFactory.createEntitiesWithStatus("ACTIVE", 5);
            var inactiveEntities = testDataFactory.createEntitiesWithStatus("INACTIVE", 3);
            repository.saveAll(activeEntities);
            repository.saveAll(inactiveEntities);

            // Act & Assert
            given()
                .queryParam("status", "ACTIVE")
            .when()
                .get("/entities")
            .then()
                .statusCode(200)
                .body("content", hasSize(5))
                .body("content.status", everyItem(equalTo("ACTIVE")));
        }
    }

    // ========================================
    // Update Operations
    // ========================================

    @Nested
    @DisplayName("Update Operations")
    class UpdateOperationTests {

        @Test
        @DisplayName("Should update entity successfully")
        void testUpdateEntity() {
            // Arrange
            var entity = testDataFactory.createEntity("test-id");
            repository.save(entity);

            var updateRequest = """
                {
                    "name": "Updated Name",
                    "amount": 2000.00,
                    "status": "ACTIVE"
                }
                """;

            // Act & Assert
            given()
                .contentType(ContentType.JSON)
                .body(updateRequest)
            .when()
                .put("/entities/{id}", "test-id")
            .then()
                .statusCode(200)
                .body("name", equalTo("Updated Name"))
                .body("amount", equalTo(2000.00f))
                .body("updatedAt", notNullValue());

            // Verify persistence
            var updated = repository.findById("test-id").orElseThrow();
            assertThat(updated.getName()).isEqualTo("Updated Name");
            assertThat(updated.getAmount()).isEqualByComparingTo(new BigDecimal("2000.00"));
        }

        @Test
        @DisplayName("Should partially update entity with PATCH")
        void testPartialUpdate() {
            // Arrange
            var entity = testDataFactory.createEntity("test-id");
            entity.setName("Original Name");
            entity.setAmount(new BigDecimal("1000.00"));
            repository.save(entity);

            var patchRequest = """
                {
                    "amount": 1500.00
                }
                """;

            // Act & Assert
            given()
                .contentType(ContentType.JSON)
                .body(patchRequest)
            .when()
                .patch("/entities/{id}", "test-id")
            .then()
                .statusCode(200)
                .body("name", equalTo("Original Name"))
                .body("amount", equalTo(1500.00f));
        }
    }

    // ========================================
    // Delete Operations
    // ========================================

    @Nested
    @DisplayName("Delete Operations")
    class DeleteOperationTests {

        @Test
        @DisplayName("Should delete entity successfully")
        void testDeleteEntity() {
            // Arrange
            var entity = testDataFactory.createEntity("test-id");
            repository.save(entity);

            // Act & Assert
            given()
            .when()
                .delete("/entities/{id}", "test-id")
            .then()
                .statusCode(204);

            // Verify deletion
            assertThat(repository.findById("test-id")).isEmpty();
        }

        @Test
        @DisplayName("Should return 404 when deleting non-existent entity")
        void testDeleteNonExistent() {
            // Act & Assert
            given()
            .when()
                .delete("/entities/{id}", "non-existent")
            .then()
                .statusCode(404);
        }
    }

    // ========================================
    // Business Workflow Tests
    // ========================================

    @Nested
    @DisplayName("Business Workflow")
    class BusinessWorkflowTests {

        @Test
        @DisplayName("Should complete full entity lifecycle")
        void testFullEntityLifecycle() {
            // 1. Create
            var createResponse = given()
                .contentType(ContentType.JSON)
                .body("""
                    {
                        "name": "Lifecycle Test",
                        "amount": 5000.00,
                        "currency": "USD"
                    }
                    """)
            .when()
                .post("/entities")
            .then()
                .statusCode(201)
                .extract()
                .jsonPath();

            var entityId = createResponse.getString("id");

            // 2. Read
            given()
            .when()
                .get("/entities/{id}", entityId)
            .then()
                .statusCode(200)
                .body("name", equalTo("Lifecycle Test"));

            // 3. Update
            given()
                .contentType(ContentType.JSON)
                .body("""
                    {
                        "name": "Updated Lifecycle Test",
                        "amount": 7500.00
                    }
                    """)
            .when()
                .put("/entities/{id}", entityId)
            .then()
                .statusCode(200)
                .body("name", equalTo("Updated Lifecycle Test"));

            // 4. Delete
            given()
            .when()
                .delete("/entities/{id}", entityId)
            .then()
                .statusCode(204);

            // 5. Verify deletion
            given()
            .when()
                .get("/entities/{id}", entityId)
            .then()
                .statusCode(404);
        }
    }

    // ========================================
    // Transaction Tests
    // ========================================

    @Nested
    @DisplayName("Transaction Management")
    class TransactionTests {

        @Test
        @DisplayName("Should rollback transaction on error")
        void testTransactionRollback() {
            // Arrange
            var validEntity = testDataFactory.createValidEntity();

            // Act - Trigger error mid-transaction
            given()
                .contentType(ContentType.JSON)
                .body(validEntity)
                .queryParam("simulateError", true)
            .when()
                .post("/entities/batch")
            .then()
                .statusCode(500);

            // Assert - Verify no data persisted
            assertThat(repository.findAll()).isEmpty();
        }
    }

    // ========================================
    // Security Tests
    // ========================================

    @Nested
    @DisplayName("Security and Authorization")
    class SecurityTests {

        @Test
        @DisplayName("Should require authentication")
        void testAuthenticationRequired() {
            // Act & Assert
            given()
                .header("Authorization", "")
            .when()
                .get("/entities")
            .then()
                .statusCode(401);
        }

        @Test
        @DisplayName("Should enforce role-based access control")
        void testRoleBasedAccess() {
            // Arrange
            var userToken = testDataFactory.generateUserToken("USER");

            // Act & Assert - User cannot delete
            given()
                .header("Authorization", "Bearer " + userToken)
            .when()
                .delete("/entities/{id}", "test-id")
            .then()
                .statusCode(403);
        }

        @Test
        @DisplayName("Should allow admin full access")
        void testAdminAccess() {
            // Arrange
            var adminToken = testDataFactory.generateAdminToken();
            var entity = testDataFactory.createEntity("test-id");
            repository.save(entity);

            // Act & Assert
            given()
                .header("Authorization", "Bearer " + adminToken)
            .when()
                .delete("/entities/{id}", "test-id")
            .then()
                .statusCode(204);
        }
    }
}
```

## Financial Domain Example: Zakat Payment Integration

```java
package com.openshariaenterprise.zakat.payment.integration;

import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import io.restassured.RestAssured;
import static io.restassured.RestAssured.*;
import static org.assertj.core.api.Assertions.*;
import static org.hamcrest.Matchers.*;

/**
 * Integration tests for Zakat payment processing.
 *
 * <p>Tests complete Zakat payment workflow:
 * <ul>
 *   <li>Calculate Zakat for account</li>
 *   <li>Process payment</li>
 *   <li>Record transaction</li>
 *   <li>Update account balance</li>
 *   <li>Generate receipt</li>
 * </ul>
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DisplayName("Zakat Payment Integration Tests")
class ZakatPaymentIntegrationTest {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine")
        .withDatabaseName("zakat_test")
        .withUsername("test")
        .withPassword("test");

    @Autowired
    private ZakatAccountRepository accountRepository;

    @Autowired
    private ZakatPaymentRepository paymentRepository;

    @BeforeEach
    void setUp() {
        accountRepository.deleteAll();
        paymentRepository.deleteAll();
    }

    @Test
    @DisplayName("Should process complete zakat payment workflow")
    void testCompleteZakatPaymentWorkflow() {
        // 1. Create Zakat account
        var createAccountResponse = given()
            .contentType("application/json")
            .body("""
                {
                    "accountHolder": "Ahmad bin Abdullah",
                    "balance": 100000.00,
                    "currency": "USD",
                    "accountOpenDate": "2025-01-01"
                }
                """)
        .when()
            .post("/api/v1/zakat/accounts")
        .then()
            .statusCode(201)
            .extract()
            .jsonPath();

        var accountId = createAccountResponse.getString("id");

        // 2. Calculate Zakat
        var calculationResponse = given()
        .when()
            .get("/api/v1/zakat/accounts/{id}/calculate", accountId)
        .then()
            .statusCode(200)
            .body("zakatDue", equalTo(2500.00f))
            .body("nisab", equalTo(5000.00f))
            .body("eligible", equalTo(true))
            .extract()
            .jsonPath();

        var zakatAmount = calculationResponse.getFloat("zakatDue");

        // 3. Process payment
        var paymentResponse = given()
            .contentType("application/json")
            .body(String.format("""
                {
                    "accountId": "%s",
                    "amount": %.2f,
                    "currency": "USD",
                    "paymentMethod": "BANK_TRANSFER",
                    "recipient": "Local Zakat Foundation"
                }
                """, accountId, zakatAmount))
        .when()
            .post("/api/v1/zakat/payments")
        .then()
            .statusCode(201)
            .body("status", equalTo("COMPLETED"))
            .body("amount", equalTo(2500.00f))
            .body("transactionId", notNullValue())
            .extract()
            .jsonPath();

        var paymentId = paymentResponse.getString("id");

        // 4. Verify account updated
        given()
        .when()
            .get("/api/v1/zakat/accounts/{id}", accountId)
        .then()
            .statusCode(200)
            .body("balance", equalTo(97500.00f))
            .body("lastZakatPaymentDate", notNullValue());

        // 5. Verify payment recorded
        given()
        .when()
            .get("/api/v1/zakat/payments/{id}", paymentId)
        .then()
            .statusCode(200)
            .body("status", equalTo("COMPLETED"))
            .body("amount", equalTo(2500.00f));

        // 6. Get receipt
        given()
        .when()
            .get("/api/v1/zakat/payments/{id}/receipt", paymentId)
        .then()
            .statusCode(200)
            .contentType("application/pdf");
    }

    @Test
    @DisplayName("Should reject zakat payment when balance below nisab")
    void testRejectPaymentBelowNisab() {
        // Arrange - Create account below nisab
        var accountResponse = given()
            .contentType("application/json")
            .body("""
                {
                    "accountHolder": "Fatimah binti Omar",
                    "balance": 3000.00,
                    "currency": "USD"
                }
                """)
        .when()
            .post("/api/v1/zakat/accounts")
        .then()
            .statusCode(201)
            .extract()
            .jsonPath();

        var accountId = accountResponse.getString("id");

        // Act & Assert - Attempt payment
        given()
            .contentType("application/json")
            .body(String.format("""
                {
                    "accountId": "%s",
                    "amount": 75.00,
                    "currency": "USD"
                }
                """, accountId))
        .when()
            .post("/api/v1/zakat/payments")
        .then()
            .statusCode(400)
            .body("message", containsString("below nisab"));
    }
}
```

## Usage Guidelines

1. **TestContainers**: Use real database instances for accurate integration testing
2. **Test Data**: Clean and seed test data before each test for isolation
3. **REST Assured**: Use fluent API for readable HTTP testing
4. **Full Workflows**: Test complete business workflows, not just individual operations
5. **Security**: Test authentication and authorization requirements
6. **Transactions**: Verify transactional behavior and rollback scenarios
7. **Performance**: Consider using `@Timeout` for integration tests that should complete quickly

## Related Templates

- [Unit Test Template](./ex-so-prla-ja-te__unit-test-template.md)
- [BDD Step Definition Template](./ex-so-prla-ja-te__bdd-step-definition-template.md)

## See Also

- [Test-Driven Development](../ex-so-prla-ja__test-driven-development.md)
- [TestContainers Documentation](https://testcontainers.com/)
- [REST Assured Documentation](https://rest-assured.io/)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit, Automation Over Manual

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
