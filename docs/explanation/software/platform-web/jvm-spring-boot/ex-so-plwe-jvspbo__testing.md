---
title: "Spring Boot Testing"
description: Testing strategies for Spring Boot applications
category: explanation
subcategory: platform-web
tags:
  - spring-boot
  - testing
  - junit
  - mockito
  - testcontainers
related:
  - ./ex-so-plwe-jvspbo__best-practices.md
last_updated: 2026-01-25
---

# Spring Boot Testing

## Overview

Comprehensive testing guide covering unit tests, integration tests, and test slices for Spring Boot applications.

See [Best Practices - Testing](./ex-so-plwe-jvspbo__best-practices.md#testing-strategy) for detailed testing approaches.

## Key Topics

- **Unit Tests** - Plain JUnit (no Spring context)
- **@WebMvcTest** - Controller testing
- **@DataJpaTest** - Repository testing
- **@SpringBootTest** - Integration testing
- **TestContainers** - Real database testing
- **MockBean** - Mocking dependencies

## Testing Examples

```java
// Unit Test - No Spring
class MoneyTest {
    @Test
    void add_sameCurrency_addsAmounts() {
        Money m1 = new Money(BigDecimal.TEN, "USD");
        Money m2 = new Money(BigDecimal.ONE, "USD");
        assertThat(m1.add(m2).amount()).isEqualByComparingTo("11");
    }
}

// Controller Test - @WebMvcTest
@WebMvcTest(ZakatCalculationController.class)
class ZakatCalculationControllerTest {
    @Autowired private MockMvc mockMvc;
    @MockBean private ZakatCalculationService service;

    @Test
    void calculate_returns200() throws Exception {
        mockMvc.perform(post("/api/v1/zakat/calculate")
                .contentType(MediaType.APPLICATION_JSON)
                .content("{}"))
            .andExpect(status().isOk());
    }
}

// Integration Test - @SpringBootTest + TestContainers
@SpringBootTest
@Testcontainers
class ZakatIntegrationTest {
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16");

    @DynamicPropertySource
    static void configureDb(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
    }
}
```

---

**Last Updated**: 2026-01-25
