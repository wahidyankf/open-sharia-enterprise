---
title: "Spring Boot Testing"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000060
description: "@ContextConfiguration → @SpringBootTest for auto-configured integration testing"
tags: ["spring-boot", "in-the-field", "production", "testing", "integration-test"]
---

## Why Spring Boot Test Matters

Spring Boot @SpringBootTest auto-configures test ApplicationContext, eliminating manual @ContextConfiguration and @EnableAutoConfiguration. In production test suites with 500+ integration tests, @SpringBootTest reduces test configuration from 50+ lines per test class to single annotation—while providing embedded databases, mock web servers, and test property overrides.

**Solution**: @SpringBootTest with auto-configured test context.

```java
@SpringBootTest  // => Loads full ApplicationContext with auto-configuration
// => Equivalent to running SpringApplication.run() in tests
class DonationServiceIntegrationTest {

    @Autowired
    private DonationService donationService;  // => Real service with all dependencies

    @Autowired
    private DonationRepository repository;  // => Real repository with embedded database

    @Test
    void shouldProcessDonation() {
        DonationRequest request = new DonationRequest();
        request.setAmount(BigDecimal.valueOf(1000));
        request.setDonorName("Ahmad");

        DonationResponse response = donationService.processDonation(request);

        assertThat(response.getId()).isNotNull();
        assertThat(repository.findById(response.getId())).isPresent();
    }
}
```

**Test properties**:

```java
@SpringBootTest(properties = {
    "spring.datasource.url=jdbc:h2:mem:testdb",  // => Override datasource
    "logging.level.org.hibernate.SQL=DEBUG"  // => Enable SQL logging in tests
})
```

**Web environment**:

```java
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
// => Starts embedded server on random port
class DonationControllerIntegrationTest {

    @Autowired
    private TestRestTemplate restTemplate;  // => Auto-configured HTTP client

    @Test
    void shouldCreateDonation() {
        DonationRequest request = new DonationRequest();

        ResponseEntity<DonationResponse> response = restTemplate.postForEntity(
            "/api/donations",
            request,
            DonationResponse.class
        );

        assertThat(response.getStatusCode()).isEqualTo(HttpStatus.CREATED);
    }
}
```

## Next Steps

- [Test Slices](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/test-slices) - @WebMvcTest, @DataJpaTest
- [Testcontainers](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/testcontainers) - Real database testing
