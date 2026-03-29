---
title: "Test Slices"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000061
description: "Full @SpringBootTest → @WebMvcTest/@DataJpaTest slices for faster focused testing"
tags: ["spring-boot", "in-the-field", "production", "testing", "test-slices"]
---

## Why Test Slices Matter

Spring Boot test slices (@WebMvcTest, @DataJpaTest) load minimal ApplicationContext for specific layers, reducing test startup from 10+ seconds (@SpringBootTest full context) to <2 seconds (slice). In production test suites with 1000+ tests, slice testing saves 2+ hours of CI/CD time by loading only required beans (controllers without services, repositories without web layer).

**Solution**: @WebMvcTest for controllers, @DataJpaTest for repositories.

```java
@WebMvcTest(DonationController.class)
// => Only loads web layer: DonationController + Spring MVC infrastructure
// => NO services, repositories, or database
class DonationControllerTest {

    @Autowired
    private MockMvc mockMvc;  // => Test Spring MVC without HTTP

    @MockBean  // => Mock service (not loaded in web slice)
    private DonationService donationService;

    @Test
    void shouldReturnDonation() throws Exception {
        when(donationService.findById(1L))
            .thenReturn(new DonationResponse(1L, BigDecimal.valueOf(1000)));

        mockMvc.perform(get("/api/donations/1"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").value(1))
            .andExpect(jsonPath("$.amount").value(1000));
    }
}
```

**@DataJpaTest** (repository slice):

```java
@DataJpaTest  // => Only loads JPA infrastructure: repositories + embedded database
// => NO controllers, services, or web layer
class DonationRepositoryTest {

    @Autowired
    private DonationRepository repository;  // => Real repository

    @Autowired
    private TestEntityManager entityManager;  // => Test helper for JPA

    @Test
    void shouldFindByDonorName() {
        ZakatDonation donation = new ZakatDonation();
        donation.setDonorName("Ahmad");
        donation.setAmount(BigDecimal.valueOf(1000));
        entityManager.persist(donation);

        List<ZakatDonation> found = repository.findByDonorName("Ahmad");

        assertThat(found).hasSize(1);
        assertThat(found.get(0).getAmount()).isEqualTo(BigDecimal.valueOf(1000));
    }
}
```

**Performance comparison**:

| Test Type       | Startup Time | Beans Loaded         | Use Case               |
| --------------- | ------------ | -------------------- | ---------------------- |
| @SpringBootTest | 10-15s       | All (~500)           | End-to-end integration |
| @WebMvcTest     | <2s          | Web layer only (~50) | Controller logic       |
| @DataJpaTest    | <2s          | JPA layer only (~30) | Repository queries     |

**Trade-offs**: Test slices for fast feedback (unit-like speed for integration tests). @SpringBootTest for critical flows.

## Next Steps

- [Spring Boot Test](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/spring-boot-test) - Full integration testing
- [Mocking Dependencies](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/mocking-dependencies) - @MockBean patterns
