---
title: "Mocking Dependencies"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000063
description: "Production implementation guide for mocking dependencies"
tags: ["spring-boot", "in-the-field", "production", "mocking"]
---

## Why Mocking Dependencies Matters

Spring Boot @MockBean replaces real beans with Mockito mocks in test ApplicationContext. In integration tests requiring external API mocking (payment gateways, email services), @MockBean eliminates TestConfiguration boilerplate—injecting mocks into Spring context while maintaining autowiring and dependency injection for focused testing.

**Problem**: Manual Mockito setup requires explicit mock creation and Spring context configuration.

**Solution**: @MockBean annotation replaces beans in test ApplicationContext.

## Implementation Example

```java
@SpringBootTest
class DonationServiceTest {

    @Autowired
    private DonationService donationService;

    @MockBean  // => Replaces real ReceiptService with Mockito mock
    private ReceiptService receiptService;

    @Test
    void shouldProcessDonation() {
        // => Configure mock behavior
        when(receiptService.generateReceipt(any()))
            .thenReturn(new Receipt("RCP-001"));

        DonationResponse response = donationService.processDonation(request);

        // => Verify mock interaction
        verify(receiptService).generateReceipt(any());
        assertThat(response.getReceiptNumber()).isEqualTo("RCP-001");
    }
}
```

## Production Configuration

```yaml
# Configuration for mocking-dependencies
# See full guide for detailed configuration
```

## Production Patterns

**Best Practices**:

- Follow Spring Boot conventions
- Test in staging before production
- Monitor metrics and health checks
- Use environment-specific configuration

## Trade-offs

| Aspect               | Spring Boot Approach       | Manual Approach                |
| -------------------- | -------------------------- | ------------------------------ |
| **Complexity**       | Auto-configured (simple)   | Manual configuration (complex) |
| **Flexibility**      | Conventions with overrides | Full control                   |
| **Maintenance**      | Framework-maintained       | Custom code maintenance        |
| **Production ready** | Defaults optimized         | Requires tuning                |

**Production recommendation**: Use Spring Boot auto-configuration as default. Manual configuration only for edge cases.

## Next Steps

- See related in-the-field guides for comprehensive production patterns
