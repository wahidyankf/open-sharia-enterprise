---
title: "REST API Development"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000031
description: "Production implementation guide for rest api development"
tags: ["spring-boot", "in-the-field", "production", "rest"]
---

## Why Rest Api Development Matters

Spring Boot @RestController provides auto-configured REST API development with Jackson serialization, content negotiation, and exception handling. In production APIs serving millions of requests, Boot's web auto-configuration eliminates 200+ lines of MessageConverter and ViewResolver setup—enabling teams to implement REST endpoints immediately with proper HTTP status codes, JSON responses, and validation integration.

**Problem**: Manual REST API setup requires explicit MessageConverter, exception handler, and content negotiation configuration.

**Solution**: Spring Boot @RestController with auto-configured Jackson, exception handling, and validation.

## Implementation Example

```java
@RestController
@RequestMapping("/api/donations")
public class DonationController {

    @GetMapping("/{id}")
    public ResponseEntity<DonationResponse> getDonation(@PathVariable Long id) {
        // => Auto-serialized to JSON by Jackson
        // => Content-Type: application/json (auto-negotiated)
        DonationResponse response = donationService.findById(id);
        return ResponseEntity.ok(response);
    }

    @PostMapping
    public ResponseEntity<DonationResponse> createDonation(
            @RequestBody @Valid DonationRequest request) {
        // => @Valid triggers Bean Validation (auto-configured)
        // => Jackson deserializes JSON to DonationRequest
        DonationResponse response = donationService.create(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @GetMapping
    public Page<DonationResponse> listDonations(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        // => Pagination auto-configured with Spring Data
        Pageable pageable = PageRequest.of(page, size);
        return donationService.findAll(pageable);
    }
}
```

## Production Configuration

```yaml
spring:
  jackson:
    serialization:
      write-dates-as-timestamps: false # => ISO 8601 dates
    default-property-inclusion: non_null # => Exclude null fields
  mvc:
    throw-exception-if-no-handler-found: true
    # => 404 exceptions for missing endpoints
```

## Production Patterns

**Best Practices**:

- Use DTOs for request/response (not entities)
- Implement pagination for list endpoints
- Add @Valid for input validation
- Return appropriate HTTP status codes (201 for POST, 204 for DELETE)
- Use HATEOAS for hypermedia APIs (Spring HATEOAS)

## Trade-offs

| Aspect               | Spring Boot Approach       | Manual Approach                |
| -------------------- | -------------------------- | ------------------------------ |
| **Complexity**       | Auto-configured (simple)   | Manual configuration (complex) |
| **Flexibility**      | Conventions with overrides | Full control                   |
| **Maintenance**      | Framework-maintained       | Custom code maintenance        |
| **Production ready** | Defaults optimized         | Requires tuning                |

**Production recommendation**: Use Spring Boot REST auto-configuration for 95% of APIs. Custom MessageConverter for specialized formats.

## Next Steps

- [Validation](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/validation) - Bean Validation integration
- [Error Handling](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/error-handling) - @ControllerAdvice patterns
