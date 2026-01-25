---
title: "Spring Boot REST APIs"
description: Building RESTful web services with Spring Boot
category: explanation
subcategory: stack-libs
tags:
  - spring-boot
  - rest-api
  - controllers
  - validation
  - http
related:
  - ./ex-so-stli-jvsb__best-practices.md
last_updated: 2026-01-25
---

# Spring Boot REST APIs

## Overview

Comprehensive guide to building production-ready RESTful APIs with Spring Boot, covering controllers, validation, exception handling, and HTTP semantics.

See [Best Practices - REST API Design](./ex-so-stli-jvsb__best-practices.md#rest-api-design) and [Anti-Patterns - API Issues](./ex-so-stli-jvsb__anti-patterns.md#api-anti-patterns) for detailed patterns.

## Key Topics

- **@RestController** - REST endpoint creation
- **Request Validation** - Bean Validation with @Valid
- **Exception Handling** - @ControllerAdvice for global handlers
- **HTTP Semantics** - Proper status codes and methods
- **API Versioning** - URI-based versioning
- **DTO Pattern** - Request/Response objects
- **Pagination** - Page and Sort parameters
- **CORS Configuration** - Cross-origin requests

## Quick Example

```java
@RestController
@RequestMapping("/api/v1/zakat")
@Validated
public class ZakatCalculationController {
    private final ZakatCalculationService service;

    public ZakatCalculationController(ZakatCalculationService service) {
        this.service = service;
    }

    @PostMapping("/calculations")
    public ResponseEntity<ZakatResponse> calculate(
        @Valid @RequestBody CreateZakatRequest request
    ) {
        ZakatResponse response = service.calculate(request);
        URI location = ServletUriComponentsBuilder
            .fromCurrentRequest()
            .path("/{id}")
            .buildAndExpand(response.id())
            .toUri();
        return ResponseEntity.created(location).body(response);
    }

    @GetMapping("/calculations/{id}")
    public ResponseEntity<ZakatResponse> getById(@PathVariable String id) {
        return service.findById(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }
}
```

---

**Last Updated**: 2026-01-25
