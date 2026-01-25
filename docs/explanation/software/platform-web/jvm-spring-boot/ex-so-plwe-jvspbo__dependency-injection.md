---
title: "Spring Boot Dependency Injection"
description: Spring IoC container and dependency injection patterns
category: explanation
subcategory: platform-web
tags:
  - spring-boot
  - dependency-injection
  - ioc
  - beans
  - constructor-injection
related:
  - ./ex-so-plwe-jvspbo__idioms.md
principles:
  - explicit-over-implicit
last_updated: 2026-01-25
---

# Spring Boot Dependency Injection

## Quick Reference

- [Constructor Injection](#constructor-injection-recommended) - Preferred approach
- [Bean Scopes](#bean-scopes) - Singleton, prototype, request
- [Lazy Initialization](#lazy-initialization) - On-demand bean creation

## Constructor Injection (Recommended)

```java
@Service
public class ZakatCalculationService {
    private final ZakatCalculationRepository repository;
    private final ApplicationEventPublisher eventPublisher;

    // No @Autowired needed (Spring 4.3+)
    public ZakatCalculationService(
        ZakatCalculationRepository repository,
        ApplicationEventPublisher eventPublisher
    ) {
        this.repository = repository;
        this.eventPublisher = eventPublisher;
    }
}
```

**Benefits**:

- Immutable dependencies (final fields)
- Easy to test (pass mocks in constructor)
- Explicit dependencies
- Circular dependency detection at startup

## Bean Scopes

```java
// Singleton (default) - one instance per container
@Service
public class ZakatCalculationService { }

// Prototype - new instance per injection
@Component
@Scope("prototype")
public class ReportGenerator { }

// Request - new instance per HTTP request
@Component
@Scope(value = WebApplicationContext.SCOPE_REQUEST, proxyMode = ScopedProxyMode.TARGET_CLASS)
public class UserContextHolder { }
```

## Lazy Initialization

```java
@Service
@Lazy  // Created only when first used
public class ExpensiveService {
    // Heavy initialization deferred
}

@Configuration
public class AppConfig {

    @Bean
    @Lazy
    public HeavyResource heavyResource() {
        // Created on-demand
        return new HeavyResource();
    }
}
```

## Conditional Beans

```java
@Configuration
public class CacheConfig {

    @Bean
    @ConditionalOnProperty(name = "ose.cache.enabled", havingValue = "true")
    public CacheManager cacheManager() {
        return new CaffeineCacheManager();
    }

    @Bean
    @ConditionalOnMissingBean
    public ObjectMapper objectMapper() {
        return new ObjectMapper();
    }
}
```

## Related Documentation

- **[Idioms](./ex-so-plwe-jvspbo__idioms.md#constructor-injection-over-field-injection)** - DI patterns
- **[Anti-Patterns](./ex-so-plwe-jvspbo__anti-patterns.md#1-field-injection)** - DI mistakes

---

**Last Updated**: 2026-01-25
