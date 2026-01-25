---
title: "Spring Boot Domain-Driven Design"
description: Implementing DDD patterns with Spring Boot
category: explanation
subcategory: stack-libs
tags:
  - spring-boot
  - ddd
  - domain-driven-design
  - aggregates
  - repositories
related:
  - ./ex-so-stli-jvspbo__idioms.md
last_updated: 2026-01-25
---

# Spring Boot Domain-Driven Design

## Overview

Implementing Domain-Driven Design tactical patterns with Spring Boot including aggregates, repositories, domain events, and value objects.

See [README - DDD Integration](./README.md#architecture-integration) for architectural guidance.

## Key Patterns

- **Aggregates** - JPA entities with invariants
- **Value Objects** - Java records for immutability
- **Repositories** - Spring Data for persistence
- **Domain Events** - ApplicationEvent publishing
- **Bounded Contexts** - Package organization

## DDD Example

```java
// Value Object
public record Money(BigDecimal amount, String currency) {
    public Money {
        if (amount.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Amount cannot be negative");
        }
    }

    public Money add(Money other) {
        if (!this.currency.equals(other.currency)) {
            throw new IllegalArgumentException("Currency mismatch");
        }
        return new Money(this.amount.add(other.amount), this.currency);
    }
}

// Aggregate Root
@Entity
public class ZakatCalculation {
    @EmbeddedId
    private ZakatCalculationId id;

    @Embedded
    private Money wealth;

    @Embedded
    private Money zakatAmount;

    private boolean eligible;

    // Business logic encapsulated
    public static ZakatCalculation calculate(Money wealth, Money nisab) {
        boolean eligible = wealth.isGreaterThanOrEqual(nisab);
        Money zakatAmount = eligible
            ? wealth.multiply(new BigDecimal("0.025"))
            : Money.zero(wealth.currency());

        return new ZakatCalculation(
            ZakatCalculationId.generate(),
            wealth,
            zakatAmount,
            eligible
        );
    }
}

// Repository
public interface ZakatCalculationRepository extends JpaRepository<ZakatCalculation, ZakatCalculationId> {
    List<ZakatCalculation> findByEligible(boolean eligible);
}

// Application Service
@Service
@Transactional
public class ZakatCalculationService {
    private final ZakatCalculationRepository repository;
    private final ApplicationEventPublisher eventPublisher;

    public ZakatResponse calculate(CreateZakatRequest request) {
        // Pure domain logic
        ZakatCalculation calculation = ZakatCalculation.calculate(
            request.wealth(),
            request.nisab()
        );

        // Persist
        ZakatCalculation saved = repository.save(calculation);

        // Publish domain event
        eventPublisher.publishEvent(new ZakatCalculatedEvent(saved.getId()));

        return ZakatMapper.toResponse(saved);
    }
}
```

---

**Last Updated**: 2026-01-25
