---
title: "Spring Boot Functional Programming"
description: Functional programming patterns with Spring Boot
category: explanation
subcategory: stack-libs
tags:
  - spring-boot
  - functional-programming
  - immutability
  - pure-functions
related:
  - ./ex-so-stli-jvsb__idioms.md
principles:
  - immutability
  - pure-functions
last_updated: 2026-01-25
---

# Spring Boot Functional Programming

## Overview

Applying functional programming principles in Spring Boot applications: immutability, pure functions, and functional core/imperative shell architecture.

## Key Principles

- **Immutable DTOs** - Java records
- **Pure Functions** - Domain logic without side effects
- **Functional Core, Imperative Shell** - Separate pure logic from I/O
- **Stream API** - Functional collection processing
- **Optional** - Null safety

## Functional Patterns

```java
// Immutable DTOs
public record CreateZakatRequest(
    BigDecimal wealth,
    BigDecimal nisab,
    LocalDate calculationDate
) {
    public CreateZakatRequest {
        if (wealth.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Wealth cannot be negative");
        }
    }
}

// Functional Core - Pure calculation
public class ZakatCalculation {
    public static Money calculateZakat(Money wealth, Money nisab) {
        // Pure function - no side effects
        if (wealth.isLessThan(nisab)) {
            return Money.zero(wealth.currency());
        }
        return wealth.multiply(new BigDecimal("0.025"));
    }
}

// Imperative Shell - Coordinates side effects
@Service
@Transactional
public class ZakatCalculationService {
    private final ZakatCalculationRepository repository;

    public ZakatResponse calculate(CreateZakatRequest request) {
        // Call pure function
        Money zakatAmount = ZakatCalculation.calculateZakat(
            request.wealth(),
            request.nisab()
        );

        // Side effect: persist
        ZakatCalculation saved = repository.save(
            new ZakatCalculation(request.wealth(), zakatAmount)
        );

        // Side effect: publish event
        eventPublisher.publishEvent(new ZakatCalculatedEvent(saved.getId()));

        return ZakatMapper.toResponse(saved);
    }
}

// Stream API usage
@Service
public class DonationReportService {
    public DonationSummary generateSummary(List<Donation> donations) {
        Money total = donations.stream()
            .map(Donation::getAmount)
            .reduce(Money.ZERO, Money::add);

        Map<String, Money> byCategory = donations.stream()
            .collect(Collectors.groupingBy(
                Donation::getCategory,
                Collectors.reducing(Money.ZERO, Donation::getAmount, Money::add)
            ));

        return new DonationSummary(total, byCategory);
    }
}
```

---

**Last Updated**: 2026-01-25
