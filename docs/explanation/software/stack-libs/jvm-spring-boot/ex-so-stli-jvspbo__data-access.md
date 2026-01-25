---
title: "Spring Boot Data Access"
description: Data persistence with Spring Data JPA and repositories
category: explanation
subcategory: stack-libs
tags:
  - spring-boot
  - spring-data-jpa
  - repositories
  - transactions
  - database
related:
  - ./ex-so-stli-jvspbo__best-practices.md
last_updated: 2026-01-25
---

# Spring Boot Data Access

## Overview

Comprehensive guide to data persistence using Spring Data JPA, covering repositories, transactions, query methods, and database migrations.

See [Best Practices - Data Access](./ex-so-stli-jvspbo__best-practices.md#data-access-patterns) for detailed patterns.

## Key Topics

- **Repository Pattern** - Spring Data JPA repositories
- **Query Methods** - Derived and custom queries
- **Transactions** - @Transactional management
- **N+1 Prevention** - JOIN FETCH strategies
- **Database Migrations** - Flyway integration
- **Connection Pooling** - HikariCP configuration
- **Projections** - DTOs for read models

## Repository Example

```java
public interface ZakatCalculationRepository extends JpaRepository<ZakatCalculation, String> {

    // Query derivation
    List<ZakatCalculation> findByUserIdAndEligible(String userId, boolean eligible);

    // Custom JPQL
    @Query("""
        SELECT z FROM ZakatCalculation z
        WHERE z.userId = :userId
        AND z.calculationDate BETWEEN :start AND :end
        ORDER BY z.calculationDate DESC
        """)
    List<ZakatCalculation> findByUserIdAndDateRange(
        @Param("userId") String userId,
        @Param("start") LocalDate start,
        @Param("end") LocalDate end
    );

    // JOIN FETCH to prevent N+1
    @Query("SELECT z FROM ZakatCalculation z JOIN FETCH z.assets WHERE z.id = :id")
    Optional<ZakatCalculation> findByIdWithAssets(@Param("id") String id);
}
```

---

**Last Updated**: 2026-01-25
