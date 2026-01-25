---
title: Repository Template
description: Template for Spring Data JPA repositories
category: explanation
subcategory: platform-web-templates
tags:
  - spring-boot
  - template
  - repository
  - jpa
last_updated: 2026-01-25
---

# Repository Template

```java
package com.oseplatform.[domain].domain.repository;

import com.oseplatform.[domain].domain.model.[Resource];
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface [Resource]Repository extends JpaRepository<[Resource], String> {

    // Query derivation - simple property conditions
    Optional<[Resource]> findByReferenceNumber(String referenceNumber);

    List<[Resource]> findByStatus(String status);

    boolean existsByReferenceNumber(String referenceNumber);

    // Query derivation - date range
    List<[Resource]> findByCreatedAtBetween(
        LocalDate startDate,
        LocalDate endDate
    );

    // Query derivation - complex conditions
    Page<[Resource]> findByStatusAndCreatedAtAfter(
        String status,
        LocalDate createdAfter,
        Pageable pageable
    );

    // Custom JPQL query
    @Query("""
        SELECT r FROM [Resource] r
        WHERE r.userId = :userId
        AND r.status IN :statuses
        ORDER BY r.createdAt DESC
        """)
    List<[Resource]> findByUserIdAndStatuses(
        @Param("userId") String userId,
        @Param("statuses") List<String> statuses
    );

    // JOIN FETCH to prevent N+1
    @Query("""
        SELECT r FROM [Resource] r
        LEFT JOIN FETCH r.details
        WHERE r.id = :id
        """)
    Optional<[Resource]> findByIdWithDetails(@Param("id") String id);

    // Pagination with JPQL
    @Query("""
        SELECT r FROM [Resource] r
        WHERE (:filter IS NULL OR r.name LIKE %:filter%)
        """)
    Page<[Resource]> findByFilter(
        @Param("filter") String filter,
        Pageable pageable
    );

    // Native query (use sparingly)
    @Query(value = """
        SELECT * FROM [resource]_table
        WHERE status = :status
        AND created_at >= CURRENT_DATE - INTERVAL '30 days'
        """, nativeQuery = true)
    List<[Resource]> findRecentByStatus(@Param("status") String status);

    // DTO projection interface
    interface [Resource]Summary {
        String getId();
        String getReferenceNumber();
        String getStatus();
        LocalDate getCreatedAt();
    }

    // Projection query
    @Query("""
        SELECT r.id as id,
               r.referenceNumber as referenceNumber,
               r.status as status,
               r.createdAt as createdAt
        FROM [Resource] r
        WHERE r.userId = :userId
        """)
    List<[Resource]Summary> findSummariesByUserId(@Param("userId") String userId);
}
```

**Replace**:

- `[domain]` - Your bounded context (e.g., `zakat`, `murabaha`)
- `[Resource]` - Resource name PascalCase (e.g., `Calculation`, `Contract`)
- `[resource]` - Resource name lowercase (for native queries table names)

**Best Practices**:

- Use query derivation for simple queries
- Use JPQL for complex queries
- Use JOIN FETCH to prevent N+1 problems
- Use projections (interfaces) for read-only DTOs
- Use native queries only when JPQL is insufficient
- Always use @Param for query parameters
- Prefer Pageable over custom pagination logic

---

**Last Updated**: 2026-01-25
