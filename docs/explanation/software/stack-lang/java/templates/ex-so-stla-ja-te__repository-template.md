---
title: Java Repository Template
description: Template for creating Domain-Driven Design repositories with Spring Data JPA and custom implementations
category: template
tags:
  - java
  - ddd
  - repository
  - spring-data-jpa
  - persistence
related:
  - ex-so-stla-ja-te__aggregate-template.md
  - ex-so-stla-ja-te__entity-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2026-01-21
---

# Java Repository Template

This template provides a standardized structure for creating Domain-Driven Design repositories in Java using Spring Data JPA with custom query methods and specifications.

## Template Structure

```java
package com.openshariaenterprise.{domain}.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Repository interface for {@link AggregateRoot} persistence operations.
 *
 * <p>Repository Responsibilities:
 * <ul>
 *   <li>Load aggregates by ID or business criteria</li>
 *   <li>Save aggregates to persistence store</li>
 *   <li>Delete aggregates when needed</li>
 *   <li>Execute complex queries using Specifications</li>
 *   <li>Maintain aggregate integrity during persistence</li>
 * </ul>
 *
 * <p>Design Principles:
 * <ul>
 *   <li>Collection-like interface - Feels like in-memory collection</li>
 *   <li>Aggregate-oriented - Works with complete aggregates, not fragments</li>
 *   <li>Persistence ignorance - Domain model doesn't know about persistence</li>
 *   <li>Query by business criteria - Not just by ID</li>
 * </ul>
 *
 * @see AggregateRoot
 */
@Repository
public interface AggregateRepository extends
    JpaRepository<AggregateRoot, AggregateId>,
    JpaSpecificationExecutor<AggregateRoot>,
    CustomAggregateRepository {

    // ========================================
    // Query by Business Criteria
    // ========================================

    /**
     * Finds aggregate by business identifier.
     *
     * @param businessId the business identifier
     * @return optional aggregate
     */
    Optional<AggregateRoot> findByBusinessId(String businessId);

    /**
     * Finds all aggregates with the specified status.
     *
     * @param status the aggregate status
     * @return list of matching aggregates
     */
    List<AggregateRoot> findByStatus(AggregateStatus status);

    /**
     * Finds aggregates created within a date range.
     *
     * @param startDate start of date range
     * @param endDate end of date range
     * @return list of matching aggregates
     */
    List<AggregateRoot> findByCreatedAtBetween(
        LocalDateTime startDate,
        LocalDateTime endDate
    );

    /**
     * Finds aggregates by value object property.
     *
     * @param propertyValue value to match
     * @return list of matching aggregates
     */
    List<AggregateRoot> findByValueObjectProperty(String propertyValue);

    // ========================================
    // Custom JPQL Queries
    // ========================================

    /**
     * Finds aggregates with child entities matching criteria.
     *
     * @param childProperty property of child entity
     * @return list of matching aggregates
     */
    @Query("SELECT a FROM AggregateRoot a " +
           "JOIN a.childEntities c " +
           "WHERE c.property = :childProperty")
    List<AggregateRoot> findByChildEntityProperty(
        @Param("childProperty") String childProperty
    );

    /**
     * Finds aggregates with complex business logic.
     *
     * @param minValue minimum value threshold
     * @param status required status
     * @return list of matching aggregates
     */
    @Query("SELECT a FROM AggregateRoot a " +
           "WHERE a.valueObject.amount >= :minValue " +
           "AND a.status = :status " +
           "ORDER BY a.createdAt DESC")
    List<AggregateRoot> findEligibleAggregates(
        @Param("minValue") BigDecimal minValue,
        @Param("status") AggregateStatus status
    );

    // ========================================
    // Existence Checks
    // ========================================

    /**
     * Checks if aggregate exists with given business ID.
     *
     * @param businessId the business identifier
     * @return true if exists
     */
    boolean existsByBusinessId(String businessId);

    /**
     * Checks if any aggregate has the specified status.
     *
     * @param status the status to check
     * @return true if at least one exists
     */
    boolean existsByStatus(AggregateStatus status);

    // ========================================
    // Count Operations
    // ========================================

    /**
     * Counts aggregates by status.
     *
     * @param status the status to count
     * @return number of aggregates with status
     */
    long countByStatus(AggregateStatus status);

    /**
     * Counts aggregates created after date.
     *
     * @param date the cutoff date
     * @return count of recent aggregates
     */
    long countByCreatedAtAfter(LocalDateTime date);
}

/**
 * Custom repository interface for complex operations.
 */
interface CustomAggregateRepository {

    /**
     * Finds aggregates using custom criteria.
     *
     * @param criteria search criteria
     * @return list of matching aggregates
     */
    List<AggregateRoot> findByCriteria(SearchCriteria criteria);

    /**
     * Performs batch update operation.
     *
     * @param ids aggregate IDs to update
     * @param newStatus status to set
     * @return number of updated aggregates
     */
    int batchUpdateStatus(List<AggregateId> ids, AggregateStatus newStatus);
}

/**
 * Custom repository implementation.
 */
@Repository
class CustomAggregateRepositoryImpl implements CustomAggregateRepository {

    private final EntityManager entityManager;

    public CustomAggregateRepositoryImpl(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

    @Override
    public List<AggregateRoot> findByCriteria(SearchCriteria criteria) {
        var cb = entityManager.getCriteriaBuilder();
        var query = cb.createQuery(AggregateRoot.class);
        var root = query.from(AggregateRoot.class);

        var predicates = new ArrayList<Predicate>();

        // Build dynamic query based on criteria
        if (criteria.getStatus() != null) {
            predicates.add(cb.equal(root.get("status"), criteria.getStatus()));
        }

        if (criteria.getMinAmount() != null) {
            predicates.add(cb.greaterThanOrEqualTo(
                root.get("valueObject").get("amount"),
                criteria.getMinAmount()
            ));
        }

        if (!predicates.isEmpty()) {
            query.where(predicates.toArray(new Predicate[0]));
        }

        return entityManager.createQuery(query).getResultList();
    }

    @Override
    @Transactional
    public int batchUpdateStatus(List<AggregateId> ids, AggregateStatus newStatus) {
        var query = entityManager.createQuery(
            "UPDATE AggregateRoot a SET a.status = :status WHERE a.id IN :ids"
        );
        query.setParameter("status", newStatus);
        query.setParameter("ids", ids);

        return query.executeUpdate();
    }
}
```

## Financial Domain Example: Zakat Account Repository

```java
package com.openshariaenterprise.zakat.repository;

import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * Repository for {@link ZakatAccount} aggregate persistence.
 *
 * <p>Provides access to Zakat accounts with business-oriented queries:
 * <ul>
 *   <li>Find accounts eligible for Zakat calculation</li>
 *   <li>Query accounts by holder and status</li>
 *   <li>Retrieve payment history</li>
 *   <li>Statistical queries for reporting</li>
 * </ul>
 */
@Repository
public interface ZakatAccountRepository extends
    JpaRepository<ZakatAccount, ZakatAccountId>,
    JpaSpecificationExecutor<ZakatAccount> {

    // ========================================
    // Query by Business Criteria
    // ========================================

    /**
     * Finds account by holder ID.
     *
     * @param holderId the account holder identifier
     * @return optional account
     */
    Optional<ZakatAccount> findByHolderId(AccountHolderId holderId);

    /**
     * Finds all accounts for a specific holder.
     *
     * @param holderId the account holder identifier
     * @return list of accounts
     */
    List<ZakatAccount> findAllByHolderId(AccountHolderId holderId);

    /**
     * Finds accounts by status.
     *
     * @param status the account status
     * @return list of matching accounts
     */
    List<ZakatAccount> findByStatus(ZakatAccountStatus status);

    /**
     * Finds accounts eligible for Zakat calculation.
     *
     * <p>Criteria:
     * <ul>
     *   <li>Active status</li>
     *   <li>Balance >= nisab threshold</li>
     *   <li>Haul completed (>= 354 days since haul start)</li>
     * </ul>
     *
     * @param nisabThreshold the nisab threshold
     * @param haulCutoffDate haul completion cutoff date (354 days ago)
     * @return list of eligible accounts
     */
    @Query("SELECT za FROM ZakatAccount za " +
           "WHERE za.status = 'ACTIVE' " +
           "AND za.balance.amount >= :nisabThreshold " +
           "AND za.haulStartDate <= :haulCutoffDate")
    List<ZakatAccount> findEligibleForZakat(
        @Param("nisabThreshold") BigDecimal nisabThreshold,
        @Param("haulCutoffDate") LocalDate haulCutoffDate
    );

    /**
     * Finds accounts with balance above threshold.
     *
     * @param minBalance minimum balance threshold
     * @return list of accounts
     */
    @Query("SELECT za FROM ZakatAccount za " +
           "WHERE za.balance.amount >= :minBalance " +
           "ORDER BY za.balance.amount DESC")
    List<ZakatAccount> findByBalanceGreaterThanEqual(
        @Param("minBalance") BigDecimal minBalance
    );

    /**
     * Finds accounts that have never made a Zakat payment.
     *
     * @return list of accounts without payments
     */
    @Query("SELECT za FROM ZakatAccount za " +
           "WHERE za.lastZakatPaymentDate IS NULL " +
           "AND za.status = 'ACTIVE'")
    List<ZakatAccount> findAccountsWithoutPayments();

    /**
     * Finds accounts due for Zakat payment reminder.
     *
     * @param reminderDate date to check against haul start
     * @return list of accounts
     */
    @Query("SELECT za FROM ZakatAccount za " +
           "WHERE za.status = 'ACTIVE' " +
           "AND za.haulStartDate <= :reminderDate " +
           "AND (za.lastZakatPaymentDate IS NULL " +
           "     OR za.lastZakatPaymentDate < :reminderDate)")
    List<ZakatAccount> findAccountsDueForReminder(
        @Param("reminderDate") LocalDate reminderDate
    );

    // ========================================
    // Statistical Queries
    // ========================================

    /**
     * Calculates total Zakat-eligible wealth across all accounts.
     *
     * @param nisabThreshold nisab threshold
     * @param haulCutoffDate haul completion date
     * @return total eligible wealth
     */
    @Query("SELECT COALESCE(SUM(za.balance.amount), 0) FROM ZakatAccount za " +
           "WHERE za.status = 'ACTIVE' " +
           "AND za.balance.amount >= :nisabThreshold " +
           "AND za.haulStartDate <= :haulCutoffDate")
    BigDecimal calculateTotalEligibleWealth(
        @Param("nisabThreshold") BigDecimal nisabThreshold,
        @Param("haulCutoffDate") LocalDate haulCutoffDate
    );

    /**
     * Counts accounts by status.
     *
     * @param status the status to count
     * @return number of accounts
     */
    long countByStatus(ZakatAccountStatus status);

    /**
     * Finds accounts opened within date range.
     *
     * @param startDate range start
     * @param endDate range end
     * @return list of accounts
     */
    List<ZakatAccount> findByOpenedAtBetween(
        LocalDateTime startDate,
        LocalDateTime endDate
    );

    // ========================================
    // Existence Checks
    // ========================================

    /**
     * Checks if holder has any active accounts.
     *
     * @param holderId holder identifier
     * @return true if active account exists
     */
    boolean existsByHolderIdAndStatus(
        AccountHolderId holderId,
        ZakatAccountStatus status
    );
}
```

## Repository Specification Pattern

```java
package com.openshariaenterprise.zakat.repository;

import org.springframework.data.jpa.domain.Specification;
import jakarta.persistence.criteria.*;

/**
 * Specifications for dynamic Zakat account queries.
 */
public class ZakatAccountSpecifications {

    /**
     * Specification for accounts with status.
     */
    public static Specification<ZakatAccount> hasStatus(ZakatAccountStatus status) {
        return (root, query, cb) -> cb.equal(root.get("status"), status);
    }

    /**
     * Specification for accounts with balance above threshold.
     */
    public static Specification<ZakatAccount> hasBalanceGreaterThan(BigDecimal amount) {
        return (root, query, cb) ->
            cb.greaterThan(root.get("balance").get("amount"), amount);
    }

    /**
     * Specification for accounts eligible for Zakat.
     */
    public static Specification<ZakatAccount> isEligibleForZakat(
        BigDecimal nisabThreshold,
        LocalDate haulCutoffDate
    ) {
        return Specification
            .where(hasStatus(ZakatAccountStatus.ACTIVE))
            .and(hasBalanceGreaterThan(nisabThreshold))
            .and(hasCompletedHaul(haulCutoffDate));
    }

    /**
     * Specification for accounts with completed haul.
     */
    public static Specification<ZakatAccount> hasCompletedHaul(LocalDate cutoffDate) {
        return (root, query, cb) ->
            cb.lessThanOrEqualTo(root.get("haulStartDate"), cutoffDate);
    }

    /**
     * Specification for accounts belonging to holder.
     */
    public static Specification<ZakatAccount> belongsToHolder(AccountHolderId holderId) {
        return (root, query, cb) -> cb.equal(root.get("holderId"), holderId);
    }
}

/**
 * Usage example of specifications.
 */
@Service
public class ZakatAccountService {

    private final ZakatAccountRepository repository;

    public List<ZakatAccount> findEligibleAccounts(
        BigDecimal nisabThreshold,
        LocalDate haulCutoffDate
    ) {
        return repository.findAll(
            ZakatAccountSpecifications.isEligibleForZakat(
                nisabThreshold,
                haulCutoffDate
            )
        );
    }

    public List<ZakatAccount> findHolderActiveAccounts(AccountHolderId holderId) {
        return repository.findAll(
            Specification
                .where(ZakatAccountSpecifications.belongsToHolder(holderId))
                .and(ZakatAccountSpecifications.hasStatus(ZakatAccountStatus.ACTIVE))
        );
    }
}
```

## Repository Testing

```java
package com.openshariaenterprise.zakat.repository;

import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.*;

/**
 * Tests for {@link ZakatAccountRepository}.
 */
@DataJpaTest
@DisplayName("ZakatAccountRepository Tests")
class ZakatAccountRepositoryTest {

    @Autowired
    private TestEntityManager entityManager;

    @Autowired
    private ZakatAccountRepository repository;

    @BeforeEach
    void setUp() {
        repository.deleteAll();
    }

    @Nested
    @DisplayName("Find Eligible Accounts")
    class FindEligibleTests {

        @Test
        @DisplayName("Should find accounts above nisab with completed haul")
        void testFindEligibleForZakat() {
            // Arrange - Create eligible account
            var eligibleAccount = ZakatAccount.open(
                ZakatAccountId.generate(),
                AccountHolderId.generate(),
                Money.of(new BigDecimal("100000"), "USD"),
                Money.of(new BigDecimal("5000"), "USD"),
                UserId.system()
            );
            eligibleAccount.setHaulStartDate(LocalDate.now().minusDays(355));
            repository.save(eligibleAccount);

            // Arrange - Create ineligible account (below nisab)
            var ineligibleAccount = ZakatAccount.open(
                ZakatAccountId.generate(),
                AccountHolderId.generate(),
                Money.of(new BigDecimal("3000"), "USD"),
                Money.of(new BigDecimal("5000"), "USD"),
                UserId.system()
            );
            repository.save(ineligibleAccount);

            // Act
            var eligible = repository.findEligibleForZakat(
                new BigDecimal("5000"),
                LocalDate.now().minusDays(354)
            );

            // Assert
            assertThat(eligible)
                .hasSize(1)
                .first()
                .satisfies(account -> {
                    assertThat(account.getId()).isEqualTo(eligibleAccount.getId());
                    assertThat(account.isEligibleForZakat()).isTrue();
                });
        }
    }

    @Nested
    @DisplayName("Statistical Queries")
    class StatisticsTests {

        @Test
        @DisplayName("Should calculate total eligible wealth")
        void testCalculateTotalEligibleWealth() {
            // Arrange - Create multiple accounts
            createAccount(new BigDecimal("100000"));
            createAccount(new BigDecimal("50000"));
            createAccount(new BigDecimal("3000")); // Below nisab

            // Act
            var totalWealth = repository.calculateTotalEligibleWealth(
                new BigDecimal("5000"),
                LocalDate.now().minusDays(354)
            );

            // Assert
            assertThat(totalWealth)
                .isEqualByComparingTo(new BigDecimal("150000"));
        }
    }

    private ZakatAccount createAccount(BigDecimal balance) {
        var account = ZakatAccount.open(
            ZakatAccountId.generate(),
            AccountHolderId.generate(),
            Money.of(balance, "USD"),
            Money.of(new BigDecimal("5000"), "USD"),
            UserId.system()
        );
        account.setHaulStartDate(LocalDate.now().minusDays(355));
        return repository.save(account);
    }
}
```

## Usage Guidelines

1. **One Repository Per Aggregate**: Each aggregate root has one repository
2. **Business-Oriented Queries**: Query methods reflect business concepts, not database structure
3. **Collection Metaphor**: Repository feels like an in-memory collection
4. **Load Complete Aggregates**: Always load entire aggregate, not fragments
5. **Specifications for Complex Queries**: Use Specification pattern for dynamic queries
6. **Test Repository Queries**: Write integration tests for custom queries
7. **Avoid Anemic Repositories**: Put logic in domain model, not repository

## Related Templates

- [Aggregate Template](./ex-so-stla-ja-te__aggregate-template.md)
- [Entity Template](./ex-so-stla-ja-te__entity-template.md)
- [Integration Test Template](./ex-so-stla-ja-te__integration-test-template.md)

## See Also

- [Repository Pattern](https://martinfowler.com/eaaCatalog/repository.html)
- [Spring Data JPA Documentation](https://spring.io/projects/spring-data-jpa)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
