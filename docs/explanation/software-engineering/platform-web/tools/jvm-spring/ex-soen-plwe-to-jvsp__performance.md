---
title: Spring Framework Performance
description: Optimization covering bean initialization, lazy initialization, connection pooling, caching with Spring Cache, @Async, virtual threads, transaction optimization, query optimization, profiling, and memory management
category: explanation
subcategory: platform-web
tags:
  - spring-framework
  - performance
  - optimization
  - caching
  - java
  - kotlin
principles:
  - automation-over-manual
created: 2026-01-29
updated: 2026-01-29
---

# Spring Framework Performance

**Understanding-oriented documentation** for optimizing Spring Framework applications.

## Overview

This document covers performance optimization techniques for Spring applications, including caching, async processing, connection pooling, and query optimization for Islamic finance workloads.

**Version**: Spring Framework 6.1+ (Java 17+, Kotlin 1.9+)

## Quick Reference

**Jump to:**

- [Bean Initialization Optimization](#bean-initialization-optimization)
- [Connection Pooling](#connection-pooling-configuration)
- [Caching](#caching-with-spring-cache)
- [Async Execution](#async-execution)
- [Virtual Threads (Java 21+)](#virtual-threads-java-21)
- [Transaction Optimization](#transaction-optimization)
- [Query Optimization](#query-optimization)

## Bean Initialization Optimization

### Lazy Initialization

**Java Example**:

```java
@Configuration
public class PerformanceConfig {

  @Bean
  @Lazy
  public ExpensiveService expensiveService() {
    // Only initialized when first requested
    return new ExpensiveService();
  }

  @Bean
  public LazyInitializationBeanFactoryPostProcessor lazyInitialization() {
    // Global lazy initialization (use with caution)
    return new LazyInitializationBeanFactoryPostProcessor();
  }
}
```

**Kotlin Example**:

```kotlin
@Configuration
class PerformanceConfig {

  @Bean
  @Lazy
  fun expensiveService(): ExpensiveService {
    // Only initialized when first requested
    return ExpensiveService()
  }
}
```

## Connection Pooling Configuration

### HikariCP Optimization

**Java Example**:

```java
@Configuration
public class DataSourceConfig {

  @Bean
  public DataSource dataSource() {
    HikariConfig config = new HikariConfig();

    // Connection pool size
    config.setMaximumPoolSize(20);      // Maximum connections
    config.setMinimumIdle(5);           // Minimum idle connections

    // Timeouts (milliseconds)
    config.setConnectionTimeout(30000);  // 30 seconds
    config.setIdleTimeout(600000);       // 10 minutes
    config.setMaxLifetime(1800000);      // 30 minutes
    config.setValidationTimeout(5000);   // 5 seconds

    // Performance settings
    config.setAutoCommit(false);         // Manual transaction management
    config.setConnectionTestQuery("SELECT 1");

    // PreparedStatement cache
    config.addDataSourceProperty("cachePrepStmts", "true");
    config.addDataSourceProperty("prepStmtCacheSize", "250");
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");

    return new HikariDataSource(config);
  }
}
```

## Caching with Spring Cache

### Enable Caching

**Java Example**:

```java
@Configuration
@EnableCaching
public class CacheConfig {

  @Bean
  public CacheManager cacheManager() {
    CaffeineCacheManager cacheManager = new CaffeineCacheManager(
      "zakatCalculations",
      "nisabValues",
      "donationCategories"
    );

    cacheManager.setCaffeine(Caffeine.newBuilder()
      .maximumSize(1000)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .recordStats()
    );

    return cacheManager;
  }
}
```

**Kotlin Example**:

```kotlin
@Configuration
@EnableCaching
class CacheConfig {

  @Bean
  fun cacheManager(): CacheManager {
    val cacheManager = CaffeineCacheManager(
      "zakatCalculations",
      "nisabValues",
      "donationCategories"
    )

    cacheManager.setCaffeine(Caffeine.newBuilder()
      .maximumSize(1000)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .recordStats()
    )

    return cacheManager
  }
}
```

### Using @Cacheable

**Java Example** (Nisab Service):

```java
@Service
public class NisabService {
  private static final Logger logger = LoggerFactory.getLogger(NisabService.class);

  private final GoldPriceService goldPriceService;

  public NisabService(GoldPriceService goldPriceService) {
    this.goldPriceService = goldPriceService;
  }

  @Cacheable(value = "nisabValues", key = "#currency")
  public Money calculateCurrentNisab(String currency) {
    logger.info("Calculating nisab for currency: {} (cache miss)", currency);

    BigDecimal goldPricePerGram = goldPriceService.getCurrentPrice(currency);
    BigDecimal nisabGoldGrams = new BigDecimal("85");
    BigDecimal nisabValue = goldPricePerGram.multiply(nisabGoldGrams);

    return Money.of(nisabValue, currency);
  }

  @CacheEvict(value = "nisabValues", allEntries = true)
  public void clearNisabCache() {
    logger.info("Clearing nisab cache");
  }

  @CachePut(value = "nisabValues", key = "#currency")
  public Money updateNisab(String currency, Money nisab) {
    logger.info("Updating nisab for currency: {}", currency);
    return nisab;
  }
}
```

**Kotlin Example**:

```kotlin
@Service
class NisabService(private val goldPriceService: GoldPriceService) {
  companion object {
    private val logger = LoggerFactory.getLogger(NisabService::class.java)
  }

  @Cacheable(value = ["nisabValues"], key = "#currency")
  fun calculateCurrentNisab(currency: String): Money {
    logger.info("Calculating nisab for currency: {} (cache miss)", currency)

    val goldPricePerGram = goldPriceService.getCurrentPrice(currency)
    val nisabGoldGrams = BigDecimal("85")
    val nisabValue = goldPricePerGram * nisabGoldGrams

    return Money.of(nisabValue, currency)
  }

  @CacheEvict(value = ["nisabValues"], allEntries = true)
  fun clearNisabCache() {
    logger.info("Clearing nisab cache")
  }
}
```

## Async Execution

### Enable Async

**Java Example**:

```java
@Configuration
@EnableAsync
public class AsyncConfig implements AsyncConfigurer {

  @Override
  public Executor getAsyncExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(5);
    executor.setMaxPoolSize(10);
    executor.setQueueCapacity(100);
    executor.setThreadNamePrefix("async-");
    executor.initialize();
    return executor;
  }
}
```

### Using @Async

**Java Example** (Email Notification):

```java
@Service
public class EmailNotificationService {
  private static final Logger logger = LoggerFactory.getLogger(EmailNotificationService.class);

  @Async
  public CompletableFuture<Void> sendDonationReceipt(String email, Donation donation) {
    logger.info("Sending donation receipt to: {}", email);

    try {
      // Simulate email sending (expensive operation)
      Thread.sleep(2000);

      logger.info("Donation receipt sent successfully to: {}", email);
      return CompletableFuture.completedFuture(null);
    } catch (InterruptedException e) {
      logger.error("Failed to send donation receipt", e);
      return CompletableFuture.failedFuture(e);
    }
  }
}

@Service
public class DonationService {
  private final EmailNotificationService emailService;

  public DonationService(EmailNotificationService emailService) {
    this.emailService = emailService;
  }

  @Transactional
  public DonationResponse processDonation(CreateDonationRequest request) {
    Donation donation = Donation.create(request);
    repository.save(donation);

    // Async email - doesn't block transaction
    emailService.sendDonationReceipt(request.donorEmail(), donation);

    return toResponse(donation);
  }
}
```

**Kotlin Example**:

```kotlin
@Service
class EmailNotificationService {
  companion object {
    private val logger = LoggerFactory.getLogger(EmailNotificationService::class.java)
  }

  @Async
  fun sendDonationReceipt(email: String, donation: Donation): CompletableFuture<Unit> {
    logger.info("Sending donation receipt to: $email")

    return try {
      // Simulate email sending (expensive operation)
      Thread.sleep(2000)

      logger.info("Donation receipt sent successfully to: $email")
      CompletableFuture.completedFuture(Unit)
    } catch (e: InterruptedException) {
      logger.error("Failed to send donation receipt", e)
      CompletableFuture.failedFuture(e)
    }
  }
}
```

## Virtual Threads (Java 21+)

**Java Example**:

```java
@Configuration
@EnableAsync
public class VirtualThreadAsyncConfig implements AsyncConfigurer {

  @Override
  public Executor getAsyncExecutor() {
    // Use virtual threads for async execution (Java 21+)
    return Executors.newVirtualThreadPerTaskExecutor();
  }
}
```

## Transaction Optimization

### Read-Only Transactions

**Java Example**:

```java
@Service
public class DonationQueryService {

  @Transactional(readOnly = true)
  public List<DonationResponse> findDonations(
    String donorId,
    LocalDate startDate,
    LocalDate endDate
  ) {
    // Read-only optimization - no flush, no dirty checking
    return repository.findByDonorIdAndDateRange(donorId, startDate, endDate).stream()
      .map(this::toResponse)
      .toList();
  }
}
```

### Batch Operations

**Java Example** (Batch Insert):

```java
@Repository
public class JdbcDonationRepository {
  private final JdbcTemplate jdbcTemplate;

  public JdbcDonationRepository(JdbcTemplate jdbcTemplate) {
    this.jdbcTemplate = jdbcTemplate;
  }

  @Transactional
  public void saveBatch(List<Donation> donations) {
    String sql = """
      INSERT INTO donations (id, amount, category, donor_id, donation_date)
      VALUES (?, ?, ?, ?, ?)
      """;

    List<Object[]> batchArgs = donations.stream()
      .map(donation -> new Object[]{
        donation.getId().getValue(),
        donation.getAmount().getAmount(),
        donation.getCategory().name(),
        donation.getDonorId(),
        donation.getDonationDate()
      })
      .toList();

    jdbcTemplate.batchUpdate(sql, batchArgs);
  }
}
```

## Query Optimization

### Efficient Queries

**Java Example**:

```java
@Repository
public class OptimizedZakatRepository {

  // ❌ Bad: N+1 query problem
  public List<ZakatCalculation> findAllWithDonor() {
    List<ZakatCalculation> calculations = jdbcTemplate.query(
      "SELECT * FROM zakat_calculations",
      new ZakatCalculationRowMapper()
    );

    // Each iteration queries database again (N+1)
    calculations.forEach(calc -> {
      Donor donor = findDonorById(calc.getDonorId());  // N queries
      calc.setDonor(donor);
    });

    return calculations;
  }

  // ✅ Good: Single query with JOIN
  public List<ZakatCalculation> findAllWithDonorOptimized() {
    String sql = """
      SELECT zc.*, d.name, d.email
      FROM zakat_calculations zc
      JOIN donors d ON zc.donor_id = d.id
      """;

    return jdbcTemplate.query(sql, (rs, rowNum) -> {
      ZakatCalculation calculation = new ZakatCalculation(
        rs.getString("id"),
        // ... other fields
      );

      Donor donor = new Donor(
        rs.getString("donor_id"),
        rs.getString("name"),
        rs.getString("email")
      );

      calculation.setDonor(donor);
      return calculation;
    });
  }
}
```

### Pagination

**Java Example**:

```java
@Service
public class DonationQueryService {

  @Transactional(readOnly = true)
  public Page<DonationResponse> findDonations(int page, int size) {
    int offset = page * size;

    String countSql = "SELECT COUNT(*) FROM donations";
    long total = jdbcTemplate.queryForObject(countSql, Long.class);

    String sql = """
      SELECT * FROM donations
      ORDER BY donation_date DESC
      LIMIT ? OFFSET ?
      """;

    List<DonationResponse> donations = jdbcTemplate.query(
      sql,
      new DonationRowMapper(),
      size,
      offset
    ).stream()
      .map(this::toResponse)
      .toList();

    return new Page<>(donations, page, size, total);
  }
}
```

## Related Documentation

### Core Spring Framework Documentation

- **[Spring Framework README](./README.md)** - Framework overview
- **[Data Access](ex-soen-plwe-to-jvsp__data-access.md)** - Database optimization
- **[Best Practices](ex-soen-plwe-to-jvsp__best-practices.md)** - Performance patterns

---

**Last Updated**: 2026-01-29
**Spring Framework Version**: 6.1+ (Java 17+, Kotlin 1.9+)
**Maintainers**: Platform Documentation Team
