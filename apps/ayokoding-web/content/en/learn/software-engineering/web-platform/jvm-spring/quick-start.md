---
title: "Quick Start"
weight: 100002
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Build a complete Zakat Calculator application using Spring Framework, demonstrating IoC, dependency injection, data access, and transaction management"
tags: ["spring", "java", "kotlin", "tutorial", "zakat", "islamic-finance"]
---

## Overview

This quick start builds a **Zakat Calculator Application** demonstrating core Spring Framework features:

- Java-based configuration with `@Configuration` and `@Bean`
- Component scanning with `@ComponentScan`
- Dependency injection (constructor injection)
- Service layer with business logic
- Repository pattern with JdbcTemplate
- Transaction management with `@Transactional`
- Console interface for user interaction

**Complete working application** with both Java and Kotlin implementations.

## Prerequisites

Complete [Initial Setup](/en/learn/software-engineering/web-platform/jvm-spring/initial-setup) to have:

- Java 17+ installed
- Maven or Gradle configured
- Spring Framework project created

## Project Structure

```
zakat-calculator/
├── pom.xml (or build.gradle.kts)
└── src/
    ├── main/
    │   ├── java/com/ayokoding/zakat/
    │   │   ├── AppConfig.java          # => Spring configuration
    │   │   ├── ZakatCalculatorApp.java # => Main application
    │   │   ├── model/
    │   │   │   └── ZakatRecord.java    # => Domain model
    │   │   ├── repository/
    │   │   │   ├── ZakatRepository.java     # => Repository interface
    │   │   │   └── JdbcZakatRepository.java # => JDBC implementation
    │   │   └── service/
    │   │       └── ZakatService.java   # => Business logic
    │   └── resources/
    │       └── schema.sql              # => Database schema
    └── test/
        └── java/com/ayokoding/zakat/
            └── ZakatServiceTest.java   # => Service tests
```

## Step 1: Domain Model

Create domain model representing Zakat payment records.

**Create file**: `src/main/java/com/ayokoding/zakat/model/ZakatRecord.java`

```java
package com.ayokoding.zakat.model;

import java.math.BigDecimal;          // => Precise decimal arithmetic
import java.time.LocalDateTime;       // => Timestamp

// => Domain model - represents Zakat payment record
// => Plain Java object (POJO) with no Spring dependencies
public class ZakatRecord {
    private Long id;                  // => Primary key (null for new records)
    private BigDecimal amount;        // => Zakat amount paid
    private LocalDateTime createdAt;  // => Payment timestamp

    // => No-arg constructor (required for JdbcTemplate)
    public ZakatRecord() {}

    // => Constructor with amount (for new records)
    public ZakatRecord(BigDecimal amount) {
        this.amount = amount;           // => Set amount
        this.createdAt = LocalDateTime.now();  // => Timestamp creation
    }

    // => Full constructor (for database-loaded records)
    public ZakatRecord(Long id, BigDecimal amount, LocalDateTime createdAt) {
        this.id = id;                   // => Database-assigned ID
        this.amount = amount;           // => Loaded amount
        this.createdAt = createdAt;     // => Loaded timestamp
    }

    // => Getters and setters
    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

    public BigDecimal getAmount() { return amount; }
    public void setAmount(BigDecimal amount) { this.amount = amount; }

    public LocalDateTime getCreatedAt() { return createdAt; }
    public void setCreatedAt(LocalDateTime createdAt) { this.createdAt = createdAt; }

    @Override
    public String toString() {
        return "ZakatRecord{id=" + id +
               ", amount=" + amount +
               ", createdAt=" + createdAt + '}';
        // => Human-readable representation
    }
}
```

**Kotlin version**: `src/main/kotlin/com/ayokoding/zakat/model/ZakatRecord.kt`

```kotlin
package com.ayokoding.zakat.model

import java.math.BigDecimal
import java.time.LocalDateTime

// => Data class - automatic equals, hashCode, toString
// => Concise domain model
data class ZakatRecord(
    var id: Long? = null,                      // => Nullable ID (null for new records)
    val amount: BigDecimal,                    // => Immutable amount
    val createdAt: LocalDateTime = LocalDateTime.now()  // => Default to now
)  // => Complete domain model in 5 lines
```

## Step 2: Repository Layer

Create repository interface and JDBC implementation for data access.

**Create file**: `src/main/java/com/ayokoding/zakat/repository/ZakatRepository.java`

```java
package com.ayokoding.zakat.repository;

import com.ayokoding.zakat.model.ZakatRecord;
import java.util.List;

// => Repository interface - defines data access operations
// => Abstracts persistence mechanism (could be JDBC, JPA, NoSQL)
public interface ZakatRepository {
    void save(ZakatRecord record);      // => Save new record
    List<ZakatRecord> findAll();        // => Retrieve all records
    ZakatRecord findById(Long id);      // => Find by primary key
}
```

**Create file**: `src/main/java/com/ayokoding/zakat/repository/JdbcZakatRepository.java`

```java
package com.ayokoding.zakat.repository;

import com.ayokoding.zakat.model.ZakatRecord;
import org.springframework.jdbc.core.JdbcTemplate;           // => Spring JDBC template
import org.springframework.jdbc.core.RowMapper;              // => ResultSet mapping
import org.springframework.jdbc.support.GeneratedKeyHolder;  // => Generated key holder
import org.springframework.jdbc.support.KeyHolder;           // => Key holder interface
import org.springframework.stereotype.Repository;            // => Repository stereotype

import java.sql.PreparedStatement;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.List;

// => @Repository marks this as data access component
// => Spring auto-detects during component scanning
// => Translates JDBC exceptions to Spring DataAccessException
@Repository
public class JdbcZakatRepository implements ZakatRepository {

    private final JdbcTemplate jdbcTemplate;  // => Spring JDBC template
                                              // => Handles connection management

    // => Constructor injection (recommended)
    // => Spring injects JdbcTemplate bean automatically
    public JdbcZakatRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;     // => Dependency injected
    }

    @Override
    public void save(ZakatRecord record) {
        // => SQL insert statement
        String sql = "INSERT INTO zakat_records (amount, created_at) VALUES (?, ?)";

        // => KeyHolder captures generated ID
        KeyHolder keyHolder = new GeneratedKeyHolder();

        // => Execute update with PreparedStatementCreator
        jdbcTemplate.update(connection -> {
            PreparedStatement ps = connection.prepareStatement(
                sql,
                Statement.RETURN_GENERATED_KEYS  // => Request generated keys
            );
            ps.setBigDecimal(1, record.getAmount());  // => Set amount parameter
            ps.setTimestamp(2, Timestamp.valueOf(record.getCreatedAt()));  // => Set timestamp
            return ps;                                  // => Return prepared statement
        }, keyHolder);

        // => Extract generated ID and set on record
        record.setId(keyHolder.getKey().longValue());  // => ID now available
    }

    @Override
    public List<ZakatRecord> findAll() {
        // => SQL select statement
        String sql = "SELECT id, amount, created_at FROM zakat_records ORDER BY created_at DESC";

        // => Execute query with RowMapper
        // => RowMapper converts each ResultSet row to ZakatRecord
        return jdbcTemplate.query(sql, zakatRecordRowMapper());
        // => Returns List<ZakatRecord>
    }

    @Override
    public ZakatRecord findById(Long id) {
        // => SQL select by ID
        String sql = "SELECT id, amount, created_at FROM zakat_records WHERE id = ?";

        // => queryForObject returns single result
        // => Throws exception if not found or multiple results
        return jdbcTemplate.queryForObject(sql, zakatRecordRowMapper(), id);
        // => Returns single ZakatRecord
    }

    // => RowMapper - converts ResultSet row to domain object
    private RowMapper<ZakatRecord> zakatRecordRowMapper() {
        return (rs, rowNum) -> new ZakatRecord(
            rs.getLong("id"),                        // => Extract ID column
            rs.getBigDecimal("amount"),              // => Extract amount column
            rs.getTimestamp("created_at").toLocalDateTime()  // => Convert timestamp
        );  // => Returns ZakatRecord instance
    }
}
```

**Kotlin version**: `src/main/kotlin/com/ayokoding/zakat/repository/JdbcZakatRepository.kt`

```kotlin
package com.ayokoding.zakat.repository

import com.ayokoding.zakat.model.ZakatRecord
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.support.GeneratedKeyHolder
import org.springframework.stereotype.Repository
import java.sql.Statement

@Repository  // => Repository stereotype
class JdbcZakatRepository(
    private val jdbcTemplate: JdbcTemplate  // => Constructor injection
) : ZakatRepository {

    override fun save(record: ZakatRecord) {
        val sql = "INSERT INTO zakat_records (amount, created_at) VALUES (?, ?)"
        val keyHolder = GeneratedKeyHolder()

        jdbcTemplate.update({ connection ->
            connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS).apply {
                setBigDecimal(1, record.amount)         // => Set parameters
                setTimestamp(2, java.sql.Timestamp.valueOf(record.createdAt))
            }
        }, keyHolder)

        record.id = keyHolder.key?.toLong()  // => Set generated ID
    }

    override fun findAll(): List<ZakatRecord> {
        val sql = "SELECT id, amount, created_at FROM zakat_records ORDER BY created_at DESC"
        return jdbcTemplate.query(sql) { rs, _ ->  // => Lambda RowMapper
            ZakatRecord(
                rs.getLong("id"),
                rs.getBigDecimal("amount"),
                rs.getTimestamp("created_at").toLocalDateTime()
            )
        }  // => Returns List<ZakatRecord>
    }

    override fun findById(id: Long): ZakatRecord {
        val sql = "SELECT id, amount, created_at FROM zakat_records WHERE id = ?"
        return jdbcTemplate.queryForObject(sql, { rs, _ ->
            ZakatRecord(
                rs.getLong("id"),
                rs.getBigDecimal("amount"),
                rs.getTimestamp("created_at").toLocalDateTime()
            )
        }, id)!!  // => Non-null assertion (throws if not found)
    }
}
```

## Step 3: Service Layer

Create service layer containing business logic.

**Create file**: `src/main/java/com/ayokoding/zakat/service/ZakatService.java`

```java
package com.ayokoding.zakat.service;

import com.ayokoding.zakat.model.ZakatRecord;
import com.ayokoding.zakat.repository.ZakatRepository;
import org.springframework.stereotype.Service;                // => Service stereotype
import org.springframework.transaction.annotation.Transactional;  // => Transaction management

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

// => @Service marks this as business logic component
// => Spring auto-detects during component scanning
@Service
public class ZakatService {

    // => Zakat rate - 2.5% of qualifying wealth
    private static final BigDecimal ZAKAT_RATE = new BigDecimal("0.025");

    private final ZakatRepository repository;  // => Repository dependency

    // => Constructor injection
    // => Spring injects ZakatRepository bean automatically
    public ZakatService(ZakatRepository repository) {
        this.repository = repository;          // => Dependency injected
    }

    // => Calculate Zakat amount from total wealth
    public BigDecimal calculateZakat(BigDecimal totalWealth) {
        if (totalWealth == null || totalWealth.compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Total wealth must be positive");
            // => Validate input
        }

        // => Calculate 2.5% of wealth, round to 2 decimal places
        return totalWealth.multiply(ZAKAT_RATE)
                         .setScale(2, RoundingMode.HALF_UP);
        // => Returns calculated Zakat amount
    }

    // => @Transactional ensures database consistency
    // => Auto-commit on success, auto-rollback on exception
    @Transactional
    public ZakatRecord recordZakatPayment(BigDecimal zakatAmount) {
        // => Create new record
        ZakatRecord record = new ZakatRecord(zakatAmount);
        // => Save to database (generates ID)
        repository.save(record);
        // => Return saved record with ID populated
        return record;
    }

    // => Retrieve all Zakat payment history
    public List<ZakatRecord> getPaymentHistory() {
        return repository.findAll();  // => Fetch all records
    }

    // => Calculate total Zakat paid
    public BigDecimal getTotalZakatPaid() {
        // => Stream all records, sum amounts
        return repository.findAll()
                        .stream()
                        .map(ZakatRecord::getAmount)       // => Extract amounts
                        .reduce(BigDecimal.ZERO, BigDecimal::add);  // => Sum
        // => Returns total amount paid
    }
}
```

**Kotlin version**: `src/main/kotlin/com/ayokoding/zakat/service/ZakatService.kt`

```kotlin
package com.ayokoding.zakat.service

import com.ayokoding.zakat.model.ZakatRecord
import com.ayokoding.zakat.repository.ZakatRepository
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import java.math.BigDecimal
import java.math.RoundingMode

@Service  // => Service stereotype
class ZakatService(
    private val repository: ZakatRepository  // => Constructor injection
) {
    companion object {
        private val ZAKAT_RATE = BigDecimal("0.025")  // => 2.5% rate
    }

    fun calculateZakat(totalWealth: BigDecimal): BigDecimal {
        require(totalWealth > BigDecimal.ZERO) {  // => Kotlin require validation
            "Total wealth must be positive"
        }
        return totalWealth.multiply(ZAKAT_RATE)
                         .setScale(2, RoundingMode.HALF_UP)
        // => Returns calculated amount
    }

    @Transactional  // => Transaction boundary
    fun recordZakatPayment(zakatAmount: BigDecimal): ZakatRecord {
        val record = ZakatRecord(amount = zakatAmount)
        repository.save(record)       // => Persist to database
        return record                 // => Return with ID
    }

    fun getPaymentHistory(): List<ZakatRecord> {
        return repository.findAll()   // => Fetch all
    }

    fun getTotalZakatPaid(): BigDecimal {
        return repository.findAll()
            .sumOf { it.amount }      // => Kotlin sumOf extension
        // => Returns total
    }
}
```

## Step 4: Spring Configuration

Update configuration to enable component scanning and transaction management.

**Update file**: `src/main/java/com/ayokoding/zakat/AppConfig.java`

```java
package com.ayokoding.zakat;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;

@Configuration  // => Marks as Spring config class
@ComponentScan(basePackages = "com.ayokoding.zakat")  // => Scan for components
@EnableTransactionManagement  // => Enable @Transactional support
public class AppConfig {

    // => DataSource bean - database connection pool
    @Bean
    public DataSource dataSource() {
        return new EmbeddedDatabaseBuilder()
            .setType(EmbeddedDatabaseType.H2)        // => H2 embedded database
            .addScript("classpath:schema.sql")       // => Initialize schema
            .build();
        // => Spring manages DataSource lifecycle
    }

    // => JdbcTemplate bean - simplifies JDBC operations
    @Bean
    public JdbcTemplate jdbcTemplate(DataSource dataSource) {
        // => Spring automatically injects DataSource parameter
        return new JdbcTemplate(dataSource);
        // => JdbcTemplate configured with DataSource
    }

    // => TransactionManager bean - manages transactions
    @Bean
    public PlatformTransactionManager transactionManager(DataSource dataSource) {
        // => DataSourceTransactionManager for JDBC transactions
        return new DataSourceTransactionManager(dataSource);
        // => Enables @Transactional annotation support
    }
}
```

## Step 5: Database Schema

**Create file**: `src/main/resources/schema.sql`

```sql
-- => Drop table if exists (clean slate)
DROP TABLE IF EXISTS zakat_records;

-- => Create zakat_records table
CREATE TABLE zakat_records (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,    -- => Auto-increment ID
    amount DECIMAL(15, 2) NOT NULL,          -- => Zakat amount (precise)
    created_at TIMESTAMP NOT NULL            -- => Payment timestamp
);
-- => Table initialized when ApplicationContext starts
```

## Step 6: Main Application

Create console application demonstrating the Zakat calculator.

**Create file**: `src/main/java/com/ayokoding/zakat/ZakatCalculatorApp.java`

```java
package com.ayokoding.zakat;

import com.ayokoding.zakat.model.ZakatRecord;
import com.ayokoding.zakat.service.ZakatService;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.math.BigDecimal;
import java.util.List;
import java.util.Scanner;

public class ZakatCalculatorApp {

    public static void main(String[] args) {
        // => Create Spring ApplicationContext from Java config
        ApplicationContext context = new AnnotationConfigApplicationContext(AppConfig.class);
        // => Loads configuration
        // => Initializes IoC container
        // => Scans for components
        // => Creates and injects beans

        // => Retrieve ZakatService bean from container
        ZakatService zakatService = context.getBean(ZakatService.class);
        // => Spring returns configured service with all dependencies injected

        // => Console interface
        Scanner scanner = new Scanner(System.in);

        System.out.println("=== Zakat Calculator ===");
        System.out.println("Zakat Rate: 2.5% of qualifying wealth");
        System.out.println();

        while (true) {
            System.out.println("1. Calculate Zakat");
            System.out.println("2. Record Payment");
            System.out.println("3. View Payment History");
            System.out.println("4. View Total Paid");
            System.out.println("5. Exit");
            System.out.print("Choose option: ");

            int choice = scanner.nextInt();  // => Read user choice

            switch (choice) {
                case 1 -> calculateZakat(scanner, zakatService);
                case 2 -> recordPayment(scanner, zakatService);
                case 3 -> viewHistory(zakatService);
                case 4 -> viewTotal(zakatService);
                case 5 -> {
                    System.out.println("Exiting...");
                    return;  // => Exit application
                }
                default -> System.out.println("Invalid option");
            }
            System.out.println();
        }
    }

    // => Calculate Zakat from total wealth
    private static void calculateZakat(Scanner scanner, ZakatService service) {
        System.out.print("Enter total wealth: ");
        BigDecimal wealth = scanner.nextBigDecimal();  // => Read wealth amount

        // => Calculate Zakat through service
        BigDecimal zakat = service.calculateZakat(wealth);
        // => Service applies business logic (2.5% calculation)

        System.out.println("Zakat amount: " + zakat);
    }

    // => Record Zakat payment
    private static void recordPayment(Scanner scanner, ZakatService service) {
        System.out.print("Enter Zakat amount to record: ");
        BigDecimal amount = scanner.nextBigDecimal();  // => Read payment amount

        // => Record payment (transactional)
        ZakatRecord record = service.recordZakatPayment(amount);
        // => Service saves to database within transaction

        System.out.println("Payment recorded: " + record);
    }

    // => View payment history
    private static void viewHistory(ZakatService service) {
        // => Retrieve all payment records
        List<ZakatRecord> history = service.getPaymentHistory();

        if (history.isEmpty()) {
            System.out.println("No payment history");
        } else {
            System.out.println("Payment History:");
            history.forEach(System.out::println);  // => Print each record
        }
    }

    // => View total Zakat paid
    private static void viewTotal(ZakatService service) {
        // => Calculate total from all records
        BigDecimal total = service.getTotalZakatPaid();
        System.out.println("Total Zakat paid: " + total);
    }
}
```

**Kotlin version**: `src/main/kotlin/com/ayokoding/zakat/ZakatCalculatorApp.kt`

```kotlin
package com.ayokoding.zakat

import com.ayokoding.zakat.service.ZakatService
import org.springframework.context.annotation.AnnotationConfigApplicationContext
import java.math.BigDecimal
import java.util.Scanner

fun main() {
    // => Create ApplicationContext
    val context = AnnotationConfigApplicationContext(AppConfig::class.java)
    // => Initialize Spring container

    // => Get ZakatService bean
    val zakatService = context.getBean(ZakatService::class.java)
    // => Spring-managed service with dependencies

    val scanner = Scanner(System.`in`)

    println("=== Zakat Calculator ===")
    println("Zakat Rate: 2.5% of qualifying wealth")
    println()

    while (true) {
        println("1. Calculate Zakat")
        println("2. Record Payment")
        println("3. View Payment History")
        println("4. View Total Paid")
        println("5. Exit")
        print("Choose option: ")

        when (scanner.nextInt()) {
            1 -> calculateZakat(scanner, zakatService)
            2 -> recordPayment(scanner, zakatService)
            3 -> viewHistory(zakatService)
            4 -> viewTotal(zakatService)
            5 -> {
                println("Exiting...")
                return  // => Exit
            }
            else -> println("Invalid option")
        }
        println()
    }
}

private fun calculateZakat(scanner: Scanner, service: ZakatService) {
    print("Enter total wealth: ")
    val wealth = scanner.nextBigDecimal()
    val zakat = service.calculateZakat(wealth)
    println("Zakat amount: $zakat")
}

private fun recordPayment(scanner: Scanner, service: ZakatService) {
    print("Enter Zakat amount to record: ")
    val amount = scanner.nextBigDecimal()
    val record = service.recordZakatPayment(amount)
    println("Payment recorded: $record")
}

private fun viewHistory(service: ZakatService) {
    val history = service.getPaymentHistory()
    if (history.isEmpty()) {
        println("No payment history")
    } else {
        println("Payment History:")
        history.forEach { println(it) }
    }
}

private fun viewTotal(service: ZakatService) {
    val total = service.getTotalZakatPaid()
    println("Total Zakat paid: $total")
}
```

## Step 7: Run the Application

### Using Maven

```bash
# => Compile and run
mvn clean compile exec:java -Dexec.mainClass="com.ayokoding.zakat.ZakatCalculatorApp"
# => Compiles all classes
# => Starts ApplicationContext
# => Runs main method
```

### Using Gradle

```bash
# => Run application
./gradlew run
# => Compiles project
# => Executes main class
```

### Sample Interaction

```
=== Zakat Calculator ===
Zakat Rate: 2.5% of qualifying wealth

1. Calculate Zakat
2. Record Payment
3. View Payment History
4. View Total Paid
5. Exit
Choose option: 1
Enter total wealth: 100000
Zakat amount: 2500.00

1. Calculate Zakat
2. Record Payment
3. View Payment History
4. View Total Paid
5. Exit
Choose option: 2
Enter Zakat amount to record: 2500.00
Payment recorded: ZakatRecord{id=1, amount=2500.00, createdAt=2025-01-29T10:30:00}

1. Calculate Zakat
2. Record Payment
3. View Payment History
4. View Total Paid
5. Exit
Choose option: 3
Payment History:
ZakatRecord{id=1, amount=2500.00, createdAt=2025-01-29T10:30:00}
```

## Step 8: Testing

Create tests to verify service behavior.

**Create file**: `src/test/java/com/ayokoding/zakat/ZakatServiceTest.java`

```java
package com.ayokoding.zakat;

import com.ayokoding.zakat.model.ZakatRecord;
import com.ayokoding.zakat.service.ZakatService;
import org.junit.jupiter.api.Test;                          // => JUnit 5 test
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.*;

// => @SpringJUnitConfig loads Spring context for testing
// => Specifies configuration class
@SpringJUnitConfig(AppConfig.class)
public class ZakatServiceTest {

    @Autowired  // => Inject ZakatService bean
    private ZakatService zakatService;
    // => Spring test context provides service with all dependencies

    @Test
    public void testCalculateZakat() {
        // => Given: Total wealth of 100,000
        BigDecimal wealth = new BigDecimal("100000");

        // => When: Calculate Zakat
        BigDecimal zakat = zakatService.calculateZakat(wealth);

        // => Then: Zakat is 2,500 (2.5% of 100,000)
        assertEquals(new BigDecimal("2500.00"), zakat);
    }

    @Test
    public void testRecordPayment() {
        // => Given: Zakat amount
        BigDecimal amount = new BigDecimal("1000.00");

        // => When: Record payment
        ZakatRecord record = zakatService.recordZakatPayment(amount);

        // => Then: Record has ID (generated by database)
        assertNotNull(record.getId());
        // => And amount matches
        assertEquals(amount, record.getAmount());
    }

    @Test
    public void testGetTotalZakatPaid() {
        // => Given: Two payments recorded
        zakatService.recordZakatPayment(new BigDecimal("500.00"));
        zakatService.recordZakatPayment(new BigDecimal("750.00"));

        // => When: Get total paid
        BigDecimal total = zakatService.getTotalZakatPaid();

        // => Then: Total is at least 1,250 (may include previous test data)
        assertTrue(total.compareTo(new BigDecimal("1250.00")) >= 0);
    }
}
```

**Run tests**:

```bash
# => Maven
mvn test
# => Executes all tests with Spring context

# => Gradle
./gradlew test
# => Runs test suite
```

**Expected output**:

```
[INFO] Tests run: 3, Failures: 0, Errors: 0, Skipped: 0
# => All tests pass
# => Spring context loaded successfully
# => Service layer working correctly
```

## What You've Built

Congratulations! You've created a complete Spring Framework application demonstrating:

**Spring Core Container**:

- Java-based configuration with `@Configuration`
- Component scanning with `@ComponentScan`
- Bean lifecycle management

**Dependency Injection**:

- Constructor injection (recommended pattern)
- Automatic dependency resolution

**Data Access Layer**:

- Repository pattern
- JdbcTemplate for simplified JDBC
- Database schema initialization

**Business Logic Layer**:

- Service stereotype with `@Service`
- Transactional methods with `@Transactional`
- Business rule implementation (2.5% Zakat calculation)

**Transaction Management**:

- Declarative transactions
- Automatic commit/rollback

## Key Concepts Demonstrated

### IoC Container

```java
// => Create container from Java config
ApplicationContext context = new AnnotationConfigApplicationContext(AppConfig.class);
// => Container manages all beans
// => Handles dependency injection
// => Controls lifecycle
```

### Dependency Injection

```java
// => Service depends on Repository
// => Spring injects dependency automatically
public ZakatService(ZakatRepository repository) {
    this.repository = repository;  // => No manual instantiation
}
```

### Component Scanning

```java
@ComponentScan(basePackages = "com.ayokoding.zakat")
// => Spring scans package for @Component, @Service, @Repository
// => Automatically registers beans
```

### Declarative Transactions

```java
@Transactional  // => Transaction boundary
public ZakatRecord recordZakatPayment(BigDecimal zakatAmount) {
    // => All database operations in one transaction
    // => Auto-commit on success
    // => Auto-rollback on exception
}
```

## Next Steps

Now that you've built a working Spring Framework application, continue learning:

**[By Example](/en/learn/software-engineering/web-platform/jvm-spring/by-example)** - Learn through 75-90 heavily annotated examples covering:

- Advanced dependency injection patterns
- AOP (Aspect-Oriented Programming)
- Spring Web MVC
- RESTful APIs
- Testing strategies
- Production configurations

**[Spring Boot Tutorial](/en/learn/software-engineering/web-platform/jvm-spring-boot)** - After mastering Spring Framework fundamentals, learn how Spring Boot simplifies configuration and deployment
