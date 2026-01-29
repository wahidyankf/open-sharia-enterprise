---
title: "Intermediate"
date: 2025-12-24T00:00:00+07:00
draft: false
weight: 10000002
description: "Intermediate Spring Boot through 30 examples: transactions, validation, security, caching, async processing, WebSocket, API versioning, and advanced patterns"
tags: ["spring-boot", "tutorial", "by-example", "intermediate", "spring-security", "transactions", "testing", "caching"]
categories: ["learn"]
---

Learn intermediate Spring Boot patterns through 30 annotated examples covering production-ready techniques: transactions, security, testing, caching, async processing, WebSocket, API versioning, and advanced architectural patterns.

## Prerequisites

- Completed [beginner by-example tutorial](/en/learn/software-engineering/web-platform/jvm-spring-boot/by-example/beginner)
- Spring Boot 3.4.x + Java 17
- Understanding of JPA, REST APIs, and dependency injection

## Group 1: Transactions & Data

### Example 21: @Transactional Basics

Spring's declarative transaction management ensures data consistency through ACID properties.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-data-jpa</artifactId>
</dependency>

// Domain model
@Entity
public class BankAccount {
  @Id @GeneratedValue
  private Long id;
  private String owner;
  private BigDecimal balance;
  // constructors, getters, setters
}

@Entity
public class TransferLog {
  @Id @GeneratedValue
  private Long id;
  private Long fromAccount;
  private Long toAccount;
  private BigDecimal amount;
  private LocalDateTime timestamp;
  // constructors, getters, setters
}

// Repository
public interface AccountRepository extends JpaRepository<BankAccount, Long> {}
public interface TransferLogRepository extends JpaRepository<TransferLog, Long> {}

// Service with transactions
@Service
public class TransferService {
  @Autowired private AccountRepository accountRepo;
  // => Injected repository for BankAccount CRUD operations
  @Autowired private TransferLogRepository logRepo;
  // => Injected repository for TransferLog audit trail

  @Transactional // Default: REQUIRED propagation, rollback on RuntimeException
  public void transfer(Long fromId, Long toId, BigDecimal amount) {
    BankAccount from = accountRepo.findById(fromId)
      // => Retrieves source account, throws IllegalArgumentException if not found
      .orElseThrow(() -> new IllegalArgumentException("Source not found"));
    BankAccount to = accountRepo.findById(toId)
      // => Retrieves destination account for credit operation
      .orElseThrow(() -> new IllegalArgumentException("Destination not found"));

    if (from.getBalance().compareTo(amount) < 0) {
      throw new IllegalStateException("Insufficient funds"); // => Rollback entire transaction
    }

    from.setBalance(from.getBalance().subtract(amount));
    // => Debits source account (new balance = old balance - amount)
    to.setBalance(to.getBalance().add(amount));
    // => Credits destination account (new balance = old balance + amount)
    accountRepo.save(from);
    // => Persists updated source account to database
    accountRepo.save(to);
    // => Persists updated destination account to database

    // Log the transfer
    TransferLog log = new TransferLog();
    log.setFromAccount(fromId);
    log.setToAccount(toId);
    log.setAmount(amount);
    log.setTimestamp(LocalDateTime.now());
    // => Records exact time of transfer for audit trail
    logRepo.save(log);
    // => Persists transfer log entry (all-or-nothing with account updates)

    // If exception occurs here, ALL changes (both accounts + log) rollback
  }
}

// Controller
@RestController
@RequestMapping("/api/transfers")
public class TransferController {
  @Autowired private TransferService transferService;

  @PostMapping
  public ResponseEntity<String> transfer(
    @RequestParam Long fromId,
    @RequestParam Long toId,
    @RequestParam BigDecimal amount
  ) {
    try {
      transferService.transfer(fromId, toId, amount);
      return ResponseEntity.ok("Transfer successful");
    } catch (Exception e) {
      return ResponseEntity.badRequest().body(e.getMessage());
    }
  }
}
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts
dependencies {
    implementation("org.springframework.boot:spring-boot-starter-data-jpa")
}

// Domain model using Kotlin data classes with JPA annotations
@Entity
@Table(name = "bank_accounts")
open class BankAccount(
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long? = null,

    var owner: String = "",

    var balance: BigDecimal = BigDecimal.ZERO
    // BigDecimal for precise financial calculations (no floating-point errors)
) {
    // Must be 'open' class for JPA lazy loading proxies
    // Default values provide no-arg constructor for JPA
}

@Entity
@Table(name = "transfer_logs")
open class TransferLog(
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long? = null,

    var fromAccount: Long? = null,
    var toAccount: Long? = null,
    var amount: BigDecimal = BigDecimal.ZERO,
    var timestamp: LocalDateTime = LocalDateTime.now()
)

// Repository interfaces - Kotlin syntax
interface AccountRepository : JpaRepository<BankAccount, Long>
interface TransferLogRepository : JpaRepository<TransferLog, Long>

// Service with transactions using primary constructor injection
@Service
class TransferService(
    private val accountRepo: AccountRepository,
    // => Constructor injection - no @Autowired needed in Kotlin
    private val logRepo: TransferLogRepository
    // => Both repositories injected automatically by Spring
) {

    @Transactional // Default: REQUIRED propagation, rollback on RuntimeException
    fun transfer(fromId: Long, toId: Long, amount: BigDecimal) {
        val from = accountRepo.findById(fromId)
            // => Retrieves source account, throws if not found
            .orElseThrow { IllegalArgumentException("Source not found") }
        // Lambda syntax for exception supplier

        val to = accountRepo.findById(toId)
            // => Retrieves destination account for credit operation
            .orElseThrow { IllegalArgumentException("Destination not found") }

        if (from.balance < amount) {
            // Kotlin operator overloading for BigDecimal comparison
            throw IllegalStateException("Insufficient funds")
            // => Rollback entire transaction
        }

        from.balance = from.balance - amount
        // => Kotlin operator overloading: subtract() method
        // Debits source account (new balance = old balance - amount)

        to.balance = to.balance + amount
        // => Kotlin operator overloading: add() method
        // Credits destination account (new balance = old balance + amount)

        accountRepo.save(from)
        // => Persists updated source account to database
        accountRepo.save(to)
        // => Persists updated destination account to database

        // Log the transfer
        val log = TransferLog(
            fromAccount = fromId,
            toAccount = toId,
            amount = amount,
            timestamp = LocalDateTime.now()
        )
        // => Named parameters make construction clearer
        // Records exact time of transfer for audit trail

        logRepo.save(log)
        // => Persists transfer log entry (all-or-nothing with account updates)

        // If exception occurs here, ALL changes (both accounts + log) rollback
    }
}

// Controller using primary constructor injection
@RestController
@RequestMapping("/api/transfers")
class TransferController(
    private val transferService: TransferService
) {

    @PostMapping
    fun transfer(
        @RequestParam fromId: Long,
        @RequestParam toId: Long,
        @RequestParam amount: BigDecimal
    ): ResponseEntity<String> {
        return try {
            transferService.transfer(fromId, toId, amount)
            ResponseEntity.ok("Transfer successful")
        } catch (e: Exception) {
            ResponseEntity.badRequest().body(e.message)
            // e.message is nullable in Kotlin, handled by Jackson
        }
    }

    // Alternative idiomatic Kotlin using runCatching:
    // fun transfer(...) = runCatching {
    //     transferService.transfer(fromId, toId, amount)
    //     ResponseEntity.ok("Transfer successful")
    // }.getOrElse {
    //     ResponseEntity.badRequest().body(it.message)
    // }
    // Result-based error handling, more functional
}
```

**Key Takeaway**: `@Transactional` ensures all-or-nothing execution—either all database changes commit or all rollback on exception.

**Why It Matters**: Spring's declarative transaction management prevents data corruption from partial failures—without @Transactional, a bank transfer could debit one account but crash before crediting another, creating phantom money. Production systems at PayPal and Stripe rely on transaction boundaries to ensure ACID guarantees, automatically rolling back all database changes when exceptions occur, eliminating manual rollback code that developers forget 40% of the time in non-transactional systems, causing financial discrepancies.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant C as Controller
    participant S as TransferService
    participant DB as Database

    C->>S: transfer(fromId, toId, amount)
    activate S
    Note over S: @Transactional begins
    S->>DB: BEGIN TRANSACTION
    S->>DB: UPDATE account SET balance=... WHERE id=fromId
    S->>DB: UPDATE account SET balance=... WHERE id=toId
    S->>DB: INSERT INTO transfer_log...
    alt Success
        S->>DB: COMMIT
        S-->>C: Success
    else Exception
        S->>DB: ROLLBACK
        S-->>C: Error
    end
    deactivate S

```

---

### Example 22: Isolation Levels

Transaction isolation controls visibility of concurrent changes.

```java
@Service
public class InventoryService {
  @Autowired private ProductRepository productRepo;

  // READ_COMMITTED: Prevents dirty reads
  @Transactional(isolation = Isolation.READ_COMMITTED)
  public int getStock(Long productId) {
    Product p = productRepo.findById(productId).orElseThrow();
    // => Fetches product from database within transaction boundary
    return p.getStock(); // => Sees only committed data from other transactions
  }

  // REPEATABLE_READ: Prevents non-repeatable reads
  @Transactional(isolation = Isolation.REPEATABLE_READ)
  public void processOrder(Long productId, int quantity) {
    Product p = productRepo.findById(productId).orElseThrow();
    // => Fetches product from database within transaction boundary
    int initialStock = p.getStock(); // => 100
    // => Captures initial stock value (locks this value for entire transaction)

    // Simulate delay
    Thread.sleep(1000);

    // Even if another transaction updates stock, this transaction still sees 100
    int currentStock = productRepo.findById(productId).get().getStock(); // => Still 100

    if (currentStock >= quantity) {
      p.setStock(currentStock - quantity);
      productRepo.save(p);
    }
  }

  // SERIALIZABLE: Strictest isolation (rarely needed)
  @Transactional(isolation = Isolation.SERIALIZABLE)
  public void criticalOperation(Long productId) {
    // Locks prevent concurrent access—transactions execute serially
  }
}
```

**Code (Kotlin)**:

```kotlin
@Service
class InventoryService(
    private val productRepo: ProductRepository
    // Constructor injection - no @Autowired needed
) {

    // READ_COMMITTED: Prevents dirty reads
    // Kotlin requires explicit Isolation import
    @Transactional(isolation = Isolation.READ_COMMITTED)
    fun getStock(productId: Long): Int {
        val p = productRepo.findById(productId).orElseThrow()
        // => Fetches product from database within transaction boundary
        return p.stock // => Sees only committed data from other transactions
        // Kotlin property access (no getStock() method call)
    }

    // REPEATABLE_READ: Prevents non-repeatable reads
    @Transactional(isolation = Isolation.REPEATABLE_READ)
    fun processOrder(productId: Long, quantity: Int) {
        val p = productRepo.findById(productId).orElseThrow()
        // => Fetches product from database within transaction boundary
        val initialStock = p.stock // => 100
        // => Captures initial stock value (locks this value for entire transaction)

        // Simulate delay
        Thread.sleep(1000)
        // Kotlin doesn't require try-catch for InterruptedException
        // Unless you specifically need to handle it

        // Even if another transaction updates stock, this transaction still sees 100
        val currentStock = productRepo.findById(productId).get().stock // => Still 100
        // REPEATABLE_READ ensures consistent read within transaction

        if (currentStock >= quantity) {
            p.stock = currentStock - quantity
            productRepo.save(p)
        }
    }

    // SERIALIZABLE: Strictest isolation (rarely needed)
    @Transactional(isolation = Isolation.SERIALIZABLE)
    fun criticalOperation(productId: Long) {
        // Locks prevent concurrent access—transactions execute serially
        // Use only when absolute consistency required (inventory allocation, payment processing)
    }

    // Kotlin-specific: Suspend function for coroutines
    // @Transactional works with suspend functions in Spring 6+
    // @Transactional
    // suspend fun processOrderAsync(productId: Long, quantity: Int) {
    //     val p = productRepo.findById(productId).orElseThrow()
    //     delay(1000) // Non-blocking delay in coroutine
    //     p.stock -= quantity
    //     productRepo.save(p)
    // }
    // Coroutines enable non-blocking transaction processing
}
```

**Key Takeaway**: Higher isolation levels prevent concurrency issues but reduce throughput—choose based on consistency requirements.

**Why It Matters**: Isolation levels balance consistency against concurrency—SERIALIZABLE prevents all concurrency anomalies but reduces throughput to single-threaded performance, while READ_COMMITTED allows higher concurrency but risks non-repeatable reads. Production databases use READ_COMMITTED by default (PostgreSQL, Oracle) to achieve 80% of SERIALIZABLE safety at 300% higher throughput, reserving REPEATABLE_READ for financial transactions where accuracy outweighs performance.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[Transaction Start] --> B{Isolation Level}

    B -->|READ_COMMITTED| C[Prevent Dirty Reads]
    B -->|REPEATABLE_READ| D[Prevent Dirty + Non-Repeatable Reads]
    B -->|SERIALIZABLE| E[Prevent All Anomalies]

    C --> F[High Concurrency<br/>Lower Consistency]
    D --> G[Medium Concurrency<br/>Medium Consistency]
    E --> H[Low Concurrency<br/>Full Consistency]

    F --> I[Production Default]
    G --> J[Financial Systems]
    H --> K[Critical Operations]

    style A fill:#0173B2,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#DE8F05,stroke:#000,color:#000
    style E fill:#CC78BC,stroke:#000,color:#000
    style I fill:#029E73,stroke:#000,color:#fff
```

---

### Example 23: Optimistic Locking

Prevent lost updates with version-based concurrency control.

```java
@Entity
public class Product {
  @Id @GeneratedValue
  private Long id;
  private String name;
  private int stock;

  @Version // Optimistic locking version field
  private Long version;

  // constructors, getters, setters
}

@Service
public class StockService {
  @Autowired private ProductRepository productRepo;

  @Transactional
  public void decreaseStock(Long productId, int quantity) {
    Product product = productRepo.findById(productId).orElseThrow();
    // version = 1

    product.setStock(product.getStock() - quantity);
    productRepo.save(product);
    // SQL: UPDATE product SET stock=?, version=2 WHERE id=? AND version=1

    // If another transaction updated product (version=2), this fails with OptimisticLockException
  }

  // Retry logic for conflicts
  @Transactional
  public void decreaseStockWithRetry(Long productId, int quantity) {
    int maxRetries = 3;
    for (int i = 0; i < maxRetries; i++) {
      try {
        Product product = productRepo.findById(productId).orElseThrow();
        product.setStock(product.getStock() - quantity);
        productRepo.save(product);
        return; // Success
      } catch (OptimisticLockException e) {
        if (i == maxRetries - 1) throw e; // Retries exhausted
        // Retry with fresh data
      }
    }
  }
}
```

**Code (Kotlin)**:

```kotlin
@Entity
@Table(name = "products")
open class Product(
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    var id: Long? = null,

    var name: String = "",
    var stock: Int = 0,

    @Version // Optimistic locking version field
    var version: Long? = null
    // => JPA automatically increments on each update
    // Null initially, set by JPA after first persist
) {
    // Must be 'open' for JPA proxies
}

@Service
class StockService(
    private val productRepo: ProductRepository
) {

    @Transactional
    fun decreaseStock(productId: Long, quantity: Int) {
        val product = productRepo.findById(productId).orElseThrow()
        // version = 1 (current version from database)

        product.stock -= quantity
        // Kotlin operator overloading for subtraction
        productRepo.save(product)
        // SQL: UPDATE product SET stock=?, version=2 WHERE id=? AND version=1
        // => Version automatically incremented to 2

        // If another transaction updated product (version=2), this fails with OptimisticLockException
        // Exception rollbacks current transaction
    }

    // Retry logic for conflicts using Kotlin repeat
    @Transactional
    fun decreaseStockWithRetry(productId: Long, quantity: Int) {
        val maxRetries = 3
        repeat(maxRetries) { attempt ->
            try {
                val product = productRepo.findById(productId).orElseThrow()
                product.stock -= quantity
                productRepo.save(product)
                return // Success - exit function
            } catch (e: OptimisticLockException) {
                if (attempt == maxRetries - 1) throw e // Retries exhausted
                // Retry with fresh data (next iteration)
            }
        }
    }

    // Alternative using Kotlin retry utility (more functional)
    @Transactional
    fun decreaseStockFunctional(productId: Long, quantity: Int) {
        var lastException: OptimisticLockException? = null

        repeat(3) {
            try {
                val product = productRepo.findById(productId).orElseThrow()
                product.stock -= quantity
                productRepo.save(product)
                return // Success
            } catch (e: OptimisticLockException) {
                lastException = e
            }
        }

        throw lastException!! // All retries failed
    }
}
```

**Key Takeaway**: `@Version` prevents lost updates by failing conflicting transactions—use retry logic for conflict resolution.

**Why It Matters**: Optimistic locking enables high-concurrency updates without pessimistic database locks that block other transactions—version numbers detect conflicting updates at commit time instead of blocking readers during writes. E-commerce platforms use optimistic locking for shopping carts where 99% of updates succeed without conflicts, achieving 10x higher throughput than pessimistic locking while preventing lost updates when two users simultaneously buy the last item, with retry logic handling the rare 1% of conflicts gracefully.

---

### Example 24: Batch Operations

Optimize bulk database operations with batching.

```java
// application.properties
spring.jpa.properties.hibernate.jdbc.batch_size=50
spring.jpa.properties.hibernate.order_inserts=true
spring.jpa.properties.hibernate.order_updates=true

@Service
public class BulkImportService {
  @Autowired private ProductRepository productRepo;
  @Autowired private EntityManager entityManager;

  // Inefficient: N+1 queries
  @Transactional
  public void importProductsSlow(List<Product> products) {
    for (Product p : products) {
      productRepo.save(p); // => 1000 products = 1000 INSERT statements
    }
  }

  // Better: Batch inserts
  @Transactional
  public void importProductsFast(List<Product> products) {
    productRepo.saveAll(products); // => Batches 1000 products into 20 INSERTs (50 per batch)
  }

  // Best: Manual batch flushing for large datasets
  @Transactional
  public void importProductsOptimal(List<Product> products) {
    int batchSize = 50;
    for (int i = 0; i < products.size(); i++) {
      entityManager.persist(products.get(i));
      if (i % batchSize == 0 && i > 0) {
        entityManager.flush(); // Force batch execution
        entityManager.clear(); // Free memory
      }
    }
  }

  // Bulk update with JPQL
  @Transactional
  public void discountAllProducts(BigDecimal discountPercent) {
    int updated = entityManager.createQuery(
      "UPDATE Product p SET p.price = p.price * :factor"
    ).setParameter("factor", BigDecimal.ONE.subtract(discountPercent))
     .executeUpdate(); // => Single UPDATE statement for all rows

    System.out.println("Updated " + updated + " products");
  }
}
```

**Code (Kotlin)**:

```properties
# application.properties - same for Kotlin
spring.jpa.properties.hibernate.jdbc.batch_size=50
spring.jpa.properties.hibernate.order_inserts=true
spring.jpa.properties.hibernate.order_updates=true
```

```kotlin
@Service
class ProductImportService(
    private val productRepo: ProductRepository,
    private val entityManager: EntityManager
    // EntityManager injection for manual flush/clear
) {

    @Transactional
    fun importProducts(csvFile: String) {
        val products = parseCsv(csvFile)
        // => Returns List<Product> from CSV parsing

        // Small batch: Use saveAll() - simple and effective
        productRepo.saveAll(products)
        // => Hibernate batches inserts based on batch_size property (50)
        // Executes: INSERT INTO product ... (50 values per batch)
    }

    @Transactional
    fun importLargeDataset(csvFile: String) {
        val products = parseCsv(csvFile)
        // => Could be 100,000+ products

        products.chunked(50).forEach { batch ->
            // Kotlin chunked() splits list into batches of 50
            productRepo.saveAll(batch)
            // => Saves batch of 50 products

            entityManager.flush()
            // => Forces Hibernate to execute SQL immediately
            entityManager.clear()
            // => Clears persistence context (prevents memory leak)
            // Critical for large datasets - releases processed entities from memory
        }

        println("Imported ${products.size} products")
        // String template instead of concatenation
    }

    @Transactional
    fun bulkPriceUpdate(category: String, discount: Double) {
        // JPQL bulk update - most efficient for mass updates
        val updated = entityManager.createQuery(
            "UPDATE Product p SET p.price = p.price * :discount WHERE p.category = :category"
        )
            .setParameter("discount", discount)
            .setParameter("category", category)
            .executeUpdate()
        // => Executes single SQL: UPDATE product SET price = price * ? WHERE category = ?
        // Updates all matching rows without loading entities into memory

        println("Updated $updated products")
        // String template for logging
    }

    private fun parseCsv(file: String): List<Product> {
        // CSV parsing implementation
        return emptyList() // Placeholder
    }
}

// Alternative using Kotlin sequence for streaming large files
@Service
class StreamingImportService(
    private val productRepo: ProductRepository,
    private val entityManager: EntityManager
) {

    @Transactional
    fun importProductsStreaming(csvFile: String) {
        File(csvFile).useLines { lines ->
            // useLines automatically closes file - Kotlin resource management
            lines
                .drop(1) // Skip header row
                .chunked(50) // Process in batches of 50
                .forEach { batch ->
                    val products = batch.map { parseLine(it) }
                    productRepo.saveAll(products)
                    entityManager.flush()
                    entityManager.clear()
                }
        }
        // File automatically closed after useLines block
        // Memory efficient - processes file line by line
    }

    private fun parseLine(line: String): Product {
        // Parse single CSV line to Product
        return Product() // Placeholder
    }
}
```

**Key Takeaway**: Batch operations reduce database round-trips—use `saveAll()` for small batches, manual flushing for large datasets.

**Why It Matters**: Batch operations reduce database roundtrips from 1000 INSERTs to 20 batched INSERTs (50 per batch), decreasing import time from 10 minutes to 30 seconds—critical for ETL jobs processing millions of records. Production data pipelines at Airbnb and Uber use batch updates with manual flush/clear to import terabytes of data nightly without exhausting memory, while JPQL bulk updates execute single SQL statements that modify millions of rows without loading entities into memory.

---

## Group 2: Spring Security

### Example 25: Security Auto-Configuration

Spring Boot auto-configures basic security by default.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-security</artifactId>
</dependency>

// With just the dependency, Spring Boot:
// 1. Generates random password on startup (console log)
// 2. Secures all endpoints with HTTP Basic Auth
// 3. Default username: "user"
// 4. CSRF protection enabled
// 5. Session management configured

@RestController
public class SecuredController {
  @GetMapping("/public")
  public String publicEndpoint() {
    return "Accessible without auth"; // => Still requires login by default!
  }

  @GetMapping("/api/data")
  public String securedEndpoint() {
    return "Protected data"; // => Requires authentication
  }
}

// Run the app and see console:
// Using generated security password: a1b2c3d4-e5f6-7890-abcd-ef1234567890
// curl http://localhost:8080/api/data => 401 Unauthorized
// curl -u user:a1b2c3d4-e5f6-7890-abcd-ef1234567890 http://localhost:8080/api/data => "Protected data"
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts
dependencies {
    implementation("org.springframework.boot:spring-boot-starter-security")
}

// With just the dependency, Spring Boot:
// 1. Generates random password on startup (console log)
// 2. Secures all endpoints with HTTP Basic Auth
// 3. Default username: "user"
// 4. CSRF protection enabled
// 5. Session management configured

@RestController
class SecuredController {

    @GetMapping("/public")
    fun publicEndpoint(): String {
        return "Accessible without auth" // => Still requires login by default!
        // Spring Security secures ALL endpoints unless explicitly permitted
    }

    @GetMapping("/api/data")
    fun securedEndpoint(): String {
        return "Protected data" // => Requires authentication
        // Returns data only if authenticated via HTTP Basic
    }
}

// Run the app and see console:
// Using generated security password: a1b2c3d4-e5f6-7890-abcd-ef1234567890
// curl http://localhost:8080/api/data => 401 Unauthorized
// curl -u user:a1b2c3d4-e5f6-7890-abcd-ef1234567890 http://localhost:8080/api/data => "Protected data"

// Kotlin tip: Access authenticated user in controller
@RestController
class AuthenticatedController {

    @GetMapping("/me")
    fun currentUser(@AuthenticationPrincipal user: UserDetails): String {
        // @AuthenticationPrincipal injects authenticated user
        return "Hello, ${user.username}"
        // String template for user greeting
    }

    // Alternative using SecurityContextHolder (less idiomatic)
    @GetMapping("/me-manual")
    fun currentUserManual(): String {
        val auth = SecurityContextHolder.getContext().authentication
        return "Hello, ${auth.name}"
    }
}
```

**Key Takeaway**: Spring Security auto-configuration secures everything by default—customize with `SecurityFilterChain` beans.

**Why It Matters**: Spring Security's auto-configuration prevents 80% of OWASP Top 10 vulnerabilities (CSRF, session fixation, clickjacking) through secure defaults, eliminating manual security code that developers implement incorrectly. However, default form login exposes application structure through /login pages—production systems replace it with JWT or OAuth2 for stateless authentication that scales horizontally without session affinity, enabling load balancers to distribute traffic across instances without sticky sessions.

---

### Example 26: Custom Authentication

Configure users, passwords, and access rules.

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http // => Configure HTTP security chain
      .authorizeHttpRequests(auth -> auth // => Define authorization rules
        .requestMatchers("/public/**").permitAll() // => Public endpoints (no auth required)
        .requestMatchers("/admin/**").hasRole("ADMIN") // => Admin endpoints (requires ADMIN role)
        .anyRequest().authenticated() // => All other endpoints require authentication
      )
      .formLogin(form -> form // => Configure form-based login
        .loginPage("/login") // => Custom login page URL
        .permitAll()
      )
      .logout(logout -> logout.permitAll()); // => Enable logout endpoint

    return http.build(); // => Build SecurityFilterChain bean
  }

  @Bean
  public UserDetailsService userDetailsService() {
    // => In-memory users (for demo—use database/LDAP in production)
    UserDetails user = User.builder() // => Create user with builder pattern
      .username("user") // => Username for authentication
      .password(passwordEncoder().encode("password123")) // => BCrypt-hashed password
      .roles("USER") // => Granted authority: ROLE_USER
      .build(); // => Immutable UserDetails object

    UserDetails admin = User.builder() // => Create admin user
      .username("admin") // => Admin username
      .password(passwordEncoder().encode("admin123")) // => BCrypt-hashed admin password
      .roles("ADMIN", "USER") // => Multiple roles: ROLE_ADMIN + ROLE_USER
      .build(); // => Admin has both USER and ADMIN privileges

    return new InMemoryUserDetailsManager(user, admin); // => Store users in memory (non-persistent)
  }

  @Bean
  public PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder(); // => BCrypt with automatic salt generation (industry standard)
  }
}

// Controller
@RestController
public class ApiController {
  @GetMapping("/public/hello")
  public String publicHello() {
    return "Public endpoint"; // => Accessible without login
  }

  @GetMapping("/api/user-data")
  public String userData() {
    return "User data"; // => Requires USER or ADMIN role
  }

  @GetMapping("/admin/dashboard")
  public String adminDashboard() {
    return "Admin dashboard"; // => Requires ADMIN role only
  }
}
```

**Code (Kotlin)**:

```kotlin
@Configuration
@EnableWebSecurity
open class SecurityConfig {

    @Bean
    open fun filterChain(http: HttpSecurity): SecurityFilterChain {
        // Kotlin DSL for Spring Security configuration
        http {
            // Lambda-based configuration (Kotlin DSL)
            authorizeHttpRequests {
                authorize("/public/**", permitAll) // => Public endpoints (no auth required)
                authorize("/admin/**", hasRole("ADMIN")) // => Admin endpoints (requires ADMIN role)
                authorize(anyRequest, authenticated) // => All other endpoints require authentication
            }
            formLogin {
                loginPage = "/login" // => Custom login page URL
                permitAll()
            }
            logout {
                permitAll()
            }
        }
        return http.build() // => Build SecurityFilterChain bean
    }

    @Bean
    open fun userDetailsService(): UserDetailsService {
        // In-memory users (for demo—use database/LDAP in production)
        val user = User.builder() // => Create user with builder pattern
            .username("user") // => Username for authentication
            .password(passwordEncoder().encode("password123")) // => BCrypt-hashed password
            .roles("USER") // => Granted authority: ROLE_USER
            .build() // => Immutable UserDetails object

        val admin = User.builder() // => Create admin user
            .username("admin") // => Admin username
            .password(passwordEncoder().encode("admin123")) // => BCrypt-hashed admin password
            .roles("ADMIN", "USER") // => Multiple roles: ROLE_ADMIN + ROLE_USER
            .build() // => Admin has both USER and ADMIN privileges

        return InMemoryUserDetailsManager(user, admin)
        // => Store users in memory (non-persistent)
        // Kotlin allows direct constructor call without 'new'
    }

    @Bean
    open fun passwordEncoder(): PasswordEncoder {
        return BCryptPasswordEncoder()
        // => BCrypt with automatic salt generation (industry standard)
    }
}

// Controller using Kotlin
@RestController
class ApiController {

    @GetMapping("/public/hello")
    fun publicHello() = "Public endpoint"
    // Expression body - concise for simple returns
    // => Accessible without login

    @GetMapping("/api/user-data")
    fun userData() = "User data"
    // => Requires USER or ADMIN role

    @GetMapping("/admin/dashboard")
    fun adminDashboard() = "Admin dashboard"
    // => Requires ADMIN role only

    // Access authenticated user details
    @GetMapping("/api/profile")
    fun userProfile(@AuthenticationPrincipal user: UserDetails): Map<String, Any> {
        return mapOf(
            "username" to user.username,
            "authorities" to user.authorities.map { it.authority }
        )
        // => Returns {"username":"user","authorities":["ROLE_USER"]}
        // mapOf creates immutable map, to is infix function for Pair
    }
}
```

**Key Takeaway**: `SecurityFilterChain` defines authorization rules—combine with `UserDetailsService` for custom user storage.

**Why It Matters**: Custom authentication and authorization with SecurityFilterChain provides fine-grained access control beyond default form login—URL patterns, HTTP methods, and user roles combine to enforce security policies that prevent unauthorized access. Production applications use method-level security (@PreAuthorize, @Secured) for business logic protection where URL security alone is insufficient, implementing complex authorization rules (resource ownership, tenant isolation) that URL patterns cannot express.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart TD
    A[HTTP Request] --> B{Security Filter Chain}
    B -->|/public/**| C[Permit All]
    B -->|/admin/**| D{Has ADMIN Role?}
    B -->|Other| E{Authenticated?}

    D -->|Yes| F[Allow Access]
    D -->|No| G[403 Forbidden]

    E -->|Yes| F
    E -->|No| H[Redirect to Login]

    H --> I[UserDetailsService]
    I --> J[Check Credentials]
    J -->|Valid| K[Create Authentication]
    J -->|Invalid| L[401 Unauthorized]

    K --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#000
    style F fill:#029E73,stroke:#000,color:#fff
    style G fill:#CC78BC,stroke:#000,color:#000
    style L fill:#CC78BC,stroke:#000,color:#000
```

---

### Example 27: Method-Level Authorization

Secure individual methods with annotations.

```java
@Configuration
@EnableMethodSecurity // Enable method security annotations
public class MethodSecurityConfig {}

@Service
public class OrderService {
  @Autowired private OrderRepository orderRepo;

  @PreAuthorize("hasRole('USER')") // => Check role BEFORE method execution
  public List<Order> getMyOrders(String username) { // => username parameter from authenticated user
    return orderRepo.findByUsername(username); // => Query orders filtered by username
  }

  @PreAuthorize("hasRole('ADMIN')") // => ADMIN role required (throws AccessDeniedException if missing)
  public List<Order> getAllOrders() { // => Admin-only endpoint
    return orderRepo.findAll(); // => Returns ALL orders (no filtering)
  }

  @PreAuthorize("#username == authentication.name") // => SpEL expression: method param matches authenticated username
  public Order getOrder(String username, Long orderId) {
    return orderRepo.findByIdAndUsername(orderId, username) // => Query with composite key
      .orElseThrow(() -> new AccessDeniedException("Not authorized")); // => Explicit access denial
  }

  @PreAuthorize("hasRole('ADMIN') or #order.username == authentication.name") // => Admin OR resource owner can update
  public Order updateOrder(Order order) {
    return orderRepo.save(order); // => Persist updated order (authorization already checked)
  }

  @PostAuthorize("returnObject.username == authentication.name") // => Check AFTER execution (compare returned order's owner)
  public Order loadOrder(Long orderId) {
    return orderRepo.findById(orderId).orElseThrow(); // => Fetch order first
    // => THEN Spring verifies returnObject.username matches authentication.name
  }
}

// Controller
@RestController
@RequestMapping("/api/orders")
public class OrderController {
  @Autowired private OrderService orderService;

  @GetMapping("/my-orders") // => Endpoint: GET /api/orders/my-orders
  public List<Order> getMyOrders(@AuthenticationPrincipal UserDetails user) { // => Inject authenticated user
    return orderService.getMyOrders(user.getUsername()); // => Pass authenticated username to service layer
  }

  @GetMapping("/all")
  public List<Order> getAllOrders() {
    return orderService.getAllOrders(); // => Service method checks @PreAuthorize("hasRole('ADMIN')")
  }
}
```

**Code (Kotlin)**:

```kotlin
@Configuration
@EnableMethodSecurity // Enable method security annotations in Kotlin
open class MethodSecurityConfig

@Service
class OrderService(
    private val orderRepo: OrderRepository
) {

    @PreAuthorize("hasRole('USER')") // => Check role BEFORE method execution
    fun getMyOrders(username: String): List<Order> {
        // username parameter from authenticated user
        return orderRepo.findByUsername(username)
        // => Query orders filtered by username
    }

    @PreAuthorize("hasRole('ADMIN')")
    // => ADMIN role required (throws AccessDeniedException if missing)
    fun getAllOrders(): List<Order> {
        return orderRepo.findAll() // => Returns ALL orders (no filtering)
    }

    @PreAuthorize("#username == authentication.name")
    // => SpEL expression: method param matches authenticated username
    // #username refers to method parameter, authentication is Spring Security context
    fun getOrder(username: String, orderId: Long): Order {
        return orderRepo.findByIdAndUsername(orderId, username)
            .orElseThrow { AccessDeniedException("Not authorized") }
        // => Kotlin lambda for exception supplier
    }

    @PreAuthorize("hasRole('ADMIN') or #order.username == authentication.name")
    // => Admin OR resource owner can update
    // SpEL expression with 'or' operator and property access
    fun updateOrder(order: Order): Order {
        return orderRepo.save(order)
        // => Persist updated order (authorization already checked)
    }

    @PostAuthorize("returnObject.username == authentication.name")
    // => Check AFTER execution (compare returned order's owner)
    // returnObject is SpEL variable for method return value
    fun loadOrder(orderId: Long): Order {
        return orderRepo.findById(orderId).orElseThrow()
        // => Fetch order first
        // => THEN Spring verifies returnObject.username matches authentication.name
    }
}

// Controller using Kotlin
@RestController
@RequestMapping("/api/orders")
class OrderController(
    private val orderService: OrderService
) {

    @GetMapping("/my-orders")
    // => Endpoint: GET /api/orders/my-orders
    fun getMyOrders(@AuthenticationPrincipal user: UserDetails): List<Order> {
        // => Inject authenticated user details
        return orderService.getMyOrders(user.username)
        // => Pass authenticated username to service layer
        // Kotlin property access (no getUsername() call)
    }

    @GetMapping("/all")
    fun getAllOrders(): List<Order> {
        return orderService.getAllOrders()
        // => Service method checks @PreAuthorize("hasRole('ADMIN')")
    }

    // Kotlin-specific: Extension function for SpEL expressions
    @GetMapping("/{id}")
    fun getOrder(
        @PathVariable id: Long,
        @AuthenticationPrincipal user: UserDetails
    ): Order {
        return orderService.getOrder(user.username, id)
        // Method-level security validates access
    }
}
```

**Key Takeaway**: `@PreAuthorize` and `@PostAuthorize` enable fine-grained authorization at the method level using SpEL expressions.

**Why It Matters**: Method-level authorization enforces business rules at the code level where they can't be bypassed—URL-level security (`/admin/**`) fails when developers add new endpoints that forget URL patterns, while `@PreAuthorize` prevents access attempts at method invocation. Production SaaS applications use SpEL expressions for tenant isolation (`#order.tenantId == principal.tenantId`) that ensure users can't access other tenants' data even if URL tampering bypasses endpoint security, preventing data leaks that cause compliance violations and customer churn.

---

### Example 28: JWT Authentication

Implement stateless authentication with JSON Web Tokens.

```java
// pom.xml
<dependency>
  <groupId>io.jsonwebtoken</groupId>
  <artifactId>jjwt-api</artifactId>
  <version>0.12.3</version>
</dependency>
<dependency>
  <groupId>io.jsonwebtoken</groupId>
  <artifactId>jjwt-impl</artifactId>
  <version>0.12.3</version>
</dependency>

// JWT utility class
@Component
public class JwtUtil {
  private String secret = "mySecretKey1234567890123456789012"; // 256-bit key
  private long expiration = 86400000; // 24 hours

  public String generateToken(String username) { // => Create JWT token for authenticated user
    return Jwts.builder() // => Start JWT builder
      .subject(username) // => Set "sub" claim (token owner)
      .issuedAt(new Date()) // => Set "iat" claim (issued at timestamp)
      .expiration(new Date(System.currentTimeMillis() + expiration)) // => Set "exp" claim (24h from now)
      .signWith(Keys.hmacShaKeyFor(secret.getBytes())) // => Sign with HS256 algorithm
      .compact(); // => Serialize to Base64-encoded string
    // => Result format: header.payload.signature (JWT standard)
  }

  public String extractUsername(String token) { // => Parse JWT and extract username
    return Jwts.parser() // => Create JWT parser
      .verifyWith(Keys.hmacShaKeyFor(secret.getBytes())) // => Verify signature with same secret key
      .build() // => Build parser instance
      .parseSignedClaims(token) // => Parse and validate JWT (throws if invalid/expired)
      .getPayload() // => Extract claims (payload section)
      .getSubject(); // => Get "sub" claim (username)
  }

  public boolean isTokenValid(String token, String username) { // => Comprehensive token validation
    try {
      String extractedUser = extractUsername(token); // => Parse token (throws if tampered/invalid)
      return extractedUser.equals(username) && !isTokenExpired(token); // => Check username match + expiration
    } catch (Exception e) { // => Catch signature verification failures, malformed tokens
      return false; // => Invalid token
    }
  }

  private boolean isTokenExpired(String token) { // => Check if token has expired
    Date expiration = Jwts.parser() // => Create parser
      .verifyWith(Keys.hmacShaKeyFor(secret.getBytes())) // => Verify signature
      .build() // => Build parser
      .parseSignedClaims(token) // => Parse JWT
      .getPayload() // => Extract claims
      .getExpiration(); // => Get "exp" claim (expiration timestamp)
    return expiration.before(new Date()); // => Compare exp with current time
  }
}

// JWT authentication filter
@Component
public class JwtAuthFilter extends OncePerRequestFilter {
  @Autowired private JwtUtil jwtUtil; // => Inject JWT utility for token operations
  @Autowired private UserDetailsService userDetailsService; // => Inject user details service

  @Override
  protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
    throws ServletException, IOException {

    String header = request.getHeader("Authorization"); // => Extract Authorization header
    if (header != null && header.startsWith("Bearer ")) { // => Check for Bearer token format
      String token = header.substring(7); // => Remove "Bearer " prefix (7 chars)
      String username = jwtUtil.extractUsername(token); // => Parse JWT to extract username

      if (username != null && SecurityContextHolder.getContext().getAuthentication() == null) { // => Valid username + not already authenticated
        UserDetails user = userDetailsService.loadUserByUsername(username); // => Load user from database/cache
        if (jwtUtil.isTokenValid(token, username)) { // => Verify signature + expiration + username match
          UsernamePasswordAuthenticationToken auth = new UsernamePasswordAuthenticationToken( // => Create Spring Security auth token
            user, null, user.getAuthorities() // => principal, credentials (null for JWT), authorities
          );
          SecurityContextHolder.getContext().setAuthentication(auth); // => Set authentication in security context
        }
      }
    }
    chain.doFilter(request, response); // => Continue filter chain (with or without authentication)
  }
}

// Security config
@Configuration
@EnableWebSecurity
public class JwtSecurityConfig {
  @Autowired private JwtAuthFilter jwtAuthFilter;

  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .csrf(csrf -> csrf.disable()) // => Disable CSRF (not needed for stateless JWT APIs)
      .authorizeHttpRequests(auth -> auth // => Configure authorization
        .requestMatchers("/auth/**").permitAll() // => Public auth endpoints (login, register)
        .anyRequest().authenticated() // => All other endpoints require JWT
      )
      .sessionManagement(session -> session // => Configure session management
        .sessionCreationPolicy(SessionCreationPolicy.STATELESS) // => No HTTP sessions (JWT is stateless)
      )
      .addFilterBefore(jwtAuthFilter, UsernamePasswordAuthenticationFilter.class); // => Insert JWT filter before default auth filter

    return http.build(); // => Build SecurityFilterChain bean
  }
}

// Auth controller
@RestController
@RequestMapping("/auth")
public class AuthController {
  @Autowired private AuthenticationManager authManager; // => Spring Security authentication manager
  @Autowired private JwtUtil jwtUtil; // => Inject JWT utility

  @PostMapping("/login") // => Endpoint: POST /auth/login
  public ResponseEntity<String> login(@RequestBody LoginRequest request) { // => Accept JSON login request
    authManager.authenticate( // => Validate username + password (throws if invalid)
      new UsernamePasswordAuthenticationToken(request.getUsername(), request.getPassword()) // => Create auth token
    ); // => If authentication fails, throws BadCredentialsException
    String token = jwtUtil.generateToken(request.getUsername()); // => Generate JWT for authenticated user
    return ResponseEntity.ok(token); // => Return JWT to client
  }
}

// Usage:
// POST /auth/login {"username":"user1","password":"pass123"}
// Response: "eyJhbGciOiJIUzI1NiJ9..."
// GET /api/data with Header: Authorization: Bearer eyJhbGciOiJIUzI1NiJ9...
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts
dependencies {
    implementation("io.jsonwebtoken:jjwt-api:0.12.3")
    runtimeOnly("io.jsonwebtoken:jjwt-impl:0.12.3")
    runtimeOnly("io.jsonwebtoken:jjwt-jackson:0.12.3")
}

// JWT utility class in Kotlin
@Component
class JwtUtil {
    private val secret = "mySecretKey1234567890123456789012" // 256-bit key
    private val expiration = 86400000L // 24 hours (Long type)

    fun generateToken(username: String): String {
        return Jwts.builder()
            .subject(username) // => Set "sub" claim (token owner)
            .issuedAt(Date()) // => Set "iat" claim (issued at timestamp)
            .expiration(Date(System.currentTimeMillis() + expiration))
            .signWith(Keys.hmacShaKeyFor(secret.toByteArray()))
            .compact() // => Serialize to Base64-encoded string
        // => Result format: header.payload.signature (JWT standard)
    }

    fun extractUsername(token: String): String {
        return Jwts.parser()
            .verifyWith(Keys.hmacShaKeyFor(secret.toByteArray()))
            .build()
            .parseSignedClaims(token)
            .payload
            .subject // Kotlin property access
    }

    fun isTokenValid(token: String, username: String): Boolean {
        return try {
            val extractedUser = extractUsername(token)
            extractedUser == username && !isTokenExpired(token)
        } catch (e: Exception) {
            false // Invalid token
        }
    }

    private fun isTokenExpired(token: String): Boolean {
        val expiration = Jwts.parser()
            .verifyWith(Keys.hmacShaKeyFor(secret.toByteArray()))
            .build()
            .parseSignedClaims(token)
            .payload
            .expiration
        return expiration.before(Date())
    }
}

// JWT filter extending OncePerRequestFilter
@Component
class JwtAuthFilter(
    private val jwtUtil: JwtUtil,
    private val userDetailsService: UserDetailsService
) : OncePerRequestFilter() {

    override fun doFilterInternal(
        request: HttpServletRequest,
        response: HttpServletResponse,
        filterChain: FilterChain
    ) {
        val header = request.getHeader("Authorization")
        if (header != null && header.startsWith("Bearer ")) {
            val token = header.substring(7)
            val username = jwtUtil.extractUsername(token)

            if (username != null && SecurityContextHolder.getContext().authentication == null) {
                val user = userDetailsService.loadUserByUsername(username)
                if (jwtUtil.isTokenValid(token, username)) {
                    val auth = UsernamePasswordAuthenticationToken(
                        user, null, user.authorities
                    )
                    SecurityContextHolder.getContext().authentication = auth
                }
            }
        }
        filterChain.doFilter(request, response)
    }
}

// Security config with JWT
@Configuration
@EnableWebSecurity
open class JwtSecurityConfig(
    private val jwtAuthFilter: JwtAuthFilter
) {

    @Bean
    open fun filterChain(http: HttpSecurity): SecurityFilterChain {
        http {
            csrf { disable() }
            authorizeHttpRequests {
                authorize("/auth/**", permitAll)
                authorize(anyRequest, authenticated)
            }
            sessionManagement {
                sessionCreationPolicy = SessionCreationPolicy.STATELESS
            }
            addFilterBefore<UsernamePasswordAuthenticationFilter>(jwtAuthFilter)
        }
        return http.build()
    }
}

// Login request DTO
data class LoginRequest(
    val username: String,
    val password: String
)

// Auth controller
@RestController
@RequestMapping("/auth")
class AuthController(
    private val authManager: AuthenticationManager,
    private val jwtUtil: JwtUtil
) {

    @PostMapping("/login")
    fun login(@RequestBody request: LoginRequest): ResponseEntity<String> {
        authManager.authenticate(
            UsernamePasswordAuthenticationToken(request.username, request.password)
        )
        val token = jwtUtil.generateToken(request.username)
        return ResponseEntity.ok(token)
    }
}

// Usage:
// POST /auth/login {"username":"user1","password":"pass123"}
// Response: "eyJhbGciOiJIUzI1NiJ9..."
// GET /api/data with Header: Authorization: Bearer eyJhbGciOiJIUzI1NiJ9...
```

**Key Takeaway**: JWT enables stateless authentication—clients include tokens in headers, eliminating server-side session storage.

**Why It Matters**: JWT enables stateless authentication essential for horizontal scaling—servers don't share session state, so load balancers distribute requests to any instance without session replication overhead. However, JWT tokens can't be revoked before expiration (unlike sessions), requiring short expiration times (15 minutes) with refresh tokens for security, balancing convenience (fewer re-logins) against blast radius (stolen tokens valid until expiration), a trade-off production systems at Auth0 and Okta tune based on threat models.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant C as Client
    participant A as Auth Controller
    participant F as JWT Filter
    participant S as Secured API

    C->>A: POST /auth/login (username, password)
    A->>A: Authenticate credentials
    A->>A: Generate JWT token
    A-->>C: Return JWT

    Note over C: Store JWT #40;localStorage/cookie#41;

    C->>F: GET /api/data<br/>Header: Authorization: Bearer {JWT}
    F->>F: Extract & validate JWT
    F->>F: Load user details
    F->>F: Set SecurityContext
    F->>S: Forward request (authenticated)
    S-->>C: Return data

```

---

### Example 29: OAuth2 Integration

Enable social login with OAuth2 providers.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-oauth2-client</artifactId>
</dependency>

// application.yml
spring:
  security:
    oauth2:
      client:
        registration:
          google:
            client-id: YOUR_GOOGLE_CLIENT_ID
            client-secret: YOUR_GOOGLE_CLIENT_SECRET
            scope:
              - email
              - profile
          github:
            client-id: YOUR_GITHUB_CLIENT_ID
            client-secret: YOUR_GITHUB_CLIENT_SECRET
            scope:
              - user:email
              - read:user

// Security config
@Configuration
@EnableWebSecurity
public class OAuth2Config {
  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http
      .authorizeHttpRequests(auth -> auth // => Configure URL authorization
        .requestMatchers("/", "/login", "/error").permitAll() // => Public pages
        .anyRequest().authenticated() // => All others require OAuth2 login
      )
      .oauth2Login(oauth2 -> oauth2 // => Configure OAuth2 login
        .loginPage("/login") // => Custom login page (links to Google/GitHub)
        .defaultSuccessUrl("/dashboard") // => Redirect after successful OAuth2 authentication
      );

    return http.build(); // => Build SecurityFilterChain bean
  }
}

// Controller
@RestController
public class ProfileController {
  @GetMapping("/dashboard") // => Protected endpoint
  public String dashboard(@AuthenticationPrincipal OAuth2User principal) { // => Inject OAuth2 authenticated user
    String name = principal.getAttribute("name"); // => Extract "name" claim from provider
    String email = principal.getAttribute("email"); // => Extract "email" claim
    return "Welcome, " + name + " (" + email + ")";
    // => Extracts user info from OAuth2 provider
  }

  @GetMapping("/user-info")
  public Map<String, Object> userInfo(@AuthenticationPrincipal OAuth2User principal) {
    return principal.getAttributes(); // => Complete OAuth2 profile (name, email, picture, etc.)
  }
}

// Flow:
// 1. User clicks "Login with Google"
// 2. Redirects to Google's login page
// 3. User authenticates with Google
// 4. Google redirects back with authorization code
// 5. Spring exchanges code for access token
// 6. Spring fetches user info from Google
// 7. User logged in, redirected to /dashboard
```

**Code (Kotlin)**:

```kotlin
// application.properties
spring.security.oauth2.client.registration.google.client-id=YOUR_CLIENT_ID
spring.security.oauth2.client.registration.google.client-secret=YOUR_SECRET
spring.security.oauth2.client.registration.google.scope=profile,email

// Security configuration
@Configuration
@EnableWebSecurity
open class OAuth2SecurityConfig {
  @Bean
  open fun filterChain(http: HttpSecurity): SecurityFilterChain {
    http {
      authorizeHttpRequests {
        authorize("/", permitAll)
        authorize("/login/**", permitAll)  // OAuth2 login endpoints
        authorize(anyRequest, authenticated)
      }
      oauth2Login {
        defaultSuccessUrl("/dashboard", true)  // Redirect after successful login
      }
      // => Enables OAuth2 login flow with Google
    }
    return http.build()
  }
}

// Controller handling OAuth2 user
@RestController
class OAuth2Controller {
  @GetMapping("/dashboard")
  fun dashboard(@AuthenticationPrincipal principal: OAuth2User): String {
    val name = principal.getAttribute<String>("name") ?: "User"
    return "Welcome, $name!"  // String template with null safety
    // => Extracts user info from OAuth2 provider
  }

  @GetMapping("/user-info")
  fun userInfo(@AuthenticationPrincipal principal: OAuth2User): Map<String, Any> {
    return principal.attributes  // Complete OAuth2 profile (name, email, picture, etc.)
  }
}

// Flow:
// 1. User clicks "Login with Google"
// 2. Redirects to Google's login page
// 3. User authenticates with Google
// 4. Google redirects back with authorization code
// 5. Spring exchanges code for access token
// 6. Spring fetches user info from Google
// 7. User logged in, redirected to /dashboard

// Kotlin-specific: Use getAttribute<T> with type parameter for type-safe extraction
// Alternative with scope functions:
// val name = principal.getAttribute<String>("name")?.also { println("User $it logged in") } ?: "User"
```

**Key Takeaway**: Spring OAuth2 client simplifies social login—configure provider credentials and Spring handles the OAuth2 flow.

**Why It Matters**: OAuth2 delegates authentication to specialized providers (Google, GitHub, AWS Cognito) that invest millions in security infrastructure, enabling applications to avoid storing passwords that require expensive PCI/SOC2 compliance. Social login reduces signup friction by 30-50% (no password memorization), but introduces dependency on external providers—production systems implement fallback authentication for when OAuth providers have outages, preventing login failures that lock out all users during downtime.

---

## Group 3: Testing

### Example 30: @SpringBootTest

Full integration testing with complete application context.

```java
// Test class
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class ProductControllerIntegrationTest {
  @Autowired private TestRestTemplate restTemplate; // HTTP client for testing
  @Autowired private ProductRepository productRepo;

  @BeforeEach
  void setup() {
    productRepo.deleteAll(); // Clean database before each test
  }

  @Test
  void testCreateAndRetrieveProduct() {
    // Create product
    Product product = new Product("Laptop", 999.99);
    ResponseEntity<Product> createResponse = restTemplate.postForEntity(
      "/api/products", product, Product.class
    );
    assertEquals(HttpStatus.CREATED, createResponse.getStatusCode());
    Long productId = createResponse.getBody().getId();

    // Retrieve product
    ResponseEntity<Product> getResponse = restTemplate.getForEntity(
      "/api/products/" + productId, Product.class
    );
    assertEquals(HttpStatus.OK, getResponse.getStatusCode());
    assertEquals("Laptop", getResponse.getBody().getName());
    // => Full end-to-end test: controller → service → repository → database
  }

  @Test
  void testDeleteProduct() {
    Product product = productRepo.save(new Product("Mouse", 25.00));

    restTemplate.delete("/api/products/" + product.getId());

    assertFalse(productRepo.existsById(product.getId()));
    // => Verifies deletion through full stack
  }
}

// Mocking external dependencies
@SpringBootTest
public class OrderServiceTest {
  @Autowired private OrderService orderService;

  @MockBean // Replace real bean with mock
  private PaymentGateway paymentGateway;

  @Test
  void testProcessOrder() {
    // Stub mock behavior
    when(paymentGateway.charge(any(), any())).thenReturn(true);

    Order order = new Order("user1", 100.00);
    orderService.processOrder(order);

    verify(paymentGateway, times(1)).charge(eq("user1"), eq(100.00));
    // => Tests order processing without calling real payment gateway
  }
}
```

**Code (Kotlin)**:

```kotlin
// Test class
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class ProductControllerIntegrationTest {
  @Autowired private lateinit var restTemplate: TestRestTemplate  // HTTP client for testing
  @Autowired private lateinit var productRepo: ProductRepository

  @BeforeEach
  fun setup() {
    productRepo.deleteAll()  // Clean database before each test
  }

  @Test
  fun testCreateAndRetrieveProduct() {
    // Create product
    val product = Product("Laptop", 999.99)
    val createResponse = restTemplate.postForEntity(
      "/api/products", product, Product::class.java
    )
    assertEquals(HttpStatus.CREATED, createResponse.statusCode)
    val productId = createResponse.body!!.id  // Non-null assertion (test environment controlled)

    // Retrieve product
    val getResponse = restTemplate.getForEntity(
      "/api/products/$productId", Product::class.java  // String template
    )
    assertEquals(HttpStatus.OK, getResponse.statusCode)
    assertEquals("Laptop", getResponse.body!!.name)
    // => Full end-to-end test: controller → service → repository → database
  }

  @Test
  fun testDeleteProduct() {
    val product = productRepo.save(Product("Mouse", 25.00))

    restTemplate.delete("/api/products/${product.id}")  // String template for URL

    assertFalse(productRepo.existsById(product.id!!))
    // => Verifies deletion through full stack
  }
}

// Mocking external dependencies
@SpringBootTest
class OrderServiceTest {
  @Autowired private lateinit var orderService: OrderService

  @MockBean  // Replace real bean with mock
  private lateinit var paymentGateway: PaymentGateway

  @Test
  fun testProcessOrder() {
    // Stub mock behavior
    `when`(paymentGateway.charge(any(), any())).thenReturn(true)  // Backticks escape Kotlin keyword

    val order = Order("user1", 100.00)
    orderService.processOrder(order)

    verify(paymentGateway, times(1)).charge(eq("user1"), eq(100.00))
    // => Tests order processing without calling real payment gateway
  }
}

// Kotlin-specific: Use backticks to escape 'when' keyword when calling Mockito's when() method
// Alternative with MockK (Kotlin mocking library):
// every { paymentGateway.charge(any(), any()) } returns true
// verify(exactly = 1) { paymentGateway.charge("user1", 100.00) }
```

**Key Takeaway**: `@SpringBootTest` loads full application context—use `@MockBean` to replace real dependencies with mocks.

**Why It Matters**: Integration tests verify controller-to-database flows including JSON serialization, exception handling, and transaction management—catching bugs that unit tests miss because mocks don't behave like real implementations. However, @SpringBootTest loads the full context (2-5 seconds per test), making large test suites slow (20 minutes for 500 tests), requiring careful test design where unit tests cover 80% of logic with @WebMvcTest, reserving integration tests for critical paths that justify the performance cost.

---

### Example 31: @WebMvcTest

Test controllers in isolation without full context.

```java
@WebMvcTest(ProductController.class) // Only load ProductController
public class ProductControllerUnitTest {
  @Autowired private MockMvc mockMvc; // Simulates HTTP requests

  @MockBean // Mock the service layer
  private ProductService productService;

  @Test
  void testGetProduct() throws Exception {
    // Arrange
    Product product = new Product(1L, "Laptop", 999.99);
    when(productService.findById(1L)).thenReturn(Optional.of(product));

    // Act & Assert
    mockMvc.perform(get("/api/products/1"))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.name").value("Laptop"))
      .andExpect(jsonPath("$.price").value(999.99));
    // => Tests controller logic without starting full app
  }

  @Test
  void testCreateProduct() throws Exception {
    Product product = new Product("Mouse", 25.00);
    when(productService.save(any(Product.class))).thenReturn(product);

    mockMvc.perform(post("/api/products")
        .contentType(MediaType.APPLICATION_JSON)
        .content("{\"name\":\"Mouse\",\"price\":25.00}"))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.name").value("Mouse"));
  }

  @Test
  void testGetProductNotFound() throws Exception {
    when(productService.findById(999L)).thenReturn(Optional.empty());

    mockMvc.perform(get("/api/products/999"))
      .andExpect(status().isNotFound());
    // => Tests error handling
  }
}
```

**Code (Kotlin)**:

```kotlin
@WebMvcTest(ProductController::class)  // Only load ProductController
class ProductControllerUnitTest {
  @Autowired private lateinit var mockMvc: MockMvc  // Simulates HTTP requests

  @MockBean  // Mock the service layer
  private lateinit var productService: ProductService

  @Test
  fun testGetProduct() {
    // Arrange
    val product = Product(1L, "Laptop", 999.99)
    `when`(productService.findById(1L)).thenReturn(Optional.of(product))

    // Act & Assert
    mockMvc.perform(get("/api/products/1"))
      .andExpect(status().isOk)
      .andExpect(jsonPath("$.name").value("Laptop"))
      .andExpect(jsonPath("$.price").value(999.99))
    // => Tests controller logic without starting full app
  }

  @Test
  fun testCreateProduct() {
    val product = Product("Mouse", 25.00)
    `when`(productService.save(any(Product::class.java))).thenReturn(product)

    mockMvc.perform(post("/api/products")
        .contentType(MediaType.APPLICATION_JSON)
        .content("""{"name":"Mouse","price":25.00}"""))  // Triple-quoted string
      .andExpect(status().isCreated)
      .andExpect(jsonPath("$.name").value("Mouse"))
  }

  @Test
  fun testGetProductNotFound() {
    `when`(productService.findById(999L)).thenReturn(Optional.empty())

    mockMvc.perform(get("/api/products/999"))
      .andExpect(status().isNotFound)
    // => Tests error handling
  }
}

// Kotlin-specific: Use triple-quoted strings for JSON payloads to avoid escaping
// Alternative idiomatic approach with companion object for test data:
// companion object {
//   private val SAMPLE_PRODUCT = Product(1L, "Laptop", 999.99)
// }
```

**Key Takeaway**: `@WebMvcTest` loads only web layer—faster than `@SpringBootTest`, ideal for controller logic testing.

**Why It Matters**: MockMvc tests verify REST API contracts (request mapping, response codes, JSON structure) 10x faster than integration tests because they don't start HTTP servers or load full contexts, enabling rapid TDD feedback loops. Production teams use MockMvc for controller logic and @JsonTest for serialization verification, achieving 95% branch coverage with 2-minute test suite execution that enables continuous deployment where every commit triggers automated tests before merging to main.

---

### Example 32: TestContainers

Test with real databases using Docker containers.

```java
// pom.xml
<dependency>
  <groupId>org.testcontainers</groupId>
  <artifactId>postgresql</artifactId>
  <version>1.19.3</version>
  <scope>test</scope>
</dependency>

// Test class
@SpringBootTest
@Testcontainers // Enable TestContainers support
public class ProductRepositoryTestContainersTest {
  @Container // Start PostgreSQL container
  static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16")
    .withDatabaseName("testdb")
    .withUsername("test")
    .withPassword("test");

  @DynamicPropertySource // Configure Spring to use container
  static void properties(DynamicPropertyRegistry registry) {
    registry.add("spring.datasource.url", postgres::getJdbcUrl);
    registry.add("spring.datasource.username", postgres::getUsername);
    registry.add("spring.datasource.password", postgres::getPassword);
  }

  @Autowired private ProductRepository productRepo;

  @Test
  void testSaveAndFind() {
    Product product = new Product("Keyboard", 75.00);
    productRepo.save(product);

    Optional<Product> found = productRepo.findById(product.getId());
    assertTrue(found.isPresent());
    assertEquals("Keyboard", found.get().getName());
    // => Tests against real PostgreSQL database in Docker
  }

  @Test
  void testCustomQuery() {
    productRepo.save(new Product("Mouse", 20.00));
    productRepo.save(new Product("Keyboard", 75.00));
    productRepo.save(new Product("Monitor", 300.00));

    List<Product> expensive = productRepo.findByPriceGreaterThan(50.00);
    assertEquals(2, expensive.size());
    // => Verifies custom queries against real database
  }
}
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts
testImplementation("org.testcontainers:postgresql:1.19.3")

// Test class
@SpringBootTest
@Testcontainers  // Enable TestContainers support
class ProductRepositoryTestContainersTest {
  companion object {
    @Container  // Start PostgreSQL container
    @JvmStatic
    val postgres = PostgreSQLContainer<Nothing>("postgres:16").apply {
      withDatabaseName("testdb")
      withUsername("test")
      withPassword("test")
    }

    @DynamicPropertySource  // Configure Spring to use container
    @JvmStatic
    fun properties(registry: DynamicPropertyRegistry) {
      registry.add("spring.datasource.url", postgres::getJdbcUrl)
      registry.add("spring.datasource.username", postgres::getUsername)
      registry.add("spring.datasource.password", postgres::getPassword)
    }
  }

  @Autowired private lateinit var productRepo: ProductRepository

  @Test
  fun testSaveAndFind() {
    val product = Product("Keyboard", 75.00)
    productRepo.save(product)

    val found = productRepo.findById(product.id!!)
    assertTrue(found.isPresent)
    assertEquals("Keyboard", found.get().name)
    // => Tests against real PostgreSQL database in Docker
  }

  @Test
  fun testCustomQuery() {
    listOf(
      Product("Mouse", 20.00),
      Product("Keyboard", 75.00),
      Product("Monitor", 300.00)
    ).forEach { productRepo.save(it) }  // Idiomatic forEach with lambda

    val expensive = productRepo.findByPriceGreaterThan(50.00)
    assertEquals(2, expensive.size)
    // => Verifies custom queries against real database
  }
}

// Kotlin-specific: Use companion object for static container and @JvmStatic for Java interop
// Alternative with apply scope function for container configuration (as shown above)
// TestContainers automatically manages container lifecycle (starts before tests, stops after)
```

**Key Takeaway**: TestContainers provides real databases for tests—eliminates mocking discrepancies between H2 and production databases.

**Why It Matters**: TestRestTemplate tests verify the complete HTTP stack including security filters, exception handlers, and content negotiation—catching integration issues where MockMvc succeeds but real HTTP requests fail due to filter ordering or missing CORS headers. Production pipelines use TestRestTemplate for smoke tests that verify deployments serve traffic correctly before switching load balancer endpoints, preventing broken deployments where internal tests pass but external clients receive 500 errors.

---

### Example 33: Mocking with Mockito

Isolate units under test with mocks.

```java
@ExtendWith(MockitoExtension.class) // Enable Mockito
public class OrderServiceUnitTest {
  @Mock // Create mock
  private OrderRepository orderRepo;

  @Mock
  private PaymentService paymentService;

  @InjectMocks // Inject mocks into service
  private OrderService orderService;

  @Test
  void testProcessOrder() {
    // Arrange
    Order order = new Order("user1", 100.00);
    when(orderRepo.save(any(Order.class))).thenReturn(order);
    when(paymentService.charge(anyString(), anyDouble())).thenReturn(true);

    // Act
    Order result = orderService.processOrder(order);

    // Assert
    assertNotNull(result);
    verify(orderRepo, times(1)).save(order); // Verify method called once
    verify(paymentService, times(1)).charge("user1", 100.00);
  }

  @Test
  void testProcessOrderPaymentFailure() {
    Order order = new Order("user1", 100.00);
    when(paymentService.charge(anyString(), anyDouble())).thenReturn(false);

    assertThrows(PaymentException.class, () -> {
      orderService.processOrder(order);
    });

    verify(orderRepo, never()).save(any()); // Verify save never called
    // => Tests failure scenarios
  }

  @Test
  void testArgumentCaptor() {
    Order order = new Order("user1", 100.00);
    when(paymentService.charge(anyString(), anyDouble())).thenReturn(true);

    orderService.processOrder(order);

    ArgumentCaptor<Order> captor = ArgumentCaptor.forClass(Order.class);
    verify(orderRepo).save(captor.capture());

    Order captured = captor.getValue();
    assertEquals("PROCESSED", captured.getStatus());
    // => Captures arguments passed to mocked methods
  }
}
```

**Code (Kotlin)**:

```kotlin
@ExtendWith(MockitoExtension::class)  // Enable Mockito
class OrderServiceUnitTest {
  @Mock  // Create mock
  private lateinit var orderRepo: OrderRepository

  @Mock
  private lateinit var paymentService: PaymentService

  @InjectMocks  // Inject mocks into service
  private lateinit var orderService: OrderService

  @Test
  fun testProcessOrder() {
    // Arrange
    val order = Order("user1", 100.00)
    `when`(orderRepo.save(any(Order::class.java))).thenReturn(order)
    `when`(paymentService.charge(anyString(), anyDouble())).thenReturn(true)

    // Act
    val result = orderService.processOrder(order)

    // Assert
    assertNotNull(result)
    verify(orderRepo, times(1)).save(order)  // Verify method called once
    verify(paymentService, times(1)).charge("user1", 100.00)
  }

  @Test
  fun testProcessOrderPaymentFailure() {
    val order = Order("user1", 100.00)
    `when`(paymentService.charge(anyString(), anyDouble())).thenReturn(false)

    assertThrows<PaymentException> {
      orderService.processOrder(order)
    }

    verify(orderRepo, never()).save(any())  // Verify save never called
    // => Tests failure scenarios
  }

  @Test
  fun testArgumentCaptor() {
    val order = Order("user1", 100.00)
    `when`(paymentService.charge(anyString(), anyDouble())).thenReturn(true)

    orderService.processOrder(order)

    val captor = argumentCaptor<Order>()  // Kotlin extension function
    verify(orderRepo).save(captor.capture())

    val captured = captor.firstValue
    assertEquals("PROCESSED", captured.status)
    // => Captures arguments passed to mocked methods
  }
}

// Kotlin-specific: Use backticks for 'when' keyword, assertThrows<T> type parameter
// Alternative with MockK (Kotlin-native mocking library):
// every { orderRepo.save(any()) } returns order
// every { paymentService.charge(any(), any()) } returns true
// val slot = slot<Order>()
// verify { orderRepo.save(capture(slot)) }
// assertEquals("PROCESSED", slot.captured.status)
```

**Key Takeaway**: Mockito enables fast, isolated unit tests—use `when().thenReturn()` for stubbing, `verify()` for interaction verification.

**Why It Matters**: Mockito enables fast unit tests (milliseconds vs seconds for integration tests) by replacing slow dependencies (databases, external APIs, message queues) with in-memory mocks that return predetermined responses. However, over-mocking creates brittle tests that pass with green checkmarks but fail in production because mocks don't behave like real implementations—production teams limit mocking to external boundaries (APIs, databases) while testing internal logic with real objects to balance speed against accuracy.

---

## Group 4: Caching & Performance

### Example 34: Cache Abstraction

Transparent caching with Spring's cache abstraction.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-cache</artifactId>
</dependency>

// Enable caching
@SpringBootApplication
@EnableCaching // Enable cache annotations
public class Application {
  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }
}

// Service with caching
@Service
public class ProductService {
  @Autowired private ProductRepository productRepo;

  @Cacheable("products") // Cache results by method arguments
  public Product findById(Long id) {
    System.out.println("Fetching from database: " + id);
    return productRepo.findById(id).orElseThrow();
    // First call: prints "Fetching..." and queries database
    // Subsequent calls: returns cached value, no database query
  }

  @Cacheable(value = "products", key = "#name") // Custom cache key
  public List<Product> findByName(String name) {
    System.out.println("Querying database for: " + name);
    return productRepo.findByNameContaining(name);
  }

  @CachePut(value = "products", key = "#result.id") // Update cache after method
  public Product save(Product product) {
    return productRepo.save(product);
    // => Saves to database AND updates cache with new value
  }

  @CacheEvict(value = "products", key = "#id") // Remove from cache
  public void deleteById(Long id) {
    productRepo.deleteById(id);
    // => Deletes from database AND evicts from cache
  }

  @CacheEvict(value = "products", allEntries = true) // Clear entire cache
  public void clearCache() {
    System.out.println("Cache cleared");
  }

  @Caching(evict = {
    @CacheEvict(value = "products", key = "#product.id"),
    @CacheEvict(value = "categories", key = "#product.categoryId")
  })
  public Product update(Product product) {
    return productRepo.save(product);
    // => Evicts multiple cache entries
  }
}

// Usage
Product p1 = productService.findById(1L); // => Database query
Product p2 = productService.findById(1L); // => Cached (no query)
productService.deleteById(1L); // => Evicts cache
Product p3 = productService.findById(1L); // => Database query again
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts
implementation("org.springframework.boot:spring-boot-starter-cache")

// Enable caching
@SpringBootApplication
@EnableCaching  // Enable cache annotations
open class Application

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}

// Service with caching
@Service
open class ProductService(
  private val productRepo: ProductRepository
) {
  @Cacheable("products")  // Cache results by method arguments
  open fun findById(id: Long): Product {
    println("Fetching from database: $id")  // String template
    return productRepo.findById(id).orElseThrow()
    // First call: prints "Fetching..." and queries database
    // Subsequent calls: returns cached value, no database query
  }

  @Cacheable(value = ["products"], key = "#name")  // Custom cache key
  open fun findByName(name: String): List<Product> {
    println("Querying database for: $name")
    return productRepo.findByNameContaining(name)
  }

  @CachePut(value = ["products"], key = "#result.id")  // Update cache after method
  open fun save(product: Product): Product {
    return productRepo.save(product)
    // => Saves to database AND updates cache with new value
  }

  @CacheEvict(value = ["products"], key = "#id")  // Remove from cache
  open fun deleteById(id: Long) {
    productRepo.deleteById(id)
    // => Deletes from database AND evicts from cache
  }

  @CacheEvict(value = ["products"], allEntries = true)  // Clear entire cache
  open fun clearCache() {
    println("Cache cleared")
  }

  @Caching(evict = [
    CacheEvict(value = ["products"], key = "#product.id"),
    CacheEvict(value = ["categories"], key = "#product.categoryId")
  ])
  open fun update(product: Product): Product {
    return productRepo.save(product)
    // => Evicts multiple cache entries
  }
}

// Usage
val p1 = productService.findById(1L)  // => Database query
val p2 = productService.findById(1L)  // => Cached (no query)
productService.deleteById(1L)  // => Evicts cache
val p3 = productService.findById(1L)  // => Database query again

// Kotlin-specific: Methods must be open for proxy-based caching (Spring uses CGLIB)
// Alternative configuration with Kotlin All-Open plugin in build.gradle.kts:
// kotlin { allOpen { annotation("org.springframework.cache.annotation.Cacheable") } }
```

**Key Takeaway**: Spring's cache abstraction decouples caching from business logic—annotate methods to cache, update, or evict automatically.

**Why It Matters**: Spring's cache abstraction decouples caching from business logic—adding @Cacheable doesn't modify method behavior, enabling gradual cache adoption where teams can cache slow queries without refactoring service layers. Production systems use caching to reduce database load by 90% for read-heavy workloads (product catalogs, user profiles), with cache hit rates of 95%+ that keep API latency under 50ms even when databases struggle under write pressure during peak traffic.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart TD
    A[Client Request] --> B{Cache Check}
    B -->|Cache Hit| C[Return Cached Value]
    B -->|Cache Miss| D[Execute Method]
    D --> E[Query Database]
    E --> F[Store in Cache]
    F --> G[Return Result]

    H[Update Operation] --> I[@CachePut]
    I --> J[Update Database]
    J --> K[Update Cache]

    L[Delete Operation] --> M[@CacheEvict]
    M --> N[Delete from Database]
    N --> O[Remove from Cache]

    style A fill:#0173B2,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style E fill:#DE8F05,stroke:#000,color:#000
    style K fill:#CC78BC,stroke:#000,color:#000
    style O fill:#CA9161,stroke:#000,color:#000
```

---

### Example 35: Redis Integration

Use Redis as distributed cache backend.

```java
// pom.xml
<dependency>
  <groupId>org.springframework.boot</groupId>
  <artifactId>spring-boot-starter-data-redis</artifactId>
</dependency>

// application.yml
spring:
  data:
    redis:
      host: localhost
      port: 6379
  cache:
    type: redis
    redis:
      time-to-live: 600000 # 10 minutes in milliseconds

// Redis config
@Configuration
@EnableCaching
public class RedisCacheConfig {
  @Bean
  public CacheManager cacheManager(RedisConnectionFactory factory) {
    RedisCacheConfiguration config = RedisCacheConfiguration.defaultCacheConfig()
      .entryTtl(Duration.ofMinutes(10)) // Cache expiration
      .serializeKeysWith(
        RedisSerializationContext.SerializationPair.fromSerializer(new StringRedisSerializer())
      )
      .serializeValuesWith(
        RedisSerializationContext.SerializationPair.fromSerializer(new GenericJackson2JsonRedisSerializer())
      );

    return RedisCacheManager.builder(factory)
      .cacheDefaults(config)
      .build();
  }
}

// Service (same annotations as Example 34)
@Service
public class UserService {
  @Cacheable("users") // Now uses Redis instead of in-memory cache
  public User findById(Long id) {
    return userRepo.findById(id).orElseThrow();
  }
}

// Direct Redis operations (without cache abstraction)
@Service
public class SessionService {
  @Autowired private RedisTemplate<String, Object> redisTemplate;

  public void saveSession(String sessionId, UserSession session) {
    redisTemplate.opsForValue().set("session:" + sessionId, session, Duration.ofMinutes(30));
    // => Key: "session:abc123", Value: serialized UserSession, TTL: 30 minutes
  }

  public UserSession getSession(String sessionId) {
    return (UserSession) redisTemplate.opsForValue().get("session:" + sessionId);
    // => Returns null if expired or not found
  }

  public void deleteSession(String sessionId) {
    redisTemplate.delete("session:" + sessionId);
  }
}
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts
implementation("org.springframework.boot:spring-boot-starter-data-redis")

// application.yml
spring:
  data:
    redis:
      host: localhost
      port: 6379
  cache:
    type: redis
    redis:
      time-to-live: 600000  # 10 minutes in milliseconds

// Redis config
@Configuration
@EnableCaching
open class RedisCacheConfig {
  @Bean
  open fun cacheManager(factory: RedisConnectionFactory): CacheManager {
    val config = RedisCacheConfiguration.defaultCacheConfig()
      .entryTtl(Duration.ofMinutes(10))  // Cache expiration
      .serializeKeysWith(
        RedisSerializationContext.SerializationPair.fromSerializer(StringRedisSerializer())
      )
      .serializeValuesWith(
        RedisSerializationContext.SerializationPair.fromSerializer(GenericJackson2JsonRedisSerializer())
      )

    return RedisCacheManager.builder(factory)
      .cacheDefaults(config)
      .build()
  }
}

// Service (same annotations as Example 34)
@Service
open class UserService(
  private val userRepo: UserRepository
) {
  @Cacheable("users")  // Now uses Redis instead of in-memory cache
  open fun findById(id: Long): User {
    return userRepo.findById(id).orElseThrow()
  }
}

// Direct Redis operations (without cache abstraction)
@Service
class SessionService(
  private val redisTemplate: RedisTemplate<String, Any>
) {
  fun saveSession(sessionId: String, session: UserSession) {
    redisTemplate.opsForValue().set("session:$sessionId", session, Duration.ofMinutes(30))
    // => Key: "session:abc123", Value: serialized UserSession, TTL: 30 minutes
  }

  fun getSession(sessionId: String): UserSession? {
    return redisTemplate.opsForValue().get("session:$sessionId") as? UserSession
    // => Returns null if expired or not found
  }

  fun deleteSession(sessionId: String) {
    redisTemplate.delete("session:$sessionId")
  }
}

// Kotlin-specific: Use string templates for Redis keys, safe cast (as?) for type conversion
// Alternative with inline reified functions for type-safe operations:
// inline fun <reified T> RedisTemplate<String, Any>.getTyped(key: String): T? =
//   opsForValue().get(key) as? T
```

**Key Takeaway**: Redis provides distributed caching across multiple application instances—configure TTL for automatic expiration.

**Why It Matters**: Redis distributed caching enables horizontal scaling where all application instances share cache entries, unlike in-memory caches where each instance maintains separate caches causing inconsistent reads. Production systems at Twitter and Instagram use Redis to cache session data across 100+ application instances, achieving sub-millisecond cache response times at 100,000+ requests/second, with Redis persistence options (RDB snapshots, AOF logs) preventing cache warm-up delays after crashes that would overwhelm databases with cold cache load.

---

### Example 36: Cache Strategies

Common caching patterns and pitfalls.

```java
@Service
public class CacheStrategyService {
  @Autowired private ProductRepository productRepo;
  @Autowired private CacheManager cacheManager;

  // Cache-Aside (Lazy Loading)
  @Cacheable("products")
  public Product findById(Long id) {
    return productRepo.findById(id).orElseThrow();
    // => Load on demand, cache misses query database
  }

  // Write-Through (Eager Update)
  @CachePut(value = "products", key = "#result.id")
  public Product save(Product product) {
    return productRepo.save(product);
    // => Writes to database AND cache simultaneously
  }

  // Cache Stampede Prevention
  @Cacheable(value = "expensive-data", sync = true) // Synchronize cache loading
  public ExpensiveData loadExpensiveData(String key) {
    // Only one thread loads data, others wait for cached result
    return computeExpensiveData(key);
  }

  // Conditional Caching
  @Cacheable(value = "products", condition = "#id > 100") // Only cache if id > 100
  public Product findByIdConditional(Long id) {
    return productRepo.findById(id).orElseThrow();
  }

  @Cacheable(value = "products", unless = "#result.price < 10") // Don't cache cheap products
  public Product findByIdUnless(Long id) {
    return productRepo.findById(id).orElseThrow();
  }

  // Manual Cache Control
  public void warmUpCache() {
    List<Product> topProducts = productRepo.findTop100ByOrderBySalesDesc();
    Cache cache = cacheManager.getCache("products");
    topProducts.forEach(p -> cache.put(p.getId(), p));
    // => Pre-populate cache with frequently accessed data
  }

  // Cache Invalidation Pattern
  @Scheduled(cron = "0 0 3 * * ?") // Every day at 3 AM
  public void scheduledCacheEviction() {
    cacheManager.getCacheNames().forEach(name -> {
      cacheManager.getCache(name).clear();
    });
    System.out.println("All caches cleared");
  }
}

// Common Pitfalls
@Service
public class CachePitfallsService {
  // ❌ Wrong: Self-invocation bypasses proxy
  public Product getProduct(Long id) {
    return this.findById(id); // Cache annotation IGNORED (no proxy)
  }

  @Cacheable("products")
  public Product findById(Long id) {
    return productRepo.findById(id).orElseThrow();
  }

  // ✅ Correct: Inject self-reference
  @Autowired private CachePitfallsService self;

  public Product getProductCorrect(Long id) {
    return self.findById(id); // Cache annotation WORKS (via proxy)
  }
}
```

**Code (Kotlin)**:

```kotlin
@Service
open class CacheStrategyService(
  private val productRepo: ProductRepository,
  private val cacheManager: CacheManager
) {
  // Cache-Aside (Lazy Loading)
  @Cacheable("products")
  open fun findById(id: Long): Product {
    return productRepo.findById(id).orElseThrow()
    // => Load on demand, cache misses query database
  }

  // Write-Through (Eager Update)
  @CachePut(value = ["products"], key = "#result.id")
  open fun save(product: Product): Product {
    return productRepo.save(product)
    // => Writes to database AND cache simultaneously
  }

  // Cache Stampede Prevention
  @Cacheable(value = ["expensive-data"], sync = true)  // Synchronize cache loading
  open fun loadExpensiveData(key: String): ExpensiveData {
    // Only one thread loads data, others wait for cached result
    return computeExpensiveData(key)
  }

  // Conditional Caching
  @Cacheable(value = ["products"], condition = "#id > 100")  // Only cache if id > 100
  open fun findByIdConditional(id: Long): Product {
    return productRepo.findById(id).orElseThrow()
  }

  @Cacheable(value = ["products"], unless = "#result.price < 10")  // Don't cache cheap products
  open fun findByIdUnless(id: Long): Product {
    return productRepo.findById(id).orElseThrow()
  }

  // Manual Cache Control
  fun warmUpCache() {
    val topProducts = productRepo.findTop100ByOrderBySalesDesc()
    val cache = cacheManager.getCache("products")!!
    topProducts.forEach { cache.put(it.id, it) }
    // => Pre-populate cache with frequently accessed data
  }

  // Cache Invalidation Pattern
  @Scheduled(cron = "0 0 3 * * ?")  // Every day at 3 AM
  fun scheduledCacheEviction() {
    cacheManager.cacheNames.forEach { name ->
      cacheManager.getCache(name)?.clear()
    }
    println("All caches cleared")
  }
}

// Common Pitfalls
@Service
open class CachePitfallsService(
  private val productRepo: ProductRepository
) {
  // ❌ Wrong: Self-invocation bypasses proxy
  fun getProduct(id: Long): Product {
    return this.findById(id)  // Cache annotation IGNORED (no proxy)
  }

  @Cacheable("products")
  open fun findById(id: Long): Product {
    return productRepo.findById(id).orElseThrow()
  }

  // ✅ Correct: Inject self-reference
  @Autowired private lateinit var self: CachePitfallsService

  fun getProductCorrect(id: Long): Product {
    return self.findById(id)  // Cache annotation WORKS (via proxy)
  }
}

// Kotlin-specific: Methods must be open for Spring proxy, use lateinit for self-injection
// Alternative with lazy delegate:
// private val self: CachePitfallsService by lazy {
//   applicationContext.getBean(CachePitfallsService::class.java)
// }
```

**Key Takeaway**: Choose caching strategies based on consistency needs—cache-aside for reads, write-through for updates, sync for stampede prevention.

**Why It Matters**: Cache-aside pattern with sync=true prevents cache stampedes where 1000 concurrent requests for an expired cache entry trigger 1000 identical database queries, causing database CPU spikes that cascade into timeouts. Production caching strategies use short TTLs (minutes) for frequently changing data versus long TTLs (hours/days) for static data, with cache warming during deployment preventing cold cache performance degradation where the first user request experiences 5-second latency while loading cache.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A[1000 Concurrent Requests] --> B{Cache Entry Expired?}

    B -->|Without sync=true| C[1000 Threads Query DB]
    C --> D[Database Overload]
    D --> E[Timeouts & Errors]

    B -->|With sync=true| F[First Thread Locks]
    F --> G[Thread 1: Query DB]
    F --> H[Threads 2-1000: Wait]
    G --> I[Load Result]
    I --> J[Store in Cache]
    J --> K[Release Lock]
    K --> L[All Threads Get Cached Value]
    L --> M[No Database Overload]

    style A fill:#0173B2,stroke:#000,color:#fff
    style C fill:#CC78BC,stroke:#000,color:#000
    style E fill:#CA9161,stroke:#000,color:#000
    style F fill:#DE8F05,stroke:#000,color:#000
    style J fill:#029E73,stroke:#000,color:#fff
    style M fill:#029E73,stroke:#000,color:#fff
```

---

## Group 5: Async & Events

### Example 37: @Async Methods

Execute methods asynchronously with thread pools.

```java
// Enable async support
@SpringBootApplication
@EnableAsync // Enable @Async annotation
public class Application {
  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }
}

// Async service
@Service
public class EmailService {
  @Async // Runs in separate thread
  public void sendEmail(String to, String subject, String body) {
    System.out.println("Sending email to " + to + " - Thread: " + Thread.currentThread().getName());
    // Simulate delay
    try { Thread.sleep(3000); } catch (InterruptedException e) {}
    System.out.println("Email sent to " + to);
  }

  @Async
  public CompletableFuture<String> sendEmailWithResult(String to) {
    System.out.println("Sending email - Thread: " + Thread.currentThread().getName());
    try { Thread.sleep(2000); } catch (InterruptedException e) {}
    return CompletableFuture.completedFuture("Email sent to " + to);
    // => Returns CompletableFuture for async result handling
  }

  @Async
  public CompletableFuture<Integer> processLargeFile(String filename) {
    System.out.println("Processing " + filename);
    try { Thread.sleep(5000); } catch (InterruptedException e) {}
    return CompletableFuture.completedFuture(10000); // Processed 10000 records
  }
}

// Controller
@RestController
@RequestMapping("/api/async")
public class AsyncController {
  @Autowired private EmailService emailService;

  @PostMapping("/send-email")
  public ResponseEntity<String> sendEmail(@RequestParam String to) {
    emailService.sendEmail(to, "Welcome", "Hello!");
    return ResponseEntity.ok("Email queued"); // Returns immediately
    // => Controller thread doesn't wait for email to send
  }

  @GetMapping("/send-with-result")
  public CompletableFuture<String> sendWithResult(@RequestParam String to) {
    return emailService.sendEmailWithResult(to)
      .thenApply(result -> "Result: " + result);
    // => Non-blocking—returns CompletableFuture
  }

  @GetMapping("/process-multiple")
  public CompletableFuture<String> processMultiple() {
    CompletableFuture<Integer> file1 = emailService.processLargeFile("file1.csv");
    CompletableFuture<Integer> file2 = emailService.processLargeFile("file2.csv");
    CompletableFuture<Integer> file3 = emailService.processLargeFile("file3.csv");

    return CompletableFuture.allOf(file1, file2, file3)
      .thenApply(v -> {
        int total = file1.join() + file2.join() + file3.join();
        return "Total records processed: " + total;
      });
    // => Processes 3 files in parallel, waits for all to complete
  }
}
```

**Code (Kotlin)**:

```kotlin
// Enable async support
@SpringBootApplication
@EnableAsync  // Enable @Async annotation
open class Application

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}

// Async service
@Service
open class EmailService {
  @Async  // Runs in separate thread
  open fun sendEmail(to: String, subject: String, body: String) {
    println("Sending email to $to - Thread: ${Thread.currentThread().name}")
    // Simulate delay
    Thread.sleep(3000)
    println("Email sent to $to")
  }

  @Async
  open fun sendEmailWithResult(to: String): CompletableFuture<String> {
    println("Sending email - Thread: ${Thread.currentThread().name}")
    Thread.sleep(2000)
    return CompletableFuture.completedFuture("Email sent to $to")
    // => Returns CompletableFuture for async result handling
  }

  @Async
  open fun processLargeFile(filename: String): CompletableFuture<Int> {
    println("Processing $filename")
    Thread.sleep(5000)
    return CompletableFuture.completedFuture(10000)  // Processed 10000 records
  }
}

// Controller
@RestController
@RequestMapping("/api/async")
class AsyncController(
  private val emailService: EmailService
) {
  @PostMapping("/send-email")
  fun sendEmail(@RequestParam to: String): ResponseEntity<String> {
    emailService.sendEmail(to, "Welcome", "Hello!")
    return ResponseEntity.ok("Email queued")  // Returns immediately
    // => Controller thread doesn't wait for email to send
  }

  @GetMapping("/send-with-result")
  fun sendWithResult(@RequestParam to: String): CompletableFuture<String> {
    return emailService.sendEmailWithResult(to)
      .thenApply { result -> "Result: $result" }
    // => Non-blocking—returns CompletableFuture
  }

  @GetMapping("/process-multiple")
  fun processMultiple(): CompletableFuture<String> {
    val file1 = emailService.processLargeFile("file1.csv")
    val file2 = emailService.processLargeFile("file2.csv")
    val file3 = emailService.processLargeFile("file3.csv")

    return CompletableFuture.allOf(file1, file2, file3)
      .thenApply {
        val total = file1.join() + file2.join() + file3.join()
        "Total records processed: $total"
      }
    // => Processes 3 files in parallel, waits for all to complete
  }
}

// Kotlin-specific: Methods must be open for @Async proxy
// Alternative with Kotlin Coroutines (more idiomatic):
// suspend fun sendEmail(to: String) = withContext(Dispatchers.IO) { ... }
// suspend fun processMultiple() = coroutineScope {
//   val d1 = async { processLargeFile("file1.csv") }
//   val d2 = async { processLargeFile("file2.csv") }
//   val d3 = async { processLargeFile("file3.csv") }
//   "Total: ${d1.await() + d2.await() + d3.await()}"
// }
```

**Key Takeaway**: `@Async` offloads work to background threads—use `CompletableFuture` return types for composable async operations.

**Why It Matters**: Async methods prevent slow operations (email sending, PDF generation) from blocking HTTP request threads, enabling APIs to respond in 50ms while offloading 5-second background work to thread pools. However, @Async without custom thread pools shares the default pool with all async operations, causing thread starvation where one slow operation delays all others—production systems configure separate thread pools (email-pool, report-pool) sized to each operation's concurrency needs.

---

### Example 38: Task Executors

Configure thread pools for async execution.

```java
@Configuration
@EnableAsync
public class AsyncConfig implements AsyncConfigurer {

  @Override
  public Executor getAsyncExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(5); // Minimum threads
    executor.setMaxPoolSize(10); // Maximum threads
    executor.setQueueCapacity(100); // Queue size before rejecting tasks
    executor.setThreadNamePrefix("async-"); // Thread naming
    executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    executor.initialize();
    return executor;
  }

  // Custom executor for specific tasks
  @Bean(name = "emailExecutor")
  public Executor emailExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(2);
    executor.setMaxPoolSize(5);
    executor.setQueueCapacity(50);
    executor.setThreadNamePrefix("email-");
    executor.initialize();
    return executor;
  }

  @Bean(name = "reportExecutor")
  public Executor reportExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(1);
    executor.setMaxPoolSize(3);
    executor.setQueueCapacity(20);
    executor.setThreadNamePrefix("report-");
    executor.initialize();
    return executor;
  }
}

// Service with custom executors
@Service
public class NotificationService {
  @Async("emailExecutor") // Use specific executor
  public void sendEmailNotification(String to) {
    System.out.println("Email thread: " + Thread.currentThread().getName());
    // => Thread name: email-1, email-2, etc.
  }

  @Async("reportExecutor")
  public CompletableFuture<Report> generateReport(Long id) {
    System.out.println("Report thread: " + Thread.currentThread().getName());
    // => Thread name: report-1, report-2, etc.
    return CompletableFuture.completedFuture(new Report(id));
  }
}
```

**Code (Kotlin)**:

```kotlin
@Configuration
@EnableAsync
open class AsyncConfig : AsyncConfigurer {
  override fun getAsyncExecutor(): Executor {
    val executor = ThreadPoolTaskExecutor()
    executor.corePoolSize = 5  // Minimum threads
    executor.maxPoolSize = 10  // Maximum threads
    executor.queueCapacity = 100  // Queue size before rejecting tasks
    executor.setThreadNamePrefix("async-")  // Thread naming
    executor.setRejectedExecutionHandler(ThreadPoolExecutor.CallerRunsPolicy())
    executor.initialize()
    return executor
  }

  // Custom executor for specific tasks
  @Bean(name = ["emailExecutor"])
  open fun emailExecutor(): Executor {
    return ThreadPoolTaskExecutor().apply {
      corePoolSize = 2
      maxPoolSize = 5
      queueCapacity = 50
      setThreadNamePrefix("email-")
      initialize()
    }
  }

  @Bean(name = ["reportExecutor"])
  open fun reportExecutor(): Executor {
    return ThreadPoolTaskExecutor().apply {
      corePoolSize = 1
      maxPoolSize = 3
      queueCapacity = 20
      setThreadNamePrefix("report-")
      initialize()
    }
  }
}

// Service with custom executors
@Service
open class NotificationService {
  @Async("emailExecutor")  // Use specific executor
  open fun sendEmailNotification(to: String) {
    println("Email thread: ${Thread.currentThread().name}")
    // => Thread name: email-1, email-2, etc.
  }

  @Async("reportExecutor")
  open fun generateReport(id: Long): CompletableFuture<Report> {
    println("Report thread: ${Thread.currentThread().name}")
    // => Thread name: report-1, report-2, etc.
    return CompletableFuture.completedFuture(Report(id))
  }
}

// Kotlin-specific: Use apply scope function for builder-style configuration
// Alternative with property syntax:
// ThreadPoolTaskExecutor().apply {
//   this.corePoolSize = 5
//   this.maxPoolSize = 10
// }
// Note: Coroutines provide more idiomatic async model:
// @OptIn(ExperimentalCoroutinesApi::class)
// val emailDispatcher = Dispatchers.IO.limitedParallelism(5)
```

**Key Takeaway**: Configure `ThreadPoolTaskExecutor` for fine-grained control—separate executors isolate thread pools for different task types.

**Why It Matters**: Custom thread pools isolate failure domains—if report generation consumes all threads, email sending continues using its dedicated pool instead of queueing behind slow operations. Production configurations tune core pool size (CPU-bound: core count, I/O-bound: core count \* 2-4) and queue capacity (buffer for traffic spikes without rejection) based on monitoring data, preventing OutOfMemoryErrors from unbounded queues that accumulate tasks faster than threads can process them.

---

### Example 39: Application Events

Decouple components with Spring's event publishing mechanism.

```java
// Custom event
public class OrderPlacedEvent extends ApplicationEvent {
  private final Order order;

  public OrderPlacedEvent(Object source, Order order) {
    super(source);
    this.order = order;
  }

  public Order getOrder() {
    return order;
  }
}

// Event publisher
@Service
public class OrderService {
  @Autowired private ApplicationEventPublisher eventPublisher;
  @Autowired private OrderRepository orderRepo;

  public Order placeOrder(Order order) {
    order.setStatus("PLACED");
    Order saved = orderRepo.save(order);

    // Publish event
    eventPublisher.publishEvent(new OrderPlacedEvent(this, saved));
    // => Listeners are notified asynchronously

    return saved;
  }
}

// Event listeners
@Component
public class EmailNotificationListener {
  @EventListener // Subscribe to event
  public void handleOrderPlaced(OrderPlacedEvent event) {
    Order order = event.getOrder();
    System.out.println("Sending confirmation email for order " + order.getId());
    // => Executes synchronously by default
  }
}

@Component
public class InventoryListener {
  @EventListener
  @Async // Make listener async
  public void handleOrderPlaced(OrderPlacedEvent event) {
    System.out.println("Updating inventory - Thread: " + Thread.currentThread().getName());
    // => Executes in background thread
  }
}

@Component
public class AnalyticsListener {
  @EventListener
  @Async
  public void handleOrderPlaced(OrderPlacedEvent event) {
    System.out.println("Recording analytics for order " + event.getOrder().getId());
  }
}

// Conditional listeners
@Component
public class LargeOrderListener {
  @EventListener(condition = "#event.order.total > 1000") // Only for large orders
  public void handleLargeOrder(OrderPlacedEvent event) {
    System.out.println("Large order detected: " + event.getOrder().getTotal());
  }
}

// Transaction-aware events
@Component
public class TransactionalListener {
  @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT) // After transaction commits
  public void handleOrderPlaced(OrderPlacedEvent event) {
    System.out.println("Order committed, safe to send external API call");
    // => Only fires if transaction succeeds
  }
}
```

**Code (Kotlin)**:

```kotlin
// Custom event (data class instead of extending ApplicationEvent)
data class OrderPlacedEvent(
  val order: Order,
  val source: Any = Object()
)

// Event publisher
@Service
class OrderService(
  private val eventPublisher: ApplicationEventPublisher,
  private val orderRepo: OrderRepository
) {
  fun placeOrder(order: Order): Order {
    order.status = "PLACED"
    val saved = orderRepo.save(order)

    // Publish event
    eventPublisher.publishEvent(OrderPlacedEvent(saved, this))
    // => Listeners are notified asynchronously

    return saved
  }
}

// Event listeners
@Component
class EmailNotificationListener {
  @EventListener  // Subscribe to event
  fun handleOrderPlaced(event: OrderPlacedEvent) {
    val order = event.order
    println("Sending confirmation email for order ${order.id}")
    // => Executes synchronously by default
  }
}

@Component
class InventoryListener {
  @Async  // Process asynchronously
  @EventListener
  fun handleOrderPlaced(event: OrderPlacedEvent) {
    println("Reducing inventory for order ${event.order.id}")
    // => Runs in separate thread
  }
}

// Conditional listeners
@Component
class LargeOrderListener {
  @EventListener(condition = "#event.order.total > 1000")  // Only for large orders
  fun handleLargeOrder(event: OrderPlacedEvent) {
    println("Large order detected: ${event.order.total}")
  }
}

// Transaction-aware events
@Component
class TransactionalListener {
  @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)  // After transaction commits
  fun handleOrderPlaced(event: OrderPlacedEvent) {
    println("Order committed, safe to send external API call")
    // => Only fires if transaction succeeds
  }
}

// Kotlin-specific: Use data class for events (cleaner than extending ApplicationEvent)
// Alternative with sealed classes for event hierarchies:
// sealed class OrderEvent
// data class OrderPlacedEvent(val order: Order) : OrderEvent()
// data class OrderCancelledEvent(val order: Order) : OrderEvent()
// @EventListener fun handle(event: OrderEvent) = when(event) {
//   is OrderPlacedEvent -> handlePlaced(event)
//   is OrderCancelledEvent -> handleCancelled(event)
// }
```

**Key Takeaway**: Events decouple publishers from listeners—use `@Async` for parallel processing, `@TransactionalEventListener` for transaction safety.

**Why It Matters**: Application events decouple components through publish-subscribe messaging—order placement triggers inventory reduction, email sending, and analytics recording without OrderService knowing about these dependencies, enabling feature toggles where new event listeners activate without modifying existing code. Production event-driven architectures support eventual consistency where events propagate asynchronously, accepting temporary inconsistency (order placed, inventory not yet reduced) for higher throughput than synchronous request/response chains that wait for all operations to complete.

---

### Example 40: Scheduling

Execute tasks on fixed schedules with `@Scheduled`.

```java
// Enable scheduling
@SpringBootApplication
@EnableScheduling // Enable @Scheduled annotation
public class Application {
  public static void main(String[] args) {
    SpringApplication.run(Application.class, args);
  }
}

// Scheduled tasks
@Component
public class ScheduledTasks {

  // Fixed rate: Executes every 5 seconds (from start of previous execution)
  @Scheduled(fixedRate = 5000)
  public void reportCurrentTime() {
    System.out.println("Current time: " + LocalDateTime.now());
    // If task takes 2 seconds, next execution starts at: 0s, 5s, 10s, 15s...
  }

  // Fixed delay: Waits 5 seconds AFTER previous execution finishes
  @Scheduled(fixedDelay = 5000)
  public void processQueueWithDelay() {
    System.out.println("Processing queue at " + LocalDateTime.now());
    try { Thread.sleep(3000); } catch (InterruptedException e) {}
    // Next execution starts 5 seconds after this finishes
    // Execution pattern: 0s, 8s, 16s, 24s... (3s task + 5s delay)
  }

  // Initial delay: Wait 10 seconds before first execution
  @Scheduled(initialDelay = 10000, fixedRate = 5000)
  public void delayedStart() {
    System.out.println("Delayed task executed");
    // First execution at 10s, then every 5s: 10s, 15s, 20s...
  }

  // Cron expression: Every day at 2:00 AM
  @Scheduled(cron = "0 0 2 * * ?")
  public void dailyBackup() {
    System.out.println("Running daily backup at " + LocalDateTime.now());
  }

  // Cron: Every weekday at 9:00 AM
  @Scheduled(cron = "0 0 9 ? * MON-FRI")
  public void weekdayReport() {
    System.out.println("Weekday report generated");
  }

  // Cron: Every 15 minutes
  @Scheduled(cron = "0 */15 * * * ?")
  public void quarterHourlyCheck() {
    System.out.println("15-minute check at " + LocalDateTime.now());
    // Runs at: 00:00, 00:15, 00:30, 00:45, 01:00, 01:15...
  }

  // Cron from configuration
  @Scheduled(cron = "${app.cleanup.schedule}") // Read from application.properties
  public void configurableSchedule() {
    System.out.println("Cleanup task running");
  }
}

// Cron format: second minute hour day-of-month month day-of-week
// Examples:
// "0 0 * * * ?" - Every hour
// "0 30 9 * * ?" - 9:30 AM daily
// "0 0 12 1 * ?" - 12:00 PM on 1st day of month
```

**Code (Kotlin)**:

```kotlin
// Enable scheduling
@SpringBootApplication
@EnableScheduling  // Enable @Scheduled annotation
open class Application

fun main(args: Array<String>) {
  runApplication<Application>(*args)
}

// Scheduled tasks
@Component
class ScheduledTasks {
  // Fixed rate: Executes every 5 seconds (from start of previous execution)
  @Scheduled(fixedRate = 5000)
  fun reportCurrentTime() {
    println("Current time: ${LocalDateTime.now()}")
    // If task takes 2 seconds, next execution starts at: 0s, 5s, 10s, 15s...
  }

  // Fixed delay: Waits 5 seconds AFTER previous execution finishes
  @Scheduled(fixedDelay = 5000)
  fun processQueueWithDelay() {
    println("Processing queue at ${LocalDateTime.now()}")
    Thread.sleep(3000)
    // Next execution starts 5 seconds after this finishes
    // Execution pattern: 0s, 8s, 16s, 24s... (3s task + 5s delay)
  }

  // Initial delay: Wait 10 seconds before first execution
  @Scheduled(initialDelay = 10000, fixedRate = 5000)
  fun delayedStart() {
    println("Delayed task executed")
    // First execution at 10s, then every 5s: 10s, 15s, 20s...
  }

  // Cron expression: Every day at 2:00 AM
  @Scheduled(cron = "0 0 2 * * ?")
  fun dailyBackup() {
    println("Running daily backup at ${LocalDateTime.now()}")
  }

  // Cron: Every weekday at 9:00 AM
  @Scheduled(cron = "0 0 9 ? * MON-FRI")
  fun weekdayReport() {
    println("Weekday report generated")
  }

  // Cron: Every 15 minutes
  @Scheduled(cron = "0 */15 * * * ?")
  fun quarterHourlyCheck() {
    println("15-minute check at ${LocalDateTime.now()}")
    // Runs at: 00:00, 00:15, 00:30, 00:45, 01:00, 01:15...
  }

  // Cron from configuration
  @Scheduled(cron = "\${app.cleanup.schedule}")  // Read from application.properties
  fun configurableSchedule() {
    println("Cleanup task running")
  }
}

// Cron format: second minute hour day-of-month month day-of-week
// Examples:
// "0 0 * * * ?" - Every hour
// "0 30 9 * * ?" - 9:30 AM daily
// "0 0 12 1 * ?" - 12:00 PM on 1st day of month

// Kotlin-specific: Use string templates in println, escape $ in property placeholder with \$
// Alternative with Kotlin Coroutines for cancellable scheduled tasks:
// @OptIn(DelicateCoroutinesApi::class)
// GlobalScope.launch {
//   while(isActive) {
//     println("Coroutine task: ${LocalDateTime.now()}")
//     delay(5000)  // Non-blocking delay
//   }
// }
```

**Key Takeaway**: Use `fixedRate` for periodic tasks, `fixedDelay` to prevent overlap, `cron` for specific times—Spring handles scheduling automatically.

**Why It Matters**: Scheduled tasks automate maintenance operations (cleanup, backups, report generation) that would require manual intervention, reducing operational overhead and human error. However, @Scheduled without distributed locking causes duplicate execution in multi-instance deployments where all instances run the same scheduled method—production systems use ShedLock to ensure only one instance executes each task, preventing duplicate payments or duplicate email sends that frustrate customers and waste resources.

---

### Example 31: WebSocket - Real-Time Communication

WebSocket enables bidirectional, real-time communication between server and clients for chat, notifications, and live updates.

```java
// pom.xml: spring-boot-starter-websocket

@Configuration
@EnableWebSocketMessageBroker
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer {
    @Override
    public void configureMessageBroker(MessageBrokerRegistry config) {
        config.enableSimpleBroker("/topic", "/queue"); // => Broadcast and P2P channels
        config.setApplicationDestinationPrefixes("/app");
    }

    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws")
            .setAllowedOrigins("http://localhost:3000")
            .withSockJS(); // => Fallback for browsers without WebSocket
    }
}

@Controller
public class ChatController {
    @MessageMapping("/chat.send") // Client sends to /app/chat.send
    @SendTo("/topic/messages") // Broadcast to all subscribers of /topic/messages
    public ChatMessage sendMessage(ChatMessage message) {
        message.setTimestamp(LocalDateTime.now());
        return message; // => Broadcasts to all connected clients
    }

    @MessageMapping("/chat.private")
    @SendToUser("/queue/private") // Send to specific user's queue
    public ChatMessage sendPrivateMessage(ChatMessage message, Principal principal) {
        message.setRecipient(principal.getName());
        return message; // => Only recipient receives this
    }
}

record ChatMessage(String sender, String content, String recipient, LocalDateTime timestamp) {}
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts: implementation("org.springframework.boot:spring-boot-starter-websocket")

@Configuration
@EnableWebSocketMessageBroker
open class WebSocketConfig : WebSocketMessageBrokerConfigurer {
  override fun configureMessageBroker(config: MessageBrokerRegistry) {
    config.enableSimpleBroker("/topic", "/queue")  // => Broadcast and P2P channels
    config.setApplicationDestinationPrefixes("/app")
  }

  override fun registerStompEndpoints(registry: StompEndpointRegistry) {
    registry.addEndpoint("/ws")
      .setAllowedOrigins("http://localhost:3000")
      .withSockJS()  // => Fallback for browsers without WebSocket
  }
}

@Controller
class ChatController {
  @MessageMapping("/chat.send")  // Client sends to /app/chat.send
  @SendTo("/topic/messages")  // Broadcast to all subscribers of /topic/messages
  fun sendMessage(message: ChatMessage): ChatMessage {
    return message.copy(timestamp = LocalDateTime.now())
    // => Broadcasts to all connected clients
  }

  @MessageMapping("/chat.private")
  @SendToUser("/queue/private")  // Send to specific user's queue
  fun sendPrivateMessage(message: ChatMessage, principal: Principal): ChatMessage {
    return message.copy(recipient = principal.name)
    // => Only recipient receives this
  }
}

data class ChatMessage(
  val sender: String,
  val content: String,
  val recipient: String? = null,
  val timestamp: LocalDateTime? = null
)

// Kotlin-specific: Use data class with default parameters instead of Java record
// Alternative with sealed class for message types:
// sealed class ChatMessage
// data class BroadcastMessage(val sender: String, val content: String, val timestamp: LocalDateTime) : ChatMessage()
// data class PrivateMessage(val sender: String, val recipient: String, val content: String) : ChatMessage()
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
sequenceDiagram
    participant C1 as Client 1
    participant S as Spring Server
    participant C2 as Client 2

    C1->>S: Connect /ws (WebSocket handshake)
    C2->>S: Connect /ws (WebSocket handshake)
    C1->>S: Subscribe /topic/messages
    C2->>S: Subscribe /topic/messages

    C1->>S: Send /app/chat.send {"sender":"Alice","content":"Hello"}
    S->>C1: Broadcast /topic/messages
    S->>C2: Broadcast /topic/messages

    Note over C1,C2: Both clients receive message in real-time

```

**Key Takeaway**: WebSocket enables real-time bidirectional communication—use `@MessageMapping` for client messages, `@SendTo` for broadcast to all subscribers, and `@SendToUser` for user-specific messages.

**Why It Matters**: Spring's declarative transaction management prevents data corruption from partial failures—without @Transactional, a bank transfer could debit one account but crash before crediting another, creating phantom money. Production systems at PayPal and Stripe rely on transaction boundaries to ensure ACID guarantees, automatically rolling back all database changes when exceptions occur, eliminating manual rollback code that developers forget 40% of the time in non-transactional systems, causing financial discrepancies.

---

### Example 32: Server-Sent Events (SSE) - Unidirectional Streaming

SSE streams server updates to clients over HTTP, simpler than WebSocket for one-way communication.

```java
@RestController
@RequestMapping("/api/sse")
public class SseController {
    @GetMapping(value = "/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<ServerSentEvent<String>> streamEvents() {
        return Flux.interval(Duration.ofSeconds(1))
            .map(seq -> ServerSentEvent.<String>builder()
                .id(String.valueOf(seq))
                .event("message")
                .data("Server time: " + LocalDateTime.now())
                .build()
            );
        // => Sends event every second: data: Server time: 2024-12-24T10:00:00
    }

    @GetMapping(value = "/stock-prices", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<ServerSentEvent<StockPrice>> streamStockPrices() {
        return Flux.interval(Duration.ofSeconds(2))
            .map(i -> ServerSentEvent.<StockPrice>builder()
                .data(new StockPrice("AAPL", 150.0 + Math.random() * 10))
                .build()
            );
        // => Streams stock updates every 2 seconds
    }
}

record StockPrice(String symbol, double price) {}
```

**Code (Kotlin)**:

```kotlin
@RestController
@RequestMapping("/api/sse")
class SseController {
  @GetMapping(value = ["/stream"], produces = [MediaType.TEXT_EVENT_STREAM_VALUE])
  fun streamEvents(): Flux<ServerSentEvent<String>> {
    return Flux.interval(Duration.ofSeconds(1))
      .map { seq ->
        ServerSentEvent.builder<String>()
          .id(seq.toString())
          .event("message")
          .data("Server time: ${LocalDateTime.now()}")
          .build()
      }
    // => Sends event every second: data: Server time: 2024-12-24T10:00:00
  }

  @GetMapping(value = ["/stock-prices"], produces = [MediaType.TEXT_EVENT_STREAM_VALUE])
  fun streamStockPrices(): Flux<ServerSentEvent<StockPrice>> {
    return Flux.interval(Duration.ofSeconds(2))
      .map {
        ServerSentEvent.builder<StockPrice>()
          .data(StockPrice("AAPL", 150.0 + Math.random() * 10))
          .build()
      }
    // => Streams stock updates every 2 seconds
  }
}

data class StockPrice(val symbol: String, val price: Double)

// Kotlin-specific: Use data class instead of record, string templates in data content
// Alternative with Kotlin Flow (more idiomatic than Reactor):
// @GetMapping("/stream-flow", produces = [MediaType.TEXT_EVENT_STREAM_VALUE])
// fun streamEventsFlow(): Flow<ServerSentEvent<String>> = flow {
//   var seq = 0L
//   while(true) {
//     emit(ServerSentEvent.builder<String>()
//       .id((seq++).toString())
//       .data("Server time: ${LocalDateTime.now()}")
//       .build())
//     delay(1000)
//   }
// }
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart LR
    Server[Spring Server] -->|SSE Stream| C1[Client 1]
    Server -->|SSE Stream| C2[Client 2]
    Server -->|SSE Stream| C3[Client 3]

    Note1[data: event 1] --> Server
    Note2[data: event 2] --> Server
    Note3[data: event 3] --> Server

    style Server fill:#0173B2,color:#fff
    style C1 fill:#029E73,color:#fff
    style C2 fill:#DE8F05,color:#000
    style C3 fill:#CC78BC,color:#000
```

**Key Takeaway**: SSE provides unidirectional server-to-client streaming over HTTP—simpler than WebSocket for use cases like live dashboards, notifications, and progress updates where bidirectional communication isn't needed.

**Why It Matters**: Isolation levels balance consistency against concurrency—SERIALIZABLE prevents all concurrency anomalies but reduces throughput to single-threaded performance, while READ_COMMITTED allows higher concurrency but risks non-repeatable reads. Production databases use READ_COMMITTED by default (PostgreSQL, Oracle) to achieve 80% of SERIALIZABLE safety at 300% higher throughput, reserving REPEATABLE_READ for financial transactions where accuracy outweighs performance.

---

### Example 33: API Versioning Strategies

Manage API evolution while maintaining backward compatibility through URL, header, or parameter versioning.

```java
// Strategy 1: URL Path Versioning
@RestController
@RequestMapping("/api/v1/users")
public class UserV1Controller {
    @GetMapping("/{id}")
    public UserV1 getUser(@PathVariable Long id) {
        return new UserV1(id, "Alice", "alice@example.com");
        // => /api/v1/users/1 returns version 1 format
    }
}

@RestController
@RequestMapping("/api/v2/users")
public class UserV2Controller {
    @GetMapping("/{id}")
    public UserV2 getUser(@PathVariable Long id) {
        return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        // => /api/v2/users/1 returns version 2 format (added lastName, phone)
    }
}

record UserV1(Long id, String name, String email) {}
record UserV2(Long id, String firstName, String lastName, String email, String phone) {}

// Strategy 2: Header Versioning
@RestController
@RequestMapping("/api/users")
public class UserHeaderVersionController {
    @GetMapping(value = "/{id}", headers = "X-API-Version=1")
    public UserV1 getUserV1(@PathVariable Long id) {
        return new UserV1(id, "Alice", "alice@example.com");
        // => Header: X-API-Version: 1
    }

    @GetMapping(value = "/{id}", headers = "X-API-Version=2")
    public UserV2 getUserV2(@PathVariable Long id) {
        return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        // => Header: X-API-Version: 2
    }
}

// Strategy 3: Content Negotiation (Accept Header)
@RestController
@RequestMapping("/api/users")
public class UserContentNegotiationController {
    @GetMapping(value = "/{id}", produces = "application/vnd.myapp.v1+json")
    public UserV1 getUserV1(@PathVariable Long id) {
        return new UserV1(id, "Alice", "alice@example.com");
        // => Header: Accept: application/vnd.myapp.v1+json
    }

    @GetMapping(value = "/{id}", produces = "application/vnd.myapp.v2+json")
    public UserV2 getUserV2(@PathVariable Long id) {
        return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        // => Header: Accept: application/vnd.myapp.v2+json
    }
}

// Strategy 4: Request Parameter Versioning
@RestController
@RequestMapping("/api/users")
public class UserParamVersionController {
    @GetMapping("/{id}")
    public Object getUser(@PathVariable Long id, @RequestParam(defaultValue = "1") int version) {
        if (version == 2) {
            return new UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890");
        }
        return new UserV1(id, "Alice", "alice@example.com");
        // => /api/users/1?version=2
    }
}
```

**Code (Kotlin)**:

```kotlin
// Strategy 1: URL Path Versioning
@RestController
@RequestMapping("/api/v1/users")
class UserV1Controller {
  @GetMapping("/{id}")
  fun getUser(@PathVariable id: Long): UserV1 {
    return UserV1(id, "Alice", "alice@example.com")
    // => /api/v1/users/1 returns version 1 format
  }
}

@RestController
@RequestMapping("/api/v2/users")
class UserV2Controller {
  @GetMapping("/{id}")
  fun getUser(@PathVariable id: Long): UserV2 {
    return UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890")
    // => /api/v2/users/1 returns version 2 format (added lastName, phone)
  }
}

data class UserV1(val id: Long, val name: String, val email: String)
data class UserV2(val id: Long, val firstName: String, val lastName: String, val email: String, val phone: String)

// Strategy 2: Header Versioning
@RestController
@RequestMapping("/api/users")
class UserHeaderVersionController {
  @GetMapping(value = ["/{id}"], headers = ["X-API-Version=1"])
  fun getUserV1(@PathVariable id: Long): UserV1 {
    return UserV1(id, "Alice", "alice@example.com")
    // => Header: X-API-Version: 1
  }

  @GetMapping(value = ["/{id}"], headers = ["X-API-Version=2"])
  fun getUserV2(@PathVariable id: Long): UserV2 {
    return UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890")
    // => Header: X-API-Version: 2
  }
}

// Strategy 3: Content Negotiation (Accept Header)
@RestController
@RequestMapping("/api/users")
class UserContentNegotiationController {
  @GetMapping(value = ["/{id}"], produces = ["application/vnd.myapp.v1+json"])
  fun getUserV1(@PathVariable id: Long): UserV1 {
    return UserV1(id, "Alice", "alice@example.com")
    // => Header: Accept: application/vnd.myapp.v1+json
  }

  @GetMapping(value = ["/{id}"], produces = ["application/vnd.myapp.v2+json"])
  fun getUserV2(@PathVariable id: Long): UserV2 {
    return UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890")
    // => Header: Accept: application/vnd.myapp.v2+json
  }
}

// Strategy 4: Request Parameter Versioning
@RestController
@RequestMapping("/api/users")
class UserParamVersionController {
  @GetMapping("/{id}")
  fun getUser(@PathVariable id: Long, @RequestParam(defaultValue = "1") version: Int): Any {
    return when (version) {
      2 -> UserV2(id, "Alice", "Smith", "alice@example.com", "123-456-7890")
      else -> UserV1(id, "Alice", "alice@example.com")
    }
    // => /api/users/1?version=2
  }
}

// Kotlin-specific: Use when expression instead of if-else chain, data classes for DTOs
// Alternative with sealed classes for type-safe versioning:
// sealed class UserResponse
// data class UserV1Response(val id: Long, val name: String, val email: String) : UserResponse()
// data class UserV2Response(val id: Long, val firstName: String, val lastName: String,
//                           val email: String, val phone: String) : UserResponse()
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    Client[API Client] --> V1[/api/v1/users]
    Client --> V2[/api/v2/users]

    V1 --> R1["UserV1<br/>{id, name, email}"]
    V2 --> R2["UserV2<br/>{id, firstName, lastName,<br/>email, phone}"]

    Note1["URL Versioning<br/>Most visible"] --> V1
    Note2["Backward Compatible<br/>New fields added"] --> V2

    style Client fill:#0173B2,color:#fff
    style V1 fill:#029E73,color:#fff
    style V2 fill:#DE8F05,color:#000
    style R1 fill:#CC78BC,color:#000
    style R2 fill:#CA9161,color:#000
```

**Key Takeaway**: Choose versioning strategy based on client capabilities—URL versioning is most visible and cacheable, header versioning keeps URLs clean, content negotiation follows REST standards, and parameter versioning is simplest for internal APIs.

**Why It Matters**: Optimistic locking enables high-concurrency updates without pessimistic database locks that block other transactions—version numbers detect conflicting updates at commit time instead of blocking readers during writes. E-commerce platforms use optimistic locking for shopping carts where 99% of updates succeed without conflicts, achieving 10x higher throughput than pessimistic locking while preventing lost updates when two users simultaneously buy the last item, with retry logic handling the rare 1% of conflicts gracefully.

---

### Example 34: Custom Argument Resolvers

Create custom argument resolvers to extract and inject domain objects from requests.

```java
// Custom annotation
@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
public @interface CurrentUser {}

// Argument resolver
@Component
public class CurrentUserArgumentResolver implements HandlerMethodArgumentResolver {
    @Override
    public boolean supportsParameter(MethodParameter parameter) {
        return parameter.hasParameterAnnotation(CurrentUser.class)
            && parameter.getParameterType().equals(User.class);
        // => Activates when @CurrentUser User parameter detected
    }

    @Override
    public Object resolveArgument(
        MethodParameter parameter,
        ModelAndViewContainer mavContainer,
        NativeWebRequest webRequest,
        WebDataBinderFactory binderFactory
    ) {
        String authHeader = webRequest.getHeader("Authorization");
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            return extractUserFromToken(token);
            // => Extracts user from JWT token
        }
        return null;
    }

    private User extractUserFromToken(String token) {
        return new User(1L, "alice", "alice@example.com");
        // => Simplified token parsing
    }
}

// Register resolver
@Configuration
public class WebConfig implements WebMvcConfigurer {
    @Autowired
    private CurrentUserArgumentResolver currentUserArgumentResolver;

    @Override
    public void addArgumentResolvers(List<HandlerMethodArgumentResolver> resolvers) {
        resolvers.add(currentUserArgumentResolver);
    }
}

// Usage in controller
@RestController
@RequestMapping("/api/profile")
public class ProfileController {
    @GetMapping
    public User getProfile(@CurrentUser User user) {
        // user automatically resolved from JWT token
        return user; // => No need to manually parse Authorization header
    }

    @PutMapping
    public User updateProfile(@CurrentUser User user, @RequestBody ProfileUpdate update) {
        // Both user and request body available
        return user;
    }
}

record User(Long id, String username, String email) {}
record ProfileUpdate(String email, String phone) {}
```

**Code (Kotlin)**:

```kotlin
// Custom annotation
@Target(AnnotationTarget.VALUE_PARAMETER)
@Retention(AnnotationRetention.RUNTIME)
annotation class CurrentUser

// Argument resolver
@Component
class CurrentUserArgumentResolver : HandlerMethodArgumentResolver {
  override fun supportsParameter(parameter: MethodParameter): Boolean {
    return parameter.hasParameterAnnotation(CurrentUser::class.java) &&
           parameter.parameterType == User::class.java
    // => Activates when @CurrentUser User parameter detected
  }

  override fun resolveArgument(
    parameter: MethodParameter,
    mavContainer: ModelAndViewContainer?,
    webRequest: NativeWebRequest,
    binderFactory: WebDataBinderFactory?
  ): Any? {
    val authHeader = webRequest.getHeader("Authorization")
    return authHeader?.takeIf { it.startsWith("Bearer ") }
      ?.substring(7)
      ?.let { extractUserFromToken(it) }
    // => Extracts user from JWT token using safe call chains
  }

  private fun extractUserFromToken(token: String): User {
    return User(1L, "alice", "alice@example.com")
    // => Simplified token parsing
  }
}

// Register resolver
@Configuration
class WebConfig(
  private val currentUserArgumentResolver: CurrentUserArgumentResolver
) : WebMvcConfigurer {
  override fun addArgumentResolvers(resolvers: MutableList<HandlerMethodArgumentResolver>) {
    resolvers.add(currentUserArgumentResolver)
  }
}

// Usage in controller
@RestController
@RequestMapping("/api/profile")
class ProfileController {
  @GetMapping
  fun getProfile(@CurrentUser user: User): User {
    // user automatically resolved from JWT token
    return user  // => No need to manually parse Authorization header
  }

  @PutMapping
  fun updateProfile(@CurrentUser user: User, @RequestBody update: ProfileUpdate): User {
    // Both user and request body available
    return user
  }
}

data class User(val id: Long, val username: String, val email: String)
data class ProfileUpdate(val email: String, val phone: String)

// Kotlin-specific: Use safe call operators (?., ?:) and scope functions for null-safe extraction
// Alternative with extension function:
// fun NativeWebRequest.extractBearerToken(): String? =
//   getHeader("Authorization")?.takeIf { it.startsWith("Bearer ") }?.substring(7)
```

**Key Takeaway**: Custom argument resolvers eliminate repetitive parameter extraction—implement `HandlerMethodArgumentResolver` to automatically inject domain objects from headers, cookies, or custom authentication mechanisms.

**Why It Matters**: Batch operations reduce database roundtrips from 1000 INSERTs to 20 batched INSERTs (50 per batch), decreasing import time from 10 minutes to 30 seconds—critical for ETL jobs processing millions of records. Production data pipelines at Airbnb and Uber use batch updates with manual flush/clear to import terabytes of data nightly without exhausting memory, while JPQL bulk updates execute single SQL statements that modify millions of rows without loading entities into memory.

---

### Example 35: Filter vs Interceptor vs AOP

Understand the differences and use cases for filters, interceptors, and AOP for cross-cutting concerns.

```java
// 1. Servlet Filter - Operates at servlet container level
@Component
@Order(1)
public class RequestResponseLoggingFilter implements Filter {
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
        throws IOException, ServletException {
        HttpServletRequest req = (HttpServletRequest) request;
        System.out.println("FILTER: Before request - " + req.getRequestURI());
        // => Executes before DispatcherServlet

        chain.doFilter(request, response); // => Continue filter chain (with or without authentication)

        System.out.println("FILTER: After response");
        // => Executes after response sent
    }
}

// 2. HandlerInterceptor - Operates at Spring MVC level
@Component
public class PerformanceInterceptor implements HandlerInterceptor {
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        request.setAttribute("startTime", System.currentTimeMillis());
        System.out.println("INTERCEPTOR: Before controller method");
        return true;
        // => Executes after DispatcherServlet, before controller
    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response,
                          Object handler, ModelAndView modelAndView) {
        System.out.println("INTERCEPTOR: After controller method, before view");
        // => Executes after controller, only if no exception
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response,
                               Object handler, Exception ex) {
        long duration = System.currentTimeMillis() - (Long) request.getAttribute("startTime");
        System.out.println("INTERCEPTOR: Request completed in " + duration + "ms");
        // => Always executes, even if exception occurred
    }
}

// 3. AOP Aspect - Operates at method level
@Aspect
@Component
public class LoggingAspect {
    @Before("execution(* com.example.demo.service.*.*(..))")
    public void logBefore(JoinPoint joinPoint) {
        System.out.println("AOP: Before method - " + joinPoint.getSignature().getName());
        // => Executes before any service method
    }

    @AfterReturning(pointcut = "execution(* com.example.demo.service.*.*(..))", returning = "result")
    public void logAfterReturning(JoinPoint joinPoint, Object result) {
        System.out.println("AOP: Method returned - " + result);
        // => Executes after successful method execution
    }

    @Around("@annotation(org.springframework.transaction.annotation.Transactional)")
    public Object logTransaction(ProceedingJoinPoint joinPoint) throws Throwable {
        System.out.println("AOP: Transaction starting");
        Object result = joinPoint.proceed();
        System.out.println("AOP: Transaction completed");
        return result;
        // => Wraps @Transactional methods
    }
}
```

**Code (Kotlin)**:

```kotlin
// 1. Servlet Filter - Operates at servlet container level
@Component
@Order(1)
class RequestResponseLoggingFilter : Filter {
  override fun doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
    val req = request as HttpServletRequest
    println("FILTER: Before request - ${req.requestURI}")
    // => Executes before DispatcherServlet

    chain.doFilter(request, response)  // => Continue filter chain (with or without authentication)

    println("FILTER: After response")
    // => Executes after response sent
  }
}

// 2. HandlerInterceptor - Operates at Spring MVC level
@Component
class PerformanceInterceptor : HandlerInterceptor {
  override fun preHandle(request: HttpServletRequest, response: HttpServletResponse, handler: Any): Boolean {
    request.setAttribute("startTime", System.currentTimeMillis())
    println("INTERCEPTOR: Before controller method")
    return true
    // => Executes after DispatcherServlet, before controller
  }

  override fun postHandle(
    request: HttpServletRequest,
    response: HttpServletResponse,
    handler: Any,
    modelAndView: ModelAndView?
  ) {
    println("INTERCEPTOR: After controller method, before view")
    // => Executes after controller, only if no exception
  }

  override fun afterCompletion(
    request: HttpServletRequest,
    response: HttpServletResponse,
    handler: Any,
    ex: Exception?
  ) {
    val duration = System.currentTimeMillis() - (request.getAttribute("startTime") as Long)
    println("INTERCEPTOR: Request completed in ${duration}ms")
    // => Always executes, even if exception occurred
  }
}

// 3. AOP Aspect - Operates at method level
@Aspect
@Component
class LoggingAspect {
  @Before("execution(* com.example.demo.service.*.*(..))")
  fun logBefore(joinPoint: JoinPoint) {
    println("AOP: Before method - ${joinPoint.signature.name}")
    // => Executes before any service method
  }

  @AfterReturning(pointcut = "execution(* com.example.demo.service.*.*(..))", returning = "result")
  fun logAfterReturning(joinPoint: JoinPoint, result: Any?) {
    println("AOP: Method returned - $result")
    // => Executes after successful method execution
  }

  @Around("@annotation(org.springframework.transaction.annotation.Transactional)")
  fun logTransaction(joinPoint: ProceedingJoinPoint): Any? {
    println("AOP: Transaction starting")
    val result = joinPoint.proceed()
    println("AOP: Transaction completed")
    return result
    // => Wraps @Transactional methods
  }
}

// Kotlin-specific: Use string templates, safe cast (as?), nullable types (Any?, Exception?)
// Alternative with inline reified functions for type-safe getAttribute:
// inline fun <reified T> HttpServletRequest.getTypedAttribute(name: String): T? =
//   getAttribute(name) as? T
// val duration = System.currentTimeMillis() - (request.getTypedAttribute<Long>("startTime") ?: 0L)
```

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
flowchart TD
    Request[HTTP Request] --> Filter[Servlet Filter]
    Filter --> DispatcherServlet[DispatcherServlet]
    DispatcherServlet --> InterceptorPre[Interceptor preHandle]
    InterceptorPre --> AOP_Before[AOP @Before]
    AOP_Before --> Controller[Controller Method]
    Controller --> AOP_After[AOP @AfterReturning]
    AOP_After --> InterceptorPost[Interceptor postHandle]
    InterceptorPost --> View[View Rendering]
    View --> InterceptorAfter[Interceptor afterCompletion]
    InterceptorAfter --> FilterAfter[Filter doFilter return]
    FilterAfter --> Response[HTTP Response]

    style Request fill:#0173B2,color:#fff
    style Filter fill:#DE8F05,color:#000
    style DispatcherServlet fill:#029E73,color:#fff
    style InterceptorPre fill:#CC78BC,color:#000
    style AOP_Before fill:#CA9161,color:#000
    style Controller fill:#0173B2,color:#fff
    style Response fill:#029E73,color:#fff
```

**Key Takeaway**: Use **Filters** for servlet-level concerns (encoding, security, CORS), **Interceptors** for Spring MVC concerns (authentication, logging, request/response modification), and **AOP** for business logic concerns (transactions, caching, auditing) targeting specific methods.

**Why It Matters**: Spring Security's auto-configuration prevents 80% of OWASP Top 10 vulnerabilities (CSRF, session fixation, clickjacking) through secure defaults, eliminating manual security code that developers implement incorrectly. However, default form login exposes application structure through /login pages—production systems replace it with JWT or OAuth2 for stateless authentication that scales horizontally without session affinity, enabling load balancers to distribute traffic across instances without sticky sessions.

---

### Example 36: Custom Annotations with AOP

Combine custom annotations with AOP for declarative cross-cutting concerns.

```java
// Custom annotation
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface LogExecutionTime {}

// AOP Aspect
@Aspect
@Component
@Slf4j
public class ExecutionTimeAspect {
    @Around("@annotation(LogExecutionTime)")
    public Object logExecutionTime(ProceedingJoinPoint joinPoint) throws Throwable {
        long start = System.currentTimeMillis();

        Object result = joinPoint.proceed(); // => Execute method

        long duration = System.currentTimeMillis() - start;
        log.info("{} executed in {}ms", joinPoint.getSignature(), duration);
        // => Logs: com.example.demo.service.UserService.findUser(..) executed in 45ms

        return result;
    }
}

// Custom audit annotation
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Audit {
    String action();
}

@Aspect
@Component
public class AuditAspect {
    @Autowired
    private AuditLogRepository auditLogRepository;

    @AfterReturning("@annotation(audit)")
    public void logAudit(JoinPoint joinPoint, Audit audit) {
        String username = SecurityContextHolder.getContext().getAuthentication().getName();
        String methodName = joinPoint.getSignature().getName();

        AuditLog log = new AuditLog(
            username,
            audit.action(),
            methodName,
            LocalDateTime.now()
        );
        auditLogRepository.save(log);
        // => Automatically logs all audited operations
    }
}

// Usage in service
@Service
public class UserService {
    @LogExecutionTime
    @Audit(action = "USER_CREATED")
    public User createUser(User user) {
        // Method automatically timed and audited
        return userRepository.save(user);
    }

    @LogExecutionTime
    public List<User> findAll() {
        // Only execution time logged (no audit)
        return userRepository.findAll();
    }
}
```

**Code (Kotlin)**:

```kotlin
// Custom annotation
@Target(AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class LogExecutionTime

@Aspect
@Component
class ExecutionTimeAspect {
  @Around("@annotation(LogExecutionTime)")
  fun logExecutionTime(joinPoint: ProceedingJoinPoint): Any? {
    val start = System.currentTimeMillis()

    val result = joinPoint.proceed()  // Execute method

    val duration = System.currentTimeMillis() - start
    println("${joinPoint.signature} executed in ${duration}ms")
    // => Logs: com.example.demo.service.UserService.findUser(..) executed in 45ms

    return result
  }
}

// Custom audit annotation
@Target(AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class Audit(val action: String)

@Aspect
@Component
class AuditAspect(
  private val auditLogRepository: AuditLogRepository
) {
  @AfterReturning("@annotation(audit)")
  fun logAudit(joinPoint: JoinPoint, audit: Audit) {
    val username = SecurityContextHolder.getContext().authentication.name
    val methodName = joinPoint.signature.name

    val log = AuditLog(
      username = username,
      action = audit.action,
      methodName = methodName,
      timestamp = LocalDateTime.now()
    )
    auditLogRepository.save(log)
    // => Automatically logs all audited operations
  }
}

// Usage in service
@Service
open class UserService(
  private val userRepository: UserRepository
) {
  @LogExecutionTime
  @Audit(action = "USER_CREATED")
  open fun createUser(user: User): User {
    // Method automatically timed and audited
    return userRepository.save(user)
  }

  @LogExecutionTime
  open fun findAll(): List<User> {
    // Only execution time logged (no audit)
    return userRepository.findAll()
  }
}

// Kotlin-specific: Use named parameters in data class for audit log, string templates
// Alternative with inline class for action type safety:
// @JvmInline value class AuditAction(val value: String)
// annotation class Audit(val action: AuditAction)
// @Audit(action = AuditAction("USER_CREATED"))
```

**Key Takeaway**: Combine custom annotations with AOP for declarative cross-cutting concerns—create domain-specific annotations (`@LogExecutionTime`, `@Audit`, `@RateLimit`) and implement behavior in aspects for clean, reusable functionality.

**Why It Matters**: Method-level authorization enforces business rules at the code level where they can't be bypassed—URL-level security (`/admin/**`) fails when developers add new endpoints that forget URL patterns, while `@PreAuthorize` prevents access attempts at method invocation. Production SaaS applications use SpEL expressions for tenant isolation (`#order.tenantId == principal.tenantId`) that ensure users can't access other tenants' data even if URL tampering bypasses endpoint security, preventing data leaks that cause compliance violations and customer churn.

---

### Example 37: Bean Post Processors

Modify or enhance beans during initialization with BeanPostProcessor.

```java
// Custom annotation for initialization
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface InitializeOnStartup {}

// Bean post processor
@Component
public class InitializationBeanPostProcessor implements BeanPostProcessor {
    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) {
        // Called before @PostConstruct
        if (bean.getClass().isAnnotationPresent(InitializeOnStartup.class)) {
            System.out.println("Initializing bean: " + beanName);
        }
        return bean;
    }

    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) {
        // Called after @PostConstruct
        if (bean instanceof CacheManager) {
            System.out.println("CacheManager bean ready: " + beanName);
            ((CacheManager) bean).warmUpCache();
            // => Automatically warm up cache after initialization
        }
        return bean;
    }
}

// Auto-proxy creation example
@Component
public class PerformanceProxyBeanPostProcessor implements BeanPostProcessor {
    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) {
        if (bean.getClass().getPackageName().startsWith("com.example.demo.service")) {
            // Wrap service beans in performance monitoring proxy
            return createProxy(bean);
        }
        return bean;
    }

    private Object createProxy(Object target) {
        return Proxy.newProxyInstance(
            target.getClass().getClassLoader(),
            target.getClass().getInterfaces(),
            (proxy, method, args) -> {
                long start = System.currentTimeMillis();
                Object result = method.invoke(target, args);
                long duration = System.currentTimeMillis() - start;
                System.out.println(method.getName() + " took " + duration + "ms");
                return result;
            }
        );
    }
}

@InitializeOnStartup
@Service
public class DataPreloadService {
    @PostConstruct
    public void init() {
        System.out.println("Preloading data...");
    }
}
```

**Code (Kotlin)**:

```kotlin
// Custom annotation for initialization
@Target(AnnotationTarget.CLASS)
@Retention(AnnotationRetention.RUNTIME)
annotation class InitializeOnStartup

// Bean post processor
@Component
class InitializationBeanPostProcessor : BeanPostProcessor {
  override fun postProcessBeforeInitialization(bean: Any, beanName: String): Any {
    // Called before @PostConstruct
    if (bean::class.java.isAnnotationPresent(InitializeOnStartup::class.java)) {
      println("Initializing bean: $beanName")
    }
    return bean
  }

  override fun postProcessAfterInitialization(bean: Any, beanName: String): Any {
    // Called after @PostConstruct
    if (bean is CacheManager) {
      println("CacheManager bean ready: $beanName")
      bean.warmUpCache()
      // => Automatically warm up cache after initialization
    }
    return bean
  }
}

// Auto-proxy creation example
@Component
class PerformanceProxyBeanPostProcessor : BeanPostProcessor {
  override fun postProcessAfterInitialization(bean: Any, beanName: String): Any {
    return if (bean::class.java.packageName.startsWith("com.example.demo.service")) {
      // Wrap service beans in performance monitoring proxy
      createProxy(bean)
    } else {
      bean
    }
  }

  private fun createProxy(target: Any): Any {
    return Proxy.newProxyInstance(
      target::class.java.classLoader,
      target::class.java.interfaces
    ) { _, method, args ->
      val start = System.currentTimeMillis()
      val result = method.invoke(target, *args.orEmpty())
      val duration = System.currentTimeMillis() - start
      println("${method.name} took ${duration}ms")
      result
    }
  }
}

@InitializeOnStartup
@Service
class DataPreloadService {
  @PostConstruct
  fun init() {
    println("Preloading data...")
  }
}

// Kotlin-specific: Use is operator for type checking, spread operator (*) for varargs
// Alternative with extension function:
// fun Any.isInPackage(packagePrefix: String): Boolean =
//   this::class.java.packageName.startsWith(packagePrefix)
// if (bean.isInPackage("com.example.demo.service")) { createProxy(bean) } else { bean }
```

**Key Takeaway**: BeanPostProcessors enable bean customization during initialization—use `postProcessBeforeInitialization` for pre-init configuration and `postProcessAfterInitialization` for auto-proxying, validation, or post-init setup.

**Why It Matters**: JWT enables stateless authentication essential for horizontal scaling—servers don't share session state, so load balancers distribute requests to any instance without session replication overhead. However, JWT tokens can't be revoked before expiration (unlike sessions), requiring short expiration times (15 minutes) with refresh tokens for security, balancing convenience (fewer re-logins) against blast radius (stolen tokens valid until expiration), a trade-off production systems at Auth0 and Okta tune based on threat models.

---

### Example 38: Custom Spring Boot Starter (Simplified)

Create a lightweight auto-configuration module for reusable functionality.

```java
// 1. Create auto-configuration class
@Configuration
@ConditionalOnClass(EmailService.class)
@EnableConfigurationProperties(EmailProperties.class)
public class EmailAutoConfiguration {
    @Bean
    @ConditionalOnMissingBean
    public EmailService emailService(EmailProperties properties) {
        return new EmailService(properties);
    }
}

// 2. Configuration properties
@ConfigurationProperties(prefix = "app.email")
public class EmailProperties {
    private String host = "smtp.gmail.com";
    private int port = 587;
    private String username;
    private String password;
    // getters/setters
}

// 3. Service implementation
public class EmailService {
    private final EmailProperties properties;

    public EmailService(EmailProperties properties) {
        this.properties = properties;
    }

    public void sendEmail(String to, String subject, String body) {
        System.out.println("Sending email to " + to + " via " + properties.getHost());
        // Actual email sending logic
    }
}

// 4. Register auto-configuration
// Create: META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports
// Add line: com.example.starter.EmailAutoConfiguration

// 5. Usage in application
// Just add dependency and configure:
// app.email.username=user@gmail.com
// app.email.password=secret

@Service
public class NotificationService {
    @Autowired
    private EmailService emailService; // Automatically available!

    public void notifyUser(String email) {
        emailService.sendEmail(email, "Welcome", "Thanks for signing up!");
    }
}
```

**Code (Kotlin)**:

```kotlin
// 1. Create auto-configuration class
@Configuration
@ConditionalOnClass(EmailService::class)
@EnableConfigurationProperties(EmailProperties::class)
open class EmailAutoConfiguration {
  @Bean
  @ConditionalOnMissingBean
  open fun emailService(properties: EmailProperties): EmailService {
    return EmailService(properties)
  }
}

// 2. Configuration properties
@ConfigurationProperties(prefix = "app.email")
data class EmailProperties(
  var host: String = "smtp.gmail.com",
  var port: Int = 587,
  var username: String? = null,
  var password: String? = null
)

// 3. Service implementation
class EmailService(
  private val properties: EmailProperties
) {
  fun sendEmail(to: String, subject: String, body: String) {
    println("Sending email to $to via ${properties.host}")
    // Actual email sending logic
  }
}

// 4. Register auto-configuration
// Create: META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports
// Add line: com.example.starter.EmailAutoConfiguration

// 5. Usage in application
// Just add dependency and configure:
// app.email.username=user@gmail.com
// app.email.password=secret

@Service
class NotificationService(
  private val emailService: EmailService  // Automatically available!
) {
  fun notifyUser(email: String) {
    emailService.sendEmail(email, "Welcome", "Thanks for signing up!")
  }
}

// Kotlin-specific: Use data class with var properties for @ConfigurationProperties binding
// Alternative with sealed class for multiple email providers:
// sealed class EmailProvider
// data class SmtpProvider(val host: String, val port: Int) : EmailProvider()
// data class SendGridProvider(val apiKey: String) : EmailProvider()
// @ConfigurationProperties(prefix = "app.email")
// data class EmailProperties(var provider: EmailProvider)
```

**Key Takeaway**: Create Spring Boot starters for reusable auto-configuration—define `@Configuration` with `@ConditionalOnClass` and `@EnableConfigurationProperties`, register in `AutoConfiguration.imports`, and users get automatic bean registration with type-safe properties.

**Why It Matters**: OAuth2 delegates authentication to specialized providers (Google, GitHub, AWS Cognito) that invest millions in security infrastructure, enabling applications to avoid storing passwords that require expensive PCI/SOC2 compliance. Social login reduces signup friction by 30-50% (no password memorization), but introduces dependency on external providers—production systems implement fallback authentication for when OAuth providers have outages, preventing login failures that lock out all users during downtime.

---

### Example 39: Reactive Repositories with R2DBC

Use R2DBC for reactive, non-blocking database access.

```java
// pom.xml: spring-boot-starter-data-r2dbc, r2dbc-h2

@Entity
@Table(name = "products")
public class Product {
    @Id
    private Long id;
    private String name;
    private BigDecimal price;
    // getters/setters
}

// Reactive repository
public interface ProductRepository extends ReactiveCrudRepository<Product, Long> {
    Flux<Product> findByNameContaining(String name); // => Returns Flux (0..N items)
    Mono<Product> findByName(String name); // => Returns Mono (0..1 item)

    @Query("SELECT * FROM products WHERE price > :minPrice")
    Flux<Product> findExpensiveProducts(BigDecimal minPrice);
}

@Service
public class ProductService {
    @Autowired
    private ProductRepository productRepository;

    public Flux<Product> getAllProducts() {
        return productRepository.findAll();
        // => Non-blocking stream of products
    }

    public Mono<Product> createProduct(Product product) {
        return productRepository.save(product);
        // => Non-blocking save operation
    }

    public Flux<Product> searchProducts(String keyword) {
        return productRepository.findByNameContaining(keyword)
            .filter(p -> p.getPrice().compareTo(BigDecimal.ZERO) > 0)
            .map(p -> {
                p.setName(p.getName().toUpperCase());
                return p;
            });
        // => Reactive pipeline: fetch → filter → transform
    }
}

@RestController
@RequestMapping("/api/products")
public class ProductController {
    @Autowired
    private ProductService productService;

    @GetMapping(produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public Flux<Product> streamProducts() {
        return productService.getAllProducts();
        // => Streams products as Server-Sent Events
    }

    @PostMapping
    public Mono<Product> createProduct(@RequestBody Product product) {
        return productService.createProduct(product);
        // => Non-blocking POST handling
    }
}

// application.yml
// spring:
//   r2dbc:
//     url: r2dbc:h2:mem:///testdb
//     username: sa
//     password:
```

**Code (Kotlin)**:

```kotlin
// build.gradle.kts: implementation("org.springframework.boot:spring-boot-starter-data-r2dbc")
// implementation("io.r2dbc:r2dbc-h2")

@Entity
@Table(name = "products")
data class Product(
  @Id var id: Long? = null,
  var name: String = "",
  var price: BigDecimal = BigDecimal.ZERO
)

// Reactive repository
interface ProductRepository : ReactiveCrudRepository<Product, Long> {
  fun findByNameContaining(name: String): Flux<Product>  // => Returns Flux (0..N items)
  fun findByName(name: String): Mono<Product>  // => Returns Mono (0..1 item)

  @Query("SELECT * FROM products WHERE price > :minPrice")
  fun findExpensiveProducts(minPrice: BigDecimal): Flux<Product>
}

@Service
class ProductService(
  private val productRepository: ProductRepository
) {
  fun getAllProducts(): Flux<Product> {
    return productRepository.findAll()
    // => Non-blocking stream of products
  }

  fun createProduct(product: Product): Mono<Product> {
    return productRepository.save(product)
    // => Non-blocking save operation
  }

  fun searchProducts(keyword: String): Flux<Product> {
    return productRepository.findByNameContaining(keyword)
      .filter { it.price > BigDecimal.ZERO }
      .map { product ->
        product.copy(name = product.name.uppercase())
      }
    // => Reactive pipeline: fetch → filter → transform
  }
}

@RestController
@RequestMapping("/api/products")
class ProductController(
  private val productService: ProductService
) {
  @GetMapping(produces = [MediaType.TEXT_EVENT_STREAM_VALUE])
  fun streamProducts(): Flux<Product> {
    return productService.getAllProducts()
    // => Streams products as Server-Sent Events
  }

  @PostMapping
  fun createProduct(@RequestBody product: Product): Mono<Product> {
    return productService.createProduct(product)
    // => Non-blocking POST handling
  }
}

// application.yml
// spring:
//   r2dbc:
//     url: r2dbc:h2:mem:///testdb
//     username: sa
//     password:

// Kotlin-specific: Use copy() for immutable transformations, operator overloading for BigDecimal
// Alternative with coroutines (more idiomatic than Reactor):
// interface ProductRepository : CoroutineCrudRepository<Product, Long> {
//   suspend fun findByNameContaining(name: String): Flow<Product>
//   suspend fun findByName(name: String): Product?
// }
// suspend fun searchProducts(keyword: String): Flow<Product> = flow {
//   productRepository.findByNameContaining(keyword)
//     .filter { it.price > BigDecimal.ZERO }
//     .map { it.copy(name = it.name.uppercase()) }
//     .collect { emit(it) }
// }
```

**Key Takeaway**: R2DBC enables reactive database access—use `ReactiveCrudRepository` for non-blocking queries returning `Mono<T>` (0..1) or `Flux<T>` (0..N), enabling end-to-end reactive pipelines from database to HTTP response.

**Why It Matters**: Integration tests verify controller-to-database flows including JSON serialization, exception handling, and transaction management—catching bugs that unit tests miss because mocks don't behave like real implementations. However, @SpringBootTest loads the full context (2-5 seconds per test), making large test suites slow (20 minutes for 500 tests), requiring careful test design where unit tests cover 80% of logic with @WebMvcTest, reserving integration tests for critical paths that justify the performance cost.

---

### Example 40: Composite Keys and Embedded IDs

Handle composite primary keys with `@IdClass` or `@EmbeddedId`.

```java
// Strategy 1: @IdClass
@IdClass(OrderItemId.class)
@Entity
public class OrderItem {
    @Id
    private Long orderId;

    @Id
    private Long productId;

    private int quantity;
    private BigDecimal price;

    // Constructors, getters, setters
}

// Composite key class
public class OrderItemId implements Serializable {
    private Long orderId;
    private Long productId;

    // Must have equals() and hashCode()
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof OrderItemId)) return false;
        OrderItemId that = (OrderItemId) o;
        return Objects.equals(orderId, that.orderId) &&
               Objects.equals(productId, that.productId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(orderId, productId);
    }
}

// Repository with composite key
public interface OrderItemRepository extends JpaRepository<OrderItem, OrderItemId> {
    List<OrderItem> findByOrderId(Long orderId);
}

// Usage
OrderItemId id = new OrderItemId();
id.setOrderId(1L);
id.setProductId(100L);
Optional<OrderItem> item = orderItemRepository.findById(id);

// Strategy 2: @EmbeddedId (recommended)
@Embeddable
public class OrderItemKey implements Serializable {
    private Long orderId;
    private Long productId;

    // equals(), hashCode(), getters, setters
}

@Entity
public class OrderItemEmbedded {
    @EmbeddedId
    private OrderItemKey id;

    private int quantity;
    private BigDecimal price;

    // Access composite key fields
    public Long getOrderId() {
        return id.getOrderId();
    }
}

// Repository
public interface OrderItemEmbeddedRepository extends JpaRepository<OrderItemEmbedded, OrderItemKey> {
    @Query("SELECT o FROM OrderItemEmbedded o WHERE o.id.orderId = :orderId")
    List<OrderItemEmbedded> findByOrderId(Long orderId);
}

// Usage
OrderItemKey key = new OrderItemKey(1L, 100L);
OrderItemEmbedded item = new OrderItemEmbedded();
item.setId(key);
item.setQuantity(5);
orderItemEmbeddedRepository.save(item);
```

**Code (Kotlin)**:

```kotlin
// Strategy 1: @IdClass
@IdClass(OrderItemId::class)
@Entity
open class OrderItem(
  @Id var orderId: Long? = null,
  @Id var productId: Long? = null,
  var quantity: Int = 0,
  var price: BigDecimal = BigDecimal.ZERO
)

// Composite key class
data class OrderItemId(
  var orderId: Long? = null,
  var productId: Long? = null
) : Serializable {
  // equals() and hashCode() auto-generated by data class
}

// Repository with composite key
interface OrderItemRepository : JpaRepository<OrderItem, OrderItemId> {
  fun findByOrderId(orderId: Long): List<OrderItem>
}

// Usage
val id = OrderItemId(orderId = 1L, productId = 100L)
val item = orderItemRepository.findById(id)

// Strategy 2: @EmbeddedId (recommended)
@Embeddable
data class OrderItemKey(
  var orderId: Long? = null,
  var productId: Long? = null
) : Serializable
// equals() and hashCode() auto-generated by data class

@Entity
open class OrderItemEmbedded(
  @EmbeddedId var id: OrderItemKey? = null,
  var quantity: Int = 0,
  var price: BigDecimal = BigDecimal.ZERO
) {
  // Access composite key fields
  val orderId: Long?
    get() = id?.orderId
}

// Repository
interface OrderItemEmbeddedRepository : JpaRepository<OrderItemEmbedded, OrderItemKey> {
  @Query("SELECT o FROM OrderItemEmbedded o WHERE o.id.orderId = :orderId")
  fun findByOrderId(orderId: Long): List<OrderItemEmbedded>
}

// Usage
val key = OrderItemKey(orderId = 1L, productId = 100L)
val item = OrderItemEmbedded(
  id = key,
  quantity = 5,
  price = BigDecimal("29.99")
)
orderItemEmbeddedRepository.save(item)

// Kotlin-specific: Use data class for automatic equals()/hashCode() implementation
// Alternative with inline value class for type safety (Kotlin 1.5+):
// @JvmInline value class OrderId(val value: Long)
// @JvmInline value class ProductId(val value: Long)
// @Embeddable
// data class OrderItemKey(val orderId: OrderId, val productId: ProductId) : Serializable
```

**Key Takeaway**: Use `@EmbeddedId` over `@IdClass` for composite keys—it encapsulates key fields in a single object, provides better type safety, and makes queries clearer by explicitly referencing the embedded ID.

**Why It Matters**: MockMvc tests verify REST API contracts (request mapping, response codes, JSON structure) 10x faster than integration tests because they don't start HTTP servers or load full contexts, enabling rapid TDD feedback loops. Production teams use MockMvc for controller logic and @JsonTest for serialization verification, achieving 95% branch coverage with 2-minute test suite execution that enables continuous deployment where every commit triggers automated tests before merging to main.

---

## Summary

You've learned 30 intermediate Spring Boot patterns:

**Transactions & Data**:

- `@Transactional` for ACID operations
- Isolation levels for concurrency control
- Optimistic locking with `@Version`
- Batch operations for performance

**Spring Security**:

- Auto-configuration and custom security chains
- Method-level authorization with `@PreAuthorize`
- JWT stateless authentication
- OAuth2 social login

**Testing**:

- `@SpringBootTest` for integration tests
- `@WebMvcTest` for controller tests
- TestContainers for real databases
- Mockito for unit testing

**Caching & Performance**:

- Cache abstraction with `@Cacheable`
- Redis distributed caching
- Cache strategies and pitfalls

**Async & Events**:

- `@Async` methods with CompletableFuture
- Custom thread pools with TaskExecutor
- Application events for decoupling
- `@Scheduled` tasks for automation

**Real-Time & Advanced Patterns**:

- WebSocket for bidirectional real-time communication
- Server-Sent Events for unidirectional streaming
- API versioning strategies (URL, header, content negotiation)
- Custom argument resolvers for parameter extraction
- Filter vs Interceptor vs AOP comparisons
- Custom annotations with AOP for declarative concerns
- Bean post processors for bean customization
- Custom Spring Boot starters
- Reactive repositories with R2DBC
- Composite keys and embedded IDs

## Next Steps

- [Advanced by-example tutorial](/en/learn/software-engineering/web-platform/jvm-spring-boot/by-example/advanced) - Microservices, observability, advanced patterns
