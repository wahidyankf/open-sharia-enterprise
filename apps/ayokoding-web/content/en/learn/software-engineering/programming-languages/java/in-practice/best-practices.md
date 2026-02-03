---
title: "Best Practices"
date: 2025-12-12T00:00:00+07:00
draft: false
description: Proven approaches and modern Java coding standards for maintainable, reliable software
weight: 10000002
tags: ["java", "best-practices", "clean-code", "solid-principles", "code-quality"]
---

## Understanding Best Practices

Best practices represent proven approaches to common programming challenges, distilled from years of collective experience. Unlike anti-patterns which demonstrate what to avoid, best practices guide you toward robust, maintainable solutions.

**Why follow best practices:**

- **Predictability**: Code behavior becomes easier to reason about
- **Maintainability**: Changes require less effort and introduce fewer bugs
- **Collaboration**: Teams share common vocabulary and expectations
- **Quality**: Fewer defects reach production environments

This guide presents Java best practices organized by category, explaining their rationale and demonstrating applications.

## Core Development Principles

### Code Clarity Over Cleverness

Write code optimizing for human comprehension rather than minimal line count. Clear code enables thorough review and reduces debugging time.

**Key insight**: Code is read far more often than written. Optimizing for readability pays dividends throughout software lifecycle.

**Comparison:**

| Approach   | Characteristics                                         | Impact                                   |
| ---------- | ------------------------------------------------------- | ---------------------------------------- |
| **Clear**  | Explicit variable names, simple logic flow              | Easy to understand, maintain, debug      |
| **Clever** | Abbreviated names, nested ternaries, complex one-liners | Saves lines but costs comprehension time |

**Before (clever but unclear):**

```java
public boolean check(Money w, Money n) {
    return w.compareTo(n) >= 0;
}
```

**After (clear and explicit):**

```java
public boolean isEligibleForProcessing(Money wealth, Money threshold) {
    return wealth.isGreaterThanOrEqualTo(threshold);
}
```

### Single Responsibility Principle

Each method and class should have one clearly defined purpose. This makes code easier to test, modify, and reuse.

**Recognition signals:**

- Methods exceed 20 lines
- Classes handle multiple unrelated concerns
- Method names contain "and" or "or"
- Difficulty naming methods concisely

**Before (mixed responsibilities):**

```java
public class PaymentService {
    public Money processAndSave(Money amount) {
        Money fee = amount.multiply(new BigDecimal("0.025"));
        // Database code mixed with calculation
        saveToDatabase(fee);
        return fee;
    }
}
```

**After (separated concerns):**

```java
public class FeeCalculator {
    public Money calculateFee(Money amount) {
        return amount.multiply(new BigDecimal("0.025"));
    }
}

public class PaymentRepository {
    public void savePaymentRecord(PaymentRecord record) {
        // Persistence logic only
    }
}
```

### Fail Fast and Explicitly

Validate inputs early and throw meaningful exceptions. Don't let invalid data propagate through the system.

**Benefits:**

- Problems detected at source rather than far downstream
- Error messages provide actionable context
- Debugging time reduced significantly
- System state remains consistent

**Before (no validation):**

```java
public Contract createContract(Money price, String customerId) {
    return new Contract(price, customerId); // What if price is null or negative?
}
```

**After (early validation):**

```java
public Contract createContract(Money price, String customerId) {
    if (price == null || price.isNegative()) {
        throw new IllegalArgumentException("Price must be a positive amount");
    }
    if (customerId == null || customerId.isBlank()) {
        throw new IllegalArgumentException("Customer ID is required");
    }

    return new Contract(price, customerId);
}
```

### Embrace Immutability

Prefer immutable objects, especially for value objects and domain entities. Use `final` fields and return new instances rather than modifying existing ones.

**Why immutability matters:**

- Thread-safe by default (no synchronization needed)
- Prevents accidental modification bugs
- Enables safe sharing across system components
- Simplifies reasoning about code behavior

**Before (mutable object):**

```java
public class Money {
    private BigDecimal amount;
    private Currency currency;

    public void setAmount(BigDecimal amount) {
        this.amount = amount; // Dangerous mutation
    }
}
```

**After (immutable object):**

```java
public final class Money {
    private final BigDecimal amount;
    private final Currency currency;

    public Money(BigDecimal amount, Currency currency) {
        this.amount = amount;
        this.currency = currency;
    }

    public Money add(Money other) {
        validateSameCurrency(other);
        return new Money(this.amount.add(other.amount), this.currency);
    }

    // Getters only, no setters
    public BigDecimal getAmount() {
        return amount;
    }
}
```

### Composition Over Inheritance

Use composition and delegation instead of extending classes. This provides greater flexibility and reduces coupling.

**When composition excels:**

- Behavior combinations vary independently
- Multiple inheritance would be useful (Java doesn't support)
- Avoiding fragile base class problems
- Runtime behavior changes needed

**Before (inheritance hierarchy):**

```java
public abstract class Product { }
public abstract class PricedProduct extends Product { }
public class StandardProduct extends PricedProduct { }
public class PremiumProduct extends PricedProduct { }
```

**After (composition with strategies):**

```java
public interface PricingStrategy {
    Money calculatePrice(Money basePrice);
}

public class StandardPricing implements PricingStrategy {
    public Money calculatePrice(Money basePrice) {
        return basePrice;
    }
}

public class PremiumPricing implements PricingStrategy {
    private final BigDecimal multiplier;

    public PremiumPricing(BigDecimal multiplier) {
        this.multiplier = multiplier;
    }

    public Money calculatePrice(Money basePrice) {
        return basePrice.multiply(multiplier);
    }
}

public class Product {
    private final PricingStrategy pricingStrategy;

    public Product(PricingStrategy pricingStrategy) {
        this.pricingStrategy = pricingStrategy;
    }

    public Money getPrice(Money basePrice) {
        return pricingStrategy.calculatePrice(basePrice);
    }
}
```

## Code Organization

### Keep Methods Small (10-20 Lines)

Methods should do one thing well. Aim for 10-20 lines per method. Extract helper methods when exceeding this range.

**Benefits of small methods:**

- Easier to understand at a glance
- Simpler to test in isolation
- More reusable across codebase
- Better names (focused purpose)

**Before (long method):**

```java
public Contract generate(Request request) {
    if (request == null) throw new IllegalArgumentException("Request cannot be null");
    if (request.getPrice().isNegativeOrZero()) throw new IllegalArgumentException("Price must be positive");

    Money total = request.getPrice().add(request.getFee()).add(request.getTax());

    List<Payment> payments = new ArrayList<>();
    Money installment = total.divide(
        BigDecimal.valueOf(request.getTermMonths()),
        RoundingMode.HALF_UP
    );
    LocalDate paymentDate = LocalDate.now();

    for (int i = 0; i < request.getTermMonths(); i++) {
        paymentDate = paymentDate.plusMonths(1);
        payments.add(new Payment(installment, paymentDate));
    }

    return new Contract(total, new Schedule(payments), request.getCustomerId());
}
```

**After (extracted methods):**

```java
public Contract generate(Request request) {
    validateRequest(request);
    Money totalCost = calculateTotalCost(request);
    PaymentSchedule schedule = generatePaymentSchedule(request, totalCost);

    return new Contract(totalCost, schedule, request.getCustomerId());
}

private void validateRequest(Request request) {
    if (request == null) {
        throw new IllegalArgumentException("Request cannot be null");
    }
    if (request.getPrice().isNegativeOrZero()) {
        throw new IllegalArgumentException("Price must be positive");
    }
}

private Money calculateTotalCost(Request request) {
    return request.getPrice()
        .add(request.getFee())
        .add(request.getTax());
}

private PaymentSchedule generatePaymentSchedule(Request request, Money totalCost) {
    return scheduleGenerator.generate(
        totalCost,
        request.getTermMonths(),
        request.getPaymentFrequency()
    );
}
```

### Use Intention-Revealing Names

Choose names that clearly express purpose. Include units in variable names when appropriate.

**Naming guidelines:**

| Element   | Convention          | Example                                 |
| --------- | ------------------- | --------------------------------------- |
| Classes   | Noun or noun phrase | `PaymentCalculator`, `UserRepository`   |
| Methods   | Verb or verb phrase | `calculateTotal()`, `findUserById()`    |
| Booleans  | Question form       | `isValid()`, `hasPermission()`          |
| Constants | ALL_CAPS_SNAKE_CASE | `MAX_RETRY_COUNT`, `DEFAULT_TIMEOUT_MS` |

**Before (unclear names):**

```java
public class Calculator {
    private static final BigDecimal RATE = new BigDecimal("0.025");

    public Money calc(Money w, int d) {
        if (d < 365) {
            throw new IllegalArgumentException("Invalid days");
        }
        return w.multiply(RATE);
    }
}
```

**After (clear names):**

```java
public class AnnualFeeCalculator {
    private static final BigDecimal ANNUAL_FEE_RATE = new BigDecimal("0.025");

    public Money calculateAnnualFee(Money balance, int daysHeld) {
        if (daysHeld < 365) {
            throw new IllegalArgumentException(
                "Balance must be held for at least 365 days"
            );
        }
        return balance.multiply(ANNUAL_FEE_RATE);
    }
}
```

### Organize Code by Feature, Not Layer

Structure packages around business capabilities rather than technical layers (controllers, services, repositories).

**Benefits:**

- All related code in one location
- Easier to locate feature-specific logic
- Natural module boundaries for refactoring
- Domain concepts remain visible

**Before (layer-based structure):**

```
com.example.controllers
├── PaymentController.java
├── UserController.java
└── OrderController.java

com.example.services
├── PaymentService.java
├── UserService.java
└── OrderService.java

com.example.repositories
├── PaymentRepository.java
├── UserRepository.java
└── OrderRepository.java
```

**After (feature-based structure):**

```
com.example.payment
├── PaymentController.java
├── PaymentService.java
├── PaymentRepository.java
└── Payment.java

com.example.user
├── UserController.java
├── UserService.java
├── UserRepository.java
└── User.java

com.example.order
├── OrderController.java
├── OrderService.java
├── OrderRepository.java
└── Order.java
```

## Modern Java Features

### Use Switch Expressions (Java 14+)

Replace chains of if-else statements with switch expressions for cleaner, more maintainable code.

**Advantages:**

- Compiler enforces exhaustiveness
- Returns values directly (no mutation needed)
- Clearer intent than if-else chains
- Less repetitive code

**Before (if-else chain):**

```java
public Duration calculateDuration(Frequency frequency, int count) {
    Duration duration;
    if (frequency == Frequency.DAILY) {
        duration = Duration.ofDays(count);
    } else if (frequency == Frequency.WEEKLY) {
        duration = Duration.ofWeeks(count);
    } else if (frequency == Frequency.MONTHLY) {
        duration = Duration.ofDays(count * 30);
    } else if (frequency == Frequency.QUARTERLY) {
        duration = Duration.ofDays(count * 90);
    } else if (frequency == Frequency.ANNUALLY) {
        duration = Duration.ofDays(count * 365);
    } else {
        throw new IllegalArgumentException("Unknown frequency: " + frequency);
    }
    return duration;
}
```

**After (switch expression):**

```java
public Duration calculateDuration(Frequency frequency, int count) {
    return switch (frequency) {
        case DAILY -> Duration.ofDays(count);
        case WEEKLY -> Duration.ofWeeks(count);
        case MONTHLY -> Duration.ofDays(count * 30);
        case QUARTERLY -> Duration.ofDays(count * 90);
        case ANNUALLY -> Duration.ofDays(count * 365);
    };
}
```

### Use Records for Simple Data Carriers (Java 14+)

Records automate boilerplate for simple data carriers (constructor, getters, equals, hashCode, toString).

**When to use records:**

- Simple data transfer objects
- Value objects (immutable by nature)
- Configuration holders
- Return types bundling multiple values

**Before (manual boilerplate):**

```java
public class Money {
    private final BigDecimal amount;
    private final Currency currency;

    public Money(BigDecimal amount, Currency currency) {
        this.amount = amount;
        this.currency = currency;
    }

    public BigDecimal getAmount() { return amount; }
    public Currency getCurrency() { return currency; }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Money money = (Money) o;
        return Objects.equals(amount, money.amount) &&
               Objects.equals(currency, money.currency);
    }

    @Override
    public int hashCode() {
        return Objects.hash(amount, currency);
    }

    @Override
    public String toString() {
        return "Money{amount=" + amount + ", currency=" + currency + "}";
    }
}
```

**After (record):**

```java
public record Money(BigDecimal amount, Currency currency) {
    // Compiler auto-generates constructor, getters, equals, hashCode, toString

    // Can add custom validation
    public Money {
        if (amount == null) {
            throw new IllegalArgumentException("Amount cannot be null");
        }
        if (currency == null) {
            throw new IllegalArgumentException("Currency cannot be null");
        }
    }
}
```

### Use Optional for Potentially Absent Values

Use `Optional<T>` instead of returning null to make absence explicit and prevent NullPointerException.

**Benefits:**

- Explicit in method signature (caller knows to check)
- Compiler enforces handling of absence
- Functional operations (map, filter, orElse)
- Self-documenting code

**Before (null return):**

```java
public User findUserById(String userId) {
    // May return null
    return database.query(userId);
}

// Caller must remember to check
User user = service.findUserById("123");
if (user != null) {
    processUser(user);
}
```

**After (Optional):**

```java
public Optional<User> findUserById(String userId) {
    return Optional.ofNullable(database.query(userId));
}

// Explicit handling required
service.findUserById("123")
    .ifPresent(this::processUser);

// Or with default
User user = service.findUserById("123")
    .orElse(User.guest());

// Or throw exception
User user = service.findUserById("123")
    .orElseThrow(() -> new UserNotFoundException("123"));
```

## Exception Handling

### Handle Exceptions Meaningfully

Never use empty catch blocks. Log exceptions with context, rethrow when appropriate, and provide actionable error messages.

**Exception handling principles:**

- **Catch specific exceptions**: Never use `catch (Exception e)`
- **Log with context**: Include IDs, request details, timestamps
- **Wrap in domain exceptions**: Convert technical to business exceptions
- **Never silent**: Always log or rethrow

**Before (poor exception handling):**

```java
public Contract createContract(Request request) {
    try {
        validateRequest(request);
        return repository.save(contract);
    } catch (Exception e) {
        // Silent failure
    }
    return null;
}
```

**After (meaningful handling):**

```java
public class ContractService {
    private static final Logger logger = LoggerFactory.getLogger(ContractService.class);

    public Contract createContract(Request request) {
        try {
            validateRequest(request);
            Contract contract = contractGenerator.generate(request);
            return repository.save(contract);

        } catch (IllegalArgumentException e) {
            logger.error("Invalid contract request: customerId={}, error={}",
                request.getCustomerId(), e.getMessage());
            throw new ContractValidationException(
                "Cannot create contract: " + e.getMessage(), e);

        } catch (DataAccessException e) {
            logger.error("Database error creating contract: customerId={}",
                request.getCustomerId(), e);
            throw new ContractServiceException(
                "Failed to save contract. Please try again.", e);
        }
    }
}
```

### Use Try-With-Resources for Resource Management

Always use try-with-resources for resources implementing `AutoCloseable` to ensure proper cleanup.

**Why it matters:**

- Guarantees resource closure even with exceptions
- Cleaner code (no manual finally blocks)
- Handles suppressed exceptions correctly
- Prevents resource leaks

**Before (manual management):**

```java
public void writeReport(String filePath, List<Record> records) throws IOException {
    BufferedWriter writer = null;
    try {
        writer = Files.newBufferedWriter(Paths.get(filePath));
        writer.write("Report Header");
        // ... write records
    } finally {
        if (writer != null) {
            writer.close(); // May throw, hiding original exception
        }
    }
}
```

**After (try-with-resources):**

```java
public void writeReport(String filePath, List<Record> records) throws IOException {
    try (BufferedWriter writer = Files.newBufferedWriter(
            Paths.get(filePath),
            StandardCharsets.UTF_8)) {

        writer.write("Report - " + LocalDate.now());
        writer.newLine();

        for (Record record : records) {
            writer.write(record.toString());
            writer.newLine();
        }

        // Writer automatically closed, even if exception occurs
    }
}
```

## Collections and Iteration

### Choose Collections Over Arrays

Use `ArrayList`, `HashSet`, `HashMap` instead of arrays for flexibility, type safety, and rich API support.

**Collections advantages:**

- Dynamic sizing (no fixed capacity)
- Type-safe generics
- Rich API (filter, map, sort, etc.)
- Better integration with Java ecosystem

**Before (arrays):**

```java
public Distribution[] distributeAmount(Money total, Beneficiary[] beneficiaries) {
    if (beneficiaries.length == 0) {
        throw new IllegalArgumentException("Empty array");
    }

    Money perBeneficiary = total.divide(
        BigDecimal.valueOf(beneficiaries.length),
        RoundingMode.HALF_UP
    );

    Distribution[] distributions = new Distribution[beneficiaries.length];
    for (int i = 0; i < beneficiaries.length; i++) {
        distributions[i] = new Distribution(
            beneficiaries[i].getId(),
            perBeneficiary
        );
    }
    return distributions;
}
```

**After (collections):**

```java
public List<Distribution> distributeAmount(Money total, List<Beneficiary> beneficiaries) {
    if (beneficiaries.isEmpty()) {
        throw new IllegalArgumentException("Beneficiary list cannot be empty");
    }

    Money perBeneficiary = total.divide(
        BigDecimal.valueOf(beneficiaries.size()),
        RoundingMode.HALF_UP
    );

    return beneficiaries.stream()
        .map(beneficiary -> new Distribution(
            beneficiary.getId(),
            perBeneficiary
        ))
        .collect(Collectors.toList());
}
```

### Use Lambdas for Functional Interfaces

Replace anonymous inner classes with lambda expressions for cleaner, more readable code.

**Lambda advantages:**

- Concise syntax
- Better readability
- Encourage functional style
- Easier to test

**Before (anonymous classes):**

```java
public List<User> findEligibleUsers(List<User> allUsers, Money threshold) {
    return allUsers.stream()
        .filter(new Predicate<User>() {
            @Override
            public boolean test(User user) {
                return user.getBalance().isGreaterThanOrEqualTo(threshold);
            }
        })
        .collect(Collectors.toList());
}
```

**After (lambdas):**

```java
public List<User> findEligibleUsers(List<User> allUsers, Money threshold) {
    return allUsers.stream()
        .filter(user -> user.getBalance().isGreaterThanOrEqualTo(threshold))
        .filter(User::isActive)
        .collect(Collectors.toList());
}

public Money calculateTotalBalance(List<User> users) {
    return users.stream()
        .map(User::getBalance)
        .reduce(Money.ZERO, Money::add);
}
```

### Loop Efficiently with Streams

Use enhanced for-loops for simple iteration and streams for transformations, filtering, and aggregations.

**When to use each:**

| Pattern           | Best For                                   | Example Use Case          |
| ----------------- | ------------------------------------------ | ------------------------- |
| Enhanced for-loop | Simple iteration with side effects         | Sending notifications     |
| Stream            | Transformation, filtering, aggregation     | Data processing pipelines |
| Parallel stream   | CPU-intensive operations on large datasets | Batch calculations        |

**Enhanced for-loop (simple iteration):**

```java
public void notifyAllUsers(List<User> users, String message) {
    for (User user : users) {
        notificationService.send(user.getEmail(), message);
    }
}
```

**Stream (transformation and filtering):**

```java
public List<Payment> getOverduePayments(List<Payment> allPayments) {
    LocalDate today = LocalDate.now();

    return allPayments.stream()
        .filter(payment -> payment.getDueDate().isBefore(today))
        .filter(payment -> !payment.isPaid())
        .sorted(Comparator.comparing(Payment::getDueDate))
        .collect(Collectors.toList());
}
```

## Control Flow Simplification

### Simplify Nested Conditionals with Guard Clauses

Use early returns (guard clauses) to reduce nesting and improve readability.

**Benefits:**

- Reduces cognitive load
- Flattens code structure
- Makes happy path obvious
- Easier to add new validations

**Before (deep nesting):**

```java
public Money calculateFee(User user, Money threshold) {
    if (user != null) {
        if (user.isActive()) {
            Money balance = user.getBalance();
            if (balance.isGreaterThanOrEqualTo(threshold)) {
                if (user.hasDebt()) {
                    balance = balance.subtract(user.getDebt());
                    if (balance.isGreaterThanOrEqualTo(threshold)) {
                        return balance.multiply(new BigDecimal("0.025"));
                    } else {
                        return Money.ZERO;
                    }
                } else {
                    return balance.multiply(new BigDecimal("0.025"));
                }
            } else {
                return Money.ZERO;
            }
        } else {
            return Money.ZERO;
        }
    } else {
        throw new IllegalArgumentException("User cannot be null");
    }
}
```

**After (guard clauses):**

```java
public Money calculateFee(User user, Money threshold) {
    if (user == null) {
        throw new IllegalArgumentException("User cannot be null");
    }

    if (!user.isActive()) {
        return Money.ZERO;
    }

    Money balance = user.getBalance();
    if (balance.isLessThan(threshold)) {
        return Money.ZERO;
    }

    if (user.hasDebt()) {
        balance = balance.subtract(user.getDebt());
        if (balance.isLessThan(threshold)) {
            return Money.ZERO;
        }
    }

    return balance.multiply(new BigDecimal("0.025"));
}
```

## Immutability Patterns

### Use Final Fields for Immutability

Declare fields as `final` whenever possible to prevent accidental modification and clearly communicate immutability.

**Final field benefits:**

- Compiler enforces initialization
- Thread-safe (no synchronization needed)
- Clear intent (immutable design)
- Easier reasoning about state

**Example:**

```java
public final class Money {
    private final BigDecimal amount;
    private final Currency currency;

    public Money(BigDecimal amount, Currency currency) {
        if (amount == null) {
            throw new IllegalArgumentException("Amount cannot be null");
        }
        if (currency == null) {
            throw new IllegalArgumentException("Currency cannot be null");
        }

        this.amount = amount;
        this.currency = currency;
    }

    public Money add(Money other) {
        validateSameCurrency(other);
        return new Money(this.amount.add(other.amount), this.currency);
    }

    public Money multiply(BigDecimal multiplier) {
        return new Money(this.amount.multiply(multiplier), this.currency);
    }

    // Getters only, no setters
    public BigDecimal getAmount() {
        return amount;
    }

    public Currency getCurrency() {
        return currency;
    }
}
```

### Implement Defensive Copying

When accepting or returning mutable objects, create copies to prevent external modification.

**When defensive copying needed:**

- Constructor parameters (mutable collections, dates)
- Getter methods returning mutable fields
- Maintaining class invariants
- Security-sensitive data

**Before (exposed mutable state):**

```java
public class Account {
    private final List<Transaction> transactions;

    public Account(List<Transaction> transactions) {
        this.transactions = transactions; // Caller can modify
    }

    public List<Transaction> getTransactions() {
        return transactions; // Caller can modify
    }
}
```

**After (defensive copying):**

```java
public class Account {
    private final List<Transaction> transactions;

    public Account(List<Transaction> transactions) {
        this.transactions = new ArrayList<>(transactions); // Copy
    }

    public List<Transaction> getTransactions() {
        return new ArrayList<>(transactions); // Return copy
    }

    // Or return unmodifiable view
    public List<Transaction> getTransactionsView() {
        return Collections.unmodifiableList(transactions);
    }
}
```

## Dependency Management

### Use Dependency Injection

Use dependency injection instead of creating dependencies internally or using static access.

**Dependency injection benefits:**

- Testability (inject mocks/stubs)
- Flexibility (swap implementations)
- Explicit dependencies (visible in constructor)
- Loose coupling

**Before (hard-coded dependencies):**

```java
public class PaymentService {
    public void processPayment(String userId, Money amount) {
        Money fee = FeeCalculator.calculate(amount); // Static dependency
        DatabaseHelper.save(new PaymentRecord(userId, fee)); // Hidden coupling
        NotificationManager.notify(userId, fee); // Hard to test
    }
}
```

**After (dependency injection):**

```java
public class PaymentService {
    private final FeeCalculator calculator;
    private final PaymentRepository repository;
    private final NotificationService notificationService;

    public PaymentService(
            FeeCalculator calculator,
            PaymentRepository repository,
            NotificationService notificationService) {
        this.calculator = calculator;
        this.repository = repository;
        this.notificationService = notificationService;
    }

    public void processPayment(String userId, Money amount) {
        Money fee = calculator.calculate(amount);
        repository.save(new PaymentRecord(userId, fee));
        notificationService.notifyUser(userId, fee);
    }
}
```

### Separate Configuration from Code

Externalize configuration to properties files, environment variables, or configuration services.

**Configuration separation benefits:**

- Environment-specific settings (dev, staging, prod)
- No recompilation for config changes
- Centralized configuration management
- Security (credentials outside code)

**Before (hardcoded config):**

```java
public class DatabaseConnector {
    private static final String DB_URL = "jdbc:postgresql://localhost:5432/mydb";
    private static final String DB_USER = "admin";
    private static final String DB_PASS = "password123"; // Security risk!

    public Connection connect() {
        return DriverManager.getConnection(DB_URL, DB_USER, DB_PASS);
    }
}
```

**After (externalized config):**

```java
// application.properties
database.url=jdbc:postgresql://localhost:5432/mydb
database.user=${DB_USER}
database.password=${DB_PASSWORD}

// Configuration class
@Configuration
public class DatabaseConfig {
    @Value("${database.url}")
    private String dbUrl;

    @Value("${database.user}")
    private String dbUser;

    @Value("${database.password}")
    private String dbPassword;

    @Bean
    public DataSource dataSource() {
        return DataSourceBuilder.create()
            .url(dbUrl)
            .username(dbUser)
            .password(dbPassword)
            .build();
    }
}
```

## Testing Best Practices

### Write Comprehensive, Well-Named Tests

Tests should document expected behavior through descriptive names and comprehensive coverage.

**Test naming patterns:**

- `should[ExpectedBehavior]When[Condition]`
- `[MethodName]_[Scenario]_[ExpectedResult]`
- Given-When-Then structure in test body

**Example:**

```java
public class MoneyCalculatorTest {

    private MoneyCalculator calculator;

    @BeforeEach
    void setUp() {
        calculator = new MoneyCalculator();
    }

    @Test
    void shouldCalculateFeeAt2Point5PercentForEligibleAmount() {
        // Given
        Money amount = Money.ofUSD(new BigDecimal("100000"));
        Money expectedFee = Money.ofUSD(new BigDecimal("2500"));

        // When
        Money actualFee = calculator.calculateFee(amount);

        // Then
        assertEquals(expectedFee, actualFee);
    }

    @Test
    void shouldReturnZeroWhenAmountBelowThreshold() {
        // Given
        Money belowThreshold = Money.ofUSD(new BigDecimal("100"));
        Money threshold = Money.ofUSD(new BigDecimal("1000"));

        // When
        Money fee = calculator.calculateFee(belowThreshold, threshold);

        // Then
        assertEquals(Money.ZERO_USD, fee);
    }

    @Test
    void shouldThrowExceptionWhenAmountIsNull() {
        // When/Then
        assertThrows(
            IllegalArgumentException.class,
            () -> calculator.calculateFee(null)
        );
    }

    @Test
    void shouldThrowExceptionWithDescriptiveMessageWhenAmountIsNegative() {
        // Given
        Money negativeAmount = Money.ofUSD(new BigDecimal("-100"));

        // When/Then
        IllegalArgumentException exception = assertThrows(
            IllegalArgumentException.class,
            () -> calculator.calculateFee(negativeAmount)
        );

        assertTrue(exception.getMessage().contains("Amount cannot be negative"));
    }
}
```

## Logging Best Practices

### Use Proper Logging Levels

Choose appropriate logging levels (TRACE, DEBUG, INFO, WARN, ERROR) and include contextual information.

**Logging level guidelines:**

| Level | Purpose                                    | Example Use Case                       |
| ----- | ------------------------------------------ | -------------------------------------- |
| ERROR | System errors requiring attention          | Database connection failures           |
| WARN  | Unexpected but handled situations          | Validation failures, retries           |
| INFO  | Significant business events                | User registration, payment processing  |
| DEBUG | Detailed diagnostic information            | Method parameters, intermediate values |
| TRACE | Very detailed debugging (usually disabled) | Loop iterations, fine-grained flow     |

**Example:**

```java
public class PaymentService {
    private static final Logger logger = LoggerFactory.getLogger(PaymentService.class);

    public Payment createPayment(PaymentRequest request) {
        logger.info("Creating payment for customer: {}",
            request.getCustomerId());

        logger.debug("Payment details - Amount: {}, Currency: {}",
            request.getAmount(),
            request.getCurrency());

        try {
            validateRequest(request);

            Payment payment = paymentProcessor.process(request);
            Payment savedPayment = repository.save(payment);

            logger.info("Successfully created payment: paymentId={}, customerId={}",
                savedPayment.getId(),
                savedPayment.getCustomerId());

            return savedPayment;

        } catch (IllegalArgumentException e) {
            logger.warn("Invalid payment request: customerId={}, reason={}",
                request.getCustomerId(),
                e.getMessage());
            throw new PaymentValidationException(
                "Cannot create payment: " + e.getMessage(), e);

        } catch (DataAccessException e) {
            logger.error("Database error while creating payment: customerId={}",
                request.getCustomerId(), e);
            throw new PaymentServiceException(
                "Failed to save payment. Please try again.", e);

        } catch (Exception e) {
            logger.error("Unexpected error creating payment: customerId={}",
                request.getCustomerId(), e);
            throw new PaymentServiceException(
                "An unexpected error occurred. Please contact support.", e);
        }
    }
}
```

## Input Validation

### Validate Input at Boundaries

Validate all external inputs (API requests, user input, external service responses) at system boundaries.

**Validation strategies:**

- Bean Validation annotations (@NotNull, @Positive, @Size)
- Custom validators for complex business rules
- Early validation (fail fast)
- Meaningful error messages

**Example:**

```java
// Request DTO with validation annotations
public class PaymentRequest {

    @NotBlank(message = "Customer ID is required")
    private String customerId;

    @NotNull(message = "Amount is required")
    @Positive(message = "Amount must be positive")
    private BigDecimal amount;

    @NotNull(message = "Currency is required")
    @Pattern(regexp = "^[A-Z]{3}$", message = "Currency must be valid 3-letter ISO code")
    private String currency;

    @NotNull(message = "Payment date is required")
    @FutureOrPresent(message = "Payment date cannot be in the past")
    private LocalDate paymentDate;

    // Getters and setters
}

// Controller with validation
@RestController
@RequestMapping("/api/v1/payments")
public class PaymentController {
    private final PaymentService service;

    @PostMapping
    public ResponseEntity<PaymentResponse> createPayment(
            @Valid @RequestBody PaymentRequest request) {

        Payment payment = service.createPayment(request.toDomain());

        return ResponseEntity
            .status(HttpStatus.CREATED)
            .body(PaymentResponse.from(payment));
    }
}

// Custom validator for complex rules
@Constraint(validatedBy = ValidPaymentRequestValidator.class)
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ValidPaymentRequest {
    String message() default "Invalid payment request";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}

public class ValidPaymentRequestValidator
        implements ConstraintValidator<ValidPaymentRequest, PaymentRequest> {

    @Override
    public boolean isValid(
            PaymentRequest request,
            ConstraintValidatorContext context) {

        if (request == null) {
            return false;
        }

        // Business rule: Amount must not exceed daily limit
        if (request.getAmount().compareTo(new BigDecimal("10000")) > 0) {
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(
                "Amount cannot exceed daily limit of 10,000")
                .addPropertyNode("amount")
                .addConstraintViolation();
            return false;
        }

        return true;
    }
}
```

## Type Selection

### Use Appropriate Data Types

Choose data types that accurately represent domain concepts.

**Type selection guidelines:**

| Domain Concept   | Recommended Type           | Avoid                            |
| ---------------- | -------------------------- | -------------------------------- |
| Money amounts    | `BigDecimal`               | `double`, `float`                |
| Dates            | `LocalDate`                | `java.util.Date`, `long`         |
| Timestamps       | `Instant`, `ZonedDateTime` | `long`, `java.util.Date`         |
| Fixed value sets | `enum`                     | `String`, `int`                  |
| Whole numbers    | `int`, `long`              | `BigInteger` (unless very large) |
| Optional values  | `Optional<T>`              | `null`                           |

**Example:**

```java
public class Transaction {
    private final String transactionId;
    private final String customerId;
    private final BigDecimal amount;           // BigDecimal for precise money
    private final Currency currency;            // Proper currency type
    private final LocalDate transactionDate;   // LocalDate for dates
    private final Instant timestamp;           // Instant for UTC timestamps
    private final TransactionStatus status;    // Enum for fixed set
    private final TransactionType type;        // Enum for fixed set

    // Constructor, methods
}

public enum TransactionStatus {
    PENDING,
    PROCESSING,
    COMPLETED,
    FAILED,
    CANCELLED
}

public enum TransactionType {
    DEPOSIT,
    WITHDRAWAL,
    TRANSFER,
    FEE
}
```

## Access Control

### Use Appropriate Access Modifiers

Apply the principle of least privilege to class members.

**Access modifier guidelines:**

| Modifier          | Visibility           | When to Use                                 |
| ----------------- | -------------------- | ------------------------------------------- |
| `private`         | Class only           | Default for fields, internal helper methods |
| `package-private` | Package              | Package-internal APIs                       |
| `protected`       | Package + subclasses | Designed for inheritance                    |
| `public`          | Everywhere           | Public APIs, designed for external use      |

**Example:**

```java
public class Account {
    // Private fields (encapsulation)
    private final String accountId;
    private final String customerId;
    private Money balance;

    // Public constructor (external creation)
    public Account(String accountId, String customerId, Money initialBalance) {
        this.accountId = accountId;
        this.customerId = customerId;
        this.balance = initialBalance;
    }

    // Public methods (external API)
    public void deposit(Money amount) {
        validatePositiveAmount(amount);
        this.balance = this.balance.add(amount);
        recordTransaction(TransactionType.DEPOSIT, amount);
    }

    public Money getBalance() {
        return balance;
    }

    // Private helper methods (internal only)
    private void validatePositiveAmount(Money amount) {
        if (amount.isNegativeOrZero()) {
            throw new IllegalArgumentException("Amount must be positive");
        }
    }

    private void recordTransaction(TransactionType type, Money amount) {
        // Internal transaction recording logic
    }

    // Package-private for testing
    void resetBalance() {
        this.balance = Money.ZERO;
    }
}
```

## Summary

Best practices emerge from collective experience addressing common challenges. Key themes:

**Clarity and Simplicity**:

- Write clear code over clever code
- Keep methods small and focused
- Use intention-revealing names

**Immutability and Safety**:

- Prefer immutable objects
- Use final fields
- Implement defensive copying

**Modern Java Features**:

- Switch expressions for decision logic
- Records for simple data carriers
- Optional for potentially absent values
- Streams for data transformation

**Robust Error Handling**:

- Validate at boundaries
- Fail fast with meaningful exceptions
- Use try-with-resources
- Log with appropriate levels and context

**Maintainable Design**:

- Composition over inheritance
- Dependency injection
- Separate configuration from code
- Organize by feature, not layer

**Quality Assurance**:

- Comprehensive, well-named tests
- Appropriate data types
- Proper access modifiers
- Defensive programming

Applying these practices systematically leads to codebases that are easier to understand, modify, and maintain over their lifetime.
