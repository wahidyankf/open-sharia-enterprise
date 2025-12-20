---
title: "Common Java Anti-Patterns"
date: 2025-12-17T07:06:48+07:00
draft: false
weight: 1000040
description: "Recognizing and avoiding common Java anti-patterns that lead to unmaintainable code"
tags: ["java", "anti-patterns", "code-smells", "refactoring"]
categories: ["learn"]
---

## Overview

Anti-patterns are common solutions to recurring problems that appear helpful initially but create more problems than they solve. Recognizing these patterns helps you avoid technical debt and maintain code quality.

## Structural Anti-Patterns

### God Object (God Class)

A class that knows or does too much, violating Single Responsibility Principle.

**Why it's bad:**

- Difficult to understand (hundreds or thousands of lines)
- Hard to test (too many dependencies)
- Fragile (changes ripple unpredictably)
- Prevents reuse (coupling to everything)
- Merge conflicts (everyone touches the same file)

**Example:**

```java
// ❌ God Object - does everything
public class OrderManager {
  private DatabaseConnection db;
  private EmailService email;
  private PaymentGateway payment;
  private InventorySystem inventory;
  private ShippingService shipping;
  private TaxCalculator tax;
  private DiscountEngine discount;
  private NotificationService notifier;

  // Order validation
  public boolean validateOrder(Order order) { /* ... */ }

  // Payment processing
  public PaymentResult processPayment(Order order) { /* ... */ }

  // Inventory management
  public void updateInventory(Order order) { /* ... */ }

  // Shipping coordination
  public void arrangeShipping(Order order) { /* ... */ }

  // Tax calculation
  public BigDecimal calculateTax(Order order) { /* ... */ }

  // Discount application
  public BigDecimal applyDiscounts(Order order) { /* ... */ }

  // Email sending
  public void sendOrderConfirmation(Order order) { /* ... */ }

  // Database operations
  public void saveOrder(Order order) { /* ... */ }
  public Order loadOrder(String id) { /* ... */ }

  // Reporting
  public Report generateSalesReport() { /* ... */ }

  // And 50 more methods...
}
```

**Solution:**

```java
// ✅ Separated responsibilities
public class OrderService {
  private final OrderValidator validator;
  private final PaymentProcessor paymentProcessor;
  private final InventoryManager inventoryManager;
  private final OrderRepository repository;
  private final OrderNotifier notifier;

  public OrderService(
    OrderValidator validator,
    PaymentProcessor paymentProcessor,
    InventoryManager inventoryManager,
    OrderRepository repository,
    OrderNotifier notifier
  ) {
    this.validator = validator;
    this.paymentProcessor = paymentProcessor;
    this.inventoryManager = inventoryManager;
    this.repository = repository;
    this.notifier = notifier;
  }

  public void processOrder(Order order) {
    validator.validate(order);
    paymentProcessor.process(order);
    inventoryManager.reserve(order);
    repository.save(order);
    notifier.sendConfirmation(order);
  }
}
```

**How to identify:**

- Class has more than 500 lines
- Class has more than 10 dependencies
- Multiple developers frequently modify same class
- Class name is vague (Manager, Handler, Util, Service)

### Magic Numbers and Strings

Hard-coded literal values scattered throughout code without explanation.

**Why it's bad:**

- Unclear meaning (what does 86400 represent?)
- Duplication (same value repeated everywhere)
- Error-prone changes (miss one occurrence)
- Difficult to maintain

**Example:**

```java
// ❌ Magic numbers everywhere
public class SubscriptionService {
  public boolean isExpired(LocalDateTime subscribedAt) {
    return subscribedAt.plusSeconds(2592000).isBefore(LocalDateTime.now());
  }

  public BigDecimal calculateFee(String tier) {
    if (tier.equals("basic")) {
      return new BigDecimal("9.99");
    } else if (tier.equals("premium")) {
      return new BigDecimal("19.99");
    } else if (tier.equals("enterprise")) {
      return new BigDecimal("49.99");
    }
    return BigDecimal.ZERO;
  }

  public int getMaxProjects(String tier) {
    if (tier.equals("basic")) {
      return 3;
    } else if (tier.equals("premium")) {
      return 10;
    } else {
      return 100;
    }
  }
}
```

**Solution:**

```java
// ✅ Named constants with clear meaning
public class SubscriptionService {
  private static final long SUBSCRIPTION_DURATION_SECONDS = 30L * 24 * 60 * 60; // 30 days

  public boolean isExpired(LocalDateTime subscribedAt) {
    return subscribedAt.plusSeconds(SUBSCRIPTION_DURATION_SECONDS)
      .isBefore(LocalDateTime.now());
  }
}

// Even better: Use enums for related constants
public enum SubscriptionTier {
  BASIC(new BigDecimal("9.99"), 3),
  PREMIUM(new BigDecimal("19.99"), 10),
  ENTERPRISE(new BigDecimal("49.99"), 100);

  private final BigDecimal monthlyFee;
  private final int maxProjects;

  SubscriptionTier(BigDecimal monthlyFee, int maxProjects) {
    this.monthlyFee = monthlyFee;
    this.maxProjects = maxProjects;
  }

  public BigDecimal getMonthlyFee() { return monthlyFee; }
  public int getMaxProjects() { return maxProjects; }
}

// Usage
SubscriptionTier tier = SubscriptionTier.PREMIUM;
BigDecimal fee = tier.getMonthlyFee();
int projects = tier.getMaxProjects();
```

### Tight Coupling

Classes directly depend on concrete implementations rather than abstractions.

**Why it's bad:**

- Cannot change implementations without modifying clients
- Difficult to test (cannot mock dependencies)
- Reduces reusability
- Creates fragile systems

**Example:**

```java
// ❌ Tightly coupled to concrete implementations
public class UserService {
  private MySQLUserRepository repository;
  private SendGridEmailService emailService;
  private StripePaymentProcessor paymentProcessor;

  public UserService() {
    // Hard-coded dependencies
    this.repository = new MySQLUserRepository();
    this.emailService = new SendGridEmailService();
    this.paymentProcessor = new StripePaymentProcessor();
  }

  public void registerUser(User user) {
    repository.save(user); // Cannot test without real database
    emailService.sendWelcomeEmail(user.getEmail()); // Cannot test without SendGrid
    paymentProcessor.createAccount(user); // Cannot test without Stripe
  }
}
```

**Solution:**

```java
// ✅ Loosely coupled via interfaces
public interface UserRepository {
  void save(User user);
}

public interface EmailService {
  void sendWelcomeEmail(String email);
}

public interface PaymentProcessor {
  void createAccount(User user);
}

public class UserService {
  private final UserRepository repository;
  private final EmailService emailService;
  private final PaymentProcessor paymentProcessor;

  public UserService(
    UserRepository repository,
    EmailService emailService,
    PaymentProcessor paymentProcessor
  ) {
    this.repository = repository;
    this.emailService = emailService;
    this.paymentProcessor = paymentProcessor;
  }

  public void registerUser(User user) {
    repository.save(user);
    emailService.sendWelcomeEmail(user.getEmail());
    paymentProcessor.createAccount(user);
  }
}

// Easy to test
@Test
void shouldRegisterUser() {
  UserRepository mockRepo = mock(UserRepository.class);
  EmailService mockEmail = mock(EmailService.class);
  PaymentProcessor mockPayment = mock(PaymentProcessor.class);

  UserService service = new UserService(mockRepo, mockEmail, mockPayment);
  service.registerUser(testUser);

  verify(mockRepo).save(testUser);
  verify(mockEmail).sendWelcomeEmail(testUser.getEmail());
}
```

## Error Handling Anti-Patterns

### Swallowing Exceptions

Catching exceptions without handling them or logging.

**Why it's bad:**

- Errors fail silently
- Impossible to debug
- Corrupted state continues undetected
- Users see no error feedback

**Example:**

```java
// ❌ Swallowed exception - error disappears
public User loadUser(String userId) {
  try {
    return userRepository.findById(userId);
  } catch (DatabaseException e) {
    // Silent failure - caller thinks everything is fine
  }
  return null;
}

// ❌ Worse: Exception as control flow
public boolean userExists(String userId) {
  try {
    User user = userRepository.findById(userId);
    return true;
  } catch (UserNotFoundException e) {
    return false; // Expected case, not exceptional
  }
}
```

**Solution:**

```java
// ✅ Proper exception handling
public User loadUser(String userId) {
  try {
    return userRepository.findById(userId);
  } catch (DatabaseException e) {
    logger.error("Failed to load user: " + userId, e);
    throw new UserServiceException("Unable to load user data", e);
  }
}

// ✅ Return Optional for normal cases
public Optional<User> findUser(String userId) {
  return userRepository.findById(userId); // Returns Optional
}

// Usage
findUser(userId)
  .ifPresentOrElse(
    user -> processUser(user),
    () -> handleMissingUser()
  );
```

### Exception Overuse

Using checked exceptions for every possible error condition.

**Why it's bad:**

- Pollutes method signatures with throws clauses
- Forces callers to handle exceptions they cannot recover from
- Encourages empty catch blocks
- Violates fail-fast principle

**Example:**

```java
// ❌ Excessive checked exceptions
public class UserValidator {
  public void validateEmail(String email)
    throws InvalidEmailException,
           EmptyEmailException,
           TooLongEmailException,
           InvalidDomainException { /* ... */ }

  public void validatePassword(String password)
    throws TooShortPasswordException,
           TooLongPasswordException,
           NoUppercaseException,
           NoNumberException,
           NoSpecialCharException { /* ... */ }
}

// Caller forced to handle every case
try {
  validator.validateEmail(email);
  validator.validatePassword(password);
} catch (InvalidEmailException | EmptyEmailException |
         TooLongEmailException | InvalidDomainException |
         TooShortPasswordException | TooLongPasswordException |
         NoUppercaseException | NoNumberException |
         NoSpecialCharException e) {
  // Most callers just give up
  throw new RuntimeException(e);
}
```

**Solution:**

```java
// ✅ Use validation result object
public class ValidationResult {
  private final boolean valid;
  private final List<String> errors;

  public static ValidationResult success() {
    return new ValidationResult(true, Collections.emptyList());
  }

  public static ValidationResult failure(String... errors) {
    return new ValidationResult(false, Arrays.asList(errors));
  }

  // Getters...
}

public class UserValidator {
  public ValidationResult validateEmail(String email) {
    List<String> errors = new ArrayList<>();

    if (email == null || email.isEmpty()) {
      errors.add("Email cannot be empty");
    }
    if (email.length() > 255) {
      errors.add("Email too long");
    }
    if (!email.contains("@")) {
      errors.add("Invalid email format");
    }

    return errors.isEmpty()
      ? ValidationResult.success()
      : ValidationResult.failure(errors.toArray(new String[0]));
  }
}

// Clean usage
ValidationResult result = validator.validateEmail(email);
if (!result.isValid()) {
  return ResponseEntity.badRequest()
    .body(new ErrorResponse(result.getErrors()));
}
```

## Performance Anti-Patterns

### Premature Optimization

Optimizing code before measuring actual performance problems.

**Why it's bad:**

- Wastes development resources
- Makes code harder to understand
- Often optimizes wrong things
- Prevents focusing on real bottlenecks

**Example:**

```java
// ❌ Premature optimization - complex and hard to read
public class CacheEverythingUserService {
  private Map<String, User> cache = new ConcurrentHashMap<>();
  private Map<String, Long> accessTimes = new ConcurrentHashMap<>();
  private ScheduledExecutorService cleaner = Executors.newScheduledThreadPool(1);

  public CacheEverythingUserService() {
    // Complex cache eviction logic before any performance problem found
    cleaner.scheduleAtFixedRate(() -> {
      long now = System.currentTimeMillis();
      accessTimes.entrySet().stream()
        .filter(e -> now - e.getValue() > 60000)
        .map(Map.Entry::getKey)
        .forEach(key -> {
          cache.remove(key);
          accessTimes.remove(key);
        });
    }, 1, 1, TimeUnit.MINUTES);
  }

  public User getUser(String id) {
    // Application handles 10 requests per day, caching unnecessary
    User user = cache.get(id);
    if (user == null) {
      user = loadUserFromDatabase(id);
      cache.put(id, user);
    }
    accessTimes.put(id, System.currentTimeMillis());
    return user;
  }
}
```

**Solution:**

```java
// ✅ Start simple, optimize when needed
public class UserService {
  private final UserRepository repository;

  public UserService(UserRepository repository) {
    this.repository = repository;
  }

  public User getUser(String id) {
    return repository.findById(id)
      .orElseThrow(() -> new UserNotFoundException(id));
  }
}

// If profiling shows database is bottleneck, THEN add caching
// If profiling shows database is NOT bottleneck, keep it simple
```

### N+1 Query Problem

Executing one query to fetch entities, then N additional queries to fetch related data.

**Why it's bad:**

- Multiplies database roundtrips
- Kills performance as data grows
- Easy to miss in development (small datasets)
- Appears suddenly in production

**Example:**

```java
// ❌ N+1 queries - executes 1 + N queries
public List<OrderSummary> getOrderSummaries() {
  List<Order> orders = orderRepository.findAll(); // 1 query

  return orders.stream()
    .map(order -> {
      // N queries - one per order
      Customer customer = customerRepository.findById(order.getCustomerId());
      return new OrderSummary(order, customer);
    })
    .collect(Collectors.toList());
}
```

**Solution:**

```java
// ✅ Single query with join
public List<OrderSummary> getOrderSummaries() {
  // 1 query with JOIN to fetch orders and customers together
  return orderRepository.findAllWithCustomers();
}

// In repository
@Query("SELECT o FROM Order o JOIN FETCH o.customer")
List<Order> findAllWithCustomers();

// Or use batching
public List<OrderSummary> getOrderSummaries() {
  List<Order> orders = orderRepository.findAll();

  // Fetch all customers in one query
  Set<String> customerIds = orders.stream()
    .map(Order::getCustomerId)
    .collect(Collectors.toSet());

  Map<String, Customer> customers = customerRepository
    .findAllById(customerIds)
    .stream()
    .collect(Collectors.toMap(Customer::getId, Function.identity()));

  // Combine in memory
  return orders.stream()
    .map(order -> new OrderSummary(order, customers.get(order.getCustomerId())))
    .collect(Collectors.toList());
}
```

## Design Anti-Patterns

### Singletons Abuse

Overusing singleton pattern for global state management.

**Why it's bad:**

- Hidden dependencies (appears nowhere in signatures)
- Difficult to test (global mutable state)
- Thread-safety issues
- Violates Single Responsibility Principle
- Tight coupling

**Example:**

```java
// ❌ Singleton abuse - global mutable state
public class ApplicationConfig {
  private static ApplicationConfig instance;
  private Map<String, String> settings = new HashMap<>();

  private ApplicationConfig() {}

  public static ApplicationConfig getInstance() {
    if (instance == null) {
      instance = new ApplicationConfig();
    }
    return instance;
  }

  public void set(String key, String value) {
    settings.put(key, value);
  }

  public String get(String key) {
    return settings.get(key);
  }
}

// Hidden dependency, global state
public class PaymentService {
  public void processPayment(Payment payment) {
    // Hidden dependency - not obvious from method signature
    String apiKey = ApplicationConfig.getInstance().get("payment.api.key");
    // ...
  }
}
```

**Solution:**

```java
// ✅ Dependency injection - explicit dependencies
public class ApplicationConfig {
  private final Map<String, String> settings;

  public ApplicationConfig(Map<String, String> settings) {
    this.settings = new HashMap<>(settings);
  }

  public String get(String key) {
    return settings.get(key);
  }
}

public class PaymentService {
  private final String apiKey;

  // Explicit dependency - visible in constructor
  public PaymentService(ApplicationConfig config) {
    this.apiKey = config.get("payment.api.key");
  }

  public void processPayment(Payment payment) {
    // Use apiKey
  }
}

// Easy to test
@Test
void shouldProcessPayment() {
  Map<String, String> testConfig = new HashMap<>();
  testConfig.put("payment.api.key", "test-key");
  ApplicationConfig config = new ApplicationConfig(testConfig);

  PaymentService service = new PaymentService(config);
  service.processPayment(testPayment);
}
```

### Anemic Domain Model

Domain objects with no behavior, only getters and setters.

**Why it's bad:**

- Business logic scattered across service classes
- Violates encapsulation
- Duplicated validation logic
- Difficult to maintain invariants

**Example:**

```java
// ❌ Anemic model - just data, no behavior
public class BankAccount {
  private BigDecimal balance;
  private String accountNumber;

  public BigDecimal getBalance() { return balance; }
  public void setBalance(BigDecimal balance) { this.balance = balance; }
  public String getAccountNumber() { return accountNumber; }
  public void setAccountNumber(String accountNumber) {
    this.accountNumber = accountNumber;
  }
}

// Business logic in service layer
public class BankAccountService {
  public void withdraw(BankAccount account, BigDecimal amount) {
    if (amount.compareTo(BigDecimal.ZERO) <= 0) {
      throw new IllegalArgumentException("Amount must be positive");
    }
    if (account.getBalance().compareTo(amount) < 0) {
      throw new InsufficientFundsException();
    }
    account.setBalance(account.getBalance().subtract(amount));
  }

  public void deposit(BankAccount account, BigDecimal amount) {
    if (amount.compareTo(BigDecimal.ZERO) <= 0) {
      throw new IllegalArgumentException("Amount must be positive");
    }
    account.setBalance(account.getBalance().add(amount));
  }
}
```

**Solution:**

```java
// ✅ Rich domain model - encapsulates behavior
public class BankAccount {
  private BigDecimal balance;
  private final String accountNumber;

  public BankAccount(String accountNumber, BigDecimal initialBalance) {
    this.accountNumber = Objects.requireNonNull(accountNumber);
    this.balance = Objects.requireNonNull(initialBalance);

    if (initialBalance.compareTo(BigDecimal.ZERO) < 0) {
      throw new IllegalArgumentException("Initial balance cannot be negative");
    }
  }

  public void withdraw(BigDecimal amount) {
    validateAmount(amount);

    if (balance.compareTo(amount) < 0) {
      throw new InsufficientFundsException(
        "Cannot withdraw " + amount + " from balance " + balance
      );
    }

    balance = balance.subtract(amount);
  }

  public void deposit(BigDecimal amount) {
    validateAmount(amount);
    balance = balance.add(amount);
  }

  private void validateAmount(BigDecimal amount) {
    if (amount.compareTo(BigDecimal.ZERO) <= 0) {
      throw new IllegalArgumentException("Amount must be positive");
    }
  }

  public BigDecimal getBalance() { return balance; }
  public String getAccountNumber() { return accountNumber; }
}

// Service layer becomes thin orchestration
public class BankAccountService {
  private final AccountRepository repository;

  public void transferFunds(String fromId, String toId, BigDecimal amount) {
    BankAccount from = repository.findById(fromId);
    BankAccount to = repository.findById(toId);

    from.withdraw(amount); // Domain object enforces rules
    to.deposit(amount);

    repository.save(from);
    repository.save(to);
  }
}
```

## Recognition and Prevention

### How to Spot Anti-Patterns

Anti-patterns rarely announce themselves with clear warning labels. Instead, they reveal themselves through symptoms that make development painful. Pay attention to code smells - those gut feelings that something isn't quite right. Long methods that scroll off the screen, classes with thousands of lines, blocks of identical or nearly identical code scattered throughout the codebase - these are your canaries in the coal mine.

Testing difficulty provides another strong signal. When you cannot test a component without spinning up the entire system, tight coupling has taken over. When you need to mock dozens of dependencies just to test simple logic, your class is doing too much. These struggles during testing reflect deeper structural problems in your design.

Watch for areas of code that repeatedly break. If the same components keep appearing in bug reports despite fixes, anti-patterns are making that code inherently fragile. Similarly, if developers fear touching certain parts of the codebase because changes unpredictably break distant features, you're dealing with the ripple effects of tight coupling or god objects.

Communication overhead signals architectural rot. When understanding a single feature requires meetings with multiple people because the logic is scattered across dozens of classes, or when onboarding new developers takes weeks because the code doesn't reveal its structure, anti-patterns have made your codebase incomprehensible.

### Prevention Strategies

Prevention starts with code reviews that catch anti-patterns before they enter the codebase. Train your team to recognize these patterns and provide feedback when they appear in pull requests. Automated analysis tools like SonarQube and PMD act as tireless reviewers, flagging complexity metrics and code smells that humans might miss.

Embrace continuous refactoring as a core practice. Small issues are easy to fix - large problems are intimidating. When you notice a method growing too large, extract smaller methods immediately rather than waiting until it reaches hundreds of lines. This incremental approach prevents anti-patterns from taking root.

Share knowledge regularly through team discussions about patterns and anti-patterns. When someone discovers a particularly elegant solution or falls into an anti-pattern trap, make it a teaching moment for the whole team. This builds collective wisdom faster than individual learning.

Finally, document your architectural decisions. When you choose to deviate from standard patterns for good reasons, write down why. This prevents future developers from "fixing" intentional decisions or repeating experiments that didn't work. Documentation creates institutional memory that survives team turnover.

## Summary

Anti-patterns represent solutions that seem reasonable initially but create mounting problems over time. The technical debt they introduce compounds with interest - the longer they persist, the harder they become to fix and the more damage they cause to development velocity and system reliability.

Structural anti-patterns like god objects, magic numbers, and tight coupling attack your codebase's organization. God objects centralize too much knowledge and responsibility, becoming bottlenecks that everyone must work around. Magic numbers hide meaning throughout your code, making changes error-prone when you inevitably need to update that scattered value. Tight coupling weaves components together so thoroughly that you cannot change or test them independently.

Error handling anti-patterns sabotage your ability to debug and recover from problems. Swallowed exceptions make errors vanish silently, turning straightforward bugs into mysteries that consume hours of investigation. Exception overuse pollutes your API with checked exceptions that provide no value, encouraging developers to write empty catch blocks just to satisfy the compiler.

Performance anti-patterns waste resources on the wrong problems. Premature optimization adds complexity before you understand what actually needs optimizing, often optimizing parts of the system that aren't bottlenecks. The N+1 query problem multiplies database roundtrips, creating performance disasters that only appear when data volumes grow beyond development environments.

Design anti-patterns undermine your architecture's flexibility. Singleton abuse creates global mutable state with hidden dependencies that make testing and reasoning about code difficult. Anemic domain models push business logic into service layers, violating encapsulation and scattering related logic across multiple files.

The key to managing anti-patterns lies in recognition and early intervention. Learn to spot their warning signs before they metastasize throughout your codebase. Refactor proactively when you see them forming rather than waiting until they're entrenched. This vigilance keeps technical debt manageable and maintains the health of your codebase over its lifetime.

## Related Content

- [Java Best Practices and Design Principles](/en/learn/swe/prog-lang/java/explanation/best-practices)
- [How to Refactor God Classes](/en/learn/swe/prog-lang/java/how-to/refactor-god-classes)
- [How to Avoid NullPointerException](/en/learn/swe/prog-lang/java/how-to/avoid-nullpointerexception)
- [How to Implement Proper Exception Handling](/en/learn/swe/prog-lang/java/how-to/exception-handling)
