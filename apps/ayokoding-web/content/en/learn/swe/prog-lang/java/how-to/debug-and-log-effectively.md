---
title: "How to Debug and Log Effectively"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000120
description: "Master debugging techniques and structured logging with SLF4J and Logback"
tags: ["java", "debugging", "logging", "slf4j", "logback"]
categories: ["learn"]
---

## Problem

Debugging without proper tools wastes hours on issues that debuggers solve in minutes. Poor logging creates noise instead of insights, making production issues harder to diagnose. The challenge is knowing which debugging approach fits each situation and how to log effectively without overwhelming logs.

This guide shows effective debugging and logging practices in Java.

## Using the Debugger

### Breakpoints and Stepping

```java
public class OrderProcessor {
  public OrderResult processOrder(Order order) {
    // Set breakpoint here (click line number in IDE)
    validateOrder(order);

    // Step Over (F8) - executes method without entering
    BigDecimal total = calculateTotal(order);

    // Step Into (F7) - enters method to debug internals
    PaymentResult payment = processPayment(order, total);

    // Step Out (Shift+F8) - completes method and returns to caller
    if (!payment.isSuccess()) {
      return OrderResult.failed(payment.getReason());
    }

    return OrderResult.success(order.getId());
  }
}
```

**Debugger controls:**

- **Breakpoint**: Pause execution at specific line
- **Step Over**: Execute current line, don't enter method calls
- **Step Into**: Enter method call to debug its internals
- **Step Out**: Complete current method and return to caller
- **Resume**: Continue execution until next breakpoint
- **Evaluate Expression**: Run code in current context

### Conditional Breakpoints

```java
public void processOrders(List<Order> orders) {
  for (Order order : orders) {
    // Regular breakpoint stops on every iteration
    processOrder(order);

    // ✅ Conditional breakpoint: order.getTotal() > 1000
    // Only stops when total exceeds 1000
    if (order.isVip()) {
      applyDiscount(order);
    }
  }
}
```

**Setting conditional breakpoints:**

1. Right-click breakpoint (red dot)
2. Add condition: `order.getTotal().compareTo(new BigDecimal("1000")) > 0`
3. Breakpoint only triggers when condition is true

### Watches and Variables

```java
public BigDecimal calculateDiscount(Order order) {
  // View Variables panel shows all local variables:
  // order = Order@1234
  // customerType = PREMIUM
  // baseDiscount = 0.10

  BigDecimal baseDiscount = getBaseDiscount(order.getCustomerType());

  // Add watch for complex expression:
  // order.getTotal().multiply(baseDiscount)
  // Shows computed value without modifying code

  if (order.getTotal().compareTo(new BigDecimal("500")) > 0) {
    baseDiscount = baseDiscount.add(new BigDecimal("0.05"));
  }

  return baseDiscount;
}
```

**Why it matters**: Debugger lets you inspect program state without adding print statements. See variable values, evaluate expressions, and understand code flow. Breakpoints pause execution exactly where you need to investigate.

## Debugging Strategies

### Divide and Conquer

```java
// ✅ Bug somewhere in this method
public OrderResult processComplexOrder(Order order) {
  validateOrder(order);              // Breakpoint 1
  BigDecimal total = calculateTotal(order);  // Breakpoint 2
  applyDiscounts(order, total);      // Breakpoint 3
  PaymentResult payment = processPayment(order, total); // Breakpoint 4
  updateInventory(order);            // Breakpoint 5
  return createResult(payment);      // Breakpoint 6
}

// Place breakpoint in middle (step 3)
// If bug occurs before breakpoint, move breakpoint earlier
// If bug occurs after breakpoint, move breakpoint later
// Repeat until you isolate the problem
```

### Rubber Duck Debugging

```java
// Explain code line-by-line to rubber duck (or colleague):

public boolean isValidEmail(String email) {
  // "This method checks if email is valid"
  if (email == null) {
    // "Returns false if email is null" - OK
    return false;
  }

  // "Checks if email contains @" - OK
  if (!email.contains("@")) {
    return false;
  }

  // "Splits on @ and checks parts" - Wait...
  String[] parts = email.split("@");

  // "Returns true if both parts non-empty" - AH! Bug found!
  // What if email is "@example.com" or "user@"?
  // split() returns arrays with empty strings!
  return parts.length == 2; // ❌ Not enough validation

  // ✅ Fixed version:
  // return parts.length == 2 && !parts[0].isEmpty() && !parts[1].isEmpty();
}
```

**Why it works**: Verbalizing logic forces you to think clearly about what code actually does versus what you assume it does. Often reveals bugs through the act of explanation.

### Binary Search Through History

```java
// ✅ Bug appeared recently but you don't know when

// Use git bisect to find the commit that introduced the bug:
// 1. git bisect start
// 2. git bisect bad (current commit is bad)
// 3. git bisect good abc123 (commit abc123 was good)
// 4. Git checks out middle commit
// 5. Test: if bug exists, run 'git bisect bad', else 'git bisect good'
// 6. Git narrows down until it finds exact commit that introduced bug

// This finds problematic commit in O(log n) time instead of O(n)
```

### Print Debugging vs Debugger

```java
// ❌ Print debugging - slow and clutters code
public void processPayment(Payment payment) {
  System.out.println("Processing payment: " + payment); // Added
  validatePayment(payment);
  System.out.println("Payment validated"); // Added

  BigDecimal amount = payment.getAmount();
  System.out.println("Amount: " + amount); // Added

  boolean success = chargeCard(payment.getCard(), amount);
  System.out.println("Charge result: " + success); // Added

  if (success) {
    updateBalance(payment.getUserId(), amount);
    System.out.println("Balance updated"); // Added
  }

  // Forgot to remove print statements before commit!
}

// ✅ Use debugger instead
public void processPayment(Payment payment) {
  validatePayment(payment); // Set breakpoint, inspect payment

  BigDecimal amount = payment.getAmount(); // Step over, check amount

  boolean success = chargeCard(payment.getCard(), amount); // Step into if needed

  if (success) {
    updateBalance(payment.getUserId(), amount); // Inspect all variables
  }
  // No code changes needed, no cleanup required
}
```

**When to use print debugging:**

- Quick sanity checks
- Production issues (can't attach debugger)
- Concurrent code where debugger changes timing
- Remote servers without debug access

**When to use debugger:**

- Complex logic with many variables
- Need to inspect object state
- Want to evaluate expressions dynamically
- Local development

## SLF4J and Logback

### SLF4J Facade

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class UserService {
  // ✅ Use SLF4J logger
  private static final Logger logger = LoggerFactory.getLogger(UserService.class);

  public User getUser(String userId) {
    logger.debug("Fetching user with ID: {}", userId);

    try {
      User user = userRepository.findById(userId);

      if (user == null) {
        logger.warn("User not found: {}", userId);
        return null;
      }

      logger.info("Successfully retrieved user: {}", userId);
      return user;

    } catch (DatabaseException e) {
      logger.error("Database error while fetching user: {}", userId, e);
      throw e;
    }
  }
}
```

**Why SLF4J**: Acts as a facade over logging implementations (Logback, Log4j2, etc.). Code depends on SLF4J interface, not specific implementation. Can switch logging frameworks without code changes.

### Log Levels

```java
public class PaymentService {
  private static final Logger logger = LoggerFactory.getLogger(PaymentService.class);

  public PaymentResult processPayment(Payment payment) {
    // TRACE - Very detailed, rarely used
    logger.trace("Entering processPayment with: {}", payment);

    // DEBUG - Diagnostic information for development
    logger.debug("Validating payment amount: {}", payment.getAmount());
    validatePayment(payment);

    // INFO - Important business events
    logger.info("Processing payment {} for user {}", payment.getId(), payment.getUserId());

    try {
      PaymentResult result = chargeCard(payment);

      // WARN - Unexpected but recoverable
      if (!result.isSuccess()) {
        logger.warn("Payment failed: {} - Reason: {}", payment.getId(), result.getReason());
      }

      return result;

    } catch (Exception e) {
      // ERROR - Serious problems requiring attention
      logger.error("Payment processing failed for payment: {}", payment.getId(), e);
      throw e;
    }
  }
}
```

**Log level guidelines:**

- **ERROR**: Something failed that shouldn't have (exceptions, critical errors)
- **WARN**: Something unexpected but handled (validation failures, retries)
- **INFO**: Important business events (user login, payment processed)
- **DEBUG**: Diagnostic info for developers (method entry/exit, variable values)
- **TRACE**: Very detailed debugging (loop iterations, fine-grained flow)

### Structured Logging

```java
import org.slf4j.MDC;

public class OrderService {
  private static final Logger logger = LoggerFactory.getLogger(OrderService.class);

  public void processOrder(Order order) {
    // ✅ Add context to MDC (Mapped Diagnostic Context)
    MDC.put("orderId", order.getId());
    MDC.put("userId", order.getUserId());

    try {
      logger.info("Processing order"); // MDC automatically included
      // Log output: [orderId=123, userId=456] Processing order

      validateOrder(order);
      processPayment(order);
      updateInventory(order);

      logger.info("Order processed successfully");

    } catch (Exception e) {
      logger.error("Order processing failed", e);
      throw e;
    } finally {
      // ✅ Always clean up MDC
      MDC.clear();
    }
  }
}

// ✅ MDC with try-finally for safety
public void processRequest(Request request) {
  MDC.put("requestId", request.getId());
  MDC.put("clientIp", request.getClientIp());

  try {
    handleRequest(request);
  } finally {
    MDC.clear();
  }
}
```

**Why MDC matters**: Adds contextual information to all log statements without passing it explicitly. Especially useful in multi-threaded applications where each thread processes different requests.

### Parameterized Logging

```java
// ❌ String concatenation - always evaluates even if logging disabled
logger.debug("User " + user.getName() + " with ID " + user.getId() +
  " processed order " + order.getId());
// Wastes CPU creating string even when DEBUG is disabled

// ✅ Parameterized logging - only evaluates if level enabled
logger.debug("User {} with ID {} processed order {}",
  user.getName(), user.getId(), order.getId());
// String formatting skipped when DEBUG disabled = better performance

// ✅ More parameters
logger.info("Payment {} for user {} with amount {} processed at {}",
  paymentId, userId, amount, timestamp);

// ✅ With exception (exception always last parameter)
logger.error("Failed to process payment {} for user {}", paymentId, userId, exception);
```

### Log Format Configuration

```xml
<!-- logback.xml -->
<configuration>
  <!-- Console appender for development -->
  <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
    <encoder>
      <pattern>%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n</pattern>
    </encoder>
  </appender>

  <!-- File appender for production -->
  <appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
    <file>logs/application.log</file>
    <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
      <fileNamePattern>logs/application-%d{yyyy-MM-dd}.log</fileNamePattern>
      <maxHistory>30</maxHistory>
    </rollingPolicy>
    <encoder>
      <pattern>%d{ISO8601} [%thread] %-5level %logger - %msg%n</pattern>
    </encoder>
  </appender>

  <!-- JSON appender for structured logging -->
  <appender name="JSON" class="ch.qos.logback.core.rolling.RollingFileAppender">
    <file>logs/application.json</file>
    <encoder class="net.logstash.logback.encoder.LogstashEncoder">
      <includeMdc>true</includeMdc>
    </encoder>
  </appender>

  <!-- Set log levels per package -->
  <logger name="com.myapp.service" level="DEBUG" />
  <logger name="org.springframework" level="WARN" />
  <logger name="org.hibernate" level="WARN" />

  <!-- Root logger -->
  <root level="INFO">
    <appender-ref ref="CONSOLE" />
    <appender-ref ref="FILE" />
  </root>
</configuration>
```

**Pattern elements:**

- `%d{HH:mm:ss.SSS}` - Timestamp
- `%thread` - Thread name
- `%-5level` - Log level (left-aligned, 5 chars)
- `%logger{36}` - Logger name (max 36 chars)
- `%msg` - Log message
- `%n` - Newline
- `%X{orderId}` - MDC value for key "orderId"

## Logging Best Practices

### Include Context

```java
// ❌ Insufficient context
logger.error("Payment failed");

// ✅ Include relevant context
logger.error("Payment {} failed for user {} with amount {}",
  payment.getId(), payment.getUserId(), payment.getAmount());

// ❌ Vague error message
logger.warn("Invalid input");

// ✅ Specific error message
logger.warn("Order validation failed: missing required field 'email' for order {}",
  order.getId());
```

### Log Exceptions Properly

```java
// ❌ Lost stack trace
try {
  processPayment(payment);
} catch (PaymentException e) {
  logger.error("Payment failed: " + e.getMessage());
  // Stack trace lost!
}

// ✅ Include exception for stack trace
try {
  processPayment(payment);
} catch (PaymentException e) {
  logger.error("Payment processing failed for payment {}", payment.getId(), e);
  // Full stack trace logged
}

// ✅ Log and rethrow
try {
  criticalOperation();
} catch (CriticalException e) {
  logger.error("Critical operation failed", e);
  throw e; // Rethrow after logging
}
```

### Avoid Logging Sensitive Data

```java
// ❌ Logging sensitive data
logger.info("User login: username={}, password={}", username, password);
logger.debug("Credit card charged: {}", creditCard); // Full card number!

// ✅ Mask or omit sensitive data
logger.info("User login: username={}", username); // No password
logger.debug("Credit card charged: {}",
  creditCard.getMaskedNumber()); // Last 4 digits only
logger.info("Payment processed for user {} with amount {}",
  userId, amount); // No card details
```

### Guard Expensive Log Statements

```java
// ❌ Expensive computation even when DEBUG disabled
logger.debug("User details: " + generateDetailedReport(user));
// generateDetailedReport() always runs!

// ✅ Check level before expensive operations
if (logger.isDebugEnabled()) {
  logger.debug("User details: {}", generateDetailedReport(user));
  // Report only generated when DEBUG enabled
}

// ✅ Use parameterized logging (automatically lazy)
logger.debug("User count: {}", () -> users.stream().count());
// Count only computed when DEBUG enabled
```

## Debugging Concurrent Code

```java
public class ConcurrentDebugger {
  private static final Logger logger = LoggerFactory.getLogger(ConcurrentDebugger.class);

  public void processConcurrently(List<Task> tasks) {
    // ✅ Include thread info in logs
    logger.info("Processing {} tasks across threads", tasks.size());

    tasks.parallelStream().forEach(task -> {
      // MDC for thread-specific context
      MDC.put("taskId", task.getId());
      MDC.put("thread", Thread.currentThread().getName());

      try {
        logger.debug("Starting task");
        processTask(task);
        logger.debug("Task completed");
      } catch (Exception e) {
        logger.error("Task failed", e);
      } finally {
        MDC.clear();
      }
    });
  }

  // ✅ Log timestamps to detect race conditions
  public void updateSharedResource(String resourceId) {
    long timestamp = System.currentTimeMillis();
    logger.debug("Accessing resource {} at {}", resourceId, timestamp);

    // If you see interleaved access from multiple threads,
    // you may have a race condition
  }
}
```

## Summary

Effective debugging starts with using the debugger instead of print statements. Breakpoints pause execution where needed, stepping controls move through code line by line, and watches evaluate expressions without code changes. Conditional breakpoints stop only when specific conditions are met, avoiding repetitive manual checks in loops.

Debugging strategies include divide and conquer for isolating problems, rubber duck debugging for logic errors, and git bisect for finding commits that introduced bugs. Use debuggers for local development with complex state. Use logging for production issues, remote systems, and concurrent code where debuggers affect timing.

SLF4J provides a logging facade that decouples code from logging implementations. Log levels organize messages by severity - ERROR for failures, WARN for unexpected but handled events, INFO for business events, DEBUG for diagnostic information, and TRACE for fine-grained details. Choose levels based on who needs the information and when.

Structured logging with MDC adds context to all log statements in a thread. Set orderId, userId, requestId in MDC and all subsequent logs include this information automatically. Always clear MDC in finally blocks to prevent context leaking between requests in thread pools.

Parameterized logging with {} placeholders avoids string concatenation overhead when logging is disabled. The formatting only happens if the log level is enabled, improving performance. Always put exceptions as the last parameter to include stack traces.

Logback configuration controls where logs go and how they're formatted. Console appenders for development, rolling file appenders for production, JSON appenders for structured logs sent to centralized logging systems. Configure different log levels per package to reduce noise from frameworks.

Log context makes messages actionable. Include IDs, amounts, timestamps, and relevant details. Log exceptions with full stack traces. Avoid logging sensitive data like passwords or full credit card numbers. Guard expensive log statement computations with level checks.

Debugging and logging work together - debuggers for local investigation, logging for production monitoring and diagnosis. Master both to diagnose problems quickly and maintain reliable systems.

## Related Content

- [Java Best Practices and Design Principles](/en/learn/swe/prog-lang/java/explanation/best-practices)
- [How to Implement Proper Exception Handling](/en/learn/swe/prog-lang/java/how-to/exception-handling)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/java/how-to/write-effective-tests)
