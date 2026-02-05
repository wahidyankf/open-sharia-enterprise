---
title: "Logging"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Comprehensive guide to logging in Java from standard library fundamentals to production logging frameworks
weight: 10000003
tags: ["java", "logging", "slf4j", "logback", "log4j2", "best-practices"]
---

## Why Logging Matters

Logging is essential for understanding application behavior in production environments where debuggers cannot be attached. Effective logging enables troubleshooting, monitoring, auditing, and performance analysis.

**Core Benefits**:

- **Debugging**: Diagnose issues in production without reproducing locally
- **Monitoring**: Track application health and performance metrics
- **Auditing**: Record security-relevant events for compliance
- **Analytics**: Understand user behavior and system usage patterns
- **Alerting**: Trigger notifications for critical errors

**Problem**: Console output (System.out) is unstructured, lacks log levels, cannot be controlled at runtime, and provides no filtering or routing capabilities.

**Solution**: Use logging frameworks that provide levels, configuration, formatting, and routing to multiple destinations.

## Logging Framework Comparison

| Framework             | Pros                                  | Cons                           | Use When                        |
| --------------------- | ------------------------------------- | ------------------------------ | ------------------------------- |
| **Logback**           | Fast, flexible, mature, wide adoption | XML configuration verbose      | Most production applications    |
| **Log4j2**            | Async, garbage-free, high performance | More complex configuration     | High-throughput systems         |
| **SLF4J**             | Facade (not implementation)           | Requires implementation        | Library code (API only)         |
| **java.util.logging** | Built-in, no dependencies             | Limited features, poor perf    | Simple scripts, learning basics |
| **System.out/err**    | Simplest possible                     | No levels, routing, or control | Throwaway prototypes only       |

**Recommendation**: Use SLF4J API with Logback implementation for production applications - it's the modern standard with excellent performance and flexibility.

**Recommended progression**: Start with java.util.logging to understand logging fundamentals → Learn SLF4J facade pattern → Use Logback for production.

## Standard Library Logging (java.util.logging)

Java's standard library provides basic logging through java.util.logging (JUL). Use this to understand logging fundamentals before introducing external frameworks.

### Basic Logger Usage

Create loggers and write log messages at different severity levels.

**Basic pattern**:

```java
import java.util.logging.Logger;  // => Standard library logging API
import java.util.logging.Level;   // => Log level constants (SEVERE, WARNING, INFO, etc.)

public class PaymentService {
    // Logger named after the class
    private static final Logger logger = Logger.getLogger(PaymentService.class.getName());
    // => Logger.getLogger() returns cached logger instance
    // => Class name used as logger name for hierarchical configuration

    public void processPayment(String customerId, double amount) {
        // INFO: Significant business events
        logger.info("Processing payment for customer: " + customerId);
        // => INFO level for business-significant events
        // => String concatenation happens always (performance issue!)

        // FINE: Detailed diagnostic information
        logger.fine("Payment amount: " + amount);
        // => FINE level for diagnostic details
        // => Only visible if level configured to FINE or lower

        try {
            validateAmount(amount);
            // => Validation occurs before payment processing
            // Process payment...

            logger.info("Payment processed successfully: " + customerId);
            // => Log success for audit trail and monitoring

        } catch (IllegalArgumentException e) {
            // WARNING: Unexpected but handled
            logger.warning("Invalid payment amount for customer " + customerId + ": " + e.getMessage());
            // => WARNING level: expected validation failure
            // => Include customer context for troubleshooting
            throw e;
            // => Re-throw after logging (caller handles)

        } catch (Exception e) {
            // SEVERE: System errors requiring attention
            logger.log(Level.SEVERE, "Payment processing failed for customer: " + customerId, e);
            // => SEVERE level: system error requiring immediate attention
            // => Exception parameter includes full stack trace
            // => logger.log() variant accepts exception as third parameter
            throw new PaymentException("Payment failed", e);
            // => Wrap in domain exception preserving cause chain
        }
    }

    private void validateAmount(double amount) {
        if (amount <= 0) {
            // => Business rule: payments must be positive
            throw new IllegalArgumentException("Amount must be positive");
            // => IllegalArgumentException for validation failures
        }
    }
}
```

**Log Levels in java.util.logging**:

| Level   | Value | Purpose                                     | Example Use Case     |
| ------- | ----- | ------------------------------------------- | -------------------- |
| SEVERE  | 1000  | System errors requiring immediate attention | Database unavailable |
| WARNING | 900   | Unexpected but handled situations           | Validation failures  |
| INFO    | 800   | Significant business events                 | Payment processed    |
| CONFIG  | 700   | Configuration messages                      | Settings loaded      |
| FINE    | 500   | Detailed diagnostic information             | Method parameters    |
| FINER   | 400   | More detailed diagnostic information        | Method entry/exit    |
| FINEST  | 300   | Highly detailed diagnostic information      | Loop iterations      |

### Configuring java.util.logging

Configure logging behavior using logging.properties file.

**logging.properties**:

```properties
# Root logger level
.level=INFO
# => Dot prefix means root logger (all packages)
# => INFO level: shows INFO, WARNING, SEVERE (hides FINE, FINER, FINEST)

# Console handler configuration
handlers=java.util.logging.ConsoleHandler
# => Comma-separated list of handlers to attach to root logger
# => ConsoleHandler writes to System.err by default

# Console handler level
java.util.logging.ConsoleHandler.level=ALL
# => ALL allows all levels through handler (root logger controls actual filtering)
# => Handler level acts as second filter after logger level
java.util.logging.ConsoleHandler.formatter=java.util.logging.SimpleFormatter
# => SimpleFormatter produces human-readable text output
# => Alternative: XMLFormatter for machine-readable logs

# Package-specific levels
com.example.payment.level=FINE
# => Override root level for payment package and subpackages
# => Enables diagnostic logging for specific subsystems
com.example.database.level=WARNING
# => Suppress INFO logs from noisy database package
# => Only WARNING and SEVERE will appear

# Simple formatter pattern (Java 7+)
java.util.logging.SimpleFormatter.format=%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS %4$-6s %2$s %5$s%6$s%n
# => %1$ = date argument (tY=year, tm=month, td=day, tH=hour, tM=minute, tS=second)
# => %4$ = log level (left-aligned in 6 chars)
# => %2$ = source class/method
# => %5$ = message text
# => %6$ = exception stack trace if present
# => %n = platform-specific newline
```

**Load configuration**:

```java
import java.io.IOException;       // => Checked exception for I/O operations
import java.io.InputStream;       // => Stream for reading configuration file
import java.util.logging.LogManager;  // => Global logging configuration manager

public class Application {
    static {
        // Load logging configuration at startup
        // => Static block executes once during class loading
        // => Ensures logging configured before any code runs
        try (InputStream is = Application.class.getResourceAsStream("/logging.properties")) {
            // => try-with-resources ensures stream closed automatically
            // => getResourceAsStream() loads from classpath root (src/main/resources/)
            // => Returns null if file not found
            LogManager.getLogManager().readConfiguration(is);
            // => LogManager singleton manages all loggers globally
            // => readConfiguration() parses properties and configures loggers
            // => Replaces default java.util.logging configuration
        } catch (IOException e) {
            // => IOException if file cannot be read or parsed
            System.err.println("Could not load logging configuration: " + e.getMessage());
            // => Fallback to System.err because logging not configured yet
            // => Application continues with default logging configuration
        }
    }

    public static void main(String[] args) {
        // Application code...
        // => Logging already configured when main() executes
    }
}
```

### File Logging with FileHandler

Write logs to files with optional rotation.

**Pattern**:

```java
import java.util.logging.*;  // => Logger, FileHandler, Level, SimpleFormatter
import java.io.IOException;  // => FileHandler throws IOException

public class ConfigureFileLogging {
    public static void main(String[] args) throws IOException {
        // => Throws IOException because FileHandler constructor can fail
        Logger logger = Logger.getLogger("com.example");
        // => Get logger for package hierarchy (com.example.*)
        // => Logger created on first call, cached for subsequent calls

        // Remove default console handler
        logger.setUseParentHandlers(false);
        // => Prevents delegation to parent logger's handlers
        // => Stops logs from also appearing on console
        // => Useful when you want file-only logging

        // Create file handler with rotation
        // %g = generation number, %u = unique number to resolve conflicts
        FileHandler fileHandler = new FileHandler(
            "application-%g.log",  // Pattern
            // => %g replaced with 0, 1, 2, etc. for each rotation
            // => Creates: application-0.log, application-1.log, etc.
            1024 * 1024,           // Max file size: 1MB
            // => 1048576 bytes per file before rotation
            // => Prevents unbounded log file growth
            5,                      // Max number of files
            // => Keeps 5 most recent log files (0-4)
            // => Oldest deleted when limit reached
            true                    // Append mode
            // => true: append to existing file
            // => false: overwrite file on each startup
        );
        // => FileHandler manages rotation automatically
        // => Thread-safe for concurrent logging

        // Set formatter
        fileHandler.setFormatter(new SimpleFormatter());
        // => SimpleFormatter produces human-readable text
        // => Uses SimpleFormatter.format property if configured
        // => Alternative: XMLFormatter for structured logs

        // Add handler to logger
        logger.addHandler(fileHandler);
        // => Loggers can have multiple handlers
        // => Each handler receives all log records passing level filter
        logger.setLevel(Level.ALL);
        // => Allow all levels through logger
        // => Handler's level still applies (second filter)

        // Test logging
        logger.info("Application started");
        // => INFO level: appears in file (above ALL threshold)
        logger.fine("Detailed information");
        // => FINE level: appears in file (ALL allows all levels)
        // => File rotation happens automatically at 1MB
    }
}
```

### Why java.util.logging is Limited

**Limitations**:

1. **Verbose API**: String concatenation in log messages (no parameterization)
2. **Performance**: String concatenation happens even when level is disabled
3. **Configuration**: Properties file format is inflexible
4. **Limited formatters**: SimpleFormatter and XMLFormatter only
5. **No advanced features**: No async logging, no structured logging, no MDC
6. **Integration**: Poor support for external log aggregation systems

**Example of performance problem**:

```java
// BAD: String concatenation happens even if FINE is disabled
logger.fine("Processing payment for customer " + customerId + " with amount " + amount);
// => String concatenation executed before logger.fine() call
// => Creates temporary String objects even when FINE disabled
// => Wastes CPU and creates garbage collector pressure

// Better: Use guard clause
if (logger.isLoggable(Level.FINE)) {
    // => Check level first before expensive string operations
    // => Returns false immediately if FINE disabled
    logger.fine("Processing payment for customer " + customerId + " with amount " + amount);
    // => Concatenation only happens when FINE enabled
    // => Reduces overhead but code is verbose
}

// Best: Use SLF4J parameterized messages (shown later)
logger.debug("Processing payment for customer {} with amount {}", customerId, amount);
// => SLF4J performs level check internally before formatting
// => Placeholders {} replaced with arguments only if level enabled
// => Best performance without guard clause boilerplate
// => Cleaner code, same performance benefit
```

**Before**: java.util.logging with verbose API and manual guards
**After**: Modern logging frameworks with parameterized messages and better performance

## SLF4J API (Logging Facade)

SLF4J (Simple Logging Facade for Java) provides an API that decouples your code from specific logging implementations. Use SLF4J in your application and library code.

### Why Facades Matter

**Problem**: If your code uses Logback directly and a library uses Log4j2, you have multiple logging frameworks competing in the same JVM.

**Solution**: Write all code against SLF4J API. Choose one implementation (Logback or Log4j2) and bridge other frameworks to SLF4J.

**Benefits**:

- **Implementation independence**: Change logging framework without code changes
- **Library compatibility**: Libraries use SLF4J, applications choose implementation
- **Single configuration**: One logging configuration for entire application
- **Better performance**: Modern implementations optimize parameterized messages

### Basic SLF4J Usage

Use parameterized messages to avoid string concatenation.

**Maven dependency** (API only, implementation added separately):

```xml
<!-- => SLF4J API only (facade, not implementation) -->
<dependency>
    <groupId>org.slf4j</groupId>
    <!-- => org.slf4j group: official SLF4J project -->
    <artifactId>slf4j-api</artifactId>
    <!-- => API artifact: logging facade interfaces -->
    <!-- => Does NOT include actual logging implementation -->
    <version>2.0.9</version>
    <!-- => Version 2.x: requires Java 8+ -->
    <!-- => Separate implementation dependency required (Logback or Log4j2) -->
</dependency>
<!-- => At runtime, need implementation binding (logback-classic or log4j-slf4j2-impl) -->
```

**Basic pattern**:

```java
import org.slf4j.Logger;         // => SLF4J facade interface (not java.util.logging)
import org.slf4j.LoggerFactory;  // => Factory for creating logger instances

public class PaymentService {
    // Logger named after the class
    private static final Logger logger = LoggerFactory.getLogger(PaymentService.class);
    // => LoggerFactory.getLogger() returns logger for given class
    // => Implementation-agnostic: works with Logback or Log4j2
    // => Class-based naming enables hierarchical configuration
    // => static final: logger is thread-safe and immutable

    public void processPayment(String customerId, double amount) {
        // Parameterized messages (no string concatenation!)
        logger.info("Processing payment for customer: {}", customerId);
        // => {} placeholder replaced with customerId at runtime
        // => Formatting only happens if INFO level enabled
        // => No string concatenation overhead when logging disabled
        logger.debug("Payment details - customer: {}, amount: {}", customerId, amount);
        // => Multiple placeholders filled left-to-right with varargs
        // => DEBUG typically disabled in production
        // => No performance penalty when disabled (arguments not evaluated)

        try {
            validateAmount(amount);
            // => Validation throws exception on failure

            logger.info("Payment processed successfully: customerId={}, amount={}",
                customerId, amount);
            // => Structured message: customerId= prefix aids log parsing
            // => Key-value pairs easily searchable in log aggregation systems

        } catch (IllegalArgumentException e) {
            logger.warn("Invalid payment: customerId={}, reason={}",
                customerId, e.getMessage());
            // => WARN level: expected validation failure (not system error)
            // => getMessage() extracts error message without stack trace
            // => Stack trace not needed for validation failures
            throw e;
            // => Re-throw after logging for caller to handle

        } catch (Exception e) {
            // Exception as last parameter (automatically includes stack trace)
            logger.error("Payment processing failed: customerId={}", customerId, e);
            // => ERROR level: unexpected system error
            // => Exception as last parameter triggers stack trace logging
            // => SLF4J automatically detects Throwable and includes trace
            // => customerId provides business context for troubleshooting
            throw new PaymentException("Payment failed", e);
            // => Wrap in domain exception preserving cause chain
        }
    }

    private void validateAmount(double amount) {
        if (amount <= 0) {
            // => Business rule validation
            throw new IllegalArgumentException("Amount must be positive");
            // => IllegalArgumentException for invalid arguments
        }
    }
}
```

**SLF4J Log Levels**:

| Level | Purpose                                     | Example Use Case                       |
| ----- | ------------------------------------------- | -------------------------------------- |
| ERROR | System errors requiring immediate attention | Database connection failures           |
| WARN  | Unexpected but handled situations           | Validation failures, retries           |
| INFO  | Significant business events                 | User registration, payment processing  |
| DEBUG | Detailed diagnostic information             | Method parameters, intermediate values |
| TRACE | Very detailed debugging (usually disabled)  | Loop iterations, fine-grained flow     |

**See**: [Best Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/best-practices) for comprehensive logging level guidelines with examples.

### Parameterized Messages

Parameterized messages improve performance by avoiding string concatenation when logging is disabled.

**Performance comparison**:

```java
// BAD: String concatenation always happens
logger.debug("User " + userId + " performed action " + action + " at " + timestamp);
// Problem: If DEBUG is disabled, concatenation still wastes CPU and creates garbage
// => Operator + creates intermediate String objects
// => StringBuilder used behind scenes for multiple concatenations
// => Object allocation happens before logger.debug() called
// => Garbage collector must clean up unused String objects

// BETTER: Guard clause prevents concatenation
if (logger.isDebugEnabled()) {
    // => Check if DEBUG level enabled before expensive operations
    // => Returns false immediately if DEBUG disabled
    logger.debug("User " + userId + " performed action " + action + " at " + timestamp);
    // => String concatenation only happens when DEBUG enabled
    // => No wasted allocations when logging disabled
}
// Problem: Verbose, easy to forget
// => Clutters code with guard clauses
// => Developers often forget to add guards
// => Manual performance optimization required

// BEST: Parameterized messages
logger.debug("User {} performed action {} at {}", userId, action, timestamp);
// Solution: Concatenation only happens if DEBUG is enabled
// => SLF4J checks level before formatting message
// => Arguments passed as varargs (no array creation overhead)
// => toString() only called on arguments if level enabled
// => Clean code without guard clause boilerplate
// No guard clause needed!
// => Automatic lazy evaluation built into SLF4J
```

**How it works**:

1. If DEBUG level is disabled, SLF4J immediately returns without processing arguments
2. If DEBUG level is enabled, SLF4J formats the message using arguments
3. No string concatenation overhead when logging is disabled

### Marker API for Filtering

Use markers to classify log messages for advanced filtering.

**Pattern**:

```java
import org.slf4j.Logger;         // => SLF4J logger interface
import org.slf4j.LoggerFactory;  // => Logger factory
import org.slf4j.Marker;         // => Marker interface for log classification
import org.slf4j.MarkerFactory;  // => Factory for creating marker instances

public class PaymentService {
    private static final Logger logger = LoggerFactory.getLogger(PaymentService.class);
    // => Single logger instance for class

    // Define markers for classification
    private static final Marker SECURITY = MarkerFactory.getMarker("SECURITY");
    // => SECURITY marker: authentication, authorization, security events
    // => Used for routing security-sensitive logs to dedicated files
    // => Enables security team to monitor security events separately
    private static final Marker PERFORMANCE = MarkerFactory.getMarker("PERFORMANCE");
    // => PERFORMANCE marker: timing, throughput, latency measurements
    // => Routes to performance monitoring systems
    private static final Marker AUDIT = MarkerFactory.getMarker("AUDIT");
    // => AUDIT marker: business events requiring compliance trail
    // => Typically retained longer than other logs (e.g., 7 years)
    // => Immutable once written (append-only)

    public void processPayment(String customerId, double amount) {
        long startTime = System.nanoTime();
        // => Capture start time for performance measurement
        // => nanoTime() for elapsed time (not wall clock time)

        // Audit log with marker
        logger.info(AUDIT, "Payment initiated: customerId={}, amount={}",
            customerId, amount);
        // => Marker as first parameter routes to audit log file
        // => Compliance requirement: log all payment attempts
        // => Includes customer and amount for audit trail

        try {
            authenticateCustomer(customerId);
            // => Verify customer identity before charging
            chargePayment(customerId, amount);
            // => Execute actual payment transaction

            long duration = System.nanoTime() - startTime;
            // => Calculate elapsed time in nanoseconds

            // Performance log with marker
            logger.info(PERFORMANCE, "Payment processed in {}ms: customerId={}",
                duration / 1_000_000, customerId);
            // => Convert nanoseconds to milliseconds (divide by 1,000,000)
            // => PERFORMANCE marker routes to monitoring systems
            // => Enables SLA tracking and performance analysis
            // => Underscore separator (1_000_000) improves readability

            // Audit log
            logger.info(AUDIT, "Payment completed: customerId={}, amount={}",
                customerId, amount);
            // => Compliance requirement: log successful completions
            // => Paired with "initiated" log for transaction tracking

        } catch (AuthenticationException e) {
            // Security log with marker
            logger.warn(SECURITY, "Authentication failed: customerId={}", customerId, e);
            // => SECURITY marker: routes to security monitoring system
            // => WARN level: failed authentication is suspicious but expected
            // => Includes stack trace (e parameter) for investigation
            // => Security team alerted for potential account compromise
            throw e;
            // => Re-throw after logging for caller handling
        }
    }
}
```

**Configuration** (filter by marker in logback.xml - shown later).

## Logback (SLF4J Implementation)

Logback is the recommended SLF4J implementation for most applications. It's fast, flexible, and widely adopted.

### Adding Logback Dependencies

**Maven dependencies**:

```xml
<!-- SLF4J API -->
<!-- => Facade: code depends on API, not implementation -->
<dependency>
    <groupId>org.slf4j</groupId>
    <!-- => org.slf4j: official SLF4J project group -->
    <artifactId>slf4j-api</artifactId>
    <!-- => API artifact: interfaces only, no implementation -->
    <version>2.0.9</version>
    <!-- => Version 2.x requires Java 8+ -->
    <!-- => Compile scope (default): needed for compilation and runtime -->
</dependency>

<!-- Logback implementation (includes slf4j binding) -->
<!-- => Implementation: fulfills SLF4J API at runtime -->
<dependency>
    <groupId>ch.qos.logback</groupId>
    <!-- => ch.qos.logback: Logback project group -->
    <artifactId>logback-classic</artifactId>
    <!-- => classic module: SLF4J implementation + binding -->
    <!-- => Transitively includes logback-core (core logging engine) -->
    <!-- => Automatically provides SLF4J→Logback binding -->
    <version>1.4.11</version>
    <!-- => Version 1.4.x compatible with SLF4J 2.x -->
    <!-- => Runtime scope: only needed at runtime, not compilation -->
</dependency>
<!-- => Only ONE SLF4J implementation allowed per application -->
<!-- => Multiple implementations cause conflicts (SLF4J warns on startup) -->
```

**Note**: logback-classic includes logback-core and SLF4J binding automatically.

### Basic Logback Configuration

Configure Logback using logback.xml in src/main/resources.

**logback.xml** (basic configuration):

```xml
<configuration>
    <!-- => Root element: Logback configuration file -->
    <!-- => Loaded from src/main/resources/logback.xml -->
    <!-- => Auto-reloaded on changes if scanPeriod configured -->

    <!-- Console appender -->
    <!-- => Appender: destination for log output -->
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <!-- => ConsoleAppender writes to System.out (stdout) -->
        <!-- => name="CONSOLE": identifier for appender-ref below -->
        <encoder>
            <!-- => Encoder converts LoggingEvent to bytes -->
            <!-- => PatternLayoutEncoder: format logs using pattern -->
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
            <!-- => %d{...}: timestamp format (yyyy-MM-dd HH:mm:ss) -->
            <!-- => [%thread]: thread name in brackets -->
            <!-- => %-5level: log level left-aligned in 5 chars (INFO, DEBUG, ERROR) -->
            <!-- => %logger{36}: logger name shortened to 36 chars max -->
            <!-- => %msg: formatted log message -->
            <!-- => %n: platform-specific newline -->
        </encoder>
    </appender>

    <!-- Root logger -->
    <!-- => Root logger: catches all log events not handled by named loggers -->
    <root level="INFO">
        <!-- => level="INFO": threshold (INFO, WARN, ERROR pass; DEBUG/TRACE blocked) -->
        <!-- => Root applies to all packages unless overridden -->
        <appender-ref ref="CONSOLE" />
        <!-- => Attach CONSOLE appender to root logger -->
        <!-- => All log events passing INFO threshold write to console -->
    </root>

    <!-- Package-specific levels -->
    <!-- => Named loggers override root level for specific packages -->
    <logger name="com.example.payment" level="DEBUG" />
    <!-- => payment package and subpackages use DEBUG level -->
    <!-- => More verbose than root INFO (enables diagnostic logging) -->
    <logger name="com.example.database" level="WARN" />
    <!-- => database package suppressed to WARN (reduces noise) -->
    <!-- => Only warnings and errors logged -->
    <!-- => additivity="true" (default): also passes to root logger -->
</configuration>
```

**Pattern Layout Placeholders**:

| Placeholder     | Description                       | Example                     |
| --------------- | --------------------------------- | --------------------------- |
| %d{format}      | Date/time                         | 2026-02-03 14:30:15         |
| %thread         | Thread name                       | main, http-nio-8080-exec-1  |
| %-5level        | Log level (left-aligned, 5 chars) | INFO, DEBUG, ERROR          |
| %logger{length} | Logger name (shortened to length) | c.e.p.PaymentService        |
| %msg            | Log message                       | Payment processed: id=123   |
| %n              | Platform newline                  | \n (Unix) or \r\n (Windows) |
| %ex             | Exception stack trace             | Full stack trace if present |

### File Appenders with Rotation

Write logs to files with automatic rotation based on size or date.

**logback.xml** (file appenders):

```xml
<configuration>
    <!-- => Logback configuration root element -->

    <!-- Console appender -->
    <!-- => Write logs to console (System.out) -->
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <!-- => ConsoleAppender for development/debugging -->
        <encoder>
            <!-- => Encoder formats log events as text -->
            <pattern>%d{yyyy-MM-dd HH:mm:ss} %-5level %logger{36} - %msg%n</pattern>
            <!-- => Pattern defines output format -->
        </encoder>
    </appender>

    <!-- Rolling file appender (size-based) -->
    <!-- => Automatic rotation when size/time limits reached -->
    <appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <!-- => RollingFileAppender manages log file rotation -->
        <file>logs/application.log</file>
        <!-- => Current active log file path -->
        <!-- => Relative to application working directory -->

        <rollingPolicy class="ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy">
            <!-- => Rolls based on BOTH size AND time -->
            <!-- => Combines size and date-based rotation -->
            <!-- Daily rollover with size limit -->
            <fileNamePattern>logs/application-%d{yyyy-MM-dd}.%i.log</fileNamePattern>
            <!-- => %d{yyyy-MM-dd}: date pattern for daily rotation -->
            <!-- => %i: index (0, 1, 2...) when multiple files same day -->
            <!-- => Example: application-2026-02-03.0.log, application-2026-02-03.1.log -->
            <maxFileSize>10MB</maxFileSize>
            <!-- => Rotate when file reaches 10MB -->
            <!-- => Prevents single file from growing too large -->
            <maxHistory>30</maxHistory>
            <!-- => Keep 30 days of logs -->
            <!-- => Older files automatically deleted -->
            <totalSizeCap>1GB</totalSizeCap>
            <!-- => Maximum total size of all archived logs -->
            <!-- => Oldest archives deleted when cap exceeded -->
            <!-- => Prevents unlimited disk usage -->
        </rollingPolicy>

        <encoder>
            <!-- => Format for file output (same as console) -->
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
            <!-- => Includes thread name for concurrent debugging -->
        </encoder>
    </appender>

    <!-- Error file (separate file for errors) -->
    <!-- => Dedicated file for ERROR level logs -->
    <appender name="ERROR_FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <!-- => Separate file makes error investigation easier -->
        <!-- Only ERROR level logs -->
        <filter class="ch.qos.logback.classic.filter.LevelFilter">
            <!-- => Filter controls which levels reach appender -->
            <level>ERROR</level>
            <!-- => Only ERROR level matched -->
            <onMatch>ACCEPT</onMatch>
            <!-- => ACCEPT: let ERROR events through -->
            <onMismatch>DENY</onMismatch>
            <!-- => DENY: reject all non-ERROR events -->
            <!-- => Ensures only ERROR logs in this file -->
        </filter>

        <file>logs/error.log</file>
        <!-- => Dedicated error log file -->

        <rollingPolicy class="ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy">
            <!-- => Same rotation strategy as main file -->
            <fileNamePattern>logs/error-%d{yyyy-MM-dd}.%i.log</fileNamePattern>
            <!-- => error- prefix distinguishes from main logs -->
            <maxFileSize>10MB</maxFileSize>
            <!-- => Rotate at 10MB (errors usually less frequent) -->
            <maxHistory>90</maxHistory>
            <!-- => Keep errors longer (90 days vs 30 for main) -->
            <!-- => Errors may need investigation weeks later -->
        </rollingPolicy>

        <encoder>
            <!-- => Format includes full exception stack trace -->
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n%ex</pattern>
            <!-- => %ex: full exception stack trace -->
            <!-- => Critical for error diagnosis -->
        </encoder>
    </appender>

    <root level="INFO">
        <!-- => Root logger configuration -->
        <appender-ref ref="CONSOLE" />
        <!-- => All logs to console (development) -->
        <appender-ref ref="FILE" />
        <!-- => All logs to main file (INFO and above) -->
        <appender-ref ref="ERROR_FILE" />
        <!-- => Errors also to dedicated file (filter applies) -->
        <!-- => ERROR logs appear in BOTH application.log and error.log -->
    </root>
</configuration>
```

### Marker-Based Filtering

Filter logs based on markers for targeted routing.

**logback.xml** (marker filtering):

```xml
<configuration>
    <!-- Audit log (only AUDIT marker) -->
    <appender name="AUDIT_FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <filter class="ch.qos.logback.core.filter.EvaluatorFilter">
            <evaluator>
                <matcher>
                    <Name>audit-matcher</Name>
                    <regex>AUDIT</regex>
                </matcher>
                <expression>audit-matcher.matches(marker)</expression>
            </evaluator>
            <onMatch>ACCEPT</onMatch>
            <onMismatch>DENY</onMismatch>
        </filter>

        <file>logs/audit.log</file>
        <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
            <fileNamePattern>logs/audit-%d{yyyy-MM-dd}.log</fileNamePattern>
            <maxHistory>365</maxHistory>
        </rollingPolicy>

        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} %msg%n</pattern>
        </encoder>
    </appender>

    <!-- Security log (only SECURITY marker) -->
    <appender name="SECURITY_FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <filter class="ch.qos.logback.core.filter.EvaluatorFilter">
            <evaluator>
                <matcher>
                    <Name>security-matcher</Name>
                    <regex>SECURITY</regex>
                </matcher>
                <expression>security-matcher.matches(marker)</expression>
            </evaluator>
            <onMatch>ACCEPT</onMatch>
            <onMismatch>DENY</onMismatch>
        </filter>

        <file>logs/security.log</file>
        <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
            <fileNamePattern>logs/security-%d{yyyy-MM-dd}.log</fileNamePattern>
            <maxHistory>365</maxHistory>
        </rollingPolicy>

        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %logger - %msg%n</pattern>
        </encoder>
    </appender>

    <root level="INFO">
        <appender-ref ref="AUDIT_FILE" />
        <appender-ref ref="SECURITY_FILE" />
    </root>
</configuration>
```

### Logback vs java.util.logging

**Comparison**:

| Feature                | java.util.logging | Logback                             |
| ---------------------- | ----------------- | ----------------------------------- |
| **Configuration**      | Properties file   | XML, Groovy, or programmatic        |
| **Performance**        | Moderate          | Fast (optimized for throughput)     |
| **Parameterized msgs** | No                | Yes (via SLF4J)                     |
| **Async logging**      | No                | Yes (AsyncAppender)                 |
| **MDC support**        | No                | Yes                                 |
| **Marker support**     | No                | Yes                                 |
| **Rolling policies**   | Basic             | Advanced (size, time, composite)    |
| **Filter options**     | Limited           | Extensive (threshold, marker, eval) |
| **JSON output**        | Manual            | Built-in encoders                   |

**Before**: java.util.logging with limited features
**After**: Logback with advanced configuration, performance, and features

## Log4j2 (Alternative Implementation)

Log4j2 is an alternative to Logback with focus on high performance through async logging and garbage-free operation.

### When to Choose Log4j2

**Use Log4j2 when**:

- **High throughput**: Processing millions of log messages per second
- **Garbage-free required**: Minimizing GC pressure is critical
- **Async required**: All logging must be fully asynchronous
- **Plugin system needed**: Custom appenders, filters, or layouts

**Use Logback when**:

- **Standard applications**: Most web applications and services
- **Simpler configuration**: Logback XML is more straightforward
- **Wide adoption**: Larger community and ecosystem

### Adding Log4j2 Dependencies

**Maven dependencies**:

```xml
<!-- SLF4J API -->
<dependency>
    <groupId>org.slf4j</groupId>
    <artifactId>slf4j-api</artifactId>
    <version>2.0.9</version>
</dependency>

<!-- Log4j2 to SLF4J adapter -->
<dependency>
    <groupId>org.apache.logging.log4j</groupId>
    <artifactId>log4j-slf4j2-impl</artifactId>
    <version>2.20.0</version>
</dependency>

<!-- Log4j2 core -->
<dependency>
    <groupId>org.apache.logging.log4j</groupId>
    <artifactId>log4j-core</artifactId>
    <version>2.20.0</version>
</dependency>
```

### Basic Log4j2 Configuration

Configure Log4j2 using log4j2.xml in src/main/resources.

**log4j2.xml**:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Configuration status="WARN">
    <Appenders>
        <!-- Console appender -->
        <Console name="Console" target="SYSTEM_OUT">
            <PatternLayout pattern="%d{yyyy-MM-dd HH:mm:ss} [%t] %-5level %logger{36} - %msg%n"/>
        </Console>

        <!-- Rolling file appender -->
        <RollingFile name="RollingFile" fileName="logs/application.log"
                     filePattern="logs/application-%d{yyyy-MM-dd}.%i.log.gz">
            <PatternLayout pattern="%d{yyyy-MM-dd HH:mm:ss} [%t] %-5level %logger{36} - %msg%n"/>
            <Policies>
                <TimeBasedTriggeringPolicy />
                <SizeBasedTriggeringPolicy size="10 MB"/>
            </Policies>
            <DefaultRolloverStrategy max="30"/>
        </RollingFile>

        <!-- Async appender for performance -->
        <Async name="AsyncFile">
            <AppenderRef ref="RollingFile"/>
        </Async>
    </Appenders>

    <Loggers>
        <Logger name="com.example.payment" level="debug" additivity="false">
            <AppenderRef ref="AsyncFile"/>
        </Logger>

        <Root level="info">
            <AppenderRef ref="Console"/>
            <AppenderRef ref="AsyncFile"/>
        </Root>
    </Loggers>
</Configuration>
```

### Async Logging Performance

Log4j2 provides fully asynchronous logging using the LMAX Disruptor library.

**System property** (enable async for all loggers):

```bash
java -Dlog4j2.contextSelector=org.apache.logging.log4j.core.async.AsyncLoggerContextSelector \
     -jar application.jar
```

**Mixed async** (some loggers async, some sync):

```xml
<Configuration status="WARN">
    <Appenders>
        <RollingFile name="RollingFile" fileName="logs/application.log"
                     filePattern="logs/application-%d{yyyy-MM-dd}.log">
            <PatternLayout pattern="%d{yyyy-MM-dd HH:mm:ss} [%t] %-5level %logger - %msg%n"/>
        </RollingFile>
    </Appenders>

    <Loggers>
        <!-- Async logger -->
        <AsyncLogger name="com.example.payment" level="info" additivity="false">
            <AppenderRef ref="RollingFile"/>
        </AsyncLogger>

        <!-- Sync logger -->
        <Logger name="com.example.database" level="warn">
            <AppenderRef ref="RollingFile"/>
        </Logger>

        <Root level="info">
            <AppenderRef ref="RollingFile"/>
        </Root>
    </Loggers>
</Configuration>
```

## Structured Logging

Structured logging adds contextual information to log messages for better searchability and correlation across distributed systems.

### MDC (Mapped Diagnostic Context)

Use MDC to add contextual information that automatically appears in all log messages within the same thread.

**Pattern**:

```java
import org.slf4j.Logger;         // => SLF4J logger interface
import org.slf4j.LoggerFactory;  // => Logger factory
import org.slf4j.MDC;            // => Mapped Diagnostic Context (thread-local storage)
import java.util.UUID;           // => UUID generation for correlation IDs

public class PaymentController {
    private static final Logger logger = LoggerFactory.getLogger(PaymentController.class);
    // => Logger instance for controller
    private final PaymentService paymentService;
    // => Injected payment service dependency

    public void handlePaymentRequest(PaymentRequest request) {
        // => Entry point for payment HTTP requests
        // Generate unique request ID
        String requestId = UUID.randomUUID().toString();
        // => UUID provides globally unique correlation ID
        // => Format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (36 chars)
        // => Enables tracking request across services and log files

        try {
            // Add to MDC (thread-local context)
            // => MDC uses ThreadLocal storage (isolated per thread)
            MDC.put("requestId", requestId);
            // => requestId automatically included in all logs on this thread
            // => Enables correlation of all logs for single request
            MDC.put("customerId", request.getCustomerId());
            // => Customer context for business analysis
            // => Searchable in log aggregation systems
            MDC.put("operation", "payment");
            // => Operation type for categorization
            // => Useful for filtering logs by business operation

            logger.info("Processing payment request");
            // => Simple message, but MDC values automatically appended
            // Output: Processing payment request requestId=abc-123 customerId=customer-456 operation=payment
            // => Configured in logback.xml using %X{requestId} pattern

            paymentService.processPayment(request);
            // => Service call inherits MDC context (same thread)
            // => All logs in paymentService also include MDC values

            logger.info("Payment request completed");
            // => Success log with same MDC context
            // => Correlation ID links start and end logs

        } catch (Exception e) {
            logger.error("Payment request failed", e);
            // => Error log still includes MDC context
            // => Exception stack trace for debugging
            throw e;
            // => Re-throw after logging for caller handling

        } finally {
            // Clean up MDC (critical to prevent leaks!)
            MDC.clear();
            // => CRITICAL: Remove MDC values to prevent leaks
            // => Thread pools reuse threads (MDC persists without clear)
            // => Next request on same thread inherits old MDC values!
            // => Memory leak: MDC grows unbounded without cleanup
            // => Always use finally block for guaranteed cleanup
        }
    }
}
```

**Logback configuration** (include MDC in pattern):

```xml
<configuration>
    <!-- => Logback configuration for MDC output -->
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <!-- => Console appender for structured logging -->
        <encoder>
            <!-- => PatternLayout encoder for formatting -->
            <!-- Include MDC values in output -->
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg %X{requestId} %X{customerId}%n</pattern>
            <!-- => %X{key}: extract MDC value for key -->
            <!-- => %X{requestId}: prints requestId from MDC context -->
            <!-- => %X{customerId}: prints customerId from MDC context -->
            <!-- => MDC values appear after message in every log line -->
            <!-- => Empty string if MDC key not set -->
        </encoder>
    </appender>

    <root level="INFO">
        <!-- => Root logger configuration -->
        <appender-ref ref="CONSOLE" />
        <!-- => Attach console appender -->
        <!-- => All logs include MDC values per pattern -->
    </root>
</configuration>
```

**Output**:

```
2026-02-03 14:30:15 [http-nio-8080-exec-1] INFO  c.e.p.PaymentController - Processing payment request abc-123 customer-456
2026-02-03 14:30:15 [http-nio-8080-exec-1] INFO  c.e.p.PaymentService - Validating payment abc-123 customer-456
2026-02-03 14:30:16 [http-nio-8080-exec-1] INFO  c.e.p.PaymentController - Payment request completed abc-123 customer-456
```

### Request Tracing Across Services

Propagate correlation IDs across microservices for distributed tracing.

**Pattern** (with HTTP headers):

```java
import org.slf4j.MDC;
import javax.servlet.*;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.UUID;

public class RequestTrackingFilter implements Filter {
    private static final String REQUEST_ID_HEADER = "X-Request-ID";

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        HttpServletRequest httpRequest = (HttpServletRequest) request;

        // Get or generate request ID
        String requestId = httpRequest.getHeader(REQUEST_ID_HEADER);
        if (requestId == null || requestId.isEmpty()) {
            requestId = UUID.randomUUID().toString();
        }

        try {
            // Add to MDC for this request
            MDC.put("requestId", requestId);
            MDC.put("method", httpRequest.getMethod());
            MDC.put("uri", httpRequest.getRequestURI());

            // Continue processing
            chain.doFilter(request, response);

        } finally {
            MDC.clear();
        }
    }
}
```

**Propagate to downstream services**:

```java
import org.slf4j.MDC;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

public class PaymentClient {
    private final RestTemplate restTemplate;

    public void callDownstreamService(String url, Object payload) {
        // Get request ID from MDC
        String requestId = MDC.get("requestId");

        // Add to outgoing request headers
        HttpHeaders headers = new HttpHeaders();
        headers.set("X-Request-ID", requestId);
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<Object> request = new HttpEntity<>(payload, headers);

        // Call downstream service
        restTemplate.exchange(url, HttpMethod.POST, request, String.class);
    }
}
```

### JSON Logging for Log Aggregation

Output logs in JSON format for easy parsing by log aggregation systems (ELK, Splunk).

**Logback with Logstash encoder**:

**Maven dependency**:

```xml
<dependency>
    <groupId>net.logstash.logback</groupId>
    <artifactId>logstash-logback-encoder</artifactId>
    <version>7.4</version>
</dependency>
```

**logback.xml**:

```xml
<configuration>
    <!-- JSON appender for log aggregation -->
    <appender name="JSON_FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <file>logs/application.json</file>

        <rollingPolicy class="ch.qos.logback.core.rolling.TimeBasedRollingPolicy">
            <fileNamePattern>logs/application-%d{yyyy-MM-dd}.json</fileNamePattern>
            <maxHistory>30</maxHistory>
        </rollingPolicy>

        <!-- Logstash JSON encoder -->
        <encoder class="net.logstash.logback.encoder.LogstashEncoder">
            <!-- Include MDC fields -->
            <includeMdcKeyName>requestId</includeMdcKeyName>
            <includeMdcKeyName>customerId</includeMdcKeyName>
            <includeMdcKeyName>operation</includeMdcKeyName>

            <!-- Custom fields -->
            <customFields>{"application":"payment-service","environment":"production"}</customFields>
        </encoder>
    </appender>

    <root level="INFO">
        <appender-ref ref="JSON_FILE" />
    </root>
</configuration>
```

**JSON output**:

```json
{
  "@timestamp": "2026-02-03T14:30:15.123+07:00",
  "@version": "1",
  "message": "Payment processed successfully",
  "logger_name": "com.example.payment.PaymentService",
  "thread_name": "http-nio-8080-exec-1",
  "level": "INFO",
  "level_value": 20000,
  "application": "payment-service",
  "environment": "production",
  "requestId": "abc-123",
  "customerId": "customer-456",
  "operation": "payment"
}
```

## Logging Best Practices

### Log Level Selection

Choose appropriate log levels based on message importance and frequency.

**See**: [Best Practices - Logging Best Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/best-practices#logging-best-practices) for comprehensive logging level guidelines with detailed examples.

**Quick reference**:

| Level | Purpose                                | Production Volume | Example                                |
| ----- | -------------------------------------- | ----------------- | -------------------------------------- |
| ERROR | System errors requiring attention      | Low               | Database unavailable, payment failed   |
| WARN  | Unexpected but handled                 | Low-Medium        | Validation failures, retry attempts    |
| INFO  | Significant business events            | Medium            | User registered, payment completed     |
| DEBUG | Detailed diagnostic (disabled in prod) | High (dev only)   | Method parameters, intermediate values |
| TRACE | Very detailed (disabled in prod)       | Very High (dev)   | Loop iterations, fine-grained flow     |

### Performance Considerations

Optimize logging for minimal performance impact.

**Lazy evaluation with parameterized messages**:

```java
// BAD: String concatenation always happens
logger.debug("User " + userId + " performed action " + action);

// BETTER: Guard clause
if (logger.isDebugEnabled()) {
    logger.debug("User " + userId + " performed action " + action);
}

// BEST: Parameterized messages (lazy evaluation built-in)
logger.debug("User {} performed action {}", userId, action);
```

**Avoid expensive operations in log statements**:

```java
// BAD: toString() called even if DEBUG disabled
logger.debug("Processing request: {}", request.toString());

// BAD: Method call executed regardless of level
logger.debug("User details: {}", userService.getUserDetails(userId));

// BETTER: Guard expensive operations
if (logger.isDebugEnabled()) {
    logger.debug("User details: {}", userService.getUserDetails(userId));
}

// BEST: Use lazy evaluation with Supplier (Java 8+)
logger.debug("User details: {}", () -> userService.getUserDetails(userId));
```

### Security: Never Log Sensitive Data

Protect sensitive information from appearing in logs.

**Sensitive data to avoid**:

- Passwords, API keys, tokens
- Credit card numbers, CVV codes
- Personal Identifiable Information (PII): SSN, passport numbers
- Session IDs, authentication tokens
- Encryption keys, secrets

**Bad examples**:

```java
// BAD: Logging password
logger.info("User login attempt: username={}, password={}", username, password);

// BAD: Logging full credit card
logger.debug("Processing payment with card: {}", creditCardNumber);

// BAD: Logging API token
logger.info("Calling external service with token: {}", apiToken);
```

**Good examples**:

```java
// GOOD: Log username only
logger.info("User login attempt: username={}", username);

// GOOD: Log masked credit card
logger.debug("Processing payment with card: {}xxxx", creditCardNumber.substring(0, 4));

// GOOD: Don't log token at all
logger.info("Calling external service");
```

**See**: [Security Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/security-practices) for comprehensive security guidelines.

### Log Rotation and Retention

Configure appropriate rotation and retention policies.

**Guidelines**:

| Log Type  | Rotation       | Retention | Reason                           |
| --------- | -------------- | --------- | -------------------------------- |
| **Audit** | Daily          | 1-7 years | Compliance requirements          |
| **Error** | Daily or 10MB  | 90 days   | Troubleshooting recent issues    |
| **Info**  | Daily or 100MB | 30 days   | General application monitoring   |
| **Debug** | Hourly or 50MB | 7 days    | Development troubleshooting only |

**Example configuration**:

```xml
<configuration>
    <!-- Info log: 30 day retention -->
    <appender name="INFO_FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <file>logs/info.log</file>
        <rollingPolicy class="ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy">
            <fileNamePattern>logs/info-%d{yyyy-MM-dd}.%i.log</fileNamePattern>
            <maxFileSize>100MB</maxFileSize>
            <maxHistory>30</maxHistory>
            <totalSizeCap>3GB</totalSizeCap>
        </rollingPolicy>
    </appender>

    <!-- Error log: 90 day retention -->
    <appender name="ERROR_FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <filter class="ch.qos.logback.classic.filter.LevelFilter">
            <level>ERROR</level>
            <onMatch>ACCEPT</onMatch>
            <onMismatch>DENY</onMismatch>
        </filter>

        <file>logs/error.log</file>
        <rollingPolicy class="ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy">
            <fileNamePattern>logs/error-%d{yyyy-MM-dd}.%i.log</fileNamePattern>
            <maxFileSize>10MB</maxFileSize>
            <maxHistory>90</maxHistory>
        </rollingPolicy>
    </appender>
</configuration>
```

### Exception Logging

Log exceptions with full context and stack traces.

**Pattern**:

```java
public class PaymentService {
    private static final Logger logger = LoggerFactory.getLogger(PaymentService.class);

    public void processPayment(PaymentRequest request) {
        try {
            // Business logic
            chargePayment(request);

        } catch (PaymentValidationException e) {
            // Expected exception: log at WARN with context
            logger.warn("Payment validation failed: customerId={}, reason={}",
                request.getCustomerId(), e.getMessage());
            throw e;

        } catch (PaymentGatewayException e) {
            // External service exception: log at ERROR with full stack trace
            logger.error("Payment gateway error: customerId={}, gatewayResponse={}",
                request.getCustomerId(), e.getGatewayResponse(), e);
            throw new PaymentServiceException("Payment processing failed", e);

        } catch (Exception e) {
            // Unexpected exception: log at ERROR with full context
            logger.error("Unexpected error processing payment: customerId={}, request={}",
                request.getCustomerId(), request, e);
            throw new PaymentServiceException("Unexpected payment error", e);
        }
    }
}
```

**Guidelines**:

- **Expected exceptions**: Log at WARN level without stack trace (message only)
- **External service errors**: Log at ERROR level with full stack trace
- **Unexpected exceptions**: Log at ERROR level with full context and stack trace
- **Don't log and rethrow**: Log once at the appropriate layer (usually service layer)

### Contextual Information

Include relevant context in log messages for troubleshooting.

**Good contextual logging**:

```java
public class OrderService {
    private static final Logger logger = LoggerFactory.getLogger(OrderService.class);

    public void processOrder(String orderId, String customerId, List<OrderItem> items) {
        logger.info("Processing order: orderId={}, customerId={}, itemCount={}",
            orderId, customerId, items.size());

        try {
            validateOrder(items);
            calculateTotal(items);
            chargeCustomer(customerId);
            saveOrder(orderId, customerId, items);

            logger.info("Order processed successfully: orderId={}, customerId={}",
                orderId, customerId);

        } catch (ValidationException e) {
            logger.warn("Order validation failed: orderId={}, customerId={}, reason={}",
                orderId, customerId, e.getMessage());
            throw e;
        }
    }

    private void chargeCustomer(String customerId) {
        long startTime = System.currentTimeMillis();

        try {
            paymentGateway.charge(customerId);

            long duration = System.currentTimeMillis() - startTime;
            logger.debug("Customer charged successfully: customerId={}, durationMs={}",
                customerId, duration);

        } catch (PaymentException e) {
            logger.error("Payment failed: customerId={}", customerId, e);
            throw e;
        }
    }
}
```

**What to include**:

- Business identifiers (orderId, customerId, transactionId)
- Operation context (method name implicit in logger name)
- Timing information for performance-sensitive operations
- Error details and reasons for failures
- State transitions for critical operations

## Testing with Logging

Verify logging behavior in unit tests without depending on files or console output.

### Capturing Logs in Tests

Use in-memory appenders to capture log output during tests.

**Test appender** (Logback):

```java
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;

import static org.junit.jupiter.api.Assertions.*;

public class PaymentServiceTest {
    private PaymentService paymentService;
    private ListAppender<ILoggingEvent> logAppender;

    @BeforeEach
    void setUp() {
        paymentService = new PaymentService();

        // Attach list appender to capture logs
        Logger logger = (Logger) LoggerFactory.getLogger(PaymentService.class);
        logAppender = new ListAppender<>();
        logAppender.start();
        logger.addAppender(logAppender);
    }

    @Test
    void processPayment_logsSuccessMessage() {
        // When
        paymentService.processPayment("customer-123", 100.0);

        // Then
        assertEquals(2, logAppender.list.size());

        ILoggingEvent firstLog = logAppender.list.get(0);
        assertEquals("INFO", firstLog.getLevel().toString());
        assertTrue(firstLog.getFormattedMessage().contains("Processing payment"));
        assertTrue(firstLog.getFormattedMessage().contains("customer-123"));

        ILoggingEvent secondLog = logAppender.list.get(1);
        assertEquals("INFO", secondLog.getLevel().toString());
        assertTrue(secondLog.getFormattedMessage().contains("successfully"));
    }

    @Test
    void processPayment_logsWarningOnValidationFailure() {
        // When/Then
        assertThrows(IllegalArgumentException.class, () -> {
            paymentService.processPayment("customer-123", -10.0);
        });

        // Verify warning logged
        assertEquals(2, logAppender.list.size());

        ILoggingEvent warningLog = logAppender.list.get(1);
        assertEquals("WARN", warningLog.getLevel().toString());
        assertTrue(warningLog.getFormattedMessage().contains("Invalid payment"));
    }
}
```

### Verifying Log Levels

Test that appropriate log levels are used.

**Pattern**:

```java
@Test
void processPayment_usesCorrectLogLevels() {
    // When
    paymentService.processPayment("customer-123", 100.0);

    // Then - verify log level progression
    List<ILoggingEvent> logs = logAppender.list;

    // First log should be INFO (processing started)
    assertEquals("INFO", logs.get(0).getLevel().toString());

    // Last log should be INFO (processing completed)
    assertEquals("INFO", logs.get(logs.size() - 1).getLevel().toString());

    // Count DEBUG logs (should be present in test)
    long debugCount = logs.stream()
        .filter(log -> "DEBUG".equals(log.getLevel().toString()))
        .count();

    assertTrue(debugCount > 0, "Expected debug logs during processing");
}
```

### Testing MDC Context

Verify that MDC values are properly set and cleaned up.

**Pattern**:

```java
import org.slf4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class RequestHandlerTest {

    @AfterEach
    void cleanUpMDC() {
        // Ensure MDC is clean after each test
        MDC.clear();
    }

    @Test
    void handleRequest_setsMDCValues() {
        // Given
        RequestHandler handler = new RequestHandler();

        // When
        handler.handleRequest("request-123", "customer-456");

        // Then - verify MDC was set during execution
        // (This test requires refactoring handleRequest to be testable,
        // or using a callback/listener pattern to capture MDC state)
    }

    @Test
    void handleRequest_clearsMDCAfterCompletion() {
        // Given
        RequestHandler handler = new RequestHandler();

        // When
        handler.handleRequest("request-123", "customer-456");

        // Then - verify MDC is clean
        assertNull(MDC.get("requestId"));
        assertNull(MDC.get("customerId"));
    }

    @Test
    void handleRequest_clearsMDCOnException() {
        // Given
        RequestHandler handler = new RequestHandler();

        // When/Then
        assertThrows(RuntimeException.class, () -> {
            handler.handleRequestThatFails("request-123");
        });

        // Verify MDC is still clean despite exception
        assertNull(MDC.get("requestId"));
    }
}
```

## Related Content

- [Best Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/best-practices) - Comprehensive logging level guidelines and patterns
- [Security Practices](/en/learn/software-engineering/programming-languages/java/in-the-field/security-practices) - Protecting sensitive data in logs
- [Cloud-Native Patterns](/en/learn/software-engineering/programming-languages/java/in-the-field/cloud-native-patterns) - Observability and distributed tracing
- [Performance](/en/learn/software-engineering/programming-languages/java/in-the-field/performance) - Performance optimization techniques
- [Test-Driven Development](/en/learn/software-engineering/programming-languages/java/in-the-field/test-driven-development) - Testing patterns
