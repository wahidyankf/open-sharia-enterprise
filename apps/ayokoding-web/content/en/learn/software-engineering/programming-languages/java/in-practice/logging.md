---
title: "Logging Best Practices"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Comprehensive guide to logging in Java from standard library fundamentals to production logging frameworks
weight: 10000018
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
import java.util.logging.Logger;
import java.util.logging.Level;

public class PaymentService {
    // Logger named after the class
    private static final Logger logger = Logger.getLogger(PaymentService.class.getName());

    public void processPayment(String customerId, double amount) {
        // INFO: Significant business events
        logger.info("Processing payment for customer: " + customerId);

        // FINE: Detailed diagnostic information
        logger.fine("Payment amount: " + amount);

        try {
            validateAmount(amount);
            // Process payment...

            logger.info("Payment processed successfully: " + customerId);

        } catch (IllegalArgumentException e) {
            // WARNING: Unexpected but handled
            logger.warning("Invalid payment amount for customer " + customerId + ": " + e.getMessage());
            throw e;

        } catch (Exception e) {
            // SEVERE: System errors requiring attention
            logger.log(Level.SEVERE, "Payment processing failed for customer: " + customerId, e);
            throw new PaymentException("Payment failed", e);
        }
    }

    private void validateAmount(double amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Amount must be positive");
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

# Console handler configuration
handlers=java.util.logging.ConsoleHandler

# Console handler level
java.util.logging.ConsoleHandler.level=ALL
java.util.logging.ConsoleHandler.formatter=java.util.logging.SimpleFormatter

# Package-specific levels
com.example.payment.level=FINE
com.example.database.level=WARNING

# Simple formatter pattern (Java 7+)
java.util.logging.SimpleFormatter.format=%1$tY-%1$tm-%1$td %1$tH:%1$tM:%1$tS %4$-6s %2$s %5$s%6$s%n
```

**Load configuration**:

```java
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.LogManager;

public class Application {
    static {
        // Load logging configuration at startup
        try (InputStream is = Application.class.getResourceAsStream("/logging.properties")) {
            LogManager.getLogManager().readConfiguration(is);
        } catch (IOException e) {
            System.err.println("Could not load logging configuration: " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        // Application code...
    }
}
```

### File Logging with FileHandler

Write logs to files with optional rotation.

**Pattern**:

```java
import java.util.logging.*;
import java.io.IOException;

public class ConfigureFileLogging {
    public static void main(String[] args) throws IOException {
        Logger logger = Logger.getLogger("com.example");

        // Remove default console handler
        logger.setUseParentHandlers(false);

        // Create file handler with rotation
        // %g = generation number, %u = unique number to resolve conflicts
        FileHandler fileHandler = new FileHandler(
            "application-%g.log",  // Pattern
            1024 * 1024,           // Max file size: 1MB
            5,                      // Max number of files
            true                    // Append mode
        );

        // Set formatter
        fileHandler.setFormatter(new SimpleFormatter());

        // Add handler to logger
        logger.addHandler(fileHandler);
        logger.setLevel(Level.ALL);

        // Test logging
        logger.info("Application started");
        logger.fine("Detailed information");
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

// Better: Use guard clause
if (logger.isLoggable(Level.FINE)) {
    logger.fine("Processing payment for customer " + customerId + " with amount " + amount);
}

// Best: Use SLF4J parameterized messages (shown later)
logger.debug("Processing payment for customer {} with amount {}", customerId, amount);
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
<dependency>
    <groupId>org.slf4j</groupId>
    <artifactId>slf4j-api</artifactId>
    <version>2.0.9</version>
</dependency>
```

**Basic pattern**:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PaymentService {
    // Logger named after the class
    private static final Logger logger = LoggerFactory.getLogger(PaymentService.class);

    public void processPayment(String customerId, double amount) {
        // Parameterized messages (no string concatenation!)
        logger.info("Processing payment for customer: {}", customerId);
        logger.debug("Payment details - customer: {}, amount: {}", customerId, amount);

        try {
            validateAmount(amount);

            logger.info("Payment processed successfully: customerId={}, amount={}",
                customerId, amount);

        } catch (IllegalArgumentException e) {
            logger.warn("Invalid payment: customerId={}, reason={}",
                customerId, e.getMessage());
            throw e;

        } catch (Exception e) {
            // Exception as last parameter (automatically includes stack trace)
            logger.error("Payment processing failed: customerId={}", customerId, e);
            throw new PaymentException("Payment failed", e);
        }
    }

    private void validateAmount(double amount) {
        if (amount <= 0) {
            throw new IllegalArgumentException("Amount must be positive");
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

**See**: [Best Practices](/en/learn/software-engineering/programming-languages/java/in-practice/best-practices) for comprehensive logging level guidelines with examples.

### Parameterized Messages

Parameterized messages improve performance by avoiding string concatenation when logging is disabled.

**Performance comparison**:

```java
// BAD: String concatenation always happens
logger.debug("User " + userId + " performed action " + action + " at " + timestamp);
// Problem: If DEBUG is disabled, concatenation still wastes CPU and creates garbage

// BETTER: Guard clause prevents concatenation
if (logger.isDebugEnabled()) {
    logger.debug("User " + userId + " performed action " + action + " at " + timestamp);
}
// Problem: Verbose, easy to forget

// BEST: Parameterized messages
logger.debug("User {} performed action {} at {}", userId, action, timestamp);
// Solution: Concatenation only happens if DEBUG is enabled
// No guard clause needed!
```

**How it works**:

1. If DEBUG level is disabled, SLF4J immediately returns without processing arguments
2. If DEBUG level is enabled, SLF4J formats the message using arguments
3. No string concatenation overhead when logging is disabled

### Marker API for Filtering

Use markers to classify log messages for advanced filtering.

**Pattern**:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.Marker;
import org.slf4j.MarkerFactory;

public class PaymentService {
    private static final Logger logger = LoggerFactory.getLogger(PaymentService.class);

    // Define markers for classification
    private static final Marker SECURITY = MarkerFactory.getMarker("SECURITY");
    private static final Marker PERFORMANCE = MarkerFactory.getMarker("PERFORMANCE");
    private static final Marker AUDIT = MarkerFactory.getMarker("AUDIT");

    public void processPayment(String customerId, double amount) {
        long startTime = System.nanoTime();

        // Audit log with marker
        logger.info(AUDIT, "Payment initiated: customerId={}, amount={}",
            customerId, amount);

        try {
            authenticateCustomer(customerId);
            chargePayment(customerId, amount);

            long duration = System.nanoTime() - startTime;

            // Performance log with marker
            logger.info(PERFORMANCE, "Payment processed in {}ms: customerId={}",
                duration / 1_000_000, customerId);

            // Audit log
            logger.info(AUDIT, "Payment completed: customerId={}, amount={}",
                customerId, amount);

        } catch (AuthenticationException e) {
            // Security log with marker
            logger.warn(SECURITY, "Authentication failed: customerId={}", customerId, e);
            throw e;
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
<dependency>
    <groupId>org.slf4j</groupId>
    <artifactId>slf4j-api</artifactId>
    <version>2.0.9</version>
</dependency>

<!-- Logback implementation (includes slf4j binding) -->
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.4.11</version>
</dependency>
```

**Note**: logback-classic includes logback-core and SLF4J binding automatically.

### Basic Logback Configuration

Configure Logback using logback.xml in src/main/resources.

**logback.xml** (basic configuration):

```xml
<configuration>
    <!-- Console appender -->
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>

    <!-- Root logger -->
    <root level="INFO">
        <appender-ref ref="CONSOLE" />
    </root>

    <!-- Package-specific levels -->
    <logger name="com.example.payment" level="DEBUG" />
    <logger name="com.example.database" level="WARN" />
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
    <!-- Console appender -->
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>

    <!-- Rolling file appender (size-based) -->
    <appender name="FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <file>logs/application.log</file>

        <rollingPolicy class="ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy">
            <!-- Daily rollover with size limit -->
            <fileNamePattern>logs/application-%d{yyyy-MM-dd}.%i.log</fileNamePattern>
            <maxFileSize>10MB</maxFileSize>
            <maxHistory>30</maxHistory>
            <totalSizeCap>1GB</totalSizeCap>
        </rollingPolicy>

        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>

    <!-- Error file (separate file for errors) -->
    <appender name="ERROR_FILE" class="ch.qos.logback.core.rolling.RollingFileAppender">
        <!-- Only ERROR level logs -->
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

        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n%ex</pattern>
        </encoder>
    </appender>

    <root level="INFO">
        <appender-ref ref="CONSOLE" />
        <appender-ref ref="FILE" />
        <appender-ref ref="ERROR_FILE" />
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import java.util.UUID;

public class PaymentController {
    private static final Logger logger = LoggerFactory.getLogger(PaymentController.class);
    private final PaymentService paymentService;

    public void handlePaymentRequest(PaymentRequest request) {
        // Generate unique request ID
        String requestId = UUID.randomUUID().toString();

        try {
            // Add to MDC (thread-local context)
            MDC.put("requestId", requestId);
            MDC.put("customerId", request.getCustomerId());
            MDC.put("operation", "payment");

            logger.info("Processing payment request");
            // Output: Processing payment request requestId=abc-123 customerId=customer-456 operation=payment

            paymentService.processPayment(request);

            logger.info("Payment request completed");

        } catch (Exception e) {
            logger.error("Payment request failed", e);
            throw e;

        } finally {
            // Clean up MDC (critical to prevent leaks!)
            MDC.clear();
        }
    }
}
```

**Logback configuration** (include MDC in pattern):

```xml
<configuration>
    <appender name="CONSOLE" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
            <!-- Include MDC values in output -->
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg %X{requestId} %X{customerId}%n</pattern>
        </encoder>
    </appender>

    <root level="INFO">
        <appender-ref ref="CONSOLE" />
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

**See**: [Best Practices - Logging Best Practices](/en/learn/software-engineering/programming-languages/java/in-practice/best-practices#logging-best-practices) for comprehensive logging level guidelines with detailed examples.

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

**See**: [Security Practices](/en/learn/software-engineering/programming-languages/java/in-practice/security-practices) for comprehensive security guidelines.

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

- [Best Practices](/en/learn/software-engineering/programming-languages/java/in-practice/best-practices) - Comprehensive logging level guidelines and patterns
- [Security Practices](/en/learn/software-engineering/programming-languages/java/in-practice/security-practices) - Protecting sensitive data in logs
- [Cloud-Native Patterns](/en/learn/software-engineering/programming-languages/java/in-practice/cloud-native-patterns) - Observability and distributed tracing
- [Performance](/en/learn/software-engineering/programming-languages/java/in-practice/performance) - Performance optimization techniques
- [Test-Driven Development](/en/learn/software-engineering/programming-languages/java/in-practice/test-driven-development) - Testing patterns
