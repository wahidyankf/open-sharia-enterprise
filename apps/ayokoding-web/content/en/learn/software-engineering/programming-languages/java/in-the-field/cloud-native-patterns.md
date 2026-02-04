---
title: "Cloud Native Patterns"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Build production-ready cloud-native Java applications with health checks, metrics, configuration, and fault tolerance
weight: 10000027
tags: ["java", "cloud-native", "microprofile", "observability", "twelve-factor", "microservices"]
---

## Understanding Cloud-Native Java

Cloud-native applications are designed for dynamic, distributed cloud environments. They embrace microservices architecture, containerization, continuous deployment, and observable operation.

**Why cloud-native matters:**

- **Scalability**: Horizontal scaling with stateless services
- **Resilience**: Fault tolerance and graceful degradation
- **Observability**: Health checks, metrics, and distributed tracing
- **Portability**: Run anywhere (on-premises, cloud, hybrid)

This guide covers essential cloud-native patterns using MicroProfile and Spring Boot standards.

## Health Checks - Service Readiness Verification

**Problem**: Orchestrators (Kubernetes, Docker Swarm) need to know if service instances are healthy. Routing traffic to unhealthy instances causes failures.

**Recognition signals:**

- No way to verify service health programmatically
- Load balancer sends traffic to failed instances
- Slow startup causes "connection refused" errors
- Dependencies failure goes undetected
- Manual health verification needed

**Solution**: Expose health endpoints for liveness and readiness checks.

| Check Type | Purpose                  | Failure Action              |
| ---------- | ------------------------ | --------------------------- |
| Liveness   | Is service running?      | Restart container           |
| Readiness  | Ready to accept traffic? | Remove from load balancer   |
| Startup    | Has service started?     | Wait before liveness checks |

**Example (MicroProfile Health):**

```java
import org.eclipse.microprofile.health.*;
import jakarta.enterprise.context.ApplicationScoped;

@Liveness
@ApplicationScoped
public class LivenessCheck implements HealthCheck {
    @Override
    public HealthCheckResponse call() {
        // CHECK: Can service process requests?
        boolean isAlive = checkInternalState();

        return HealthCheckResponse
            .named("service-liveness")
            .status(isAlive)
            .withData("uptime", getUptimeSeconds())
            .build();
    }

    private boolean checkInternalState() {
        // Verify critical components
        return threadPoolHealthy() && memoryAvailable();
    }
}

@Readiness
@ApplicationScoped
public class ReadinessCheck implements HealthCheck {
    @Inject
    DatabaseConnection database;

    @Inject
    CacheConnection cache;

    @Override
    public HealthCheckResponse call() {
        // CHECK: Are dependencies available?
        boolean databaseReady = database.ping();
        boolean cacheReady = cache.ping();

        return HealthCheckResponse
            .named("service-readiness")
            .status(databaseReady && cacheReady)
            .withData("database", databaseReady)
            .withData("cache", cacheReady)
            .build();
    }
}
```

**Kubernetes integration:**

```yaml
apiVersion: v1
kind: Pod
spec:
  containers:
    - name: app
      image: myapp:latest
      livenessProbe:
        httpGet:
          path: /health/live
          port: 8080
        initialDelaySeconds: 30 # Wait for startup
        periodSeconds: 10 # Check every 10s
      readinessProbe:
        httpGet:
          path: /health/ready
          port: 8080
        initialDelaySeconds: 5
        periodSeconds: 5
      startupProbe:
        httpGet:
          path: /health/started
          port: 8080
        failureThreshold: 30 # 30 * 10s = 5min max startup
        periodSeconds: 10
```

**Benefits:**

- Automatic restart of failed instances
- No traffic to unready services
- Graceful handling of slow startups

## Metrics - Observable System State

**Problem**: Without metrics, you can't measure performance, detect anomalies, or capacity plan. Troubleshooting requires guessing.

**Recognition signals:**

- No visibility into request rates, latency, errors
- Performance issues discovered by users
- No capacity planning data
- Cannot prove SLA compliance
- Troubleshooting requires adding logging retroactively

**Solution**: Expose standardized metrics for monitoring systems.

### Key Metric Types

| Type      | Purpose                        | Examples                              |
| --------- | ------------------------------ | ------------------------------------- |
| Counter   | Monotonically increasing count | Requests total, errors total          |
| Gauge     | Current value                  | Active connections, memory usage      |
| Histogram | Distribution of values         | Request duration, response size       |
| Timer     | Rate and duration              | Request rate, 95th percentile latency |

**Example (MicroProfile Metrics):**

```java
import org.eclipse.microprofile.metrics.*;
import org.eclipse.microprofile.metrics.annotation.*;
import jakarta.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class OrderService {
    @Inject
    MetricRegistry registry;

    @Counted(name = "orders_created_total", description = "Total orders created")
    @Timed(name = "order_creation_duration", description = "Order creation duration")
    public Order createOrder(OrderRequest request) {
        // METRICS: Automatically tracked
        return processOrder(request);
    }

    @Gauge(name = "active_orders", unit = MetricUnits.NONE,
           description = "Currently active orders")
    public long getActiveOrderCount() {
        return orderRepository.countActive();
    }

    public void recordPaymentProcessing(long durationMillis) {
        // CUSTOM HISTOGRAM
        Histogram paymentDuration = registry.histogram(
            Metadata.builder()
                .withName("payment_processing_duration")
                .withDescription("Payment processing time in milliseconds")
                .build()
        );
        paymentDuration.update(durationMillis);
    }
}
```

**Prometheus exposition format:**

```
# HELP orders_created_total Total orders created
# TYPE orders_created_total counter
orders_created_total 1547

# HELP order_creation_duration_seconds Order creation duration
# TYPE order_creation_duration_seconds summary
order_creation_duration_seconds_count 1547
order_creation_duration_seconds_sum 45.234
order_creation_duration_seconds{quantile="0.5"} 0.023
order_creation_duration_seconds{quantile="0.95"} 0.087
order_creation_duration_seconds{quantile="0.99"} 0.154

# HELP active_orders Currently active orders
# TYPE active_orders gauge
active_orders 23
```

### RED Method (Request-based Services)

Monitor three key metrics for every service:

| Metric       | Meaning                  | Alert Threshold         |
| ------------ | ------------------------ | ----------------------- |
| **R**ate     | Requests per second      | Spike or drop 50%+      |
| **E**rrors   | Error rate (%)           | > 1%                    |
| **D**uration | Response time (p95, p99) | p95 > SLA, p99 > 2x SLA |

```java
@ApplicationScoped
public class MetricsCollector {
    @Inject
    MetricRegistry registry;

    public void recordRequest(long durationMillis, boolean success) {
        // RATE: Request counter
        Counter requests = registry.counter("http_requests_total");
        requests.inc();

        // ERRORS: Error counter
        if (!success) {
            Counter errors = registry.counter("http_requests_errors_total");
            errors.inc();
        }

        // DURATION: Response time histogram
        Histogram duration = registry.histogram("http_request_duration_milliseconds");
        duration.update(durationMillis);
    }
}
```

## Configuration - Externalized Settings

**Problem**: Hardcoded configuration requires recompilation for environment changes. Secrets in source code create security risks.

**Recognition signals:**

- Database URLs hardcoded
- Different builds for each environment
- Secrets committed to version control
- Cannot change configuration without redeployment
- Configuration scattered across code

**Solution**: Externalize configuration, inject at runtime.

### Twelve-Factor Configuration

**Principle**: Store config in environment (separate from code).

```java
import org.eclipse.microprofile.config.inject.ConfigProperty;
import jakarta.inject.Inject;

@ApplicationScoped
public class DatabaseService {
    @Inject
    @ConfigProperty(name = "database.url")
    String databaseUrl;

    @Inject
    @ConfigProperty(name = "database.pool.size", defaultValue = "10")
    int poolSize;

    @Inject
    @ConfigProperty(name = "database.username")
    String username;

    @Inject
    @ConfigProperty(name = "database.password")
    String password;  // INJECT: Never hardcode

    public Connection getConnection() {
        // USE: Injected configuration
        return DriverManager.getConnection(databaseUrl, username, password);
    }
}
```

**Configuration sources (priority order):**

1. System properties (`-Ddatabase.url=...`)
2. Environment variables (`DATABASE_URL=...`)
3. application.properties file
4. Default values (`defaultValue = "10"`)

**Example: Environment-specific configuration**

```properties
# application.properties (defaults)
database.pool.size=10
cache.ttl=3600

# dev environment: Override via environment variables
DATABASE_URL=jdbc:postgresql://localhost:5432/dev
DATABASE_USERNAME=dev_user
DATABASE_PASSWORD=dev_password

# prod environment: Override via Kubernetes secrets
DATABASE_URL=jdbc:postgresql://prod-db:5432/production
DATABASE_USERNAME=prod_user
DATABASE_PASSWORD=${DB_PASSWORD}  # From Kubernetes secret
```

**Kubernetes secret injection:**

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: database-credentials
type: Opaque
data:
  password: <base64-encoded-password>
---
apiVersion: v1
kind: Pod
spec:
  containers:
    - name: app
      image: myapp:latest
      env:
        - name: DATABASE_PASSWORD
          valueFrom:
            secretKeyRef:
              name: database-credentials
              key: password
```

## Fault Tolerance - Graceful Degradation

**Problem**: Distributed systems have partial failures. Without fault tolerance, one failing service cascades to entire system.

**Solution**: Circuit breakers, timeouts, retries, and fallbacks.

**MicroProfile Fault Tolerance:**

```java
import org.eclipse.microprofile.faulttolerance.*;
import java.time.temporal.ChronoUnit;

@ApplicationScoped
public class ExternalServiceClient {
    @Retry(
        maxRetries = 3,
        delay = 100,
        delayUnit = ChronoUnit.MILLIS,
        jitter = 50
    )
    @Timeout(value = 2, unit = ChronoUnit.SECONDS)
    @CircuitBreaker(
        requestVolumeThreshold = 10,
        failureRatio = 0.5,
        delay = 10000
    )
    @Fallback(fallbackMethod = "getCachedData")
    public String fetchData(String key) {
        // PROTECTED: Retry + Timeout + Circuit Breaker + Fallback
        return externalService.get(key);
    }

    public String getCachedData(String key) {
        // FALLBACK: Return cached data if available
        String cached = cache.get(key);
        return cached != null ? cached : "Service temporarily unavailable";
    }

    @Bulkhead(value = 10, waitingTaskQueue = 20)
    public void processRequest(Request request) {
        // RATE LIMIT: Max 10 concurrent, 20 queued
        // Prevents resource exhaustion
    }
}
```

**Annotation effects:**

| Annotation        | Effect                        | Configuration              |
| ----------------- | ----------------------------- | -------------------------- |
| `@Retry`          | Retry failed operations       | max retries, delay, jitter |
| `@Timeout`        | Abort long-running operations | duration                   |
| `@CircuitBreaker` | Stop calling failing service  | failure threshold, delay   |
| `@Fallback`       | Provide alternative result    | fallback method            |
| `@Bulkhead`       | Limit concurrent executions   | max concurrent, queue size |

## Distributed Tracing - Request Flow Visibility

**Problem**: In microservices, single request spans multiple services. Troubleshooting requires correlating logs across services.

**Solution**: Distributed tracing propagates trace context across service boundaries.

```java
import io.opentelemetry.api.trace.*;
import io.opentelemetry.context.Context;

@ApplicationScoped
public class OrderService {
    @Inject
    Tracer tracer;

    public Order createOrder(OrderRequest request) {
        // CREATE SPAN: Track operation
        Span span = tracer.spanBuilder("create-order")
            .setSpanKind(SpanKind.SERVER)
            .startSpan();

        try (var scope = span.makeCurrent()) {
            span.setAttribute("order.id", request.getId());
            span.setAttribute("order.items", request.getItems().size());

            // NESTED OPERATION: Child span
            validateOrder(request);

            // EXTERNAL CALL: Propagate trace context
            paymentService.processPayment(request);

            span.setStatus(StatusCode.OK);
            return saveOrder(request);
        } catch (Exception e) {
            span.recordException(e);
            span.setStatus(StatusCode.ERROR, e.getMessage());
            throw e;
        } finally {
            span.end();
        }
    }

    private void validateOrder(OrderRequest request) {
        Span span = tracer.spanBuilder("validate-order")
            .startSpan();
        try (var scope = span.makeCurrent()) {
            // Validation logic
        } finally {
            span.end();
        }
    }
}
```

**Trace visualization:**

```
TraceID: abc123
├─ create-order (200ms)
   ├─ validate-order (10ms)
   ├─ payment-service.process (150ms)
   │  ├─ database.query (50ms)
   │  └─ external-api.call (100ms)
   └─ database.save (40ms)
```

## Twelve-Factor App Checklist

| Factor                 | Description                          | Implementation             |
| ---------------------- | ------------------------------------ | -------------------------- |
| I. Codebase            | One codebase, many deploys           | Git repository             |
| II. Dependencies       | Explicitly declared dependencies     | Maven/Gradle               |
| III. Config            | Store config in environment          | MicroProfile Config        |
| IV. Backing services   | Treat as attached resources          | Injected dependencies      |
| V. Build, release, run | Strict separation                    | CI/CD pipeline             |
| VI. Processes          | Stateless processes                  | Horizontal scaling         |
| VII. Port binding      | Export services via port             | Embedded server (Undertow) |
| VIII. Concurrency      | Scale out via process model          | Container orchestration    |
| IX. Disposability      | Fast startup, graceful shutdown      | Health checks              |
| X. Dev/prod parity     | Keep environments similar            | Containers                 |
| XI. Logs               | Treat logs as event streams          | stdout, log aggregation    |
| XII. Admin processes   | Run admin tasks as one-off processes | Management endpoints       |

## Guidelines

**When to use cloud-native patterns:**

- ✓ Microservices architectures
- ✓ Container deployments (Docker, Kubernetes)
- ✓ Distributed systems
- ✓ Production environments requiring high availability

**When to simplify:**

- ✗ Monolithic applications
- ✗ Single-server deployments
- ✗ Development/test environments
- ✗ Simple CRUD applications

**Best practices:**

1. **Implement all health checks**: Liveness, readiness, startup
2. **Expose key metrics**: RED method (rate, errors, duration)
3. **Externalize all config**: No hardcoded URLs, credentials
4. **Add fault tolerance**: Timeouts, retries, circuit breakers
5. **Enable distributed tracing**: Correlate requests across services

## Conclusion

Cloud-native Java requires:

- **Health checks**: Service readiness verification
- **Metrics**: Observable system state (RED method)
- **Configuration**: Externalized, environment-specific settings
- **Fault tolerance**: Graceful degradation under failure
- **Distributed tracing**: Request flow visibility

MicroProfile and Spring Boot provide standardized APIs for cloud-native patterns. Adopt incrementally: start with health checks and metrics (observability), then add configuration externalization (portability), and finally implement fault tolerance (resilience). Cloud-native patterns enable reliable, scalable, observable production systems.
