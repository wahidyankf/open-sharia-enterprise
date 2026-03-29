---
title: "Spring Boot Actuator"
date: 2026-02-06T00:00:00+07:00
draft: false
weight: 1000050
description: "Manual management endpoints → /actuator auto-configuration for production monitoring and management"
tags: ["spring-boot", "in-the-field", "production", "actuator", "monitoring"]
---

## Why Actuator Matters

Spring Boot Actuator provides production-ready management endpoints (/health, /metrics, /info) eliminating custom monitoring code. In production systems requiring health checks, metrics collection, and runtime configuration visibility, Actuator exposes 20+ HTTP endpoints out-of-the-box—enabling Kubernetes liveness probes, Prometheus scraping, and operational dashboards without implementation.

**Problem**: Production systems need health checks, metrics, and management endpoints.

**Solution**: Spring Boot Actuator auto-configures /actuator/\* endpoints for monitoring and management.

## Manual Health Endpoint

```java
@RestController
public class HealthEndpoint {

    @Autowired
    private DataSource dataSource;

    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> health = new HashMap<>();

        // => Manual database check
        try (Connection conn = dataSource.getConnection()) {
            health.put("database", "UP");
        } catch (SQLException e) {
            health.put("database", "DOWN");
            health.put("status", "DOWN");
            return health;
        }

        // => Manual disk space check
        File root = new File("/");
        long freeSpace = root.getFreeSpace();
        health.put("diskSpace", freeSpace > 10_000_000 ? "UP" : "DOWN");

        health.put("status", "UP");
        return health;
    }
}
```

**Limitations**: Manual checks, no standard format, no composite health indicators.

## Spring Boot Actuator

**Step 1: Add dependency**:

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-actuator</artifactId>
</dependency>
```

**Step 2: Configure endpoints**:

```yaml
management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics,prometheus
        # => Expose specific endpoints (security-conscious)
        # => Production: selective exposure (not "*")
      base-path: /actuator # => Base path for all endpoints

  endpoint:
    health:
      show-details: when-authorized
      # => Show details only for authorized users
      # => Production: hide internal details from public
      probes:
        enabled: true # => Kubernetes liveness/readiness probes

  health:
    livenessState:
      enabled: true
    readinessState:
      enabled: true
```

**Step 3: Access endpoints**:

```bash
# => Health check (Kubernetes liveness probe)
curl http://localhost:8080/actuator/health
# => {"status":"UP"}

# => Detailed health (authenticated)
curl -H "Authorization: Bearer $TOKEN" http://localhost:8080/actuator/health
# => {
#   "status": "UP",
#   "components": {
#     "db": { "status": "UP", "details": { "database": "PostgreSQL", "validationQuery": "isValid()" } },
#     "diskSpace": { "status": "UP", "details": { "total": 500GB, "free": 200GB } }
#   }
# }

# => Metrics
curl http://localhost:8080/actuator/metrics
# => {"names": ["jvm.memory.used", "http.server.requests", ...]}

curl http://localhost:8080/actuator/metrics/jvm.memory.used
# => {"name": "jvm.memory.used", "measurements": [{"statistic": "VALUE", "value": 512MB}]}
```

## Available Actuator Endpoints

| Endpoint    | Purpose                | Production Use                  |
| ----------- | ---------------------- | ------------------------------- |
| /health     | Health status          | Kubernetes liveness/readiness   |
| /info       | Application info       | Version, git commit, build time |
| /metrics    | Metrics collection     | Prometheus scraping             |
| /prometheus | Prometheus format      | Direct scraping                 |
| /env        | Environment properties | Configuration verification      |
| /loggers    | Log level management   | Runtime log level changes       |
| /threaddump | Thread dump            | Debugging production issues     |
| /heapdump   | Heap dump              | Memory leak analysis            |
| /mappings   | Request mappings       | API documentation               |

## Custom Health Indicator

```java
@Component
public class ZakatServiceHealthIndicator implements HealthIndicator {

    @Autowired
    private ZakatCalculationService calculationService;

    @Override
    public Health health() {
        try {
            // => Custom health check: verify service works
            BigDecimal testResult = calculationService.calculateNisab();

            if (testResult.compareTo(BigDecimal.ZERO) > 0) {
                return Health.up()
                    .withDetail("nisab", testResult)
                    .withDetail("service", "operational")
                    .build();  // => Status: UP
            } else {
                return Health.down()
                    .withDetail("error", "Invalid nisab calculation")
                    .build();  // => Status: DOWN
            }
        } catch (Exception e) {
            return Health.down(e)
                .withDetail("error", e.getMessage())
                .build();  // => Status: DOWN with exception details
        }
    }
}
```

## Kubernetes Integration

```yaml
# => Kubernetes deployment with Actuator health probes
apiVersion: apps/v1
kind: Deployment
metadata:
  name: zakat-api
spec:
  template:
    spec:
      containers:
        - name: zakat-api
          image: zakat-api:1.0.0
          ports:
            - containerPort: 8080

          # => Liveness probe: restart if fails
          livenessProbe:
            httpGet:
              path: /actuator/health/liveness
              port: 8080
            initialDelaySeconds: 30
            periodSeconds: 10
            timeoutSeconds: 5
            failureThreshold: 3

          # => Readiness probe: remove from service if fails
          readinessProbe:
            httpGet:
              path: /actuator/health/readiness
              port: 8080
            initialDelaySeconds: 10
            periodSeconds: 5
            timeoutSeconds: 3
            failureThreshold: 3
```

## Prometheus Integration

```yaml
# => application.yml
management:
  metrics:
    export:
      prometheus:
        enabled: true # => Enable Prometheus format
  endpoints:
    web:
      exposure:
        include: prometheus # => Expose /actuator/prometheus
```

**Prometheus scrape configuration**:

```yaml
# => prometheus.yml
scrape_configs:
  - job_name: "spring-boot-zakat-api"
    metrics_path: "/actuator/prometheus"
    static_configs:
      - targets: ["zakat-api:8080"]
```

**Trade-offs**: Actuator provides production essentials. Custom monitoring for business-specific metrics.

## Next Steps

- [Health Checks](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/health-checks) - Advanced health indicators
- [Metrics Monitoring](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/metrics-monitoring) - Micrometer and Prometheus
- [Graceful Shutdown](/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/graceful-shutdown) - Zero-downtime deployments
