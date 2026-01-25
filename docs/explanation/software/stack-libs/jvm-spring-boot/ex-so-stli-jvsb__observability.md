---
title: "Spring Boot Observability"
description: Monitoring, metrics, and production readiness
category: explanation
subcategory: stack-libs
tags:
  - spring-boot
  - actuator
  - metrics
  - monitoring
  - observability
related:
  - ./ex-so-stli-jvsb__best-practices.md
last_updated: 2026-01-25
---

# Spring Boot Observability

## Overview

Production monitoring and observability with Spring Boot Actuator, Micrometer, and distributed tracing.

See [Best Practices - Observability](./ex-so-stli-jvsb__best-practices.md#observability) for implementation details.

## Key Topics

- **Spring Boot Actuator** - Production endpoints
- **Health Checks** - Liveness and readiness probes
- **Metrics** - Micrometer integration
- **Distributed Tracing** - OpenTelemetry
- **Logging** - SLF4J and structured logging
- **Custom Metrics** - Application-specific metrics

## Actuator Configuration

```yaml
management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics,prometheus
  endpoint:
    health:
      show-details: when-authorized
      probes:
        enabled: true
  metrics:
    export:
      prometheus:
        enabled: true
```

## Custom Health Indicator

```java
@Component
public class PaymentGatewayHealthIndicator implements HealthIndicator {
    private final PaymentGatewayClient client;

    @Override
    public Health health() {
        try {
            boolean healthy = client.checkHealth();
            return healthy
                ? Health.up().withDetail("gateway", "responsive").build()
                : Health.down().withDetail("gateway", "not responding").build();
        } catch (Exception ex) {
            return Health.down().withDetail("error", ex.getMessage()).build();
        }
    }
}
```

---

**Last Updated**: 2026-01-25
