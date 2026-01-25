---
title: "Spring Boot Configuration"
description: Configuration management and externalized settings in Spring Boot
category: explanation
subcategory: platform-web
tags:
  - spring-boot
  - configuration
  - properties
  - profiles
  - externalized-config
related:
  - ./ex-so-plwe-jvspbo__idioms.md
  - ./ex-so-plwe-jvspbo__best-practices.md
principles:
  - explicit-over-implicit
  - reproducibility
last_updated: 2026-01-25
---

# Spring Boot Configuration

## Quick Reference

### Configuration Sources

- [application.yml](#applicationyml-structure) - Main configuration
- [Profiles](#profile-based-configuration) - Environment-specific settings
- [Environment Variables](#environment-variables) - Externalized config
- [@ConfigurationProperties](#configurationproperties) - Type-safe properties

### Related Documentation

- [Best Practices](./ex-so-plwe-jvspbo__best-practices.md#configuration-management)
- [Idioms](./ex-so-plwe-jvspbo__idioms.md#externalized-configuration)

## Overview

Spring Boot provides flexible configuration management through properties files, environment variables, and type-safe configuration classes. This guide covers configuration best practices for the open-sharia-enterprise platform.

## application.yml Structure

```yaml
# Application identity
spring:
  application:
    name: payment-service
  profiles:
    active: ${SPRING_PROFILES_ACTIVE:dev}

# Server configuration
server:
  port: ${SERVER_PORT:8080}
  compression:
    enabled: true
    min-response-size: 1024

# Database (externalized)
spring:
  datasource:
    url: ${DATABASE_URL:jdbc:postgresql://localhost:5432/ose}
    username: ${DATABASE_USERNAME:dev_user}
    password: ${DATABASE_PASSWORD:dev_password}
    hikari:
      maximum-pool-size: ${DB_POOL_SIZE:10}
      minimum-idle: 5

# JPA configuration
spring:
  jpa:
    hibernate:
      ddl-auto: validate
    properties:
      hibernate:
        dialect: org.hibernate.dialect.PostgreSQLDialect
    show-sql: false

# Flyway migrations
spring:
  flyway:
    enabled: true
    locations: classpath:db/migration
    baseline-on-migrate: true

# Security (secrets externalized)
ose:
  security:
    jwt:
      secret: ${JWT_SECRET}
      expiration-ms: ${JWT_EXPIRATION_MS:3600000}
      issuer: ose-platform

# Actuator
management:
  endpoints:
    web:
      exposure:
        include: ${ACTUATOR_ENDPOINTS:health,info,metrics}
  endpoint:
    health:
      show-details: when-authorized

# Logging
logging:
  level:
    root: INFO
    com.oseplatform: DEBUG
    org.springframework.web: INFO
    org.hibernate.SQL: DEBUG
```

## Profile-Based Configuration

### Development Profile

```yaml
# application-dev.yml
spring:
  jpa:
    show-sql: true
    properties:
      hibernate:
        format_sql: true
  devtools:
    restart:
      enabled: true

logging:
  level:
    com.oseplatform: DEBUG
    org.hibernate.SQL: DEBUG
    org.hibernate.type.descriptor.sql.BasicBinder: TRACE

# Development features
ose:
  features:
    email-notifications: false
    sms-notifications: false
```

### Production Profile

```yaml
# application-prod.yml
spring:
  jpa:
    show-sql: false
  datasource:
    hikari:
      maximum-pool-size: 20
      minimum-idle: 10

logging:
  level:
    root: WARN
    com.oseplatform: INFO

# Production features
ose:
  features:
    email-notifications: true
    sms-notifications: true

management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics,prometheus
```

## @ConfigurationProperties

```java
@ConfigurationProperties(prefix = "ose.zakat")
@Validated
public class ZakatProperties {

    @NotNull
    @DecimalMin("0.001")
    @DecimalMax("1.0")
    private BigDecimal nisabPercentage = new BigDecimal("0.025");

    @Min(1)
    @Max(366)
    private int hawalDays = 354;

    @NotNull
    private Currency defaultCurrency = Currency.getInstance("USD");

    @Valid
    private Notifications notifications = new Notifications();

    public static class Notifications {
        private boolean emailEnabled = true;
        private boolean smsEnabled = false;
        private String fromEmail = "noreply@oseplatform.com";

        // Getters and setters
    }

    // Getters and setters
}

@Configuration
@EnableConfigurationProperties(ZakatProperties.class)
public class ZakatConfig {
    // Properties now available for injection
}
```

**application.yml**:

```yaml
ose:
  zakat:
    nisab-percentage: 0.025
    hawal-days: 354
    default-currency: USD
    notifications:
      email-enabled: true
      sms-enabled: false
      from-email: noreply@oseplatform.com
```

## Environment Variables

### Local Development (.env)

```bash
# Database
DATABASE_URL=jdbc:postgresql://localhost:5432/ose_platform
DATABASE_USERNAME=dev_user
DATABASE_PASSWORD=dev_password

# Security
JWT_SECRET=dev-secret-key-change-in-production
JWT_EXPIRATION_MS=3600000

# Profiles
SPRING_PROFILES_ACTIVE=dev
```

### Production Environment

```bash
# Kubernetes ConfigMap/Secrets
DATABASE_URL=jdbc:postgresql://prod-db.cluster.local:5432/ose_platform
DATABASE_USERNAME=prod_user
DATABASE_PASSWORD=${VAULT_DB_PASSWORD}

JWT_SECRET=${VAULT_JWT_SECRET}
JWT_EXPIRATION_MS=1800000

SPRING_PROFILES_ACTIVE=prod
SERVER_PORT=8080
```

## Configuration Validation

```java
@ConfigurationProperties(prefix = "ose.payment.gateway")
@Validated
public class PaymentGatewayProperties {

    @NotBlank(message = "API URL is required")
    @URL(message = "API URL must be valid")
    private String apiUrl;

    @NotBlank(message = "API key is required")
    private String apiKey;

    @Min(value = 1000, message = "Timeout must be at least 1000ms")
    @Max(value = 60000, message = "Timeout cannot exceed 60000ms")
    private int timeoutMs = 30000;

    @Min(1)
    @Max(10)
    private int maxRetries = 3;

    // Getters and setters
}
```

**Application fails to start if validation fails**:

```
***************************
APPLICATION FAILED TO START
***************************

Description:

Binding to target org.springframework.boot.context.properties.bind.BindException:
Failed to bind properties under 'ose.payment.gateway' to PaymentGatewayProperties

Reason: Field error in object 'ose.payment.gateway' on field 'apiUrl':
rejected value [invalid-url]; API URL must be valid
```

## Configuration Precedence

Spring Boot loads configuration in this order (later sources override earlier):

1. Default properties (in code)
2. @PropertySource annotations
3. application.properties/yml
4. Profile-specific application-{profile}.yml
5. OS environment variables
6. Java system properties (-D flags)
7. Command line arguments

## Related Documentation

- **[Idioms](./ex-so-plwe-jvspbo__idioms.md)** - Configuration patterns
- **[Best Practices](./ex-so-plwe-jvspbo__best-practices.md)** - Configuration management
- **[Anti-Patterns](./ex-so-plwe-jvspbo__anti-patterns.md)** - Configuration mistakes

---

**Last Updated**: 2026-01-25
**Spring Boot Version**: 3.3+
