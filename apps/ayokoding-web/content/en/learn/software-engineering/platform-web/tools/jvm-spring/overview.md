---
title: "Overview"
weight: 10000000
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Introduction to Spring Framework - the foundational IoC/DI framework that powers Spring Boot and enterprise Java/Kotlin applications"
tags: ["spring", "java", "kotlin", "dependency-injection", "ioc", "framework"]
---

## What is Spring Framework?

Spring Framework is the **foundational enterprise application framework** for the Java/Kotlin ecosystem. It provides comprehensive infrastructure support for developing enterprise applications through its core Inversion of Control (IoC) container, dependency injection (DI) capabilities, and extensive modular features.

**CRITICAL**: Spring Framework is the **prerequisite foundation** for Spring Boot. Understanding Spring Framework's core concepts is essential before learning Spring Boot, which builds on top of Spring Framework by providing convention-over-configuration and auto-configuration capabilities.

## Why Spring Framework Before Spring Boot?

Spring Boot has gained immense popularity for its ease of use, but it **abstracts away** many fundamental Spring Framework concepts. Learning Spring Framework first provides:

**Deep Understanding**: Understand what Spring Boot auto-configures for you
**Troubleshooting Skills**: Debug issues by understanding underlying Spring mechanics
**Flexibility**: Configure Spring applications manually when needed
**Career Foundation**: Many enterprise applications still use Spring Framework directly

**Learning Path**:

1. **Start Here** - Spring Framework (core IoC, DI, AOP, data access)
2. **Then Progress** - Spring Boot (auto-configuration, starters, production features)

## Core Concepts

### Inversion of Control (IoC) Container

The IoC container manages object lifecycle and dependencies. Instead of objects creating their own dependencies, the container injects them.

**Traditional Approach** (tight coupling):

```java
// => ZakatService creates its own dependencies
// => Hard to test, hard to change implementations
public class ZakatService {
    private ZakatRepository repository = new ZakatRepository();  // => Tight coupling

    public void calculate() {
        repository.save();  // => Depends on concrete implementation
    }
}
```

**Spring IoC Approach** (loose coupling):

```java
// => Container manages dependencies
// => Easy to test with mocks, easy to swap implementations
public class ZakatService {
    private final ZakatRepository repository;  // => Interface dependency

    // => Constructor injection - Spring provides implementation
    public ZakatService(ZakatRepository repository) {
        this.repository = repository;  // => Dependency injected by container
    }

    public void calculate() {
        repository.save();  // => Works with any implementation
    }
}
```

### Dependency Injection (DI)

Spring provides dependencies to objects through:

**Constructor Injection** (recommended):

```java
// => Dependencies injected via constructor
// => Immutable, required dependencies
public ZakatService(ZakatRepository repository, ZakatCalculator calculator) {
    this.repository = repository;      // => Final field, immutable
    this.calculator = calculator;      // => Cannot be null after construction
}
```

**Setter Injection** (optional dependencies):

```java
// => Dependencies injected via setter methods
// => Optional, can be changed after construction
public void setEmailService(EmailService emailService) {
    this.emailService = emailService;  // => Optional notification service
}
```

**Field Injection** (not recommended):

```java
// => Dependencies injected directly into fields
// => Hard to test, hides dependencies
@Autowired
private ZakatRepository repository;  // => Avoid this pattern
```

### Java-Based Configuration

Modern Spring uses Java classes (not XML) for configuration:

```java
// => @Configuration marks this as Spring config class
@Configuration
@ComponentScan(basePackages = "com.ayokoding.zakat")  // => Scan for @Component classes
public class AppConfig {

    // => @Bean declares a Spring-managed bean
    // => Method name becomes bean ID (zakatRepository)
    @Bean
    public ZakatRepository zakatRepository() {
        return new JdbcZakatRepository();  // => Returns bean instance
    }                                       // => Spring manages lifecycle

    // => Spring automatically injects zakatRepository parameter
    @Bean
    public ZakatService zakatService(ZakatRepository repository) {
        return new ZakatService(repository);  // => Dependencies injected
    }                                          // => Constructor receives repository bean
}
```

### Component Scanning

Spring can automatically discover and register beans:

```java
// => @Component marks class for auto-detection
// => Spring creates bean automatically
@Component
public class ZakatCalculator {
    public BigDecimal calculate(BigDecimal wealth) {
        return wealth.multiply(new BigDecimal("0.025"));  // => 2.5% calculation
    }                                                     // => Zakat nisab rate
}
```

**Common Stereotypes**:

- `@Component` - Generic Spring-managed component
- `@Service` - Business logic layer
- `@Repository` - Data access layer
- `@Controller` - Web presentation layer

## Key Modules

### Spring Core Container

**Purpose**: IoC container, dependency injection, bean lifecycle management

**Key Interfaces**:

- `ApplicationContext` - Central interface for application configuration
- `BeanFactory` - Basic container for bean management

### Spring Data Access

**Purpose**: Database access, transaction management, ORM integration

**Technologies**:

- **JdbcTemplate** - Simplified JDBC operations
- **Transactions** - Declarative transaction management with `@Transactional`
- **ORM Integration** - Hibernate, JPA support

**Example** (JdbcTemplate):

```java
// => JdbcTemplate simplifies JDBC operations
// => Handles connection management, exception translation
public class JdbcZakatRepository implements ZakatRepository {
    private final JdbcTemplate jdbcTemplate;  // => Spring-provided template

    public JdbcZakatRepository(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;      // => Constructor injection
    }

    // => Query database with automatic resource management
    public List<ZakatRecord> findAll() {
        return jdbcTemplate.query(                      // => Execute query
            "SELECT id, amount FROM zakat_records",     // => SQL query
            (rs, rowNum) -> new ZakatRecord(            // => RowMapper lambda
                rs.getLong("id"),                       // => Map result set to object
                rs.getBigDecimal("amount")              // => Extract columns
            )
        );  // => Returns List<ZakatRecord> automatically
    }      // => Closes connection automatically
}
```

### Spring AOP (Aspect-Oriented Programming)

**Purpose**: Cross-cutting concerns (logging, security, transactions)

**Example** (Transactions):

```java
// => @Transactional declares transaction boundaries
// => Spring creates proxy to manage transactions
@Service
public class ZakatService {

    @Transactional  // => Method executes within transaction
                    // => Auto-commit on success, rollback on exception
    public void processZakat(BigDecimal amount) {
        zakatRepository.save(amount);       // => DB operation 1
        notificationService.send(amount);   // => DB operation 2
    }  // => Both operations commit together or rollback together
}
```

### Spring Web MVC

**Purpose**: Web application development with Model-View-Controller pattern

**Example**:

```java
// => @Controller marks web controller
// => Handles HTTP requests
@Controller
public class ZakatController {
    private final ZakatService zakatService;  // => Service layer dependency

    // => @GetMapping maps HTTP GET to method
    @GetMapping("/zakat/calculate")
    public String calculateZakat(
        @RequestParam BigDecimal wealth,  // => Extract query parameter
        Model model                       // => Spring-provided model
    ) {
        BigDecimal zakat = zakatService.calculate(wealth);  // => Business logic
        model.addAttribute("zakat", zakat);                 // => Add to model
        return "zakat-result";                              // => View name
    }                                                       // => Renders zakat-result.html
}
```

## When to Use Spring Framework vs Spring Boot

### Use Spring Framework When

**Full Control Needed**: Complex enterprise applications requiring manual configuration
**Legacy Integration**: Working with existing Spring Framework applications
**Learning Foundation**: Understanding how Spring works under the hood
**Custom Infrastructure**: Building custom frameworks or libraries

### Use Spring Boot When

**Rapid Development**: Quick startup for microservices and web applications
**Convention Over Configuration**: Prefer sensible defaults over manual setup
**Production Features**: Need built-in monitoring, health checks, externalized config
**Modern Applications**: Building new cloud-native applications

**Recommendation**: Learn Spring Framework first, then leverage Spring Boot's productivity features with deep understanding.

## Prerequisites

### Technical Requirements

**Java Development Kit**:

- Java 17+ (LTS version recommended)
- OR Kotlin 1.9+ for Kotlin-based development

**Build Tool**:

- Maven 3.9+ or Gradle 8.0+

**IDE** (recommended):

- IntelliJ IDEA Community/Ultimate
- Eclipse with Spring Tools
- VS Code with Java extensions

### Knowledge Prerequisites

**Required Knowledge**:

- Object-Oriented Programming (OOP) concepts
- Java/Kotlin syntax and core libraries
- Maven or Gradle basics
- SQL basics (for data access tutorials)

**Helpful Background**:

- Design patterns (Singleton, Factory, Proxy)
- Web basics (HTTP, REST) for web MVC tutorials

## What You'll Learn

This tutorial series covers:

**Core Container**: IoC, dependency injection, Java-based configuration, component scanning
**Data Access**: JdbcTemplate, transaction management, exception handling
**Web MVC**: Controllers, request mapping, model-view separation
**AOP**: Cross-cutting concerns, declarative transactions
**Testing**: Unit testing with Spring Test framework

All examples use **Islamic finance scenarios** (Zakat calculation, Murabaha contracts, Sadaqah tracking) to demonstrate concepts in real-world context.

## Example Application Preview

Throughout these tutorials, you'll build a **Zakat Calculator Application** demonstrating:

**Business Logic**:

- Calculate Zakat (2.5% of qualifying wealth)
- Track Zakat payments
- Generate reports

**Spring Features**:

- IoC container configuration
- Dependency injection (constructor, setter)
- Data access with JdbcTemplate
- Transaction management
- Web MVC for user interface

**Both Java and Kotlin**: All examples provided in both languages for flexibility.

## Islamic Finance Context

### What is Zakat?

Zakat is one of the Five Pillars of Islam - an obligatory charitable contribution on wealth. Muslims with wealth above the nisab threshold (minimum amount) must pay 2.5% annually to eligible recipients.

**Key Concepts**:

- **Nisab**: Minimum wealth threshold (approximately 85 grams of gold equivalent)
- **Hawl**: One lunar year of ownership
- **Zakat Rate**: 2.5% of qualifying wealth

### What is Murabaha?

Murabaha is a Shariah-compliant financing structure where the financier purchases an asset and sells it to the customer at cost plus disclosed profit margin, payable in installments.

**Example**: Bank purchases equipment for 100,000, sells to customer for 110,000 (disclosed 10% profit) payable over 12 months.

## Next Steps

Ready to start? Proceed to:

**[Initial Setup](/en/learn/software-engineering/platform-web/tools/jvm-spring/initial-setup)** - Install Java/Kotlin, set up build tools, create project structure

**[Quick Start](/en/learn/software-engineering/platform-web/tools/jvm-spring/quick-start)** - Build a complete Zakat Calculator application in 30 minutes

**[By Example](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example)** - Learn through 75-90 heavily annotated code examples
