---
title: Java Templates
description: Collection of code templates for Java development including testing, domain models, and build configurations
category: explanation
subcategory: stack-lang
tags:
  - java
  - templates
  - ddd
  - testing
  - build
created: 2026-01-21
updated: 2026-01-25
---

# Java Templates

This directory contains standardized code templates for Java development, designed to accelerate development while maintaining consistency and quality across the Open Sharia Enterprise platform.

## Available Templates

### Testing Templates

#### [Unit Test Template](./ex-so-stla-ja-te__unit-test-template.md)

Comprehensive template for JUnit 5 unit tests with AssertJ assertions and Mockito mocking.

**Key Features:**

- Arrange-Act-Assert pattern
- Nested test classes for organization
- Parameterized tests
- Mock verification
- Financial domain example (Zakat Calculator)

**Use When:** Testing individual classes in isolation

#### [Integration Test Template](./ex-so-stla-ja-te__integration-test-template.md)

Template for Spring Boot integration tests with TestContainers and REST Assured.

**Key Features:**

- TestContainers for real database instances
- REST Assured for API testing
- Complete CRUD operation coverage
- Transaction and security testing
- Financial domain example (Zakat Payment)

**Use When:** Testing complete workflows with real infrastructure dependencies

#### [BDD Step Definition Template](./ex-so-stla-ja-te__bdd-step-definition-template.md)

Cucumber step definitions implementing Given-When-Then patterns for behavior-driven development.

**Key Features:**

- Given-When-Then step patterns
- Data tables for complex input
- Shared test context
- Reusable step definitions
- Financial domain example (Zakat Calculation)

**Use When:** Implementing executable specifications with Cucumber/Gherkin

### Domain Model Templates

#### [Entity Template](./ex-so-stla-ja-te__entity-template.md)

Domain-Driven Design entity template with identity, lifecycle, and business logic.

**Key Features:**

- Identity-based equality
- Factory methods for creation
- Business method encapsulation
- State transition validation
- Audit field tracking
- Financial domain example (Zakat Account)

**Use When:** Creating entities with identity and mutable state

#### [Value Object Template](./ex-so-stla-ja-te__value-object-template.md)

Immutable value object template with value-based equality.

**Key Features:**

- Complete immutability
- Value-based equality
- Self-validation
- Side-effect free operations
- Financial domain example (Money)

**Use When:** Modeling concepts without identity (amounts, dates, addresses)

#### [Aggregate Template](./ex-so-stla-ja-te__aggregate-template.md)

Aggregate root template enforcing transactional boundaries and consistency.

**Key Features:**

- Transactional boundary definition
- Child entity management through root
- Aggregate-level invariant enforcement
- Domain event registration
- Financial domain example (Donation Campaign)

**Use When:** Defining consistency boundaries in complex domain models

#### [Domain Event Template](./ex-so-stla-ja-te__domain-event-template.md)

Immutable domain event template for capturing significant business occurrences.

**Key Features:**

- Immutable event design
- Past-tense naming convention
- Event metadata (ID, timestamp, aggregate info)
- Event handler and publisher patterns
- Financial domain examples (ZakatPaymentProcessed, DonationReceived, CampaignGoalReached)

**Use When:** Capturing domain state changes, enabling loose coupling between aggregates, building event-sourced systems

### Infrastructure Templates

#### [Repository Template](./ex-so-stla-ja-te__repository-template.md)

DDD repository template with Spring Data JPA and custom query methods.

**Key Features:**

- Collection-like interface
- Business-oriented query methods
- Specification pattern for complex queries
- Custom repository implementations
- Financial domain example (Zakat Account Repository)

**Use When:** Persisting aggregates, querying by business criteria, implementing data access layer

#### [Service Layer Template](./ex-so-stla-ja-te__service-layer-template.md)

Application service template for orchestrating domain operations and managing transactions.

**Key Features:**

- Transaction management
- Cross-aggregate orchestration
- Domain event publishing
- Request/response objects
- Financial domain example (Zakat Calculation Service)

**Use When:** Coordinating complex operations across multiple aggregates, managing transaction boundaries

### Build & Configuration Templates

#### [Build Configuration Template](./ex-so-stla-ja-te__build-configuration-template.md)

Maven and Gradle build configurations with testing, code quality, and CI/CD integration.

**Key Features:**

- Dependency management (BOMs)
- Test separation (unit vs integration)
- Code coverage thresholds (JaCoCo)
- Static analysis (Checkstyle, SpotBugs)
- Multi-environment profiles

**Use When:** Setting up new Java projects or standardizing existing builds

## Usage Guidelines

### Getting Started

1. **Choose the Right Template**: Select the template that matches your use case
2. **Copy and Customize**: Copy the template and replace placeholder values
3. **Follow Conventions**: Maintain the structure and patterns shown in templates
4. **Add Financial Context**: Use Sharia-compliant financial examples where applicable

### Template Naming Convention

Template files follow the pattern: `ex-so-stla-ja-te__{template-name}.md`

- `ex` - Explanation (Diátaxis category)
- `so` - Software
- `stla` - Stack-Lang
- `ja` - Java
- `te` - Template

### Common Patterns Across Templates

#### Validation

All templates emphasize early validation:

```java
Objects.requireNonNull(value, "Value must not be null");
validateBusinessRules(value);
```

#### Immutability

Value objects and critical fields use `final`:

```java
private final EntityId id;
private final LocalDateTime createdAt;
```

#### Factory Methods

Prefer static factory methods over constructors:

```java
public static Entity create(/* params */) {
    return new Entity(/* params */);
}
```

#### Explicit Error Handling

Use specific exception types with clear messages:

```java
throw new IllegalArgumentException("Amount must be positive");
throw new IllegalStateException("Cannot transition from COMPLETED to PENDING");
```

## Financial Domain Integration

All templates include examples from the financial domain relevant to Open Sharia Enterprise:

- **Zakat Calculation**: 2.5% wealth tax with nisab threshold and haul period
- **Donation Processing**: Campaign management with beneficiaries
- **Account Management**: Sharia-compliant wealth tracking
- **Payment Processing**: Transaction recording and receipt generation

These examples demonstrate:

- Islamic finance principles
- Regulatory compliance requirements
- Real-world business complexity
- Domain-driven design in practice

## Quality Standards

All templates enforce these quality standards:

- **Test Coverage**: 80% line coverage, 75% branch coverage (minimum)
- **Code Style**: Checkstyle validation with zero warnings
- **Static Analysis**: SpotBugs checks with maximum effort
- **Compiler Warnings**: All warnings treated as errors
- **Documentation**: Comprehensive JavaDoc on public APIs

## Related Documentation

### Core Java Documentation

- [Test-Driven Development](../ex-so-stla-ja__test-driven-development.md) - TDD practices and patterns
- [Behaviour-Driven Development](../ex-so-stla-ja__behaviour-driven-development.md) - BDD with Cucumber
- [Security](../ex-so-stla-ja__security.md) - Security best practices

## Contributing

When creating new templates:

1. Follow the established structure and naming conventions
2. Include comprehensive code examples
3. Add financial domain examples where applicable
4. Document all key features and use cases
5. Link to related templates and documentation
6. Update this README with the new template

## Version History

- **2026-01-21**: Complete template collection created
  - Unit Test Template
  - Integration Test Template
  - BDD Step Definition Template
  - Entity Template
  - Value Object Template
  - Aggregate Template
  - Domain Event Template
  - Repository Template
  - Service Layer Template
  - Build Configuration Template

---

**License**: MIT
**Maintained By**: Open Sharia Enterprise Platform Team

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
