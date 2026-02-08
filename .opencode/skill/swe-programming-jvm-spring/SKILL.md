---
name: swe-programming-jvm-spring
description: Spring Framework coding standards from authoritative docs/explanation/software-engineering/platform-web/tools/jvm-spring/ documentation
---

# Spring Framework Coding Standards

## Purpose

Progressive disclosure of Spring Framework standards for agents writing Spring applications.

**Authoritative Source**: [docs/explanation/software-engineering/platform-web/tools/jvm-spring/README.md](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/README.md)

**Usage**: Auto-loaded for agents when writing Spring Framework code. Provides quick reference to Spring IoC, DI, AOP, data access, and web MVC patterns.

**Foundation**: Requires Java skill (swe-programming-java). Spring Framework is built on core Java concepts.

## Prerequisite Knowledge

**CRITICAL**: This skill provides **OSE Platform Spring Framework standards**, not Spring tutorials.

**You MUST complete both Java AND Spring learning paths**:

**1. Java Foundation** (prerequisite for Spring):

- [Java Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/) - Initial setup, language overview (0-95% coverage)
- [Java By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/by-example/) - 75+ annotated code examples
- [Java In the Field](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/in-the-field/) - Production patterns
- [Java Release Highlights](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/release-highlights/) - Java 17, 21, 25 LTS features

**2. Spring Framework Learning Path**:

- [Spring Initial Setup](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring/initial-setup.md) - Environment setup
- [Spring Overview](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring/overview.md) - IoC, DI, AOP concepts
- [Spring By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/) - 75+ examples
- [Spring In-the-Field](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring/in-the-field/) - 30 production guides

**Documentation Separation**:

- **AyoKoding Spring** - "How to use Spring Framework" (educational, universal patterns)
- **docs/explanation/jvm-spring** - "How to use Spring in OSE Platform" (repository conventions)

**What this skill covers**: OSE Platform Spring Framework configuration, dependency injection patterns, AOP usage, transaction management, web MVC structure.

**What this skill does NOT cover**: Spring Framework basics, IoC fundamentals (those are in ayokoding-web).

## Quick Standards Reference

### IoC Container and ApplicationContext

- Use `ApplicationContext` for production code
- Prefer annotation-based configuration (`@Configuration`, `@Bean`)
- Use component scanning with explicit base packages

### Dependency Injection Patterns

- **Constructor injection** (preferred): Use for required dependencies
- Field injection (avoid): Creates testing difficulties
- Setter injection: Use for optional dependencies

### Bean Lifecycle and Scopes

- Singleton (default): Stateless services
- Prototype: Stateful objects
- Request/Session: Web-scoped beans
- Use `@PostConstruct` and `@PreDestroy` for lifecycle callbacks

### AOP Patterns

- Use `@Aspect` for cross-cutting concerns
- Prefer declarative transactions (`@Transactional`)
- Apply aspects for logging, security, caching

### Data Access

- Use `JdbcTemplate` for SQL operations
- Prefer Spring Data JPA repositories
- Apply transaction management at service layer
- Use `@Transactional` for declarative transactions

### Web MVC

- Use `@RestController` for REST APIs
- Apply `@RequestMapping` for endpoint routing
- Use `@Valid` for request validation
- Implement `@ControllerAdvice` for global exception handling

## Comprehensive Documentation

For detailed guidance, refer to the 21 Spring Framework standards files:

**Core Patterns**:

- [Idioms](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__idioms.md) - Spring-specific patterns
- [Best Practices](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__best-practices.md) - Framework standards
- [Anti-Patterns](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__anti-patterns.md) - Common mistakes

**Configuration & Architecture**:

- [Configuration](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__configuration.md)
- [Dependency Injection](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__dependency-injection.md)
- [AOP](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__aop.md)
- [Build Configuration](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__build-configuration.md)

**Data & Web**:

- [Data Access](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__data-access.md)
- [Web MVC](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__web-mvc.md)
- [REST APIs](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__rest-apis.md)

**Quality & Operations**:

- [Security](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__security.md)
- [Testing](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__testing.md)
- [Performance](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__performance.md)
- [Observability](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__observability.md)
- [Deployment](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__deployment.md)
- [Code Quality](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__code-quality.md)

**Domain & Design**:

- [DDD Standards](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__ddd-standards.md)
- [API Standards](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__api-standards.md)
- [Concurrency Standards](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__concurrency-standards.md)
- [Error Handling Standards](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__error-handling-standards.md)

**Maintenance**:

- [Version Migration](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/ex-soen-plwe-to-jvsp__version-migration.md)

## Related Skills

- swe-programming-java - Java language fundamentals (prerequisite)
- docs-applying-content-quality - Content quality standards
- repo-practicing-trunk-based-development - Git workflow

## References

- [Spring Framework README](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/README.md)
- [Java README](../../../docs/explanation/software-engineering/programming-languages/java/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
