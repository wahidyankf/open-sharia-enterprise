---
name: swe-programming-jvm-spring-boot
description: Spring Boot coding standards from authoritative docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ documentation
---

# Spring Boot Coding Standards

## Purpose

Progressive disclosure of Spring Boot standards for agents writing Spring Boot applications.

**Authoritative Source**: [docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/README.md](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/README.md)

**Usage**: Auto-loaded for agents when writing Spring Boot code. Provides quick reference to auto-configuration, starters, REST APIs, testing, and production-ready features.

**Foundation**: Requires Spring Framework skill (swe-programming-jvm-spring). Spring Boot builds on Spring Framework with auto-configuration and opinionated defaults.

## Prerequisite Knowledge

**CRITICAL**: This skill provides **OSE Platform Spring Boot standards**, not Spring Boot tutorials.

**You MUST complete Java, Spring Framework, AND Spring Boot learning paths IN ORDER**:

**1. Java Foundation** (prerequisite for Spring):

- [Java Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/) - Initial setup, language overview (0-95% coverage)
- [Java By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/by-example/) - 75+ annotated code examples
- [Java In the Field](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/in-the-field/) - Production patterns
- [Java Release Highlights](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/release-highlights/) - Java 17, 21, 25 LTS features

**2. Spring Framework Foundation** (prerequisite for Spring Boot):

- Complete all Spring Framework learning content (see swe-programming-jvm-spring skill)
- Understand IoC, DI, AOP, configuration before using Spring Boot auto-configuration

**3. Spring Boot Learning Path**:

- [Spring Boot Initial Setup](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/initial-setup.md) - Environment setup
- [Spring Boot Overview](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/overview.md) - Auto-configuration, starters
- [Spring Boot By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/by-example/) - 85+ examples
- [Spring Boot In-the-Field](../../../apps/ayokoding-web/content/en/learn/software-engineering/platform-web/tools/jvm-spring-boot/in-the-field/) - 37 production guides

**Documentation Separation**:

- **AyoKoding Spring Boot** - "How to use Spring Boot" (educational, universal patterns)
- **docs/explanation/jvm-spring-boot** - "How to use Spring Boot in OSE Platform" (repository conventions)

**What this skill covers**: OSE Platform Spring Boot configuration (application.yml), auto-configuration customization, REST API design, testing strategies, production observability.

**What this skill does NOT cover**: Spring Boot basics, auto-configuration fundamentals (those are in ayokoding-web).

## Quick Standards Reference

### Auto-configuration and Starters

- Use Spring Boot starters for dependency management
- Customize auto-configuration with `application.yml` properties
- Exclude unnecessary auto-configurations explicitly
- Document custom auto-configurations

### Configuration Properties

- Use `@ConfigurationProperties` for type-safe configuration
- Group related properties in dedicated classes
- Validate configuration with Bean Validation annotations
- Use profiles for environment-specific configuration

### REST API Development

- Use `@RestController` for REST endpoints
- Apply `@Valid` for request validation
- Implement global exception handling with `@ControllerAdvice`
- Use HATEOAS for hypermedia-driven APIs
- Document APIs with OpenAPI/Swagger

### Data Access

- Use Spring Data JPA repositories
- Prefer derived query methods over `@Query`
- Apply `@Transactional` at service layer
- Use `@DataJpaTest` for repository testing

### Security

- Use Spring Security with OAuth2/JWT
- Implement method-level security with `@PreAuthorize`
- Configure CORS for API access
- Use `@AuthenticationPrincipal` for user context

### Testing

- Use `@SpringBootTest` for integration tests
- Apply test slices: `@WebMvcTest`, `@DataJpaTest`, `@JsonTest`
- Use TestContainers for external dependencies
- Mock external services with `@MockBean`

### Observability

- Enable Spring Boot Actuator for monitoring
- Expose custom metrics with Micrometer
- Implement custom health indicators
- Use structured logging with Logback

## Comprehensive Documentation

For detailed guidance, refer to the 18 Spring Boot standards files:

**Core Patterns**:

- [Idioms](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__idioms.md) - Spring Boot-specific patterns
- [Best Practices](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__best-practices.md) - Framework standards
- [Anti-Patterns](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__anti-patterns.md) - Common mistakes

**Configuration & Architecture**:

- [Configuration](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__configuration.md)
- [Dependency Injection](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__dependency-injection.md)
- [AOP](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__aop.md)

**Data & Web**:

- [Data Access](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__data-access.md)
- [REST APIs](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__rest-apis.md)
- [WebFlux Reactive](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__webflux-reactive.md)

**Quality & Operations**:

- [Security](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__security.md)
- [Testing](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__testing.md)
- [Performance](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__performance.md)
- [Observability](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__observability.md)
- [Deployment](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__deployment.md)

**Domain & Design**:

- [Domain-Driven Design](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__domain-driven-design.md)
- [Functional Programming](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__functional-programming.md)

**Maintenance**:

- [Version Migration](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/ex-soen-plwe-to-jvspbo__version-migration.md)

## Related Skills

- swe-programming-java - Java language fundamentals (foundation)
- swe-programming-jvm-spring - Spring Framework patterns (prerequisite)
- docs-applying-content-quality - Content quality standards
- repo-practicing-trunk-based-development - Git workflow

## References

- [Spring Boot README](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring-boot/README.md)
- [Spring Framework README](../../../docs/explanation/software-engineering/platform-web/tools/jvm-spring/README.md)
- [Java README](../../../docs/explanation/software-engineering/programming-languages/java/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
