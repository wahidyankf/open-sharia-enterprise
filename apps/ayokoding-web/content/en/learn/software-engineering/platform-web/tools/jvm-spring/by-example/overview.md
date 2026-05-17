---
title: "Overview"
weight: 100000000
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Learn Spring Framework through 75-90 heavily annotated code examples covering IoC, dependency injection, data access, transactions, AOP, and web MVC"
tags: ["spring", "java", "kotlin", "examples", "tutorial"]
---

## Learning Approach

This **By Example** series teaches Spring Framework through **code-first learning** with heavily annotated examples. Each example is self-contained and runnable, demonstrating specific Spring Framework concepts.

**Target**: 75-90 examples achieving **95% Spring Framework coverage**

**Annotation Density**: 1.0-2.25 comment lines per code line (per example)

**Format**: All examples in both **Java and Kotlin**

## Why By Example?

Traditional tutorials often provide long explanations before showing code. This approach inverts that pattern:

**Code First**: Start with working, runnable examples
**Inline Documentation**: Every line annotated with `// =>` explaining what happens
**Incremental Complexity**: Simple examples first, building to advanced patterns
**Islamic Finance Context**: Real-world scenarios (Zakat, Murabaha, Sadaqah)

## Structure

### Beginner Examples (0-50% Coverage)

**Topics Covered**:

- IoC Container basics
- Bean definitions with `@Configuration` and `@Bean`
- Component scanning with `@Component`, `@Service`, `@Repository`
- Dependency injection (constructor, setter, field)
- Bean scopes (singleton, prototype)
- Lifecycle callbacks (`@PostConstruct`, `@PreDestroy`)
- Property injection with `@Value`
- Profile-based configuration (`@Profile`)

**Example Count**: ~25-30 examples

**Skill Level**: No prior Spring knowledge required

### Intermediate Examples (50-85% Coverage)

**Topics Covered**:

- Advanced dependency injection (qualifiers, primary beans)
- Data access with JdbcTemplate
- Transaction management (`@Transactional`)
- Spring AOP (aspects, pointcuts, advice types)
- Event publishing and handling
- SpEL (Spring Expression Language)
- Resource handling
- Validation with Bean Validation API
- Testing with Spring Test framework

**Example Count**: ~30-35 examples

**Skill Level**: Completed beginner examples or equivalent knowledge

### Advanced Examples (85-95% Coverage)

**Topics Covered**:

- Custom bean post-processors
- Advanced AOP (custom annotations, around advice)
- Programmatic transaction management
- Custom scope implementations
- Conditional bean registration
- Spring Web MVC (controllers, REST APIs)
- Exception handling strategies
- Async processing with `@Async`
- Caching with `@Cacheable`
- Integration testing strategies

**Example Count**: ~20-25 examples

**Skill Level**: Completed intermediate examples or production Spring experience

## Example Format

Each example follows a **five-part structure**:

### 1. Concept Introduction

A 1-3 sentence explanation of what the example demonstrates, often including an Islamic finance scenario for real-world context.

### 2. Heavily Annotated Code

Self-contained, runnable Java and Kotlin implementations with `// =>` annotations on every significant line.

**Annotation density**: 1.0-2.25 comment lines per code line

**Example**:

```java
@Service  // => Marks as Spring-managed service component
public class ZakatService {

    private final ZakatRepository repository;  // => Final field - immutable
    // => Constructor injection (recommended)
    // => Spring automatically injects repository parameter
    public ZakatService(ZakatRepository repository) {
        this.repository = repository;  // => Dependency injected
    }

    @Transactional  // => Transaction boundary
    public void recordPayment(BigDecimal amount) {
        repository.save(new ZakatRecord(amount));  // => Persists within transaction
        // => Auto-commit on success
    }
}
```

### 3. Optional Diagram

Mermaid diagrams for complex concepts showing data flow, object hierarchies, or execution sequences.

### 4. Key Takeaways

Bullet points highlighting critical concepts and common pitfalls.

### 5. Why It Matters

50-100 words explaining the business and production relevance of the concept, specifically in the context of Islamic finance systems.

## Islamic Finance Context

Examples use authentic Islamic finance scenarios:

### Zakat (Obligatory Charity)

**Definition**: 2.5% annual payment on qualifying wealth above nisab threshold

**Examples**:

- Calculating Zakat on savings
- Recording Zakat payments
- Tracking distribution to eligible recipients

### Murabaha (Cost-Plus Financing)

**Definition**: Shariah-compliant financing where financier buys asset and sells at disclosed profit

**Examples**:

- Calculating payment schedules
- Recording installments
- Profit distribution calculations

### Sadaqah (Voluntary Charity)

**Definition**: Voluntary charitable giving beyond obligatory Zakat

**Examples**:

- Recording donations
- Tracking campaigns
- Generating tax receipts

### Qard Hassan (Benevolent Loan)

**Definition**: Interest-free loan as act of charity

**Examples**:

- Loan disbursement
- Repayment tracking
- Beneficiary management

## How to Use This Series

### Sequential Learning (Recommended)

Follow examples in order:

1. **Start**: Beginner examples (foundational concepts)
2. **Progress**: Intermediate examples (practical patterns)
3. **Master**: Advanced examples (production techniques)

### Reference Learning

Jump to specific topics using index:

- Need transaction management? → Intermediate #15
- Need REST APIs? → Advanced #08
- Need custom annotations? → Advanced #12

### Practice-Based Learning

After each example:

1. **Read** the annotated code
2. **Type** the code yourself (muscle memory)
3. **Modify** the example (experiment)
4. **Test** your changes (verify understanding)

## Prerequisites

### Required Knowledge

**Java or Kotlin**:

- OOP concepts (classes, interfaces, inheritance)
- Generics basics
- Lambda expressions (Java 8+) or Kotlin functions

**Build Tools**:

- Maven or Gradle basics
- Dependency management concepts

**Database Basics**:

- SQL fundamentals (SELECT, INSERT, UPDATE)
- JDBC concepts (connections, statements)

### Setup Requirements

Complete [Initial Setup](/en/learn/software-engineering/platform-web/tools/jvm-spring/initial-setup) to have:

- Java 17+ or Kotlin 1.9+ installed
- Maven or Gradle configured
- IDE configured (IntelliJ IDEA recommended)

## Learning Outcomes

After completing all examples, you will:

**Understand**:

- Spring IoC container architecture
- Dependency injection patterns and best practices
- Bean lifecycle and scopes
- Transaction management strategies
- AOP concepts and applications

**Apply**:

- Java-based configuration effectively
- Component scanning and stereotype annotations
- JdbcTemplate for data access
- Declarative transaction management
- Testing strategies with Spring Test

**Evaluate**:

- When to use Spring Framework vs Spring Boot
- Trade-offs between injection types
- Appropriate transaction boundaries
- Performance implications of AOP

## Example Catalog

### Core Container (Examples 1-15)

**Beginner Level**:

1. Basic bean definition with `@Bean`
2. Constructor injection
3. Setter injection
4. Component scanning with `@Component`
5. Autowiring by type
6. Singleton vs prototype scope
7. `@PostConstruct` and `@PreDestroy`
8. `@Value` property injection
9. Profile-based configuration
10. Multiple configuration classes

**Intermediate Level**:

1. `@Qualifier` for disambiguation
2. `@Primary` for default beans
3. Method injection with `@Lookup`
4. Circular dependency resolution
5. Lazy initialization with `@Lazy`

### Data Access (Examples 16-35)

**Beginner Level**:

1. JdbcTemplate basics - insert
2. JdbcTemplate - query with RowMapper
3. JdbcTemplate - update and delete
4. Named parameters with NamedParameterJdbcTemplate

**Intermediate Level**:

1. `@Transactional` basics
2. Transaction propagation (REQUIRED, REQUIRES_NEW)
3. Transaction isolation levels
4. Rollback rules
5. Programmatic transaction management
6. Multiple DataSource configuration
7. Connection pooling with HikariCP
8. Database initialization with schema.sql

**Advanced Level**:

1. Custom TransactionManager
2. Distributed transactions overview
3. Optimistic locking patterns
4. Batch operations with JdbcTemplate
5. Stored procedure calls
6. Custom exception translation
7. JDBC testing strategies
8. Transaction testing with `@Transactional` rollback

### AOP (Examples 36-50)

**Intermediate Level**:

1. Basic aspect with `@Before` advice
2. `@After` and `@AfterReturning` advice
3. `@Around` advice for method interception
4. Pointcut expressions
5. Custom annotations as pointcuts
6. Passing arguments to advice
7. Aspect ordering

**Advanced Level**:

1. Introduction aspects (`@DeclareParents`)
2. Performance monitoring aspect
3. Logging aspect
4. Security aspect (authorization checks)
5. Retry logic with AOP
6. Caching with AOP
7. Async execution with AOP
8. AOP testing strategies

### Web MVC (Examples 51-70)

**Intermediate Level**:

1. Basic `@Controller` with request mapping
2. `@RequestParam` for query parameters
3. `@PathVariable` for URL variables
4. `@RequestBody` for JSON input
5. `@ResponseBody` for JSON output
6. Model and View resolution
7. Form handling with `@ModelAttribute`
8. Validation with Bean Validation API
9. Exception handling with `@ExceptionHandler`
10. RESTful API with `@RestController`

**Advanced Level**:

1. Content negotiation
2. Custom argument resolvers
3. Interceptors for request processing
4. File upload handling
5. Async request processing
6. Server-Sent Events (SSE)
7. CORS configuration
8. Security headers
9. REST API testing
10. Integration testing with MockMvc

### Advanced Topics (Examples 71-90)

1. Event publishing with `ApplicationEventPublisher`
2. Event listening with `@EventListener`
3. SpEL in annotations
4. Resource loading
5. Custom property sources
6. Custom scope implementation
7. Bean post-processors
8. `@Conditional` annotations
9. Caching with `@Cacheable`
10. Async methods with `@Async`
11. Scheduling with `@Scheduled`
12. Custom validators
13. Type conversion with ConversionService
14. Internationalization (i18n)
15. Profile-specific beans
16. Environment abstraction
17. Application context events
18. Graceful shutdown
19. Health checks
20. Metrics collection

## Navigation

**Start Learning**:

- **[Beginner Examples](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner)** - Examples 1-30 (0-50% coverage)
- **[Intermediate Examples](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate)** - Examples 31-65 (50-85% coverage)
- **[Advanced Examples](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced)** - Examples 66-90 (85-95% coverage)

**Other Learning Paths**:

- **[Overview](/en/learn/software-engineering/platform-web/tools/jvm-spring/overview)** - Conceptual introduction
- **[Initial Setup](/en/learn/software-engineering/platform-web/tools/jvm-spring/initial-setup)** - Installation and project setup
- **[Quick Start](/en/learn/software-engineering/platform-web/tools/jvm-spring/quick-start)** - Complete working application

## Examples by Level

### Beginner (Examples 1–25)

- [Example 1: Creating Spring ApplicationContext (Coverage: 1.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-1-creating-spring-applicationcontext-coverage-10)
- [Example 2: Defining and Retrieving Simple Bean (Coverage: 3.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-2-defining-and-retrieving-simple-bean-coverage-30)
- [Example 3: Constructor Dependency Injection (Coverage: 6.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-3-constructor-dependency-injection-coverage-60)
- [Example 4: Component Scanning with @Component (Coverage: 10.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-4-component-scanning-with-component-coverage-100)
- [Example 5: @Autowired Constructor Injection (Coverage: 13.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-5-autowired-constructor-injection-coverage-130)
- [Example 6: Custom Bean Names (Coverage: 16.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-6-custom-bean-names-coverage-160)
- [Example 7: Bean Aliases (Coverage: 18.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-7-bean-aliases-coverage-180)
- [Example 8: Setter Injection (Coverage: 20.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-8-setter-injection-coverage-200)
- [Example 9: Field Injection (Coverage: 22.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-9-field-injection-coverage-220)
- [Example 10: @Qualifier for Disambiguation (Coverage: 25.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-10-qualifier-for-disambiguation-coverage-250)
- [Example 11: Singleton Scope (Default) (Coverage: 27.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-11-singleton-scope-default-coverage-270)
- [Example 12: Prototype Scope (Coverage: 30.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-12-prototype-scope-coverage-300)
- [Example 13: @PostConstruct Lifecycle Callback (Coverage: 33.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-13-postconstruct-lifecycle-callback-coverage-330)
- [Example 14: @PreDestroy Lifecycle Callback (Coverage: 36.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-14-predestroy-lifecycle-callback-coverage-360)
- [Example 15: @Primary for Default Bean (Coverage: 38.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-15-primary-for-default-bean-coverage-380)
- [Example 16: @Value with Literal Values (Coverage: 40.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-16-value-with-literal-values-coverage-400)
- [Example 17: @Value with Property Placeholders (Coverage: 42.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-17-value-with-property-placeholders-coverage-420)
- [Example 18: @Value with Default Values (Coverage: 44.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-18-value-with-default-values-coverage-440)
- [Example 19: Profile-Based Configuration (@Profile) (Coverage: 46.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-19-profile-based-configuration-profile-coverage-460)
- [Example 20: Environment Abstraction (Coverage: 48.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-20-environment-abstraction-coverage-480)
- [Example 21: Loading Resources (Coverage: 50.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-21-loading-resources-coverage-500)
- [Example 22: Injecting Collections (Coverage: 52.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-22-injecting-collections-coverage-520)
- [Example 23: Conditional Bean Registration (@Conditional) (Coverage: 54.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-23-conditional-bean-registration-conditional-coverage-540)
- [Example 24: Lazy Bean Initialization (@Lazy) (Coverage: 56.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-24-lazy-bean-initialization-lazy-coverage-560)
- [Example 25: DependsOn for Bean Creation Order (Coverage: 58.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/beginner#example-25-dependson-for-bean-creation-order-coverage-580)

### Intermediate (Examples 26–50)

- [Example 26: Constructor Injection with Multiple Dependencies (Coverage: 61.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-26-constructor-injection-with-multiple-dependencies-coverage-610)
- [Example 27: Optional Dependencies with @Autowired(required=false) (Coverage: 63.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-27-optional-dependencies-with-autowiredrequiredfalse-coverage-630)
- [Example 28: Injecting ApplicationContext (Coverage: 65.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-28-injecting-applicationcontext-coverage-650)
- [Example 29: Circular Dependency Resolution (Coverage: 67.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-29-circular-dependency-resolution-coverage-670)
- [Example 30: Generic Type Injection (Coverage: 69.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-30-generic-type-injection-coverage-690)
- [Example 31: Basic @Aspect with @Before Advice (Coverage: 71.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-31-basic-aspect-with-before-advice-coverage-710)
- [Example 32: @After and @AfterReturning Advice (Coverage: 73.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-32-after-and-afterreturning-advice-coverage-730)
- [Example 33: @Around Advice (Coverage: 74.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-33-around-advice-coverage-740)
- [Example 34: Pointcut Expressions (Coverage: 75.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-34-pointcut-expressions-coverage-755)
- [Example 35: @AfterThrowing Exception Handling (Coverage: 77.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-35-afterthrowing-exception-handling-coverage-770)
- [Example 36: Basic @Transactional (Coverage: 78.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-36-basic-transactional-coverage-785)
- [Example 37: Transaction Propagation (Coverage: 80.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-37-transaction-propagation-coverage-800)
- [Example 38: Transaction Isolation (Coverage: 81.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-38-transaction-isolation-coverage-815)
- [Example 39: Rollback Rules (Coverage: 83.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-39-rollback-rules-coverage-830)
- [Example 40: Programmatic Transactions (Coverage: 84.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-40-programmatic-transactions-coverage-845)
- [Example 41: Basic JdbcTemplate Query (Coverage: 86.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-41-basic-jdbctemplate-query-coverage-860)
- [Example 42: Querying with RowMapper (Coverage: 87.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-42-querying-with-rowmapper-coverage-875)
- [Example 43: NamedParameterJdbcTemplate (Coverage: 89.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-43-namedparameterjdbctemplate-coverage-890)
- [Example 44: Batch Operations (Coverage: 90.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-44-batch-operations-coverage-905)
- [Example 45: Result Extraction with ResultSetExtractor (Coverage: 92.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-45-result-extraction-with-resultsetextractor-coverage-920)
- [Example 46: Simple @Controller (Coverage: 93.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-46-simple-controller-coverage-935)
- [Example 47: @RequestParam and @PathVariable (Coverage: 95.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-47-requestparam-and-pathvariable-coverage-950)
- [Example 48: @RequestBody and JSON (Coverage: 96.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-48-requestbody-and-json-coverage-965)
- [Example 49: Exception Handling with @ExceptionHandler (Coverage: 98.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-49-exception-handling-with-exceptionhandler-coverage-980)
- [Example 50: Form Validation with @Valid (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/intermediate#example-50-form-validation-with-valid-coverage-1000)

### Advanced (Examples 51–75)

- [Example 51: @RestController and ResponseEntity (Coverage: 76.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-51-restcontroller-and-responseentity-coverage-765)
- [Example 52: Content Negotiation (Coverage: 78.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-52-content-negotiation-coverage-780)
- [Example 53: CORS Configuration (Coverage: 79.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-53-cors-configuration-coverage-795)
- [Example 54: API Versioning (Coverage: 81.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-54-api-versioning-coverage-810)
- [Example 55: Global Exception Handler (Coverage: 82.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-55-global-exception-handler-coverage-825)
- [Example 56: Basic Security Configuration (Coverage: 84.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-56-basic-security-configuration-coverage-840)
- [Example 57: Method Security (Coverage: 85.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-57-method-security-coverage-855)
- [Example 58: Custom UserDetailsService (Coverage: 87.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-58-custom-userdetailsservice-coverage-870)
- [Example 59: JWT Token Authentication (Coverage: 88.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-59-jwt-token-authentication-coverage-885)
- [Example 60: Password Encoding (Coverage: 90.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-60-password-encoding-coverage-900)
- [Example 61: Spring Cache Abstraction (Coverage: 91.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-61-spring-cache-abstraction-coverage-915)
- [Example 62: Async Method Execution (Coverage: 93.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-62-async-method-execution-coverage-930)
- [Example 63: Application Events (Coverage: 94.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-63-application-events-coverage-945)
- [Example 64: Scheduled Tasks (Coverage: 96.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-64-scheduled-tasks-coverage-960)
- [Example 65: Custom Conditional Bean Registration (Coverage: 97.5%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-65-custom-conditional-bean-registration-coverage-975)
- [Example 66: Spring TestContext Framework (Coverage: 99.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-66-spring-testcontext-framework-coverage-990)
- [Example 67: MockMvc for Web Layer Testing (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-67-mockmvc-for-web-layer-testing-coverage-1000)
- [Example 68: @Transactional in Tests (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-68-transactional-in-tests-coverage-1000)
- [Example 69: Test Profiles (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-69-test-profiles-coverage-1000)
- [Example 70: Mocking with @MockBean (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-70-mocking-with-mockbean-coverage-1000)
- [Example 71: Custom Health Indicator (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-71-custom-health-indicator-coverage-1000)
- [Example 72: Custom Metrics (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-72-custom-metrics-coverage-1000)
- [Example 73: Request/Response Logging Interceptor (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-73-requestresponse-logging-interceptor-coverage-1000)
- [Example 74: Connection Pooling with HikariCP (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-74-connection-pooling-with-hikaricp-coverage-1000)
- [Example 75: Custom Validation Annotation (Coverage: 100.0%)](/en/learn/software-engineering/platform-web/tools/jvm-spring/by-example/advanced#example-75-custom-validation-annotation-coverage-1000)
