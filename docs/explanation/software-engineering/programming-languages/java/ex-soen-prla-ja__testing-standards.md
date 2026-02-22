---
title: "Java Testing Standards"
description: Authoritative OSE Platform testing standards (JUnit 6, AssertJ, Mockito, TestContainers, Cucumber BDD)
category: explanation
subcategory: prog-lang
tags:
  - java
  - testing
  - junit6
  - assertj
  - mockito
  - testcontainers
  - cucumber
  - bdd
  - tdd
principles:
  - automation-over-manual
  - reproducibility
  - explicit-over-implicit
created: 2026-02-03
updated: 2026-02-03
---

## Prerequisite Knowledge

**REQUIRED**: You MUST understand Java fundamentals from [AyoKoding Java Learning Path](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/_index.md) before using these standards.

**This document is OSE Platform-specific**, not a Java tutorial. We define HOW to apply Java in THIS codebase, not WHAT Java is.

**See**: [Programming Language Documentation Separation Convention](../../../../../governance/conventions/structure/programming-language-docs-separation.md)

## Purpose

This document defines **authoritative testing standards** for Java development in the OSE Platform. These prescriptive rules govern test frameworks, naming conventions, test organization, and testing patterns.

**Target Audience**: OSE Platform Java developers, QA engineers, technical reviewers

**Scope**: JUnit 6 setup, assertion libraries, mocking patterns, integration testing, BDD acceptance tests

## JUnit 6 Setup

**MUST** use JUnit 6 Jupiter (not JUnit 4) for all unit and integration tests.

### Test Class Naming

**MUST** suffix test classes with `Test`.

**Format**: `[ClassUnderTest]Test`

**Examples**:

- `TaxCalculatorTest` (tests `TaxCalculator` class)
- `InvoiceServiceTest` (tests `InvoiceService` class)
- `PaymentRepositoryTest` (tests `PaymentRepository` class)

**Prohibited**:

- ❌ `TestTaxCalculator` (wrong prefix)
- ❌ `TaxCalculatorTests` (wrong suffix - plural)
- ❌ `TaxCalculatorUnitTest` (redundant - all tests in `src/test/java` are tests)

### Test Method Naming

**MUST** use descriptive, behavior-focused method names.

**Format**: `should[ExpectedBehavior]When[Condition]`

**Examples**:

- `shouldCalculateTaxWhenAmountAboveThreshold()`
- `shouldThrowExceptionWhenInvoiceIsNull()`
- `shouldReturnEmptyListWhenNoPaymentsFound()`

**Prohibited**:

- ❌ `testCalculate()` (not descriptive)
- ❌ `test1()` (no business meaning)
- ❌ `calculateTax_success()` (not behavior-focused)

**Rationale**: Descriptive names serve as living documentation and make test failures immediately understandable.

### Test Lifecycle

**MUST** use `@BeforeEach` and `@AfterEach` for setup/cleanup (not JUnit 4 `@Before`/`@After`).

**Example**:

```java
class TaxCalculatorTest {
  private TaxCalculator calculator;

  @BeforeEach
  void setUp() {
    calculator = new TaxCalculator();
  }

  @AfterEach
  void tearDown() {
    // Cleanup if needed
  }

  @Test
  void shouldCalculateTaxWhenAmountIsPositive() {
    // Test implementation
  }
}
```

**Prohibited**:

- ❌ `@Before` / `@After` (JUnit 4 - deprecated)
- ❌ Setup in constructor (not isolated per test)

## Assertion Library

**MUST** use AssertJ for all assertions (not JUnit assertions).

### AssertJ Assertions (Required)

**Correct**:

```java
import static org.assertj.core.api.Assertions.*;

@Test
void shouldCalculateTaxCorrectly() {
  BigDecimal result = calculator.calculateTax(amount);

  assertThat(result)
    .isNotNull()
    .isEqualByComparingTo(expectedTax);
}
```

**Benefits**:

- Fluent, readable API
- Better error messages
- Rich assertion types (collections, exceptions, etc.)

### JUnit Assertions (Prohibited)

**Wrong**:

```java
import static org.junit.jupiter.api.Assertions.*;

@Test
void shouldCalculateTaxCorrectly() {
  BigDecimal result = calculator.calculateTax(amount);

  assertNotNull(result);  // WRONG - use AssertJ
  assertEquals(expectedTax, result);  // WRONG - use AssertJ
}
```

**Why prohibited**: JUnit assertions have inferior error messages and less expressive API compared to AssertJ.

## Mockito Patterns

**MUST** use Mockito for test doubles with JUnit 6 extension.

### Mockito Setup

**MUST** use `@ExtendWith(MockitoExtension.class)` (not `MockitoAnnotations.initMocks()`).

**Correct**:

```java
@ExtendWith(MockitoExtension.class)
class InvoiceServiceTest {
  @Mock
  private InvoiceRepository repository;

  @InjectMocks
  private InvoiceService service;

  @Test
  void shouldCreateInvoiceSuccessfully() {
    // Test implementation
  }
}
```

**Prohibited**:

- ❌ `MockitoAnnotations.initMocks(this)` (deprecated)
- ❌ Manual mock creation without extension

### Mocking Strategy

**Constructor injection enables easy mocking** - production code uses constructor injection, test code provides mocks via constructor.

**Example**:

```java
// Production code (constructor injection)
@Service
public class TaxService {
  private final TaxRepository repository;

  public TaxService(TaxRepository repository) {
    this.repository = repository;
  }
}

// Test code (easy mocking)
@Test
void shouldCalculateTax() {
  TaxRepository mockRepository = mock(TaxRepository.class);
  TaxService service = new TaxService(mockRepository);
  // Test with mock
}
```

### Verification

**SHOULD** verify behavior, not implementation details.

**Correct** (verify behavior):

```java
@Test
void shouldSaveInvoiceWhenCreated() {
  service.createInvoice(data);

  verify(repository).save(any(Invoice.class));
}
```

**Avoid** (verify implementation):

```java
@Test
void shouldCallValidateBeforeSave() {
  service.createInvoice(data);

  // AVOID - tests implementation, not behavior
  InOrder inOrder = inOrder(validator, repository);
  inOrder.verify(validator).validate(any());
  inOrder.verify(repository).save(any());
}
```

**Rationale**: Behavior verification allows refactoring without breaking tests.

## TestContainers

**MUST** use TestContainers for integration tests requiring external dependencies (databases, message queues, external services).

### TestContainers Setup

**Example**:

```java
@Testcontainers
class InvoiceRepositoryIntegrationTest {
  @Container
  private static PostgreSQLContainer<?> postgres =
    new PostgreSQLContainer<>("postgres:15-alpine")
      .withDatabaseName("testdb")
      .withUsername("test")
      .withPassword("test");

  @Test
  void shouldPersistInvoice() {
    // Test with real PostgreSQL container
  }
}
```

**Benefits**:

- Real external dependencies (no mocking database behavior)
- Isolated test environment
- Reproducible across machines

**Prohibited**:

- ❌ H2 in-memory database for PostgreSQL-specific features
- ❌ Mocking database repositories (use TestContainers instead)

## BDD Process

**SHOULD** use Cucumber for acceptance tests (BDD - Behavior-Driven Development).

### Gherkin Scenarios

**Location**: `src/test/resources/features/`

**Example**: `src/test/resources/features/tax-calculation.feature`

```gherkin
Feature: Tax Calculation

  Scenario: Calculate tax for standard invoice
    Given an invoice with amount of 1000
    When tax is calculated
    Then the tax amount should be 150
```

### Step Definitions

**Location**: Test packages (e.g., `com.oseplatform.tax.steps`)

**Example**:

```java
public class TaxCalculationSteps {
  private Invoice invoice;
  private BigDecimal taxAmount;

  @Given("an invoice with amount of {int}")
  public void anInvoiceWithAmount(int amount) {
    invoice = new Invoice(BigDecimal.valueOf(amount));
  }

  @When("tax is calculated")
  public void taxIsCalculated() {
    taxAmount = calculator.calculateTax(invoice.getAmount());
  }

  @Then("the tax amount should be {int}")
  public void theTaxAmountShouldBe(int expected) {
    assertThat(taxAmount).isEqualByComparingTo(BigDecimal.valueOf(expected));
  }
}
```

**When to use BDD**:

- Acceptance tests with business stakeholders
- End-to-end user journey validation
- Complex business rules requiring clear specification

**When NOT to use BDD**:

- Simple unit tests (use JUnit + AssertJ directly)
- Technical integration tests (use TestContainers)

## Test Organization

**MUST** organize tests into three categories:

### Unit Tests (Fast)

- **Location**: `src/test/java`
- **Dependencies**: None (mocks only)
- **Run frequency**: Every commit
- **Naming**: `[Class]Test.java`

### Integration Tests (TestContainers)

- **Location**: `src/test/java` (separate package: `*.integration`)
- **Dependencies**: External systems via TestContainers
- **Run frequency**: Pre-push, CI/CD
- **Naming**: `[Component]IntegrationTest.java`

### E2E Tests (Cucumber BDD)

- **Location**: Separate Maven module (`[project]-e2e`)
- **Dependencies**: Full application deployment
- **Run frequency**: CI/CD only
- **Naming**: `*.feature` (Gherkin scenarios)

## Coverage Requirements

**MUST** maintain ≥80% line coverage measured by JaCoCo.

**Coverage enforcement**: CI/CD pipeline fails if coverage drops below threshold.

**See**: [Java Code Quality](./ex-soen-prla-ja__code-quality.md) for JaCoCo configuration.

## Enforcement

Testing standards are enforced through:

- **Maven Surefire/Failsafe** - Runs unit and integration tests
- **JaCoCo** - Measures code coverage, enforces ≥80% threshold
- **Code reviews** - Human verification of test quality
- **CI/CD pipeline** - Blocks merges if tests fail or coverage drops

## Learning Resources

For learning Java fundamentals and concepts referenced in these standards, see:

- **[Java Learning Path](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/_index.md)** - Complete Java learning journey
- **[Java By Example](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/by-example/_index.md)** - 157+ annotated code examples
  - **[Intermediate Examples](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/by-example/intermediate.md)** - Testing with JUnit, Mockito, AssertJ
  - **[Advanced Examples](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/by-example/advanced.md)** - TestContainers, integration testing
- **[Java In Practice](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/in-the-field/_index.md)** - Test-Driven Development (TDD) and Behavior-Driven Development (BDD) practices
- **[Java Release Highlights](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/java/release-highlights/_index.md)** - Java 17, 21, and 25 LTS features

**Note**: These standards assume you've learned Java basics from ayokoding-web. We don't re-explain fundamental concepts here.

## Related Standards

- [Java Coding Standards](./ex-soen-prla-ja__coding-standards.md) - Constructor injection enables easy testing
- [Java Framework Integration](./ex-soen-prla-ja__framework-integration.md) - Testing Spring Boot and Jakarta EE components
- [Java Code Quality](./ex-soen-prla-ja__code-quality.md) - JaCoCo coverage configuration
- [Java Build Configuration](./ex-soen-prla-ja__build-configuration.md) - Maven test dependencies

## Software Engineering Principles

These standards enforce the five software engineering principles:

1. **[Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md)**
   - Maven Surefire/Failsafe automatically run tests on every build
   - JaCoCo enforces ≥80% coverage threshold (build fails if not met)
   - CI/CD pipeline runs all tests before merge approval

2. **[Reproducibility](../../../../../governance/principles/software-engineering/reproducibility.md)**
   - TestContainers spin up identical database instances for every test run
   - Tests isolated with `@BeforeEach` setup (no shared state between tests)
   - Cucumber BDD scenarios provide reproducible acceptance criteria

3. **[Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md)**
   - Test names explicitly describe behavior (`shouldCalculateTaxWhenAmountAboveThreshold`)
   - AssertJ provides explicit, readable assertions (`assertThat(result).isNotNull().isEqualByComparingTo(expected)`)
   - `@ExtendWith(MockitoExtension.class)` makes mocking framework explicit

## Related Documentation

**Test Organization**:

- [Coding Standards](./ex-soen-prla-ja__coding-standards.md) - Test class naming, package structure, and file organization

**Domain Testing**:

- [DDD Standards](./ex-soen-prla-ja__ddd-standards.md) - Aggregate testing patterns, domain event verification, and repository testing

**Coverage Requirements**:

- [Code Quality Standards](./ex-soen-prla-ja__code-quality.md) - JaCoCo coverage enforcement and quality gate thresholds

---

**Maintainers**: Platform Documentation Team
**Last Updated**: 2026-02-04
