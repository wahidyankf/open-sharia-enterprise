---
name: repo-coding-java
description: Java coding standards from authoritative docs/explanation/software/stack-lang/java/ documentation
---

# Java Coding Standards

## Purpose

Progressive disclosure of Java coding standards for agents writing Java code.

**Authoritative Source**: [docs/explanation/software/stack-lang/java/README.md](../../../docs/explanation/software/stack-lang/java/README.md)

**Usage**: Auto-loaded for agents when writing Java code. Provides quick reference to idioms, best practices, and antipatterns.

## Quick Standards Reference

### Naming Conventions

**Classes and Interfaces**: PascalCase

- Classes: `UserAccount`, `PaymentProcessor`
- Interfaces: `Comparable`, `Serializable` (adjective form preferred)
- Abstract classes: `AbstractProcessor`, `BaseEntity`

**Methods and Variables**: camelCase

- Methods: `calculateTotal()`, `findUserById()`
- Variables: `userName`, `totalAmount`
- Constants: `UPPER_SNAKE_CASE` (`MAX_RETRIES`, `DEFAULT_TIMEOUT`)

**Packages**: lowercase with dots

- `com.oseplatform.domain.account`
- `com.oseplatform.infrastructure.persistence`

### Modern Java Features (Java 17+)

**Records**: Use for immutable data carriers

```java
public record UserAccount(String id, String name, LocalDateTime createdAt) {}
```

**Sealed Classes**: Use for closed type hierarchies

```java
public sealed interface Payment permits CreditCard, BankTransfer {}
```

**Pattern Matching**: Use for type-safe casts

```java
if (obj instanceof String s) {
    return s.toUpperCase();
}
```

**Text Blocks**: Use for multi-line strings

```java
String json = """
    {
        "name": "value"
    }
    """;
```

### Error Handling

**Checked Exceptions**: For recoverable errors

- Use for business logic failures
- Document with `@throws` javadoc

**Unchecked Exceptions**: For programming errors

- Use for validation failures, illegal state
- Extend `RuntimeException`

**Try-with-resources**: Always use for AutoCloseable

```java
try (var reader = Files.newBufferedReader(path)) {
    // Use reader
}
```

### Testing Standards

**JUnit 5**: Primary testing framework

- `@Test` for test methods
- `@BeforeEach`, `@AfterEach` for setup/teardown
- `@ParameterizedTest` for data-driven tests

**AssertJ**: Fluent assertions

```java
assertThat(result)
    .isNotNull()
    .hasSize(3)
    .containsExactly("a", "b", "c");
```

**Mockito**: Mocking framework

```java
@Mock
private UserRepository repository;
```

### Security Practices

**Input Validation**: Always validate external input

- Use Bean Validation annotations (`@NotNull`, `@Size`)
- Validate before processing

**Sensitive Data**: Never log secrets

- Use `SensitiveDataFilter` for logging
- Clear sensitive data after use

**SQL Injection**: Use prepared statements

```java
var stmt = connection.prepareStatement("SELECT * FROM users WHERE id = ?");
stmt.setString(1, userId);
```

## Comprehensive Documentation

For detailed guidance, refer to:

- **[Idioms](../../../docs/explanation/software/stack-lang/java/ex-so-stla-ja__idioms.md)** - Java-specific patterns
- **[Best Practices](../../../docs/explanation/software/stack-lang/java/ex-so-stla-ja__best-practices.md)** - Clean code standards
- **[Anti-Patterns](../../../docs/explanation/software/stack-lang/java/ex-so-stla-ja__anti-patterns.md)** - Common mistakes

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Java README](../../../docs/explanation/software/stack-lang/java/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
