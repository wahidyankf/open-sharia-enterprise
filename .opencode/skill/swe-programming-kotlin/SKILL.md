---
name: swe-programming-kotlin
description: Kotlin coding standards from authoritative docs/explanation/software-engineering/programming-languages/kotlin/ documentation
---

# Kotlin Coding Standards

## Purpose

Progressive disclosure of Kotlin coding standards for agents writing Kotlin code.

**Authoritative Source**: [docs/explanation/software-engineering/programming-languages/kotlin/README.md](../../../docs/explanation/software-engineering/programming-languages/kotlin/README.md)

**Usage**: Auto-loaded for agents when writing Kotlin code. Provides quick reference to idioms, best practices, and antipatterns.

## Prerequisite Knowledge

**IMPORTANT**: This skill provides **OSE Platform-specific style guides**, not educational tutorials.

**You MUST understand Kotlin fundamentals before using these standards.** Complete the AyoKoding Kotlin learning path first:

1. **[Kotlin Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/kotlin/)** - Initial setup, language overview, quick start guide (0-95% language coverage)
2. **[Kotlin By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/kotlin/by-example/)** - 75+ heavily annotated code examples (beginner to advanced patterns)

**What this skill covers**: OSE Platform naming conventions, framework choices, repository-specific patterns, how to apply Kotlin knowledge in THIS codebase.

**What this skill does NOT cover**: Kotlin syntax, language fundamentals, generic patterns (those are in ayokoding-web).

**See**: [Programming Language Documentation Separation](../../../governance/conventions/structure/programming-language-docs-separation.md) for content separation rules.

## Quick Standards Reference

### Naming Conventions

**Classes/Types**: PascalCase - `ZakatCalculator`, `MurabahaContract`

**Functions/Variables**: camelCase - `calculateZakat()`, `totalAmount`

**Constants**: UPPER_SNAKE_CASE - `MAX_NISAB_THRESHOLD`, `ZAKAT_RATE`

**Files**: PascalCase matching primary class - `ZakatCalculator.kt`

### Null Safety

```kotlin
// CORRECT: Safe call operator
val length = text?.length ?: 0

// CORRECT: Smart cast after null check
if (contract != null) {
    println(contract.id) // Smart cast to non-null
}

// WRONG: Unsafe assertion without justification
val value = nullableValue!! // Crashes if null
```

### Coroutines

```kotlin
// CORRECT: Structured concurrency with coroutineScope
suspend fun processPayments(payments: List<Payment>): List<Result<BigDecimal>> =
    coroutineScope {
        payments.map { payment ->
            async { processPayment(payment) }
        }.awaitAll()
    }

// CORRECT: Flow for reactive streams
fun zakatCalculations(): Flow<ZakatResult> = flow {
    repository.getAllContracts().forEach { contract ->
        emit(calculateZakat(contract))
    }
}

// WRONG: Blocking inside coroutine
suspend fun badExample() {
    Thread.sleep(1000) // WRONG: blocks thread
    delay(1000)        // CORRECT: suspends coroutine
}
```

### Data Classes and Sealed Classes

```kotlin
// CORRECT: Data class for value objects
data class ZakatCalculation(
    val wealth: BigDecimal,
    val nisab: BigDecimal,
    val amount: BigDecimal,
    val calculationDate: LocalDate = LocalDate.now()
)

// CORRECT: Sealed class for domain states
sealed class ZakatResult {
    data class Due(val amount: BigDecimal) : ZakatResult()
    data object BelowNisab : ZakatResult()
    data class Error(val message: String) : ZakatResult()
}

// CORRECT: Exhaustive when expression
fun handleResult(result: ZakatResult): String = when (result) {
    is ZakatResult.Due -> "Zakat due: ${result.amount}"
    is ZakatResult.BelowNisab -> "Below nisab threshold"
    is ZakatResult.Error -> "Error: ${result.message}"
}
```

### Error Handling

```kotlin
// CORRECT: Result<T> for fallible operations
suspend fun calculateZakat(wealth: BigDecimal, nisab: BigDecimal): Result<BigDecimal> =
    runCatching {
        require(wealth >= BigDecimal.ZERO) { "Wealth cannot be negative" }
        if (wealth >= nisab) wealth.multiply(BigDecimal("0.025"))
        else BigDecimal.ZERO
    }

// CORRECT: Sealed error hierarchy
sealed class ZakatError {
    data class ValidationError(val field: String, val message: String) : ZakatError()
    data class CalculationError(val reason: String) : ZakatError()
}
```

### Testing with MockK

```kotlin
// CORRECT: MockK for Kotlin-native mocking
@Test
fun `calculateZakat returns 2_5 percent when above nisab`() = runTest {
    val mockRepo = mockk<ZakatRepository>()
    coEvery { mockRepo.getNisabThreshold() } returns BigDecimal("5000")

    val calculator = ZakatCalculator(mockRepo)
    val result = calculator.calculate(BigDecimal("10000"))

    assertThat(result.getOrThrow()).isEqualByComparingTo("250.00")
    coVerify { mockRepo.getNisabThreshold() }
}
```

## Comprehensive Documentation

**Authoritative Index**: [docs/explanation/software-engineering/programming-languages/kotlin/README.md](../../../docs/explanation/software-engineering/programming-languages/kotlin/README.md)

### Mandatory Standards (All Kotlin Code MUST Follow)

1. **[Coding Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__coding-standards.md)** - Naming conventions, Effective Kotlin idioms
2. **[Testing Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__testing-standards.md)** - JUnit 5, Kotest, MockK, coroutines-test
3. **[Code Quality Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__code-quality-standards.md)** - ktlint, Detekt, compiler warnings
4. **[Build Configuration](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__build-configuration.md)** - Gradle KTS, version catalogs

### Context-Specific Standards (Apply When Relevant)

1. **[Error Handling Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__error-handling-standards.md)** - Result<T>, sealed error hierarchies
2. **[Concurrency Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__concurrency-standards.md)** - Coroutines, Flow, structured concurrency
3. **[Type Safety Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__type-safety-standards.md)** - Null safety, sealed classes, data classes
4. **[Performance Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__performance-standards.md)** - Inline functions, lazy, sequences
5. **[Security Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__security-standards.md)** - Spring Security, JWT, input validation
6. **[API Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__api-standards.md)** - Ktor routing, REST conventions
7. **[DDD Standards](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__ddd-standards.md)** - Domain-Driven Design with sealed classes
8. **[Framework Integration](../../../docs/explanation/software-engineering/programming-languages/kotlin/ex-soen-prla-ko__framework-integration.md)** - Ktor, Spring Boot, Android

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Kotlin README](../../../docs/explanation/software-engineering/programming-languages/kotlin/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
