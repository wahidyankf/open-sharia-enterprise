---
name: swe-programming-csharp
description: C# coding standards from authoritative docs/explanation/software-engineering/programming-languages/c-sharp/ documentation
---

# C# Coding Standards

## Purpose

Progressive disclosure of C# coding standards for agents writing C# code.

**Authoritative Source**: [docs/explanation/software-engineering/programming-languages/c-sharp/README.md](../../../docs/explanation/software-engineering/programming-languages/c-sharp/README.md)

## Prerequisite Knowledge

**IMPORTANT**: This skill provides **OSE Platform-specific style guides**, not educational tutorials.

Complete the AyoKoding C# learning path first:

1. **[C# Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/c-sharp/)** - 0-95% language coverage
2. **[C# By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/c-sharp/by-example/)** - 75+ annotated examples

**See**: [Programming Language Documentation Separation](../../../governance/conventions/structure/programming-language-docs-separation.md)

## Quick Standards Reference

### Naming Conventions

**Classes/Interfaces/Methods/Properties**: PascalCase

- `ZakatCalculator`, `IZakatRepository`, `CalculateAmount()`, `TotalWealth`

**Local Variables/Parameters**: camelCase

- `zakatAmount`, `nisabThreshold`, `paymentDate`

**Private Fields**: `_camelCase` prefix

- `private readonly IZakatRepository _repository;`

**Constants**: PascalCase

- `public const decimal ZakatRate = 0.025m;`

### Nullable Reference Types

```csharp
// CORRECT: Enable nullable in .csproj
// <Nullable>enable</Nullable>

// CORRECT: Non-nullable by default
public string ContractId { get; init; } = string.Empty;

// CORRECT: Nullable when intentional
public string? Notes { get; init; }

// CORRECT: Null-forgiving with justification
var value = GetValue()!; // Safe because we validated above
```

### Records for Value Objects

```csharp
// CORRECT: Record for immutable value object
public record ZakatCalculation(
    decimal Wealth,
    decimal Nisab,
    decimal Amount,
    DateOnly CalculationDate
)
{
    public static ZakatCalculation Calculate(decimal wealth, decimal nisab)
    {
        var amount = wealth >= nisab ? wealth * 0.025m : 0m;
        return new ZakatCalculation(wealth, nisab, amount, DateOnly.FromDateTime(DateTime.UtcNow));
    }
}
```

### Async/Await

```csharp
// CORRECT: async Task with CancellationToken
public async Task<ZakatCalculation> CalculateAsync(
    decimal wealth,
    CancellationToken cancellationToken = default)
{
    var nisab = await _repository.GetCurrentNisabAsync(cancellationToken);
    return ZakatCalculation.Calculate(wealth, nisab);
}

// WRONG: Blocking async code
public ZakatCalculation Calculate(decimal wealth)
{
    var nisab = _repository.GetCurrentNisabAsync().Result; // DEADLOCK RISK!
    return ZakatCalculation.Calculate(wealth, nisab);
}
```

### Error Handling

```csharp
// CORRECT: ProblemDetails for HTTP errors (RFC 7807)
app.UseExceptionHandler(exceptionHandlerApp =>
    exceptionHandlerApp.Run(async context =>
    {
        context.Response.ContentType = "application/problem+json";
        var problemDetails = new ProblemDetails
        {
            Status = StatusCodes.Status500InternalServerError,
            Title = "An unexpected error occurred"
        };
        await context.Response.WriteAsJsonAsync(problemDetails);
    }));

// CORRECT: Result pattern for domain errors
public Result<ZakatCalculation> Calculate(decimal wealth, decimal nisab)
{
    if (wealth < 0)
        return Result.Failure<ZakatCalculation>("Wealth cannot be negative");

    return Result.Success(ZakatCalculation.Calculate(wealth, nisab));
}
```

### Testing with xUnit and FluentAssertions

```csharp
public class ZakatCalculatorTests
{
    [Theory]
    [InlineData(10000, 5000, 250)]
    [InlineData(3000, 5000, 0)]
    public async Task CalculateAsync_ReturnsCorrectAmount(
        decimal wealth, decimal nisab, decimal expectedAmount)
    {
        // Arrange
        var mockRepo = new Mock<IZakatRepository>();
        mockRepo.Setup(r => r.GetCurrentNisabAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(nisab);
        var calculator = new ZakatCalculator(mockRepo.Object);

        // Act
        var result = await calculator.CalculateAsync(wealth);

        // Assert
        result.Amount.Should().Be(expectedAmount);
    }
}
```

## Comprehensive Documentation

**Authoritative Index**: [docs/explanation/software-engineering/programming-languages/c-sharp/README.md](../../../docs/explanation/software-engineering/programming-languages/c-sharp/README.md)

### Mandatory Standards

1. **[Coding Standards](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__coding-standards.md)**
2. **[Testing Standards](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__testing-standards.md)**
3. **[Code Quality Standards](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__code-quality-standards.md)**
4. **[Build Configuration](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__build-configuration.md)**

### Context-Specific Standards

1. **[Error Handling](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__error-handling-standards.md)**
2. **[Concurrency](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__concurrency-standards.md)**
3. **[Type Safety](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__type-safety-standards.md)**
4. **[Performance](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__performance-standards.md)**
5. **[Security](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__security-standards.md)**
6. **[API Standards](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__api-standards.md)**
7. **[DDD Standards](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__ddd-standards.md)**
8. **[Framework Integration](../../../docs/explanation/software-engineering/programming-languages/c-sharp/ex-soen-prla-cs__framework-integration.md)**

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [C# README](../../../docs/explanation/software-engineering/programming-languages/c-sharp/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
