---
name: swe-programming-dart
description: Dart coding standards from authoritative docs/explanation/software-engineering/programming-languages/dart/ documentation
---

# Dart Coding Standards

## Purpose

Progressive disclosure of Dart coding standards for agents writing Dart code.

**Authoritative Source**: [docs/explanation/software-engineering/programming-languages/dart/README.md](../../../docs/explanation/software-engineering/programming-languages/dart/README.md)

**Usage**: Auto-loaded for agents when writing Dart code. Provides quick reference to idioms, best practices, and antipatterns.

## Prerequisite Knowledge

**IMPORTANT**: This skill provides **OSE Platform-specific style guides**, not educational tutorials.

**You MUST understand Dart fundamentals before using these standards.** Complete the AyoKoding Dart learning path first:

1. **[Dart Learning Path](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/dart/)** - Initial setup, language overview, quick start guide (0-95% language coverage)
2. **[Dart By Example](../../../apps/ayokoding-web/content/en/learn/software-engineering/programming-languages/dart/by-example/)** - 75+ heavily annotated code examples (beginner to advanced patterns)

**What this skill covers**: OSE Platform naming conventions, framework choices, repository-specific patterns.

**What this skill does NOT cover**: Dart syntax, language fundamentals, generic patterns (those are in ayokoding-web).

**See**: [Programming Language Documentation Separation](../../../governance/conventions/structure/programming-language-docs-separation.md) for content separation rules.

## Quick Standards Reference

### Naming Conventions

**Files and Packages**: lowercase_with_underscores

- `zakat_calculator.dart`, `murabaha_service.dart`
- Package names: `zakat_app`, `islamic_finance`

**Classes and Types**: UpperCamelCase

- `ZakatCalculator`, `MurabahaContract`, `PaymentStatus`

**Functions, Variables, Parameters**: lowerCamelCase

- `calculateZakat()`, `totalAmount`, `paymentDate`

**Constants**: lowerCamelCase (not UPPER_CASE in Dart)

- `const defaultNisab = 5000.0;`
- `static const zakatRate = 0.025;`

### Null Safety (Dart 3.0+)

**Non-nullable by default**:

```dart
// CORRECT: Non-nullable
String name = 'Ahmed';

// CORRECT: Nullable when needed
String? optionalEmail;

// CORRECT: Null-aware operators
String greeting = optionalEmail ?? 'Guest';
int? length = optionalEmail?.length;
```

**WRONG: Null assertion without justification**:

```dart
// WRONG: Unsafe null assertion
String definitelyPresent = possiblyNull!; // crashes if null
```

### Error Handling

**Typed exceptions**:

```dart
// CORRECT: Typed exception hierarchy
class DomainException implements Exception {
  final String message;
  const DomainException(this.message);
}

class ZakatValidationException extends DomainException {
  const ZakatValidationException(super.message);
}

// CORRECT: Catch specific types
try {
  final result = await calculateZakat(wealth, nisab);
} on ZakatValidationException catch (e) {
  handleValidation(e);
} on DomainException catch (e) {
  handleDomain(e);
}
```

**WRONG: Catching Object or dynamic**:

```dart
// WRONG: Too broad
try {
  doSomething();
} catch (e) { // Catches everything including Errors
  print(e);
}
```

### Async Patterns

**Prefer async/await**:

```dart
// CORRECT: async/await
Future<ZakatResult> calculateAsync(double wealth, double nisab) async {
  await Future.delayed(Duration(milliseconds: 100));
  return wealth >= nisab ? ZakatResult.due(wealth * 0.025) : ZakatResult.notDue();
}

// CORRECT: Stream for multiple values
Stream<Payment> paymentsStream(String contractId) async* {
  final payments = await repository.getPayments(contractId);
  for (final payment in payments) {
    yield payment;
  }
}
```

### Immutability

**Use final and const**:

```dart
// CORRECT: Immutable class
class ZakatCalculation {
  final double wealth;
  final double nisab;
  final double amount;

  const ZakatCalculation({
    required this.wealth,
    required this.nisab,
    required this.amount,
  });
}

// CORRECT: const for compile-time constants
const zakatRate = 0.025;
```

### Testing Standards

**package:test structure**:

```dart
import 'package:test/test.dart';

void main() {
  group('ZakatCalculator', () {
    late ZakatCalculator calculator;

    setUp(() {
      calculator = ZakatCalculator();
    });

    test('returns 2.5% when wealth above nisab', () {
      final result = calculator.calculate(10000, 5000);
      expect(result, equals(250.0));
    });

    test('returns 0 when wealth below nisab', () {
      final result = calculator.calculate(1000, 5000);
      expect(result, equals(0.0));
    });
  });
}
```

### Security Practices

**Input Validation**:

- Validate all external input before processing
- Never log passwords, tokens, or financial details

**Secrets Management**:

```dart
// CORRECT: Use flutter_secure_storage
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

final storage = FlutterSecureStorage();
await storage.write(key: 'api_token', value: token);
```

## Comprehensive Documentation

**Authoritative Index**: [docs/explanation/software-engineering/programming-languages/dart/README.md](../../../docs/explanation/software-engineering/programming-languages/dart/README.md)

### Mandatory Standards (All Dart Code MUST Follow)

1. **[Coding Standards](../../../docs/explanation/software-engineering/programming-languages/dart/coding-standards.md)** - Naming conventions, package organization, Effective Dart
2. **[Testing Standards](../../../docs/explanation/software-engineering/programming-languages/dart/testing-standards.md)** - package:test, mockito, coverage >=95%
3. **[Code Quality Standards](../../../docs/explanation/software-engineering/programming-languages/dart/code-quality-standards.md)** - dart analyze, lints, dart format
4. **[Build Configuration](../../../docs/explanation/software-engineering/programming-languages/dart/build-configuration.md)** - pubspec.yaml, build_runner

### Context-Specific Standards (Apply When Relevant)

1. **[Error Handling Standards](../../../docs/explanation/software-engineering/programming-languages/dart/error-handling-standards.md)** - Typed exceptions, Result patterns
2. **[Concurrency Standards](../../../docs/explanation/software-engineering/programming-languages/dart/concurrency-standards.md)** - async/await, Future, Stream, Isolates
3. **[Type Safety Standards](../../../docs/explanation/software-engineering/programming-languages/dart/type-safety-standards.md)** - Null safety, sealed classes, records (Dart 3.0+)
4. **[Performance Standards](../../../docs/explanation/software-engineering/programming-languages/dart/performance-standards.md)** - const constructors, lazy init, Isolates
5. **[Security Standards](../../../docs/explanation/software-engineering/programming-languages/dart/security-standards.md)** - Input validation, secure storage
6. **[API Standards](../../../docs/explanation/software-engineering/programming-languages/dart/api-standards.md)** - shelf HTTP patterns, REST conventions
7. **[DDD Standards](../../../docs/explanation/software-engineering/programming-languages/dart/ddd-standards.md)** - Domain-Driven Design patterns
8. **[Framework Integration](../../../docs/explanation/software-engineering/programming-languages/dart/framework-integration.md)** - Flutter, Riverpod, shelf

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Dart README](../../../docs/explanation/software-engineering/programming-languages/dart/README.md)
- [Functional Programming](../../../governance/development/pattern/functional-programming.md)
