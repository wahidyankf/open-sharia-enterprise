---
title: "Overview"
weight: 10000000
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Learn Dart through 75 heavily annotated examples covering 95% of Dart features, from basics to advanced patterns"
tags: ["dart", "by-example", "overview"]
---

Learn Dart through heavily annotated code examples. This section provides 75 practical examples covering 95% of Dart's features, organized by difficulty level.

**Example Coverage**:

- Beginner (Examples 1-25) - Basic syntax, types, control flow, functions, OOP basics
- Intermediate (Examples 26-50) - Async/await, streams, advanced OOP, generics, mixins
- Advanced (Examples 51-75) - Isolates, design patterns, testing, performance optimization

## Learning Approach

**By Example** uses a code-first methodology:

1. **Self-contained examples** - Each example is complete and runnable
2. **Heavy annotation** - 1-2.25 comments per line of code using `// =>` notation
3. **Progressive difficulty** - From beginner fundamentals to advanced patterns
4. **Islamic finance context** - Real-world examples using Zakat, Murabaha, donations

This approach efficiently teaches Dart through practical code rather than extensive prose.

## Annotation Density Standard

All examples maintain **1.0-2.25 comment lines per code line** to ensure thorough understanding.

**Annotation pattern**:

```dart
String name = 'Ahmad';           // => name stores string value 'Ahmad'
                                 // => String type (immutable reference)

int calculateZakat(int wealth) {  // => Function takes int parameter wealth
                                 // => Returns int (Zakat amount)
  return (wealth * 0.025).round(); // => Multiply by 2.5% rate
                                 // => round() converts double to int
}                                // => Returns calculated Zakat amount

var zakat = calculateZakat(10000); // => Call function with 10000
                                  // => zakat stores result: 250
```

**Simple lines**: Get 1 annotation explaining what happens
**Complex lines**: Get 2 annotations explaining logic and result

## Organization by Difficulty

### Beginner (25-30 examples)

Covers fundamental Dart concepts (0-30% language coverage):

- Variables and basic types
- Control flow (if/else, loops, switch)
- Functions and parameters
- Basic null safety
- Lists and Maps
- String manipulation
- Simple classes
- Basic error handling

**Prerequisites**: Programming fundamentals (variables, functions, basic logic)

### Intermediate (25-30 examples)

Covers practical Dart development (30-70% language coverage):

- Advanced null safety patterns
- Async/await and Futures
- Streams and subscriptions
- Advanced collections (fold, map, where, reduce)
- Object-oriented programming (inheritance, interfaces, mixins)
- Exception handling
- File I/O
- JSON parsing
- HTTP requests

**Prerequisites**: Completed beginner section or equivalent experience

### Advanced (25-30 examples)

Covers sophisticated patterns (70-95% language coverage):

- Generators (sync\* and async\*)
- Isolates for concurrency
- Extension methods
- Generics and type parameters
- Advanced async patterns (Future.wait, Stream.periodic)
- Custom operators
- Reflection and mirrors
- FFI (Foreign Function Interface)
- Performance optimization
- Design patterns in Dart

**Prerequisites**: Completed intermediate section or production Dart experience

## Example Structure

Each example follows a consistent five-part structure:

### 1. Title and Context

```dart
// Example 23: Calculating Zakat on Investment Portfolio
// Context: Managing multiple investment types with different rates
```

### 2. Complete Code

```dart
void main() {
  // Full working example
}
```

### 3. Heavy Annotations

```dart
double amount = 1000.0;  // => amount stores 1000.0 (type: double)
                         // => Used for calculation
```

### 4. Output Documentation

```
// Output:
// Portfolio total: Rp10,000,000
// Zakat due: Rp250,000
```

### 5. Key Takeaways

```
// Takeaways:
// - Collections can store different types with proper generics
// - Fold reduces collection to single accumulated value
// - Null safety prevents runtime errors
```

## Running Examples

All examples are self-contained and can be run directly:

```bash
# Save example to file
cat > example.dart << 'EOF'
void main() {
  print('As-salamu alaykum!'); // => Outputs greeting
                               // => Prints to console
}
EOF

# Run immediately
dart run example.dart
# => Output: As-salamu alaykum!
```

**No project setup required** - each example is a single file.

## Islamic Finance Examples

Examples use authentic Islamic finance concepts:

**Zakat (Obligatory Charity)**:

```dart
double calculateZakat(double wealth) {
  // => wealth: Total zakatable wealth
  const nisab = 85.0 * 1000000.0;  // => 85 grams gold × price per gram
                                   // => Minimum threshold

  if (wealth < nisab) return 0.0;  // => Below threshold: no Zakat
                                   // => Returns zero

  return wealth * 0.025;           // => 2.5% of eligible wealth
                                   // => Returns Zakat amount
}
```

**Murabaha (Cost-Plus Financing)**:

```dart
class MurabahaContract {
  // => Islamic financing structure
  final double cost;        // => Original cost of asset
  final double markup;      // => Agreed-upon profit margin
  final int months;         // => Payment period

  double get totalPrice => cost + markup;  // => Total amount payable
                                          // => Transparent calculation

  double get monthlyPayment => totalPrice / months;  // => Fixed monthly amount
                                                    // => Equal installments
}
```

**Sadaqah (Voluntary Charity)**:

```dart
Stream<double> trackDonations() async* {
  // => Asynchronous stream generator
  // => Yields donation amounts over time
  yield 100.0;   // => First donation: 100
  yield 250.0;   // => Second donation: 250
  yield 500.0;   // => Third donation: 500
}                // => Stream completes
```

## Coverage Target: 95%

These 75-90 examples cover approximately **95% of Dart features** you'll use in real applications.

**Not covered** (remaining 5%):

- Obscure language features rarely used in practice
- Platform-specific APIs (covered in framework tutorials)
- Experimental features not yet stabilized
- Low-level VM internals

The 95% coverage includes all practical features for:

- Mobile development with Flutter
- Web development
- Server-side applications
- CLI tools
- Package development

## How to Use This Section

**For beginners**:

1. Start with [Beginner](/en/learn/software-engineering/programming-languages/dart/by-example/beginner) section
2. Read each example carefully
3. Run the code yourself
4. Modify examples to experiment
5. Move to Intermediate when comfortable

**For experienced programmers**:

1. Skim [Beginner](/en/learn/software-engineering/programming-languages/dart/by-example/beginner) for Dart-specific syntax
2. Focus on [Intermediate](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate) for practical patterns
3. Study [Advanced](/en/learn/software-engineering/programming-languages/dart/by-example/advanced) for sophisticated techniques

**For reference**:

- Use as quick lookup for specific features
- Copy and adapt examples for your projects
- Reference annotation patterns for documentation

## Complementary Resources

**Combine with**:

- [Overview](/en/learn/software-engineering/programming-languages/dart/overview) - Conceptual understanding of Dart
- [Initial Setup](/en/learn/software-engineering/programming-languages/dart/initial-setup) - Installation and configuration
- [Quick Start](/en/learn/software-engineering/programming-languages/dart/quick-start) - Complete application tutorial

**Next level**:

- Official Dart documentation for comprehensive API reference
- Flutter documentation for UI development
- Package ecosystem for specialized libraries

## Start Learning

Begin with [Beginner](/en/learn/software-engineering/programming-languages/dart/by-example/beginner) examples to learn Dart fundamentals through annotated code, or jump to [Intermediate](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate) or [Advanced](/en/learn/software-engineering/programming-languages/dart/by-example/advanced) based on your experience level.

## Examples by Level

### Beginner (Examples 1–25)

- [Example 1: Variable Declaration and Type Inference](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-1-variable-declaration-and-type-inference)
- [Example 2: Null Safety Basics](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-2-null-safety-basics)
- [Example 3: String Manipulation](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-3-string-manipulation)
- [Example 4: Basic Arithmetic and Operators](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-4-basic-arithmetic-and-operators)
- [Example 5: Control Flow - If/Else](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-5-control-flow---ifelse)
- [Example 6: Control Flow - For Loops](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-6-control-flow---for-loops)
- [Example 7: Control Flow - While and Do-While](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-7-control-flow---while-and-do-while)
- [Example 8: Switch Statements](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-8-switch-statements)
- [Example 9: Lists - Creation and Basic Operations](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-9-lists---creation-and-basic-operations)
- [Example 10: Maps - Key-Value Storage](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-10-maps---key-value-storage)
- [Example 11: Function Basics](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-11-function-basics)
- [Example 12: Function Advanced - First-Class Functions](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-12-function-advanced---first-class-functions)
- [Example 13: Classes and Objects Basics](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-13-classes-and-objects-basics)
- [Example 14: Classes - Named Constructors and Factory](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-14-classes---named-constructors-and-factory)
- [Example 15: Classes - Getters and Setters](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-15-classes---getters-and-setters)
- [Example 16: Enums - Type-Safe Constants](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-16-enums---type-safe-constants)
- [Example 17: Collections - Sets](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-17-collections---sets)
- [Example 18: Exception Handling Basics](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-18-exception-handling-basics)
- [Example 19: String Advanced - Pattern Matching](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-19-string-advanced---pattern-matching)
- [Example 20: Type Conversion and Casting](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-20-type-conversion-and-casting)
- [Example 21: Collection Iteration Methods](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-21-collection-iteration-methods)
- [Example 22: DateTime Basics](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-22-datetime-basics)
- [Example 23: Classes - Static Members](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-23-classes---static-members)
- [Example 24: Command-Line Input/Output](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-24-command-line-inputoutput)
- [Example 25: Comments and Documentation](/en/learn/software-engineering/programming-languages/dart/by-example/beginner#example-25-comments-and-documentation)

### Intermediate (Examples 26–50)

- [Example 26: Future Basics - async/await](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-26-future-basics---asyncawait)
- [Example 27: Future.wait - Parallel Execution](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-27-futurewait---parallel-execution)
- [Example 28: Async Error Handling](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-28-async-error-handling)
- [Example 29: Future Timeouts](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-29-future-timeouts)
- [Example 30: Completer - Manual Future Control](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-30-completer---manual-future-control)
- [Example 31: Stream Basics](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-31-stream-basics)
- [Example 32: Stream Transformations](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-32-stream-transformations)
- [Example 33: StreamController - Manual Stream Creation](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-33-streamcontroller---manual-stream-creation)
- [Example 34: Async Generators (async\*)](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-34-async-generators-async)
- [Example 35: Stream Subscription Management](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-35-stream-subscription-management)
- [Example 36: Inheritance and Method Overriding](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-36-inheritance-and-method-overriding)
- [Example 37: Abstract Classes and Methods](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-37-abstract-classes-and-methods)
- [Example 38: Implementing Multiple Interfaces](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-38-implementing-multiple-interfaces)
- [Example 39: Mixins for Composition](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-39-mixins-for-composition)
- [Example 40: Extension Methods](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-40-extension-methods)
- [Example 41: Generic Classes with Type Constraints](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-41-generic-classes-with-type-constraints)
- [Example 42: Generic Functions and Methods](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-42-generic-functions-and-methods)
- [Example 43: Callable Classes with call() Method](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-43-callable-classes-with-call-method)
- [Example 44: Typedef for Function Types](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-44-typedef-for-function-types)
- [Example 45: Cascade Notation (.., ?..)](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-45-cascade-notation--)
- [Example 46: Metadata and Annotations](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-46-metadata-and-annotations)
- [Example 47: Assert Statements for Development-Time Checks](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-47-assert-statements-for-development-time-checks)
- [Example 48: Factory Constructors and Named Constructors](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-48-factory-constructors-and-named-constructors)
- [Example 49: Operator Overloading](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-49-operator-overloading)
- [Example 50: Record Types (Dart 3.0+)](/en/learn/software-engineering/programming-languages/dart/by-example/intermediate#example-50-record-types-dart-30)

### Advanced (Examples 51–75)

- [Example 51: Isolates Basics](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-51-isolates-basics)
- [Example 52: Isolate Communication](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-52-isolate-communication)
- [Example 53: Compute Function](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-53-compute-function)
- [Example 54: Stream.periodic for Recurring Tasks](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-54-streamperiodic-for-recurring-tasks)
- [Example 55: Future.any and Future.race](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-55-futureany-and-futurerace)
- [Example 56: Singleton Pattern](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-56-singleton-pattern)
- [Example 57: Factory Pattern](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-57-factory-pattern)
- [Example 58: Observer Pattern with Streams](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-58-observer-pattern-with-streams)
- [Example 59: Unit Testing Basics](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-59-unit-testing-basics)
- [Example 60: Integration Testing](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-60-integration-testing)
- [Example 61: Lazy Initialization](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-61-lazy-initialization)
- [Example 62: Memoization and Caching](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-62-memoization-and-caching)
- [Example 63: Memory Management and Object Pooling](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-63-memory-management-and-object-pooling)
- [Example 64: String Optimization Techniques](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-64-string-optimization-techniques)
- [Example 65: Collection Performance Optimization](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-65-collection-performance-optimization)
- [Example 66: Benchmarking and Profiling](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-66-benchmarking-and-profiling)
- [Example 67: Production Logging Patterns](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-67-production-logging-patterns)
- [Example 68: Error Handling and Monitoring](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-68-error-handling-and-monitoring)
- [Example 69: Configuration Management](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-69-configuration-management)
- [Example 70: Dependency Injection Pattern](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-70-dependency-injection-pattern)
- [Example 71: Repository Pattern](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-71-repository-pattern)
- [Example 72: Service Layer Pattern](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-72-service-layer-pattern)
- [Example 73: Clean Architecture Principles](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-73-clean-architecture-principles)
- [Example 74: Event-Driven Architecture with Streams](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-74-event-driven-architecture-with-streams)
- [Example 75: Advanced Testing Strategies](/en/learn/software-engineering/programming-languages/dart/by-example/advanced#example-75-advanced-testing-strategies)
