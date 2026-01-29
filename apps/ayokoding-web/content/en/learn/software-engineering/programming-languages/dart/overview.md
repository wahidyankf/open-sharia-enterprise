---
title: "Overview"
weight: 100000
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Conceptual introduction to Dart programming language"
tags: ["dart", "programming", "overview"]
---

Dart is a modern, object-oriented programming language developed by Google that combines the developer productivity of high-level languages with the performance of compiled languages. Originally designed for building web applications, Dart has evolved into a versatile platform for cross-platform development, most notably through the Flutter framework.

## What is Dart?

Dart is a client-optimized programming language that enables developers to build applications across multiple platforms using a single codebase. The language features a sound type system, ahead-of-time compilation for production, and just-in-time compilation for development.

**Key characteristics**:

- **Modern syntax** - Clean, familiar syntax similar to Java and JavaScript
- **Sound null safety** - Compile-time null safety preventing null reference errors
- **Strong typing** - Static typing with type inference for better tooling and reliability
- **Object-oriented** - Class-based inheritance with mixins and interfaces
- **Asynchronous programming** - Built-in async/await support for responsive applications
- **Hot reload** - Instant code changes during development (with Flutter)

## Sound Null Safety System

Dart's null safety system is one of its most powerful features, preventing null reference errors at compile time rather than runtime.

**Non-nullable by default**:

```dart
String name = 'Ahmad';     // => Cannot be null
String? maybeEmail;        // => Can be null (notice ?)
                          // => maybeEmail is null by default

// name = null;            // => Compile error!
maybeEmail = null;         // => OK, explicitly nullable type
```

**Null safety benefits**:

- **Catches errors early** - Null errors caught during compilation
- **Better tooling** - IDEs provide accurate warnings and suggestions
- **Cleaner code** - Explicit about when values can be null
- **Fewer crashes** - Eliminates common null pointer exceptions

## Cross-Platform Development

Dart enables building applications for multiple platforms from a single codebase.

**Target platforms**:

- **Mobile** - iOS and Android via Flutter
- **Web** - Modern JavaScript compilation
- **Desktop** - Windows, macOS, Linux via Flutter
- **Server** - Backend services and APIs
- **Command-line** - CLI tools and scripts

**Development approach**:

```dart
// Same Dart code runs everywhere
void calculateZakat(double wealth) {
  // => wealth: Total wealth subject to Zakat
  const double nisab = 85.0 * 6.61;  // => 85 grams gold × current price
                                     // => nisab threshold: approx 561.85

  if (wealth >= nisab) {
    // => Wealth meets minimum threshold
    double zakat = wealth * 0.025;   // => 2.5% of eligible wealth
    // => zakat: Amount due
    print('Zakat due: ${zakat.toStringAsFixed(2)}'); // => Output formatted
  } else {
    // => Wealth below nisab threshold
    print('Wealth below nisab threshold'); // => No Zakat required
  }
}
```

This same function works in Flutter mobile apps, web applications, desktop programs, and server-side code.

## Key Concepts

### Async/Await for Asynchronous Operations

Dart provides first-class support for asynchronous programming through futures and async/await syntax.

```dart
Future<double> fetchGoldPrice() async {
  // => Returns Future<double> (promise-like)
  // => async keyword enables await usage

  await Future.delayed(Duration(seconds: 1)); // => Simulates API call
                                              // => Waits 1 second

  return 6.61;                                // => Returns gold price per gram
                                              // => Future automatically wraps value
}

void main() async {
  // => main function marked async
  print('Fetching gold price...');    // => Immediate output

  double price = await fetchGoldPrice(); // => Awaits future completion
                                        // => Suspends execution until ready

  print('Current price: $price');       // => Output: Current price: 6.61
}
```

### Futures and Streams

**Futures** represent single asynchronous values:

```dart
Future<String> processPayment(double amount) async {
  // => amount: Payment amount to process
  await Future.delayed(Duration(seconds: 2)); // => Simulates processing
  return 'Payment of $amount processed';      // => Success message
}
```

**Streams** represent sequences of asynchronous values:

```dart
Stream<int> generatePrayerReminders() async* {
  // => async* creates stream generator
  for (int hour in [5, 12, 15, 18, 20]) {
    // => Prayer times (simplified)
    await Future.delayed(Duration(hours: 1)); // => Wait between reminders
    yield hour;                               // => Emit prayer time
                                              // => Continues to next iteration
  }
}
```

### Strong Typing with Type Inference

Dart combines static typing with intelligent type inference:

```dart
var amount = 1000.0;              // => Inferred as double
var recipient = 'Baitulmal';      // => Inferred as String
var transactions = <String>[];    // => Explicitly List<String>
                                  // => Empty list of strings

// amount = 'invalid';            // => Compile error! Type mismatch
transactions.add('Donation: 500'); // => OK, adding String
// transactions.add(500);         // => Compile error! Expects String
```

## Why Learn Dart?

### Flutter Ecosystem

Dart is the official language for Flutter, Google's UI framework for building natively compiled applications.

**Benefits**:

- **Hot reload** - See changes instantly without rebuilding
- **Rich widgets** - Extensive library of customizable UI components
- **Native performance** - Compiles to native ARM code
- **Single codebase** - One codebase for mobile, web, and desktop

### Performance

Dart compiles to highly optimized native code:

- **AOT compilation** - Ahead-of-time compilation for production (fast startup, predictable performance)
- **JIT compilation** - Just-in-time compilation for development (hot reload, fast iteration)
- **Tree shaking** - Removes unused code automatically
- **Native performance** - Comparable to Java and C#

### Type Safety

Sound null safety and strong typing prevent common errors:

```dart
class ZakatCalculator {
  final double nisabThreshold; // => Cannot be null or reassigned
                               // => Must be initialized in constructor

  ZakatCalculator(this.nisabThreshold); // => Constructor sets final field

  double? calculate(double? wealth) {   // => Nullable parameters and return
    if (wealth == null) return null;    // => Handle null explicitly
                                        // => Returns null if input null

    return wealth >= nisabThreshold     // => Ternary operator
        ? wealth * 0.025                // => Calculate 2.5% if above threshold
        : 0.0;                          // => Return 0 if below threshold
  }
}
```

## Prerequisites

Before learning Dart, you should have:

- **Programming fundamentals** - Variables, functions, control flow
- **Object-oriented concepts** - Classes, objects, inheritance
- **Basic understanding of types** - Static vs dynamic typing
- **Command-line familiarity** - Running commands in terminal

**Helpful but not required**:

- Experience with Java, JavaScript, or similar C-style languages
- Familiarity with asynchronous programming concepts
- Understanding of package management systems

## Use Cases

### Mobile Applications

Build native mobile apps with Flutter:

```dart
// Flutter widget for Zakat calculator
class ZakatCalculatorApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    // => Returns widget tree
    return MaterialApp(              // => Material Design app
      title: 'Zakat Calculator',    // => App title
      home: ZakatCalculatorScreen(), // => Main screen widget
    );
  }
}
```

### Web Applications

Compile to JavaScript for web deployment:

```dart
import 'dart:html';

void main() {
  // => Entry point for web app
  querySelector('#calculate')?.onClick.listen((_) {
    // => Find button by CSS selector
    // => Listen to click events
    var input = querySelector('#wealth') as InputElement;
    double wealth = double.parse(input.value ?? '0'); // => Parse input value
                                                      // => Default to '0' if null
    calculateAndDisplay(wealth); // => Process calculation
  });
}
```

### Server-Side Applications

Build backend services and REST APIs:

```dart
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as io;

void main() async {
  // => async main for server setup
  var handler = const Pipeline()       // => Request pipeline
      .addMiddleware(logRequests())    // => Add logging middleware
      .addHandler(_echoRequest);       // => Add request handler

  await io.serve(handler, 'localhost', 8080); // => Start server
                                              // => Listen on port 8080
  print('Server running on localhost:8080');
}

Response _echoRequest(Request request) {
  // => Handler function for requests
  return Response.ok('Request received'); // => Return 200 OK response
}
```

### Command-Line Tools

Create powerful CLI applications:

```dart
import 'dart:io';

void main(List<String> arguments) {
  // => arguments: Command-line arguments
  print('Zakat Calculator CLI');

  stdout.write('Enter wealth amount: '); // => Prompt without newline
  String? input = stdin.readLineSync();   // => Read user input
                                          // => Returns nullable String

  double wealth = double.tryParse(input ?? '') ?? 0.0; // => Parse safely
                                                       // => Default to 0.0

  calculateZakat(wealth); // => Process calculation
}
```

## Islamic Finance Examples

Throughout this tutorial, we'll use Islamic finance concepts to demonstrate Dart's features:

- **Zakat Calculator** - Calculating obligatory charity based on wealth thresholds
- **Murabaha Contract** - Cost-plus financing with transparent markup
- **Prayer Time Reminders** - Stream-based notifications for daily prayers
- **Donation Tracking** - Managing Sadaqah (voluntary charity) contributions
- **Halal Product Verification** - Checking product compliance

These real-world applications will help you understand both Dart programming and its practical applications in Islamic contexts.

## Next Steps

Now that you understand what Dart is and why it's valuable, proceed to:

1. **Initial Setup** - Install Dart SDK and set up your development environment
2. **Quick Start** - Build a complete Zakat Calculator application
3. **By Example** - Learn through 75-90 heavily annotated code examples
4. **By Concept** - Deep dive into Dart concepts with progressive tutorials

Start with [Initial Setup](/en/learn/software-engineering/programming-languages/dart/initial-setup) to install Dart and configure your development environment.
