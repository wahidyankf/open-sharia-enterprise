---
title: "Quick Start"
weight: 100002
date: 2025-01-29T00:00:00+07:00
draft: false
description: "Build a complete Zakat Calculator CLI application to learn Dart fundamentals"
tags: ["dart", "quick-start", "tutorial", "zakat"]
---

This quick start guide walks you through building a complete Zakat Calculator CLI application. You'll learn Dart's core features by building a practical application that calculates Islamic obligatory charity.

## What You'll Build

A command-line Zakat Calculator that:

- Prompts for wealth and asset information
- Calculates nisab threshold based on current gold prices
- Determines if Zakat is due
- Calculates the exact Zakat amount (2.5% of eligible wealth)
- Displays formatted results

**Skills covered**:

- Variables and data types
- Functions and parameters
- Classes and objects
- Null safety
- Collections (Lists and Maps)
- User input/output
- String formatting
- Error handling

## Prerequisites

Ensure you have:

- Dart SDK installed (verify with `dart --version`)
- A text editor or IDE
- Completed [Initial Setup](/en/learn/software-engineering/programming-languages/dart/initial-setup)

## Project Setup

Create a new Dart project:

```bash
mkdir zakat_calculator
cd zakat_calculator
dart create . --template console
```

This creates:

```
zakat_calculator/
├── bin/
│   └── zakat_calculator.dart  # Main program (we'll edit this)
├── lib/
│   └── zakat_calculator.dart  # Library code (we'll create classes here)
├── test/
│   └── zakat_calculator_test.dart
├── pubspec.yaml
└── README.md
```

## Step 1: Understanding the Domain

### Zakat Basics

**Zakat** is one of the Five Pillars of Islam, requiring Muslims to give 2.5% of their wealth annually to those in need.

**Key concepts**:

- **Nisab**: Minimum wealth threshold (equivalent to 85 grams of gold)
- **Eligible wealth**: Cash, gold, silver, business inventory, investments
- **Rate**: 2.5% of eligible wealth exceeding nisab
- **Lunar year**: Calculated annually after wealth held for one lunar year

## Step 2: Create Data Models

Edit `lib/zakat_calculator.dart` to define our data structures:

```dart
// lib/zakat_calculator.dart

/// Represents different types of wealth subject to Zakat
enum WealthType {
  // => Enumeration of wealth categories
  cash,           // => Cash holdings
  gold,           // => Gold assets
  silver,         // => Silver assets
  investments,    // => Investment portfolios
  business,       // => Business inventory/assets
}

/// Represents a single wealth item
class WealthItem {
  // => Models individual wealth component
  final WealthType type;        // => Category of wealth
  final String description;     // => Human-readable description
  final double amount;          // => Monetary value

  WealthItem({
    // => Constructor with named parameters
    required this.type,         // => Must provide type
    required this.description,  // => Must provide description
    required this.amount,       // => Must provide amount
  });

  @override
  String toString() {
    // => Custom string representation
    return '$description: Rp${_formatCurrency(amount)}';
    // => Returns formatted string
    // => Example: "Savings: Rp1,000,000"
  }

  static String _formatCurrency(double amount) {
    // => Helper method for formatting
    // => Static: belongs to class, not instance
    return amount.toStringAsFixed(0)
        // => Converts to string with 0 decimal places
        .replaceAllMapped(
          // => Applies pattern matching
          RegExp(r'(\d{1,3})(?=(\d{3})+(?!\d))'),
          // => Regex: finds groups of 3 digits
          (Match match) => '${match[1]},',
          // => Adds comma after each group
        );
    // => Returns formatted currency string
  }
}

/// Represents the complete Zakat calculation
class ZakatCalculation {
  // => Models calculation results
  final double totalWealth;     // => Sum of all wealth
  final double nisabThreshold;  // => Minimum threshold
  final double zakatAmount;     // => Calculated Zakat due
  final bool isZakatDue;        // => Whether Zakat is obligatory

  ZakatCalculation({
    // => Constructor with named parameters
    required this.totalWealth,
    required this.nisabThreshold,
    required this.zakatAmount,
    required this.isZakatDue,
  });

  @override
  String toString() {
    // => String representation of calculation
    var buffer = StringBuffer();  // => Efficient string builder
                                  // => Better than concatenation

    buffer.writeln('=== Zakat Calculation Results ===');
    // => Adds line with newline
    buffer.writeln('Total Wealth: Rp${_formatCurrency(totalWealth)}');
    buffer.writeln('Nisab Threshold: Rp${_formatCurrency(nisabThreshold)}');
    buffer.writeln();  // => Empty line for spacing

    if (isZakatDue) {
      // => Zakat is obligatory
      buffer.writeln('✓ Zakat is DUE');
      buffer.writeln('Zakat Amount: Rp${_formatCurrency(zakatAmount)}');
      // => Shows amount due
    } else {
      // => Wealth below threshold
      buffer.writeln('✗ Zakat is NOT due (below nisab threshold)');
    }

    return buffer.toString();  // => Converts buffer to string
  }

  static String _formatCurrency(double amount) {
    // => Same formatting logic as WealthItem
    return amount.toStringAsFixed(0).replaceAllMapped(
          RegExp(r'(\d{1,3})(?=(\d{3})+(?!\d))'),
          (Match match) => '${match[1]},',
        );
  }
}
```

## Step 3: Create Zakat Calculator Service

Add the calculator logic to `lib/zakat_calculator.dart`:

```dart
/// Service class for Zakat calculations
class ZakatCalculatorService {
  // => Business logic for calculations
  static const double ZAKAT_RATE = 0.025;  // => 2.5% rate (constant)
  static const double GOLD_NISAB_GRAMS = 85.0;  // => 85 grams threshold

  final double goldPricePerGram;  // => Current market price
  final List<WealthItem> wealthItems = [];  // => Collection of wealth
                                            // => Initialized as empty list

  ZakatCalculatorService({
    // => Constructor
    required this.goldPricePerGram,  // => Must provide gold price
  });

  void addWealthItem(WealthItem item) {
    // => Adds wealth to portfolio
    wealthItems.add(item);  // => Appends to list
  }

  double get totalWealth {
    // => Getter computed property
    // => Calculates total on access
    return wealthItems.fold<double>(
      // => fold: reduce collection to single value
      0.0,  // => Initial accumulator value
      (sum, item) => sum + item.amount,
      // => Accumulator function
      // => sum: current total
      // => item: current wealth item
      // => Returns: new total
    );
  }

  double get nisabThreshold {
    // => Computed nisab threshold
    return GOLD_NISAB_GRAMS * goldPricePerGram;
    // => 85 grams × current price per gram
  }

  ZakatCalculation calculate() {
    // => Performs Zakat calculation
    final wealth = totalWealth;  // => Get current total wealth
    final nisab = nisabThreshold;  // => Get current threshold
    final isZakatDue = wealth >= nisab;  // => Compare wealth to nisab
                                         // => true if Zakat obligatory

    final zakatAmount = isZakatDue
        ? wealth * ZAKAT_RATE  // => Calculate 2.5% if due
        : 0.0;                 // => 0 if not due

    return ZakatCalculation(
      // => Create result object
      totalWealth: wealth,
      nisabThreshold: nisab,
      zakatAmount: zakatAmount,
      isZakatDue: isZakatDue,
    );
  }

  void reset() {
    // => Clear all wealth items
    wealthItems.clear();  // => Removes all elements from list
  }
}
```

## Step 4: Create Main Program

Edit `bin/zakat_calculator.dart` for the CLI interface:

```dart
// bin/zakat_calculator.dart

import 'dart:io';  // => Import I/O library for console
                   // => Provides stdin, stdout, exit

import 'package:zakat_calculator/zakat_calculator.dart';
// => Import our library
// => Package import uses project name

void main() {
  // => Entry point of program
  print('=== Zakat Calculator ===\n');

  // Get gold price
  final goldPrice = promptForGoldPrice();
  // => Gets current gold price from user
  // => final: variable can't be reassigned

  // Create calculator service
  final calculator = ZakatCalculatorService(
    goldPricePerGram: goldPrice,
  );
  // => Instantiates calculator with gold price

  // Collect wealth items
  print('\nEnter your wealth items (enter empty description to finish):');
  collectWealthItems(calculator);
  // => Prompts user for wealth items
  // => Adds items to calculator

  // Calculate and display results
  final result = calculator.calculate();
  // => Performs calculation
  // => Returns ZakatCalculation object

  print('\n${result}');
  // => Prints formatted results
  // => Calls toString() on result
}

double promptForGoldPrice() {
  // => Gets gold price from user input
  while (true) {
    // => Loop until valid input
    stdout.write('Enter current gold price per gram (IDR): ');
    // => Prompt without newline
    final input = stdin.readLineSync();
    // => Read line from console
    // => Returns String? (nullable)

    if (input == null || input.isEmpty) {
      // => Check for null or empty input
      print('Please enter a valid price.');
      continue;  // => Skip to next iteration
    }

    final price = double.tryParse(input);
    // => Attempt to parse as double
    // => Returns null if parsing fails

    if (price == null || price <= 0) {
      // => Validate parsed value
      print('Please enter a positive number.');
      continue;  // => Try again
    }

    return price;  // => Valid price entered, return it
  }
}

void collectWealthItems(ZakatCalculatorService calculator) {
  // => Collects wealth items from user
  // => calculator: service to add items to

  while (true) {
    // => Loop until user finishes
    stdout.write('\nDescription (or press Enter to finish): ');
    final description = stdin.readLineSync();
    // => Read wealth item description

    if (description == null || description.isEmpty) {
      // => Empty input signals completion
      break;  // => Exit loop
    }

    final type = promptForWealthType();
    // => Get wealth category from user
    final amount = promptForAmount();
    // => Get monetary amount from user

    final item = WealthItem(
      // => Create wealth item object
      type: type,
      description: description,
      amount: amount,
    );

    calculator.addWealthItem(item);
    // => Add item to calculator
    print('Added: ${item}');
    // => Confirm addition to user
  }
}

WealthType promptForWealthType() {
  // => Gets wealth type from user
  print('\nSelect wealth type:');
  print('1. Cash');
  print('2. Gold');
  print('3. Silver');
  print('4. Investments');
  print('5. Business');

  while (true) {
    // => Loop until valid selection
    stdout.write('Enter number (1-5): ');
    final input = stdin.readLineSync();
    // => Read user choice

    final choice = int.tryParse(input ?? '');
    // => Parse as integer
    // => Uses empty string if input is null

    switch (choice) {
      // => Match choice to wealth type
      case 1:
        return WealthType.cash;
      case 2:
        return WealthType.gold;
      case 3:
        return WealthType.silver;
      case 4:
        return WealthType.investments;
      case 5:
        return WealthType.business;
      default:
        // => Invalid choice
        print('Invalid selection. Please enter 1-5.');
        // => Continues loop
    }
  }
}

double promptForAmount() {
  // => Gets amount from user
  while (true) {
    // => Loop until valid input
    stdout.write('Amount (IDR): ');
    final input = stdin.readLineSync();

    if (input == null || input.isEmpty) {
      print('Please enter an amount.');
      continue;
    }

    final amount = double.tryParse(input);
    // => Attempt to parse as double

    if (amount == null || amount < 0) {
      // => Validate parsed amount
      print('Please enter a non-negative number.');
      continue;
    }

    return amount;  // => Valid amount entered
  }
}
```

## Step 5: Run the Application

Execute your Zakat Calculator:

```bash
dart run
```

**Sample interaction**:

```
=== Zakat Calculator ===

Enter current gold price per gram (IDR): 1000000

Enter your wealth items (enter empty description to finish):

Description (or press Enter to finish): Savings Account

Select wealth type:
1. Cash
2. Gold
3. Silver
4. Investments
5. Business
Enter number (1-5): 1
Amount (IDR): 50000000
Added: Savings Account: Rp50,000,000

Description (or press Enter to finish): Gold Jewelry

Select wealth type:
1. Cash
2. Gold
3. Silver
4. Investments
5. Business
Enter number (1-5): 2
Amount (IDR): 30000000
Added: Gold Jewelry: Rp30,000,000

Description (or press Enter to finish): Investment Portfolio

Select wealth type:
1. Cash
2. Gold
3. Silver
4. Investments
5. Business
Enter number (1-5): 4
Amount (IDR): 20000000
Added: Investment Portfolio: Rp20,000,000

Description (or press Enter to finish):

=== Zakat Calculation Results ===
Total Wealth: Rp100,000,000
Nisab Threshold: Rp85,000,000

✓ Zakat is DUE
Zakat Amount: Rp2,500,000
```

## Understanding the Code

### Variables and Types

```dart
final double goldPricePerGram;  // => final: assigned once, immutable
                                // => double: decimal number type

const double ZAKAT_RATE = 0.025;  // => const: compile-time constant
                                  // => Always 2.5%, never changes

var amount = 1000.0;  // => var: type inferred from value
                      // => Inferred as double
```

**Type categories**:

- **final**: Runtime constant (assigned once, value known at runtime)
- **const**: Compile-time constant (value known at compile time)
- **var**: Type inferred from assignment
- **Explicit type**: Specified type (double, String, int, etc.)

### Functions

```dart
double promptForGoldPrice() {
  // => Function declaration
  // => double: return type
  // => (): no parameters
  // Function body
  return price;  // => Returns double value
}

void collectWealthItems(ZakatCalculatorService calculator) {
  // => void: returns no value
  // => calculator: parameter of type ZakatCalculatorService
  // Function body
}
```

### Classes and Objects

```dart
class WealthItem {
  // => Class definition
  final WealthType type;  // => Instance field
  final String description;
  final double amount;

  WealthItem({
    required this.type,  // => Named constructor parameter
                         // => required: must be provided
                         // => this.type: assigns to field directly
    required this.description,
    required this.amount,
  });
}

// Creating instance:
final item = WealthItem(
  type: WealthType.cash,  // => Named argument
  description: 'Savings',
  amount: 50000000.0,
);
```

### Null Safety

Dart prevents null reference errors at compile time:

```dart
String? input = stdin.readLineSync();
// => String?: nullable type (can be null)
// => String: non-nullable (never null)

if (input == null || input.isEmpty) {
  // => Must check null before using
  return;
}

// After null check, input is promoted to non-nullable
print(input.toUpperCase());  // => Safe: null checked above
```

### Collections

**Lists** (ordered collections):

```dart
final List<WealthItem> wealthItems = [];
// => List<WealthItem>: list of WealthItem objects
// => []: empty list literal

wealthItems.add(item);  // => Adds item to end
wealthItems.clear();    // => Removes all items
```

**Maps** (key-value pairs):

```dart
final Map<String, double> prices = {
  // => Map<String, double>: string keys, double values
  'gold': 1000000.0,    // => Key: 'gold', Value: 1000000.0
  'silver': 15000.0,
};

prices['gold'];  // => Access value by key: 1000000.0
prices['platinum'] = 2000000.0;  // => Add new entry
```

### String Formatting

```dart
var name = 'Ahmad';
var age = 30;

// String interpolation
print('Name: $name, Age: $age');
// => Output: Name: Ahmad, Age: 30

// Expression interpolation
print('Next year: ${age + 1}');
// => Output: Next year: 31

// StringBuffer for efficiency
var buffer = StringBuffer();
buffer.writeln('Line 1');
buffer.writeln('Line 2');
print(buffer.toString());
// => Output: Line 1
//            Line 2
```

### Error Handling

```dart
final amount = double.tryParse(input);
// => tryParse: returns null if parsing fails
// => Safe alternative to parse (which throws exception)

if (amount == null) {
  // => Check for parsing failure
  print('Invalid input');
  return;
}

// amount is non-null here
print('Valid amount: $amount');
```

## Testing Your Application

Create tests in `test/zakat_calculator_test.dart`:

```dart
import 'package:test/test.dart';  // => Testing framework
import 'package:zakat_calculator/zakat_calculator.dart';

void main() {
  // => Test suite entry point
  group('ZakatCalculatorService', () {
    // => Group related tests
    test('calculates Zakat correctly when due', () {
      // => Individual test case
      final calculator = ZakatCalculatorService(
        goldPricePerGram: 1000000.0,
      );

      calculator.addWealthItem(WealthItem(
        type: WealthType.cash,
        description: 'Savings',
        amount: 100000000.0,  // 100 million IDR
      ));

      final result = calculator.calculate();
      // => Perform calculation

      expect(result.isZakatDue, isTrue);
      // => Assert Zakat is due
      expect(result.zakatAmount, equals(2500000.0));
      // => Assert amount is 2.5%
      expect(result.totalWealth, equals(100000000.0));
      // => Assert total wealth correct
    });

    test('returns no Zakat when below nisab', () {
      final calculator = ZakatCalculatorService(
        goldPricePerGram: 1000000.0,
      );

      calculator.addWealthItem(WealthItem(
        type: WealthType.cash,
        description: 'Savings',
        amount: 50000000.0,  // 50 million IDR (below nisab)
      ));

      final result = calculator.calculate();

      expect(result.isZakatDue, isFalse);
      // => Assert Zakat not due
      expect(result.zakatAmount, equals(0.0));
      // => Assert amount is zero
    });
  });
}
```

**Run tests**:

```bash
dart test
# => Runs all tests
# => Output shows pass/fail for each test
```

## Next Steps

Congratulations! You've built a complete Zakat Calculator and learned:

- ✓ Dart syntax and basic types
- ✓ Functions and classes
- ✓ Null safety system
- ✓ Collections (Lists and Maps)
- ✓ User input/output
- ✓ String formatting
- ✓ Basic testing

**Continue learning**:

1. **By Example** - Learn through 75-90 heavily annotated code examples covering:
   - Advanced null safety patterns
   - Asynchronous programming (async/await)
   - Error handling with try-catch
   - Functional programming with collections
   - Object-oriented patterns

2. **By Concept** - Deep dive into Dart concepts with progressive tutorials:
   - Type system deep dive
   - Object-oriented programming
   - Functional programming techniques
   - Asynchronous programming mastery
   - Package development

**Enhance this application**:

- Add persistent storage (save calculations to file)
- Create a web interface using `dart:html`
- Build a Flutter mobile app version
- Add support for multiple currencies
- Implement prayer time reminders
- Add Sadaqah (voluntary charity) tracking

Proceed to [By Example](/en/learn/software-engineering/programming-languages/dart/by-example) to learn through annotated code examples,.
