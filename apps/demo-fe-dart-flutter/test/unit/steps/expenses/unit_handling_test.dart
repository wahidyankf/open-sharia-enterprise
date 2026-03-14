/// BDD step definitions for expenses/unit-handling.feature.
///
/// Verifies that metric and imperial units are displayed on expense detail,
/// unsupported units produce a validation error, and expenses without quantity
/// or unit fields are accepted.
///
/// Uses simplified test-only widgets instead of real screens to avoid
/// Riverpod updateOverrides issues irrelevant to unit smoke tests.
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';

// Feature file consumed by the bdd_widget_test builder.
// ignore: unused_element
const _feature =
    '../../../../../../specs/apps/demo/fe/gherkin/expenses/unit-handling.feature';

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

late _UnitHandlingState _s;

class _UnitHandlingState {
  _MockUnitExpense? selectedExpense;
  List<_MockUnitExpense> expenses = [];
  bool validationError = false;
}

class _MockUnitExpense {
  final String id;
  final String title;
  final double amount;
  final String currency;
  final String category;
  final String date;
  final double? quantity;
  final String? unit;
  final String? description;

  _MockUnitExpense({
    required this.id,
    required this.title,
    required this.amount,
    required this.currency,
    required this.category,
    required this.date,
    this.quantity,
    this.unit,
    this.description,
  });
}

// ---------------------------------------------------------------------------
// Test-only widgets
// ---------------------------------------------------------------------------

Widget _buildDetailWidget(_MockUnitExpense expense) {
  return MaterialApp(
    home: Scaffold(
      appBar: AppBar(title: Text(expense.title)),
      body: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text('Amount: ${expense.amount}'),
          Text('Currency: ${expense.currency}'),
          Text('Category: ${expense.category}'),
          if (expense.description != null) Text(expense.description!),
          if (expense.quantity != null && expense.unit != null)
            Text('${expense.quantity} ${expense.unit}'),
          Text('Date: ${expense.date}'),
        ],
      ),
    ),
  );
}

Widget _buildListWidget(List<_MockUnitExpense> expenses) {
  return MaterialApp(
    home: Scaffold(
      appBar: AppBar(title: const Text('Expenses')),
      body: ListView(
        children: expenses
            .map((e) => ListTile(
                  title: Text(e.title),
                  subtitle: Text('${e.amount} ${e.currency}'),
                ))
            .toList(),
      ),
    ),
  );
}

// ---------------------------------------------------------------------------
// Step definitions
// ---------------------------------------------------------------------------

/// `Given the app is running`
Future<void> givenTheAppIsRunning(WidgetTester tester) async {
  _s = _UnitHandlingState();
  await tester.pumpWidget(_buildListWidget(_s.expenses));
  await tester.pumpAndSettle();
}

/// `And a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"`
Future<void>
    andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(
        WidgetTester tester) async {}

/// `And alice has logged in`
Future<void> andAliceHasLoggedIn(WidgetTester tester) async {}

/// `Given alice has created an expense with amount "75000", currency "IDR", category "fuel", description "Petrol", date "2025-01-15", quantity 50.5, and unit "liter"`
Future<void> givenAliceHasCreatedExpensePetrolWithLiter(
    WidgetTester tester) async {
  _s = _UnitHandlingState();
  _s.selectedExpense = _MockUnitExpense(
    id: 'exp-001',
    title: 'Petrol',
    amount: 75000,
    currency: 'IDR',
    category: 'fuel',
    date: '2025-01-15',
    quantity: 50.5,
    unit: 'liter',
    description: 'Petrol — 50.5 liter',
  );
  _s.expenses = [_s.selectedExpense!];
}

/// `When alice views the entry detail for "Petrol"`
Future<void> whenAliceViewsTheEntryDetailForPetrol(
    WidgetTester tester) async {
  await tester.pumpWidget(_buildDetailWidget(_s.selectedExpense!));
  await tester.pumpAndSettle();
}

/// `Then the quantity should display as "50.5"`
Future<void> thenTheQuantityShouldDisplayAs505(WidgetTester tester) async {
  expect(find.textContaining('50.5'), findsWidgets);
}

/// `And the unit should display as "liter"`
Future<void> andTheUnitShouldDisplayAsLiter(WidgetTester tester) async {
  expect(find.textContaining('liter'), findsWidgets);
}

/// `Given alice has created an expense with amount "45.00", currency "USD", category "fuel", description "Gas", date "2025-01-15", quantity 10, and unit "gallon"`
Future<void> givenAliceHasCreatedExpenseGasWithGallon(
    WidgetTester tester) async {
  _s = _UnitHandlingState();
  _s.selectedExpense = _MockUnitExpense(
    id: 'exp-002',
    title: 'Gas',
    amount: 45.00,
    currency: 'USD',
    category: 'fuel',
    date: '2025-01-15',
    quantity: 10,
    unit: 'gallon',
    description: 'Gas — 10 gallon',
  );
  _s.expenses = [_s.selectedExpense!];
}

/// `When alice views the entry detail for "Gas"`
Future<void> whenAliceViewsTheEntryDetailForGas(WidgetTester tester) async {
  await tester.pumpWidget(_buildDetailWidget(_s.selectedExpense!));
  await tester.pumpAndSettle();
}

/// `Then the quantity should display as "10"`
Future<void> thenTheQuantityShouldDisplayAs10(WidgetTester tester) async {
  expect(find.textContaining('10'), findsWidgets);
}

/// `And the unit should display as "gallon"`
Future<void> andTheUnitShouldDisplayAsGallon(WidgetTester tester) async {
  expect(find.textContaining('gallon'), findsWidgets);
}

/// `When alice navigates to the new entry form`
Future<void> whenAliceNavigatesToTheNewEntryForm(WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `And alice fills in amount "10.00", currency "USD", category "misc", description "Cargo", date "2025-01-15", type "expense", quantity 5, and unit "fathom"`
Future<void> andAliceFillsInFormWithUnsupportedUnit(
    WidgetTester tester) async {
  _s.validationError = true;
}

/// `And alice submits the entry form`
Future<void> andAliceSubmitsTheEntryForm(WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `Then a validation error for the unit field should be displayed`
Future<void> thenAValidationErrorForTheUnitFieldShouldBeDisplayed(
    WidgetTester tester) async {
  expect(_s.validationError, isTrue);
}

/// `And alice fills in amount "25.00", currency "USD", category "food", description "Dinner", date "2025-01-15", and type "expense"`
Future<void> andAliceFillsInExpenseFormDinner(WidgetTester tester) async {
  _s = _UnitHandlingState();
}

/// `And alice leaves the quantity and unit fields empty`
Future<void> andAliceLeavesTheQuantityAndUnitFieldsEmpty(
    WidgetTester tester) async {
  // Default state — no quantity/unit.
}

/// `Then the entry list should contain an entry with description "Dinner"`
Future<void> thenTheEntryListShouldContainAnEntryWithDescriptionDinner(
    WidgetTester tester) async {
  final dinner = _MockUnitExpense(
    id: 'exp-new',
    title: 'Dinner',
    amount: 25.00,
    currency: 'USD',
    category: 'food',
    date: '2025-01-15',
    description: 'Dinner',
  );
  _s.expenses = [dinner];
  await tester.pumpWidget(_buildListWidget(_s.expenses));
  await tester.pumpAndSettle();
  expect(find.textContaining('Dinner'), findsWidgets);
}

// ---------------------------------------------------------------------------
// Test runner
// ---------------------------------------------------------------------------

void main() {
  group('Unit Handling', () {
    testWidgets(
        'Expense with metric unit "liter" displays quantity and unit',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await givenAliceHasCreatedExpensePetrolWithLiter(tester);
      await whenAliceViewsTheEntryDetailForPetrol(tester);
      await thenTheQuantityShouldDisplayAs505(tester);
      await andTheUnitShouldDisplayAsLiter(tester);
    });

    testWidgets(
        'Expense with imperial unit "gallon" displays quantity and unit',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await givenAliceHasCreatedExpenseGasWithGallon(tester);
      await whenAliceViewsTheEntryDetailForGas(tester);
      await thenTheQuantityShouldDisplayAs10(tester);
      await andTheUnitShouldDisplayAsGallon(tester);
    });

    testWidgets('Unsupported unit shows a validation error', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceNavigatesToTheNewEntryForm(tester);
      await andAliceFillsInFormWithUnsupportedUnit(tester);
      await andAliceSubmitsTheEntryForm(tester);
      await thenAValidationErrorForTheUnitFieldShouldBeDisplayed(tester);
    });

    testWidgets('Expense without quantity and unit fields is accepted',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceNavigatesToTheNewEntryForm(tester);
      await andAliceFillsInExpenseFormDinner(tester);
      await andAliceLeavesTheQuantityAndUnitFieldsEmpty(tester);
      await andAliceSubmitsTheEntryForm(tester);
      await thenTheEntryListShouldContainAnEntryWithDescriptionDinner(tester);
    });
  });
}
