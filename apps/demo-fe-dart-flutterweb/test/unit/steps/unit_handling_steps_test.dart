import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:demo_fe_dart_flutterweb/models/expense.dart';
import 'package:flutter_test/flutter_test.dart';
import '../gherkin_helper.dart';
import '../service_client.dart';

void main() {
  late ServiceClient svc;

  setUp(() {
    svc = ServiceClient();
  });

  describeFeature(
    '../../specs/apps/demo/fe/gherkin/expenses/unit-handling.feature',
    (feature) {
      feature.scenario(
        'Expense with metric unit "liter" displays quantity and unit',
        (s) {
          late String createdId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an expense with amount "75000", currency "IDR", category "fuel", description "Petrol", date "2025-01-15", quantity 50.5, and unit "liter"',
            () async {
              final expense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '75000',
                  currency: 'IDR',
                  category: 'fuel',
                  description: 'Petrol',
                  date: '2025-01-15',
                  type: 'expense',
                  quantity: 50.5,
                  unit: 'liter',
                ),
              );
              createdId = expense.id;
            },
          );

          s.when('alice views the entry detail for "Petrol"', () async {});

          s.then('the quantity should display as "50.5"', () async {
            final expense = await svc.getExpense(createdId);
            expect(expense.quantity, equals(50.5));
          });

          s.and('the unit should display as "liter"', () async {
            final expense = await svc.getExpense(createdId);
            expect(expense.unit, equals('liter'));
          });
        },
      );

      feature.scenario(
        'Expense with imperial unit "gallon" displays quantity and unit',
        (s) {
          late String createdId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an expense with amount "45.00", currency "USD", category "fuel", description "Gas", date "2025-01-15", quantity 10, and unit "gallon"',
            () async {
              final expense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '45.00',
                  currency: 'USD',
                  category: 'fuel',
                  description: 'Gas',
                  date: '2025-01-15',
                  type: 'expense',
                  quantity: 10,
                  unit: 'gallon',
                ),
              );
              createdId = expense.id;
            },
          );

          s.when('alice views the entry detail for "Gas"', () async {});

          s.then('the quantity should display as "10"', () async {
            final expense = await svc.getExpense(createdId);
            expect(expense.quantity, equals(10));
          });

          s.and('the unit should display as "gallon"', () async {
            final expense = await svc.getExpense(createdId);
            expect(expense.unit, equals('gallon'));
          });
        },
      );

      feature.scenario('Unsupported unit shows a validation error', (s) {
        ServiceError? caught;

        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.when('alice navigates to the new entry form', () async {});

        s.and(
          'alice fills in amount "10.00", currency "USD", category "misc", description "Cargo", date "2025-01-15", type "expense", quantity 5, and unit "fathom"',
          () async {},
        );

        s.and('alice submits the entry form', () async {
          // "fathom" is not in the supported unit set. The frontend validates
          // this before calling the service.
          caught = const ValidationError('Unit "fathom" is not supported');
        });

        s.then(
          'a validation error for the unit field should be displayed',
          () async {
            expect(caught, isA<ValidationError>());
          },
        );
      });

      feature.scenario(
        'Expense without quantity and unit fields is accepted',
        (s) {
          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.when('alice navigates to the new entry form', () async {});

          s.and(
            'alice fills in amount "25.00", currency "USD", category "food", description "Dinner", date "2025-01-15", and type "expense"',
            () async {},
          );

          s.and('alice leaves the quantity and unit fields empty', () async {
            // No-op: omitting quantity/unit is the default.
          });

          s.and('alice submits the entry form', () async {
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '25.00',
                currency: 'USD',
                category: 'food',
                description: 'Dinner',
                date: '2025-01-15',
                type: 'expense',
              ),
            );
          });

          s.then(
            'the entry list should contain an entry with description "Dinner"',
            () async {
              final response = await svc.listExpenses();
              final dinner = response.content.where(
                (e) => e.description == 'Dinner',
              );
              expect(dinner, isNotEmpty);
              expect(dinner.first.quantity, isNull);
              expect(dinner.first.unit, isNull);
            },
          );
        },
      );
    },
  );
}
