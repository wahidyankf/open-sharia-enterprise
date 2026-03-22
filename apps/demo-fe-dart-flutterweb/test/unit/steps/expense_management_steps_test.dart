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
    '../../specs/apps/demo/fe/gherkin/expenses/expense-management.feature',
    (feature) {
      feature.scenario(
        'Creating an expense entry adds it to the entry list',
        (s) {
          s.given('the app is running', () async {
            // No-op: ServiceClient starts in a clean state.
          });

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

          s.when('alice navigates to the new entry form', () async {
            // No-op: navigation is a UI concern; service state is ready.
          });

          s.and(
            'alice fills in amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              // Captured in the submit step below.
            },
          );

          s.and('alice submits the entry form', () async {
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '10.50',
                currency: 'USD',
                category: 'food',
                description: 'Lunch',
                date: '2025-01-15',
                type: 'expense',
              ),
            );
          });

          s.then(
            'the entry list should contain an entry with description "Lunch"',
            () async {
              final response = await svc.listExpenses();
              expect(
                response.content.any((e) => e.description == 'Lunch'),
                isTrue,
              );
            },
          );
        },
      );

      feature.scenario(
        'Creating an income entry adds it to the entry list',
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
            'alice fills in amount "3000.00", currency "USD", category "salary", description "Monthly salary", date "2025-01-31", and type "income"',
            () async {},
          );

          s.and('alice submits the entry form', () async {
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '3000.00',
                currency: 'USD',
                category: 'salary',
                description: 'Monthly salary',
                date: '2025-01-31',
                type: 'income',
              ),
            );
          });

          s.then(
            'the entry list should contain an entry with description "Monthly salary"',
            () async {
              final response = await svc.listExpenses();
              expect(
                response.content.any((e) => e.description == 'Monthly salary'),
                isTrue,
              );
            },
          );
        },
      );

      feature.scenario('Clicking an entry shows its full details', (s) {
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
          'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
          () async {
            final expense = await svc.createExpense(
              const CreateExpenseRequest(
                amount: '10.50',
                currency: 'USD',
                category: 'food',
                description: 'Lunch',
                date: '2025-01-15',
                type: 'expense',
              ),
            );
            createdId = expense.id;
          },
        );

        s.when('alice clicks the entry "Lunch" in the list', () async {
          // No-op: selection is a UI concern; ID captured above.
        });

        s.then('the entry detail should display amount "10.50"', () async {
          final expense = await svc.getExpense(createdId);
          expect(expense.amount, equals('10.50'));
        });

        s.and('the entry detail should display currency "USD"', () async {
          final expense = await svc.getExpense(createdId);
          expect(expense.currency, equals('USD'));
        });

        s.and('the entry detail should display category "food"', () async {
          final expense = await svc.getExpense(createdId);
          expect(expense.category, equals('food'));
        });

        s.and('the entry detail should display description "Lunch"', () async {
          final expense = await svc.getExpense(createdId);
          expect(expense.description, equals('Lunch'));
        });

        s.and('the entry detail should display date "2025-01-15"', () async {
          final expense = await svc.getExpense(createdId);
          expect(expense.date, equals('2025-01-15'));
        });

        s.and('the entry detail should display type "expense"', () async {
          final expense = await svc.getExpense(createdId);
          expect(expense.type, equals('expense'));
        });
      });

      feature.scenario(
        'Entry list shows pagination for multiple entries',
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

          s.and('alice has created 3 entries', () async {
            for (var i = 1; i <= 3; i++) {
              await svc.createExpense(
                CreateExpenseRequest(
                  amount: '${i * 10}.00',
                  currency: 'USD',
                  category: 'food',
                  description: 'Entry $i',
                  date: '2025-01-0$i',
                  type: 'expense',
                ),
              );
            }
          });

          s.when('alice navigates to the entry list page', () async {});

          s.then(
            'the entry list should display pagination controls',
            () async {
              final response = await svc.listExpenses();
              // Pagination is present when the response carries a page/size/totalPages.
              expect(response.page, equals(0));
              expect(response.size, greaterThan(0));
              expect(response.totalPages, greaterThan(0));
            },
          );

          s.and('the entry list should show the total count', () async {
            final response = await svc.listExpenses();
            expect(response.totalElements, equals(3));
          });
        },
      );

      feature.scenario('Editing an entry updates the displayed values', (s) {
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
          'alice has created an entry with amount "10.00", currency "USD", category "food", description "Breakfast", date "2025-01-10", and type "expense"',
          () async {
            final expense = await svc.createExpense(
              const CreateExpenseRequest(
                amount: '10.00',
                currency: 'USD',
                category: 'food',
                description: 'Breakfast',
                date: '2025-01-10',
                type: 'expense',
              ),
            );
            createdId = expense.id;
          },
        );

        s.when('alice clicks the edit button on the entry "Breakfast"',
            () async {
          // No-op: edit navigation is a UI concern.
        });

        s.and(
          'alice changes the amount to "12.00" and description to "Updated breakfast"',
          () async {
            // Captured for the save step below.
          },
        );

        s.and('alice saves the changes', () async {
          await svc.updateExpense(
            createdId,
            const UpdateExpenseRequest(
              amount: '12.00',
              description: 'Updated breakfast',
            ),
          );
        });

        s.then('the entry detail should display amount "12.00"', () async {
          final expense = await svc.getExpense(createdId);
          expect(expense.amount, equals('12.00'));
        });

        s.and(
          'the entry detail should display description "Updated breakfast"',
          () async {
            final expense = await svc.getExpense(createdId);
            expect(expense.description, equals('Updated breakfast'));
          },
        );
      });

      feature.scenario('Deleting an entry removes it from the list', (s) {
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
          'alice has created an entry with amount "10.00", currency "USD", category "food", description "Snack", date "2025-01-05", and type "expense"',
          () async {
            final expense = await svc.createExpense(
              const CreateExpenseRequest(
                amount: '10.00',
                currency: 'USD',
                category: 'food',
                description: 'Snack',
                date: '2025-01-05',
                type: 'expense',
              ),
            );
            createdId = expense.id;
          },
        );

        s.when('alice clicks the delete button on the entry "Snack"',
            () async {
          // No-op: button click is a UI concern.
        });

        s.and('alice confirms the deletion', () async {
          await svc.deleteExpense(createdId);
        });

        s.then(
          'the entry list should not contain an entry with description "Snack"',
          () async {
            final response = await svc.listExpenses();
            expect(
              response.content.any((e) => e.description == 'Snack'),
              isFalse,
            );
          },
        );
      });

      feature.scenario(
        'Unauthenticated visitor cannot access the entry form',
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

          s.and('alice has logged out', () async {
            await svc.logout();
          });

          s.when('alice navigates to the new entry form URL directly',
              () async {
            // No-op: navigation is a UI routing concern.
          });

          s.then('alice should be redirected to the login page', () async {
            // Without an active session, any expense operation throws UnauthorizedError.
            expect(svc.isAuthenticated, isFalse);
            expect(
              () async => svc.listExpenses(),
              throwsA(isA<UnauthorizedError>()),
            );
          });
        },
      );
    },
  );
}
