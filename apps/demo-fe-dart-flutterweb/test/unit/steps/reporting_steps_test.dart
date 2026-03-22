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
    '../../specs/apps/demo/fe/gherkin/expenses/reporting.feature',
    (feature) {
      feature.scenario(
        'P&L report displays income total, expense total, and net for a period',
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

          s.and(
            'alice has created an income entry of "5000.00" USD on "2025-01-15"',
            () async {
              await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '5000.00',
                  currency: 'USD',
                  category: 'salary',
                  description: 'January salary',
                  date: '2025-01-15',
                  type: 'income',
                ),
              );
            },
          );

          s.and(
            'alice has created an expense entry of "150.00" USD on "2025-01-20"',
            () async {
              await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '150.00',
                  currency: 'USD',
                  category: 'food',
                  description: 'Groceries',
                  date: '2025-01-20',
                  type: 'expense',
                ),
              );
            },
          );

          s.when('alice navigates to the reporting page', () async {});

          s.and(
            'alice selects date range "2025-01-01" to "2025-01-31" with currency "USD"',
            () async {},
          );

          s.then('the report should display income total "5000.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2025-01-01',
              endDate: '2025-01-31',
              currency: 'USD',
            );
            expect(report.totalIncome, equals('5000.00'));
          });

          s.and('the report should display expense total "150.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2025-01-01',
              endDate: '2025-01-31',
              currency: 'USD',
            );
            expect(report.totalExpense, equals('150.00'));
          });

          s.and('the report should display net "4850.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2025-01-01',
              endDate: '2025-01-31',
              currency: 'USD',
            );
            expect(report.net, equals('4850.00'));
          });
        },
      );

      feature.scenario('P&L breakdown shows category-level amounts', (s) {
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
          'alice has created income entries in categories "salary" and "freelance"',
          () async {
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '3000.00',
                currency: 'USD',
                category: 'salary',
                description: 'Salary',
                date: '2025-01-15',
                type: 'income',
              ),
            );
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '500.00',
                currency: 'USD',
                category: 'freelance',
                description: 'Freelance project',
                date: '2025-01-20',
                type: 'income',
              ),
            );
          },
        );

        s.and(
          'alice has created expense entries in category "transport"',
          () async {
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '75.00',
                currency: 'USD',
                category: 'transport',
                description: 'Bus pass',
                date: '2025-01-10',
                type: 'expense',
              ),
            );
          },
        );

        s.when('alice navigates to the reporting page', () async {});

        s.and(
          'alice selects the appropriate date range and currency "USD"',
          () async {},
        );

        s.then(
          'the income breakdown should list "salary" and "freelance" categories',
          () async {
            final report = await svc.getPLReport(
              startDate: '2025-01-01',
              endDate: '2025-01-31',
              currency: 'USD',
            );
            final incomeCategories =
                report.incomeBreakdown.map((c) => c.category).toList();
            expect(incomeCategories, containsAll(['salary', 'freelance']));
          },
        );

        s.and(
          'the expense breakdown should list "transport" category',
          () async {
            final report = await svc.getPLReport(
              startDate: '2025-01-01',
              endDate: '2025-01-31',
              currency: 'USD',
            );
            final expenseCategories =
                report.expenseBreakdown.map((c) => c.category).toList();
            expect(expenseCategories, contains('transport'));
          },
        );
      });

      feature.scenario(
        'Income entries are excluded from expense total',
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

          s.and(
            'alice has created only an income entry of "1000.00" USD on "2025-03-05"',
            () async {
              await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '1000.00',
                  currency: 'USD',
                  category: 'salary',
                  description: 'March salary',
                  date: '2025-03-05',
                  type: 'income',
                ),
              );
            },
          );

          s.when('alice views the P&L report for March 2025 in USD', () async {});

          s.then('the report should display income total "1000.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2025-03-01',
              endDate: '2025-03-31',
              currency: 'USD',
            );
            expect(report.totalIncome, equals('1000.00'));
          });

          s.and('the report should display expense total "0.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2025-03-01',
              endDate: '2025-03-31',
              currency: 'USD',
            );
            expect(report.totalExpense, equals('0.00'));
          });
        },
      );

      feature.scenario(
        'Expense entries are excluded from income total',
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

          s.and(
            'alice has created only an expense entry of "75.00" USD on "2025-04-10"',
            () async {
              await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '75.00',
                  currency: 'USD',
                  category: 'food',
                  description: 'April groceries',
                  date: '2025-04-10',
                  type: 'expense',
                ),
              );
            },
          );

          s.when('alice views the P&L report for April 2025 in USD', () async {});

          s.then('the report should display income total "0.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2025-04-01',
              endDate: '2025-04-30',
              currency: 'USD',
            );
            expect(report.totalIncome, equals('0.00'));
          });

          s.and('the report should display expense total "75.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2025-04-01',
              endDate: '2025-04-30',
              currency: 'USD',
            );
            expect(report.totalExpense, equals('75.00'));
          });
        },
      );

      feature.scenario('P&L report filters by currency without mixing', (s) {
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
          'alice has created income entries in both USD and IDR',
          () async {
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '2000.00',
                currency: 'USD',
                category: 'salary',
                description: 'USD income',
                date: '2025-06-01',
                type: 'income',
              ),
            );
            await svc.createExpense(
              const CreateExpenseRequest(
                amount: '5000000',
                currency: 'IDR',
                category: 'salary',
                description: 'IDR income',
                date: '2025-06-01',
                type: 'income',
              ),
            );
          },
        );

        s.when(
          'alice views the P&L report filtered to "USD" only',
          () async {},
        );

        s.then('the report should display only USD amounts', () async {
          final report = await svc.getPLReport(
            startDate: '2025-06-01',
            endDate: '2025-06-30',
            currency: 'USD',
          );
          expect(report.currency, equals('USD'));
          expect(report.totalIncome, equals('2000.00'));
        });

        s.and('no IDR amounts should be included', () async {
          final report = await svc.getPLReport(
            startDate: '2025-06-01',
            endDate: '2025-06-30',
            currency: 'USD',
          );
          // The USD report total must not reflect the IDR 5000000 income.
          expect(double.parse(report.totalIncome), isNot(equals(5000000.0)));
          expect(double.parse(report.totalIncome), equals(2000.0));
        });
      });

      feature.scenario(
        'P&L report for a period with no entries shows zero totals',
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

          s.when('alice navigates to the reporting page', () async {});

          s.and(
            'alice selects date range "2099-01-01" to "2099-01-31" with currency "USD"',
            () async {},
          );

          s.then('the report should display income total "0.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2099-01-01',
              endDate: '2099-01-31',
              currency: 'USD',
            );
            expect(report.totalIncome, equals('0.00'));
          });

          s.and('the report should display expense total "0.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2099-01-01',
              endDate: '2099-01-31',
              currency: 'USD',
            );
            expect(report.totalExpense, equals('0.00'));
          });

          s.and('the report should display net "0.00"', () async {
            final report = await svc.getPLReport(
              startDate: '2099-01-01',
              endDate: '2099-01-31',
              currency: 'USD',
            );
            expect(report.net, equals('0.00'));
          });
        },
      );
    },
  );
}
