import 'package:flutter_test/flutter_test.dart';
import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import '../gherkin_helper.dart';
import '../service_client.dart';

void main() {
  late ServiceClient svc;

  setUp(() {
    svc = ServiceClient();
  });

  describeFeature(
    '../../specs/apps/demo/fe/gherkin/authentication/login.feature',
    (feature) {
      // -----------------------------------------------------------------------
      // Scenario: Successful login navigates to the dashboard
      // -----------------------------------------------------------------------
      feature.scenario('Successful login navigates to the dashboard', (s) {
        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.when(
          'alice submits the login form with username "alice" and password "Str0ng#Pass1"',
          () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          },
        );

        s.then('alice should be on the dashboard page', () async {
          expect(svc.isAuthenticated, isTrue);
        });

        s.and("the navigation should display alice's username", () async {
          final user = await svc.getCurrentUser();
          expect(user.username, equals('alice'));
        });
      });

      // -----------------------------------------------------------------------
      // Scenario: Successful login stores session tokens
      // -----------------------------------------------------------------------
      feature.scenario('Successful login stores session tokens', (s) {
        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.when(
          'alice submits the login form with username "alice" and password "Str0ng#Pass1"',
          () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          },
        );

        s.then('an authentication session should be active', () async {
          expect(svc.isAuthenticated, isTrue);
          expect(svc.currentAccessToken, isNotNull);
        });

        s.and('a refresh token should be stored', () async {
          expect(svc.currentRefreshToken, isNotNull);
        });
      });

      // -----------------------------------------------------------------------
      // Scenario: Login with wrong password shows an error
      // -----------------------------------------------------------------------
      feature.scenario('Login with wrong password shows an error', (s) {
        ServiceError? caughtError;

        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.when(
          'alice submits the login form with username "alice" and password "Wr0ngPass!"',
          () async {
            try {
              await svc.login(
                const LoginRequest(username: 'alice', password: 'Wr0ngPass!'),
              );
            } on ServiceError catch (e) {
              caughtError = e;
            }
          },
        );

        s.then(
          'an error message about invalid credentials should be displayed',
          () async {
            expect(caughtError, isA<UnauthorizedError>());
          },
        );

        s.and('alice should remain on the login page', () async {
          expect(svc.isAuthenticated, isFalse);
        });
      });

      // -----------------------------------------------------------------------
      // Scenario: Login for non-existent user shows an error
      // -----------------------------------------------------------------------
      feature.scenario('Login for non-existent user shows an error', (s) {
        ServiceError? caughtError;

        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.when(
          'alice submits the login form with username "ghost" and password "Str0ng#Pass1"',
          () async {
            try {
              await svc.login(
                const LoginRequest(username: 'ghost', password: 'Str0ng#Pass1'),
              );
            } on ServiceError catch (e) {
              caughtError = e;
            }
          },
        );

        s.then(
          'an error message about invalid credentials should be displayed',
          () async {
            expect(caughtError, isA<UnauthorizedError>());
          },
        );

        s.and('alice should remain on the login page', () async {
          expect(svc.isAuthenticated, isFalse);
        });
      });

      // -----------------------------------------------------------------------
      // Scenario: Login for deactivated account shows an error
      // -----------------------------------------------------------------------
      feature.scenario('Login for deactivated account shows an error', (s) {
        ServiceError? caughtError;

        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.given('a user "alice" is registered and deactivated', () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
            status: 'INACTIVE',
          );
        });

        s.when(
          'alice submits the login form with username "alice" and password "Str0ng#Pass1"',
          () async {
            try {
              await svc.login(
                const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
              );
            } on ServiceError catch (e) {
              caughtError = e;
            }
          },
        );

        s.then(
          'an error message about account deactivation should be displayed',
          () async {
            expect(caughtError, isA<AccountInactiveError>());
          },
        );

        s.and('alice should remain on the login page', () async {
          expect(svc.isAuthenticated, isFalse);
        });
      });
    },
  );
}
