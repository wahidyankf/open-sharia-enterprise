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
    '../../specs/apps/demo/fe/gherkin/authentication/session.feature',
    (feature) {
      // -----------------------------------------------------------------------
      // Scenario: Session refreshes automatically before the access token expires
      // -----------------------------------------------------------------------
      feature.scenario(
        'Session refreshes automatically before the access token expires',
        (s) {
          late String originalAccessToken;
          late String originalRefreshToken;

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

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
            originalAccessToken = svc.currentAccessToken!;
            originalRefreshToken = svc.currentRefreshToken!;
          });

          s.given("alice's access token is about to expire", () async {
            // Simulated by the scenario: we record the current tokens and
            // proceed to trigger a refresh on the next step.
          });

          s.when('the app performs a background token refresh', () async {
            await svc.refreshToken(originalRefreshToken);
          });

          s.then('a new access token should be stored', () async {
            expect(svc.currentAccessToken, isNotNull);
            expect(svc.currentAccessToken, isNot(equals(originalAccessToken)));
          });

          s.and('a new refresh token should be stored', () async {
            expect(svc.currentRefreshToken, isNotNull);
            expect(
              svc.currentRefreshToken,
              isNot(equals(originalRefreshToken)),
            );
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Expired refresh token redirects to login
      // -----------------------------------------------------------------------
      feature.scenario('Expired refresh token redirects to login', (s) {
        ServiceError? caughtError;
        const expiredToken = 'refresh.nonexistent.99999';

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

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given("alice's refresh token has expired", () async {
          // We use a token that was never issued so the service rejects it.
        });

        s.when('the app attempts a background token refresh', () async {
          try {
            await svc.refreshToken(expiredToken);
          } on ServiceError catch (e) {
            caughtError = e;
          }
        });

        s.then('alice should be redirected to the login page', () async {
          expect(svc.isAuthenticated, isFalse);
        });

        s.and(
          'an error message about session expiration should be displayed',
          () async {
            expect(caughtError, isA<TokenExpiredError>());
          },
        );
      });

      // -----------------------------------------------------------------------
      // Scenario: Original refresh token is rejected after rotation
      // -----------------------------------------------------------------------
      feature.scenario(
        'Original refresh token is rejected after rotation',
        (s) {
          late String originalRefreshToken;
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

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
            originalRefreshToken = svc.currentRefreshToken!;
          });

          s.given(
            'alice has refreshed her session and received a new token pair',
            () async {
              await svc.refreshToken(originalRefreshToken);
            },
          );

          s.when(
            'the app attempts to refresh using the original refresh token',
            () async {
              try {
                await svc.refreshToken(originalRefreshToken);
              } on ServiceError catch (e) {
                caughtError = e;
              }
            },
          );

          s.then('alice should be redirected to the login page', () async {
            expect(caughtError, isA<TokenExpiredError>());
            expect(svc.isAuthenticated, isFalse);
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Deactivated user is redirected to login on next action
      // -----------------------------------------------------------------------
      feature.scenario(
        'Deactivated user is redirected to login on next action',
        (s) {
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

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.given("alice's account has been deactivated", () async {
            await svc.deactivateAccount();
          });

          s.when('alice navigates to a protected page', () async {
            // After deactivation + logout, alice tries to log in again
            // which simulates navigating to a protected page (backend rejects)
            try {
              await svc.login(
                const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
              );
            } on ServiceError catch (e) {
              caughtError = e;
            }
          });

          s.then('alice should be redirected to the login page', () async {
            expect(svc.isAuthenticated, isFalse);
          });

          s.and(
            'an error message about account deactivation should be displayed',
            () async {
              expect(caughtError, isA<AccountInactiveError>());
            },
          );
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Clicking logout ends the current session
      // -----------------------------------------------------------------------
      feature.scenario('Clicking logout ends the current session', (s) {
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

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.when('alice clicks the "Logout" button', () async {
          await svc.logout();
        });

        s.then('alice should be redirected to the login page', () async {
          expect(svc.isAuthenticated, isFalse);
        });

        s.and('the authentication session should be cleared', () async {
          expect(svc.currentAccessToken, isNull);
          expect(svc.currentRefreshToken, isNull);
        });
      });

      // -----------------------------------------------------------------------
      // Scenario: Clicking "Log out all devices" ends all sessions
      // -----------------------------------------------------------------------
      feature.scenario(
        'Clicking "Log out all devices" ends all sessions',
        (s) {
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

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.when('alice clicks the "Log out all devices" option', () async {
            await svc.logoutAll();
          });

          s.then('alice should be redirected to the login page', () async {
            expect(svc.isAuthenticated, isFalse);
          });

          s.and('the authentication session should be cleared', () async {
            expect(svc.currentAccessToken, isNull);
            expect(svc.currentRefreshToken, isNull);
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Clicking logout twice does not cause an error
      // -----------------------------------------------------------------------
      feature.scenario('Clicking logout twice does not cause an error', (s) {
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

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('alice has already clicked logout', () async {
          await svc.logout();
        });

        s.when('alice navigates to the login page', () async {
          // No-op: navigation to login page does not interact with the service.
        });

        s.then('no error should be displayed', () async {
          // A second logout should complete without throwing.
          await expectLater(svc.logout(), completes);
        });
      });
    },
  );
}
