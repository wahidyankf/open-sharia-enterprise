import 'package:flutter_test/flutter_test.dart';
import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:demo_fe_dart_flutterweb/models/token.dart';
import 'package:demo_fe_dart_flutterweb/models/user.dart';
import '../gherkin_helper.dart';
import '../service_client.dart';

void main() {
  late ServiceClient svc;

  setUp(() {
    svc = ServiceClient();
  });

  describeFeature(
    '../../specs/apps/demo/fe/gherkin/token-management/tokens.feature',
    (feature) {
      // -----------------------------------------------------------------------
      // Scenario: Session info displays the authenticated user's identity
      // -----------------------------------------------------------------------
      feature.scenario(
        "Session info displays the authenticated user's identity",
        (s) {
          late String aliceId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
              aliceId = svc.getUserByUsername('alice').id;
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.when('alice opens the session info panel', () async {
            // Represented by the token decoding in the assertion step.
          });

          s.then("the panel should display alice's user ID", () async {
            final claims = await svc.decodeTokenClaims(
              svc.currentAccessToken!,
            );
            expect(claims.sub, equals(aliceId));
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Session info shows the token issuer
      // -----------------------------------------------------------------------
      feature.scenario('Session info shows the token issuer', (s) {
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

        s.when('alice opens the session info panel', () async {
          // Represented by the token decoding in the assertion step.
        });

        s.then('the panel should display a non-empty issuer value', () async {
          final claims = await svc.decodeTokenClaims(svc.currentAccessToken!);
          expect(claims.iss, isNotEmpty);
        });
      });

      // -----------------------------------------------------------------------
      // Scenario: JWKS endpoint is accessible for token verification
      // -----------------------------------------------------------------------
      feature.scenario(
        'JWKS endpoint is accessible for token verification',
        (s) {
          late JwksResponse jwks;

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

          s.given('the app is running', () async {
            // Re-stated in the feature file; no additional setup needed.
          });

          s.when('the app fetches the JWKS endpoint', () async {
            jwks = await svc.getJwks();
          });

          s.then('at least one public key should be available', () async {
            expect(jwks.keys, isNotEmpty);
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Logging out marks the session as ended
      // -----------------------------------------------------------------------
      feature.scenario('Logging out marks the session as ended', (s) {
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

        s.when('alice clicks the "Logout" button', () async {
          await svc.logout();
        });

        s.then('the authentication session should be cleared', () async {
          expect(svc.isAuthenticated, isFalse);
          expect(svc.currentAccessToken, isNull);
        });

        s.and(
          'navigating to a protected page should redirect to login',
          () async {
            try {
              await svc.getCurrentUser();
            } on ServiceError catch (e) {
              caughtError = e;
            }
            expect(caughtError, isA<UnauthorizedError>());
          },
        );
      });

      // -----------------------------------------------------------------------
      // Scenario: Blacklisted token is rejected on protected page navigation
      // -----------------------------------------------------------------------
      feature.scenario(
        'Blacklisted token is rejected on protected page navigation',
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

          s.given('alice has logged out', () async {
            await svc.logout();
          });

          s.when('alice attempts to access the dashboard directly', () async {
            try {
              await svc.getCurrentUser();
            } on ServiceError catch (e) {
              caughtError = e;
            }
          });

          s.then('alice should be redirected to the login page', () async {
            expect(caughtError, isA<UnauthorizedError>());
            expect(svc.isAuthenticated, isFalse);
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Disabled user is immediately logged out
      // -----------------------------------------------------------------------
      feature.scenario('Disabled user is immediately logged out', (s) {
        // The ServiceClient holds a single session at a time. The admin
        // disabling alice's account is modelled by: alice logs in, an admin
        // logs in (replacing alice's session), the admin disables alice via
        // disableUser(), the admin logs out, and then alice's next login
        // attempt fails with AccountDisabledError — demonstrating that the
        // disabled guard is enforced on the next authentication attempt.
        ServiceError? caughtError;
        late String aliceId;

        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
            aliceId = svc.getUserByUsername('alice').id;
          },
        );

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given("an admin has disabled alice's account", () async {
          // Log in as admin (replaces alice's session in the single-session
          // client), disable alice, then log out the admin.
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Adm1n#Secure!',
            roles: ['USER', 'ADMIN'],
          );
          await svc.login(
            const LoginRequest(username: 'superadmin', password: 'Adm1n#Secure!'),
          );
          await svc.disableUser(aliceId, const DisableRequest(reason: 'test'));
          await svc.logout();
        });

        s.when('alice navigates to a protected page', () async {
          // Alice's session was cleared when the admin logged in. Her next
          // navigation triggers a login attempt which now fails due to the
          // DISABLED status set by the admin.
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
          'an error message about account being disabled should be displayed',
          () async {
            expect(caughtError, isA<AccountDisabledError>());
          },
        );
      });
    },
  );
}
