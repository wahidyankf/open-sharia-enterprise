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
    '../../specs/apps/demo/fe/gherkin/security/security.feature',
    (feature) {
      // -----------------------------------------------------------------------
      // Scenario: Registration form rejects password shorter than 12 characters
      // -----------------------------------------------------------------------
      feature.scenario(
        'Registration form rejects password shorter than 12 characters',
        (s) {
          ServiceError? caughtError;

          s.given('the app is running', () async {});

          s.when(
            'a visitor fills in the registration form with username "alice", email "alice@example.com", and password "Short1!Ab"',
            () async {
              // Form pre-fill — state held in the subsequent submit step.
            },
          );

          s.and('the visitor submits the registration form', () async {
            try {
              await svc.register(
                const RegisterRequest(
                  username: 'alice',
                  email: 'alice@example.com',
                  password: 'Short1!Ab',
                ),
              );
            } on ServiceError catch (e) {
              caughtError = e;
            }
          });

          s.then(
            'a validation error for the password field should be displayed',
            () async {
              expect(caughtError, isA<ValidationError>());
            },
          );

          s.and('the error should mention minimum length requirements', () async {
            expect(
              caughtError!.message.toLowerCase(),
              anyOf(contains('12'), contains('length'), contains('characters')),
            );
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Registration form rejects password with no special character
      // -----------------------------------------------------------------------
      feature.scenario(
        'Registration form rejects password with no special character',
        (s) {
          ServiceError? caughtError;

          s.given('the app is running', () async {});

          s.when(
            'a visitor fills in the registration form with username "alice", email "alice@example.com", and password "AllUpperCase1234"',
            () async {
              // Form pre-fill — state held in the subsequent submit step.
            },
          );

          s.and('the visitor submits the registration form', () async {
            try {
              await svc.register(
                const RegisterRequest(
                  username: 'alice',
                  email: 'alice@example.com',
                  password: 'AllUpperCase1234',
                ),
              );
            } on ServiceError catch (e) {
              caughtError = e;
            }
          });

          s.then(
            'a validation error for the password field should be displayed',
            () async {
              expect(caughtError, isA<ValidationError>());
            },
          );

          s.and(
            'the error should mention special character requirements',
            () async {
              expect(
                caughtError!.message.toLowerCase(),
                anyOf(
                  contains('special'),
                  contains('character'),
                  contains('uppercase'),
                ),
              );
            },
          );
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Account is locked after exceeding maximum failed login attempts
      // -----------------------------------------------------------------------
      feature.scenario(
        'Account is locked after exceeding maximum failed login attempts',
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

          s.and(
            'alice has entered the wrong password the maximum number of times',
            () async {
              // 5 consecutive wrong-password attempts trigger a lockout.
              for (var i = 0; i < 5; i++) {
                try {
                  await svc.login(
                    const LoginRequest(
                      username: 'alice',
                      password: 'WrongPassword!99',
                    ),
                  );
                } on ServiceError catch (_) {
                  // Expected — keep looping.
                }
              }
            },
          );

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
            'an error message about account lockout should be displayed',
            () async {
              expect(caughtError, isA<AccountLockedError>());
            },
          );

          s.and('alice should remain on the login page', () async {
            expect(svc.isAuthenticated, isFalse);
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Admin unlocks a locked account via the admin panel
      // -----------------------------------------------------------------------
      feature.scenario(
        'Admin unlocks a locked account via the admin panel',
        (s) {
          late String aliceId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered and locked after too many failed logins',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
                status: 'LOCKED',
              );
              aliceId = svc.getUserByUsername('alice').id;
            },
          );

          s.and('an admin user "superadmin" is logged in', () async {
            svc.seedUser(
              username: 'superadmin',
              email: 'superadmin@example.com',
              password: 'Adm1n#Secure!',
              roles: ['USER', 'ADMIN'],
            );
            await svc.login(
              const LoginRequest(
                username: 'superadmin',
                password: 'Adm1n#Secure!',
              ),
            );
          });

          s.when(
            "the admin navigates to alice's user detail in the admin panel",
            () async {
              // Navigation is represented by the subsequent admin action.
            },
          );

          s.and('the admin clicks the "Unlock" button', () async {
            await svc.unlockUser(aliceId);
          });

          s.then('alice\'s status should display as "active"', () async {
            final alice = await svc.getUserById(aliceId);
            expect(alice.status.toLowerCase(), equals('active'));
          });
        },
      );

      // -----------------------------------------------------------------------
      // Scenario: Unlocked account can log in with correct password
      // -----------------------------------------------------------------------
      feature.scenario(
        'Unlocked account can log in with correct password',
        (s) {
          s.given('the app is running', () async {});

          s.and(
            'a user "alice" was locked and has been unlocked by an admin',
            () async {
              // Set up alice in LOCKED state, then unlock via admin.
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
                status: 'LOCKED',
              );
              final aliceId = svc.getUserByUsername('alice').id;

              svc.seedUser(
                username: 'superadmin',
                email: 'superadmin@example.com',
                password: 'Adm1n#Secure!',
                roles: ['USER', 'ADMIN'],
              );
              await svc.login(
                const LoginRequest(
                  username: 'superadmin',
                  password: 'Adm1n#Secure!',
                ),
              );
              await svc.unlockUser(aliceId);
              await svc.logout();
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
        },
      );
    },
  );
}
