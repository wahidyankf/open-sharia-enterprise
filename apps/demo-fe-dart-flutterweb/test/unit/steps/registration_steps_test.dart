import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:flutter_test/flutter_test.dart';
import '../gherkin_helper.dart';
import '../service_client.dart';

void main() {
  late ServiceClient svc;
  // Captured error from last service call.
  Object? lastError;
  // Whether the last registration succeeded.
  bool registrationSucceeded = false;

  setUp(() {
    svc = ServiceClient();
    lastError = null;
    registrationSucceeded = false;
  });

  describeFeature(
    '../../specs/apps/demo/fe/gherkin/user-lifecycle/registration.feature',
    (feature) {
      // ---------------------------------------------------------------------------
      // Scenario: Successful registration navigates to the login page with success message
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Successful registration navigates to the login page with success message',
          (s) {
        s.given('the app is running', () async {
          // No-op: ServiceClient starts in a clean state.
        });

        s.when(
            'a visitor fills in the registration form with username "alice", email "alice@example.com", and password "Str0ng#Pass1"',
            () async {
          // Form data captured; submission happens in the next step.
        });

        s.and('the visitor submits the registration form', () async {
          try {
            await svc.register(
              const RegisterRequest(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              ),
            );
            registrationSucceeded = true;
          } catch (e) {
            lastError = e;
            registrationSucceeded = false;
          }
        });

        s.then('the visitor should be on the login page', () async {
          // Successful registration completes without error, which drives the UI
          // to navigate to the login page.
          expect(registrationSucceeded, isTrue,
              reason: 'Registration should succeed with valid credentials');
          expect(lastError, isNull);
        });

        s.and(
            'a success message about account creation should be displayed',
            () async {
          // The service completed without error — the UI can show a success banner.
          expect(registrationSucceeded, isTrue);
          final user = svc.getUserByUsername('alice');
          expect(user.username, equals('alice'));
          expect(user.status, equals('ACTIVE'));
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Successful registration does not display the password in any confirmation
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Successful registration does not display the password in any confirmation',
          (s) {
        s.given('the app is running', () async {});

        s.when(
            'a visitor fills in the registration form with username "alice", email "alice@example.com", and password "Str0ng#Pass1"',
            () async {
          // Form data captured; submission happens in the next step.
        });

        s.and('the visitor submits the registration form', () async {
          await svc.register(
            const RegisterRequest(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            ),
          );
          registrationSucceeded = true;
        });

        s.then('no password value should be visible on the page', () async {
          // The User model returned by the service does not expose a password
          // field — asserting the model structure enforces this at the data layer.
          expect(registrationSucceeded, isTrue);
          final user = svc.getUserByUsername('alice');
          // User model has no password field — verify all exposed fields.
          expect(user.username, equals('alice'));
          expect(user.email, equals('alice@example.com'));
          expect(user.displayName, isNotNull);
          expect(user.status, isNotNull);
          // No password property exists on User — this is enforced by the type system.
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Registration with duplicate username shows an error
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Registration with duplicate username shows an error', (s) {
        s.given('the app is running', () async {});

        s.given(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.when(
            'a visitor fills in the registration form with username "alice", email "new@example.com", and password "Str0ng#Pass1"',
            () async {
          // Form data captured; submission happens in the next step.
        });

        s.and('the visitor submits the registration form', () async {
          try {
            await svc.register(
              const RegisterRequest(
                username: 'alice',
                email: 'new@example.com',
                password: 'Str0ng#Pass1',
              ),
            );
            registrationSucceeded = true;
          } catch (e) {
            lastError = e;
            registrationSucceeded = false;
          }
        });

        s.then(
            'an error message about duplicate username should be displayed',
            () async {
          expect(lastError, isA<ConflictError>(),
              reason: 'Duplicate username must raise ConflictError');
        });

        s.and('the visitor should remain on the registration page', () async {
          // Registration failed — the visitor stays on the registration page.
          expect(registrationSucceeded, isFalse);
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Registration with invalid email shows a validation error
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Registration with invalid email shows a validation error', (s) {
        s.given('the app is running', () async {});

        s.when(
            'a visitor fills in the registration form with username "alice", email "not-an-email", and password "Str0ng#Pass1"',
            () async {
          // Form data captured; submission happens in the next step.
        });

        s.and('the visitor submits the registration form', () async {
          try {
            await svc.register(
              const RegisterRequest(
                username: 'alice',
                email: 'not-an-email',
                password: 'Str0ng#Pass1',
              ),
            );
            registrationSucceeded = true;
          } catch (e) {
            lastError = e;
            registrationSucceeded = false;
          }
        });

        s.then(
            'a validation error for the email field should be displayed',
            () async {
          expect(lastError, isA<ValidationError>(),
              reason: 'Invalid email must raise ValidationError');
        });

        s.and('the visitor should remain on the registration page', () async {
          expect(registrationSucceeded, isFalse);
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Registration with empty password shows a validation error
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Registration with empty password shows a validation error', (s) {
        s.given('the app is running', () async {});

        s.when(
            'a visitor fills in the registration form with username "alice", email "alice@example.com", and password ""',
            () async {
          // Form data captured; submission happens in the next step.
        });

        s.and('the visitor submits the registration form', () async {
          try {
            await svc.register(
              const RegisterRequest(
                username: 'alice',
                email: 'alice@example.com',
                password: '',
              ),
            );
            registrationSucceeded = true;
          } catch (e) {
            lastError = e;
            registrationSucceeded = false;
          }
        });

        s.then(
            'a validation error for the password field should be displayed',
            () async {
          expect(lastError, isA<ValidationError>(),
              reason: 'Empty password must raise ValidationError');
        });

        s.and('the visitor should remain on the registration page', () async {
          expect(registrationSucceeded, isFalse);
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Registration with weak password shows a validation error
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Registration with weak password shows a validation error', (s) {
        s.given('the app is running', () async {});

        s.when(
            'a visitor fills in the registration form with username "alice", email "alice@example.com", and password "str0ng#pass1"',
            () async {
          // Form data captured; submission happens in the next step.
          // "str0ng#pass1" is missing an uppercase letter — it fails complexity.
        });

        s.and('the visitor submits the registration form', () async {
          try {
            await svc.register(
              const RegisterRequest(
                username: 'alice',
                email: 'alice@example.com',
                password: 'str0ng#pass1',
              ),
            );
            registrationSucceeded = true;
          } catch (e) {
            lastError = e;
            registrationSucceeded = false;
          }
        });

        s.then(
            'a validation error for the password field should be displayed',
            () async {
          expect(lastError, isA<ValidationError>(),
              reason:
                  'Weak password (missing uppercase) must raise ValidationError');
        });

        s.and('the visitor should remain on the registration page', () async {
          expect(registrationSucceeded, isFalse);
        });
      });
    },
  );
}
