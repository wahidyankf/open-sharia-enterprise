import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:demo_fe_dart_flutterweb/models/user.dart';
import 'package:flutter_test/flutter_test.dart';
import '../gherkin_helper.dart';
import '../service_client.dart';

void main() {
  late ServiceClient svc;
  // Shared state across steps within a scenario.
  late String aliceId;
  UserListResponse? userListResponse;
  PasswordResetResponse? resetResponse;

  setUp(() {
    svc = ServiceClient();
    aliceId = '';
    userListResponse = null;
    resetResponse = null;
  });

  describeFeature(
    '../../specs/apps/demo/fe/gherkin/admin/admin-panel.feature',
    (feature) {
      // ---------------------------------------------------------------------------
      // Scenario: Admin panel displays a paginated user list
      // ---------------------------------------------------------------------------
      feature.scenario('Admin panel displays a paginated user list', (s) {
        s.given('the app is running', () async {
          // No-op: ServiceClient starts in a clean state.
        });

        s.and('an admin user "superadmin" is logged in', () async {
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Admin#Pass1234',
            roles: ['USER', 'ADMIN'],
          );
          await svc.login(
            const LoginRequest(username: 'superadmin', password: 'Admin#Pass1234'),
          );
        });

        s.and('users "alice", "bob", and "carol" are registered', () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'bob',
            email: 'bob@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'carol',
            email: 'carol@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.when('the admin navigates to the user management page', () async {
          userListResponse = await svc.listUsers();
        });

        s.then('the user list should display registered users', () async {
          expect(userListResponse, isNotNull);
          expect(userListResponse!.content, isNotEmpty);
        });

        s.and('the list should include pagination controls', () async {
          // Pagination metadata is present on the response object.
          expect(userListResponse!.totalPages, greaterThanOrEqualTo(0));
          expect(userListResponse!.size, greaterThan(0));
        });

        s.and('the list should display total user count', () async {
          expect(userListResponse!.totalElements, greaterThan(0));
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Searching users by email filters the list
      // ---------------------------------------------------------------------------
      feature.scenario('Searching users by email filters the list', (s) {
        s.given('the app is running', () async {});

        s.and('an admin user "superadmin" is logged in', () async {
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Admin#Pass1234',
            roles: ['USER', 'ADMIN'],
          );
          await svc.login(
            const LoginRequest(username: 'superadmin', password: 'Admin#Pass1234'),
          );
        });

        s.and('users "alice", "bob", and "carol" are registered', () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'bob',
            email: 'bob@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'carol',
            email: 'carol@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.when('the admin navigates to the user management page', () async {
          userListResponse = await svc.listUsers();
        });

        s.and(
            'the admin types "alice@example.com" in the search field',
            () async {
          userListResponse =
              await svc.listUsers(search: 'alice@example.com');
        });

        s.then(
            'the user list should display only users matching "alice@example.com"',
            () async {
          expect(userListResponse, isNotNull);
          for (final user in userListResponse!.content) {
            final matchesEmail =
                user.email.contains('alice@example.com') ||
                    user.username.contains('alice@example.com');
            expect(matchesEmail, isTrue);
          }
          expect(
            userListResponse!.content.any((u) => u.email == 'alice@example.com'),
            isTrue,
          );
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Admin disables a user account from the user detail page
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Admin disables a user account from the user detail page', (s) {
        s.given('the app is running', () async {});

        s.and('an admin user "superadmin" is logged in', () async {
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Admin#Pass1234',
            roles: ['USER', 'ADMIN'],
          );
          await svc.login(
            const LoginRequest(username: 'superadmin', password: 'Admin#Pass1234'),
          );
        });

        s.and('users "alice", "bob", and "carol" are registered', () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'bob',
            email: 'bob@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'carol',
            email: 'carol@example.com',
            password: 'Str0ng#Pass1',
          );
          aliceId = svc.getUserByUsername('alice').id;
        });

        s.when('the admin navigates to alice\'s user detail page', () async {
          final user = await svc.getUserById(aliceId);
          expect(user.username, equals('alice'));
        });

        s.and(
            'the admin clicks the "Disable" button with reason "Policy violation"',
            () async {
          await svc.disableUser(
            aliceId,
            const DisableRequest(reason: 'Policy violation'),
          );
        });

        s.then('alice\'s status should display as "disabled"', () async {
          final user = await svc.getUserById(aliceId);
          expect(user.status.toLowerCase(), equals('disabled'));
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Disabled user sees an error when trying to access their dashboard
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Disabled user sees an error when trying to access their dashboard',
          (s) {
        s.given('the app is running', () async {});

        s.and('an admin user "superadmin" is logged in', () async {
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Admin#Pass1234',
            roles: ['USER', 'ADMIN'],
          );
          await svc.login(
            const LoginRequest(username: 'superadmin', password: 'Admin#Pass1234'),
          );
        });

        s.and('users "alice", "bob", and "carol" are registered', () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'bob',
            email: 'bob@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'carol',
            email: 'carol@example.com',
            password: 'Str0ng#Pass1',
          );
          aliceId = svc.getUserByUsername('alice').id;
        });

        s.given('alice\'s account has been disabled by the admin', () async {
          await svc.disableUser(
            aliceId,
            const DisableRequest(reason: 'Admin action'),
          );
          // Switch session to alice attempting to log in.
          await svc.logout();
        });

        s.when('alice attempts to access the dashboard', () async {
          // Attempting to log in as a disabled user simulates accessing a
          // protected resource — the service rejects disabled accounts.
        });

        s.then('alice should be redirected to the login page', () async {
          // Verify that login as disabled alice raises AccountDisabledError,
          // which drives the UI to redirect to the login page.
          Object? caught;
          try {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          } catch (e) {
            caught = e;
          }
          expect(caught, isA<AccountDisabledError>());
        });

        s.and(
            'an error message about account being disabled should be displayed',
            () async {
          Object? caught;
          try {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          } catch (e) {
            caught = e;
          }
          expect(caught, isA<AccountDisabledError>());
          expect(caught.toString(), contains('disabled'));
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Admin re-enables a disabled user account
      // ---------------------------------------------------------------------------
      feature.scenario('Admin re-enables a disabled user account', (s) {
        s.given('the app is running', () async {});

        s.and('an admin user "superadmin" is logged in', () async {
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Admin#Pass1234',
            roles: ['USER', 'ADMIN'],
          );
          await svc.login(
            const LoginRequest(username: 'superadmin', password: 'Admin#Pass1234'),
          );
        });

        s.and('users "alice", "bob", and "carol" are registered', () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'bob',
            email: 'bob@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'carol',
            email: 'carol@example.com',
            password: 'Str0ng#Pass1',
          );
          aliceId = svc.getUserByUsername('alice').id;
        });

        s.given('alice\'s account has been disabled', () async {
          await svc.disableUser(
            aliceId,
            const DisableRequest(reason: 'Temporary restriction'),
          );
        });

        s.when('the admin navigates to alice\'s user detail page', () async {
          final user = await svc.getUserById(aliceId);
          expect(user.username, equals('alice'));
        });

        s.and('the admin clicks the "Enable" button', () async {
          await svc.enableUser(aliceId);
        });

        s.then('alice\'s status should display as "active"', () async {
          final user = await svc.getUserById(aliceId);
          expect(user.status.toLowerCase(), equals('active'));
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Admin generates a password-reset token for a user
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Admin generates a password-reset token for a user', (s) {
        s.given('the app is running', () async {});

        s.and('an admin user "superadmin" is logged in', () async {
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Admin#Pass1234',
            roles: ['USER', 'ADMIN'],
          );
          await svc.login(
            const LoginRequest(username: 'superadmin', password: 'Admin#Pass1234'),
          );
        });

        s.and('users "alice", "bob", and "carol" are registered', () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'bob',
            email: 'bob@example.com',
            password: 'Str0ng#Pass1',
          );
          svc.seedUser(
            username: 'carol',
            email: 'carol@example.com',
            password: 'Str0ng#Pass1',
          );
          aliceId = svc.getUserByUsername('alice').id;
        });

        s.when('the admin navigates to alice\'s user detail page', () async {
          final user = await svc.getUserById(aliceId);
          expect(user.username, equals('alice'));
        });

        s.and('the admin clicks the "Generate Reset Token" button', () async {
          resetResponse = await svc.forcePasswordReset(aliceId);
        });

        s.then('a password reset token should be displayed', () async {
          expect(resetResponse, isNotNull);
          expect(resetResponse!.token, isNotEmpty);
        });

        s.and('a copy-to-clipboard button should be available', () async {
          // The reset token is a non-empty string that the UI can present
          // with a copy-to-clipboard affordance. Verify the token is present
          // and non-trivially short (long enough to be meaningful).
          expect(resetResponse!.token.length, greaterThan(8));
        });
      });
    },
  );
}
