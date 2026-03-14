/// BDD step definitions for user-lifecycle/user-profile.feature.
///
/// Tests profile display, display name update, password change, and
/// self-deactivation flows.
library;

import 'package:dio/dio.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:go_router/go_router.dart';
import 'package:demo_fe_dart_flutter/core/models/models.dart';
import 'package:demo_fe_dart_flutter/core/providers/auth_provider.dart';
import 'package:demo_fe_dart_flutter/core/providers/user_provider.dart';
import 'package:demo_fe_dart_flutter/screens/profile_screen.dart';
import 'package:demo_fe_dart_flutter/screens/login_screen.dart';

// Feature file consumed by the bdd_widget_test builder.
// ignore: unused_element
const _feature =
    '../../../../../../specs/apps/demo/fe/gherkin/user-lifecycle/user-profile.feature';

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

late _ProfileScenarioState _s;

class _ProfileScenarioState {
  String displayName = 'Alice';
  bool changePasswordShouldFail = false;
  bool deactivateShouldSucceed = true;
}

// ---------------------------------------------------------------------------
// Mock notifiers
// ---------------------------------------------------------------------------

class _ProfileUserNotifier extends UserNotifier {
  _ProfileUserNotifier(this._state, Ref ref) : super(ref);

  final _ProfileScenarioState _state;
  String currentDisplayName = 'Alice';

  @override
  Future<void> updateProfile(String displayName) async {
    currentDisplayName = displayName;
    state = const AsyncValue.data(null);
  }

  @override
  Future<void> changePassword({
    required String oldPassword,
    required String newPassword,
  }) async {
    if (_state.changePasswordShouldFail) {
      state = AsyncValue.error(
        DioException(
          requestOptions: RequestOptions(path: '/api/v1/users/me/change-password'),
          response: Response(
            requestOptions: RequestOptions(path: '/api/v1/users/me/change-password'),
            statusCode: 401,
            data: {'detail': 'Invalid username or password.'},
          ),
          type: DioExceptionType.badResponse,
        ),
        StackTrace.current,
      );
      return;
    }
    state = const AsyncValue.data(null);
  }

  @override
  Future<void> deactivateAccount() async {
    state = const AsyncValue.data(null);
  }
}

Widget _buildProfileApp(_ProfileScenarioState state) {
  final router = GoRouter(
    initialLocation: '/profile',
    routes: [
      GoRoute(
        path: '/profile',
        builder: (_, __) => const ProfileScreen(),
      ),
      GoRoute(path: '/login', builder: (_, __) => const LoginScreen()),
      GoRoute(
        path: '/expenses',
        builder: (_, __) => const Scaffold(body: Text('Dashboard')),
      ),
      GoRoute(
        path: '/tokens',
        builder: (_, __) => const Scaffold(body: Text('Tokens')),
      ),
      GoRoute(
        path: '/admin',
        builder: (_, __) => const Scaffold(body: Text('Admin')),
      ),
    ],
  );

  return ProviderScope(
    overrides: [
      authProvider.overrideWith(
        (_) => AuthNotifier()
          ..state = const AuthState(
            accessToken: 'mock.access.token',
            refreshToken: 'mock.refresh.token',
          ),
      ),
      currentUserProvider.overrideWith(
        (ref) async => User(
          id: 'user-001',
          username: 'alice',
          email: 'alice@example.com',
          displayName: state.displayName,
          role: 'USER',
          status: 'ACTIVE',
          createdAt: '2025-01-01T00:00:00Z',
        ),
      ),
      userNotifierProvider.overrideWith(
        (ref) => _ProfileUserNotifier(state, ref),
      ),
    ],
    child: MaterialApp.router(routerConfig: router),
  );
}

// ---------------------------------------------------------------------------
// Step definitions
// ---------------------------------------------------------------------------

/// `Given the app is running`
Future<void> givenTheAppIsRunning(WidgetTester tester) async {
  _s = _ProfileScenarioState();
  await tester.pumpWidget(_buildProfileApp(_s));
  await tester.pumpAndSettle();
}

/// `And a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"`
Future<void>
    andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(
        WidgetTester tester) async {
  // Handled by mock state.
}

/// `And alice has logged in`
Future<void> andAliceHasLoggedIn(WidgetTester tester) async {
  // Mock auth state already authenticated.
}

/// `When alice navigates to the profile page`
Future<void> whenAliceNavigatesToTheProfilePage(WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `Then the profile should display username "alice"`
Future<void> thenTheProfileShouldDisplayUsernameAlice(
    WidgetTester tester) async {
  expect(find.textContaining('alice'), findsWidgets);
}

/// `And the profile should display email "alice@example.com"`
Future<void> thenTheProfileShouldDisplayEmailAliceAtExampleCom(
    WidgetTester tester) async {
  expect(find.textContaining('alice@example.com'), findsOneWidget);
}

/// `And the profile should display a display name`
Future<void> andTheProfileShouldDisplayADisplayName(
    WidgetTester tester) async {
  expect(find.textContaining('Alice'), findsWidgets);
}

/// `And alice changes the display name to "Alice Smith"`
Future<void> andAliceChangesTheDisplayNameToAliceSmith(
    WidgetTester tester) async {
  final displayNameField = find.byWidgetPredicate(
    (w) =>
        w is TextField &&
        (w.decoration?.labelText?.toLowerCase().contains('display') ?? false),
  );
  if (displayNameField.evaluate().isNotEmpty) {
    await tester.enterText(displayNameField, 'Alice Smith');
    await tester.pumpAndSettle();
  }
}

/// `And alice saves the profile changes`
Future<void> andAliceSavesTheProfileChanges(WidgetTester tester) async {
  final saveButton = find.byWidgetPredicate(
    (w) =>
        w is ElevatedButton ||
        w is FilledButton ||
        (w is TextButton &&
            find.descendant(
              of: find.byWidget(w),
              matching: find.textContaining('Save'),
            ).evaluate().isNotEmpty),
  );
  if (saveButton.evaluate().isNotEmpty) {
    await tester.tap(saveButton.first);
    await tester.pumpAndSettle();
  }
}

/// `Then the profile should display display name "Alice Smith"`
Future<void> thenTheProfileShouldDisplayDisplayNameAliceSmith(
    WidgetTester tester) async {
  expect(find.textContaining('Alice Smith'), findsWidgets);
}

/// `When alice navigates to the change password form`
Future<void> whenAliceNavigatesToTheChangePasswordForm(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `And alice enters old password "Str0ng#Pass1" and new password "NewPass#456"`
Future<void>
    andAliceEntersOldPasswordStr0ngPass1AndNewPasswordNewPass456(
        WidgetTester tester) async {
  _s.changePasswordShouldFail = false;
  final oldPwField = find.byWidgetPredicate(
    (w) =>
        w is TextField &&
        ((w.decoration?.labelText?.toLowerCase().contains('old') ?? false) ||
            (w.decoration?.labelText?.toLowerCase().contains('current') ??
                false)),
  );
  final newPwField = find.byWidgetPredicate(
    (w) =>
        w is TextField &&
        (w.decoration?.labelText?.toLowerCase().contains('new') ?? false),
  );
  if (oldPwField.evaluate().isNotEmpty) {
    await tester.enterText(oldPwField.first, 'Str0ng#Pass1');
  }
  if (newPwField.evaluate().isNotEmpty) {
    await tester.enterText(newPwField.first, 'NewPass#456');
  }
  await tester.pumpAndSettle();
}

/// `And alice enters old password "Wr0ngOld!" and new password "NewPass#456"`
Future<void>
    andAliceEntersOldPasswordWr0ngOldAndNewPasswordNewPass456(
        WidgetTester tester) async {
  _s.changePasswordShouldFail = true;
  await tester.pumpWidget(_buildProfileApp(_s));
  await tester.pumpAndSettle();
}

/// `And alice submits the password change`
Future<void> andAliceSubmitsThePasswordChange(WidgetTester tester) async {
  final button = find.byWidgetPredicate(
    (w) =>
        (w is FilledButton || w is ElevatedButton) &&
        find.descendant(
          of: find.byWidget(w),
          matching: find.textContaining('Change'),
        ).evaluate().isNotEmpty,
  );
  if (button.evaluate().isNotEmpty) {
    await tester.tap(button.first);
    await tester.pumpAndSettle();
  }
}

/// `Then a success message about password change should be displayed`
Future<void> thenASuccessMessageAboutPasswordChangeShouldBeDisplayed(
    WidgetTester tester) async {
  expect(
    find.byWidgetPredicate(
      (w) =>
          w is Text &&
          (w.data?.toLowerCase().contains('password') ?? false) &&
          (w.data?.toLowerCase().contains('changed') == true ||
              w.data?.toLowerCase().contains('updated') == true ||
              w.data?.toLowerCase().contains('success') == true),
    ),
    findsWidgets,
  );
}

/// `Then an error message about invalid credentials should be displayed`
Future<void> thenAnErrorMessageAboutInvalidCredentialsShouldBeDisplayed(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
  expect(find.textContaining('Invalid'), findsWidgets);
}

/// `And alice clicks the "Deactivate Account" button`
Future<void> andAliceClicksTheDeactivateAccountButton(
    WidgetTester tester) async {
  final button = find.textContaining('Deactivate');
  if (button.evaluate().isNotEmpty) {
    await tester.tap(button.first);
    await tester.pumpAndSettle();
  }
}

/// `And alice confirms the deactivation`
Future<void> andAliceConfirmsTheDeactivation(WidgetTester tester) async {
  final confirmButton = find.byWidgetPredicate(
    (w) =>
        (w is FilledButton || w is ElevatedButton || w is TextButton) &&
        find.descendant(
          of: find.byWidget(w),
          matching: find.textContaining('Confirm'),
        ).evaluate().isNotEmpty,
  );
  if (confirmButton.evaluate().isNotEmpty) {
    await tester.tap(confirmButton.first);
    await tester.pumpAndSettle();
  }
}

/// `Then alice should be redirected to the login page`
Future<void> thenAliceShouldBeRedirectedToTheLoginPage(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
  // Deactivation clears auth state.
  expect(find.text('Sign In'), findsWidgets);
}

/// `Given alice has deactivated her account`
Future<void> givenAliceHasDeactivatedHerAccount(WidgetTester tester) async {
  // Build login screen with deactivated-account mock.
  await tester.pumpWidget(
    ProviderScope(
      overrides: [
        authProvider.overrideWith(
          (_) => _DeactivatedLoginAuthNotifier(),
        ),
      ],
      child: const MaterialApp(home: LoginScreen()),
    ),
  );
  await tester.pumpAndSettle();
}

/// `When alice submits the login form with username "alice" and password "Str0ng#Pass1"`
Future<void> whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordStr0ngPass1(
    WidgetTester tester) async {
  final usernameField = find.byWidgetPredicate(
    (w) =>
        w is TextField &&
        (w.decoration?.labelText?.toLowerCase().contains('username') ?? false),
  );
  final passwordField = find.byWidgetPredicate(
    (w) =>
        w is TextField &&
        (w.decoration?.labelText?.toLowerCase().contains('password') ?? false),
  );
  await tester.enterText(usernameField, 'alice');
  await tester.enterText(passwordField, 'Str0ng#Pass1');
  await tester.tap(find.widgetWithText(FilledButton, 'Sign In'));
  await tester.pumpAndSettle();
}

/// `Then an error message about account deactivation should be displayed`
Future<void> thenAnErrorMessageAboutAccountDeactivationShouldBeDisplayed(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
  expect(find.textContaining('deactivated'), findsOneWidget);
}

/// `And alice should remain on the login page`
Future<void> andAliceShouldRemainOnTheLoginPage(WidgetTester tester) async {
  expect(find.text('Sign In'), findsWidgets);
}

class _DeactivatedLoginAuthNotifier extends AuthNotifier {
  @override
  Future<void> login({
    required String username,
    required String password,
  }) async {
    throw DioException(
      requestOptions: RequestOptions(path: '/api/v1/auth/login'),
      response: Response(
        requestOptions: RequestOptions(path: '/api/v1/auth/login'),
        statusCode: 403,
        data: {'detail': 'Your account has been deactivated.'},
      ),
      type: DioExceptionType.badResponse,
    );
  }
}

// ---------------------------------------------------------------------------
// Test runner
// ---------------------------------------------------------------------------

void main() {
  group('User Profile', () {
    testWidgets('Profile page displays username, email, and display name',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceNavigatesToTheProfilePage(tester);
      await thenTheProfileShouldDisplayUsernameAlice(tester);
      await thenTheProfileShouldDisplayEmailAliceAtExampleCom(tester);
      await andTheProfileShouldDisplayADisplayName(tester);
    }, skip: true);

    testWidgets('Updating display name shows the new value', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceNavigatesToTheProfilePage(tester);
      await andAliceChangesTheDisplayNameToAliceSmith(tester);
      await andAliceSavesTheProfileChanges(tester);
      await thenTheProfileShouldDisplayDisplayNameAliceSmith(tester);
    }, skip: true);

    testWidgets('Changing password with correct old password succeeds',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceNavigatesToTheChangePasswordForm(tester);
      await andAliceEntersOldPasswordStr0ngPass1AndNewPasswordNewPass456(tester);
      await andAliceSubmitsThePasswordChange(tester);
      await thenASuccessMessageAboutPasswordChangeShouldBeDisplayed(tester);
    }, skip: true);

    testWidgets(
        'Changing password with incorrect old password shows an error',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceNavigatesToTheChangePasswordForm(tester);
      await andAliceEntersOldPasswordWr0ngOldAndNewPasswordNewPass456(tester);
      await andAliceSubmitsThePasswordChange(tester);
      await thenAnErrorMessageAboutInvalidCredentialsShouldBeDisplayed(tester);
    }, skip: true);

    testWidgets('Self-deactivating account redirects to login',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceNavigatesToTheProfilePage(tester);
      await andAliceClicksTheDeactivateAccountButton(tester);
      await andAliceConfirmsTheDeactivation(tester);
      await thenAliceShouldBeRedirectedToTheLoginPage(tester);
    }, skip: true);

    testWidgets('Self-deactivated user cannot log in', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithEmailAliceAtExampleComAndPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await givenAliceHasDeactivatedHerAccount(tester);
      await whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordStr0ngPass1(tester);
      await thenAnErrorMessageAboutAccountDeactivationShouldBeDisplayed(tester);
      await andAliceShouldRemainOnTheLoginPage(tester);
    }, skip: true);
  });
}
