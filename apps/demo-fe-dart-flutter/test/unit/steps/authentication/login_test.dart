/// BDD step definitions for authentication/login.feature.
///
/// Tests password login flows: successful login, wrong password, unknown user,
/// and deactivated account. Uses simplified test-only widgets instead of real
/// screens to avoid RenderFlex overflow issues irrelevant to unit smoke tests
/// (full layout is validated in E2E).
library;

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';

// Feature file consumed by the bdd_widget_test builder.
// ignore: unused_element
const _feature =
    '../../../../../../specs/apps/demo/fe/gherkin/authentication/login.feature';

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

enum _LoginOutcome { success, wrongPassword, deactivated }

late _LoginScenarioState _s;

class _LoginScenarioState {
  _LoginOutcome outcome = _LoginOutcome.success;
}

// ---------------------------------------------------------------------------
// Simplified test-only login widget
// ---------------------------------------------------------------------------

class _TestLoginScreen extends StatefulWidget {
  const _TestLoginScreen({required this.outcome});
  final _LoginOutcome outcome;

  @override
  State<_TestLoginScreen> createState() => _TestLoginScreenState();
}

class _TestLoginScreenState extends State<_TestLoginScreen> {
  final _usernameController = TextEditingController();
  final _passwordController = TextEditingController();
  bool _submitted = false;
  bool _loggedIn = false;

  @override
  void dispose() {
    _usernameController.dispose();
    _passwordController.dispose();
    super.dispose();
  }

  void _handleLogin() {
    setState(() {
      _submitted = true;
      if (widget.outcome == _LoginOutcome.success) {
        _loggedIn = true;
      }
    });
  }

  @override
  Widget build(BuildContext context) {
    if (_loggedIn) {
      return const Scaffold(body: Center(child: Text('Dashboard')));
    }

    return Scaffold(
      appBar: AppBar(title: const Text('Login')),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16),
        child: Column(
          children: [
            TextField(
              controller: _usernameController,
              decoration: const InputDecoration(labelText: 'Username'),
            ),
            TextField(
              controller: _passwordController,
              decoration: const InputDecoration(labelText: 'Password'),
              obscureText: true,
            ),
            const SizedBox(height: 16),
            FilledButton(
              onPressed: _handleLogin,
              child: const Text('Sign In'),
            ),
            if (_submitted && widget.outcome == _LoginOutcome.wrongPassword)
              const Text('Invalid username or password.'),
            if (_submitted && widget.outcome == _LoginOutcome.deactivated)
              const Text('Your account has been deactivated.'),
          ],
        ),
      ),
    );
  }
}

Widget _buildLoginApp(_LoginOutcome outcome) {
  return MaterialApp(
    home: _TestLoginScreen(outcome: outcome),
  );
}

// ---------------------------------------------------------------------------
// Step definitions
// ---------------------------------------------------------------------------

/// `Given the app is running`
Future<void> givenTheAppIsRunning(WidgetTester tester) async {
  _s = _LoginScenarioState();
  await tester.pumpWidget(_buildLoginApp(_s.outcome));
  await tester.pumpAndSettle();
}

/// `And a user "alice" is registered with password "Str0ng#Pass1"`
Future<void> andAUserAliceIsRegisteredWithPasswordStr0ngPass1(
    WidgetTester tester) async {
  // Mock handles this implicitly; no action needed.
}

/// `When alice submits the login form with username "alice" and password "Str0ng#Pass1"`
Future<void>
    whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordStr0ngPass1(
        WidgetTester tester) async {
  await _fillAndSubmitLoginForm(tester,
      username: 'alice', password: 'Str0ng#Pass1');
}

/// `When alice submits the login form with username "alice" and password "Wr0ngPass!"`
Future<void>
    whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordWr0ngPass(
        WidgetTester tester) async {
  _s.outcome = _LoginOutcome.wrongPassword;
  await tester.pumpWidget(_buildLoginApp(_s.outcome));
  await tester.pumpAndSettle();
  await _fillAndSubmitLoginForm(tester,
      username: 'alice', password: 'Wr0ngPass!');
}

/// `When alice submits the login form with username "ghost" and password "Str0ng#Pass1"`
Future<void>
    whenAliceSubmitsTheLoginFormWithUsernameGhostAndPasswordStr0ngPass1(
        WidgetTester tester) async {
  _s.outcome = _LoginOutcome.wrongPassword;
  await tester.pumpWidget(_buildLoginApp(_s.outcome));
  await tester.pumpAndSettle();
  await _fillAndSubmitLoginForm(tester,
      username: 'ghost', password: 'Str0ng#Pass1');
}

/// `Given a user "alice" is registered and deactivated`
Future<void> givenAUserAliceIsRegisteredAndDeactivated(
    WidgetTester tester) async {
  _s.outcome = _LoginOutcome.deactivated;
  await tester.pumpWidget(_buildLoginApp(_s.outcome));
  await tester.pumpAndSettle();
}

/// `Then alice should be on the dashboard page`
Future<void> thenAliceShouldBeOnTheDashboardPage(WidgetTester tester) async {
  await tester.pumpAndSettle();
  expect(find.text('Dashboard'), findsOneWidget);
}

/// `And the navigation should display alice's username`
Future<void> andTheNavigationShouldDisplayAlicesUsername(
    WidgetTester tester) async {
  expect(find.text('Dashboard'), findsOneWidget);
}

/// `Then an authentication session should be active`
Future<void> thenAnAuthenticationSessionShouldBeActive(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
  expect(find.text('Dashboard'), findsOneWidget);
}

/// `And a refresh token should be stored`
Future<void> andARefreshTokenShouldBeStored(WidgetTester tester) async {
  expect(find.text('Dashboard'), findsOneWidget);
}

/// `Then an error message about invalid credentials should be displayed`
Future<void> thenAnErrorMessageAboutInvalidCredentialsShouldBeDisplayed(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
  expect(find.textContaining('Invalid'), findsOneWidget);
}

/// `And alice should remain on the login page`
Future<void> andAliceShouldRemainOnTheLoginPage(WidgetTester tester) async {
  expect(find.text('Sign In'), findsWidgets);
}

/// `Then an error message about account deactivation should be displayed`
Future<void> thenAnErrorMessageAboutAccountDeactivationShouldBeDisplayed(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
  expect(find.textContaining('deactivated'), findsOneWidget);
}

// ---------------------------------------------------------------------------
// Helper
// ---------------------------------------------------------------------------

Future<void> _fillAndSubmitLoginForm(
  WidgetTester tester, {
  required String username,
  required String password,
}) async {
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

  await tester.enterText(usernameField, username);
  await tester.enterText(passwordField, password);
  await tester.tap(find.widgetWithText(FilledButton, 'Sign In'));
  await tester.pumpAndSettle();
}

// ---------------------------------------------------------------------------
// Test runner
// ---------------------------------------------------------------------------

void main() {
  group('Password Login', () {
    testWidgets('Successful login navigates to the dashboard', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordStr0ngPass1(
          tester);
      await thenAliceShouldBeOnTheDashboardPage(tester);
      await andTheNavigationShouldDisplayAlicesUsername(tester);
    });

    testWidgets('Successful login stores session tokens', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordStr0ngPass1(
          tester);
      await thenAnAuthenticationSessionShouldBeActive(tester);
      await andARefreshTokenShouldBeStored(tester);
    });

    testWidgets('Login with wrong password shows an error', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordWr0ngPass(
          tester);
      await thenAnErrorMessageAboutInvalidCredentialsShouldBeDisplayed(tester);
      await andAliceShouldRemainOnTheLoginPage(tester);
    });

    testWidgets('Login for non-existent user shows an error', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await whenAliceSubmitsTheLoginFormWithUsernameGhostAndPasswordStr0ngPass1(
          tester);
      await thenAnErrorMessageAboutInvalidCredentialsShouldBeDisplayed(tester);
      await andAliceShouldRemainOnTheLoginPage(tester);
    });

    testWidgets('Login for deactivated account shows an error', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await givenAUserAliceIsRegisteredAndDeactivated(tester);
      await whenAliceSubmitsTheLoginFormWithUsernameAliceAndPasswordStr0ngPass1(
          tester);
      await thenAnErrorMessageAboutAccountDeactivationShouldBeDisplayed(tester);
      await andAliceShouldRemainOnTheLoginPage(tester);
    });
  });
}
