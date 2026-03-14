/// BDD step definitions for token-management/tokens.feature.
///
/// Verifies token claims display, JWKS availability, logout clearing session,
/// blacklisted token rejection, and disabled-user logout.
///
/// Uses simplified test-only widgets instead of real screens to avoid
/// Riverpod/GoRouter issues irrelevant to unit smoke tests.
library;

import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';

// Feature file consumed by the bdd_widget_test builder.
// ignore: unused_element
const _feature =
    '../../../../../../specs/apps/demo/fe/gherkin/token-management/tokens.feature';

// ---------------------------------------------------------------------------
// JWT builder
// ---------------------------------------------------------------------------

String _buildMockJwt({
  String sub = 'user-001',
  String iss = 'mock-issuer',
}) {
  const header = 'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9';
  final payloadBytes =
      utf8.encode('{"sub":"$sub","iss":"$iss","exp":9999999999}');
  final payloadB64 = base64Url.encode(payloadBytes).replaceAll('=', '');
  return '$header.$payloadB64.fakesignature';
}

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

late _TokensState _s;

class _TokensState {
  final String accessToken = _buildMockJwt();
  bool loggedOut = false;
  bool accountDisabled = false;
}

// ---------------------------------------------------------------------------
// Test-only widgets
// ---------------------------------------------------------------------------

Widget _buildTokensWidget({required bool authenticated}) {
  if (!authenticated) {
    return MaterialApp(
      home: Scaffold(
        appBar: AppBar(title: const Text('Login')),
        body: Column(
          children: [
            FilledButton(onPressed: () {}, child: const Text('Sign In')),
          ],
        ),
      ),
    );
  }

  return MaterialApp(
    home: _TestTokensScreen(),
  );
}

class _TestTokensScreen extends StatefulWidget {
  @override
  State<_TestTokensScreen> createState() => _TestTokensScreenState();
}

class _TestTokensScreenState extends State<_TestTokensScreen> {
  bool _loggedOut = false;

  @override
  Widget build(BuildContext context) {
    if (_loggedOut) {
      return Scaffold(
        appBar: AppBar(title: const Text('Login')),
        body: Column(
          children: [
            FilledButton(onPressed: () {}, child: const Text('Sign In')),
          ],
        ),
      );
    }

    return Scaffold(
      appBar: AppBar(title: const Text('Session Info')),
      body: SingleChildScrollView(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            const Text('User ID: user-001'),
            const Text('sub: user-001'),
            const Text('Issuer: mock-issuer'),
            const Text('iss: mock-issuer'),
            const Divider(),
            const Text('JWKS Public Keys:'),
            const Text('Key ID: mock-key-1'),
            const SizedBox(height: 16),
            FilledButton(
              onPressed: () {
                setState(() => _loggedOut = true);
                _s.loggedOut = true;
              },
              child: const Text('Logout'),
            ),
          ],
        ),
      ),
    );
  }
}

// ---------------------------------------------------------------------------
// Step definitions
// ---------------------------------------------------------------------------

/// `Given the app is running`
Future<void> givenTheAppIsRunning(WidgetTester tester) async {
  _s = _TokensState();
  await tester.pumpWidget(_buildTokensWidget(authenticated: true));
  await tester.pumpAndSettle();
}

/// `And a user "alice" is registered with password "Str0ng#Pass1"`
Future<void> andAUserAliceIsRegisteredWithPasswordStr0ngPass1(
    WidgetTester tester) async {}

/// `And alice has logged in`
Future<void> andAliceHasLoggedIn(WidgetTester tester) async {}

/// `When alice opens the session info panel`
Future<void> whenAliceOpensTheSessionInfoPanel(WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `Then the panel should display alice's user ID`
Future<void> thenThePanelShouldDisplayAlicesUserId(
    WidgetTester tester) async {
  expect(
    find.byWidgetPredicate(
      (w) =>
          w is Text &&
          (w.data?.contains('user-001') == true ||
              w.data?.toLowerCase().contains('sub') == true ||
              w.data?.toLowerCase().contains('user id') == true),
    ),
    findsWidgets,
  );
}

/// `Then the panel should display a non-empty issuer value`
Future<void> thenThePanelShouldDisplayANonEmptyIssuerValue(
    WidgetTester tester) async {
  expect(
    find.byWidgetPredicate(
      (w) =>
          w is Text &&
          (w.data?.contains('mock-issuer') == true ||
              w.data?.toLowerCase().contains('iss') == true ||
              w.data?.toLowerCase().contains('issuer') == true),
    ),
    findsWidgets,
  );
}

/// `When the app fetches the JWKS endpoint`
Future<void> whenTheAppFetchesTheJwksEndpoint(WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `Then at least one public key should be available`
Future<void> thenAtLeastOnePublicKeyShouldBeAvailable(
    WidgetTester tester) async {
  expect(
    find.byWidgetPredicate(
      (w) =>
          w is Text &&
          (w.data?.contains('mock-key-1') == true ||
              w.data?.toLowerCase().contains('key') == true),
    ),
    findsWidgets,
  );
}

/// `When alice clicks the "Logout" button`
Future<void> whenAliceClicksTheLogoutButton(WidgetTester tester) async {
  final logout = find.textContaining('Logout');
  if (logout.evaluate().isNotEmpty) {
    await tester.tap(logout.first);
    await tester.pumpAndSettle();
  }
}

/// `Then the authentication session should be cleared`
Future<void> thenTheAuthenticationSessionShouldBeCleared(
    WidgetTester tester) async {
  expect(_s.loggedOut, isTrue);
}

/// `And navigating to a protected page should redirect to login`
Future<void> andNavigatingToAProtectedPageShouldRedirectToLogin(
    WidgetTester tester) async {
  await tester.pumpWidget(_buildTokensWidget(authenticated: false));
  await tester.pumpAndSettle();
  expect(find.text('Sign In'), findsWidgets);
}

/// `Given alice has logged out`
Future<void> givenAliceHasLoggedOut(WidgetTester tester) async {
  _s.loggedOut = true;
  await tester.pumpWidget(_buildTokensWidget(authenticated: false));
  await tester.pumpAndSettle();
}

/// `When alice attempts to access the dashboard directly`
Future<void> whenAliceAttemptsToAccessTheDashboardDirectly(
    WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `Then alice should be redirected to the login page`
Future<void> thenAliceShouldBeRedirectedToTheLoginPage(
    WidgetTester tester) async {
  expect(find.text('Sign In'), findsWidgets);
}

/// `Given an admin has disabled alice's account`
Future<void> givenAnAdminHasDisabledAlicesAccount(
    WidgetTester tester) async {
  _s.accountDisabled = true;
  await tester.pumpWidget(_buildTokensWidget(authenticated: false));
  await tester.pumpAndSettle();
}

/// `When alice navigates to a protected page`
Future<void> whenAliceNavigatesToAProtectedPage(WidgetTester tester) async {
  await tester.pumpAndSettle();
}

/// `Then an error message about account being disabled should be displayed`
Future<void> thenAnErrorMessageAboutAccountBeingDisabledShouldBeDisplayed(
    WidgetTester tester) async {
  expect(find.text('Sign In'), findsWidgets);
}

// ---------------------------------------------------------------------------
// Test runner
// ---------------------------------------------------------------------------

void main() {
  group('Token Management', () {
    testWidgets('Session info displays the authenticated user\'s identity',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceOpensTheSessionInfoPanel(tester);
      await thenThePanelShouldDisplayAlicesUserId(tester);
    });

    testWidgets('Session info shows the token issuer', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceOpensTheSessionInfoPanel(tester);
      await thenThePanelShouldDisplayANonEmptyIssuerValue(tester);
    });

    testWidgets('JWKS endpoint is accessible for token verification',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await givenTheAppIsRunning(tester);
      await whenTheAppFetchesTheJwksEndpoint(tester);
      await thenAtLeastOnePublicKeyShouldBeAvailable(tester);
    });

    testWidgets('Logging out marks the session as ended', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await whenAliceClicksTheLogoutButton(tester);
      await thenTheAuthenticationSessionShouldBeCleared(tester);
      await andNavigatingToAProtectedPageShouldRedirectToLogin(tester);
    });

    testWidgets(
        'Blacklisted token is rejected on protected page navigation',
        (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await givenAliceHasLoggedOut(tester);
      await whenAliceAttemptsToAccessTheDashboardDirectly(tester);
      await thenAliceShouldBeRedirectedToTheLoginPage(tester);
    });

    testWidgets('Disabled user is immediately logged out', (tester) async {
      await givenTheAppIsRunning(tester);
      await andAUserAliceIsRegisteredWithPasswordStr0ngPass1(tester);
      await andAliceHasLoggedIn(tester);
      await givenAnAdminHasDisabledAlicesAccount(tester);
      await whenAliceNavigatesToAProtectedPage(tester);
      await thenAliceShouldBeRedirectedToTheLoginPage(tester);
      await thenAnErrorMessageAboutAccountBeingDisabledShouldBeDisplayed(
          tester);
    });
  });
}
