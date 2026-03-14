/// Minimal GoRouter setup for widget tests.
///
/// Provides a test router that serves the supplied [home] widget at '/'
/// and stub routes for common redirect targets.
library;

import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';
import 'package:demo_fe_dart_flutter/screens/login_screen.dart';
import 'package:demo_fe_dart_flutter/screens/expense_list_screen.dart';

/// Creates a [GoRouter] suitable for widget tests.
///
/// [home] is the widget rendered at '/'. Additional named routes for
/// '/login', '/expenses', and '/register' are provided as stubs so that
/// any `context.go(...)` calls inside tested widgets do not throw.
GoRouter buildTestRouter({required Widget Function(BuildContext, GoRouterState) home}) {
  return GoRouter(
    initialLocation: '/',
    routes: [
      GoRoute(path: '/', builder: home),
      GoRoute(
        path: '/login',
        builder: (context, state) {
          final msg = state.extra as String?;
          return LoginScreen(successMessage: msg);
        },
      ),
      GoRoute(
        path: '/register',
        builder: (context, state) =>
            const Scaffold(body: Text('Register')),
      ),
      GoRoute(
        path: '/expenses',
        builder: (context, state) => const ExpenseListScreen(),
        routes: [
          GoRoute(
            path: 'summary',
            builder: (context, state) =>
                const Scaffold(body: Text('Summary')),
          ),
          GoRoute(
            path: ':id',
            builder: (context, state) =>
                const Scaffold(body: Text('Expense Detail')),
          ),
        ],
      ),
      GoRoute(
        path: '/dashboard',
        builder: (context, state) =>
            const Scaffold(body: Text('Dashboard')),
      ),
      GoRoute(
        path: '/profile',
        builder: (context, state) =>
            const Scaffold(body: Text('Profile')),
      ),
      GoRoute(
        path: '/admin',
        builder: (context, state) =>
            const Scaffold(body: Text('Admin')),
      ),
      GoRoute(
        path: '/tokens',
        builder: (context, state) =>
            const Scaffold(body: Text('Tokens')),
      ),
    ],
  );
}
