/// Application router configuration using GoRouter.
///
/// Public routes: `/` (health), `/login`, `/register`.
/// Authenticated routes: `/profile`, `/admin`, `/tokens`, `/expenses`,
/// `/expenses/:id`, `/expenses/summary`.
///
/// The auth guard redirects unauthenticated visitors to `/login`. After a
/// successful redirect the query parameter `redirectedFrom` is preserved so
/// the login screen can navigate back to the intended location.
library;

import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:demo_fe_dart_flutter/core/providers/auth_provider.dart';
import 'package:demo_fe_dart_flutter/screens/admin_screen.dart';
import 'package:demo_fe_dart_flutter/screens/expense_detail_screen.dart';
import 'package:demo_fe_dart_flutter/screens/expense_list_screen.dart';
import 'package:demo_fe_dart_flutter/screens/expense_summary_screen.dart';
import 'package:demo_fe_dart_flutter/screens/health_screen.dart';
import 'package:demo_fe_dart_flutter/screens/login_screen.dart';
import 'package:demo_fe_dart_flutter/screens/profile_screen.dart';
import 'package:demo_fe_dart_flutter/screens/register_screen.dart';
import 'package:demo_fe_dart_flutter/screens/tokens_screen.dart';

// ---------------------------------------------------------------------------
// Router provider — refreshes on auth state changes
// ---------------------------------------------------------------------------

/// Exposes the router so it can be refreshed when the auth state changes.
///
/// Pass [ProviderScope]'s [ProviderContainer] to [createAppRouter] in
/// [main.dart] to wire the auth provider into GoRouter's [refreshListenable].
GoRouter createAppRouter(ProviderContainer container) {
  // GoRouter needs a Listenable that fires when auth changes.
  final authNotifier = _AuthStateNotifier(container);

  return GoRouter(
    initialLocation: '/',
    refreshListenable: authNotifier,
    redirect: (context, state) {
      final isAuthenticated = container.read(isAuthenticatedProvider);
      final location = state.matchedLocation;

      const publicRoutes = {'/', '/login', '/register'};
      final isPublic = publicRoutes.contains(location);

      if (!isAuthenticated && !isPublic) {
        return '/login';
      }

      // Redirect /dashboard → /expenses
      if (location == '/dashboard') return '/expenses';

      return null;
    },
    routes: [
      // ------------------------------------------------------------------
      // Public routes
      // ------------------------------------------------------------------
      GoRoute(path: '/', builder: (context, state) => const HealthScreen()),
      GoRoute(
        path: '/login',
        builder: (context, state) {
          final successMessage = state.extra as String?;
          return LoginScreen(successMessage: successMessage);
        },
      ),
      GoRoute(
        path: '/register',
        builder: (context, state) => const RegisterScreen(),
      ),

      // ------------------------------------------------------------------
      // Authenticated routes
      // ------------------------------------------------------------------
      GoRoute(path: '/dashboard', redirect: (_, __) => '/expenses'),
      GoRoute(
        path: '/profile',
        builder: (context, state) => const ProfileScreen(),
      ),
      GoRoute(path: '/admin', builder: (context, state) => const AdminScreen()),
      GoRoute(
        path: '/tokens',
        builder: (context, state) => const TokensScreen(),
      ),

      // Expense sub-routes — order matters: /summary must come before /:id
      // to prevent GoRouter matching "summary" as an ID.
      GoRoute(
        path: '/expenses',
        builder: (context, state) => const ExpenseListScreen(),
        routes: [
          GoRoute(
            path: 'summary',
            builder: (context, state) => const ExpenseSummaryScreen(),
          ),
          GoRoute(
            path: ':id',
            builder: (context, state) {
              final id = state.pathParameters['id']!;
              return ExpenseDetailScreen(expenseId: id);
            },
          ),
        ],
      ),
    ],
  );
}

// ---------------------------------------------------------------------------
// Auth state notifier (bridges Riverpod → GoRouter refreshListenable)
// ---------------------------------------------------------------------------

class _AuthStateNotifier extends ChangeNotifier {
  _AuthStateNotifier(this._container) {
    _container.listen<bool>(
      isAuthenticatedProvider,
      (_, __) => notifyListeners(),
    );
  }

  final ProviderContainer _container;
}

// ---------------------------------------------------------------------------
// Backwards-compatible singleton (used by main.dart via ProviderScope)
// ---------------------------------------------------------------------------

/// Lazy singleton for the router. Created once with the root
/// [ProviderContainer] from [main.dart].
///
/// Usage in main.dart:
/// ```dart
/// final container = ProviderContainer();
/// final router = createAppRouter(container);
/// ```
///
/// Kept as a top-level variable so it can be referenced in
/// [MaterialApp.router].
late final GoRouter appRouter;
