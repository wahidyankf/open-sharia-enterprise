/// Riverpod state management for authentication.
///
/// [AuthNotifier] manages the in-memory auth state (access token, refresh
/// token, and a derived [isAuthenticated] flag). It also updates [TokenStore]
/// so the Dio auth interceptor always carries a current token.
library;

import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/api/api_client.dart';
import 'package:demo_fe_dart_flutter/core/api/auth_api.dart' as auth_api;
import 'package:demo_fe_dart_flutter/core/models/models.dart';

// ---------------------------------------------------------------------------
// State class
// ---------------------------------------------------------------------------

/// Immutable snapshot of the authentication state.
class AuthState {
  const AuthState({this.accessToken, this.refreshToken});

  /// Unauthenticated initial state.
  const AuthState.unauthenticated() : accessToken = null, refreshToken = null;

  final String? accessToken;
  final String? refreshToken;

  bool get isAuthenticated => accessToken != null && accessToken!.isNotEmpty;

  AuthState copyWith({String? accessToken, String? refreshToken}) => AuthState(
    accessToken: accessToken ?? this.accessToken,
    refreshToken: refreshToken ?? this.refreshToken,
  );
}

// ---------------------------------------------------------------------------
// StateNotifier
// ---------------------------------------------------------------------------

/// Manages auth state and delegates all network calls to [auth_api].
class AuthNotifier extends StateNotifier<AuthState> {
  AuthNotifier() : super(const AuthState.unauthenticated());

  /// Authenticates with [username] and [password].
  ///
  /// On success the state transitions to authenticated and [TokenStore] is
  /// updated so subsequent Dio requests carry the new token.
  ///
  /// Throws [Exception] (typically [DioException]) on failure.
  Future<void> login({
    required String username,
    required String password,
  }) async {
    final tokens = await auth_api.login(username: username, password: password);
    _applyTokens(tokens);
  }

  /// Registers a new account and immediately authenticates the session.
  Future<void> register({
    required String username,
    required String email,
    required String password,
  }) async {
    final tokens = await auth_api.register(
      username: username,
      email: email,
      password: password,
    );
    _applyTokens(tokens);
  }

  /// Logs out the current session by invalidating the refresh token.
  Future<void> logout() async {
    final token = state.refreshToken;
    if (token != null && token.isNotEmpty) {
      await auth_api.logout(token);
    }
    _clearTokens();
  }

  /// Invalidates all sessions for the current user.
  Future<void> logoutAll() async {
    await auth_api.logoutAll();
    _clearTokens();
  }

  /// Refreshes the access token using the stored refresh token.
  ///
  /// Called automatically by the auth interceptor on 401; can also be called
  /// proactively before a known-expiry event.
  Future<void> refresh() async {
    final token = state.refreshToken;
    if (token == null || token.isEmpty) return;

    final tokens = await auth_api.refreshToken(token);
    _applyTokens(tokens);
  }

  // ---------------------------------------------------------------------------
  // Private helpers
  // ---------------------------------------------------------------------------

  void _applyTokens(AuthTokens tokens) {
    state = AuthState(
      accessToken: tokens.accessToken,
      refreshToken: tokens.refreshToken,
    );
    // Sync to TokenStore so the Dio interceptor picks up the new token.
    TokenStore.instance.accessToken = tokens.accessToken;
    TokenStore.instance.refreshToken = tokens.refreshToken;
  }

  void _clearTokens() {
    state = const AuthState.unauthenticated();
    TokenStore.instance.accessToken = null;
    TokenStore.instance.refreshToken = null;
  }
}

// ---------------------------------------------------------------------------
// Providers
// ---------------------------------------------------------------------------

/// Primary auth state provider.
final authProvider = StateNotifierProvider<AuthNotifier, AuthState>(
  (ref) => AuthNotifier(),
);

/// Convenience provider that exposes only the [isAuthenticated] flag.
final isAuthenticatedProvider = Provider<bool>(
  (ref) => ref.watch(authProvider).isAuthenticated,
);
