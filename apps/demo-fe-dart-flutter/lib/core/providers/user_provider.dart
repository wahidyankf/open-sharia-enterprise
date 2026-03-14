/// Riverpod providers for the current user profile.
///
/// [currentUserProvider] is a [FutureProvider] that loads (and caches until
/// invalidated) the authenticated user's profile. Profile mutations —
/// [updateProfile], [changePassword], [deactivateAccount] — invalidate the
/// cache so the UI automatically re-fetches after any change.
library;

import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/api/users_api.dart' as users_api;
import 'package:demo_fe_dart_flutter/core/models/models.dart';
import 'package:demo_fe_dart_flutter/core/providers/auth_provider.dart';

// ---------------------------------------------------------------------------
// Read-only current user provider
// ---------------------------------------------------------------------------

/// Fetches the current user's profile from the backend.
///
/// Automatically refreshes when invalidated by a mutation. Returns
/// [AsyncValue.error] when the user is unauthenticated or the request fails.
final currentUserProvider = FutureProvider<User>((ref) async {
  // Re-evaluate whenever the auth state changes (e.g. after logout).
  ref.watch(authProvider);
  return users_api.getCurrentUser();
});

// ---------------------------------------------------------------------------
// Mutation notifier
// ---------------------------------------------------------------------------

/// Provides imperative mutation actions for the current user.
///
/// Each action performs the network call and then invalidates
/// [currentUserProvider] so widgets re-fetch the updated profile.
class UserNotifier extends StateNotifier<AsyncValue<void>> {
  UserNotifier(this._ref) : super(const AsyncValue.data(null));

  final Ref _ref;

  /// Updates the current user's display name.
  Future<void> updateProfile(String displayName) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() => users_api.updateProfile(displayName));
    _ref.invalidate(currentUserProvider);
  }

  /// Changes the current user's password.
  Future<void> changePassword({
    required String oldPassword,
    required String newPassword,
  }) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(
      () => users_api.changePassword(
        oldPassword: oldPassword,
        newPassword: newPassword,
      ),
    );
  }

  /// Deactivates the current user's account and clears the auth state.
  Future<void> deactivateAccount() async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(users_api.deactivateAccount);
    if (!state.hasError) {
      // Clear auth state so the app redirects to login.
      _ref.read(authProvider.notifier).logout();
    }
  }
}

/// Provider for [UserNotifier].
final userNotifierProvider =
    StateNotifierProvider<UserNotifier, AsyncValue<void>>(
      (ref) => UserNotifier(ref),
    );
