/// Riverpod providers for admin user-management.
///
/// [adminUsersProvider] is a [FutureProvider.family] that accepts an
/// [AdminUsersParams] value object to support parameterised paginated queries.
/// [AdminNotifier] handles all mutating actions and invalidates the list after
/// each change.
library;

import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/api/admin_api.dart' as admin_api;
import 'package:demo_fe_dart_flutter/core/models/models.dart';

// ---------------------------------------------------------------------------
// Query params value object
// ---------------------------------------------------------------------------

/// Parameters for a paginated user list query.
class AdminUsersParams {
  const AdminUsersParams({this.page = 1, this.size = 20, this.search});

  final int page;
  final int size;
  final String? search;

  @override
  bool operator ==(Object other) =>
      other is AdminUsersParams &&
      other.page == page &&
      other.size == size &&
      other.search == search;

  @override
  int get hashCode => Object.hash(page, size, search);
}

// ---------------------------------------------------------------------------
// Read provider
// ---------------------------------------------------------------------------

/// Fetches a paginated user list for the given [AdminUsersParams].
///
/// Invalidated by [AdminNotifier] after every mutation so the UI reflects the
/// latest state.
final adminUsersProvider =
    FutureProvider.family<UserListResponse, AdminUsersParams>(
      (ref, params) => admin_api.listUsers(
        page: params.page,
        size: params.size,
        search: params.search,
      ),
    );

// ---------------------------------------------------------------------------
// Mutation notifier
// ---------------------------------------------------------------------------

/// Handles admin user-management mutations.
///
/// Each action performs the network call and then invalidates all cached
/// [adminUsersProvider] entries so listings refresh automatically.
class AdminNotifier extends StateNotifier<AsyncValue<void>> {
  AdminNotifier(this._ref) : super(const AsyncValue.data(null));

  final Ref _ref;

  /// Disables the user with [id] and records [reason] in the audit log.
  Future<void> disableUser(String id, String reason) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() => admin_api.disableUser(id, reason));
    _ref.invalidate(adminUsersProvider);
  }

  /// Re-enables the user with [id].
  Future<void> enableUser(String id) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() => admin_api.enableUser(id));
    _ref.invalidate(adminUsersProvider);
  }

  /// Unlocks the user with [id] after repeated login failures.
  Future<void> unlockUser(String id) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() => admin_api.unlockUser(id));
    _ref.invalidate(adminUsersProvider);
  }

  /// Forces a password reset for the user with [id] on next login.
  Future<void> forcePasswordReset(String id) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() => admin_api.forcePasswordReset(id));
    _ref.invalidate(adminUsersProvider);
  }
}

/// Provider for [AdminNotifier].
final adminNotifierProvider =
    StateNotifierProvider<AdminNotifier, AsyncValue<void>>(
      (ref) => AdminNotifier(ref),
    );
