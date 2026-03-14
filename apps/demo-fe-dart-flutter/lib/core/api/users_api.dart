/// User profile API functions.
///
/// Wraps the `/api/v1/users/*` endpoints for the currently authenticated user.
library;

import 'package:dio/dio.dart';
import 'package:demo_fe_dart_flutter/core/api/api_client.dart';
import 'package:demo_fe_dart_flutter/core/models/models.dart';

/// Returns the profile of the currently authenticated user.
Future<User> getCurrentUser() async {
  final response = await dio.get<Map<String, dynamic>>('/api/v1/users/me');
  return User.fromJson(response.data!);
}

/// Updates the display name of the current user and returns the updated [User].
Future<User> updateProfile(String displayName) async {
  final response = await dio.patch<Map<String, dynamic>>(
    '/api/v1/users/me',
    data: {'displayName': displayName},
  );
  return User.fromJson(response.data!);
}

/// Changes the current user's password.
///
/// Throws [DioException] on validation failure (e.g. wrong old password).
Future<void> changePassword({
  required String oldPassword,
  required String newPassword,
}) async {
  await dio.post<void>(
    '/api/v1/users/me/password',
    data: {'oldPassword': oldPassword, 'newPassword': newPassword},
  );
}

/// Deactivates the current user's own account.
///
/// After this call the user's tokens remain valid until expiry; the backend
/// will reject subsequent authenticated calls with 403.
Future<void> deactivateAccount() async {
  await dio.post<void>('/api/v1/users/me/deactivate');
}
