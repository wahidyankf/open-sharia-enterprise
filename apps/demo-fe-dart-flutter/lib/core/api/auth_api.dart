/// Authentication API functions.
///
/// Wraps the `/api/v1/auth/*` endpoints and returns typed [AuthTokens] or
/// void. All network errors surface as [DioException].
library;

import 'package:dio/dio.dart';
import 'package:demo_fe_dart_flutter/core/api/api_client.dart';
import 'package:demo_fe_dart_flutter/core/models/models.dart';

/// Registers a new user account.
///
/// Returns [AuthTokens] containing the initial access and refresh tokens.
Future<AuthTokens> register({
  required String username,
  required String email,
  required String password,
}) async {
  final response = await dio.post<Map<String, dynamic>>(
    '/api/v1/auth/register',
    data: {'username': username, 'email': email, 'password': password},
  );
  return AuthTokens.fromJson(response.data!);
}

/// Authenticates an existing user and returns token pair.
Future<AuthTokens> login({
  required String username,
  required String password,
}) async {
  final response = await dio.post<Map<String, dynamic>>(
    '/api/v1/auth/login',
    data: {'username': username, 'password': password},
  );
  return AuthTokens.fromJson(response.data!);
}

/// Exchanges a refresh token for a new [AuthTokens] pair.
Future<AuthTokens> refreshToken(String token) async {
  final response = await dio.post<Map<String, dynamic>>(
    '/api/v1/auth/refresh',
    data: {'refresh_token': token},
  );
  return AuthTokens.fromJson(response.data!);
}

/// Invalidates the supplied refresh token (logs out current session).
Future<void> logout(String token) async {
  await dio.post<void>('/api/v1/auth/logout', data: {'refresh_token': token});
}

/// Invalidates all refresh tokens for the authenticated user.
Future<void> logoutAll() async {
  await dio.post<void>('/api/v1/auth/logout-all');
}
