/// Riverpod providers for token inspection.
///
/// [jwksProvider] fetches the backend's JSON Web Key Set.
/// [tokenClaimsProvider] decodes the current access token's payload claims
/// without signature verification — for display purposes only.
library;

import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/api/tokens_api.dart' as tokens_api;
import 'package:demo_fe_dart_flutter/core/providers/auth_provider.dart';

/// Fetches the JWKS from `/.well-known/jwks.json`.
///
/// The result is cached by Riverpod. Invalidate this provider if the backend
/// rotates its signing keys.
final jwksProvider = FutureProvider<Map<String, dynamic>>(
  (_) => tokens_api.getJwks(),
);

/// Decodes and exposes the claims from the current access token.
///
/// Returns an empty map when the user is unauthenticated or the token is
/// malformed. Re-evaluates whenever the auth state changes (e.g. after
/// login, logout, or refresh).
final tokenClaimsProvider = Provider<Map<String, dynamic>>((ref) {
  final accessToken = ref.watch(authProvider).accessToken;
  if (accessToken == null || accessToken.isEmpty) {
    return {};
  }
  return tokens_api.decodeTokenClaims(accessToken);
});
