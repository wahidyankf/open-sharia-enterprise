/// Dio HTTP client factory with auth interceptor and token refresh logic.
///
/// The base URL defaults to [kBaseUrl] and can be overridden by injecting a
/// different [Dio] instance. The auth interceptor attaches the current Bearer
/// token to every request and retries once on HTTP 401 after refreshing.
library;

import 'package:dio/dio.dart';

/// Default backend base URL used by Flutter Web (no proxy available).
const String kBaseUrl = 'http://localhost:8201';

/// Token store used by the auth interceptor.
///
/// Populated by [AuthNotifier] after a successful login/refresh and cleared on
/// logout. Using a plain mutable class here avoids a circular dependency
/// between the Dio singleton and Riverpod providers.
class TokenStore {
  TokenStore._();

  static final TokenStore instance = TokenStore._();

  String? accessToken;
  String? refreshToken;
}

/// Creates and returns the application-wide [Dio] instance.
///
/// The instance is equipped with:
/// - [baseUrl] set to [kBaseUrl] (or [overrideBaseUrl] when testing)
/// - An [AuthInterceptor] that injects Bearer tokens and handles 401 retry
Dio createDio({String? overrideBaseUrl}) {
  final dio = Dio(
    BaseOptions(
      baseUrl: overrideBaseUrl ?? kBaseUrl,
      connectTimeout: const Duration(seconds: 15),
      receiveTimeout: const Duration(seconds: 15),
      headers: {'Content-Type': 'application/json'},
    ),
  );

  dio.interceptors.add(AuthInterceptor(dio));
  return dio;
}

/// Singleton [Dio] instance used by all API classes.
final Dio dio = createDio();

/// Interceptor that:
/// 1. Adds `Authorization: Bearer <token>` to every request when a token is
///    available in [TokenStore].
/// 2. On HTTP 401, attempts a token refresh via `/api/v1/auth/refresh` and
///    retries the original request once. On second 401 it gives up.
class AuthInterceptor extends Interceptor {
  AuthInterceptor(this._dio);

  final Dio _dio;

  static const String _retryHeader = 'x-retry-after-refresh';

  @override
  void onRequest(RequestOptions options, RequestInterceptorHandler handler) {
    final token = TokenStore.instance.accessToken;
    if (token != null && token.isNotEmpty) {
      options.headers['Authorization'] = 'Bearer $token';
    }
    handler.next(options);
  }

  @override
  void onError(DioException err, ErrorInterceptorHandler handler) async {
    final response = err.response;
    final options = err.requestOptions;

    // Only attempt refresh on 401. Avoid infinite loop by checking retry flag.
    if (response?.statusCode == 401 && options.headers[_retryHeader] == null) {
      final refreshToken = TokenStore.instance.refreshToken;
      if (refreshToken == null || refreshToken.isEmpty) {
        handler.next(err);
        return;
      }

      try {
        final refreshResponse = await _dio.post<Map<String, dynamic>>(
          '/api/v1/auth/refresh',
          data: {'refresh_token': refreshToken},
          options: Options(
            headers: {
              // Prevent this refresh call from triggering another retry.
              _retryHeader: 'true',
            },
          ),
        );

        final newAccessToken =
            (refreshResponse.data?['access_token'] as String?) ?? '';
        final newRefreshToken =
            (refreshResponse.data?['refresh_token'] as String?) ?? refreshToken;

        TokenStore.instance.accessToken = newAccessToken;
        TokenStore.instance.refreshToken = newRefreshToken;

        // Retry original request with updated token.
        final retryOptions = options.copyWith(
          headers: {
            ...options.headers,
            'Authorization': 'Bearer $newAccessToken',
            _retryHeader: 'true',
          },
        );

        final retryResponse = await _dio.fetch<dynamic>(retryOptions);
        handler.resolve(retryResponse);
      } on DioException catch (refreshErr) {
        // Refresh failed — clear stored tokens and propagate the error.
        TokenStore.instance.accessToken = null;
        TokenStore.instance.refreshToken = null;
        handler.next(refreshErr);
      }
      return;
    }

    handler.next(err);
  }
}
