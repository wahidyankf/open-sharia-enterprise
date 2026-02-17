class Environment {
  static const String apiBaseUrl = String.fromEnvironment(
    'API_BASE_URL',
    defaultValue: 'http://localhost:8100/api/v1',
  );

  static bool get isDevelopment => apiBaseUrl.contains('localhost');
  static bool get isProduction => !isDevelopment;
}
