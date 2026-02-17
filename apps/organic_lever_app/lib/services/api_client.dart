import 'dart:convert';
import 'package:http/http.dart' as http;
import '../config/environment.dart';

class ApiClient {
  final http.Client _httpClient;
  final String baseUrl;

  ApiClient({http.Client? httpClient, String? baseUrl})
      : _httpClient = httpClient ?? http.Client(),
        baseUrl = baseUrl ?? Environment.apiBaseUrl;

  Future<T> get<T>(
    String endpoint,
    T Function(Map<String, dynamic>) fromJson,
  ) async {
    final url = Uri.parse('$baseUrl$endpoint');
    final response = await _httpClient.get(
      url,
      headers: {'Content-Type': 'application/json'},
    );

    if (response.statusCode == 200) {
      final json = jsonDecode(response.body) as Map<String, dynamic>;
      return fromJson(json);
    } else {
      throw ApiException(
        statusCode: response.statusCode,
        message: 'Failed to load data',
      );
    }
  }

  void dispose() => _httpClient.close();
}

class ApiException implements Exception {
  final int statusCode;
  final String message;

  ApiException({required this.statusCode, required this.message});

  @override
  String toString() => 'ApiException: $message (HTTP $statusCode)';
}
