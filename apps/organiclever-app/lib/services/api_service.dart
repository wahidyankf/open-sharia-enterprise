import '../models/hello_response.dart';
import 'api_client.dart';

class ApiService {
  final ApiClient _client;

  ApiService({ApiClient? client}) : _client = client ?? ApiClient();

  Future<HelloResponse> getHello() async {
    return _client.get<HelloResponse>(
      '/hello',
      (json) => HelloResponse.fromJson(json),
    );
  }

  void dispose() => _client.dispose();
}
