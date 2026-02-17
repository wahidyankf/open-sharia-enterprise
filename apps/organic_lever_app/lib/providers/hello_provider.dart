import 'package:flutter/foundation.dart';
import '../models/hello_response.dart';
import '../services/api_service.dart';

class HelloProvider extends ChangeNotifier {
  final ApiService _apiService;

  HelloResponse? _response;
  bool _isLoading = false;
  String? _error;

  HelloResponse? get response => _response;
  bool get isLoading => _isLoading;
  String? get error => _error;

  HelloProvider({ApiService? apiService})
      : _apiService = apiService ?? ApiService();

  Future<void> fetchHello() async {
    _isLoading = true;
    _error = null;
    notifyListeners();

    try {
      _response = await _apiService.getHello();
      _isLoading = false;
      notifyListeners();
    } catch (e) {
      _error = e.toString();
      _isLoading = false;
      notifyListeners();
    }
  }

  @override
  void dispose() {
    _apiService.dispose();
    super.dispose();
  }
}
