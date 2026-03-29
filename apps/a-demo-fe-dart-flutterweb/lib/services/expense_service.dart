import '../models/expense.dart';
import 'api_client.dart';

Future<ExpenseListResponse> listExpenses({int page = 0, int size = 20}) async {
  final response = await apiClient.get<Map<String, dynamic>>(
    '/api/v1/expenses',
    queryParameters: {'page': page, 'size': size},
  );
  return ExpenseListResponse.fromJson(response.data!);
}

Future<Expense> getExpense(String id) async {
  final response =
      await apiClient.get<Map<String, dynamic>>('/api/v1/expenses/$id');
  return Expense.fromJson(response.data!);
}

Future<Expense> createExpense(CreateExpenseRequest data) async {
  final json = await keepalivePost('/api/v1/expenses', data.toJson());
  return Expense.fromJson(json);
}

Future<Expense> updateExpense(String id, UpdateExpenseRequest data) async {
  final json = await keepalivePut('/api/v1/expenses/$id', data.toJson());
  return Expense.fromJson(json);
}

Future<void> deleteExpense(String id) async {
  await keepaliveDelete('/api/v1/expenses/$id');
}

Future<Map<String, String>> getExpenseSummary({String? currency}) async {
  final response = await apiClient.get<Map<String, dynamic>>(
    '/api/v1/expenses/summary',
    queryParameters: currency != null ? {'currency': currency} : null,
  );
  return response.data!
      .map((String k, dynamic v) => MapEntry(k, v.toString()));
}
