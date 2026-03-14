/// Riverpod providers for expense CRUD and summary.
///
/// [expensesProvider] is a [FutureProvider.family] for paginated lists.
/// [expenseDetailProvider] loads a single expense by ID.
/// [expenseSummaryProvider] loads the summary for a currency.
/// [ExpenseNotifier] handles mutations and invalidates caches on success.
library;

import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:demo_fe_dart_flutter/core/api/expenses_api.dart'
    as expenses_api;
import 'package:demo_fe_dart_flutter/core/models/models.dart';

// ---------------------------------------------------------------------------
// Query params value object
// ---------------------------------------------------------------------------

/// Parameters for a paginated expense list query.
class ExpensesParams {
  const ExpensesParams({this.page = 1, this.size = 20});

  final int page;
  final int size;

  @override
  bool operator ==(Object other) =>
      other is ExpensesParams && other.page == page && other.size == size;

  @override
  int get hashCode => Object.hash(page, size);
}

// ---------------------------------------------------------------------------
// Read providers
// ---------------------------------------------------------------------------

/// Fetches a paginated list of expenses.
final expensesProvider =
    FutureProvider.family<ExpenseListResponse, ExpensesParams>(
      (ref, params) =>
          expenses_api.listExpenses(page: params.page, size: params.size),
    );

/// Fetches a single expense by [id].
final expenseDetailProvider = FutureProvider.family<Expense, String>(
  (ref, id) => expenses_api.getExpense(id),
);

/// Fetches the expense summary for the given [currency] code (e.g. `'USD'`).
final expenseSummaryProvider = FutureProvider.family<ExpenseSummary, String>(
  (ref, currency) => expenses_api.getExpenseSummary(currency),
);

// ---------------------------------------------------------------------------
// Mutation notifier
// ---------------------------------------------------------------------------

/// Handles expense create / update / delete mutations.
///
/// After each successful mutation all cached expense providers are invalidated
/// so the UI automatically re-fetches the latest data.
class ExpenseNotifier extends StateNotifier<AsyncValue<void>> {
  ExpenseNotifier(this._ref) : super(const AsyncValue.data(null));

  final Ref _ref;

  /// Creates a new expense.
  Future<Expense?> createExpense({
    required String title,
    required double amount,
    required String currency,
    required String category,
    required String expenseDate,
    String? description,
  }) async {
    state = const AsyncValue.loading();
    Expense? created;
    state = await AsyncValue.guard(() async {
      created = await expenses_api.createExpense(
        title: title,
        amount: amount,
        currency: currency,
        category: category,
        expenseDate: expenseDate,
        description: description,
      );
    });
    _invalidateAll();
    return created;
  }

  /// Updates an existing expense.
  Future<Expense?> updateExpense(
    String id, {
    String? title,
    double? amount,
    String? currency,
    String? category,
    String? expenseDate,
    String? description,
  }) async {
    state = const AsyncValue.loading();
    Expense? updated;
    state = await AsyncValue.guard(() async {
      updated = await expenses_api.updateExpense(
        id,
        title: title,
        amount: amount,
        currency: currency,
        category: category,
        expenseDate: expenseDate,
        description: description,
      );
    });
    _invalidateAll();
    return updated;
  }

  /// Deletes the expense with [id].
  Future<void> deleteExpense(String id) async {
    state = const AsyncValue.loading();
    state = await AsyncValue.guard(() => expenses_api.deleteExpense(id));
    _invalidateAll();
  }

  void _invalidateAll() {
    _ref.invalidate(expensesProvider);
    _ref.invalidate(expenseDetailProvider);
    _ref.invalidate(expenseSummaryProvider);
  }
}

/// Provider for [ExpenseNotifier].
final expenseNotifierProvider =
    StateNotifierProvider<ExpenseNotifier, AsyncValue<void>>(
      (ref) => ExpenseNotifier(ref),
    );
