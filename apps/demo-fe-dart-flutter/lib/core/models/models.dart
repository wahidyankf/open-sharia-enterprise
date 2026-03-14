/// Data models for the demo frontend application.
///
/// All models use simple Dart classes with fromJson factory constructors.
/// No code generation (freezed/json_serializable) is used to keep the
/// build process simple.
library;

// ---------------------------------------------------------------------------
// Auth models
// ---------------------------------------------------------------------------

/// JWT token pair returned after successful authentication.
class AuthTokens {
  const AuthTokens({
    required this.accessToken,
    required this.refreshToken,
    required this.tokenType,
    required this.expiresIn,
  });

  factory AuthTokens.fromJson(Map<String, dynamic> json) => AuthTokens(
    accessToken: json['access_token'] as String,
    refreshToken: json['refresh_token'] as String,
    tokenType: (json['token_type'] as String?) ?? 'Bearer',
    expiresIn: (json['expires_in'] as num?)?.toInt() ?? 3600,
  );

  final String accessToken;
  final String refreshToken;
  final String tokenType;
  final int expiresIn;

  Map<String, dynamic> toJson() => {
    'access_token': accessToken,
    'refresh_token': refreshToken,
    'token_type': tokenType,
    'expires_in': expiresIn,
  };
}

// ---------------------------------------------------------------------------
// User models
// ---------------------------------------------------------------------------

/// A single user account.
class User {
  const User({
    required this.id,
    required this.username,
    required this.email,
    required this.displayName,
    required this.role,
    required this.status,
    required this.createdAt,
    this.updatedAt,
  });

  factory User.fromJson(Map<String, dynamic> json) => User(
    id: json['id'] as String,
    username: json['username'] as String,
    email: json['email'] as String,
    displayName: (json['display_name'] as String?) ?? '',
    role: json['role'] as String,
    status: json['status'] as String,
    createdAt: json['created_at'] as String,
    updatedAt: json['updated_at'] as String?,
  );

  final String id;
  final String username;
  final String email;
  final String displayName;
  final String role;
  final String status;
  final String createdAt;
  final String? updatedAt;
}

/// Paginated list of users returned by the admin endpoint.
class UserListResponse {
  const UserListResponse({
    required this.users,
    required this.total,
    required this.page,
    required this.size,
  });

  factory UserListResponse.fromJson(Map<String, dynamic> json) =>
      UserListResponse(
        users: (json['users'] as List<dynamic>)
            .map((e) => User.fromJson(e as Map<String, dynamic>))
            .toList(),
        total: (json['total'] as num).toInt(),
        page: (json['page'] as num).toInt(),
        size: (json['size'] as num).toInt(),
      );

  final List<User> users;
  final int total;
  final int page;
  final int size;
}

// ---------------------------------------------------------------------------
// Expense models
// ---------------------------------------------------------------------------

/// A single expense record.
class Expense {
  const Expense({
    required this.id,
    required this.userId,
    required this.title,
    required this.amount,
    required this.currency,
    required this.category,
    required this.expenseDate,
    required this.createdAt,
    this.description,
    this.updatedAt,
    this.attachmentCount = 0,
  });

  factory Expense.fromJson(Map<String, dynamic> json) => Expense(
    id: json['id'] as String,
    userId: json['user_id'] as String,
    title: json['title'] as String,
    amount: (json['amount'] as num).toDouble(),
    currency: json['currency'] as String,
    category: json['category'] as String,
    expenseDate: json['expense_date'] as String,
    createdAt: json['created_at'] as String,
    description: json['description'] as String?,
    updatedAt: json['updated_at'] as String?,
    attachmentCount: (json['attachment_count'] as num?)?.toInt() ?? 0,
  );

  final String id;
  final String userId;
  final String title;
  final double amount;
  final String currency;
  final String category;
  final String expenseDate;
  final String createdAt;
  final String? description;
  final String? updatedAt;
  final int attachmentCount;
}

/// Paginated list of expenses.
class ExpenseListResponse {
  const ExpenseListResponse({
    required this.expenses,
    required this.total,
    required this.page,
    required this.size,
  });

  factory ExpenseListResponse.fromJson(Map<String, dynamic> json) =>
      ExpenseListResponse(
        expenses: (json['expenses'] as List<dynamic>)
            .map((e) => Expense.fromJson(e as Map<String, dynamic>))
            .toList(),
        total: (json['total'] as num).toInt(),
        page: (json['page'] as num).toInt(),
        size: (json['size'] as num).toInt(),
      );

  final List<Expense> expenses;
  final int total;
  final int page;
  final int size;
}

/// Summary of expenses grouped by category.
class ExpenseSummary {
  const ExpenseSummary({
    required this.currency,
    required this.totalAmount,
    required this.expenseCount,
    required this.categoryBreakdowns,
  });

  factory ExpenseSummary.fromJson(Map<String, dynamic> json) => ExpenseSummary(
    currency: json['currency'] as String,
    totalAmount: (json['total_amount'] as num).toDouble(),
    expenseCount: (json['expense_count'] as num).toInt(),
    categoryBreakdowns: (json['category_breakdowns'] as List<dynamic>)
        .map((e) => CategoryBreakdown.fromJson(e as Map<String, dynamic>))
        .toList(),
  );

  final String currency;
  final double totalAmount;
  final int expenseCount;
  final List<CategoryBreakdown> categoryBreakdowns;
}

/// Spending breakdown for a single expense category.
class CategoryBreakdown {
  const CategoryBreakdown({
    required this.category,
    required this.totalAmount,
    required this.expenseCount,
    required this.percentage,
  });

  factory CategoryBreakdown.fromJson(Map<String, dynamic> json) =>
      CategoryBreakdown(
        category: json['category'] as String,
        totalAmount: (json['total_amount'] as num).toDouble(),
        expenseCount: (json['expense_count'] as num).toInt(),
        percentage: (json['percentage'] as num).toDouble(),
      );

  final String category;
  final double totalAmount;
  final int expenseCount;
  final double percentage;
}

// ---------------------------------------------------------------------------
// Report models
// ---------------------------------------------------------------------------

/// Profit and loss report for a given date range and currency.
class PLReport {
  const PLReport({
    required this.startDate,
    required this.endDate,
    required this.currency,
    required this.totalIncome,
    required this.totalExpenses,
    required this.netProfitLoss,
    required this.categoryBreakdowns,
  });

  factory PLReport.fromJson(Map<String, dynamic> json) => PLReport(
    startDate: json['start_date'] as String,
    endDate: json['end_date'] as String,
    currency: json['currency'] as String,
    totalIncome: (json['total_income'] as num).toDouble(),
    totalExpenses: (json['total_expenses'] as num).toDouble(),
    netProfitLoss: (json['net_profit_loss'] as num).toDouble(),
    categoryBreakdowns: (json['category_breakdowns'] as List<dynamic>)
        .map((e) => CategoryBreakdown.fromJson(e as Map<String, dynamic>))
        .toList(),
  );

  final String startDate;
  final String endDate;
  final String currency;
  final double totalIncome;
  final double totalExpenses;
  final double netProfitLoss;
  final List<CategoryBreakdown> categoryBreakdowns;
}

// ---------------------------------------------------------------------------
// Attachment models
// ---------------------------------------------------------------------------

/// A file attachment linked to an expense.
class Attachment {
  const Attachment({
    required this.id,
    required this.expenseId,
    required this.filename,
    required this.contentType,
    required this.fileSize,
    required this.createdAt,
    this.url,
  });

  factory Attachment.fromJson(Map<String, dynamic> json) => Attachment(
    id: json['id'] as String,
    expenseId: json['expense_id'] as String,
    filename: json['filename'] as String,
    contentType: json['content_type'] as String,
    fileSize: (json['file_size'] as num).toInt(),
    createdAt: json['created_at'] as String,
    url: json['url'] as String?,
  );

  final String id;
  final String expenseId;
  final String filename;
  final String contentType;
  final int fileSize;
  final String createdAt;
  final String? url;
}

// ---------------------------------------------------------------------------
// Health model
// ---------------------------------------------------------------------------

/// Backend health check response.
class HealthResponse {
  const HealthResponse({required this.status, this.version, this.timestamp});

  factory HealthResponse.fromJson(Map<String, dynamic> json) => HealthResponse(
    status: json['status'] as String,
    version: json['version'] as String?,
    timestamp: json['timestamp'] as String?,
  );

  final String status;
  final String? version;
  final String? timestamp;
}
