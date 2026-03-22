/// In-memory service client for unit BDD tests.
///
/// Mirrors the application's service layer without importing any
/// `package:web` or `api_client.dart` code — making it safe to run under
/// the Dart VM (`flutter test test/unit`).
///
/// All business rules implemented here must match the backend contract:
/// - Password: ≥12 chars, requires uppercase + lowercase + digit + special char.
/// - Account lockout: after 5 consecutive failed login attempts.
/// - Token rotation: each refresh invalidates the previous refresh token.
/// - Admin operations require the `ADMIN` role.
/// - Expense/user pagination uses 0-based page index, default size 20.
library;

import 'package:demo_fe_dart_flutterweb/models/attachment.dart';
import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:demo_fe_dart_flutterweb/models/expense.dart';
import 'package:demo_fe_dart_flutterweb/models/health.dart';
import 'package:demo_fe_dart_flutterweb/models/report.dart';
import 'package:demo_fe_dart_flutterweb/models/token.dart';
import 'package:demo_fe_dart_flutterweb/models/user.dart';

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

/// Base class for all service errors (mirrors HTTP error semantics).
sealed class ServiceError implements Exception {
  final String message;

  const ServiceError(this.message);

  @override
  String toString() => '$runtimeType: $message';
}

final class UnauthorizedError extends ServiceError {
  const UnauthorizedError([super.message = 'Invalid credentials']);
}

final class ForbiddenError extends ServiceError {
  const ForbiddenError([super.message = 'Forbidden']);
}

final class NotFoundError extends ServiceError {
  const NotFoundError([super.message = 'Not found']);
}

final class ConflictError extends ServiceError {
  const ConflictError([super.message = 'Conflict']);
}

final class ValidationError extends ServiceError {
  const ValidationError([super.message = 'Validation failed']);
}

final class AccountInactiveError extends ServiceError {
  const AccountInactiveError([super.message = 'Account is inactive']);
}

final class AccountDisabledError extends ServiceError {
  const AccountDisabledError([super.message = 'Account is disabled']);
}

final class AccountLockedError extends ServiceError {
  const AccountLockedError([super.message = 'Account is locked']);
}

final class TokenExpiredError extends ServiceError {
  const TokenExpiredError([super.message = 'Token has expired']);
}

final class UnsupportedFileTypeError extends ServiceError {
  const UnsupportedFileTypeError([super.message = 'Unsupported file type']);
}

final class FileTooLargeError extends ServiceError {
  const FileTooLargeError([super.message = 'File exceeds size limit']);
}

// ---------------------------------------------------------------------------
// Internal state models
// ---------------------------------------------------------------------------

class _UserRecord {
  final String id;
  final String username;
  final String email;
  String displayName;
  String status; // ACTIVE | INACTIVE | DISABLED | LOCKED
  final List<String> roles;
  final DateTime createdAt;
  DateTime updatedAt;
  int failedLoginAttempts = 0;

  _UserRecord({
    required this.id,
    required this.username,
    required this.email,
    required this.displayName,
    required this.status,
    required this.roles,
    required this.createdAt,
    required this.updatedAt,
  });

  User toModel() => User(
        id: id,
        username: username,
        email: email,
        displayName: displayName,
        status: status,
        roles: List.unmodifiable(roles),
        createdAt: createdAt.toIso8601String(),
        updatedAt: updatedAt.toIso8601String(),
      );
}

class _ExpenseRecord {
  final String id;
  String amount;
  String currency;
  String category;
  String description;
  String date;
  String type;
  num? quantity;
  String? unit;
  final String userId;
  final DateTime createdAt;
  DateTime updatedAt;

  _ExpenseRecord({
    required this.id,
    required this.amount,
    required this.currency,
    required this.category,
    required this.description,
    required this.date,
    required this.type,
    this.quantity,
    this.unit,
    required this.userId,
    required this.createdAt,
    required this.updatedAt,
  });

  Expense toModel() => Expense(
        id: id,
        amount: amount,
        currency: currency,
        category: category,
        description: description,
        date: date,
        type: type,
        quantity: quantity,
        unit: unit,
        userId: userId,
        createdAt: createdAt.toIso8601String(),
        updatedAt: updatedAt.toIso8601String(),
      );
}

class _AttachmentRecord {
  final String id;
  final String expenseId;
  final String filename;
  final String contentType;
  final int size;
  final DateTime createdAt;

  _AttachmentRecord({
    required this.id,
    required this.expenseId,
    required this.filename,
    required this.contentType,
    required this.size,
    required this.createdAt,
  });

  Attachment toModel() => Attachment(
        id: id,
        filename: filename,
        contentType: contentType,
        size: size,
        createdAt: createdAt.toIso8601String(),
      );
}

class _TokenPair {
  final String accessToken;
  final String refreshToken;

  _TokenPair(this.accessToken, this.refreshToken);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const _maxLoginAttempts = 5;
const _maxFileSizeBytes = 10 * 1024 * 1024; // 10 MB

final _allowedContentTypes = {
  'image/jpeg',
  'image/png',
  'image/gif',
  'application/pdf',
};

final _allowedExtensions = {
  '.jpg',
  '.jpeg',
  '.png',
  '.gif',
  '.pdf',
};

bool _isValidPassword(String password) {
  if (password.length < 12) return false;
  if (!password.contains(RegExp('[A-Z]'))) return false;
  if (!password.contains(RegExp('[a-z]'))) return false;
  if (!password.contains(RegExp('[0-9]'))) return false;
  if (!password.contains(RegExp(r'[!@#$%^&*()_+\-=\[\]{};:"\\|,.<>/?`~]'))) {
    return false;
  }
  return true;
}

bool _isValidEmail(String email) {
  return email.contains('@') && email.contains('.');
}

String _ext(String filename) {
  final dot = filename.lastIndexOf('.');
  if (dot < 0) return '';
  return filename.substring(dot).toLowerCase();
}

String _contentTypeForFilename(String filename) {
  switch (_ext(filename)) {
    case '.jpg':
    case '.jpeg':
      return 'image/jpeg';
    case '.png':
      return 'image/png';
    case '.gif':
      return 'image/gif';
    case '.pdf':
      return 'application/pdf';
    default:
      return 'application/octet-stream';
  }
}

int _idCounter = 0;
String _nextId() => (++_idCounter).toString();

String _makeAccessToken(String userId, List<String> roles) =>
    'access.$userId.${roles.join("+")}.${_nextId()}';

String _makeRefreshToken(String userId) => 'refresh.$userId.${_nextId()}';

// ---------------------------------------------------------------------------
// ServiceClient
// ---------------------------------------------------------------------------

/// In-memory implementation of all demo app services.
///
/// Call [reset] between scenarios to start from a clean state.
class ServiceClient {
  // State maps
  final _users = <String, _UserRecord>{}; // keyed by username
  final _passwords = <String, String>{}; // keyed by username (plain text)
  final _expenses = <String, _ExpenseRecord>{}; // keyed by expense ID
  final _attachments = <String, _AttachmentRecord>{}; // keyed by attachment ID

  // Active sessions: refreshToken → userId
  final _validRefreshTokens = <String, String>{};

  // Blacklisted refresh tokens (after use or logout)
  final _usedRefreshTokens = <String>{};

  // Current authenticated session (simulates the client-side token store)
  _TokenPair? _currentSession;
  String? _currentUserId;

  // ---------------------------------------------------------------------------
  // Lifecycle
  // ---------------------------------------------------------------------------

  /// Clears all in-memory state.
  ///
  /// Call this in a test [setUp] or between Gherkin scenarios.
  void reset() {
    _users.clear();
    _passwords.clear();
    _expenses.clear();
    _attachments.clear();
    _validRefreshTokens.clear();
    _usedRefreshTokens.clear();
    _currentSession = null;
    _currentUserId = null;
  }

  // ---------------------------------------------------------------------------
  // Health
  // ---------------------------------------------------------------------------

  Future<HealthResponse> getHealth() async {
    return const HealthResponse(status: 'UP');
  }

  // ---------------------------------------------------------------------------
  // Auth
  // ---------------------------------------------------------------------------

  Future<void> register(RegisterRequest req) async {
    if (req.username.isEmpty) {
      throw const ValidationError('Username is required');
    }
    if (!_isValidEmail(req.email)) {
      throw const ValidationError('Invalid email format');
    }
    if (req.password.isEmpty) {
      throw const ValidationError('Password is required');
    }
    if (!_isValidPassword(req.password)) {
      throw const ValidationError(
        'Password must be at least 12 characters with uppercase, lowercase, digit, and special character',
      );
    }
    if (_users.containsKey(req.username)) {
      throw const ConflictError('Username already exists');
    }

    final now = DateTime.now();
    final id = _nextId();
    _users[req.username] = _UserRecord(
      id: id,
      username: req.username,
      email: req.email,
      displayName: req.username,
      status: 'ACTIVE',
      roles: ['USER'],
      createdAt: now,
      updatedAt: now,
    );
    _passwords[req.username] = req.password;
  }

  Future<AuthTokens> login(LoginRequest req) async {
    final user = _users[req.username];
    if (user == null) {
      throw const UnauthorizedError('Invalid credentials');
    }

    if (user.status == 'DISABLED') {
      throw const AccountDisabledError();
    }
    if (user.status == 'LOCKED') {
      throw const AccountLockedError();
    }
    if (user.status == 'INACTIVE') {
      throw const AccountInactiveError();
    }

    if (_passwords[req.username] != req.password) {
      user.failedLoginAttempts++;
      if (user.failedLoginAttempts >= _maxLoginAttempts) {
        user.status = 'LOCKED';
        throw const AccountLockedError();
      }
      throw const UnauthorizedError('Invalid credentials');
    }

    // Reset failed attempts on successful login.
    user.failedLoginAttempts = 0;

    final accessToken = _makeAccessToken(user.id, user.roles);
    final refreshToken = _makeRefreshToken(user.id);

    _validRefreshTokens[refreshToken] = user.id;
    _currentSession = _TokenPair(accessToken, refreshToken);
    _currentUserId = user.id;

    return AuthTokens(accessToken: accessToken, refreshToken: refreshToken);
  }

  Future<AuthTokens> refreshToken(String oldRefreshToken) async {
    if (_usedRefreshTokens.contains(oldRefreshToken)) {
      // Token reuse — invalidate session.
      _currentSession = null;
      _currentUserId = null;
      throw const TokenExpiredError('Token has been revoked');
    }

    final userId = _validRefreshTokens[oldRefreshToken];
    if (userId == null) {
      _currentSession = null;
      _currentUserId = null;
      throw const TokenExpiredError('Refresh token not found or expired');
    }

    // Find user record by ID.
    final user = _users.values.where((u) => u.id == userId).firstOrNull;
    if (user == null) {
      throw const UnauthorizedError('User not found');
    }
    if (user.status != 'ACTIVE') {
      _currentSession = null;
      _currentUserId = null;
      if (user.status == 'DISABLED') throw const AccountDisabledError();
      if (user.status == 'LOCKED') throw const AccountLockedError();
      throw const AccountInactiveError();
    }

    // Rotate token: mark old as used and issue a new pair.
    _usedRefreshTokens.add(oldRefreshToken);
    _validRefreshTokens.remove(oldRefreshToken);

    final newAccess = _makeAccessToken(user.id, user.roles);
    final newRefresh = _makeRefreshToken(user.id);

    _validRefreshTokens[newRefresh] = user.id;
    _currentSession = _TokenPair(newAccess, newRefresh);
    _currentUserId = user.id;

    return AuthTokens(accessToken: newAccess, refreshToken: newRefresh);
  }

  Future<void> logout() async {
    final session = _currentSession;
    if (session != null) {
      _usedRefreshTokens.add(session.refreshToken);
      _validRefreshTokens.remove(session.refreshToken);
    }
    _currentSession = null;
    _currentUserId = null;
  }

  Future<void> logoutAll() async {
    final userId = _currentUserId;
    if (userId != null) {
      // Revoke all refresh tokens belonging to this user.
      final toRevoke = _validRefreshTokens.entries
          .where((e) => e.value == userId)
          .map((e) => e.key)
          .toList();
      for (final token in toRevoke) {
        _usedRefreshTokens.add(token);
        _validRefreshTokens.remove(token);
      }
    }
    _currentSession = null;
    _currentUserId = null;
  }

  // ---------------------------------------------------------------------------
  // Session helpers
  // ---------------------------------------------------------------------------

  /// Returns `true` when a session is currently active.
  bool get isAuthenticated => _currentSession != null;

  /// Returns the current access token or `null`.
  String? get currentAccessToken => _currentSession?.accessToken;

  /// Returns the current refresh token or `null`.
  String? get currentRefreshToken => _currentSession?.refreshToken;

  /// Returns the logged-in user record or `null`.
  _UserRecord? get _currentUser => _currentUserId == null
      ? null
      : _users.values.where((u) => u.id == _currentUserId).firstOrNull;

  void _requireAuth() {
    if (_currentUserId == null) throw const UnauthorizedError('Not authenticated');
    final user = _currentUser;
    if (user == null) throw const UnauthorizedError('User not found');
    if (user.status == 'INACTIVE') throw const AccountInactiveError();
    if (user.status == 'DISABLED') throw const AccountDisabledError();
    if (user.status == 'LOCKED') throw const AccountLockedError();
    if (user.status == 'INACTIVE') throw const AccountInactiveError();
  }

  void _requireAdmin() {
    _requireAuth();
    final user = _currentUser!;
    if (!user.roles.contains('ADMIN')) throw const ForbiddenError();
  }

  // ---------------------------------------------------------------------------
  // Users
  // ---------------------------------------------------------------------------

  Future<User> getCurrentUser() async {
    _requireAuth();
    return _currentUser!.toModel();
  }

  Future<User> updateProfile(UpdateProfileRequest req) async {
    _requireAuth();
    final user = _currentUser!;
    user.displayName = req.displayName;
    user.updatedAt = DateTime.now();
    return user.toModel();
  }

  Future<void> changePassword(ChangePasswordRequest req) async {
    _requireAuth();
    final user = _currentUser!;
    if (_passwords[user.username] != req.oldPassword) {
      throw const UnauthorizedError('Invalid credentials');
    }
    if (!_isValidPassword(req.newPassword)) {
      throw const ValidationError(
        'New password does not meet complexity requirements',
      );
    }
    _passwords[user.username] = req.newPassword;
    user.updatedAt = DateTime.now();
  }

  Future<void> deactivateAccount() async {
    _requireAuth();
    final user = _currentUser!;
    user.status = 'INACTIVE';
    user.updatedAt = DateTime.now();
    await logout();
  }

  // ---------------------------------------------------------------------------
  // Admin: user management
  // ---------------------------------------------------------------------------

  Future<UserListResponse> listUsers({int page = 0, int size = 20, String? search}) async {
    _requireAdmin();
    var all = _users.values.toList();
    if (search != null && search.isNotEmpty) {
      final q = search.toLowerCase();
      all = all.where((u) {
        return u.username.toLowerCase().contains(q) ||
            u.email.toLowerCase().contains(q);
      }).toList();
    }
    final total = all.length;
    final totalPages = total == 0 ? 0 : (total / size).ceil();
    final start = page * size;
    final slice = start >= total ? <_UserRecord>[] : all.skip(start).take(size).toList();
    return UserListResponse(
      content: slice.map((u) => u.toModel()).toList(),
      totalElements: total,
      totalPages: totalPages,
      page: page,
      size: size,
    );
  }

  Future<User> getUserById(String userId) async {
    _requireAdmin();
    final user = _users.values.where((u) => u.id == userId).firstOrNull;
    if (user == null) throw const NotFoundError('User not found');
    return user.toModel();
  }

  Future<User> disableUser(String userId, DisableRequest req) async {
    _requireAdmin();
    final user = _users.values.where((u) => u.id == userId).firstOrNull;
    if (user == null) throw const NotFoundError('User not found');
    user.status = 'DISABLED';
    user.updatedAt = DateTime.now();
    return user.toModel();
  }

  Future<User> enableUser(String userId) async {
    _requireAdmin();
    final user = _users.values.where((u) => u.id == userId).firstOrNull;
    if (user == null) throw const NotFoundError('User not found');
    user.status = 'ACTIVE';
    user.updatedAt = DateTime.now();
    return user.toModel();
  }

  Future<User> unlockUser(String userId) async {
    _requireAdmin();
    final user = _users.values.where((u) => u.id == userId).firstOrNull;
    if (user == null) throw const NotFoundError('User not found');
    user.status = 'ACTIVE';
    user.failedLoginAttempts = 0;
    user.updatedAt = DateTime.now();
    return user.toModel();
  }

  Future<PasswordResetResponse> forcePasswordReset(String userId) async {
    _requireAdmin();
    final user = _users.values.where((u) => u.id == userId).firstOrNull;
    if (user == null) throw const NotFoundError('User not found');
    // Generate a token that the admin can share with the user.
    final token = 'reset-token-${user.id}-${_nextId()}';
    return PasswordResetResponse(token: token);
  }

  // ---------------------------------------------------------------------------
  // Expenses
  // ---------------------------------------------------------------------------

  Future<ExpenseListResponse> listExpenses({
    int page = 0,
    int size = 20,
    String? currency,
  }) async {
    _requireAuth();
    final userId = _currentUserId!;
    var all = _expenses.values.where((e) => e.userId == userId).toList();
    if (currency != null && currency.isNotEmpty) {
      all = all.where((e) => e.currency == currency).toList();
    }
    // Sort by date descending, then by createdAt descending for stability.
    all.sort((a, b) {
      final dateCmp = b.date.compareTo(a.date);
      if (dateCmp != 0) return dateCmp;
      return b.createdAt.compareTo(a.createdAt);
    });
    final total = all.length;
    final totalPages = total == 0 ? 0 : (total / size).ceil();
    final start = page * size;
    final slice = start >= total ? <_ExpenseRecord>[] : all.skip(start).take(size).toList();
    return ExpenseListResponse(
      content: slice.map((e) => e.toModel()).toList(),
      totalElements: total,
      totalPages: totalPages,
      page: page,
      size: size,
    );
  }

  Future<Expense> getExpense(String id) async {
    _requireAuth();
    final expense = _expenses[id];
    if (expense == null) throw const NotFoundError('Expense not found');
    if (expense.userId != _currentUserId) throw const ForbiddenError();
    return expense.toModel();
  }

  Future<Expense> createExpense(CreateExpenseRequest req) async {
    _requireAuth();
    final now = DateTime.now();
    final id = _nextId();
    final record = _ExpenseRecord(
      id: id,
      amount: req.amount,
      currency: req.currency,
      category: req.category,
      description: req.description,
      date: req.date,
      type: req.type,
      quantity: req.quantity,
      unit: req.unit,
      userId: _currentUserId!,
      createdAt: now,
      updatedAt: now,
    );
    _expenses[id] = record;
    return record.toModel();
  }

  Future<Expense> updateExpense(String id, UpdateExpenseRequest req) async {
    _requireAuth();
    final record = _expenses[id];
    if (record == null) throw const NotFoundError('Expense not found');
    if (record.userId != _currentUserId) throw const ForbiddenError();

    if (req.amount != null) record.amount = req.amount!;
    if (req.currency != null) record.currency = req.currency!;
    if (req.category != null) record.category = req.category!;
    if (req.description != null) record.description = req.description!;
    if (req.date != null) record.date = req.date!;
    if (req.type != null) record.type = req.type!;
    if (req.quantity != null) record.quantity = req.quantity;
    if (req.unit != null) record.unit = req.unit;
    record.updatedAt = DateTime.now();
    return record.toModel();
  }

  Future<void> deleteExpense(String id) async {
    _requireAuth();
    final record = _expenses[id];
    if (record == null) throw const NotFoundError('Expense not found');
    if (record.userId != _currentUserId) throw const ForbiddenError();
    _expenses.remove(id);
    // Also remove associated attachments.
    _attachments.removeWhere((_, a) => a.expenseId == id);
  }

  Future<ExpenseSummary> getExpenseSummary({
    required String currency,
    String? startDate,
    String? endDate,
  }) async {
    _requireAuth();
    final userId = _currentUserId!;
    var all = _expenses.values.where((e) => e.userId == userId && e.currency == currency).toList();
    if (startDate != null) {
      all = all.where((e) => e.date.compareTo(startDate) >= 0).toList();
    }
    if (endDate != null) {
      all = all.where((e) => e.date.compareTo(endDate) <= 0).toList();
    }

    var totalIncome = 0.0;
    var totalExpense = 0.0;
    final categoryMap = <String, Map<String, double>>{};

    for (final e in all) {
      final amount = double.tryParse(e.amount) ?? 0.0;
      final typeKey = e.type.toLowerCase();
      if (typeKey == 'income') {
        totalIncome += amount;
      } else {
        totalExpense += amount;
      }
      categoryMap.putIfAbsent(e.category, () => {})[e.type] =
          (categoryMap[e.category]![e.type] ?? 0.0) + amount;
    }

    final categories = categoryMap.entries.expand((catEntry) {
      return catEntry.value.entries.map(
        (typeEntry) => CategoryBreakdown(
          category: catEntry.key,
          type: typeEntry.key,
          total: typeEntry.value.toStringAsFixed(2),
        ),
      );
    }).toList();

    return ExpenseSummary(
      currency: currency,
      totalIncome: totalIncome.toStringAsFixed(2),
      totalExpense: totalExpense.toStringAsFixed(2),
      net: (totalIncome - totalExpense).toStringAsFixed(2),
      categories: categories,
    );
  }

  // ---------------------------------------------------------------------------
  // Attachments
  // ---------------------------------------------------------------------------

  Future<List<Attachment>> listAttachments(String expenseId) async {
    _requireAuth();
    final expense = _expenses[expenseId];
    if (expense == null) throw const NotFoundError('Expense not found');
    if (expense.userId != _currentUserId) throw const ForbiddenError();
    return _attachments.values
        .where((a) => a.expenseId == expenseId)
        .map((a) => a.toModel())
        .toList();
  }

  /// Simulates a file upload.
  ///
  /// Pass [contentType] explicitly, or the client will infer it from
  /// [filename]. Pass [sizeBytes] to simulate an oversized upload.
  Future<Attachment> uploadAttachment(
    String expenseId,
    String filename, {
    String? contentType,
    int sizeBytes = 1024,
  }) async {
    _requireAuth();
    final expense = _expenses[expenseId];
    if (expense == null) throw const NotFoundError('Expense not found');
    if (expense.userId != _currentUserId) throw const ForbiddenError();

    if (sizeBytes > _maxFileSizeBytes) {
      throw const FileTooLargeError();
    }

    final ext = _ext(filename);
    final ct = contentType ?? _contentTypeForFilename(filename);

    if (!_allowedExtensions.contains(ext) || !_allowedContentTypes.contains(ct)) {
      throw const UnsupportedFileTypeError();
    }

    final now = DateTime.now();
    final id = _nextId();
    final record = _AttachmentRecord(
      id: id,
      expenseId: expenseId,
      filename: filename,
      contentType: ct,
      size: sizeBytes,
      createdAt: now,
    );
    _attachments[id] = record;
    return record.toModel();
  }

  Future<void> deleteAttachment(String expenseId, String attachmentId) async {
    _requireAuth();
    final expense = _expenses[expenseId];
    if (expense == null) throw const NotFoundError('Expense not found');
    if (expense.userId != _currentUserId) throw const ForbiddenError();

    final attachment = _attachments[attachmentId];
    if (attachment == null) throw const NotFoundError('Attachment not found');
    if (attachment.expenseId != expenseId) throw const NotFoundError('Attachment not found');

    _attachments.remove(attachmentId);
  }

  // ---------------------------------------------------------------------------
  // Reports
  // ---------------------------------------------------------------------------

  Future<PLReport> getPLReport({
    required String startDate,
    required String endDate,
    required String currency,
  }) async {
    _requireAuth();
    final userId = _currentUserId!;
    final all = _expenses.values
        .where(
          (e) =>
              e.userId == userId &&
              e.currency == currency &&
              e.date.compareTo(startDate) >= 0 &&
              e.date.compareTo(endDate) <= 0,
        )
        .toList();

    var totalIncome = 0.0;
    var totalExpense = 0.0;
    final incomeByCategory = <String, double>{};
    final expenseByCategory = <String, double>{};

    for (final e in all) {
      final amount = double.tryParse(e.amount) ?? 0.0;
      if (e.type.toLowerCase() == 'income') {
        totalIncome += amount;
        incomeByCategory[e.category] =
            (incomeByCategory[e.category] ?? 0.0) + amount;
      } else {
        totalExpense += amount;
        expenseByCategory[e.category] =
            (expenseByCategory[e.category] ?? 0.0) + amount;
      }
    }

    final incomeBreakdown = incomeByCategory.entries
        .map(
          (e) => CategoryBreakdown(
            category: e.key,
            type: 'income',
            total: e.value.toStringAsFixed(2),
          ),
        )
        .toList();

    final expenseBreakdown = expenseByCategory.entries
        .map(
          (e) => CategoryBreakdown(
            category: e.key,
            type: 'expense',
            total: e.value.toStringAsFixed(2),
          ),
        )
        .toList();

    return PLReport(
      startDate: startDate,
      endDate: endDate,
      currency: currency,
      totalIncome: totalIncome.toStringAsFixed(2),
      totalExpense: totalExpense.toStringAsFixed(2),
      net: (totalIncome - totalExpense).toStringAsFixed(2),
      incomeBreakdown: incomeBreakdown,
      expenseBreakdown: expenseBreakdown,
    );
  }

  // ---------------------------------------------------------------------------
  // Tokens / JWKS
  // ---------------------------------------------------------------------------

  /// Returns a minimal JWKS response (in-memory stub — no real RSA key).
  Future<JwksResponse> getJwks() async {
    return const JwksResponse(
      keys: [
        JwkKey(
          kty: 'RSA',
          kid: 'test-key-1',
          use: 'sig',
          n: 'test-modulus',
          e: 'AQAB',
        ),
      ],
    );
  }

  /// Decodes the claims encoded in an access token produced by [login].
  ///
  /// This is a no-network stub — it parses the synthetic token format
  /// `access.<userId>.<roles>` created by [_makeAccessToken].
  Future<TokenClaims> decodeTokenClaims(String accessToken) async {
    if (!accessToken.startsWith('access.')) {
      throw const UnauthorizedError('Invalid token format');
    }
    final parts = accessToken.split('.');
    if (parts.length < 3) throw const UnauthorizedError('Invalid token');
    final userId = parts[1];
    final roles = parts[2].split('+');
    final now = DateTime.now().millisecondsSinceEpoch ~/ 1000;
    return TokenClaims(
      sub: userId,
      iss: 'demo-service-client',
      exp: now + 3600,
      iat: now,
      roles: roles,
    );
  }

  // ---------------------------------------------------------------------------
  // Test helpers — not part of the app API
  // ---------------------------------------------------------------------------

  /// Creates a user directly without going through the registration flow.
  ///
  /// Useful for setting up admin users or users with specific roles/status
  /// in Background steps.
  void seedUser({
    required String username,
    required String email,
    required String password,
    String displayName = '',
    String status = 'ACTIVE',
    List<String> roles = const ['USER'],
  }) {
    final now = DateTime.now();
    final id = _nextId();
    _users[username] = _UserRecord(
      id: id,
      username: username,
      email: email,
      displayName: displayName.isNotEmpty ? displayName : username,
      status: status,
      roles: List<String>.from(roles),
      createdAt: now,
      updatedAt: now,
    );
    _passwords[username] = password;
  }

  /// Returns the [User] record for [username], or throws [NotFoundError].
  User getUserByUsername(String username) {
    final record = _users[username];
    if (record == null) throw const NotFoundError('User not found');
    return record.toModel();
  }

  /// Returns all expenses owned by [username] (for assertion in tests).
  List<Expense> getExpensesForUser(String username) {
    final user = _users[username];
    if (user == null) return [];
    return _expenses.values
        .where((e) => e.userId == user.id)
        .map((e) => e.toModel())
        .toList();
  }

  /// Returns all attachments for [expenseId] without auth checks.
  List<Attachment> getAttachmentsForExpense(String expenseId) {
    return _attachments.values
        .where((a) => a.expenseId == expenseId)
        .map((a) => a.toModel())
        .toList();
  }

  /// Removes an attachment directly (simulates an out-of-band deletion for
  /// the "deleted from another session" scenario).
  void removeAttachmentDirectly(String attachmentId) {
    _attachments.remove(attachmentId);
  }
}
