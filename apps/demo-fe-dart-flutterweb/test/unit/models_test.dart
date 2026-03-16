import 'package:flutter_test/flutter_test.dart';

import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:demo_fe_dart_flutterweb/models/user.dart';
import 'package:demo_fe_dart_flutterweb/models/expense.dart';
import 'package:demo_fe_dart_flutterweb/models/health.dart';
import 'package:demo_fe_dart_flutterweb/models/token.dart';
import 'package:demo_fe_dart_flutterweb/models/report.dart';
import 'package:demo_fe_dart_flutterweb/models/attachment.dart';

void main() {
  group('AuthTokens', () {
    test('fromJson parses correctly', () {
      final json = {'accessToken': 'abc', 'refreshToken': 'def'};
      final tokens = AuthTokens.fromJson(json);
      expect(tokens.accessToken, 'abc');
      expect(tokens.refreshToken, 'def');
    });

    test('toJson serializes correctly', () {
      const tokens = AuthTokens(accessToken: 'a', refreshToken: 'b');
      final json = tokens.toJson();
      expect(json['accessToken'], 'a');
      expect(json['refreshToken'], 'b');
    });
  });

  group('LoginRequest', () {
    test('toJson serializes correctly', () {
      const req = LoginRequest(username: 'user', password: 'pass');
      final json = req.toJson();
      expect(json['username'], 'user');
      expect(json['password'], 'pass');
    });
  });

  group('RegisterRequest', () {
    test('toJson serializes correctly', () {
      const req =
          RegisterRequest(username: 'u', email: 'e@e.com', password: 'p');
      final json = req.toJson();
      expect(json['username'], 'u');
      expect(json['email'], 'e@e.com');
      expect(json['password'], 'p');
    });
  });

  group('User', () {
    test('fromJson parses correctly', () {
      final json = {
        'id': '1',
        'username': 'alice',
        'email': 'a@b.com',
        'displayName': 'Alice',
        'status': 'ACTIVE',
        'roles': ['USER'],
        'createdAt': '2024-01-01',
        'updatedAt': '2024-01-02',
      };
      final user = User.fromJson(json);
      expect(user.id, '1');
      expect(user.username, 'alice');
      expect(user.roles, ['USER']);
    });
  });

  group('UserListResponse', () {
    test('fromJson parses list correctly', () {
      final json = {
        'content': [
          {
            'id': '1',
            'username': 'alice',
            'email': 'a@b.com',
            'displayName': 'Alice',
            'status': 'ACTIVE',
            'roles': ['USER'],
            'createdAt': '2024-01-01',
            'updatedAt': '2024-01-02',
          }
        ],
        'totalElements': 1,
        'totalPages': 1,
        'page': 0,
        'size': 20,
      };
      final resp = UserListResponse.fromJson(json);
      expect(resp.content.length, 1);
      expect(resp.totalElements, 1);
    });
  });

  group('UpdateProfileRequest', () {
    test('toJson serializes correctly', () {
      const req = UpdateProfileRequest(displayName: 'Bob');
      expect(req.toJson()['displayName'], 'Bob');
    });
  });

  group('ChangePasswordRequest', () {
    test('toJson serializes correctly', () {
      const req =
          ChangePasswordRequest(oldPassword: 'old', newPassword: 'new');
      final json = req.toJson();
      expect(json['oldPassword'], 'old');
      expect(json['newPassword'], 'new');
    });
  });

  group('DisableRequest', () {
    test('toJson serializes correctly', () {
      const req = DisableRequest(reason: 'test');
      expect(req.toJson()['reason'], 'test');
    });
  });

  group('PasswordResetResponse', () {
    test('fromJson parses correctly', () {
      final resp = PasswordResetResponse.fromJson({'token': 'xyz'});
      expect(resp.token, 'xyz');
    });
  });

  group('Expense', () {
    test('fromJson parses correctly', () {
      final json = {
        'id': '1',
        'amount': '100.00',
        'currency': 'USD',
        'category': 'Food',
        'description': 'Lunch',
        'date': '2024-01-01',
        'type': 'EXPENSE',
        'quantity': 2,
        'unit': 'pcs',
        'userId': 'u1',
        'createdAt': '2024-01-01',
        'updatedAt': '2024-01-02',
      };
      final expense = Expense.fromJson(json);
      expect(expense.amount, '100.00');
      expect(expense.quantity, 2);
      expect(expense.unit, 'pcs');
    });

    test('fromJson handles null optional fields', () {
      final json = {
        'id': '1',
        'amount': '50.00',
        'currency': 'IDR',
        'category': 'Transport',
        'description': 'Bus',
        'date': '2024-01-01',
        'type': 'EXPENSE',
        'quantity': null,
        'unit': null,
        'userId': 'u1',
        'createdAt': '2024-01-01',
        'updatedAt': '2024-01-02',
      };
      final expense = Expense.fromJson(json);
      expect(expense.quantity, isNull);
      expect(expense.unit, isNull);
    });
  });

  group('CreateExpenseRequest', () {
    test('toJson includes optional fields when present', () {
      const req = CreateExpenseRequest(
        amount: '10',
        currency: 'USD',
        category: 'Food',
        description: 'Test',
        date: '2024-01-01',
        type: 'EXPENSE',
        quantity: 5,
        unit: 'kg',
      );
      final json = req.toJson();
      expect(json['quantity'], 5);
      expect(json['unit'], 'kg');
    });

    test('toJson excludes optional fields when null', () {
      const req = CreateExpenseRequest(
        amount: '10',
        currency: 'USD',
        category: 'Food',
        description: 'Test',
        date: '2024-01-01',
        type: 'EXPENSE',
      );
      final json = req.toJson();
      expect(json.containsKey('quantity'), false);
      expect(json.containsKey('unit'), false);
    });
  });

  group('UpdateExpenseRequest', () {
    test('toJson only includes non-null fields', () {
      const req = UpdateExpenseRequest(amount: '20', currency: 'IDR');
      final json = req.toJson();
      expect(json['amount'], '20');
      expect(json['currency'], 'IDR');
      expect(json.containsKey('category'), false);
    });
  });

  group('HealthResponse', () {
    test('fromJson parses correctly', () {
      final resp = HealthResponse.fromJson({'status': 'UP'});
      expect(resp.status, 'UP');
    });
  });

  group('TokenClaims', () {
    test('fromJson parses correctly', () {
      final json = {
        'sub': 'user-id',
        'iss': 'app',
        'exp': 1700000000,
        'iat': 1699999000,
        'roles': ['USER', 'ADMIN'],
      };
      final claims = TokenClaims.fromJson(json);
      expect(claims.sub, 'user-id');
      expect(claims.roles, ['USER', 'ADMIN']);
    });

    test('fromJson handles missing fields', () {
      final claims = TokenClaims.fromJson(<String, dynamic>{});
      expect(claims.sub, '');
      expect(claims.roles, isEmpty);
    });
  });

  group('JwkKey', () {
    test('fromJson parses correctly', () {
      final key = JwkKey.fromJson({
        'kty': 'RSA',
        'kid': 'key1',
        'use': 'sig',
        'n': 'abc',
        'e': 'AQAB',
      });
      expect(key.kty, 'RSA');
      expect(key.kid, 'key1');
    });
  });

  group('JwksResponse', () {
    test('fromJson parses keys list', () {
      final resp = JwksResponse.fromJson({
        'keys': [
          {'kty': 'RSA', 'kid': 'k1', 'use': 'sig', 'n': 'a', 'e': 'b'}
        ]
      });
      expect(resp.keys.length, 1);
    });
  });

  group('CategoryBreakdown', () {
    test('fromJson parses correctly', () {
      final cb = CategoryBreakdown.fromJson({
        'category': 'Food',
        'type': 'EXPENSE',
        'total': '150.00',
      });
      expect(cb.category, 'Food');
      expect(cb.total, '150.00');
    });
  });

  group('PLReport', () {
    test('fromJson parses correctly', () {
      final report = PLReport.fromJson({
        'startDate': '2024-01-01',
        'endDate': '2024-01-31',
        'currency': 'USD',
        'totalIncome': '500.00',
        'totalExpense': '300.00',
        'net': '200.00',
        'incomeBreakdown': [
          {'category': 'Salary', 'type': 'INCOME', 'total': '500.00'}
        ],
        'expenseBreakdown': [
          {'category': 'Food', 'type': 'EXPENSE', 'total': '300.00'}
        ],
      });
      expect(report.net, '200.00');
      expect(report.incomeBreakdown.length, 1);
      expect(report.expenseBreakdown.length, 1);
    });
  });

  group('Attachment', () {
    test('fromJson parses correctly', () {
      final a = Attachment.fromJson({
        'id': 'a1',
        'filename': 'test.pdf',
        'contentType': 'application/pdf',
        'size': 1024,
        'createdAt': '2024-01-01',
      });
      expect(a.filename, 'test.pdf');
      expect(a.size, 1024);
    });
  });
}
