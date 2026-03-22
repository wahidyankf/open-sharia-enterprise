import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:demo_fe_dart_flutterweb/models/expense.dart';
import 'package:flutter_test/flutter_test.dart';
import '../gherkin_helper.dart';
import '../service_client.dart';

void main() {
  late ServiceClient svc;

  setUp(() {
    svc = ServiceClient();
  });

  describeFeature(
    '../../specs/apps/demo/fe/gherkin/expenses/attachments.feature',
    (feature) {
      feature.scenario(
        'Uploading a JPEG image adds it to the attachment list',
        (s) {
          late String expenseId;
          late String attachmentId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              final expense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              expenseId = expense.id;
            },
          );

          s.when('alice opens the entry detail for "Lunch"', () async {});

          s.and(
            'alice uploads file "receipt.jpg" as an image attachment',
            () async {
              final attachment = await svc.uploadAttachment(
                expenseId,
                'receipt.jpg',
              );
              attachmentId = attachment.id;
            },
          );

          s.then(
            'the attachment list should contain "receipt.jpg"',
            () async {
              final attachments = await svc.listAttachments(expenseId);
              expect(
                attachments.any((a) => a.filename == 'receipt.jpg'),
                isTrue,
              );
            },
          );

          s.and(
            'the attachment should display as type "image/jpeg"',
            () async {
              final attachments = await svc.listAttachments(expenseId);
              final receipt = attachments.firstWhere(
                (a) => a.id == attachmentId,
              );
              expect(receipt.contentType, equals('image/jpeg'));
            },
          );
        },
      );

      feature.scenario(
        'Uploading a PDF document adds it to the attachment list',
        (s) {
          late String expenseId;
          late String attachmentId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              final expense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              expenseId = expense.id;
            },
          );

          s.when('alice opens the entry detail for "Lunch"', () async {});

          s.and(
            'alice uploads file "invoice.pdf" as a document attachment',
            () async {
              final attachment = await svc.uploadAttachment(
                expenseId,
                'invoice.pdf',
              );
              attachmentId = attachment.id;
            },
          );

          s.then(
            'the attachment list should contain "invoice.pdf"',
            () async {
              final attachments = await svc.listAttachments(expenseId);
              expect(
                attachments.any((a) => a.filename == 'invoice.pdf'),
                isTrue,
              );
            },
          );

          s.and(
            'the attachment should display as type "application/pdf"',
            () async {
              final attachments = await svc.listAttachments(expenseId);
              final invoice = attachments.firstWhere(
                (a) => a.id == attachmentId,
              );
              expect(invoice.contentType, equals('application/pdf'));
            },
          );
        },
      );

      feature.scenario(
        'Entry detail shows all uploaded attachments',
        (s) {
          late String expenseId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              final expense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              expenseId = expense.id;
            },
          );

          s.and(
            'alice has uploaded "receipt.jpg" and "invoice.pdf" to the entry',
            () async {
              await svc.uploadAttachment(expenseId, 'receipt.jpg');
              await svc.uploadAttachment(expenseId, 'invoice.pdf');
            },
          );

          s.when('alice opens the entry detail for "Lunch"', () async {});

          s.then('the attachment list should contain 2 items', () async {
            final attachments = await svc.listAttachments(expenseId);
            expect(attachments.length, equals(2));
          });

          s.and(
            'the attachment list should include "receipt.jpg"',
            () async {
              final attachments = await svc.listAttachments(expenseId);
              expect(
                attachments.any((a) => a.filename == 'receipt.jpg'),
                isTrue,
              );
            },
          );

          s.and(
            'the attachment list should include "invoice.pdf"',
            () async {
              final attachments = await svc.listAttachments(expenseId);
              expect(
                attachments.any((a) => a.filename == 'invoice.pdf'),
                isTrue,
              );
            },
          );
        },
      );

      feature.scenario('Deleting an attachment removes it from the list', (s) {
        late String expenseId;
        late String attachmentId;

        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.and(
          'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
          () async {
            final expense = await svc.createExpense(
              const CreateExpenseRequest(
                amount: '10.50',
                currency: 'USD',
                category: 'food',
                description: 'Lunch',
                date: '2025-01-15',
                type: 'expense',
              ),
            );
            expenseId = expense.id;
          },
        );

        s.and('alice has uploaded "receipt.jpg" to the entry', () async {
          final attachment = await svc.uploadAttachment(expenseId, 'receipt.jpg');
          attachmentId = attachment.id;
        });

        s.when('alice opens the entry detail for "Lunch"', () async {});

        s.and(
          'alice clicks the delete button on attachment "receipt.jpg"',
          () async {
            // No-op: button click is a UI concern.
          },
        );

        s.and('alice confirms the deletion', () async {
          await svc.deleteAttachment(expenseId, attachmentId);
        });

        s.then(
          'the attachment list should not contain "receipt.jpg"',
          () async {
            final attachments = await svc.listAttachments(expenseId);
            expect(
              attachments.any((a) => a.filename == 'receipt.jpg'),
              isFalse,
            );
          },
        );
      });

      feature.scenario(
        'Uploading an unsupported file type shows an error',
        (s) {
          late String expenseId;
          late List<dynamic> attachmentsBefore;
          ServiceError? caught;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              final expense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              expenseId = expense.id;
              attachmentsBefore = await svc.listAttachments(expenseId);
            },
          );

          s.when('alice opens the entry detail for "Lunch"', () async {});

          s.and('alice attempts to upload file "malware.exe"', () async {
            try {
              await svc.uploadAttachment(expenseId, 'malware.exe');
            } on UnsupportedFileTypeError catch (e) {
              caught = e;
            }
          });

          s.then(
            'an error message about unsupported file type should be displayed',
            () async {
              expect(caught, isA<UnsupportedFileTypeError>());
            },
          );

          s.and('the attachment list should remain unchanged', () async {
            final attachmentsAfter = await svc.listAttachments(expenseId);
            expect(attachmentsAfter.length, equals(attachmentsBefore.length));
          });
        },
      );

      feature.scenario('Uploading an oversized file shows an error', (s) {
        late String expenseId;
        late List<dynamic> attachmentsBefore;
        ServiceError? caught;

        s.given('the app is running', () async {});

        s.and(
          'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
          () async {
            svc.seedUser(
              username: 'alice',
              email: 'alice@example.com',
              password: 'Str0ng#Pass1',
            );
          },
        );

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.and(
          'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
          () async {
            final expense = await svc.createExpense(
              const CreateExpenseRequest(
                amount: '10.50',
                currency: 'USD',
                category: 'food',
                description: 'Lunch',
                date: '2025-01-15',
                type: 'expense',
              ),
            );
            expenseId = expense.id;
            attachmentsBefore = await svc.listAttachments(expenseId);
          },
        );

        s.when('alice opens the entry detail for "Lunch"', () async {});

        s.and('alice attempts to upload an oversized file', () async {
          // 11 MB exceeds the 10 MB limit.
          const oversizedBytes = 11 * 1024 * 1024;
          try {
            await svc.uploadAttachment(
              expenseId,
              'large.jpg',
              sizeBytes: oversizedBytes,
            );
          } on FileTooLargeError catch (e) {
            caught = e;
          }
        });

        s.then(
          'an error message about file size limit should be displayed',
          () async {
            expect(caught, isA<FileTooLargeError>());
          },
        );

        s.and('the attachment list should remain unchanged', () async {
          final attachmentsAfter = await svc.listAttachments(expenseId);
          expect(attachmentsAfter.length, equals(attachmentsBefore.length));
        });
      });

      feature.scenario(
        "Cannot upload attachment to another user's entry",
        (s) {
          late String bobExpenseId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
            },
          );

          s.and(
            'a user "bob" has created an entry with description "Taxi"',
            () async {
              // Set up bob and his entry while alice is logged out temporarily.
              await svc.logout();
              svc.seedUser(
                username: 'bob',
                email: 'bob@example.com',
                password: 'Str0ng#Bob1!',
              );
              await svc.login(
                const LoginRequest(username: 'bob', password: 'Str0ng#Bob1!'),
              );
              final bobExpense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '20.00',
                  currency: 'USD',
                  category: 'transport',
                  description: 'Taxi',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              bobExpenseId = bobExpense.id;
              // Log bob out and restore alice.
              await svc.logout();
              await svc.login(
                const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
              );
            },
          );

          s.when("alice navigates to bob's entry detail", () async {});

          s.then(
            'the upload attachment button should not be visible',
            () async {
              // The service enforces ownership: uploading to bob's expense
              // throws ForbiddenError, confirming the UI must hide the button.
              expect(
                () async => svc.uploadAttachment(bobExpenseId, 'receipt.jpg'),
                throwsA(isA<ForbiddenError>()),
              );
            },
          );
        },
      );

      feature.scenario(
        "Cannot view attachments on another user's entry",
        (s) {
          late String bobExpenseId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
            },
          );

          s.and(
            'a user "bob" has created an entry with description "Taxi"',
            () async {
              await svc.logout();
              svc.seedUser(
                username: 'bob',
                email: 'bob@example.com',
                password: 'Str0ng#Bob1!',
              );
              await svc.login(
                const LoginRequest(username: 'bob', password: 'Str0ng#Bob1!'),
              );
              final bobExpense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '20.00',
                  currency: 'USD',
                  category: 'transport',
                  description: 'Taxi',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              bobExpenseId = bobExpense.id;
              await svc.logout();
              await svc.login(
                const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
              );
            },
          );

          s.when("alice navigates to bob's entry detail", () async {});

          s.then('an access denied message should be displayed', () async {
            expect(
              () async => svc.listAttachments(bobExpenseId),
              throwsA(isA<ForbiddenError>()),
            );
          });
        },
      );

      feature.scenario(
        "Cannot delete attachment on another user's entry",
        (s) {
          late String bobExpenseId;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
            },
          );

          s.and(
            'a user "bob" has created an entry with an attachment',
            () async {
              await svc.logout();
              svc.seedUser(
                username: 'bob',
                email: 'bob@example.com',
                password: 'Str0ng#Bob1!',
              );
              await svc.login(
                const LoginRequest(username: 'bob', password: 'Str0ng#Bob1!'),
              );
              final bobExpense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '20.00',
                  currency: 'USD',
                  category: 'transport',
                  description: 'Taxi',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              bobExpenseId = bobExpense.id;
              await svc.uploadAttachment(bobExpenseId, 'bob-receipt.jpg');
              await svc.logout();
              await svc.login(
                const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
              );
            },
          );

          s.when("alice navigates to bob's entry detail", () async {});

          s.then(
            'the delete attachment button should not be visible',
            () async {
              // Attempting to list attachments on bob's entry throws ForbiddenError,
              // confirming the UI must hide the delete button.
              expect(
                () async => svc.listAttachments(bobExpenseId),
                throwsA(isA<ForbiddenError>()),
              );
            },
          );
        },
      );

      feature.scenario(
        'Deleting a non-existent attachment shows a not-found error',
        (s) {
          late String expenseId;
          late String attachmentId;
          ServiceError? caught;

          s.given('the app is running', () async {});

          s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
              svc.seedUser(
                username: 'alice',
                email: 'alice@example.com',
                password: 'Str0ng#Pass1',
              );
            },
          );

          s.and('alice has logged in', () async {
            await svc.login(
              const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
            );
          });

          s.and(
            'alice has created an entry with amount "10.50", currency "USD", category "food", description "Lunch", date "2025-01-15", and type "expense"',
            () async {
              final expense = await svc.createExpense(
                const CreateExpenseRequest(
                  amount: '10.50',
                  currency: 'USD',
                  category: 'food',
                  description: 'Lunch',
                  date: '2025-01-15',
                  type: 'expense',
                ),
              );
              expenseId = expense.id;
            },
          );

          s.and('alice has uploaded "receipt.jpg" to the entry', () async {
            final attachment =
                await svc.uploadAttachment(expenseId, 'receipt.jpg');
            attachmentId = attachment.id;
          });

          s.and(
            'the attachment has been deleted from another session',
            () async {
              svc.removeAttachmentDirectly(attachmentId);
            },
          );

          s.when(
            'alice clicks the delete button on attachment "receipt.jpg"',
            () async {},
          );

          s.and('alice confirms the deletion', () async {
            try {
              await svc.deleteAttachment(expenseId, attachmentId);
            } on NotFoundError catch (e) {
              caught = e;
            }
          });

          s.then(
            'an error message about attachment not found should be displayed',
            () async {
              expect(caught, isA<NotFoundError>());
            },
          );
        },
      );
    },
  );
}
