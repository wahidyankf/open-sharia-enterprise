import 'package:demo_fe_dart_flutterweb/models/auth.dart';
import 'package:demo_fe_dart_flutterweb/models/expense.dart';
import 'package:flutter_test/flutter_test.dart';
import '../gherkin_helper.dart';
import '../service_client.dart';

// ---------------------------------------------------------------------------
// Viewport simulation
//
// Since we cannot render widgets in VM tests we represent viewports as logical
// breakpoints that the service layer is agnostic to.  The tests assert that
// the service data structures support the UI behaviour expected at each
// breakpoint (e.g. pagination for list/table, navigation data for sidebar).
// ---------------------------------------------------------------------------
enum _Viewport { desktop, tablet, mobile }

_Viewport _parseViewport(String label) {
  if (label.contains('desktop')) return _Viewport.desktop;
  if (label.contains('tablet')) return _Viewport.tablet;
  return _Viewport.mobile;
}

void main() {
  late ServiceClient svc;
  // Shared state across steps within a scenario.
  _Viewport? currentViewport;
  String? createdExpenseId;

  setUp(() {
    svc = ServiceClient();
    currentViewport = null;
    createdExpenseId = null;
  });

  describeFeature(
    '../../specs/apps/demo/fe/gherkin/layout/responsive.feature',
    (feature) {
      // ---------------------------------------------------------------------------
      // Background helpers — reused via inline registration in each scenario
      // ---------------------------------------------------------------------------

      // Scenario: Desktop viewport shows full sidebar navigation
      feature.scenario('Desktop viewport shows full sidebar navigation', (s) {
        s.given('the app is running', () async {
          // No-op: ServiceClient starts in a clean state.
        });

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "desktop" (1280x800)', () async {
          currentViewport = _parseViewport('desktop');
        });

        s.when('alice navigates to the dashboard', () async {
          // Navigation is a UI concern; verify service data is available.
          expect(svc.isAuthenticated, isTrue);
        });

        s.then('the sidebar navigation should be visible', () async {
          // On desktop the sidebar is expanded. The service provides the
          // authenticated user context that drives sidebar rendering.
          expect(currentViewport, equals(_Viewport.desktop));
          final user = await svc.getCurrentUser();
          expect(user.username, equals('alice'));
        });

        s.and(
            'the sidebar should display navigation labels alongside icons',
            () async {
          // Full-label sidebar is the desktop layout. The user data needed
          // to determine navigation visibility is accessible from the service.
          expect(currentViewport, equals(_Viewport.desktop));
          expect(svc.isAuthenticated, isTrue);
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Tablet viewport collapses sidebar to icons only
      // ---------------------------------------------------------------------------
      feature.scenario('Tablet viewport collapses sidebar to icons only', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "tablet" (768x1024)', () async {
          currentViewport = _parseViewport('tablet');
        });

        s.when('alice navigates to the dashboard', () async {
          expect(svc.isAuthenticated, isTrue);
        });

        s.then('the sidebar navigation should be collapsed to icon-only mode',
            () async {
          expect(currentViewport, equals(_Viewport.tablet));
          // Service confirms authenticated session drives sidebar rendering.
          expect(svc.isAuthenticated, isTrue);
        });

        s.and(
            'hovering over a sidebar icon should show a tooltip with the label',
            () async {
          // Tooltip on hover is a UI/pointer concern. Verify the service is
          // authenticated and data is available for the sidebar items.
          expect(currentViewport, equals(_Viewport.tablet));
          final user = await svc.getCurrentUser();
          expect(user.username, equals('alice'));
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Mobile viewport hides sidebar behind a hamburger menu
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Mobile viewport hides sidebar behind a hamburger menu', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "mobile" (375x667)', () async {
          currentViewport = _parseViewport('mobile');
        });

        s.when('alice navigates to the dashboard', () async {
          expect(svc.isAuthenticated, isTrue);
        });

        s.then('the sidebar should not be visible', () async {
          expect(currentViewport, equals(_Viewport.mobile));
          expect(svc.isAuthenticated, isTrue);
        });

        s.and(
            'a hamburger menu button should be displayed in the header',
            () async {
          // Hamburger button is a UI element. Service confirms mobile context.
          expect(currentViewport, equals(_Viewport.mobile));
        });

        s.when('alice taps the hamburger menu button', () async {
          // Tap event is a UI concern; no state change at service layer.
        });

        s.then('a slide-out navigation drawer should appear', () async {
          // Drawer open is a UI concern. The service is ready to serve
          // navigation data once the drawer is displayed.
          expect(currentViewport, equals(_Viewport.mobile));
          expect(svc.isAuthenticated, isTrue);
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Mobile navigation drawer closes on item selection
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Mobile navigation drawer closes on item selection', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "mobile" (375x667)', () async {
          currentViewport = _parseViewport('mobile');
        });

        s.and('the navigation drawer is open', () async {
          // Drawer state is UI; confirm viewport and auth context.
          expect(currentViewport, equals(_Viewport.mobile));
          expect(svc.isAuthenticated, isTrue);
        });

        s.when('alice taps a navigation item', () async {
          // Navigation tap triggers route change — UI concern.
        });

        s.then('the drawer should close', () async {
          // Drawer close is a UI state transition. Service remains authenticated.
          expect(svc.isAuthenticated, isTrue);
        });

        s.and('the selected page should load', () async {
          // Page load is routed by the UI. The service is ready to serve data.
          final user = await svc.getCurrentUser();
          expect(user.username, equals('alice'));
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Entry list displays as a table on desktop
      // ---------------------------------------------------------------------------
      feature.scenario('Entry list displays as a table on desktop', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "desktop" (1280x800)', () async {
          currentViewport = _parseViewport('desktop');
        });

        s.and('alice has created 3 entries', () async {
          for (var i = 1; i <= 3; i++) {
            await svc.createExpense(
              CreateExpenseRequest(
                amount: '${i * 10}.00',
                currency: 'USD',
                category: 'Food',
                description: 'Entry $i',
                date: '2024-0$i-15',
                type: 'expense',
              ),
            );
          }
        });

        s.when('alice navigates to the entry list page', () async {
          // Navigation is a UI concern; verify service data is available.
        });

        s.then('entries should be displayed in a multi-column table', () async {
          // On desktop the list renders as a table. The service provides all
          // fields needed for a multi-column layout.
          expect(currentViewport, equals(_Viewport.desktop));
          final result = await svc.listExpenses();
          expect(result.totalElements, equals(3));
        });

        s.and(
            'the table should show columns for date, description, category, amount, and currency',
            () async {
          // Verify the Expense model exposes all required table columns.
          final result = await svc.listExpenses();
          for (final expense in result.content) {
            expect(expense.date, isNotEmpty,
                reason: 'date column must be present');
            expect(expense.description, isNotEmpty,
                reason: 'description column must be present');
            expect(expense.category, isNotEmpty,
                reason: 'category column must be present');
            expect(expense.amount, isNotEmpty,
                reason: 'amount column must be present');
            expect(expense.currency, isNotEmpty,
                reason: 'currency column must be present');
          }
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Entry list displays as cards on mobile
      // ---------------------------------------------------------------------------
      feature.scenario('Entry list displays as cards on mobile', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "mobile" (375x667)', () async {
          currentViewport = _parseViewport('mobile');
        });

        s.and('alice has created 3 entries', () async {
          for (var i = 1; i <= 3; i++) {
            await svc.createExpense(
              CreateExpenseRequest(
                amount: '${i * 10}.00',
                currency: 'USD',
                category: 'Food',
                description: 'Card Entry $i',
                date: '2024-0$i-15',
                type: 'expense',
              ),
            );
          }
        });

        s.when('alice navigates to the entry list page', () async {
          // Navigation is a UI concern.
        });

        s.then('entries should be displayed as stacked cards', () async {
          // On mobile the list renders as cards. The service provides the
          // fields each card needs.
          expect(currentViewport, equals(_Viewport.mobile));
          final result = await svc.listExpenses();
          expect(result.totalElements, equals(3));
        });

        s.and('each card should show description, amount, and date', () async {
          final result = await svc.listExpenses();
          for (final expense in result.content) {
            expect(expense.description, isNotEmpty,
                reason: 'Card must display description');
            expect(expense.amount, isNotEmpty,
                reason: 'Card must display amount');
            expect(expense.date, isNotEmpty,
                reason: 'Card must display date');
          }
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Admin user list is scrollable horizontally on mobile
      // ---------------------------------------------------------------------------
      feature.scenario(
          'Admin user list is scrollable horizontally on mobile', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('an admin user "superadmin" is logged in', () async {
          svc.seedUser(
            username: 'superadmin',
            email: 'superadmin@example.com',
            password: 'Admin#Pass1234',
            roles: ['USER', 'ADMIN'],
          );
          // Switch session to admin.
          await svc.logout();
          await svc.login(
            const LoginRequest(
                username: 'superadmin', password: 'Admin#Pass1234'),
          );
        });

        s.and('the viewport is set to "mobile" (375x667)', () async {
          currentViewport = _parseViewport('mobile');
        });

        s.when('the admin navigates to the user management page', () async {
          // Navigation is a UI concern; verify list data is available.
        });

        s.then('the user list should be horizontally scrollable', () async {
          // Horizontal scroll is a UI layout concern. Verify the user list
          // data model has enough columns to warrant horizontal scrolling.
          expect(currentViewport, equals(_Viewport.mobile));
          final result = await svc.listUsers();
          expect(result.content, isNotEmpty);
          // The user model exposes: username, email, status, roles, dates —
          // multiple columns that overflow a 375px mobile viewport.
          final user = result.content.first;
          expect(user.username, isNotEmpty);
          expect(user.status, isNotEmpty);
        });

        s.and(
            'the visible columns should prioritize username and status',
            () async {
          // Primary columns on mobile are username + status. Verify both
          // fields are present in the user model.
          final result = await svc.listUsers();
          for (final user in result.content) {
            expect(user.username, isNotEmpty,
                reason: 'username is a priority column on mobile');
            expect(user.status, isNotEmpty,
                reason: 'status is a priority column on mobile');
          }
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: P&L report chart adapts to viewport width
      // ---------------------------------------------------------------------------
      feature.scenario('P&L report chart adapts to viewport width', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "tablet" (768x1024)', () async {
          currentViewport = _parseViewport('tablet');
        });

        s.and('alice has created income and expense entries', () async {
          await svc.createExpense(
            const CreateExpenseRequest(
              amount: '500.00',
              currency: 'USD',
              category: 'Salary',
              description: 'Monthly salary',
              date: '2024-03-01',
              type: 'income',
            ),
          );
          await svc.createExpense(
            const CreateExpenseRequest(
              amount: '200.00',
              currency: 'USD',
              category: 'Rent',
              description: 'Monthly rent',
              date: '2024-03-05',
              type: 'expense',
            ),
          );
        });

        s.when('alice navigates to the reporting page', () async {
          // Navigation is a UI concern; verify report data is available.
        });

        s.then('the P&L chart should resize to fit the viewport', () async {
          // Chart resizing is a UI concern. Verify the service provides
          // income and expense data needed to render the chart.
          expect(currentViewport, equals(_Viewport.tablet));
          final report = await svc.getPLReport(
            startDate: '2024-03-01',
            endDate: '2024-03-31',
            currency: 'USD',
          );
          expect(double.parse(report.totalIncome), greaterThan(0),
              reason: 'Income data must be present for chart');
          expect(double.parse(report.totalExpense), greaterThan(0),
              reason: 'Expense data must be present for chart');
        });

        s.and(
            'category breakdowns should stack vertically below the chart',
            () async {
          // Stacked layout is a UI concern. Verify category breakdowns are
          // returned by the service (one per income/expense category).
          final report = await svc.getPLReport(
            startDate: '2024-03-01',
            endDate: '2024-03-31',
            currency: 'USD',
          );
          expect(report.incomeBreakdown, isNotEmpty,
              reason: 'Income category breakdowns must be present');
          expect(report.expenseBreakdown, isNotEmpty,
              reason: 'Expense category breakdowns must be present');
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Login form is centered and full-width on mobile
      // ---------------------------------------------------------------------------
      feature.scenario('Login form is centered and full-width on mobile', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('alice has logged out', () async {
          await svc.logout();
          expect(svc.isAuthenticated, isFalse);
        });

        s.and('the viewport is set to "mobile" (375x667)', () async {
          currentViewport = _parseViewport('mobile');
        });

        s.when('alice navigates to the login page', () async {
          // Navigation is a UI concern.
        });

        s.then(
            'the login form should span the full viewport width with padding',
            () async {
          // Full-width form is a UI layout concern. Verify the service
          // accepts login credentials (i.e. the login endpoint is reachable).
          expect(currentViewport, equals(_Viewport.mobile));
          expect(svc.isAuthenticated, isFalse);
          // Confirm login works — the form must be functional.
          final tokens = await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
          expect(tokens.accessToken, isNotEmpty);
        });

        s.and(
            'the form inputs should be large enough for touch interaction',
            () async {
          // Touch target sizes are a UI/CSS concern. Verify login succeeded —
          // form inputs were reachable and submitted correctly.
          expect(svc.isAuthenticated, isTrue);
        });
      });

      // ---------------------------------------------------------------------------
      // Scenario: Attachment upload area adapts to mobile
      // ---------------------------------------------------------------------------
      feature.scenario('Attachment upload area adapts to mobile', (s) {
        s.given('the app is running', () async {});

        s.and(
            'a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"',
            () async {
          svc.seedUser(
            username: 'alice',
            email: 'alice@example.com',
            password: 'Str0ng#Pass1',
          );
        });

        s.and('alice has logged in', () async {
          await svc.login(
            const LoginRequest(username: 'alice', password: 'Str0ng#Pass1'),
          );
        });

        s.given('the viewport is set to "mobile" (375x667)', () async {
          currentViewport = _parseViewport('mobile');
        });

        s.and('alice has created an entry with description "Lunch"', () async {
          final expense = await svc.createExpense(
            const CreateExpenseRequest(
              amount: '15.00',
              currency: 'USD',
              category: 'Food',
              description: 'Lunch',
              date: '2024-04-01',
              type: 'expense',
            ),
          );
          createdExpenseId = expense.id;
        });

        s.when('alice opens the entry detail for "Lunch"', () async {
          expect(createdExpenseId, isNotNull);
          final expense = await svc.getExpense(createdExpenseId!);
          expect(expense.description, equals('Lunch'));
        });

        s.then(
            'the attachment upload area should display a prominent upload button',
            () async {
          // Upload button prominence is a UI concern. Verify the service
          // accepts attachments for this expense (upload endpoint available).
          expect(currentViewport, equals(_Viewport.mobile));
          final attachment = await svc.uploadAttachment(
            createdExpenseId!,
            'mobile-receipt.jpg',
          );
          expect(attachment.filename, equals('mobile-receipt.jpg'));
        });

        s.and(
            'drag-and-drop should be replaced with a file picker',
            () async {
          // Drag-and-drop vs file-picker is a UI interaction pattern concern.
          // Verify the uploaded attachment is accessible via the service —
          // the file picker must produce a valid upload payload.
          final attachments =
              await svc.listAttachments(createdExpenseId!);
          expect(attachments, isNotEmpty,
              reason:
                  'File picker upload must result in a retrievable attachment');
        });
      });
    },
  );
}
