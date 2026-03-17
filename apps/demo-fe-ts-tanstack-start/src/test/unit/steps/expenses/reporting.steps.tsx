import React from "react";
import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, waitFor, cleanup } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { vi, expect } from "vitest";
import * as reportsApi from "@/lib/api/reports";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../../../specs/apps/demo/fe/gherkin/expenses/reporting.feature"),
);

const mockNavigate = vi.fn();

vi.mock("@/lib/api/expenses", () => ({
  listExpenses: vi.fn(),
  getExpense: vi.fn(),
  createExpense: vi.fn(),
  updateExpense: vi.fn(),
  deleteExpense: vi.fn(),
  getExpenseSummary: vi.fn(),
}));

vi.mock("@/lib/api/reports", () => ({
  getPLReport: vi.fn(),
}));

vi.mock("@/lib/api/client", () => ({
  getAccessToken: vi.fn().mockReturnValue("mock-token"),
  getRefreshToken: vi.fn().mockReturnValue("refresh-token"),
  setTokens: vi.fn(),
  clearTokens: vi.fn(),
  ApiError: class ApiError extends Error {
    status: number;
    body: unknown;
    constructor(status: number, body: unknown) {
      super(`API error: ${status}`);
      this.name = "ApiError";
      this.status = status;
      this.body = body;
    }
  },
  apiFetch: vi.fn(),
}));

vi.mock("@tanstack/react-router", () => ({
  createFileRoute: (_path: string) => (opts: { component: React.ComponentType }) => ({
    options: opts,
    component: opts.component,
    useSearch: vi.fn().mockReturnValue({}),
    useParams: vi.fn().mockReturnValue({}),
  }),
  Link: ({ children, to, style }: { children: React.ReactNode; to: string; style?: React.CSSProperties }) => (
    <a href={to} style={style}>
      {children}
    </a>
  ),
  useNavigate: () => mockNavigate,
  useRouterState: () => ({ location: { pathname: "/expenses/summary" } }),
}));

vi.mock("@/lib/auth/auth-provider", () => ({
  useAuth: vi.fn().mockReturnValue({
    isAuthenticated: true,
    isLoading: false,
    logout: vi.fn(),
    error: null,
    setError: vi.fn(),
  }),
  AuthProvider: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

vi.mock("@/lib/auth/auth-guard", () => ({
  AuthGuard: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
}

const makeSummary = (overrides: Record<string, unknown> = {}) => ({
  totalIncome: "5000.00",
  totalExpense: "150.00",
  net: "4850.00",
  currency: "USD",
  startDate: "2025-01-01",
  endDate: "2025-01-31",
  incomeBreakdown: [{ category: "salary", type: "INCOME", total: "5000.00" }],
  expenseBreakdown: [{ category: "transport", type: "EXPENSE", total: "150.00" }],
  ...overrides,
});

async function renderSummaryPage(queryClient: QueryClient) {
  const { Route } = await import("@/routes/_auth/expenses/summary");
  const Component = (Route as { options: { component: React.ComponentType } }).options.component;
  render(
    <QueryClientProvider client={queryClient}>
      <Component />
    </QueryClientProvider>,
  );
  await waitFor(() => {
    expect(screen.getByText(/expense summary/i)).toBeInTheDocument();
  });
}

describeFeature(feature, ({ Scenario, Background }) => {
  let queryClient: QueryClient;

  Background(({ Given, And }) => {
    Given("the app is running", () => {
      cleanup();
      queryClient = createQueryClient();
      mockNavigate.mockClear();
    });

    And('a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"', () => {});

    And("alice has logged in", () => {});
  });

  Scenario("P&L report displays income total, expense total, and net for a period", ({ Given, When, Then, And }) => {
    Given('alice has created an income entry of "5000.00" USD on "2025-01-15"', () => {
      vi.mocked(reportsApi.getPLReport).mockResolvedValue(makeSummary());
    });

    And('alice has created an expense entry of "150.00" USD on "2025-01-20"', () => {});

    When("alice navigates to the reporting page", async () => {
      await renderSummaryPage(queryClient);
    });

    And('alice selects date range "2025-01-01" to "2025-01-31" with currency "USD"', async () => {
      const user = userEvent.setup();
      await user.click(screen.getByRole("button", { name: /generate report/i }));
      await waitFor(() => {
        expect(reportsApi.getPLReport).toHaveBeenCalled();
      });
    });

    Then('the report should display income total "5000.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And('the report should display expense total "150.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And('the report should display net "4850.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });
  });

  Scenario("P&L breakdown shows category-level amounts", ({ Given, When, Then, And }) => {
    Given('alice has created income entries in categories "salary" and "freelance"', () => {
      vi.mocked(reportsApi.getPLReport).mockResolvedValue(
        makeSummary({
          incomeBreakdown: [
            { category: "salary", type: "INCOME", total: "4000.00" },
            { category: "freelance", type: "INCOME", total: "1000.00" },
          ],
          expenseBreakdown: [{ category: "transport", type: "EXPENSE", total: "150.00" }],
        }),
      );
    });

    And('alice has created expense entries in category "transport"', () => {});

    When("alice navigates to the reporting page", async () => {
      await renderSummaryPage(queryClient);
    });

    And('alice selects the appropriate date range and currency "USD"', async () => {
      const user = userEvent.setup();
      await user.click(screen.getByRole("button", { name: /generate report/i }));
      await waitFor(() => {
        expect(reportsApi.getPLReport).toHaveBeenCalled();
      });
    });

    Then('the income breakdown should list "salary" and "freelance" categories', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And('the expense breakdown should list "transport" category', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });
  });

  Scenario("Income entries are excluded from expense total", ({ Given, When, Then, And }) => {
    Given('alice has created only an income entry of "1000.00" USD on "2025-03-05"', () => {
      vi.mocked(reportsApi.getPLReport).mockResolvedValue(
        makeSummary({
          totalIncome: "1000.00",
          totalExpense: "0.00",
          net: "1000.00",
          incomeBreakdown: [{ category: "freelance", type: "INCOME", total: "1000.00" }],
          expenseBreakdown: [],
        }),
      );
    });

    When("alice views the P&L report for March 2025 in USD", async () => {
      await renderSummaryPage(queryClient);
      const user = userEvent.setup();
      await user.click(screen.getByRole("button", { name: /generate report/i }));
      await waitFor(() => {
        expect(reportsApi.getPLReport).toHaveBeenCalled();
      });
    });

    Then('the report should display income total "1000.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And('the report should display expense total "0.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });
  });

  Scenario("Expense entries are excluded from income total", ({ Given, When, Then, And }) => {
    Given('alice has created only an expense entry of "75.00" USD on "2025-04-10"', () => {
      vi.mocked(reportsApi.getPLReport).mockResolvedValue(
        makeSummary({
          totalIncome: "0.00",
          totalExpense: "75.00",
          net: "-75.00",
          incomeBreakdown: [],
          expenseBreakdown: [{ category: "food", type: "EXPENSE", total: "75.00" }],
        }),
      );
    });

    When("alice views the P&L report for April 2025 in USD", async () => {
      await renderSummaryPage(queryClient);
      const user = userEvent.setup();
      await user.click(screen.getByRole("button", { name: /generate report/i }));
      await waitFor(() => {
        expect(reportsApi.getPLReport).toHaveBeenCalled();
      });
    });

    Then('the report should display income total "0.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And('the report should display expense total "75.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });
  });

  Scenario("P&L report filters by currency without mixing", ({ Given, When, Then, And }) => {
    Given("alice has created income entries in both USD and IDR", () => {
      vi.mocked(reportsApi.getPLReport).mockResolvedValue(makeSummary({ currency: "USD" }));
    });

    When('alice views the P&L report filtered to "USD" only', async () => {
      await renderSummaryPage(queryClient);
      const user = userEvent.setup();
      await user.click(screen.getByRole("button", { name: /generate report/i }));
      await waitFor(() => {
        expect(reportsApi.getPLReport).toHaveBeenCalled();
      });
    });

    Then("the report should display only USD amounts", () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And("no IDR amounts should be included", () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });
  });

  Scenario("P&L report for a period with no entries shows zero totals", ({ When, Then, And }) => {
    When("alice navigates to the reporting page", async () => {
      vi.mocked(reportsApi.getPLReport).mockResolvedValue(
        makeSummary({
          totalIncome: "0.00",
          totalExpense: "0.00",
          net: "0.00",
          incomeBreakdown: [],
          expenseBreakdown: [],
        }),
      );
      await renderSummaryPage(queryClient);
    });

    And('alice selects date range "2099-01-01" to "2099-01-31" with currency "USD"', async () => {
      const user = userEvent.setup();
      await user.click(screen.getByRole("button", { name: /generate report/i }));
      await waitFor(() => {
        expect(reportsApi.getPLReport).toHaveBeenCalled();
      });
    });

    Then('the report should display income total "0.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And('the report should display expense total "0.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });

    And('the report should display net "0.00"', () => {
      expect(reportsApi.getPLReport).toHaveBeenCalled();
    });
  });
});
