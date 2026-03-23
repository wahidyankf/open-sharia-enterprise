import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/demo/be/gherkin/expenses/reporting.feature"),
);

async function createEntry(ctx: TestContext, body: Record<string, unknown>): Promise<void> {
  const resp = await ctx.client.dispatch("POST", "/api/v1/expenses", body, getAuth(ctx, "alice"));
  if (resp.status !== 201) throw new Error(`Failed: ${JSON.stringify(resp.body)}`);
}

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given, And }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });

    And('a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });

    And('"alice" has logged in and stored the access token', async () => {
      await loginUser(ctx, "alice", "Str0ng#Pass1");
    });
  });

  Scenario("P&L summary returns income total, expense total, and net for a period", ({ Given, And, When, Then }) => {
    Given(
      'alice has created an entry with body { "amount": "5000.00", "currency": "USD", "category": "salary", "description": "Monthly salary", "date": "2025-01-15", "type": "income" }',
      async () => {
        await createEntry(ctx, {
          amount: "5000.00",
          currency: "USD",
          category: "salary",
          description: "Monthly salary",
          date: "2025-01-15",
          type: "income",
        });
      },
    );

    And(
      'alice has created an entry with body { "amount": "150.00", "currency": "USD", "category": "food", "description": "Groceries", "date": "2025-01-20", "type": "expense" }',
      async () => {
        await createEntry(ctx, {
          amount: "150.00",
          currency: "USD",
          category: "food",
          description: "Groceries",
          date: "2025-01-20",
          type: "expense",
        });
      },
    );

    When("alice sends GET /api/v1/reports/pl?from=2025-01-01&to=2025-01-31&currency=USD", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        "/api/v1/reports/pl?from=2025-01-01&to=2025-01-31&currency=USD",
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "totalIncome" equal to "5000.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalIncome).toBe("5000.00");
    });

    And('the response body should contain "totalExpense" equal to "150.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalExpense).toBe("150.00");
    });

    And('the response body should contain "net" equal to "4850.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).net).toBe("4850.00");
    });
  });

  Scenario("P&L breakdown includes category-level amounts for income and expenses", ({ Given, And, When, Then }) => {
    Given(
      'alice has created an entry with body { "amount": "3000.00", "currency": "USD", "category": "salary", "description": "Salary", "date": "2025-02-10", "type": "income" }',
      async () => {
        await createEntry(ctx, {
          amount: "3000.00",
          currency: "USD",
          category: "salary",
          description: "Salary",
          date: "2025-02-10",
          type: "income",
        });
      },
    );

    And(
      'alice has created an entry with body { "amount": "500.00", "currency": "USD", "category": "freelance", "description": "Freelance project", "date": "2025-02-15", "type": "income" }',
      async () => {
        await createEntry(ctx, {
          amount: "500.00",
          currency: "USD",
          category: "freelance",
          description: "Freelance project",
          date: "2025-02-15",
          type: "income",
        });
      },
    );

    And(
      'alice has created an entry with body { "amount": "200.00", "currency": "USD", "category": "transport", "description": "Monthly pass", "date": "2025-02-05", "type": "expense" }',
      async () => {
        await createEntry(ctx, {
          amount: "200.00",
          currency: "USD",
          category: "transport",
          description: "Monthly pass",
          date: "2025-02-05",
          type: "expense",
        });
      },
    );

    When("alice sends GET /api/v1/reports/pl?from=2025-02-01&to=2025-02-28&currency=USD", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        "/api/v1/reports/pl?from=2025-02-01&to=2025-02-28&currency=USD",
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the income breakdown should contain "salary" with amount "3000.00"', () => {
      const body = ctx.response!.body as { incomeBreakdown: { category: string; total: string }[] };
      const salary = body.incomeBreakdown.find((b) => b.category === "salary");
      expect(salary).toBeDefined();
      expect(salary!.total).toBe("3000.00");
    });

    And('the income breakdown should contain "freelance" with amount "500.00"', () => {
      const body = ctx.response!.body as { incomeBreakdown: { category: string; total: string }[] };
      const freelance = body.incomeBreakdown.find((b) => b.category === "freelance");
      expect(freelance).toBeDefined();
      expect(freelance!.total).toBe("500.00");
    });

    And('the expense breakdown should contain "transport" with amount "200.00"', () => {
      const body = ctx.response!.body as { expenseBreakdown: { category: string; total: string }[] };
      const transport = body.expenseBreakdown.find((b) => b.category === "transport");
      expect(transport).toBeDefined();
      expect(transport!.total).toBe("200.00");
    });
  });

  Scenario("Income entries are excluded from expense total", ({ Given, When, Then, And }) => {
    Given(
      'alice has created an entry with body { "amount": "1000.00", "currency": "USD", "category": "salary", "description": "Bonus", "date": "2025-03-05", "type": "income" }',
      async () => {
        await createEntry(ctx, {
          amount: "1000.00",
          currency: "USD",
          category: "salary",
          description: "Bonus",
          date: "2025-03-05",
          type: "income",
        });
      },
    );

    When("alice sends GET /api/v1/reports/pl?from=2025-03-01&to=2025-03-31&currency=USD", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        "/api/v1/reports/pl?from=2025-03-01&to=2025-03-31&currency=USD",
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "totalIncome" equal to "1000.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalIncome).toBe("1000.00");
    });

    And('the response body should contain "totalExpense" equal to "0.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalExpense).toBe("0.00");
    });
  });

  Scenario("Expense entries are excluded from income total", ({ Given, When, Then, And }) => {
    Given(
      'alice has created an entry with body { "amount": "75.00", "currency": "USD", "category": "utilities", "description": "Internet bill", "date": "2025-04-10", "type": "expense" }',
      async () => {
        await createEntry(ctx, {
          amount: "75.00",
          currency: "USD",
          category: "utilities",
          description: "Internet bill",
          date: "2025-04-10",
          type: "expense",
        });
      },
    );

    When("alice sends GET /api/v1/reports/pl?from=2025-04-01&to=2025-04-30&currency=USD", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        "/api/v1/reports/pl?from=2025-04-01&to=2025-04-30&currency=USD",
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "totalIncome" equal to "0.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalIncome).toBe("0.00");
    });

    And('the response body should contain "totalExpense" equal to "75.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalExpense).toBe("75.00");
    });
  });

  Scenario("P&L summary filters by currency without cross-currency mixing", ({ Given, And, When, Then }) => {
    Given(
      'alice has created an entry with body { "amount": "1000.00", "currency": "USD", "category": "freelance", "description": "USD project", "date": "2025-05-01", "type": "income" }',
      async () => {
        await createEntry(ctx, {
          amount: "1000.00",
          currency: "USD",
          category: "freelance",
          description: "USD project",
          date: "2025-05-01",
          type: "income",
        });
      },
    );

    And(
      'alice has created an entry with body { "amount": "5000000", "currency": "IDR", "category": "freelance", "description": "IDR project", "date": "2025-05-01", "type": "income" }',
      async () => {
        await createEntry(ctx, {
          amount: "5000000",
          currency: "IDR",
          category: "freelance",
          description: "IDR project",
          date: "2025-05-01",
          type: "income",
        });
      },
    );

    When("alice sends GET /api/v1/reports/pl?from=2025-05-01&to=2025-05-31&currency=USD", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        "/api/v1/reports/pl?from=2025-05-01&to=2025-05-31&currency=USD",
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "totalIncome" equal to "1000.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalIncome).toBe("1000.00");
    });
  });

  Scenario("P&L summary for a period with no entries returns zero totals", ({ When, Then, And }) => {
    When("alice sends GET /api/v1/reports/pl?from=2099-01-01&to=2099-01-31&currency=USD", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        "/api/v1/reports/pl?from=2099-01-01&to=2099-01-31&currency=USD",
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "totalIncome" equal to "0.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalIncome).toBe("0.00");
    });

    And('the response body should contain "totalExpense" equal to "0.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalExpense).toBe("0.00");
    });

    And('the response body should contain "net" equal to "0.00"', () => {
      expect((ctx.response!.body as Record<string, unknown>).net).toBe("0.00");
    });
  });
});
