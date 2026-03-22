import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/apps/demo/be/gherkin/expenses/currency-handling.feature"),
);

async function createExpense(ctx: TestContext, body: Record<string, unknown>): Promise<string> {
  const resp = await ctx.client.dispatch("POST", "/api/v1/expenses", body, getAuth(ctx, "alice"));
  if (resp.status !== 201) throw new Error(`Failed: ${JSON.stringify(resp.body)}`);
  return (resp.body as { id: string }).id;
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

  Scenario("USD expense amount preserves two decimal places", ({ Given, When, Then, And }) => {
    Given(
      'alice has created an expense with body { "amount": "10.50", "currency": "USD", "category": "food", "description": "Coffee", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.context.expenseId = await createExpense(ctx, {
          amount: "10.50",
          currency: "USD",
          category: "food",
          description: "Coffee",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    When("alice sends GET /api/v1/expenses/{expenseId}", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        `/api/v1/expenses/${ctx.context.expenseId}`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "amount" equal to "10.50"', () => {
      expect(String((ctx.response!.body as Record<string, unknown>).amount)).toBe("10.50");
    });

    And('the response body should contain "currency" equal to "USD"', () => {
      expect((ctx.response!.body as Record<string, unknown>).currency).toBe("USD");
    });
  });

  Scenario("IDR expense amount is stored and returned as a whole number", ({ Given, When, Then, And }) => {
    Given(
      'alice has created an expense with body { "amount": "150000", "currency": "IDR", "category": "transport", "description": "Taxi", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.context.expenseId = await createExpense(ctx, {
          amount: "150000",
          currency: "IDR",
          category: "transport",
          description: "Taxi",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    When("alice sends GET /api/v1/expenses/{expenseId}", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        `/api/v1/expenses/${ctx.context.expenseId}`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "amount" equal to "150000"', () => {
      expect(String((ctx.response!.body as Record<string, unknown>).amount)).toBe("150000");
    });

    And('the response body should contain "currency" equal to "IDR"', () => {
      expect((ctx.response!.body as Record<string, unknown>).currency).toBe("IDR");
    });
  });

  Scenario("Unsupported currency code returns 400", ({ When, Then, And }) => {
    When(
      'alice sends POST /api/v1/expenses with body { "amount": "10.00", "currency": "EUR", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "10.00",
            currency: "EUR",
            category: "food",
            description: "Lunch",
            date: "2025-01-15",
            type: "expense",
          },
          getAuth(ctx, "alice"),
        );
      },
    );

    Then("the response status code should be 400", () => {
      expect(ctx.response!.status).toBe(400);
    });

    And('the response body should contain a validation error for "currency"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("currency");
    });
  });

  Scenario("Malformed currency code returns 400", ({ When, Then, And }) => {
    When(
      'alice sends POST /api/v1/expenses with body { "amount": "10.00", "currency": "US", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "10.00",
            currency: "US",
            category: "food",
            description: "Lunch",
            date: "2025-01-15",
            type: "expense",
          },
          getAuth(ctx, "alice"),
        );
      },
    );

    Then("the response status code should be 400", () => {
      expect(ctx.response!.status).toBe(400);
    });

    And('the response body should contain a validation error for "currency"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("currency");
    });
  });

  Scenario("Expense summary groups totals by currency without cross-currency mixing", ({ Given, When, Then, And }) => {
    Given(
      'alice has created an expense with body { "amount": "20.00", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" }',
      async () => {
        await createExpense(ctx, {
          amount: "20.00",
          currency: "USD",
          category: "food",
          description: "Lunch",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    And(
      'alice has created an expense with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Coffee", "date": "2025-01-15", "type": "expense" }',
      async () => {
        await createExpense(ctx, {
          amount: "10.00",
          currency: "USD",
          category: "food",
          description: "Coffee",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    And(
      'alice has created an expense with body { "amount": "150000", "currency": "IDR", "category": "transport", "description": "Taxi", "date": "2025-01-15", "type": "expense" }',
      async () => {
        await createExpense(ctx, {
          amount: "150000",
          currency: "IDR",
          category: "transport",
          description: "Taxi",
          date: "2025-01-15",
          type: "expense",
        });
      },
    );

    When("alice sends GET /api/v1/expenses/summary", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/api/v1/expenses/summary", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "USD" total equal to "30.00"', () => {
      const body = ctx.response!.body as { currency: string; totalExpense: string }[];
      const usd = body.find((s) => s.currency === "USD");
      expect(usd).toBeDefined();
      expect(parseFloat(usd!.totalExpense).toFixed(2)).toBe("30.00");
    });

    And('the response body should contain "IDR" total equal to "150000"', () => {
      const body = ctx.response!.body as { currency: string; totalExpense: string }[];
      const idr = body.find((s) => s.currency === "IDR");
      expect(idr).toBeDefined();
      expect(parseFloat(idr!.totalExpense).toFixed(0)).toBe("150000");
    });
  });

  Scenario("Negative amount is rejected with 400", ({ When, Then, And }) => {
    When(
      'alice sends POST /api/v1/expenses with body { "amount": "-10.00", "currency": "USD", "category": "food", "description": "Refund", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "-10.00",
            currency: "USD",
            category: "food",
            description: "Refund",
            date: "2025-01-15",
            type: "expense",
          },
          getAuth(ctx, "alice"),
        );
      },
    );

    Then("the response status code should be 400", () => {
      expect(ctx.response!.status).toBe(400);
    });

    And('the response body should contain a validation error for "amount"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("amount");
    });
  });
});
