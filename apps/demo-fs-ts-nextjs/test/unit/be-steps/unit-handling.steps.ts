import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/demo/be/gherkin/expenses/unit-handling.feature"),
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

  Scenario(
    'Create expense with metric unit "liter" stores quantity and unit correctly',
    ({ Given, When, Then, And }) => {
      Given(
        'alice has created an expense with body { "amount": "75000", "currency": "IDR", "category": "fuel", "description": "Petrol", "date": "2025-01-15", "type": "expense", "quantity": 50.5, "unit": "liter" }',
        async () => {
          ctx.context.expenseId = await createExpense(ctx, {
            amount: "75000",
            currency: "IDR",
            category: "fuel",
            description: "Petrol",
            date: "2025-01-15",
            type: "expense",
            quantity: "50.5",
            unit: "liter",
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

      And('the response body should contain "quantity" equal to 50.5', () => {
        expect(parseFloat(String((ctx.response!.body as Record<string, unknown>).quantity))).toBe(50.5);
      });

      And('the response body should contain "unit" equal to "liter"', () => {
        expect((ctx.response!.body as Record<string, unknown>).unit).toBe("liter");
      });
    },
  );

  Scenario(
    'Create expense with imperial unit "gallon" stores quantity and unit correctly',
    ({ Given, When, Then, And }) => {
      Given(
        'alice has created an expense with body { "amount": "45.00", "currency": "USD", "category": "fuel", "description": "Gas", "date": "2025-01-15", "type": "expense", "quantity": 10, "unit": "gallon" }',
        async () => {
          ctx.context.expenseId = await createExpense(ctx, {
            amount: "45.00",
            currency: "USD",
            category: "fuel",
            description: "Gas",
            date: "2025-01-15",
            type: "expense",
            quantity: "10",
            unit: "gallon",
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

      And('the response body should contain "quantity" equal to 10', () => {
        expect(parseFloat(String((ctx.response!.body as Record<string, unknown>).quantity))).toBe(10);
      });

      And('the response body should contain "unit" equal to "gallon"', () => {
        expect((ctx.response!.body as Record<string, unknown>).unit).toBe("gallon");
      });
    },
  );

  Scenario("Create expense with an unsupported unit returns 400", ({ When, Then, And }) => {
    When(
      'alice sends POST /api/v1/expenses with body { "amount": "10.00", "currency": "USD", "category": "misc", "description": "Cargo", "date": "2025-01-15", "type": "expense", "quantity": 5, "unit": "fathom" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "10.00",
            currency: "USD",
            category: "misc",
            description: "Cargo",
            date: "2025-01-15",
            type: "expense",
            quantity: "5",
            unit: "fathom",
          },
          getAuth(ctx, "alice"),
        );
      },
    );

    Then("the response status code should be 400", () => {
      expect(ctx.response!.status).toBe(400);
    });

    And('the response body should contain a validation error for "unit"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("unit");
    });
  });

  Scenario("Expense without quantity and unit fields is accepted", ({ When, Then, And }) => {
    When(
      'alice sends POST /api/v1/expenses with body { "amount": "25.00", "currency": "USD", "category": "food", "description": "Dinner", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "25.00",
            currency: "USD",
            category: "food",
            description: "Dinner",
            date: "2025-01-15",
            type: "expense",
          },
          getAuth(ctx, "alice"),
        );
      },
    );

    Then("the response status code should be 201", () => {
      expect(ctx.response!.status).toBe(201);
    });

    And('the response body should contain a non-null "id" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).id).toBeDefined();
    });
  });
});
