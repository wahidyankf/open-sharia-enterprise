import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/apps/demo/be/gherkin/expenses/expense-management.feature"),
);

async function createExpense(ctx: TestContext, username: string, body: Record<string, unknown>): Promise<string> {
  const resp = await ctx.client.dispatch("POST", "/api/v1/expenses", body, getAuth(ctx, username));
  if (resp.status !== 201) throw new Error(`Failed to create expense: ${JSON.stringify(resp.body)}`);
  const id = (resp.body as { id: string }).id;
  return id;
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

  Scenario("Create expense entry with amount and currency returns 201 with entry ID", ({ When, Then, And }) => {
    When(
      'alice sends POST /api/v1/expenses with body { "amount": "10.50", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "10.50",
            currency: "USD",
            category: "food",
            description: "Lunch",
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

  Scenario("Create income entry with amount and currency returns 201 with entry ID", ({ When, Then, And }) => {
    When(
      'alice sends POST /api/v1/expenses with body { "amount": "3000.00", "currency": "USD", "category": "salary", "description": "Monthly salary", "date": "2025-01-31", "type": "income" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "3000.00",
            currency: "USD",
            category: "salary",
            description: "Monthly salary",
            date: "2025-01-31",
            type: "income",
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

  Scenario(
    "Get own entry by ID returns amount, currency, category, description, date, and type",
    ({ Given, When, Then, And }) => {
      Given(
        'alice has created an entry with body { "amount": "10.50", "currency": "USD", "category": "food", "description": "Lunch", "date": "2025-01-15", "type": "expense" }',
        async () => {
          ctx.context.expenseId = await createExpense(ctx, "alice", {
            amount: "10.50",
            currency: "USD",
            category: "food",
            description: "Lunch",
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

      And('the response body should contain "category" equal to "food"', () => {
        expect((ctx.response!.body as Record<string, unknown>).category).toBe("food");
      });

      And('the response body should contain "description" equal to "Lunch"', () => {
        expect((ctx.response!.body as Record<string, unknown>).description).toBe("Lunch");
      });

      And('the response body should contain "date" equal to "2025-01-15"', () => {
        expect((ctx.response!.body as Record<string, unknown>).date).toBe("2025-01-15");
      });

      And('the response body should contain "type" equal to "expense"', () => {
        expect(String((ctx.response!.body as Record<string, unknown>).type).toLowerCase()).toBe("expense");
      });
    },
  );

  Scenario("List own entries returns a paginated response", ({ Given, When, Then, And }) => {
    Given("alice has created 3 entries", async () => {
      for (let i = 0; i < 3; i++) {
        await createExpense(ctx, "alice", {
          amount: "10.00",
          currency: "USD",
          category: "food",
          description: `Entry ${i}`,
          date: "2025-01-15",
          type: "expense",
        });
      }
    });

    When("alice sends GET /api/v1/expenses", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/api/v1/expenses", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain a non-null "content" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).content).toBeDefined();
    });

    And('the response body should contain a non-null "totalElements" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).totalElements).toBeDefined();
    });

    And('the response body should contain a non-null "page" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).page).toBeDefined();
    });
  });

  Scenario("Update an entry amount and description returns 200", ({ Given, When, Then, And }) => {
    Given(
      'alice has created an entry with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Breakfast", "date": "2025-01-10", "type": "expense" }',
      async () => {
        ctx.context.expenseId = await createExpense(ctx, "alice", {
          amount: "10.00",
          currency: "USD",
          category: "food",
          description: "Breakfast",
          date: "2025-01-10",
          type: "expense",
        });
      },
    );

    When(
      'alice sends PUT /api/v1/expenses/{expenseId} with body { "amount": "12.00", "currency": "USD", "category": "food", "description": "Updated breakfast", "date": "2025-01-10", "type": "expense" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "PUT",
          `/api/v1/expenses/${ctx.context.expenseId}`,
          {
            amount: "12.00",
            currency: "USD",
            category: "food",
            description: "Updated breakfast",
            date: "2025-01-10",
            type: "expense",
          },
          getAuth(ctx, "alice"),
        );
      },
    );

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain "amount" equal to "12.00"', () => {
      expect(String((ctx.response!.body as Record<string, unknown>).amount)).toBe("12.00");
    });

    And('the response body should contain "description" equal to "Updated breakfast"', () => {
      expect((ctx.response!.body as Record<string, unknown>).description).toBe("Updated breakfast");
    });
  });

  Scenario("Delete an entry returns 204", ({ Given, When, Then }) => {
    Given(
      'alice has created an entry with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Snack", "date": "2025-01-05", "type": "expense" }',
      async () => {
        ctx.context.expenseId = await createExpense(ctx, "alice", {
          amount: "10.00",
          currency: "USD",
          category: "food",
          description: "Snack",
          date: "2025-01-05",
          type: "expense",
        });
      },
    );

    When("alice sends DELETE /api/v1/expenses/{expenseId}", async () => {
      ctx.response = await ctx.client.dispatch(
        "DELETE",
        `/api/v1/expenses/${ctx.context.expenseId}`,
        null,
        getAuth(ctx, "alice"),
      );
    });

    Then("the response status code should be 204", () => {
      expect(ctx.response!.status).toBe(204);
    });
  });

  Scenario("Unauthenticated request to create an entry returns 401", ({ When, Then }) => {
    When(
      'the client sends POST /api/v1/expenses with body { "amount": "10.00", "currency": "USD", "category": "food", "description": "Coffee", "date": "2025-01-01", "type": "expense" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/expenses",
          {
            amount: "10.00",
            currency: "USD",
            category: "food",
            description: "Coffee",
            date: "2025-01-01",
            type: "expense",
          },
          null,
        );
      },
    );

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });
  });
});
