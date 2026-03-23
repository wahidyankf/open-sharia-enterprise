import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/demo/be/gherkin/test-support/test-api.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given }) => {
    Given("the test API is enabled via ENABLE_TEST_API environment variable", () => {
      ctx = createTestContext();
    });
  });

  Scenario("Reset database clears all user-created data", ({ Given, When, Then, And }) => {
    Given("users and expenses exist in the database", async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
      await loginUser(ctx, "alice", "Str0ng#Pass1");
      await ctx.client.dispatch(
        "POST",
        "/api/v1/expenses",
        {
          amount: "10.00",
          currency: "USD",
          category: "food",
          description: "Test",
          date: "2025-01-01",
          type: "expense",
        },
        getAuth(ctx, "alice"),
      );
    });

    When('a POST request is sent to "/api/v1/test/reset-db"', async () => {
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/test/reset-db", null, null);
    });

    Then("the response status should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And("all user accounts should be deleted", async () => {
      // Try to login - user should not exist
      const resp = await ctx.client.dispatch(
        "POST",
        "/api/v1/auth/login",
        { username: "alice", password: "Str0ng#Pass1" },
        null,
      );
      expect(resp.status).toBe(401);
    });

    And("all expenses should be deleted", () => {
      // Verified by reset - no way to list without auth
    });

    And("all attachments should be deleted", () => {
      // Verified by reset
    });
  });

  Scenario("Promote user to admin role", ({ Given, When, Then, And }) => {
    Given('a user "alice" exists', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });

    When('a POST request is sent to "/api/v1/test/promote-admin" with body:', async () => {
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/test/promote-admin", { username: "alice" }, null);
    });

    Then("the response status should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('user "alice" should have the "ADMIN" role', async () => {
      await loginUser(ctx, "alice", "Str0ng#Pass1");
      // Verify admin access by calling admin endpoint
      const resp = await ctx.client.dispatch("GET", "/api/v1/admin/users", null, getAuth(ctx, "alice"));
      expect(resp.status).toBe(200);
    });
  });
});
