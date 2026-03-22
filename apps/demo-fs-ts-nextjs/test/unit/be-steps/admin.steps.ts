import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/apps/demo/be/gherkin/admin/admin.feature"),
);

async function setupAdmin(ctx: TestContext): Promise<void> {
  await registerUser(ctx, "superadmin", "superadmin@example.com", "Str0ng#Pass1");
  await ctx.client.dispatch("POST", "/api/v1/test/promote-admin", { username: "superadmin" }, null);
  await loginUser(ctx, "superadmin", "Str0ng#Pass1");
}

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given, And }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });

    And('an admin user "superadmin" is registered and logged in', async () => {
      await setupAdmin(ctx);
    });

    And('users "alice", "bob", and "carol" are registered', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
      await registerUser(ctx, "bob", "bob@example.com", "Str0ng#Pass2");
      await registerUser(ctx, "carol", "carol@example.com", "Str0ng#Pass3");
    });
  });

  Scenario("List all users returns a paginated response", ({ When, Then, And }) => {
    When("the admin sends GET /api/v1/admin/users", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/api/v1/admin/users", null, getAuth(ctx, "superadmin"));
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

  Scenario("Search users by email returns matching results", ({ When, Then, And }) => {
    When("the admin sends GET /api/v1/admin/users?search=alice@example.com", async () => {
      ctx.response = await ctx.client.dispatch(
        "GET",
        "/api/v1/admin/users?search=alice@example.com",
        null,
        getAuth(ctx, "superadmin"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain at least one user with "email" equal to "alice@example.com"', () => {
      const body = ctx.response!.body as { content: { email: string }[] };
      expect(body.content.some((u) => u.email === "alice@example.com")).toBe(true);
    });
  });

  Scenario("Admin disables a user account", ({ Given, When, Then, And }) => {
    Given('"alice" has logged in and stored the access token', async () => {
      await loginUser(ctx, "alice", "Str0ng#Pass1");
    });

    When(
      'the admin sends POST /api/v1/admin/users/{alice_id}/disable with body { "reason": "Policy violation" }',
      async () => {
        const aliceId = ctx.userIds.get("alice")!;
        ctx.response = await ctx.client.dispatch(
          "POST",
          `/api/v1/admin/users/${aliceId}/disable`,
          { reason: "Policy violation" },
          getAuth(ctx, "superadmin"),
        );
      },
    );

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('alice\'s account status should be "disabled"', async () => {
      const resp = await ctx.client.dispatch(
        "GET",
        "/api/v1/admin/users?search=alice@example.com",
        null,
        getAuth(ctx, "superadmin"),
      );
      const body = resp.body as { content: { status: string }[] };
      expect(body.content[0]!.status.toLowerCase()).toBe("disabled");
    });
  });

  Scenario("Disabled user's access token is rejected with 401", ({ Given, And, When, Then }) => {
    Given('"alice" has logged in and stored the access token', async () => {
      await loginUser(ctx, "alice", "Str0ng#Pass1");
    });

    And("alice's account has been disabled by the admin", async () => {
      const aliceId = ctx.userIds.get("alice")!;
      await ctx.client.dispatch(
        "POST",
        `/api/v1/admin/users/${aliceId}/disable`,
        { reason: "Policy violation" },
        getAuth(ctx, "superadmin"),
      );
    });

    When("the client sends GET /api/v1/users/me with alice's access token", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/api/v1/users/me", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });
  });

  Scenario("Admin re-enables a disabled user account", ({ Given, When, Then, And }) => {
    Given("alice's account has been disabled", async () => {
      const aliceId = ctx.userIds.get("alice")!;
      await ctx.client.dispatch(
        "POST",
        `/api/v1/admin/users/${aliceId}/disable`,
        { reason: "Policy violation" },
        getAuth(ctx, "superadmin"),
      );
    });

    When("the admin sends POST /api/v1/admin/users/{alice_id}/enable", async () => {
      const aliceId = ctx.userIds.get("alice")!;
      ctx.response = await ctx.client.dispatch(
        "POST",
        `/api/v1/admin/users/${aliceId}/enable`,
        null,
        getAuth(ctx, "superadmin"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('alice\'s account status should be "active"', async () => {
      const resp = await ctx.client.dispatch(
        "GET",
        "/api/v1/admin/users?search=alice@example.com",
        null,
        getAuth(ctx, "superadmin"),
      );
      const body = resp.body as { content: { status: string }[] };
      expect(body.content[0]!.status.toLowerCase()).toBe("active");
    });
  });

  Scenario("Admin generates a password-reset token for a user", ({ When, Then, And }) => {
    When("the admin sends POST /api/v1/admin/users/{alice_id}/force-password-reset", async () => {
      const aliceId = ctx.userIds.get("alice")!;
      ctx.response = await ctx.client.dispatch(
        "POST",
        `/api/v1/admin/users/${aliceId}/force-password-reset`,
        null,
        getAuth(ctx, "superadmin"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain a non-null "token" field', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(body.resetToken ?? body.token).toBeDefined();
    });
  });
});
