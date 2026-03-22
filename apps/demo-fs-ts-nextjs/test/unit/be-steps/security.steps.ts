import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";
import { MAX_FAILED_ATTEMPTS } from "@/lib/types";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/apps/demo/be/gherkin/security/security.feature"),
);

async function failLoginAttempts(ctx: TestContext, username: string, count: number): Promise<void> {
  for (let i = 0; i < count; i++) {
    await ctx.client.dispatch("POST", "/api/v1/auth/login", { username, password: "WrongPass!123" }, null);
  }
}

async function setupAdmin(ctx: TestContext): Promise<void> {
  await registerUser(ctx, "superadmin", "superadmin@example.com", "Str0ng#Pass1");
  await ctx.client.dispatch("POST", "/api/v1/test/promote-admin", { username: "superadmin" }, null);
  await loginUser(ctx, "superadmin", "Str0ng#Pass1");
}

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });
  });

  Scenario("Reject password shorter than 12 characters", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "Short1!Ab" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "alice@example.com", password: "Short1!Ab" },
          null,
        );
      },
    );

    Then("the response status code should be 400", () => {
      expect(ctx.response!.status).toBe(400);
    });

    And('the response body should contain a validation error for "password"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("password");
    });
  });

  Scenario("Reject password with no special character", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "AllUpperCase1234" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "alice@example.com", password: "AllUpperCase1234" },
          null,
        );
      },
    );

    Then("the response status code should be 400", () => {
      expect(ctx.response!.status).toBe(400);
    });

    And('the response body should contain a validation error for "password"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("password");
    });
  });

  Scenario("Account is locked after exceeding the maximum failed login threshold", ({ Given, And, When, Then }) => {
    Given('a user "alice" is registered with password "Str0ng#Pass1"', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });

    And('"alice" has had the maximum number of failed login attempts', async () => {
      await failLoginAttempts(ctx, "alice", MAX_FAILED_ATTEMPTS);
    });

    When(
      'the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/login",
          { username: "alice", password: "Str0ng#Pass1" },
          null,
        );
      },
    );

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });

    And('alice\'s account status should be "locked"', async () => {
      await setupAdmin(ctx);
      const resp = await ctx.client.dispatch(
        "GET",
        "/api/v1/admin/users?search=alice@example.com",
        null,
        getAuth(ctx, "superadmin"),
      );
      const body = resp.body as { content: { status: string }[] };
      expect(body.content[0]!.status.toLowerCase()).toBe("locked");
    });
  });

  Scenario("Admin unlocks a locked account", ({ Given, And, When, Then }) => {
    Given('a user "alice" is registered and locked after too many failed logins', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
      await failLoginAttempts(ctx, "alice", MAX_FAILED_ATTEMPTS);
    });

    And('an admin user "superadmin" is registered and logged in', async () => {
      await setupAdmin(ctx);
    });

    When("the admin sends POST /api/v1/admin/users/{alice_id}/unlock", async () => {
      const aliceId = ctx.userIds.get("alice")!;
      ctx.response = await ctx.client.dispatch(
        "POST",
        `/api/v1/admin/users/${aliceId}/unlock`,
        null,
        getAuth(ctx, "superadmin"),
      );
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });
  });

  Scenario("Unlocked account can log in with correct password", ({ Given, And, When, Then }) => {
    Given('a user "alice" is registered and locked after too many failed logins', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
      await failLoginAttempts(ctx, "alice", MAX_FAILED_ATTEMPTS);
    });

    And("an admin has unlocked alice's account", async () => {
      await setupAdmin(ctx);
      const aliceId = ctx.userIds.get("alice")!;
      await ctx.client.dispatch("POST", `/api/v1/admin/users/${aliceId}/unlock`, null, getAuth(ctx, "superadmin"));
    });

    When(
      'the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Str0ng#Pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/login",
          { username: "alice", password: "Str0ng#Pass1" },
          null,
        );
      },
    );

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain a non-null "accessToken" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).accessToken).toBeDefined();
    });
  });
});
