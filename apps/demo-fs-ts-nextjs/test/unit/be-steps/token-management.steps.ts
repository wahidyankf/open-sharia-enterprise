import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/demo/be/gherkin/token-management/tokens.feature"),
);

async function setupAdmin(ctx: TestContext): Promise<void> {
  await registerUser(ctx, "superadmin", "superadmin@example.com", "Str0ng#Pass1");
  await ctx.client.dispatch("POST", "/api/v1/test/promote-admin", { username: "superadmin" }, null);
  await loginUser(ctx, "superadmin", "Str0ng#Pass1");
}

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;
  let decodedPayload: Record<string, unknown>;

  Background(({ Given, And }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
      decodedPayload = {};
    });

    And('a user "alice" is registered with password "Str0ng#Pass1"', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });

    And('"alice" has logged in and stored the access token', async () => {
      await loginUser(ctx, "alice", "Str0ng#Pass1");
    });
  });

  Scenario("Access token payload contains user ID claim", ({ When, Then }) => {
    When("alice decodes her access token payload", () => {
      const token = ctx.tokens.get("alice_access")!;
      const parts = token.split(".");
      decodedPayload = JSON.parse(Buffer.from(parts[1]!, "base64url").toString());
    });

    Then('the token should contain a non-null "sub" claim', () => {
      expect(decodedPayload.sub).toBeDefined();
    });
  });

  Scenario("Access token payload contains issuer claim", ({ When, Then }) => {
    When("alice decodes her access token payload", () => {
      const token = ctx.tokens.get("alice_access")!;
      const parts = token.split(".");
      decodedPayload = JSON.parse(Buffer.from(parts[1]!, "base64url").toString());
    });

    Then('the token should contain a non-null "iss" claim', () => {
      expect(decodedPayload.iss).toBeDefined();
    });
  });

  Scenario("JWKS endpoint returns the public key for token signature verification", ({ When, Then, And }) => {
    When("the client sends GET /.well-known/jwks.json", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/.well-known/jwks.json", null, null);
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain at least one key in the "keys" array', () => {
      const body = ctx.response!.body as { keys: unknown[] };
      expect(body.keys.length).toBeGreaterThanOrEqual(1);
    });
  });

  Scenario("Logout blacklists the access token", ({ When, Then, And }) => {
    When("alice sends POST /api/v1/auth/logout with her access token", async () => {
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/auth/logout", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And("alice's access token should be recorded as revoked", async () => {
      const resp = await ctx.client.dispatch("GET", "/api/v1/users/me", null, getAuth(ctx, "alice"));
      expect(resp.status).toBe(401);
    });
  });

  Scenario("Blacklisted access token is rejected with 401 on protected endpoints", ({ Given, When, Then }) => {
    Given("alice has logged out and her access token is blacklisted", async () => {
      await ctx.client.dispatch("POST", "/api/v1/auth/logout", null, getAuth(ctx, "alice"));
    });

    When("the client sends GET /api/v1/users/me with alice's access token", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/api/v1/users/me", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });
  });

  Scenario("Deactivating a user revokes all their active tokens", ({ Given, And, When, Then }) => {
    Given('an admin user "superadmin" is registered and logged in', async () => {
      await setupAdmin(ctx);
    });

    And("the admin has disabled alice's account via POST /api/v1/admin/users/{alice_id}/disable", async () => {
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
});
