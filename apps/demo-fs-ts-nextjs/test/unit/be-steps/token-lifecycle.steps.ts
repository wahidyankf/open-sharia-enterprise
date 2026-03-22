import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { SignJWT } from "jose";
import { createTestContext, registerUser, loginUser, getAuth, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/apps/demo/be/gherkin/authentication/token-lifecycle.feature"),
);

const JWT_SECRET = process.env.APP_JWT_SECRET ?? "test-jwt-secret-at-least-32-chars-long!!";

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given, And }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });

    And('a user "alice" is registered with password "Str0ng#Pass1"', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });

    And('"alice" has logged in and stored the access token and refresh token', async () => {
      await loginUser(ctx, "alice", "Str0ng#Pass1");
    });
  });

  Scenario("Successful refresh returns a new access token and refresh token", ({ When, Then, And }) => {
    When("alice sends POST /api/v1/auth/refresh with her refresh token", async () => {
      const refreshToken = ctx.tokens.get("alice_refresh")!;
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/auth/refresh", { refreshToken }, null);
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the response body should contain a non-null "accessToken" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).accessToken).toBeDefined();
    });

    And('the response body should contain a non-null "refreshToken" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).refreshToken).toBeDefined();
    });
  });

  Scenario("Reject refresh with an expired refresh token", ({ Given, When, Then, And }) => {
    Given("alice's refresh token has expired", async () => {
      const secret = new TextEncoder().encode(JWT_SECRET);
      const expiredToken = await new SignJWT({ sub: ctx.userIds.get("alice"), tokenType: "refresh" })
        .setProtectedHeader({ alg: "HS256" })
        .setIssuedAt(Math.floor(Date.now() / 1000) - 7200)
        .setExpirationTime(Math.floor(Date.now() / 1000) - 3600)
        .setJti(crypto.randomUUID())
        .setIssuer("demo-fs-ts-nextjs")
        .sign(secret);
      ctx.tokens.set("alice_refresh", expiredToken);
    });

    When("alice sends POST /api/v1/auth/refresh with her refresh token", async () => {
      const refreshToken = ctx.tokens.get("alice_refresh")!;
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/auth/refresh", { refreshToken }, null);
    });

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });

    And("the response body should contain an error message about token expiration", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).length).toBeGreaterThan(0);
    });
  });

  Scenario("Original refresh token is rejected after rotation (single-use)", ({ Given, When, Then, And }) => {
    Given("alice has used her refresh token to get a new token pair", async () => {
      const refreshToken = ctx.tokens.get("alice_refresh")!;
      ctx.context.originalRefresh = refreshToken;
      const resp = await ctx.client.dispatch("POST", "/api/v1/auth/refresh", { refreshToken }, null);
      const body = resp.body as { accessToken: string; refreshToken: string };
      ctx.tokens.set("alice_access", body.accessToken);
      ctx.tokens.set("alice_refresh", body.refreshToken);
    });

    When("alice sends POST /api/v1/auth/refresh with her original refresh token", async () => {
      ctx.response = await ctx.client.dispatch(
        "POST",
        "/api/v1/auth/refresh",
        { refreshToken: ctx.context.originalRefresh as string },
        null,
      );
    });

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });

    And("the response body should contain an error message about invalid token", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).length).toBeGreaterThan(0);
    });
  });

  Scenario("Refresh fails for a deactivated user", ({ Given, When, Then, And }) => {
    Given('the user "alice" has been deactivated', async () => {
      const auth = getAuth(ctx, "alice");
      await ctx.client.dispatch("POST", "/api/v1/users/me/deactivate", null, auth);
    });

    When("alice sends POST /api/v1/auth/refresh with her refresh token", async () => {
      const refreshToken = ctx.tokens.get("alice_refresh")!;
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/auth/refresh", { refreshToken }, null);
    });

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });

    And("the response body should contain an error message about account deactivation", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("deactivat");
    });
  });

  Scenario("Logout current session invalidates the access token", ({ When, Then, And }) => {
    When("alice sends POST /api/v1/auth/logout with her access token", async () => {
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/auth/logout", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And("alice's access token should be invalidated", async () => {
      const resp = await ctx.client.dispatch("GET", "/api/v1/users/me", null, getAuth(ctx, "alice"));
      expect(resp.status).toBe(401);
    });
  });

  Scenario("Logout all devices invalidates tokens from all sessions", ({ When, Then, And }) => {
    When("alice sends POST /api/v1/auth/logout-all with her access token", async () => {
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/auth/logout-all", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And("alice's access token should be invalidated", async () => {
      const resp = await ctx.client.dispatch("GET", "/api/v1/users/me", null, getAuth(ctx, "alice"));
      expect(resp.status).toBe(401);
    });
  });

  Scenario("Logout is idempotent — repeating logout on the same token returns 200", ({ Given, When, Then }) => {
    Given("alice has already logged out once", async () => {
      await ctx.client.dispatch("POST", "/api/v1/auth/logout", null, getAuth(ctx, "alice"));
    });

    When("alice sends POST /api/v1/auth/logout with her access token", async () => {
      ctx.response = await ctx.client.dispatch("POST", "/api/v1/auth/logout", null, getAuth(ctx, "alice"));
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });
  });
});
