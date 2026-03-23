import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/demo/be/gherkin/authentication/password-login.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given, And }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });

    And('a user "alice" is registered with password "Str0ng#Pass1"', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });
  });

  Scenario("Successful login returns access token and refresh token", ({ When, Then, And }) => {
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

    And('the response body should contain a non-null "refreshToken" field', () => {
      expect((ctx.response!.body as Record<string, unknown>).refreshToken).toBeDefined();
    });
  });

  Scenario('Successful login response includes token type "Bearer"', ({ When, Then, And }) => {
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

    And('the response body should contain "tokenType" equal to "Bearer"', () => {
      expect((ctx.response!.body as Record<string, unknown>).tokenType).toBe("Bearer");
    });
  });

  Scenario("Reject login with wrong password", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/login with body { "username": "alice", "password": "Wr0ngPass!" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/login",
          { username: "alice", password: "Wr0ngPass!" },
          null,
        );
      },
    );

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });

    And("the response body should contain an error message about invalid credentials", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("invalid");
    });
  });

  Scenario("Reject login for non-existent user", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/login with body { "username": "ghost", "password": "Str0ng#Pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/login",
          { username: "ghost", password: "Str0ng#Pass1" },
          null,
        );
      },
    );

    Then("the response status code should be 401", () => {
      expect(ctx.response!.status).toBe(401);
    });

    And("the response body should contain an error message about invalid credentials", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("invalid");
    });
  });

  Scenario("Reject login for deactivated account", ({ Given, When, Then, And }) => {
    Given('a user "alice" is registered and deactivated', async () => {
      await registerUser(ctx, "alice2", "alice2@example.com", "Str0ng#Pass1");
      // Deactivate: login first then deactivate
      const loginResp = await ctx.client.dispatch(
        "POST",
        "/api/v1/auth/login",
        { username: "alice", password: "Str0ng#Pass1" },
        null,
      );
      const token = (loginResp.body as { accessToken: string }).accessToken;
      await ctx.client.dispatch("POST", "/api/v1/users/me/deactivate", null, `Bearer ${token}`);
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

    And("the response body should contain an error message about account deactivation", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("deactivat");
    });
  });
});
