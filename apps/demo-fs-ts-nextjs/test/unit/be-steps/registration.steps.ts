import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, registerUser, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/apps/demo/be/gherkin/user-lifecycle/registration.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });
  });

  Scenario("Successful registration returns created user profile without password", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "Str0ng#Pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "alice@example.com", password: "Str0ng#Pass1" },
          null,
        );
      },
    );

    Then("the response status code should be 201", () => {
      expect(ctx.response!.status).toBe(201);
    });

    And('the response body should contain "username" equal to "alice"', () => {
      expect((ctx.response!.body as Record<string, unknown>).username).toBe("alice");
    });

    And('the response body should not contain a "password" field', () => {
      expect(ctx.response!.body).not.toHaveProperty("password");
    });
  });

  Scenario("Successful registration response includes non-null user ID", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "Str0ng#Pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "alice@example.com", password: "Str0ng#Pass1" },
          null,
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

  Scenario("Reject registration when username already exists", ({ Given, When, Then, And }) => {
    Given('a user "alice" is registered with email "alice@example.com" and password "Str0ng#Pass1"', async () => {
      await registerUser(ctx, "alice", "alice@example.com", "Str0ng#Pass1");
    });

    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "new@example.com", "password": "Str0ng#Pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "new@example.com", password: "Str0ng#Pass1" },
          null,
        );
      },
    );

    Then("the response status code should be 409", () => {
      expect(ctx.response!.status).toBe(409);
    });

    And("the response body should contain an error message about duplicate username", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("username");
    });
  });

  Scenario("Reject registration with invalid email format", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "not-an-email", "password": "Str0ng#Pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "not-an-email", password: "Str0ng#Pass1" },
          null,
        );
      },
    );

    Then("the response status code should be 400", () => {
      expect(ctx.response!.status).toBe(400);
    });

    And('the response body should contain a validation error for "email"', () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(String(body.error).toLowerCase()).toContain("email");
    });
  });

  Scenario("Reject registration with empty password", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "alice@example.com", password: "" },
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

  Scenario("Reject registration with weak password — no uppercase letter", ({ When, Then, And }) => {
    When(
      'the client sends POST /api/v1/auth/register with body { "username": "alice", "email": "alice@example.com", "password": "str0ng#pass1" }',
      async () => {
        ctx.response = await ctx.client.dispatch(
          "POST",
          "/api/v1/auth/register",
          { username: "alice", email: "alice@example.com", password: "str0ng#pass1" },
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
});
