import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { createTestContext, type TestContext } from "./helpers/test-context";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/demo/be/gherkin/health/health-check.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  let ctx: TestContext;

  Background(({ Given }) => {
    Given("the API is running", () => {
      ctx = createTestContext();
    });
  });

  Scenario("Health endpoint reports the service as UP", ({ When, Then, And }) => {
    When("an operations engineer sends GET /health", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/health", null, null);
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the health status should be "UP"', () => {
      expect((ctx.response!.body as Record<string, unknown>).status).toBe("UP");
    });
  });

  Scenario("Anonymous health check does not expose component details", ({ When, Then, And }) => {
    When("an unauthenticated engineer sends GET /health", async () => {
      ctx.response = await ctx.client.dispatch("GET", "/health", null, null);
    });

    Then("the response status code should be 200", () => {
      expect(ctx.response!.status).toBe(200);
    });

    And('the health status should be "UP"', () => {
      expect((ctx.response!.body as Record<string, unknown>).status).toBe("UP");
    });

    And("the response should not include detailed component health information", () => {
      const body = ctx.response!.body as Record<string, unknown>;
      expect(body).not.toHaveProperty("components");
    });
  });
});
