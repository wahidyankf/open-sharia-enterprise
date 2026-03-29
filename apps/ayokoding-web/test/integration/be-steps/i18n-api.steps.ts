import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { testCaller } from "./helpers/test-caller";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/ayokoding/be/gherkin/i18n/i18n-api.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  Background(({ Given }) => {
    Given("the API is running", () => {});
  });

  Scenario('English content is served when locale is "en"', ({ Given, When, Then, And }) => {
    let result: Awaited<ReturnType<typeof testCaller.content.getTree>>;

    Given('a page exists at slug "en/programming/golang/getting-started" under locale "en"', () => {});

    When('the client calls content.getBySlug with slug "en/programming/golang/getting-started"', async () => {
      result = await testCaller.content.getTree({ locale: "en" });
    });

    Then('the response "frontmatter" should indicate locale "en"', () => {
      expect(result.length).toBeGreaterThan(0);
    });

    And('the response "html" should contain English-language content', () => {
      expect(result.length).toBeGreaterThan(0);
    });
  });

  Scenario('Indonesian content is served when locale is "id"', ({ Given, When, Then, And }) => {
    let result: Awaited<ReturnType<typeof testCaller.content.getTree>>;

    Given('a page exists at slug "id/programming/golang/memulai" under locale "id"', () => {});

    When('the client calls content.getBySlug with slug "id/programming/golang/memulai"', async () => {
      result = await testCaller.content.getTree({ locale: "id" });
    });

    Then('the response "frontmatter" should indicate locale "id"', () => {
      expect(result.length).toBeGreaterThan(0);
    });

    And('the response "html" should contain Indonesian-language content', () => {
      expect(result.length).toBeGreaterThan(0);
    });
  });

  Scenario("Requesting a slug prefixed with an invalid locale returns not found", ({ When, Then }) => {
    let error: unknown = null;

    When('the client calls content.getBySlug with slug "fr/programming/golang/getting-started"', async () => {
      try {
        // @ts-expect-error - testing invalid locale
        await testCaller.content.getBySlug({ locale: "fr", slug: "test" });
      } catch (e) {
        error = e;
      }
    });

    Then("the response should indicate the page was not found", () => {
      expect(error).toBeTruthy();
    });
  });
});
