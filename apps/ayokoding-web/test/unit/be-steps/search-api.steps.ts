import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import { testCaller } from "./helpers/test-caller";
import type { SearchResult } from "@/server/content/types";

const feature = await loadFeature(
  path.resolve(process.cwd(), "../../specs/apps/ayokoding/be/gherkin/search-api/search-api.feature"),
);

describeFeature(feature, ({ Scenario, Background }) => {
  Background(({ Given }) => {
    Given("the API is running", () => {});
  });

  Scenario("Search returns matching results with title, slug, and excerpt", ({ Given, When, Then, And }) => {
    let results: SearchResult[];

    Given('published pages indexed under locale "en" include a page titled "Getting Started with Go"', () => {
      // pages are available in the test fixture
    });

    When('the client calls search.query with locale "en" and query "golang"', async () => {
      results = await testCaller.search.query({ query: "golang", locale: "en" });
    });

    Then("the response should contain at least one result", () => {
      expect(results.length).toBeGreaterThan(0);
    });

    And('each result should include a "title" field', () => {
      const first = results[0]!;
      expect(first).toHaveProperty("title");
    });

    And('each result should include a "slug" field', () => {
      const first = results[0]!;
      expect(first).toHaveProperty("slug");
    });

    And('each result should include an "excerpt" field', () => {
      const first = results[0]!;
      expect(first).toHaveProperty("excerpt");
    });
  });

  Scenario("Search results include locale information", ({ Given, When, Then }) => {
    let results: SearchResult[];

    Given('published pages indexed under locale "en" include content about "programming"', () => {
      // pages are available in the test fixture
    });

    When('the client calls search.query with locale "en" and query "programming"', async () => {
      results = await testCaller.search.query({ query: "programming", locale: "en" });
    });

    Then('each result should include a "locale" field matching "en"', () => {
      for (const result of results) {
        expect(result).toHaveProperty("locale");
        expect(result.locale).toBe("en");
      }
    });
  });

  Scenario("Search is scoped to the requested locale", ({ Given, When, Then, And }) => {
    let results: SearchResult[];

    Given('a page exists in locale "en" with title "Security Basics"', () => {
      // page is available in the test fixture
    });

    And('no equivalent page exists in locale "id"', () => {
      // no equivalent page in "id" locale
    });

    When('the client calls search.query with locale "id" and query "security"', async () => {
      results = await testCaller.search.query({ query: "security", locale: "id" });
    });

    Then("the response should contain no results", () => {
      expect(results.length).toBe(0);
    });
  });

  Scenario("Empty query returns an error", ({ When, Then }) => {
    let error: unknown = null;

    When('the client calls search.query with locale "en" and an empty query', async () => {
      try {
        await testCaller.search.query({ query: "", locale: "en" });
      } catch (e) {
        error = e;
      }
    });

    Then("the response should indicate an invalid input error", () => {
      expect(error).toBeTruthy();
    });
  });
});
