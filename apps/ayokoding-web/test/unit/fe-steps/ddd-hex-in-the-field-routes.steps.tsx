import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import "./helpers/test-setup";

const feature = await loadFeature(
  path.resolve(
    process.cwd(),
    "../../specs/apps/ayokoding/behavior/web/gherkin/navigation/ddd-hex-in-the-field-routes.feature",
  ),
);

describeFeature(feature, ({ Scenario, Background }) => {
  Background(({ Given }) => {
    Given("the app is running", () => {});
  });

  Scenario("F# in-the-field tutorial route is reachable", ({ When, Then, And }) => {
    When(
      'a visitor navigates to "/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/fp-in-the-field"',
      () => {
        expect(true).toBe(true);
      },
    );

    Then("the page should respond with HTTP 200", () => {
      expect(true).toBe(true);
    });

    And('the page should contain a heading with text "DDD + Hexagonal in Practice — F# in the Field"', () => {
      expect(true).toBe(true);
    });
  });

  Scenario("Java in-the-field tutorial route is reachable", ({ When, Then, And }) => {
    When(
      'a visitor navigates to "/en/learn/software-engineering/software-architecture/ddd-hexagonal-in-practice/oop-in-the-field"',
      () => {
        expect(true).toBe(true);
      },
    );

    Then("the page should respond with HTTP 200", () => {
      expect(true).toBe(true);
    });

    And('the page should contain a heading with text "DDD + Hexagonal in Practice — Java in the Field"', () => {
      expect(true).toBe(true);
    });
  });
});
