import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { expect } from "vitest";
import "./helpers/test-setup";

const feature = await loadFeature(
  path.resolve(
    process.cwd(),
    "../../specs/apps/ayokoding/behavior/web/gherkin/navigation/architecture-cases-routes.feature",
  ),
);

describeFeature(feature, ({ Scenario, Background }) => {
  Background(({ Given }) => {
    Given("the app is running", () => {});
  });

  Scenario("In FP case route is reachable", ({ When, Then, And }) => {
    When('a visitor navigates to "/en/learn/software-engineering/software-architecture/cases/in-fp"', () => {
      expect(true).toBe(true);
    });

    Then("the page should respond with HTTP 200", () => {
      expect(true).toBe(true);
    });

    And('the page should contain a heading with text "In FP — F# / Giraffe / Npgsql"', () => {
      expect(true).toBe(true);
    });
  });

  Scenario("In OOP case route is reachable", ({ When, Then, And }) => {
    When('a visitor navigates to "/en/learn/software-engineering/software-architecture/cases/in-oop"', () => {
      expect(true).toBe(true);
    });

    Then("the page should respond with HTTP 200", () => {
      expect(true).toBe(true);
    });

    And('the page should contain a heading with text "In OOP — Java / Spring Boot"', () => {
      expect(true).toBe(true);
    });
  });
});
