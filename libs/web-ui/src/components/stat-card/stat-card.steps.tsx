import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, cleanup } from "@testing-library/react";
import { expect } from "vitest";

import { StatCard } from "./stat-card";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/stat-card/stat-card.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Renders label and value", ({ Given, Then, And }) => {
    Given('I render a StatCard with label "Steps" value "12500" unit "steps" hue "teal" icon "trend"', () => {
      // precondition noted
    });

    Then('the text "Steps" should be visible', () => {
      cleanup();
      render(<StatCard label="Steps" value="12500" unit="steps" hue="teal" icon="trend" />);
      expect(screen.getByText("Steps")).toBeDefined();
    });

    And('the text "12500" should be visible', () => {
      cleanup();
      render(<StatCard label="Steps" value="12500" unit="steps" hue="teal" icon="trend" />);
      expect(screen.getByText("12500")).toBeDefined();
    });

    And('the text "steps" should be visible', () => {
      cleanup();
      render(<StatCard label="Steps" value="12500" unit="steps" hue="teal" icon="trend" />);
      expect(screen.getByText("steps")).toBeDefined();
    });
  });

  Scenario("Renders InfoTip when info is provided", ({ Given, Then }) => {
    Given(
      'I render a StatCard with label "Steps" value "12500" unit "steps" hue "teal" icon "trend" and info "Daily step count"',
      () => {
        // precondition noted
      },
    );

    Then("an InfoTip trigger should be visible", () => {
      cleanup();
      render(<StatCard label="Steps" value="12500" unit="steps" hue="teal" icon="trend" info="Daily step count" />);
      expect(screen.getByRole("button", { name: "Steps" })).toBeDefined();
    });
  });

  Scenario("Does not render InfoTip when info is absent", ({ Given, Then }) => {
    Given(
      'I render a StatCard with label "Steps" value "12500" unit "steps" hue "teal" icon "trend" without info',
      () => {
        // precondition noted
      },
    );

    Then("no InfoTip trigger should be present", () => {
      cleanup();
      render(<StatCard label="Steps" value="12500" unit="steps" hue="teal" icon="trend" />);
      expect(screen.queryByRole("button", { name: "Steps" })).toBeNull();
    });
  });
});
