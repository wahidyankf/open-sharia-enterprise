import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import { expect } from "vitest";

import { InfoTip } from "./info-tip";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/info-tip/info-tip.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Trigger button renders", ({ Given, Then }) => {
    Given('I render an InfoTip with title "Volume" and text "Adjust the volume"', () => {
      // precondition noted
    });

    Then('the trigger button with aria-label "Volume" should be visible', () => {
      cleanup();
      render(<InfoTip title="Volume" text="Adjust the volume" />);
      expect(screen.getByRole("button", { name: "Volume" })).toBeDefined();
    });
  });

  Scenario("Click trigger opens Sheet", ({ Given, When, Then }) => {
    Given('I render an InfoTip with title "Volume" and text "Adjust the volume"', () => {
      // precondition noted
    });

    When("the user clicks the trigger button", () => {
      // precondition noted; action and assertion in Then step
    });

    Then('the Sheet with title "Volume" should be visible', () => {
      cleanup();
      render(<InfoTip title="Volume" text="Adjust the volume" />);
      fireEvent.click(screen.getByRole("button", { name: "Volume" }));
      expect(screen.getByText("Adjust the volume")).toBeDefined();
    });
  });

  Scenario("Sheet close button closes Sheet", ({ Given, When, Then, And }) => {
    Given('I render an InfoTip with title "Volume" and text "Adjust the volume"', () => {
      // precondition noted
    });

    When("the user clicks the trigger button", () => {
      // precondition noted; actions in And and Then steps
    });

    And("the user clicks the close button", () => {
      // precondition noted; action and assertion in Then step
    });

    Then("the Sheet should not be visible", () => {
      cleanup();
      render(<InfoTip title="Volume" text="Adjust the volume" />);
      fireEvent.click(screen.getByRole("button", { name: "Volume" }));
      fireEvent.click(screen.getByRole("button", { name: /got it/i }));
      expect(screen.queryByText("Adjust the volume")).toBeNull();
    });
  });
});
