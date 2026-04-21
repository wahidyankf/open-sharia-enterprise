import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import { vi, expect } from "vitest";

import { Sheet } from "./sheet";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/sheet/sheet.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Title renders", ({ Given, Then }) => {
    Given('I render a Sheet with title "Settings"', () => {
      // precondition noted
    });

    Then('the heading "Settings" should be visible', () => {
      cleanup();
      render(<Sheet title="Settings" onClose={() => {}} />);
      expect(screen.getByText("Settings")).toBeDefined();
    });
  });

  Scenario("Close button closes sheet", ({ Given, When, Then }) => {
    const onCloseMock = vi.fn();

    Given('I render a Sheet with title "Settings" and an onClose handler', () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When("the user clicks the close button", () => {
      cleanup();
      render(<Sheet title="Settings" onClose={onCloseMock} />);
      fireEvent.click(screen.getByRole("button", { name: /close/i }));
    });

    Then("onClose should be called", () => {
      expect(onCloseMock).toHaveBeenCalled();
    });
  });

  Scenario("Has accessible title", ({ Given, Then }) => {
    Given('I render a Sheet with title "My Sheet"', () => {
      // precondition noted
    });

    Then('the dialog should have accessible label "My Sheet"', () => {
      cleanup();
      render(<Sheet title="My Sheet" onClose={() => {}} />);
      expect(screen.getByRole("dialog", { name: "My Sheet" })).toBeDefined();
    });
  });
});
