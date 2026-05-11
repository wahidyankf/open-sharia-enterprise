import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import { vi, expect } from "vitest";

import { Toggle } from "./toggle";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/toggle/toggle.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Renders in off state", ({ Given, Then }) => {
    Given("I render a Toggle with value false", () => {
      // precondition noted
    });

    Then('the toggle switch should have aria-checked "false"', () => {
      cleanup();
      render(<Toggle value={false} onChange={() => {}} />);
      expect(screen.getByRole("switch").getAttribute("aria-checked")).toBe("false");
    });
  });

  Scenario("Renders in on state", ({ Given, Then }) => {
    Given("I render a Toggle with value true", () => {
      // precondition noted
    });

    Then('the toggle switch should have aria-checked "true"', () => {
      cleanup();
      render(<Toggle value={true} onChange={() => {}} />);
      expect(screen.getByRole("switch").getAttribute("aria-checked")).toBe("true");
    });
  });

  Scenario("Click triggers onChange", ({ Given, When, Then }) => {
    const onChangeMock = vi.fn();

    Given("I render a Toggle with value false", () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When("the user clicks the toggle", () => {
      cleanup();
      render(<Toggle value={false} onChange={onChangeMock} />);
      fireEvent.click(screen.getByRole("switch"));
    });

    Then("onChange should be called with true", () => {
      expect(onChangeMock).toHaveBeenCalledWith(true);
    });
  });

  Scenario("Disabled toggle does not trigger onChange", ({ Given, When, Then }) => {
    const onChangeMock = vi.fn();

    Given("I render a Toggle with value false and disabled", () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When("the user clicks the toggle", () => {
      cleanup();
      render(<Toggle value={false} onChange={onChangeMock} disabled />);
      fireEvent.click(screen.getByRole("switch", { hidden: true }));
    });

    Then("onChange should not be called", () => {
      expect(onChangeMock).not.toHaveBeenCalled();
    });
  });

  Scenario("Renders with label", ({ Given, Then }) => {
    Given('I render a Toggle with value false and label "Enable notifications"', () => {
      // precondition noted
    });

    Then('the label "Enable notifications" should be visible', () => {
      cleanup();
      render(<Toggle value={false} onChange={() => {}} label="Enable notifications" />);
      expect(screen.getByText("Enable notifications")).toBeDefined();
    });
  });
});
