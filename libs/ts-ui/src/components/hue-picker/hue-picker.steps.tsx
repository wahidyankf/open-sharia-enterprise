import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import { vi, expect } from "vitest";

import { HuePicker } from "./hue-picker";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/hue-picker/hue-picker.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Renders 6 swatches", ({ Given, Then }) => {
    Given('I render a HuePicker with value "teal"', () => {
      // precondition noted
    });

    Then("the component should have 6 swatch buttons", () => {
      cleanup();
      render(<HuePicker value="teal" onChange={() => {}} />);
      expect(screen.getAllByRole("button")).toHaveLength(6);
    });
  });

  Scenario("Click calls onChange", ({ Given, When, Then }) => {
    const onChangeMock = vi.fn();

    Given('I render a HuePicker with value "teal"', () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When('the user clicks the "sage" swatch', () => {
      cleanup();
      render(<HuePicker value="teal" onChange={onChangeMock} />);
      fireEvent.click(screen.getByRole("button", { name: "sage" }));
    });

    Then('onChange should be called with "sage"', () => {
      expect(onChangeMock).toHaveBeenCalledWith("sage");
    });
  });

  Scenario("aria-pressed reflects selection", ({ Given, Then, And }) => {
    Given('I render a HuePicker with value "teal"', () => {
      // precondition noted
    });

    Then('the "teal" swatch should have aria-pressed "true"', () => {
      cleanup();
      render(<HuePicker value="teal" onChange={() => {}} />);
      expect(screen.getByRole("button", { name: "teal" }).getAttribute("aria-pressed")).toBe("true");
    });

    And('the "sage" swatch should have aria-pressed "false"', () => {
      cleanup();
      render(<HuePicker value="teal" onChange={() => {}} />);
      expect(screen.getByRole("button", { name: "sage" }).getAttribute("aria-pressed")).toBe("false");
    });
  });
});
