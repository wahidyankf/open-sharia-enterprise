import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, cleanup } from "@testing-library/react";
import { expect } from "vitest";

import { ProgressRing } from "./progress-ring";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/progress-ring/progress-ring.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Full progress ring", ({ Given, Then }) => {
    Given("I render a ProgressRing with progress 1", () => {
      // precondition noted
    });

    Then('the progressbar should have aria-valuenow "100"', () => {
      cleanup();
      const { container } = render(<ProgressRing progress={1} />);
      expect(container.querySelector("[role='progressbar']")?.getAttribute("aria-valuenow")).toBe("100");
    });
  });

  Scenario("Half progress ring", ({ Given, Then }) => {
    Given("I render a ProgressRing with progress 0.5", () => {
      // precondition noted
    });

    Then('the progressbar should have aria-valuenow "50"', () => {
      cleanup();
      const { container } = render(<ProgressRing progress={0.5} />);
      expect(container.querySelector("[role='progressbar']")?.getAttribute("aria-valuenow")).toBe("50");
    });
  });

  Scenario("Empty progress ring", ({ Given, Then }) => {
    Given("I render a ProgressRing with progress 0", () => {
      // precondition noted
    });

    Then('the progressbar should have aria-valuenow "0"', () => {
      cleanup();
      const { container } = render(<ProgressRing progress={0} />);
      expect(container.querySelector("[role='progressbar']")?.getAttribute("aria-valuenow")).toBe("0");
    });
  });

  Scenario("Has correct aria attributes", ({ Given, Then, And }) => {
    Given("I render a ProgressRing with progress 0.75", () => {
      // precondition noted
    });

    Then('the progressbar should have aria-valuemin "0"', () => {
      cleanup();
      const { container } = render(<ProgressRing progress={0.75} />);
      expect(container.querySelector("[role='progressbar']")?.getAttribute("aria-valuemin")).toBe("0");
    });

    And('the progressbar should have aria-valuemax "100"', () => {
      cleanup();
      const { container } = render(<ProgressRing progress={0.75} />);
      expect(container.querySelector("[role='progressbar']")?.getAttribute("aria-valuemax")).toBe("100");
    });
  });
});
