import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, cleanup } from "@testing-library/react";
import { expect } from "vitest";

import { Badge } from "./badge";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/badge/badge.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Renders default variant", ({ Given, Then, And }) => {
    Given('I render a Badge with text "workout"', () => {});

    Then('I see text "workout"', () => {
      cleanup();
      render(<Badge>workout</Badge>);
      expect(screen.getByText("workout")).toBeDefined();
    });

    And("the badge has solid background", () => {
      cleanup();
      render(<Badge>workout</Badge>);
      expect(screen.getByText("workout").className).toContain("bg-[var(--hue-color)]");
    });
  });

  Scenario("Renders outline variant with hue", ({ Given, Then, And }) => {
    Given('I render a Badge variant "outline" hue "honey"', () => {});

    Then("the badge has honey wash background", () => {
      cleanup();
      render(
        <Badge variant="outline" hue="honey">
          chip
        </Badge>,
      );
      const el = screen.getByText("chip") as HTMLElement;
      expect(el.style.getPropertyValue("--hue-wash")).toBe("var(--hue-honey-wash)");
    });

    And("the badge has honey border", () => {
      cleanup();
      render(
        <Badge variant="outline" hue="honey">
          chip
        </Badge>,
      );
      expect(screen.getByText("chip").className).toContain("border");
    });
  });

  Scenario("Renders secondary variant", ({ Given, Then }) => {
    Given('I render a Badge variant "secondary"', () => {});

    Then("the badge has background color from --color-secondary", () => {
      cleanup();
      render(<Badge variant="secondary">sec</Badge>);
      expect(screen.getByText("sec").className).toContain("bg-secondary");
    });
  });

  Scenario("Renders destructive variant", ({ Given, Then }) => {
    Given('I render a Badge variant "destructive"', () => {});

    Then("the badge uses destructive colors", () => {
      cleanup();
      render(<Badge variant="destructive">err</Badge>);
      expect(screen.getByText("err").className).toContain("bg-destructive");
    });
  });

  Scenario("Renders md size", ({ Given, Then, And }) => {
    Given('I render a Badge with size "md"', () => {});

    Then('the badge has class containing "text-[13px]"', () => {
      cleanup();
      render(<Badge size="md">md</Badge>);
      expect(screen.getByText("md").className).toContain("text-[13px]");
    });

    And('the badge has class containing "px-2.5"', () => {
      cleanup();
      render(<Badge size="md">md</Badge>);
      expect(screen.getByText("md").className).toContain("px-2.5");
    });
  });
});
