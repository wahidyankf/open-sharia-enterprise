import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import { vi, expect } from "vitest";

import { AppHeader } from "./app-header";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/app-header/app-header.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Renders title", ({ Given, Then }) => {
    Given('I render an AppHeader with title "Workouts"', () => {
      // precondition noted
    });

    Then('the heading "Workouts" should be visible', () => {
      cleanup();
      render(<AppHeader title="Workouts" />);
      expect(screen.getByText("Workouts")).toBeDefined();
    });
  });

  Scenario("Back button appears when onBack provided", ({ Given, Then }) => {
    Given('I render an AppHeader with title "Details" and an onBack handler', () => {
      // precondition noted
    });

    Then('a button with aria-label "Go back" should be visible', () => {
      cleanup();
      render(<AppHeader title="Details" onBack={() => {}} />);
      expect(screen.getByRole("button", { name: /go back/i })).toBeDefined();
    });
  });

  Scenario("Back button absent when onBack not provided", ({ Given, Then }) => {
    Given('I render an AppHeader with title "Home" without onBack', () => {
      // precondition noted
    });

    Then('no button with aria-label "Go back" should be present', () => {
      cleanup();
      render(<AppHeader title="Home" />);
      expect(screen.queryByRole("button", { name: /go back/i })).toBeNull();
    });
  });

  Scenario("Back button click triggers onBack", ({ Given, When, Then }) => {
    const onBackMock = vi.fn();

    Given('I render an AppHeader with title "Details" and an onBack handler', () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When("the user clicks the back button", () => {
      cleanup();
      render(<AppHeader title="Details" onBack={onBackMock} />);
      fireEvent.click(screen.getByRole("button", { name: /go back/i }));
    });

    Then("onBack should be called", () => {
      expect(onBackMock).toHaveBeenCalled();
    });
  });

  Scenario("Renders subtitle when provided", ({ Given, Then }) => {
    Given('I render an AppHeader with title "Workouts" and subtitle "Today"', () => {
      // precondition noted
    });

    Then('the text "Today" should be visible', () => {
      cleanup();
      render(<AppHeader title="Workouts" subtitle="Today" />);
      expect(screen.getByText("Today")).toBeDefined();
    });
  });
});
