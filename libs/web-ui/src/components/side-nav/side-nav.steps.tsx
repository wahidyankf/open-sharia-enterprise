import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import { vi, expect } from "vitest";

import { SideNav } from "./side-nav";
import type { TabItem } from "../tab-bar/tab-bar";

const tabs: TabItem[] = [
  { id: "home", label: "Home", icon: "home" },
  { id: "history", label: "History", icon: "history" },
];

const brand = { name: "OrganicLever", icon: "dumbbell", hue: "teal" as const };

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/side-nav/side-nav.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Renders brand name", ({ Given, Then }) => {
    Given('I render a SideNav with brand "OrganicLever" and tabs', () => {
      // precondition noted
    });

    Then('the text "OrganicLever" should be visible', () => {
      cleanup();
      render(<SideNav brand={brand} tabs={tabs} current="home" onChange={() => {}} />);
      expect(screen.getByText("OrganicLever")).toBeDefined();
    });
  });

  Scenario("Renders tabs", ({ Given, Then }) => {
    Given('I render a SideNav with brand "OrganicLever" and tabs', () => {
      // precondition noted
    });

    Then('the tab "Home" should be visible', () => {
      cleanup();
      render(<SideNav brand={brand} tabs={tabs} current="home" onChange={() => {}} />);
      expect(screen.getByText("Home")).toBeDefined();
    });
  });

  Scenario("Tab click triggers onChange with tab id", ({ Given, When, Then }) => {
    const onChangeMock = vi.fn();

    Given('I render a SideNav with brand "OrganicLever" and tabs', () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When('the user clicks the "History" tab', () => {
      cleanup();
      render(<SideNav brand={brand} tabs={tabs} current="home" onChange={onChangeMock} />);
      fireEvent.click(screen.getByText("History"));
    });

    Then('onChange should be called with "history"', () => {
      expect(onChangeMock).toHaveBeenCalledWith("history");
    });
  });

  Scenario("Active tab has active background", ({ Given, Then }) => {
    Given('I render a SideNav with brand "OrganicLever" current "home" and tabs', () => {
      // precondition noted
    });

    Then('the "Home" button should have the active class', () => {
      cleanup();
      render(<SideNav brand={brand} tabs={tabs} current="home" onChange={() => {}} />);
      const homeButton = screen.getByText("Home").closest("button");
      expect(homeButton?.className).toContain("bg-[var(--hue-teal-wash)]");
    });
  });

  Scenario("Brand row click always calls onChange with home", ({ Given, When, Then }) => {
    const onChangeMock = vi.fn();

    Given('I render a SideNav with brand "OrganicLever" current "history" and tabs', () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When("the user clicks the brand row", () => {
      cleanup();
      render(<SideNav brand={brand} tabs={tabs} current="history" onChange={onChangeMock} />);
      fireEvent.click(screen.getByText("OrganicLever"));
    });

    Then('onChange should be called with "home"', () => {
      expect(onChangeMock).toHaveBeenCalledWith("home");
    });
  });
});
