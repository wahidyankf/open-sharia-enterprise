import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, fireEvent, cleanup } from "@testing-library/react";
import { vi, expect } from "vitest";

import { TabBar, type TabItem } from "./tab-bar";

const tabs: TabItem[] = [
  { id: "home", label: "Home", icon: "home" },
  { id: "history", label: "History", icon: "history" },
  { id: "settings", label: "Settings", icon: "settings" },
];

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../specs/libs/ts-ui/gherkin/tab-bar/tab-bar.feature"),
);

describeFeature(feature, ({ Scenario }) => {
  Scenario("Renders tabs", ({ Given, Then }) => {
    Given('I render a TabBar with tabs "Home,History,Settings" and current "Home"', () => {
      // precondition noted
    });

    Then("the tab bar should show 3 tabs", () => {
      cleanup();
      render(<TabBar tabs={tabs} current="home" onChange={() => {}} />);
      expect(screen.getAllByRole("tab")).toHaveLength(3);
    });
  });

  Scenario("Click triggers onChange", ({ Given, When, Then }) => {
    const onChangeMock = vi.fn();

    Given('I render a TabBar with tabs "Home,History,Settings" and current "Home"', () => {
      // precondition noted; render happens in When step to persist mock state
    });

    When('the user clicks the "History" tab', () => {
      cleanup();
      render(<TabBar tabs={tabs} current="home" onChange={onChangeMock} />);
      const historyTab = screen.getByText("History").closest("button");
      if (historyTab) fireEvent.click(historyTab);
    });

    Then('onChange should be called with "history"', () => {
      expect(onChangeMock).toHaveBeenCalledWith("history");
    });
  });

  Scenario("Active tab has aria-selected true", ({ Given, Then }) => {
    Given('I render a TabBar with tabs "Home,History,Settings" and current "home"', () => {
      // precondition noted
    });

    Then('the "Home" tab should have aria-selected "true"', () => {
      cleanup();
      render(<TabBar tabs={tabs} current="home" onChange={() => {}} />);
      const homeTab = screen.getByText("Home").closest("button");
      expect(homeTab?.getAttribute("aria-selected")).toBe("true");
    });
  });

  Scenario("Inactive tab has aria-selected false", ({ Given, Then }) => {
    Given('I render a TabBar with tabs "Home,History,Settings" and current "home"', () => {
      // precondition noted
    });

    Then('the "History" tab should have aria-selected "false"', () => {
      cleanup();
      render(<TabBar tabs={tabs} current="home" onChange={() => {}} />);
      const historyTab = screen.getByText("History").closest("button");
      expect(historyTab?.getAttribute("aria-selected")).toBe("false");
    });
  });
});
