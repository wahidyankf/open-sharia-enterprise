import { render, screen, fireEvent } from "@testing-library/react";
import { vi, describe, it, expect } from "vitest";
import { SideNav } from "./side-nav";
import type { TabItem } from "../tab-bar/tab-bar";

const tabs: TabItem[] = [
  { id: "home", label: "Home", icon: "home" },
  { id: "history", label: "History", icon: "history" },
];

const brand = { name: "OrganicLever", icon: "dumbbell", hue: "teal" as const };

describe("SideNav", () => {
  it("renders brand name", () => {
    render(<SideNav brand={brand} tabs={tabs} current="home" onChange={() => {}} />);
    expect(screen.getByText("OrganicLever")).toBeTruthy();
  });

  it("renders tab labels", () => {
    render(<SideNav brand={brand} tabs={tabs} current="home" onChange={() => {}} />);
    expect(screen.getByText("Home")).toBeTruthy();
    expect(screen.getByText("History")).toBeTruthy();
  });

  it("calls onChange with tab id when tab clicked", () => {
    const onChange = vi.fn();
    render(<SideNav brand={brand} tabs={tabs} current="home" onChange={onChange} />);
    fireEvent.click(screen.getByText("History"));
    expect(onChange).toHaveBeenCalledWith("history");
  });

  it("brand row click calls onChange with home", () => {
    const onChange = vi.fn();
    render(<SideNav brand={brand} tabs={tabs} current="history" onChange={onChange} />);
    fireEvent.click(screen.getByText("OrganicLever"));
    expect(onChange).toHaveBeenCalledWith("home");
  });

  it("has role navigation with brand name", () => {
    render(<SideNav brand={brand} tabs={tabs} current="home" onChange={() => {}} />);
    expect(screen.getByRole("navigation", { name: "OrganicLever" })).toBeTruthy();
  });
});
