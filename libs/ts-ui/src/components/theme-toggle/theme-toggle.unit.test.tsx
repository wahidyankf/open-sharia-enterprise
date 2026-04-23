import { describe, it, expect, beforeEach } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import ThemeToggle from "./theme-toggle";

describe("ThemeToggle", () => {
  beforeEach(() => {
    localStorage.clear();
    document.documentElement.classList.remove("light-theme");
  });

  it("defaults to dark mode (aria-label targets switching to light)", () => {
    render(<ThemeToggle />);
    expect(screen.getByRole("button", { name: "Switch to light theme" })).toBeDefined();
  });

  it("applies light-theme class on html when switching to light", () => {
    render(<ThemeToggle />);
    const button = screen.getByRole("button", { name: "Switch to light theme" });
    fireEvent.click(button);
    expect(document.documentElement.classList.contains("light-theme")).toBe(true);
    expect(localStorage.getItem("theme")).toBe("light");
    expect(screen.getByRole("button", { name: "Switch to dark theme" })).toBeDefined();
  });

  it("restores persisted light theme from localStorage on mount", () => {
    localStorage.setItem("theme", "light");
    render(<ThemeToggle />);
    expect(document.documentElement.classList.contains("light-theme")).toBe(true);
    expect(screen.getByRole("button", { name: "Switch to dark theme" })).toBeDefined();
  });

  it("toggles back to dark on second click", () => {
    render(<ThemeToggle />);
    const button = screen.getByRole("button", { name: "Switch to light theme" });
    fireEvent.click(button);
    fireEvent.click(screen.getByRole("button", { name: "Switch to dark theme" }));
    expect(document.documentElement.classList.contains("light-theme")).toBe(false);
    expect(localStorage.getItem("theme")).toBe("dark");
  });

  it("accepts custom className for button", () => {
    render(<ThemeToggle className="custom-btn-class" />);
    const button = screen.getByRole("button", { name: "Switch to light theme" });
    expect(Array.from(button.classList)).toContain("custom-btn-class");
  });
});
