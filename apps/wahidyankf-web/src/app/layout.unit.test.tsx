import React from "react";
import { describe, it, expect, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import RootLayout from "./layout";

vi.mock("@open-sharia-enterprise/ts-ui", () => ({
  ScrollToTop: () => <div data-testid="scroll-to-top">ScrollToTop</div>,
  ThemeToggle: () => <div data-testid="theme-toggle">ThemeToggle</div>,
}));

// Mock the Inter font
vi.mock("next/font/google", () => ({
  Inter: () => ({ className: "inter-font" }),
}));

describe("RootLayout", () => {
  it("renders children correctly", () => {
    render(
      <RootLayout>
        <div>Test content</div>
      </RootLayout>,
    );

    const bodyContent = screen.getByText("Test content").closest(".body-content");
    expect(bodyContent).toBeInTheDocument();
    expect(bodyContent).toHaveClass("body-content flex-grow");

    expect(screen.getByTestId("theme-toggle")).toBeInTheDocument();
    expect(screen.getByText("Test content")).toBeInTheDocument();
  });

  it("includes ScrollToTop component", () => {
    render(
      <RootLayout>
        <div>Test content</div>
      </RootLayout>,
    );

    expect(screen.getByTestId("scroll-to-top")).toBeInTheDocument();
  });
});
