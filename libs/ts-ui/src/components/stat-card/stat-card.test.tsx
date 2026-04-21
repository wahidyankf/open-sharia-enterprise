import { render, screen } from "@testing-library/react";
import { describe, it, expect } from "vitest";
import { StatCard } from "./stat-card";

describe("StatCard", () => {
  const defaultProps = {
    label: "Steps",
    value: "12500",
    unit: "steps",
    hue: "teal" as const,
    icon: "trend",
  };

  it("renders label", () => {
    render(<StatCard {...defaultProps} />);
    expect(screen.getByText("Steps")).toBeTruthy();
  });

  it("renders value", () => {
    render(<StatCard {...defaultProps} />);
    expect(screen.getByText("12500")).toBeTruthy();
  });

  it("renders unit", () => {
    render(<StatCard {...defaultProps} />);
    expect(screen.getByText("steps")).toBeTruthy();
  });

  it("renders InfoTip when info provided", () => {
    render(<StatCard {...defaultProps} info="Daily steps" />);
    expect(screen.getByRole("button", { name: "Steps" })).toBeTruthy();
  });

  it("no InfoTip when info absent", () => {
    render(<StatCard {...defaultProps} />);
    expect(screen.queryByRole("button", { name: "Steps" })).toBeNull();
  });
});
