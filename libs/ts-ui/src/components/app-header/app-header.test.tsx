import { render, screen, fireEvent } from "@testing-library/react";
import { vi, describe, it, expect } from "vitest";
import { AppHeader } from "./app-header";

describe("AppHeader", () => {
  it("renders title", () => {
    render(<AppHeader title="Workouts" />);
    expect(screen.getByText("Workouts")).toBeTruthy();
  });

  it("shows back button when onBack provided", () => {
    render(<AppHeader title="Details" onBack={() => {}} />);
    expect(screen.getByRole("button", { name: /go back/i })).toBeTruthy();
  });

  it("hides back button when onBack absent", () => {
    render(<AppHeader title="Home" />);
    expect(screen.queryByRole("button", { name: /go back/i })).toBeNull();
  });

  it("calls onBack when back button clicked", () => {
    const onBack = vi.fn();
    render(<AppHeader title="Details" onBack={onBack} />);
    fireEvent.click(screen.getByRole("button", { name: /go back/i }));
    expect(onBack).toHaveBeenCalled();
  });

  it("renders subtitle", () => {
    render(<AppHeader title="Workouts" subtitle="Today" />);
    expect(screen.getByText("Today")).toBeTruthy();
  });

  it("renders trailing content", () => {
    render(<AppHeader title="Workouts" trailing={<span>Edit</span>} />);
    expect(screen.getByText("Edit")).toBeTruthy();
  });
});
