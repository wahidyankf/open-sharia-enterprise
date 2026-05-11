import { render, screen, fireEvent } from "@testing-library/react";
import { vi, describe, it, expect } from "vitest";
import { Sheet } from "./sheet";

describe("Sheet", () => {
  it("renders title", () => {
    render(<Sheet title="Settings" onClose={() => {}} />);
    expect(screen.getByText("Settings")).toBeTruthy();
  });

  it("renders close button", () => {
    render(<Sheet title="Settings" onClose={() => {}} />);
    expect(screen.getByRole("button", { name: /close/i })).toBeTruthy();
  });

  it("calls onClose when close button clicked", () => {
    const onClose = vi.fn();
    render(<Sheet title="Settings" onClose={onClose} />);
    fireEvent.click(screen.getByRole("button", { name: /close/i }));
    expect(onClose).toHaveBeenCalled();
  });

  it("renders children", () => {
    render(
      <Sheet title="Settings" onClose={() => {}}>
        Hello children
      </Sheet>,
    );
    expect(screen.getByText("Hello children")).toBeTruthy();
  });
});
