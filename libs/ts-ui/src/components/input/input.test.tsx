import { render, screen } from "@testing-library/react";
import { axe } from "vitest-axe";
import { describe, it, expect } from "vitest";

import { Input } from "./input";

describe("Input", () => {
  it("renders with data-slot", () => {
    render(<Input aria-label="test input" />);
    const input = screen.getByRole("textbox");
    expect(input.getAttribute("data-slot")).toBe("input");
  });

  it("forwards className", () => {
    render(<Input aria-label="test" className="custom" />);
    expect(screen.getByRole("textbox").className).toContain("custom");
  });

  it("supports disabled state", () => {
    render(<Input aria-label="test" disabled />);
    expect(screen.getByRole("textbox").hasAttribute("disabled")).toBe(true);
  });

  it("supports type prop", () => {
    render(<Input aria-label="email" type="email" />);
    expect(screen.getByRole("textbox").getAttribute("type")).toBe("email");
  });

  it("has h-11 height class (44px OL touch target)", () => {
    render(<Input aria-label="test" />);
    expect(screen.getByRole("textbox").className).toContain("h-11");
  });

  it("has no accessibility violations with label", async () => {
    const { container } = render(
      <div>
        <label htmlFor="test-input">Email</label>
        <Input id="test-input" type="email" />
      </div>,
    );
    const results = await axe(container);
    expect(results).toHaveNoViolations();
  });
});
