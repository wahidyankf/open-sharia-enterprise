import { render, screen } from "@testing-library/react";
import { axe } from "vitest-axe";
import { describe, it, expect } from "vitest";

import { Textarea } from "./textarea";

describe("Textarea", () => {
  it("renders with data-slot", () => {
    render(<Textarea aria-label="test textarea" />);
    const textarea = screen.getByRole("textbox");
    expect(textarea.getAttribute("data-slot")).toBe("textarea");
  });

  it("forwards className", () => {
    render(<Textarea aria-label="test" className="custom" />);
    expect(screen.getByRole("textbox").className).toContain("custom");
  });

  it("supports disabled state", () => {
    render(<Textarea aria-label="test" disabled />);
    expect(screen.getByRole("textbox").hasAttribute("disabled")).toBe(true);
  });

  it("has resize-none class", () => {
    render(<Textarea aria-label="test" />);
    expect(screen.getByRole("textbox").className).toContain("resize-none");
  });

  it("has min-h class", () => {
    render(<Textarea aria-label="test" />);
    expect(screen.getByRole("textbox").className).toContain("min-h-");
  });

  it("has no accessibility violations with label", async () => {
    const { container } = render(
      <div>
        <label htmlFor="test-textarea">Notes</label>
        <Textarea id="test-textarea" />
      </div>,
    );
    const results = await axe(container);
    expect(results).toHaveNoViolations();
  });
});
