import { render, screen } from "@testing-library/react";
import { axe } from "vitest-axe";
import { describe, it, expect } from "vitest";

import { Label } from "./label";

describe("Label", () => {
  it("renders with data-slot", () => {
    render(<Label>Email</Label>);
    expect(screen.getByText("Email").getAttribute("data-slot")).toBe("label");
  });

  it("forwards className", () => {
    render(<Label className="custom">Name</Label>);
    expect(screen.getByText("Name").className).toContain("custom");
  });

  it("associates with input via htmlFor", async () => {
    const { container } = render(
      <div>
        <Label htmlFor="email-input">Email</Label>
        <input id="email-input" type="email" />
      </div>,
    );
    const results = await axe(container);
    expect(results).toHaveNoViolations();
  });
});
