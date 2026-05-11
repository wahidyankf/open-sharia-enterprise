import { render, screen } from "@testing-library/react";
import { axe } from "vitest-axe";
import { describe, it, expect } from "vitest";

import { Badge } from "./badge";

describe("Badge", () => {
  it("renders with data-slot", () => {
    render(<Badge>test</Badge>);
    const badge = screen.getByText("test");
    expect(badge.getAttribute("data-slot")).toBe("badge");
  });

  it("renders children", () => {
    render(<Badge>workout</Badge>);
    expect(screen.getByText("workout")).toBeDefined();
  });

  it("forwards className", () => {
    render(<Badge className="custom">label</Badge>);
    expect(screen.getByText("label").className).toContain("custom");
  });

  it("default variant has solid bg class", () => {
    render(<Badge>solid</Badge>);
    expect(screen.getByText("solid").className).toContain("bg-[var(--hue-color)]");
  });

  it("outline variant has border class", () => {
    render(<Badge variant="outline">chip</Badge>);
    expect(screen.getByText("chip").className).toContain("border");
  });

  it("md size has text-[13px] class", () => {
    render(<Badge size="md">md</Badge>);
    expect(screen.getByText("md").className).toContain("text-[13px]");
  });

  it("applies hue CSS variables via style", () => {
    render(<Badge hue="teal">teal</Badge>);
    const el = screen.getByText("teal");
    expect((el as HTMLElement).style.getPropertyValue("--hue-color")).toBe("var(--hue-teal)");
  });

  it("has no accessibility violations", async () => {
    const { container } = render(<Badge>label</Badge>);
    const results = await axe(container);
    expect(results).toHaveNoViolations();
  });
});
