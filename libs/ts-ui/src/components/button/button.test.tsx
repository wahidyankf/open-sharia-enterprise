import { render, screen } from "@testing-library/react";
import { axe } from "vitest-axe";
import { describe, it, expect } from "vitest";

import { Button, buttonVariants } from "./button";

describe("Button", () => {
  it("renders with default variant and size", () => {
    render(<Button>Click me</Button>);
    const button = screen.getByRole("button", { name: "Click me" });
    expect(button).toBeDefined();
    expect(button.getAttribute("data-slot")).toBe("button");
    expect(button.getAttribute("data-variant")).toBe("default");
    expect(button.getAttribute("data-size")).toBe("default");
  });

  it("renders all variant types without crashing", () => {
    const variants = ["default", "destructive", "outline", "secondary", "ghost", "link"] as const;
    for (const variant of variants) {
      const { unmount } = render(<Button variant={variant}>{variant}</Button>);
      expect(screen.getByRole("button", { name: variant })).toBeDefined();
      unmount();
    }
  });

  it("renders all size types without crashing", () => {
    const sizes = ["default", "xs", "sm", "lg", "icon", "icon-xs", "icon-sm", "icon-lg"] as const;
    for (const size of sizes) {
      const { unmount } = render(
        <Button size={size} aria-label={`button-${size}`}>
          X
        </Button>,
      );
      expect(screen.getByRole("button", { name: `button-${size}` })).toBeDefined();
      unmount();
    }
  });

  it("forwards className via cn()", () => {
    render(<Button className="custom-class">Test</Button>);
    expect(screen.getByRole("button").className).toContain("custom-class");
  });

  it("supports disabled state", () => {
    render(<Button disabled>Disabled</Button>);
    expect(screen.getByRole("button").hasAttribute("disabled")).toBe(true);
  });

  it("renders as child element when asChild is true", () => {
    render(
      <Button asChild>
        <a href="/test">Link Button</a>
      </Button>,
    );
    const link = screen.getByRole("link", { name: "Link Button" });
    expect(link).toBeDefined();
    expect(link.getAttribute("href")).toBe("/test");
  });

  it("exports buttonVariants for external use", () => {
    expect(typeof buttonVariants).toBe("function");
    const classes = buttonVariants({ variant: "destructive", size: "sm" });
    expect(classes).toContain("bg-destructive");
  });

  it("has no accessibility violations", async () => {
    const { container } = render(<Button>Accessible Button</Button>);
    const results = await axe(container);
    expect(results).toHaveNoViolations();
  });

  it("icon-only button with aria-label has no a11y violations", async () => {
    const { container } = render(
      <Button size="icon" aria-label="Close">
        X
      </Button>,
    );
    const results = await axe(container);
    expect(results).toHaveNoViolations();
  });
});
