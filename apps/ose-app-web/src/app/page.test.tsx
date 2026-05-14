import { render, screen, cleanup } from "@testing-library/react";
import { describe, it, expect, afterEach } from "vitest";
import Home from "./page";

afterEach(() => {
  cleanup();
});

describe("Home page", () => {
  it("renders OSE Application heading", () => {
    render(<Home />);
    expect(screen.getByRole("heading", { name: /ose application/i })).toBeDefined();
  });

  it("renders at least one button", () => {
    render(<Home />);
    expect(screen.getAllByRole("button").length).toBeGreaterThan(0);
  });
});
