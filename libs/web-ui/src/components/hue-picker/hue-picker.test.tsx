import { render, screen, fireEvent } from "@testing-library/react";
import { vi, describe, it, expect } from "vitest";
import { HuePicker, HUES } from "./hue-picker";

describe("HuePicker", () => {
  it("renders 6 swatch buttons", () => {
    render(<HuePicker value="teal" onChange={() => {}} />);
    expect(screen.getAllByRole("button")).toHaveLength(6);
  });

  it("calls onChange when swatch clicked", () => {
    const onChange = vi.fn();
    render(<HuePicker value="teal" onChange={onChange} />);
    fireEvent.click(screen.getByRole("button", { name: "sage" }));
    expect(onChange).toHaveBeenCalledWith("sage");
  });

  it("selected swatch has aria-pressed true", () => {
    render(<HuePicker value="teal" onChange={() => {}} />);
    expect(screen.getByRole("button", { name: "teal" }).getAttribute("aria-pressed")).toBe("true");
  });

  it("unselected swatches have aria-pressed false", () => {
    render(<HuePicker value="teal" onChange={() => {}} />);
    expect(screen.getByRole("button", { name: "sage" }).getAttribute("aria-pressed")).toBe("false");
  });

  it("renders buttons for all HUES", () => {
    render(<HuePicker value="teal" onChange={() => {}} />);
    HUES.forEach((hue) => {
      expect(screen.getByRole("button", { name: hue })).toBeTruthy();
    });
  });
});
