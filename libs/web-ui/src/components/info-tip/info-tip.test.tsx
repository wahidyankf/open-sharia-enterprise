import { render, screen, fireEvent } from "@testing-library/react";
import { describe, it, expect } from "vitest";
import { InfoTip } from "./info-tip";

describe("InfoTip", () => {
  it("renders trigger button", () => {
    render(<InfoTip title="Volume" text="Adjust the volume" />);
    expect(screen.getByRole("button", { name: "Volume" })).toBeTruthy();
  });

  it("sheet not visible initially", () => {
    render(<InfoTip title="Volume" text="Adjust the volume" />);
    expect(screen.queryByText("Adjust the volume")).toBeNull();
  });

  it("click trigger opens sheet with text", () => {
    render(<InfoTip title="Volume" text="Adjust the volume" />);
    fireEvent.click(screen.getByRole("button", { name: "Volume" }));
    expect(screen.getByText("Adjust the volume")).toBeTruthy();
  });

  it("Got it button closes sheet", () => {
    render(<InfoTip title="Volume" text="Adjust the volume" />);
    fireEvent.click(screen.getByRole("button", { name: "Volume" }));
    fireEvent.click(screen.getByRole("button", { name: /got it/i }));
    expect(screen.queryByText("Adjust the volume")).toBeNull();
  });
});
