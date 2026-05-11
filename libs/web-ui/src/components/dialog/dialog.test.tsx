import { render, screen } from "@testing-library/react";
import { describe, it, expect } from "vitest";

import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
  DialogDescription,
  DialogFooter,
  DialogTrigger,
} from "./dialog";

describe("Dialog", () => {
  it("renders trigger with data-slot", () => {
    render(
      <Dialog>
        <DialogTrigger>Open</DialogTrigger>
      </Dialog>,
    );
    expect(screen.getByText("Open").getAttribute("data-slot")).toBe("dialog-trigger");
  });

  it("renders open dialog content with data-slots", () => {
    render(
      <Dialog open>
        <DialogContent showCloseButton={false}>
          <DialogHeader>
            <DialogTitle>Title</DialogTitle>
            <DialogDescription>Description</DialogDescription>
          </DialogHeader>
          <DialogFooter>Footer</DialogFooter>
        </DialogContent>
      </Dialog>,
    );
    expect(screen.getByText("Title").getAttribute("data-slot")).toBe("dialog-title");
    expect(screen.getByText("Description").getAttribute("data-slot")).toBe("dialog-description");
    expect(screen.getByText("Footer").closest("[data-slot='dialog-footer']")).toBeDefined();
  });

  it("renders close button by default", () => {
    render(
      <Dialog open>
        <DialogContent>
          <DialogTitle>Title</DialogTitle>
        </DialogContent>
      </Dialog>,
    );
    expect(screen.getByText("Close")).toBeDefined();
  });

  it("hides close button when showCloseButton=false", () => {
    render(
      <Dialog open>
        <DialogContent showCloseButton={false}>
          <DialogTitle>Title</DialogTitle>
        </DialogContent>
      </Dialog>,
    );
    expect(screen.queryByText("Close")).toBeNull();
  });
});
