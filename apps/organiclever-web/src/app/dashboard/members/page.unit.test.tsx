import React from "react";
import { render, screen, cleanup, waitFor, within } from "@testing-library/react/pure";
import userEvent from "@testing-library/user-event";
import { vi, describe, it, expect, beforeEach, afterEach } from "vitest";
import { http, HttpResponse } from "msw";
import { AUTHENTICATED } from "../../../test/helpers/auth-mock";
import { server } from "../../../test/server";

vi.mock("next/navigation", () => ({
  useRouter: vi.fn(() => ({ push: vi.fn() })),
  usePathname: vi.fn(() => "/dashboard/members"),
}));
vi.mock("next/link", () => ({
  default: ({ href, children, className }: { href: string; children: React.ReactNode; className?: string }) =>
    React.createElement("a", { href, className }, children),
}));
vi.mock("@/app/contexts/auth-context", () => ({ useAuth: vi.fn() }));
vi.mock("@/components/Navigation", () => ({ Navigation: () => null }));
vi.mock("@/components/Breadcrumb", () => ({ default: () => null }));

import { useAuth } from "@/app/contexts/auth-context";
import { useRouter } from "next/navigation";
import MembersPage from "./page";

describe("MembersPage", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.mocked(useAuth).mockReturnValue({ ...AUTHENTICATED });
    vi.mocked(useRouter).mockReturnValue({ push: vi.fn() } as unknown as ReturnType<typeof useRouter>);
  });

  afterEach(() => {
    cleanup();
  });

  it("handles delete API failure gracefully", async () => {
    server.use(http.delete("/api/members/:id", () => new HttpResponse(null, { status: 500 })));

    const consoleSpy = vi.spyOn(console, "error").mockImplementation(() => {});
    render(<MembersPage />);
    await screen.findAllByRole("row");

    const rows = screen.getAllByRole("row");
    const aliceRow = rows.find((row) => within(row).queryByText("Alice Johnson") !== null);
    const actionBtns = within(aliceRow!).getAllByRole("button");

    const user = userEvent.setup();
    await user.click(actionBtns[2]!);
    await screen.findByRole("alertdialog");

    const dialog = screen.getByRole("alertdialog");
    await user.click(within(dialog).getByRole("button", { name: /^delete$/i }));

    await waitFor(() => {
      expect(consoleSpy).toHaveBeenCalledWith("Error deleting member:", expect.any(Error));
    });

    consoleSpy.mockRestore();
  });

  it("updates role, email, and github fields in the edit dialog", async () => {
    render(<MembersPage />);
    await screen.findAllByRole("row");

    const rows = screen.getAllByRole("row");
    const aliceRow = rows.find((row) => within(row).queryByText("Alice Johnson") !== null);
    const actionBtns = within(aliceRow!).getAllByRole("button");

    const user = userEvent.setup();
    await user.click(actionBtns[1]!);
    await screen.findByRole("dialog");

    const dialog = screen.getByRole("dialog");
    const roleInput = within(dialog).getByLabelText(/^role$/i) as HTMLInputElement;
    const emailInput = within(dialog).getByLabelText(/^email$/i) as HTMLInputElement;
    const githubInput = within(dialog).getByLabelText(/^github$/i) as HTMLInputElement;

    await user.clear(roleInput);
    await user.type(roleInput, "Staff Engineer");
    expect(roleInput.value).toBe("Staff Engineer");

    await user.clear(emailInput);
    await user.type(emailInput, "updated@example.com");
    expect(emailInput.value).toBe("updated@example.com");

    await user.clear(githubInput);
    await user.type(githubInput, "aliceupdated");
    expect(githubInput.value).toBe("aliceupdated");
  });
});
