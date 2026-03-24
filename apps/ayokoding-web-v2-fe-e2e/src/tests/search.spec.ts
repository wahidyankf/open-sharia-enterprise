import { test, expect } from "@playwright/test";

test.describe("Search", () => {
  test("opens search dialog with keyboard shortcut", async ({ page }) => {
    await page.goto("/en");

    // Trigger the search dialog with Cmd+K (Meta+K on macOS, Control+K on other platforms)
    await page.keyboard.press("Meta+K");

    const searchDialog = page.getByRole("dialog");
    await expect(searchDialog).toBeVisible();
  });

  test("displays results after typing a query", async ({ page }) => {
    await page.goto("/en");

    await page.keyboard.press("Meta+K");

    const searchDialog = page.getByRole("dialog");
    await expect(searchDialog).toBeVisible();

    // Type a query that is expected to match existing content
    const searchInput = searchDialog.getByRole("searchbox");
    await searchInput.fill("programming");

    // Results list must appear
    const results = searchDialog.getByRole("listbox");
    await expect(results).toBeVisible();
    await expect(results.getByRole("option").first()).toBeVisible();
  });

  test("closes search dialog on Escape", async ({ page }) => {
    await page.goto("/en");

    await page.keyboard.press("Meta+K");

    const searchDialog = page.getByRole("dialog");
    await expect(searchDialog).toBeVisible();

    await page.keyboard.press("Escape");
    await expect(searchDialog).toBeHidden();
  });
});
