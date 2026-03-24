import { test, expect } from "@playwright/test";

test.describe("Search", () => {
  test("opens search dialog with keyboard shortcut", async ({ page }) => {
    await page.goto("/en");

    // Use ControlOrMeta+K for cross-platform support
    await page.keyboard.press("ControlOrMeta+k");

    const searchDialog = page.getByRole("dialog");
    await expect(searchDialog).toBeVisible({ timeout: 5000 });
  });

  test("displays results after typing a query", async ({ page }) => {
    await page.goto("/en");

    // Click search button to open dialog (more reliable than keyboard shortcut in CI)
    await page
      .getByRole("button", { name: /search/i })
      .first()
      .click();

    const searchDialog = page.getByRole("dialog");
    await expect(searchDialog).toBeVisible();

    const searchInput = searchDialog.getByRole("combobox");
    await searchInput.fill("programming");

    // Results list must appear (search index may need to build on first query)
    const results = searchDialog.getByRole("listbox");
    await expect(results).toBeVisible({ timeout: 15000 });
    await expect(results.getByRole("option").first()).toBeVisible({ timeout: 15000 });
  });

  test("closes search dialog on Escape", async ({ page }) => {
    await page.goto("/en");

    // Click search button to open dialog
    await page
      .getByRole("button", { name: /search/i })
      .first()
      .click();

    const searchDialog = page.getByRole("dialog");
    await expect(searchDialog).toBeVisible();

    await page.keyboard.press("Escape");
    await expect(searchDialog).toBeHidden();
  });
});
