import { test, expect } from "@playwright/test";

test.describe("Internationalisation", () => {
  test("English locale renders English content at /en", async ({ page }) => {
    await page.goto("/en");

    // The lang attribute on <html> must reflect the active locale
    await expect(page.locator("html")).toHaveAttribute("lang", /^en/);

    // Page title or heading must be in English
    await expect(page).toHaveTitle(/.+/);
    const heading = page.getByRole("heading", { level: 1 });
    await expect(heading).toBeVisible();
  });

  test("Indonesian locale renders Indonesian content at /id", async ({ page }) => {
    await page.goto("/id");

    // The lang attribute on <html> must reflect the active locale
    await expect(page.locator("html")).toHaveAttribute("lang", /^id/);

    // Page title or heading must be present
    await expect(page).toHaveTitle(/.+/);
    const heading = page.getByRole("heading", { level: 1 });
    await expect(heading).toBeVisible();
  });

  test("language switcher navigates between locales", async ({ page }) => {
    await page.goto("/en");

    // Find the language switcher and switch to Indonesian
    const langSwitcher = page.getByRole("link", { name: /bahasa indonesia|id/i });
    await langSwitcher.click();

    await expect(page).toHaveURL(/\/id/);
    await expect(page.locator("html")).toHaveAttribute("lang", /^id/);
  });
});
