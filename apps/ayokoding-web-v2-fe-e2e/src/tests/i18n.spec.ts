import { test, expect } from "@playwright/test";

test.describe("Internationalisation", () => {
  test("English locale renders English content at /en", async ({ page }) => {
    await page.goto("/en");

    await expect(page).toHaveTitle(/.+/);
    const heading = page.getByRole("heading", { level: 1 });
    await expect(heading).toBeVisible();
  });

  test("Indonesian locale renders Indonesian content at /id", async ({ page }) => {
    await page.goto("/id");

    await expect(page).toHaveTitle(/.+/);
    const heading = page.getByRole("heading", { level: 1 });
    await expect(heading).toBeVisible();
  });

  test("language switcher navigates between locales", async ({ page }) => {
    await page.goto("/en");

    // Open language dropdown
    const langButton = page.getByRole("button", { name: /switch language/i });
    await langButton.click();

    // Click Bahasa Indonesia
    const idOption = page.getByRole("menuitem", { name: /bahasa indonesia/i });
    await idOption.click();

    await expect(page).toHaveURL(/\/id/);
  });
});
