import { test, expect, type Page } from "@playwright/test";

async function navigateToContentPage(page: Page): Promise<void> {
  await page.goto("/en/learn/overview");
}

test.describe("Responsive Layout - Desktop", () => {
  test.use({ viewport: { width: 1280, height: 800 } });

  test("sidebar is visible on desktop", async ({ page }) => {
    await navigateToContentPage(page);

    const sidebar = page.getByRole("navigation", { name: /sidebar/i });
    await expect(sidebar).toBeVisible();
  });

  test("hamburger menu button is not visible on desktop", async ({ page }) => {
    await navigateToContentPage(page);

    // The mobile menu toggle should not be visible at desktop width
    const hamburger = page.getByRole("button", { name: /menu/i });
    await expect(hamburger).toBeHidden();
  });
});

test.describe("Responsive Layout - Mobile", () => {
  test.use({ viewport: { width: 375, height: 812 } });

  test("hamburger menu button is visible on mobile", async ({ page }) => {
    await navigateToContentPage(page);

    const hamburger = page.getByRole("button", { name: /menu/i });
    await expect(hamburger).toBeVisible();
  });

  test("sidebar is hidden by default on mobile", async ({ page }) => {
    await navigateToContentPage(page);

    // The sidebar should be collapsed until the hamburger is tapped
    const sidebar = page.getByRole("navigation", { name: /sidebar/i });
    await expect(sidebar).toBeHidden();
  });
});
