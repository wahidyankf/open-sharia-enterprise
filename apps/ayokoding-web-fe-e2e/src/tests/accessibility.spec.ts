import { test, expect } from "@playwright/test";

test.describe("Accessibility", () => {
  test("skip-to-content link is present in the DOM", async ({ page }) => {
    await page.goto("/en");

    // The skip link allows keyboard users to bypass repetitive navigation
    const skipLink = page.getByRole("link", { name: /skip.*(to |to main )?content/i });
    await expect(skipLink).toBeAttached();
  });

  test("skip-to-content link becomes visible on focus", async ({ page }) => {
    await page.goto("/en");

    // Tab once to bring focus to the skip link
    await page.keyboard.press("Tab");

    const skipLink = page.getByRole("link", { name: /skip.*(to |to main )?content/i });
    await expect(skipLink).toBeVisible();
  });

  test("navigation buttons have accessible ARIA labels", async ({ page }) => {
    await page.goto("/en");

    // Every interactive button must have an accessible name for screen readers
    const buttons = page.getByRole("button");
    const count = await buttons.count();

    for (let i = 0; i < count; i++) {
      const button = buttons.nth(i);
      const accessibleName = await button.getAttribute("aria-label");
      const innerText = await button.innerText();

      // A button must expose an accessible name via aria-label or visible text
      const hasAccessibleName =
        (accessibleName !== null && accessibleName.trim().length > 0) || innerText.trim().length > 0;

      expect(hasAccessibleName, `Button at index ${i} lacks an accessible name`).toBe(true);
    }
  });

  test("images have alt text", async ({ page }) => {
    await page.goto("/en/learn/overview");

    // Only check actual <img> elements (not SVG icons from lucide-react)
    const images = page.locator("img[src]");
    const count = await images.count();

    for (let i = 0; i < count; i++) {
      const img = images.nth(i);
      await expect(img).toHaveAttribute("alt", /.*/);
    }
  });
});
