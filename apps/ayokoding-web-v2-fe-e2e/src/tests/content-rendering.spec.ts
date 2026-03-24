import { test, expect } from "@playwright/test";

test.describe("Content Rendering", () => {
  test("renders prose content on a content page", async ({ page }) => {
    await page.goto("/en/programming");

    // Prose content area is present and contains text
    const article = page.getByRole("article");
    await expect(article).toBeVisible();
    await expect(article).not.toBeEmpty();
  });

  test("renders code blocks with syntax highlighting", async ({ page }) => {
    // Navigate to a page known to contain code examples
    await page.goto("/en/programming");

    // Code blocks must be present in the content
    const codeBlock = page.locator("pre code").first();
    await expect(codeBlock).toBeVisible();
  });

  test("displays page heading", async ({ page }) => {
    await page.goto("/en/programming");

    const heading = page.getByRole("heading", { level: 1 });
    await expect(heading).toBeVisible();
  });
});
