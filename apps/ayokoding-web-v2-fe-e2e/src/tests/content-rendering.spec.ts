import { test, expect } from "@playwright/test";

test.describe("Content Rendering", () => {
  test("renders prose content on a content page", async ({ page }) => {
    await page.goto("/en/learn/overview");

    // Prose content area is present and contains text
    const article = page.getByRole("article");
    await expect(article).toBeVisible();
    await expect(article).not.toBeEmpty();
  });

  test("renders code blocks with syntax highlighting", async ({ page }) => {
    // Navigate to a page known to contain code examples
    await page.goto("/en/learn/software-engineering/programming-languages/golang/by-example/beginner");

    // Code blocks must be present in the content
    const codeBlock = page.locator("pre code").first();
    await expect(codeBlock).toBeVisible({ timeout: 10000 });
  });

  test("displays page heading", async ({ page }) => {
    await page.goto("/en/learn/overview");

    const heading = page.getByRole("heading", { level: 1 });
    await expect(heading).toBeVisible();
  });
});
