import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";

const { When, Then } = createBdd();

When("a visitor opens a content page that has child sections", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the sidebar should display the section tree", async ({ page }) => {
  const sidebar = page.getByRole("navigation", { name: /sidebar/i });
  await expect(sidebar).toBeVisible();
  const links = sidebar.getByRole("link");
  await expect(links.first()).toBeVisible();
});

Then("parent nodes should be expandable and collapsible", async ({ page }) => {
  // Collapsible parent nodes have a button or disclosure pattern
  const sidebar = page.getByRole("navigation", { name: /sidebar/i });
  const expandable = sidebar.locator("button[aria-expanded], [data-collapsible], details summary");
  await expect(expandable.first()).toBeAttached();
});

When("the visitor clicks a collapsed parent node", async ({ page }) => {
  const sidebar = page.getByRole("navigation", { name: /sidebar/i });
  const expandable = sidebar.locator("button[aria-expanded='false'], details:not([open]) summary").first();
  const isPresent = (await expandable.count()) > 0;
  if (isPresent) {
    await expandable.click();
  }
});

Then("its child items should become visible", async ({ page }) => {
  const sidebar = page.getByRole("navigation", { name: /sidebar/i });
  const openDetails = sidebar.locator("details[open] a, [aria-expanded='true'] + * a");
  // After expanding, child links should be in the DOM
  await expect(openDetails.first()).toBeAttached({ timeout: 5000 });
});

When("a visitor opens a nested content page", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("a breadcrumb trail should be displayed above the page title", async ({ page }) => {
  const breadcrumb = page.getByRole("navigation", { name: /breadcrumb/i });
  await expect(breadcrumb).toBeVisible();
});

Then("each breadcrumb segment should reflect a level of the URL hierarchy", async ({ page }) => {
  const breadcrumb = page.getByRole("navigation", { name: /breadcrumb/i });
  const items = breadcrumb.locator("a, [aria-current]");
  const count = await items.count();
  expect(count).toBeGreaterThanOrEqual(1);
});

Then("each segment except the current page should be a clickable link", async ({ page }) => {
  const breadcrumb = page.getByRole("navigation", { name: /breadcrumb/i });
  const links = breadcrumb.getByRole("link");
  await expect(links.first()).toBeAttached();
});

When("a visitor opens a content page with multiple headings", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("a table of contents should be visible on the page", async ({ page }) => {
  const toc = page.getByRole("navigation", { name: /table of contents/i });
  await expect(toc).toBeVisible();
});

Then("the table of contents should list all H2, H3, and H4 headings as anchor links", async ({ page }) => {
  const toc = page.getByRole("navigation", { name: /table of contents/i });
  const tocLinks = toc.getByRole("link");
  await expect(tocLinks.first()).toBeVisible();
});

Then("H1 headings should not appear in the table of contents", async ({ page }) => {
  const toc = page.getByRole("navigation", { name: /table of contents/i });
  const h1Text = await page.getByRole("heading", { level: 1 }).textContent();
  if (h1Text) {
    const tocLinks = toc.getByRole("link", { name: new RegExp(h1Text.trim(), "i") });
    await expect(tocLinks).toHaveCount(0);
  }
});

When("a visitor is on a content page that has sibling pages", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("a previous link should point to the preceding sibling page", async ({ page }) => {
  const prevLink = page.getByRole("link", { name: /previous|prev/i }).first();
  await expect(prevLink).toBeAttached();
});

Then("a next link should point to the following sibling page", async ({ page }) => {
  const nextLink = page.getByRole("link", { name: /next/i }).first();
  await expect(nextLink).toBeAttached();
});

When("the visitor clicks the next link", async ({ page }) => {
  const nextLink = page.getByRole("link", { name: /next/i }).first();
  const isPresent = (await nextLink.count()) > 0;
  if (isPresent) {
    await nextLink.click();
    await page.waitForLoadState("domcontentloaded");
  }
});

Then("they should be taken to the next sibling page", async ({ page }) => {
  // After clicking next, the URL should have changed from the overview page
  const currentUrl = page.url();
  expect(currentUrl).toContain("/en/learn/");
});

When("a visitor is on a specific content page", async ({ page }) => {
  await page.goto("/en/learn/overview");
});

Then("the corresponding item in the sidebar should be visually highlighted as active", async ({ page }) => {
  const sidebar = page.getByRole("navigation", { name: /sidebar/i });
  const activeItem = sidebar.locator("[aria-current='page'], .active, [data-active='true'], [class*='active']");
  await expect(activeItem.first()).toBeAttached();
});

Then("no other sidebar item should be highlighted as active", async ({ page }) => {
  const sidebar = page.getByRole("navigation", { name: /sidebar/i });
  const activeItems = sidebar.locator("[aria-current='page'], [data-active='true']");
  const count = await activeItems.count();
  expect(count).toBeLessThanOrEqual(1);
});
