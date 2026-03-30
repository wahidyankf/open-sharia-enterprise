import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";

const { Given, Then } = createBdd();

Given("the header component is rendered", async ({ page }) => {
  await page.goto("/");
});

Then("the header contains a link to {string} at {string}", async ({ page }, text: string, href: string) => {
  const link = page.getByRole("link", { name: text });
  await expect(link.first()).toBeVisible();
  // Accept both /updates and /updates/ (trailing slash may vary)
  const actual = await link.first().getAttribute("href");
  expect(actual!.replace(/\/$/, "")).toBe(href.replace(/\/$/, ""));
});

Then("the header contains an external link to {string}", async ({ page }, text: string) => {
  const link = page.getByRole("link", { name: new RegExp(text, "i") });
  await expect(link.first()).toBeVisible();
});

Given("the about page is rendered with breadcrumbs", async ({ page }) => {
  await page.goto("/about/");
});

Then("the breadcrumb shows {string} linking to {string}", async ({ page }, text: string, href: string) => {
  const breadcrumb = page.getByRole("navigation", { name: /breadcrumb/i });
  const link = breadcrumb.getByRole("link", { name: text });
  await expect(link).toBeVisible();
  const actual = await link.getAttribute("href");
  expect(actual!.replace(/\/$/, "")).toBe(href.replace(/\/$/, ""));
});

Then("the breadcrumb shows {string} as the current page", async ({ page }, text: string) => {
  const breadcrumb = page.getByRole("navigation", { name: /breadcrumb/i });
  await expect(breadcrumb.getByText(text)).toBeVisible();
});

Given("an update detail page is rendered with adjacent updates", async ({ page }) => {
  // Navigate to a known update that has both prev and next
  await page.goto("/updates/");
  // Click on the first update link that is likely in the middle
  const updateLinks = page.getByRole("link").filter({ hasText: /phase/i });
  const count = await updateLinks.count();
  if (count > 1) {
    // Click the second link (likely has both prev and next)
    await updateLinks.nth(1).click();
  } else if (count > 0) {
    await updateLinks.first().click();
  }
  await page.waitForLoadState("networkidle");
});

Then("a {string} link is displayed with the previous update title", async ({ page }, _label: string) => {
  const prevLink = page.getByRole("link", { name: /prev|previous|←/i });
  await expect(prevLink.first()).toBeVisible();
});

Then("a {string} link is displayed with the next update title", async ({ page }, _label: string) => {
  const nextLink = page.getByRole("link", { name: /next|→/i });
  await expect(nextLink.first()).toBeVisible();
});
