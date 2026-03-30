import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";

const { Given, Then } = createBdd();

Given("the landing page is rendered", async ({ page }) => {
  await page.goto("/");
});

Then("the hero section displays the title {string}", async ({ page }, title: string) => {
  await expect(page.getByRole("heading", { name: title })).toBeVisible();
});

Then("the hero section displays a description of the platform mission", async ({ page }) => {
  const main = page.getByRole("main");
  await expect(main).toBeVisible();
  const text = await main.textContent();
  expect(text!.length).toBeGreaterThan(0);
});

Then("the hero section contains a {string} link to {string}", async ({ page }, linkText: string, href: string) => {
  const link = page.getByRole("link", { name: linkText });
  await expect(link).toBeVisible();
  // Accept both /about and /about/ (trailing slash may vary)
  const actual = await link.getAttribute("href");
  expect(actual!.replace(/\/$/, "")).toBe(href.replace(/\/$/, ""));
});

Then("the hero section contains a {string} link", async ({ page }, linkText: string) => {
  const link = page.getByRole("link", { name: new RegExp(linkText, "i") });
  await expect(link.first()).toBeVisible();
});

Then("a GitHub icon link is visible", async ({ page }) => {
  const githubLinks = page.getByRole("link", { name: /github/i });
  await expect(githubLinks.first()).toBeVisible();
});

Then("an RSS feed icon link is visible", async ({ page }) => {
  const rssLinks = page.getByRole("link", { name: /rss|feed/i });
  await expect(rssLinks.first()).toBeVisible();
});
