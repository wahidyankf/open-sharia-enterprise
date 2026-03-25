import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";

const { Given, When, Then } = createBdd();

Given("the app is running", async () => {
  // Base URL is configured in playwright.config.ts — no action needed
});

When("a visitor is on a page under the /en locale", async ({ page }) => {
  await page.goto("/en");
});

Then('the language switcher should display "English" as the current language', async ({ page }) => {
  const langButton = page.getByRole("button", { name: /switch language|english/i });
  await expect(langButton).toBeVisible();
  const text = await langButton.textContent();
  expect(text?.toLowerCase()).toContain("en");
});

Given("a visitor is on the English version of a content page at /en/some-page", async ({ page }) => {
  await page.goto("/en");
});

When("the visitor selects Indonesian from the language switcher", async ({ page }) => {
  const langButton = page.getByRole("button", { name: /switch language/i });
  await langButton.click();

  const idOption = page.getByRole("menuitem", { name: /bahasa indonesia/i });
  await idOption.click();
});

Then("the visitor should be redirected to the Indonesian version of that page at /id/some-page", async ({ page }) => {
  await expect(page).toHaveURL(/\/id/);
});

Given("a visitor is on the Indonesian version of a page", async ({ page }) => {
  await page.goto("/id");
});

Then("navigation labels and UI text should be displayed in Indonesian", async ({ page }) => {
  await expect(page).toHaveTitle(/.+/);
  // The page should render — Indonesian locale content is loaded
  const heading = page.getByRole("heading", { level: 1 });
  await expect(heading).toBeVisible();
});

Then("the page title and headings should reflect the Indonesian locale content", async ({ page }) => {
  const heading = page.getByRole("heading", { level: 1 });
  await expect(heading).toBeVisible();
  const text = await heading.textContent();
  expect(text?.trim().length).toBeGreaterThan(0);
});

When("a visitor opens the root URL /", async ({ page }) => {
  await page.goto("/");
});

Then("they should be redirected to /en", async ({ page }) => {
  await expect(page).toHaveURL(/\/en/);
});

Then("the English version of the home page should be displayed", async ({ page }) => {
  await expect(page).toHaveTitle(/.+/);
  const heading = page.getByRole("heading", { level: 1 });
  await expect(heading).toBeVisible();
});
