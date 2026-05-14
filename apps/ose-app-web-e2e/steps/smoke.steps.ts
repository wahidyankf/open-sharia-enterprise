/**
 * Step definitions for the OSE Application Web smoke feature.
 *
 * Covers: specs/apps/ose-app/behavior/web/gherkin/smoke.feature
 */
import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";

const { Given, When, Then } = createBdd();

Given("the ose-app-web dev server is running", async () => {
  // Dev server expected at WEB_BASE_URL (http://localhost:3300).
  // No-op: the test suite assumes the server is already running.
});

When("I navigate to {string}", async ({ page }, path: string) => {
  await page.goto(`${process.env["WEB_BASE_URL"] ?? "http://localhost:3300"}${path}`);
});

Then("I see the heading {string}", async ({ page }, heading: string) => {
  const h1 = page.getByRole("heading", { level: 1 });
  await expect(h1).toHaveText(heading);
});
