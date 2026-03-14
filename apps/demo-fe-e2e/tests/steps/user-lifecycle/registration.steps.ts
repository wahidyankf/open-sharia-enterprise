import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";

const { Then } = createBdd();

Then("a success message about account creation should be displayed", async ({ page }) => {
  await expect(page.getByRole("alert").or(page.getByText(/registered|account created|success/i))).toBeVisible();
});

Then("no password value should be visible on the page", async ({ page }) => {
  const content = await page.content();
  expect(content).not.toContain("Str0ng#Pass1");
});

Then("an error message about duplicate username should be displayed", async ({ page }) => {
  await expect(page.getByRole("alert").or(page.getByText(/already exists|duplicate|taken/i))).toBeVisible();
});

Then("a validation error for the email field should be displayed", async ({ page }) => {
  await expect(page.getByText(/invalid email|email.*invalid|valid email/i)).toBeVisible();
});
