import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";
import { loginUser, createExpense } from "@/utils/api-helpers.js";

const { Given, When, Then } = createBdd();

Given(
  "{word} has created an expense with amount {string}, currency {string}, category {string}, description {string}, date {string}, quantity {float}, and unit {string}",
  async (
    {},
    username: string,
    amount: string,
    currency: string,
    category: string,
    description: string,
    date: string,
    quantity: number,
    unit: string,
  ) => {
    const { accessToken } = await loginUser(username, "Str0ng#Pass1");
    await createExpense(accessToken, {
      amount,
      currency,
      category,
      description,
      date,
      type: "expense",
      quantity,
      unit,
    });
  },
);

When(
  "{word} fills in amount {string}, currency {string}, category {string}, description {string}, date {string}, type {string}, quantity {int}, and unit {string}",
  async (
    { page },
    _username: string,
    amount: string,
    currency: string,
    category: string,
    description: string,
    date: string,
    type: string,
    quantity: number,
    unit: string,
  ) => {
    await page
      .getByRole("textbox", { name: /amount/i })
      .or(page.getByLabel(/amount/i))
      .fill(amount);

    await page
      .getByRole("combobox", { name: /currency/i })
      .or(page.getByLabel(/currency/i))
      .or(page.getByRole("textbox", { name: /currency/i }))
      .fill(currency);

    await page
      .getByRole("combobox", { name: /category/i })
      .or(page.getByLabel(/category/i))
      .or(page.getByRole("textbox", { name: /category/i }))
      .fill(category);

    await page
      .getByRole("textbox", { name: /description/i })
      .or(page.getByLabel(/description/i))
      .fill(description);

    await page.getByRole("textbox", { name: /date/i }).or(page.getByLabel(/date/i)).fill(date);

    if (type) {
      const typeInput = page.getByRole("combobox", { name: /type/i }).or(page.getByLabel(/type/i));
      if (await typeInput.isVisible({ timeout: 1000 }).catch(() => false)) {
        await typeInput.fill(type);
      } else {
        await page.getByRole("radio", { name: new RegExp(type, "i") }).click();
      }
    }

    const quantityInput = page.getByRole("textbox", { name: /quantity/i }).or(page.getByLabel(/quantity/i));
    if (await quantityInput.isVisible({ timeout: 1000 }).catch(() => false)) {
      await quantityInput.fill(String(quantity));
    }

    const unitInput = page
      .getByRole("combobox", { name: /unit/i })
      .or(page.getByLabel(/unit/i))
      .or(page.getByRole("textbox", { name: /unit/i }));
    if (await unitInput.isVisible({ timeout: 1000 }).catch(() => false)) {
      await unitInput.fill(unit);
    }
  },
);

When("{word} leaves the quantity and unit fields empty", async ({ page }) => {
  const quantityInput = page.getByRole("textbox", { name: /quantity/i }).or(page.getByLabel(/quantity/i));
  if (await quantityInput.isVisible({ timeout: 500 }).catch(() => false)) {
    await quantityInput.clear();
  }
  const unitInput = page.getByRole("textbox", { name: /unit/i }).or(page.getByLabel(/unit/i));
  if (await unitInput.isVisible({ timeout: 500 }).catch(() => false)) {
    await unitInput.clear();
  }
});

Then("the quantity should display as {string}", async ({ page }, quantity: string) => {
  await expect(page.getByText(quantity)).toBeVisible();
});

Then("the unit should display as {string}", async ({ page }, unit: string) => {
  await expect(page.getByText(new RegExp(unit, "i"))).toBeVisible();
});

Then("a validation error for the unit field should be displayed", async ({ page }) => {
  await expect(page.getByText(/invalid unit|unsupported unit|unit.*invalid/i)).toBeVisible();
});
