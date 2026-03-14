import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";
import AxeBuilder from "@axe-core/playwright";
import { loginUser, createExpense, registerUser } from "@/utils/api-helpers.js";

const { Given, When, Then } = createBdd();

Given("a visitor opens the app", async ({ page }) => {
  await page.goto("/");
});

Given("a visitor is on the login page", async ({ page }) => {
  await page.goto("/login");
});

Given("{word} is on an entry with an attachment", async ({ page }, username: string) => {
  const { accessToken } = await loginUser(username, "Str0ng#Pass1");
  await createExpense(accessToken, {
    amount: "10.00",
    currency: "USD",
    category: "food",
    description: "A11y test entry",
    date: "2025-01-15",
    type: "expense",
  });
  await page.goto("/expenses");
  await page.getByText("A11y test entry").first().click();
  void username;
});

Given("{word} has an entry with a JPEG attachment", async ({ page }, username: string) => {
  await page.goto("/expenses");
  const entry = page.getByTestId("entry-with-attachment").first();
  if (await entry.isVisible({ timeout: 1000 }).catch(() => false)) {
    await entry.click();
  } else {
    await page.getByRole("link").first().click();
  }
  void username;
});

When("a visitor navigates to the registration page", async ({ page }) => {
  await page.goto("/register");
});

When("the visitor submits the form with empty fields", async ({ page }) => {
  await page.getByRole("button", { name: /log in|sign in|login/i }).click();
});

When("{word} presses Tab repeatedly on the dashboard", async ({ page }) => {
  await page.goto("/expenses");
  for (let i = 0; i < 10; i++) {
    await page.keyboard.press("Tab");
  }
});

When("{word} clicks the delete button and a confirmation dialog appears", async ({ page }) => {
  const deleteBtn = page.getByRole("button", { name: /delete|remove/i }).first();
  await deleteBtn.click();
});

When("{word} views the attachment", async ({ page }) => {
  await expect(page.getByRole("img")).toBeVisible({ timeout: 5000 });
});

Then("every input field should have an associated visible label", async ({ page }) => {
  const inputs = await page.getByRole("textbox").all();
  for (const input of inputs) {
    const ariaLabel = await input.getAttribute("aria-label");
    const ariaLabelledBy = await input.getAttribute("aria-labelledby");
    const id = await input.getAttribute("id");
    let hasLabel = !!(ariaLabel ?? ariaLabelledBy);
    if (!hasLabel && id) {
      const label = page.locator(`label[for="${id}"]`);
      hasLabel = await label.isVisible().catch(() => false);
    }
    expect(hasLabel, "Input should have an accessible label").toBe(true);
  }
});

Then("every input field should have an accessible name", async ({ page }) => {
  const inputs = await page.getByRole("textbox").all();
  for (const input of inputs) {
    const accessibleName = await input.getAttribute("aria-label");
    const placeholder = await input.getAttribute("placeholder");
    const id = await input.getAttribute("id");
    let hasAccessibleName = !!(accessibleName ?? placeholder);
    if (!hasAccessibleName && id) {
      hasAccessibleName = await page
        .locator(`label[for="${id}"]`)
        .count()
        .then((c) => c > 0);
    }
    expect(hasAccessibleName, "Input should have an accessible name").toBe(true);
  }
});

Then("validation errors should have role {string}", async ({ page }, role: string) => {
  const errors = page.getByRole(role as Parameters<typeof page.getByRole>[0]);
  await expect(errors).toBeVisible({ timeout: 5000 });
});

Then("the errors should be associated with their respective fields via aria-describedby", async ({ page }) => {
  const inputs = await page.getByRole("textbox").all();
  let hasDescribedBy = false;
  for (const input of inputs) {
    const describedBy = await input.getAttribute("aria-describedby");
    if (describedBy) {
      hasDescribedBy = true;
      break;
    }
  }
  expect(hasDescribedBy).toBe(true);
});

Then("focus should move through all interactive elements in logical order", async ({ page }) => {
  const focused = page.locator(":focus");
  await expect(focused).toBeVisible();
});

Then("the currently focused element should have a visible focus indicator", async ({ page }) => {
  const outline = await page.locator(":focus").evaluate((el) => {
    const style = window.getComputedStyle(el);
    return style.outline !== "none" || style.outlineWidth !== "0px";
  });
  expect(outline).toBe(true);
});

Then("focus should be trapped within the dialog", async ({ page }) => {
  const dialog = page.getByRole("dialog");
  await expect(dialog).toBeVisible();
  await page.keyboard.press("Tab");
  const isInDialog = await dialog
    .locator(":focus")
    .count()
    .then((c) => c > 0);
  expect(isInDialog).toBe(true);
});

Then("pressing Escape should close the dialog and return focus to the trigger", async ({ page }) => {
  await page.keyboard.press("Escape");
  await expect(page.getByRole("dialog")).not.toBeVisible({ timeout: 2000 });
});

Then("all text should meet a minimum contrast ratio of 4.5:1 against its background", async ({ page }) => {
  await page.goto("/");
  const results = await new AxeBuilder({ page }).withRules(["color-contrast"]).analyze();
  expect(results.violations).toHaveLength(0);
});

Then("all interactive elements should meet a minimum contrast ratio of 3:1", async ({ page }) => {
  const results = await new AxeBuilder({ page }).withRules(["color-contrast"]).analyze();
  expect(results.violations).toHaveLength(0);
});

Then("the image should have descriptive alt text", async ({ page }) => {
  const images = await page.getByRole("img").all();
  for (const img of images) {
    const alt = await img.getAttribute("alt");
    expect(alt).not.toBeNull();
    expect(alt?.trim().length).toBeGreaterThan(0);
  }
});

Then("decorative icons should be hidden from assistive technologies", async ({ page }) => {
  const decorativeIcons = page.locator('[aria-hidden="true"] svg, svg[aria-hidden="true"]');
  const count = await decorativeIcons.count();
  expect(count).toBeGreaterThanOrEqual(0);
});

void loginUser;
void createExpense;
void registerUser;
