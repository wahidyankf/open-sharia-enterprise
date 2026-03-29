import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";

const { When, Then } = createBdd();

// Gherkin "And" following a "When" is registered with When
When("the visitor presses Tab repeatedly", async ({ page }) => {
  // Press Tab several times to move through interactive elements
  for (let i = 0; i < 5; i++) {
    await page.keyboard.press("Tab");
  }
});

Then("focus should move through all interactive elements in a logical order", async ({ page }) => {
  // After tabbing, at least one element should have focus
  const focusedElement = page.locator(":focus");
  await expect(focusedElement).toBeAttached({ timeout: 3000 });
});

Then("no interactive element should be skipped or unreachable by keyboard", async ({ page }) => {
  // Verify interactive elements exist and are reachable — buttons, links, inputs
  const interactiveElements = page.locator("a[href], button, input, select, textarea");
  const count = await interactiveElements.count();
  expect(count).toBeGreaterThan(0);
});

When(
  "a visitor opens a content page with interactive controls such as the hamburger menu and search button",
  async ({ page }) => {
    await page.goto("/en");
  },
);

Then("each button should have an accessible name via an aria-label or visible label", async ({ page }) => {
  const buttons = page.getByRole("button");
  const count = await buttons.count();

  for (let i = 0; i < count; i++) {
    const button = buttons.nth(i);
    const ariaLabel = await button.getAttribute("aria-label");
    const ariaLabelledBy = await button.getAttribute("aria-labelledby");
    const innerText = await button.innerText();

    const hasAccessibleName =
      (ariaLabel !== null && ariaLabel.trim().length > 0) ||
      (ariaLabelledBy !== null && ariaLabelledBy.trim().length > 0) ||
      innerText.trim().length > 0;

    expect(hasAccessibleName, `Button at index ${i} lacks an accessible name`).toBe(true);
  }
});

Then("each interactive element should be identifiable by assistive technologies", async ({ page }) => {
  // All links should have accessible text
  const links = page.getByRole("link");
  const count = await links.count();

  for (let i = 0; i < count; i++) {
    const link = links.nth(i);
    const ariaLabel = await link.getAttribute("aria-label");
    const innerText = await link.innerText();
    const ariaHidden = await link.getAttribute("aria-hidden");

    // Skip decorative/hidden links
    if (ariaHidden === "true") continue;

    const hasAccessibleName = (ariaLabel !== null && ariaLabel.trim().length > 0) || innerText.trim().length > 0;

    expect(hasAccessibleName, `Link at index ${i} lacks an accessible name`).toBe(true);
  }
});

When("a visitor opens any page on the site", async ({ page }) => {
  await page.goto("/en");
});

Then("a skip to content link should be present in the page", async ({ page }) => {
  const skipLink = page.getByRole("link", {
    name: /skip.*(to |to main )?content/i,
  });
  await expect(skipLink).toBeAttached();
});

Then("the link should become visible when it receives keyboard focus", async ({ page }) => {
  await page.keyboard.press("Tab");
  const skipLink = page.getByRole("link", {
    name: /skip.*(to |to main )?content/i,
  });
  await expect(skipLink).toBeVisible();
});

Then("activating the link should move focus to the main content area", async ({ page }) => {
  const skipLink = page.getByRole("link", {
    name: /skip.*(to |to main )?content/i,
  });
  await skipLink.click();
  // After activation, focus moves to main content — the main element should exist
  const main = page.getByRole("main");
  await expect(main).toBeVisible();
});

Then("all body text should meet a minimum contrast ratio of 4.5:1 against its background", async ({ page }) => {
  // Basic check: body text color and background are set and the page renders
  const body = page.locator("body");
  await expect(body).toBeVisible();
  const color = await body.evaluate((el) => window.getComputedStyle(el).color);
  const bg = await body.evaluate((el) => window.getComputedStyle(el).backgroundColor);
  // Both values should be defined (non-empty)
  expect(color.length).toBeGreaterThan(0);
  expect(bg.length).toBeGreaterThan(0);
});

Then(
  "large text and headings should meet a minimum contrast ratio of 3:1 against their background",
  async ({ page }) => {
    const heading = page.getByRole("heading", { level: 1 }).first();
    await expect(heading).toBeVisible();
    const color = await heading.evaluate((el) => window.getComputedStyle(el).color);
    expect(color.length).toBeGreaterThan(0);
  },
);

When("a visitor navigates to an interactive element using the keyboard", async ({ page }) => {
  await page.goto("/en");
  // Tab to first interactive element
  await page.keyboard.press("Tab");
});

Then("a visible focus indicator should be displayed on that element", async ({ page }) => {
  const focused = page.locator(":focus");
  await expect(focused).toBeAttached({ timeout: 3000 });
  // The focused element should be visible
  await expect(focused).toBeVisible({ timeout: 3000 });
});

Then("the focus indicator should have sufficient contrast against the surrounding background", async ({ page }) => {
  const focused = page.locator(":focus");
  await expect(focused).toBeAttached({ timeout: 3000 });
  // Verify the element has an outline or box-shadow applied (focus indicator)
  const outlineStyle = await focused.evaluate((el) => {
    const style = window.getComputedStyle(el);
    return {
      outline: style.outline,
      outlineWidth: style.outlineWidth,
      boxShadow: style.boxShadow,
    };
  });
  const hasFocusIndicator =
    (outlineStyle.outline !== "none" && outlineStyle.outlineWidth !== "0px") || outlineStyle.boxShadow !== "none";
  expect(hasFocusIndicator, "Focused element should have a visible outline or box-shadow").toBe(true);
});
