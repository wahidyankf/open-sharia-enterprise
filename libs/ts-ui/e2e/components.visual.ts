import { expect, test } from "@playwright/test";

const STORYBOOK_ROOT = "#storybook-root";

async function loadStory(
  page: Parameters<typeof test>[1] extends (args: { page: infer P }, ...rest: unknown[]) => unknown ? P : never,
  storyId: string,
): Promise<void> {
  await page.goto(`/iframe.html?id=${storyId}&viewMode=story`);
  await page.locator(STORYBOOK_ROOT).waitFor({ state: "visible" });
}

// Button — Feedback/Button
test.describe("Button", () => {
  test("default variant renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-button--default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("button-default.png");
  });

  test("destructive variant renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-button--destructive");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("button-destructive.png");
  });

  test("outline variant renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-button--outline");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("button-outline.png");
  });

  test("ghost variant renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-button--ghost");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("button-ghost.png");
  });

  test("disabled state renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-button--disabled");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("button-disabled.png");
  });

  test("all variants render correctly at mobile viewport", async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 812 });
    await loadStory(page, "feedback-button--all-variants");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("button-all-variants-mobile.png");
  });

  test("all variants render correctly at desktop viewport", async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 800 });
    await loadStory(page, "feedback-button--all-variants");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("button-all-variants-desktop.png");
  });
});

// Alert — Feedback/Alert
test.describe("Alert", () => {
  test("default variant renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-alert--variant-default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("alert-default.png");
  });

  test("destructive variant renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-alert--variant-destructive");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("alert-destructive.png");
  });

  test("with icon default renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-alert--with-icon-default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("alert-with-icon-default.png");
  });

  test("with icon destructive renders correctly", async ({ page }) => {
    await loadStory(page, "feedback-alert--with-icon-destructive");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("alert-with-icon-destructive.png");
  });
});

// Dialog — Overlay/Dialog
test.describe("Dialog", () => {
  test("default dialog renders correctly", async ({ page }) => {
    await loadStory(page, "overlay-dialog--default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("dialog-default.png");
  });

  test("dialog with confirm action renders correctly", async ({ page }) => {
    await loadStory(page, "overlay-dialog--with-confirm-action");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("dialog-with-confirm-action.png");
  });
});

// Input — Forms/Input
test.describe("Input", () => {
  test("default state renders correctly", async ({ page }) => {
    await loadStory(page, "forms-input--default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("input-default.png");
  });

  test("disabled state renders correctly", async ({ page }) => {
    await loadStory(page, "forms-input--disabled");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("input-disabled.png");
  });

  test("invalid state renders correctly", async ({ page }) => {
    await loadStory(page, "forms-input--invalid");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("input-invalid.png");
  });
});

// Card — Layout/Card
test.describe("Card", () => {
  test("default card renders correctly at mobile viewport", async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 812 });
    await loadStory(page, "layout-card--default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("card-default-mobile.png");
  });

  test("default card renders correctly at desktop viewport", async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 800 });
    await loadStory(page, "layout-card--default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("card-default-desktop.png");
  });

  test("header only card renders correctly", async ({ page }) => {
    await loadStory(page, "layout-card--header-only");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("card-header-only.png");
  });
});

// Label — Forms/Label
test.describe("Label", () => {
  test("default label renders correctly", async ({ page }) => {
    await loadStory(page, "forms-label--default");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("label-default.png");
  });

  test("label paired with input renders correctly", async ({ page }) => {
    await loadStory(page, "forms-label--paired-with-input");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("label-paired-with-input.png");
  });

  test("label paired with disabled input renders correctly", async ({ page }) => {
    await loadStory(page, "forms-label--paired-with-disabled-input");
    const screenshot = await page.screenshot();
    expect(screenshot).toMatchSnapshot("label-paired-with-disabled-input.png");
  });
});
