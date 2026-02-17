import { type Page } from "@playwright/test";

export async function waitForAppReady(page: Page) {
  await page.waitForLoadState("networkidle");
}
