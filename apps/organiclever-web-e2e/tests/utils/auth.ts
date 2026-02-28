import { Page } from "@playwright/test";
import { TEST_CONFIG } from "./test-config";

export async function loginWithUI(page: Page): Promise<void> {
  await page.goto(`${TEST_CONFIG.baseUrl}/login`);
  await page.fill('input[type="email"]', "user@example.com");
  await page.fill('input[type="password"]', "password123");
  await page.click('button[type="submit"]');
  await page.waitForURL(`${TEST_CONFIG.baseUrl}/dashboard`);
}

export async function logoutViaAPI(page: Page): Promise<void> {
  await page.request.post(`${TEST_CONFIG.baseUrl}/api/auth/logout`);
}
