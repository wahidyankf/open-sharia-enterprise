import { Page } from "@playwright/test";
import { TEST_CONFIG } from "./test-config";

export async function loginWithUI(page: Page): Promise<void> {
  await page.goto(`${TEST_CONFIG.baseUrl}/login`);
  await page.fill('input[type="email"]', "user@example.com");
  await page.fill('input[type="password"]', "password123");
  await page.click('button[type="submit"]');
  // Use a regex and extended timeout: webkit in CI can be slow to complete
  // the post-login redirect (React auth state update → router.push).
  await page.waitForURL(/\/dashboard/, { timeout: 60000 });
}

export async function logoutViaAPI(page: Page): Promise<void> {
  await page.request.post(`${TEST_CONFIG.baseUrl}/api/auth/logout`);
}
