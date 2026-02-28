import { test, expect } from "@playwright/test";
import { TEST_CONFIG } from "../../utils/test-config";
import { loginWithUI } from "../../utils/auth";

test.describe("Member Detail Page", () => {
  test("should redirect unauthenticated users to /login", async ({ page }) => {
    await page.goto(`${TEST_CONFIG.baseUrl}/dashboard/members/1`);
    await page.waitForURL(`${TEST_CONFIG.baseUrl}/login`);
  });

  test.describe("when authenticated", () => {
    test.beforeEach(async ({ page }) => {
      await loginWithUI(page);
      // Navigate to members via page.goto (bounces through /login → /dashboard/members)
      await page.goto(`${TEST_CONFIG.baseUrl}/dashboard/members`);
      // Wait for the table to render before clicking (auth state may resolve async on webkit)
      await expect(page.locator("tbody tr").first()).toBeVisible();
      // Click Alice Johnson's row for client-side navigation to member detail
      await page.locator("tbody tr").first().locator("td").first().click();
      await page.waitForURL(`${TEST_CONFIG.baseUrl}/dashboard/members/1`);
      await page.waitForLoadState("networkidle");
    });

    test("should display member details for member id=1 (Alice Johnson, Senior Software Engineers)", async ({
      page,
    }) => {
      await expect(page.getByText("Alice Johnson")).toBeVisible();
      await expect(page.getByText("Senior Software Engineers")).toBeVisible();
    });

    test("should display GitHub link for the member", async ({ page }) => {
      await expect(page.getByRole("link", { name: "alicejohnson" })).toBeVisible();
    });
  });
});
