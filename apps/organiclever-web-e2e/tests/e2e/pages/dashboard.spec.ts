import { test, expect } from "@playwright/test";
import { TEST_CONFIG } from "../../utils/test-config";
import { loginWithUI } from "../../utils/auth";

test.describe("Dashboard Page", () => {
  test("should redirect unauthenticated users to /login", async ({ page }) => {
    await page.goto(`${TEST_CONFIG.baseUrl}/dashboard`);
    await page.waitForURL(`${TEST_CONFIG.baseUrl}/login`);
  });

  test.describe("when authenticated", () => {
    test.beforeEach(async ({ page }) => {
      await loginWithUI(page);
      await page.waitForLoadState("networkidle");
    });

    test("should display 'Dashboard' heading when authenticated", async ({ page }) => {
      await expect(page.getByRole("heading", { name: "Dashboard" })).toBeVisible();
    });

    test("should show 'Active Projects' card with value 12", async ({ page }) => {
      await expect(page.getByText("Active Projects")).toBeVisible();
      await expect(page.getByText("12")).toBeVisible();
    });

    test("should show 'Team Members' card with value 24", async ({ page }) => {
      await expect(page.getByText("Team Members")).toBeVisible();
      await expect(page.getByText("24")).toBeVisible();
    });

    test("should navigate to /dashboard/members when Team Members card is clicked", async ({ page }) => {
      await page.getByText("Team Members").click();
      await page.waitForURL(`${TEST_CONFIG.baseUrl}/dashboard/members`);
    });

    test("should display sidebar with Dashboard and Team navigation links", async ({ page }) => {
      await expect(page.getByRole("link", { name: "Dashboard" }).first()).toBeVisible();
      await expect(page.getByRole("link", { name: "Team" })).toBeVisible();
    });
  });
});
