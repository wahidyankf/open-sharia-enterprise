import { test, expect } from "@playwright/test";
import { TEST_CONFIG } from "../../utils/test-config";
import { loginWithUI } from "../../utils/auth";

test.describe("Login Page", () => {
  test("should display login form with email, password, and submit button", async ({ page }) => {
    await page.goto(`${TEST_CONFIG.baseUrl}/login`);
    await expect(page.locator('input[type="email"]')).toBeVisible();
    await expect(page.locator('input[type="password"]')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();
  });

  test("should show error alert on invalid credentials", async ({ page }) => {
    await page.goto(`${TEST_CONFIG.baseUrl}/login`);
    await page.fill('input[type="email"]', "wrong@example.com");
    await page.fill('input[type="password"]', "wrongpassword");
    await page.click('button[type="submit"]');
    await page.waitForLoadState("networkidle");
    await expect(page.getByText("Invalid email or password")).toBeVisible();
  });

  test("should redirect to /dashboard on valid login", async ({ page }) => {
    await loginWithUI(page);
    expect(page.url()).toBe(`${TEST_CONFIG.baseUrl}/dashboard`);
  });

  test("should show 'Forgot password?' link and 'Learn more' footer link", async ({ page }) => {
    await page.goto(`${TEST_CONFIG.baseUrl}/login`);
    await expect(page.getByText("Forgot password?")).toBeVisible();
    await expect(page.getByRole("link", { name: "Learn more about Organic Lever" })).toBeVisible();
  });
});
