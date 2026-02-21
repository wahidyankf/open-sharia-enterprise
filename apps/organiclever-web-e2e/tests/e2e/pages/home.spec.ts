import { test, expect } from "@playwright/test";
import { TEST_CONFIG } from "../../utils/test-config";

test.describe("Home Page", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto(TEST_CONFIG.baseUrl);
  });

  test("should display the header with correct navigation when not logged in", async ({ page }) => {
    // Check if the logo and site name are visible
    await expect(page.getByRole("link", { name: "Organic Lever" })).toBeVisible();

    // Check if the navigation links are visible
    await expect(page.getByRole("link", { name: "About" })).toBeVisible();
    await expect(page.getByRole("link", { name: "Login" })).toBeVisible();
  });

  test("should display the header with correct navigation when logged in and redirect to dashboard", async ({
    page,
  }) => {
    // Navigate to the login page
    await page.goto(`${TEST_CONFIG.baseUrl}/login`);

    // Fill in the login form
    await page.fill('input[type="email"]', "user@example.com");
    await page.fill('input[type="password"]', "password123");

    // Submit the form
    await page.click('button[type="submit"]');

    // Wait for the login process to complete
    await page.waitForLoadState("networkidle");

    // Manually navigate to the home page
    await page.goto(TEST_CONFIG.baseUrl);

    // Check if the logo and site name are visible
    await expect(page.getByRole("link", { name: "Organic Lever" })).toBeVisible();

    // Check if the navigation links are visible
    await expect(page.getByRole("link", { name: "About" })).toBeVisible();
    const dashboardLink = page.getByRole("link", { name: "Dashboard" });
    await expect(dashboardLink).toBeVisible();

    // Ensure the 'Login' link is not visible
    await expect(page.getByRole("link", { name: "Login" })).not.toBeVisible();

    // Click the Dashboard link
    await Promise.all([page.waitForURL(`${TEST_CONFIG.baseUrl}/dashboard`), dashboardLink.click()]);

    // Check if we're on the dashboard page
    expect(page.url()).toBe(`${TEST_CONFIG.baseUrl}/dashboard`);
  });

  test("should display the hero section with correct content", async ({ page }) => {
    await expect(
      page.getByRole("heading", {
        name: "Boost Your Software Team's Productivity",
      }),
    ).toBeVisible();
    await expect(page.getByText("Organic Lever helps software engineering and product management")).toBeVisible();
    await expect(page.getByRole("link", { name: "Get Started" })).toBeVisible();
    await expect(page.getByRole("button", { name: "Learn More" })).toBeVisible();
  });

  test("should display the team section with two cards", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "Empower Your Software Teams" })).toBeVisible();
    await expect(page.getByRole("heading", { name: "Software Engineering" })).toBeVisible();
    await expect(page.getByRole("heading", { name: "Product Management" })).toBeVisible();
  });

  test("should display the footer with correct links", async ({ page }) => {
    await expect(page.getByText("© 2024 Organic Lever. All rights reserved.")).toBeVisible();
    await expect(page.getByRole("link", { name: "Terms of Service" })).toBeVisible();
    await expect(page.getByRole("link", { name: "Privacy" })).toBeVisible();
  });

  test("should display the work in progress alert", async ({ page }) => {
    await expect(page.getByRole("heading", { name: "Work in Progress" })).toBeVisible();
    await expect(page.getByText("Organic Lever is currently under development.")).toBeVisible();
  });
});
