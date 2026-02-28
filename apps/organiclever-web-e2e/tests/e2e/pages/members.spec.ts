import { test, expect } from "@playwright/test";
import { TEST_CONFIG } from "../../utils/test-config";
import { loginWithUI } from "../../utils/auth";

test.describe("Members Page", () => {
  test("should redirect unauthenticated users to /login", async ({ page }) => {
    await page.goto(`${TEST_CONFIG.baseUrl}/dashboard/members`);
    await page.waitForURL(`${TEST_CONFIG.baseUrl}/login`);
  });

  test.describe("when authenticated", () => {
    test.beforeEach(async ({ page }) => {
      await loginWithUI(page);
      await page.goto(`${TEST_CONFIG.baseUrl}/dashboard/members`);
      await expect(page.getByRole("heading", { name: "Members" })).toBeVisible();
    });

    test("should display 'Members' heading and search input when authenticated", async ({ page }) => {
      await expect(page.getByRole("heading", { name: "Members" })).toBeVisible();
      await expect(page.getByPlaceholder("Search members")).toBeVisible();
    });

    test("should show all 6 members in the table", async ({ page }) => {
      await expect(page.getByText("Alice Johnson")).toBeVisible();
      await expect(page.getByText("Bob Smith")).toBeVisible();
      await expect(page.getByText("Charlie Davis")).toBeVisible();
      await expect(page.getByText("Diana Miller")).toBeVisible();
      await expect(page.getByText("Ethan Brown")).toBeVisible();
      await expect(page.getByText("Fiona Taylor")).toBeVisible();
    });

    test("should filter members when search query is entered", async ({ page }) => {
      await page.getByPlaceholder("Search members").fill("Alice");
      await expect(page.getByText("Alice Johnson")).toBeVisible();
      await expect(page.getByText("Bob Smith")).not.toBeVisible();
    });

    test("should open edit dialog when edit button is clicked", async ({ page }) => {
      const firstRow = page.locator("tbody tr").first();
      await firstRow.locator("button").nth(1).click();
      await expect(page.getByRole("dialog")).toBeVisible();
      await expect(page.getByText("Edit Member")).toBeVisible();
    });

    test("should open delete confirmation dialog when delete button is clicked", async ({ page }) => {
      const firstRow = page.locator("tbody tr").first();
      await firstRow.locator("button").nth(2).click();
      await expect(page.getByRole("alertdialog")).toBeVisible();
      await expect(page.getByText("Are you absolutely sure?")).toBeVisible();
    });

    test("should navigate to member detail page when a row is clicked", async ({ page }) => {
      const firstRow = page.locator("tbody tr").first();
      await firstRow.locator("td").first().click();
      await page.waitForURL(`${TEST_CONFIG.baseUrl}/dashboard/members/1`);
    });
  });
});
