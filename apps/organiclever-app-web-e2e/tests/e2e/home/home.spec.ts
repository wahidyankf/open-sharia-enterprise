import { test, expect } from "@playwright/test";
import { HomePage } from "../../pages/HomePage";

test.describe("Home Page", () => {
  test.describe("initial state", () => {
    test("shows app title", async ({ page }) => {
      const home = new HomePage(page);
      await home.goto();
      await expect(home.appTitle).toBeVisible();
    });

    test("shows Fetch Hello button", async ({ page }) => {
      const home = new HomePage(page);
      await home.goto();
      await expect(home.fetchHelloButton).toBeVisible();
    });
  });

  test.describe("fetch interaction", () => {
    test("shows API response after clicking Fetch Hello", async ({ page }) => {
      const home = new HomePage(page);
      await home.goto();
      await home.fetchHelloButton.click();
      await expect(home.apiResponseLabel).toBeVisible();
    });

    test("displays world message from backend", async ({ page }) => {
      const home = new HomePage(page);
      await home.goto();
      await home.fetchHelloButton.click();
      await expect(page.getByText("world!")).toBeVisible();
    });
  });
});
