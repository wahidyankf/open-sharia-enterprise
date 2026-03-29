import { createBdd } from "playwright-bdd";

const { Given, When } = createBdd();

Given("the app is running", async () => {});

When("a visitor opens a content page", async ({ page }) => {
  await page.goto("/en/learn/overview");
});
