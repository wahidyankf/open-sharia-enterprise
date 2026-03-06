import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import * as fs from "fs/promises";
import * as path from "path";

const { Given, When, Then, Before, After } = createBdd();

const MEMBERS_PATH = path.join(process.cwd(), "../organiclever-web/src/data/members.json");

const ORIGINAL_MEMBERS = [
  {
    id: 1,
    name: "Alice Johnson",
    role: "Senior Software Engineers",
    email: "alice@example.com",
    github: "alicejohnson",
  },
  { id: 2, name: "Bob Smith", role: "Product Manager", github: "bobsmith" },
  {
    id: 3,
    name: "Charlie Davis",
    role: "UX Designer",
    email: "charlie@example.com",
    github: "charliedavis",
  },
  { id: 4, name: "Diana Miller", role: "Backend Developer", github: "dianamiller" },
  {
    id: 5,
    name: "Ethan Brown",
    role: "Frontend Developer",
    email: "ethan@example.com",
    github: "ethanbrown",
  },
  { id: 6, name: "Fiona Taylor", role: "QA Engineer", github: "fionataylor" },
];

Before({ tags: "@member-editing" }, async () => {
  await fs.writeFile(MEMBERS_PATH, JSON.stringify(ORIGINAL_MEMBERS, null, 2) + "\n");
});

After({ tags: "@member-editing" }, async () => {
  await fs.writeFile(MEMBERS_PATH, JSON.stringify(ORIGINAL_MEMBERS, null, 2) + "\n");
});

When("the user opens the edit dialog for {string}", async ({ page }, name: string) => {
  const row = page.locator("tbody tr").filter({ hasText: name });
  const editButton = row.locator("td").last().getByRole("button").nth(1);
  await editButton.click();
  await expect(page.getByRole("dialog")).toBeVisible();
});

Then("the name field should show {string}", async ({ page }, value: string) => {
  await expect(page.getByRole("dialog").locator("#name")).toHaveValue(value);
});

Then("the role field should show {string}", async ({ page }, value: string) => {
  await expect(page.getByRole("dialog").locator("#role")).toHaveValue(value);
});

Then("the email field should show {string}", async ({ page }, value: string) => {
  await expect(page.getByRole("dialog").locator("#email")).toHaveValue(value);
});

Then("the GitHub field should show {string}", async ({ page }, value: string) => {
  await expect(page.getByRole("dialog").locator("#github")).toHaveValue(value);
});

Given("the user has opened the edit dialog for {string}", async ({ page }, name: string) => {
  const row = page.locator("tbody tr").filter({ hasText: name });
  const editButton = row.locator("td").last().getByRole("button").nth(1);
  await editButton.click();
  await expect(page.getByRole("dialog")).toBeVisible();
});

When("the user changes the name to {string} and saves", async ({ page }, newName: string) => {
  const nameInput = page.getByRole("dialog").locator("#name");
  await nameInput.clear();
  await nameInput.fill(newName);
  await page.getByRole("button", { name: "Save changes" }).click();
  await expect(page.getByRole("dialog")).not.toBeVisible();
});

When("the user changes the role to {string}", async ({ page }, value: string) => {
  const roleInput = page.getByRole("dialog").locator("#role");
  await roleInput.clear();
  await roleInput.fill(value);
});

When("the user changes the email to {string}", async ({ page }, value: string) => {
  const emailInput = page.getByRole("dialog").locator("#email");
  await emailInput.clear();
  await emailInput.fill(value);
});

When("the user changes the github to {string}", async ({ page }, value: string) => {
  const githubInput = page.getByRole("dialog").locator("#github");
  await githubInput.clear();
  await githubInput.fill(value);
});

Then("the github field should show {string}", async ({ page }, value: string) => {
  await expect(page.getByRole("dialog").locator("#github")).toHaveValue(value);
});
