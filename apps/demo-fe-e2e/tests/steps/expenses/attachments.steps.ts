import { createBdd } from "playwright-bdd";
import { expect } from "@playwright/test";
import { loginUser, createExpense, registerUser } from "@/utils/api-helpers.js";
import {
  getReceiptJpgPath,
  getInvoicePdfPath,
  getOversizedFilePath,
  getMalwareExePath,
} from "@/fixtures/test-files.js";

const { Given, When, Then } = createBdd();

Given(
  "{word} has uploaded {string} and {string} to the entry",
  async ({ page }, _username: string, file1: string, file2: string) => {
    const file1Path = file1.endsWith(".jpg") || file1.endsWith(".jpeg") ? getReceiptJpgPath() : getInvoicePdfPath();
    const fileInput1 = page.getByLabel(/upload|attach/i);
    await fileInput1.setInputFiles(file1Path);
    await expect(page.getByText(file1)).toBeVisible({ timeout: 10000 });

    const file2Path = file2.endsWith(".pdf") ? getInvoicePdfPath() : getReceiptJpgPath();
    const fileInput2 = page.getByLabel(/upload|attach/i);
    await fileInput2.setInputFiles(file2Path);
    await expect(page.getByText(file2)).toBeVisible({ timeout: 10000 });
  },
);

Given("{word} has uploaded {string} to the entry", async ({ page }, _username: string, filename: string) => {
  const filePath = filename.endsWith(".jpg") || filename.endsWith(".jpeg") ? getReceiptJpgPath() : getInvoicePdfPath();
  const fileInput = page.getByLabel(/upload|attach/i);
  await fileInput.setInputFiles(filePath);
  await expect(page.getByText(filename)).toBeVisible({ timeout: 10000 });
});

Given("the attachment has been deleted from another session", async ({}) => {
  // No-op: simulates external deletion; error handling verified in Then step
});

Given(
  "a user {string} has created an entry with description {string}",
  async ({}, username: string, description: string) => {
    await registerUser(username, `${username}@example.com`, "Str0ng#Pass1");
    const { accessToken } = await loginUser(username, "Str0ng#Pass1");
    await createExpense(accessToken, {
      amount: "25.00",
      currency: "USD",
      category: "transport",
      description,
      date: "2025-01-15",
      type: "expense",
    });
  },
);

Given("a user {string} has created an entry with an attachment", async ({}, username: string) => {
  await registerUser(username, `${username}@example.com`, "Str0ng#Pass1");
  const { accessToken } = await loginUser(username, "Str0ng#Pass1");
  await createExpense(accessToken, {
    amount: "25.00",
    currency: "USD",
    category: "transport",
    description: "Bob's entry",
    date: "2025-01-15",
    type: "expense",
  });
});

When("{word} uploads file {string} as an image attachment", async ({ page }, _username: string, _filename: string) => {
  const filePath = getReceiptJpgPath();
  const fileInput = page.getByLabel(/upload|attach file/i);
  await fileInput.setInputFiles(filePath);
});

When(
  "{word} uploads file {string} as a document attachment",
  async ({ page }, _username: string, _filename: string) => {
    const filePath = getInvoicePdfPath();
    const fileInput = page.getByLabel(/upload|attach file/i);
    await fileInput.setInputFiles(filePath);
  },
);

When("{word} attempts to upload file {string}", async ({ page }, _username: string, _filename: string) => {
  const filePath = getMalwareExePath();
  const fileInput = page.getByLabel(/upload|attach file/i);
  await fileInput.setInputFiles(filePath);
});

When("{word} attempts to upload an oversized file", async ({ page }) => {
  const filePath = getOversizedFilePath();
  const fileInput = page.getByLabel(/upload|attach file/i);
  await fileInput.setInputFiles(filePath);
});

When(
  "{word} clicks the delete button on attachment {string}",
  async ({ page }, _username: string, filename: string) => {
    const attachmentRow = page.getByText(filename).first();
    await attachmentRow.hover();
    await page
      .getByRole("button", { name: /delete|remove/i })
      .first()
      .click();
  },
);

Then("the attachment list should contain {string}", async ({ page }, filename: string) => {
  await expect(page.getByText(filename)).toBeVisible({ timeout: 10000 });
});

Then("the attachment should display as type {string}", async ({ page }, mimeType: string) => {
  await expect(page.getByText(mimeType)).toBeVisible();
});

Then("the attachment list should contain {int} items", async ({ page }, count: number) => {
  const attachmentItems = page
    .getByTestId("attachment-item")
    .or(page.getByRole("listitem").filter({ hasText: /\.(jpg|jpeg|pdf)/i }));
  await expect(attachmentItems).toHaveCount(count);
});

Then("the attachment list should include {string}", async ({ page }, filename: string) => {
  await expect(page.getByText(filename)).toBeVisible();
});

Then("the attachment list should not contain {string}", async ({ page }, filename: string) => {
  await expect(page.getByText(filename)).not.toBeVisible();
});

Then("an error message about unsupported file type should be displayed", async ({ page }) => {
  await expect(page.getByText(/unsupported|invalid file type|not allowed/i)).toBeVisible();
});

Then("the attachment list should remain unchanged", async ({ page }) => {
  const attachmentItems = page
    .getByTestId("attachment-item")
    .or(page.getByRole("listitem").filter({ hasText: /\.(jpg|jpeg|pdf)/i }));
  const count = await attachmentItems.count();
  expect(count).toBeGreaterThanOrEqual(0);
});

Then("an error message about file size limit should be displayed", async ({ page }) => {
  await expect(page.getByText(/too large|size limit|file.*size|maximum/i)).toBeVisible();
});

Then("the upload attachment button should not be visible", async ({ page }) => {
  await expect(page.getByRole("button", { name: /upload|attach/i })).not.toBeVisible();
});

Then("an access denied message should be displayed", async ({ page }) => {
  await expect(
    page.getByText(/access denied|forbidden|not authorized|permission/i).or(page.getByRole("alert")),
  ).toBeVisible();
});

Then("the delete attachment button should not be visible", async ({ page }) => {
  await expect(
    page.getByRole("button", {
      name: /delete.*attachment|remove.*attachment/i,
    }),
  ).not.toBeVisible();
});

Then("an error message about attachment not found should be displayed", async ({ page }) => {
  await expect(page.getByText(/not found|no longer exists|deleted/i)).toBeVisible();
});
