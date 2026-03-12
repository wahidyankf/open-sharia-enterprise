import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "@playwright/test";
import type { CustomWorld } from "../world.js";

const MAX_ATTACHMENT_SIZE = 10 * 1024 * 1024; // 10MB

// ---- Given ----

Given(
  /^alice has uploaded file "([^"]*)" with content type "([^"]*)" to the entry$/,
  async function (this: CustomWorld, filename: string, contentType: string) {
    const token = this.tokens.get("alice_access") ?? "";
    const expenseId = this.context["expenseId"] as string;
    const content = Buffer.from("fake file content for testing");
    const res = await this.uploadFile(
      `/api/v1/expenses/${expenseId}/attachments`,
      filename,
      contentType,
      content,
      token,
    );
    if (res.status !== 201) {
      throw new Error(`Failed to upload attachment: ${JSON.stringify(res.body)}`);
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    this.context["attachmentId"] = (res.body as any).id as string;
  },
);

// ---- When ----

When(
  /^alice uploads file "([^"]*)" with content type "([^"]*)" to POST \/api\/v1\/expenses\/\{expenseId\}\/attachments$/,
  async function (this: CustomWorld, filename: string, contentType: string) {
    const token = this.tokens.get("alice_access") ?? "";
    const expenseId = this.context["expenseId"] as string;
    const content = Buffer.from("fake file content for testing");
    this.response = await this.uploadFile(
      `/api/v1/expenses/${expenseId}/attachments`,
      filename,
      contentType,
      content,
      token,
    );
  },
);

When(
  /^alice uploads file "([^"]*)" with content type "([^"]*)" to POST \/api\/v1\/expenses\/\{bobExpenseId\}\/attachments$/,
  async function (this: CustomWorld, filename: string, contentType: string) {
    const token = this.tokens.get("alice_access") ?? "";
    const bobExpenseId = this.context["bobExpenseId"] as string;
    const content = Buffer.from("fake file content for testing");
    this.response = await this.uploadFile(
      `/api/v1/expenses/${bobExpenseId}/attachments`,
      filename,
      contentType,
      content,
      token,
    );
  },
);

When(
  /^alice uploads an oversized file to POST \/api\/v1\/expenses\/\{expenseId\}\/attachments$/,
  async function (this: CustomWorld) {
    const token = this.tokens.get("alice_access") ?? "";
    const expenseId = this.context["expenseId"] as string;
    // Create a buffer just over the limit
    const content = Buffer.alloc(MAX_ATTACHMENT_SIZE + 1, "x");
    this.response = await this.uploadFile(
      `/api/v1/expenses/${expenseId}/attachments`,
      "large.jpg",
      "image/jpeg",
      content,
      token,
    );
  },
);

When(/^alice sends GET \/api\/v1\/expenses\/\{expenseId\}\/attachments$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const expenseId = this.context["expenseId"] as string;
  this.response = await this.get(`/api/v1/expenses/${expenseId}/attachments`, token);
});

When(/^alice sends GET \/api\/v1\/expenses\/\{bobExpenseId\}\/attachments$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const bobExpenseId = this.context["bobExpenseId"] as string;
  this.response = await this.get(`/api/v1/expenses/${bobExpenseId}/attachments`, token);
});

When(
  /^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}\/attachments\/\{attachmentId\}$/,
  async function (this: CustomWorld) {
    const token = this.tokens.get("alice_access") ?? "";
    const expenseId = this.context["expenseId"] as string;
    const attachmentId = this.context["attachmentId"] as string;
    this.response = await this.delete(`/api/v1/expenses/${expenseId}/attachments/${attachmentId}`, token);
  },
);

When(
  /^alice sends DELETE \/api\/v1\/expenses\/\{bobExpenseId\}\/attachments\/\{attachmentId\}$/,
  async function (this: CustomWorld) {
    const token = this.tokens.get("alice_access") ?? "";
    const bobExpenseId = this.context["bobExpenseId"] as string;
    const attachmentId = this.context["attachmentId"] as string;
    this.response = await this.delete(`/api/v1/expenses/${bobExpenseId}/attachments/${attachmentId}`, token);
  },
);

When(
  /^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}\/attachments\/\{randomAttachmentId\}$/,
  async function (this: CustomWorld) {
    const token = this.tokens.get("alice_access") ?? "";
    const expenseId = this.context["expenseId"] as string;
    this.response = await this.delete(`/api/v1/expenses/${expenseId}/attachments/nonexistent-id-${Date.now()}`, token);
  },
);

// ---- Then ----

Then(
  "the response body should contain {int} items in the {string} array",
  function (this: CustomWorld, count: number, field: string) {
    expect(this.response).not.toBeNull();
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as Record<string, unknown>;
    const arr = body[field] as unknown[];
    expect(Array.isArray(arr)).toBe(true);
    expect(arr.length).toBe(count);
  },
);

Then(
  /^the response body should contain an attachment with "([^"]*)" equal to "([^"]*)"$/,
  function (this: CustomWorld, field: string, value: string) {
    expect(this.response).not.toBeNull();
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as any;
    const attachments = body?.attachments as Array<Record<string, string>>;
    expect(Array.isArray(attachments)).toBe(true);
    const found = attachments.some((a) => String(a[field]) === value);
    expect(found).toBe(true);
  },
);

Then("the response body should contain an error message about file size", function (this: CustomWorld) {
  expect(this.response).not.toBeNull();
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const body = this.response?.body as Record<string, string>;
  const message = (body["message"] ?? body["error"] ?? "").toLowerCase();
  expect(message.length).toBeGreaterThan(0);
});
