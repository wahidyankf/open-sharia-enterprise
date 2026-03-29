import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";
import { MAX_ATTACHMENT_SIZE } from "../../../src/lib/types";

async function uploadFile(
  world: CustomWorld,
  username: string,
  expenseId: string,
  filename: string,
  contentType: string,
  data?: Buffer,
): Promise<{ status: number; body: unknown }> {
  const fileData = data ?? Buffer.from("fake file content for testing");
  return world.dispatch("POST", `/api/v1/expenses/${expenseId}/attachments`, null, world.getAuth(username), {
    filename,
    contentType,
    size: fileData.length,
    data: fileData,
  });
}

Given(
  "alice has uploaded file {string} with content type {string} to the entry",
  async function (this: CustomWorld, filename: string, contentType: string) {
    const resp = await uploadFile(this, "alice", this.context.expenseId as string, filename, contentType);
    this.context.attachmentId = (resp.body as { id: string }).id;
  },
);

When(
  /^alice uploads file "([^"]+)" with content type "([^"]+)" to POST \/api\/v1\/expenses\/\{expenseId\}\/attachments$/,
  async function (this: CustomWorld, filename: string, contentType: string) {
    this.response = await uploadFile(this, "alice", this.context.expenseId as string, filename, contentType);
    if (this.response.status === 201) {
      this.context.attachmentId = (this.response.body as { id: string }).id;
    }
  },
);

When(
  /^alice uploads file "([^"]+)" with content type "([^"]+)" to POST \/api\/v1\/expenses\/\{bobExpenseId\}\/attachments$/,
  async function (this: CustomWorld, filename: string, contentType: string) {
    this.response = await uploadFile(this, "alice", this.context.bobExpenseId as string, filename, contentType);
  },
);

When(
  /^alice uploads an oversized file to POST \/api\/v1\/expenses\/\{expenseId\}\/attachments$/,
  async function (this: CustomWorld) {
    const oversized = Buffer.alloc(MAX_ATTACHMENT_SIZE + 1, "x");
    this.response = await uploadFile(
      this,
      "alice",
      this.context.expenseId as string,
      "big.jpg",
      "image/jpeg",
      oversized,
    );
  },
);

When(/^alice sends GET \/api\/v1\/expenses\/\{expenseId\}\/attachments$/, async function (this: CustomWorld) {
  this.response = await this.dispatch(
    "GET",
    `/api/v1/expenses/${this.context.expenseId}/attachments`,
    null,
    this.getAuth("alice"),
  );
});

When(/^alice sends GET \/api\/v1\/expenses\/\{bobExpenseId\}\/attachments$/, async function (this: CustomWorld) {
  this.response = await this.dispatch(
    "GET",
    `/api/v1/expenses/${this.context.bobExpenseId}/attachments`,
    null,
    this.getAuth("alice"),
  );
});

When(
  /^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}\/attachments\/\{attachmentId\}$/,
  async function (this: CustomWorld) {
    this.response = await this.dispatch(
      "DELETE",
      `/api/v1/expenses/${this.context.expenseId}/attachments/${this.context.attachmentId}`,
      null,
      this.getAuth("alice"),
    );
  },
);

When(
  /^alice sends DELETE \/api\/v1\/expenses\/\{bobExpenseId\}\/attachments\/\{attachmentId\}$/,
  async function (this: CustomWorld) {
    this.response = await this.dispatch(
      "DELETE",
      `/api/v1/expenses/${this.context.bobExpenseId}/attachments/${this.context.attachmentId}`,
      null,
      this.getAuth("alice"),
    );
  },
);

When(
  /^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}\/attachments\/\{randomAttachmentId\}$/,
  async function (this: CustomWorld) {
    this.response = await this.dispatch(
      "DELETE",
      `/api/v1/expenses/${this.context.expenseId}/attachments/${crypto.randomUUID()}`,
      null,
      this.getAuth("alice"),
    );
  },
);

Then(
  "the response body should contain {int} items in the {string} array",
  function (this: CustomWorld, count: number, field: string) {
    const body = this.response!.body as Record<string, unknown[]>;
    assert.strictEqual((body[field] as unknown[]).length, count);
  },
);

Then(
  "the response body should contain an attachment with {string} equal to {string}",
  function (this: CustomWorld, field: string, value: string) {
    const body = this.response!.body as { attachments: Record<string, unknown>[] };
    assert.strictEqual(
      body.attachments.some((a) => a[field] === value),
      true,
    );
  },
);
