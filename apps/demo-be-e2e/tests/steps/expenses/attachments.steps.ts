import { createBdd } from "playwright-bdd";

const { Given, When, Then } = createBdd();

// Stubs — implement alongside production features

When(
  /^alice uploads file (.+) with content type (.+) to POST \/api\/v1\/expenses\/\{expenseId\}\/attachments$/,
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _filename: string, _contentType: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  "alice has uploaded file {string} with content type {string} to the entry",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _filename: string, _contentType: string) => {
    throw new Error("TODO: not implemented");
  },
);

When(/^alice sends GET \/api\/v1\/expenses\/\{expenseId\}\/attachments$/, async () => {
  throw new Error("TODO: not implemented");
});

Then(
  "the response body should contain {int} items in the {string} array",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _count: number, _field: string) => {
    throw new Error("TODO: not implemented");
  },
);

Then(
  "the response body should contain an attachment with {string} equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _field: string, _value: string) => {
    throw new Error("TODO: not implemented");
  },
);

When(/^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}\/attachments\/\{attachmentId\}$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice uploads an oversized file to POST \/api\/v1\/expenses\/\{expenseId\}\/attachments$/, async () => {
  throw new Error("TODO: not implemented");
});

Then("the response body should contain an error message about file size", async () => {
  throw new Error("TODO: not implemented");
});

When(
  /^alice uploads file (.+) with content type (.+) to POST \/api\/v1\/expenses\/\{bobExpenseId\}\/attachments$/,
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _filename: string, _contentType: string) => {
    throw new Error("TODO: not implemented");
  },
);
