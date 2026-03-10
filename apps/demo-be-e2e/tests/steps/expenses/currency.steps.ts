import { createBdd } from "playwright-bdd";

const { When, Then } = createBdd();

// Stubs — implement alongside production features

When(/^alice sends GET \/api\/v1\/expenses\/summary$/, async () => {
  throw new Error("TODO: not implemented");
});

Then(
  "the response body should contain {string} total equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _currency: string, _total: string) => {
    throw new Error("TODO: not implemented");
  },
);
