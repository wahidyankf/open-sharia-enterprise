import { createBdd } from "playwright-bdd";

const { Given, When, Then } = createBdd();

// Stubs — implement alongside production features

When(/^alice decodes her access token payload$/, async () => {
  throw new Error("TODO: not implemented");
});

Then(
  "the token should contain a non-null {string} claim",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _claim: string) => {
    throw new Error("TODO: not implemented");
  },
);

When(/^the client sends GET \/\.well-known\/jwks\.json$/, async () => {
  throw new Error("TODO: not implemented");
});

Then(
  "the response body should contain at least one key in the {string} array",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _field: string) => {
    throw new Error("TODO: not implemented");
  },
);

Then("alice's access token should be recorded as revoked", async () => {
  throw new Error("TODO: not implemented");
});

Given("alice has logged out and her access token is blacklisted", async () => {
  throw new Error("TODO: not implemented");
});

Given(/^the admin has disabled alice's account via POST \/api\/v1\/admin\/users\/\{alice_id\}\/disable$/, async () => {
  throw new Error("TODO: not implemented");
});
