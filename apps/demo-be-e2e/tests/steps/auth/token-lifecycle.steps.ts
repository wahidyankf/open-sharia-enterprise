import { createBdd } from "playwright-bdd";

const { Given, When, Then } = createBdd();

// Stubs — implement alongside production features

Given(
  "{string} has logged in and stored the access token and refresh token",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string) => {
    throw new Error("TODO: not implemented");
  },
);

When(/^alice sends POST \/api\/v1\/auth\/refresh with her refresh token$/, async () => {
  throw new Error("TODO: not implemented");
});

Given("alice's refresh token has expired", async () => {
  throw new Error("TODO: not implemented");
});

Given("alice has used her refresh token to get a new token pair", async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends POST \/api\/v1\/auth\/refresh with her original refresh token$/, async () => {
  throw new Error("TODO: not implemented");
});

Given(
  "the user {string} has been deactivated",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string) => {
    throw new Error("TODO: not implemented");
  },
);

When(/^alice sends POST \/api\/v1\/auth\/logout-all with her access token$/, async () => {
  throw new Error("TODO: not implemented");
});

Given("alice has already logged out once", async () => {
  throw new Error("TODO: not implemented");
});

Then("alice's access token should be invalidated", async () => {
  throw new Error("TODO: not implemented");
});
