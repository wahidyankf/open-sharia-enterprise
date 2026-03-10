import { createBdd } from "playwright-bdd";

const { Given, When, Then } = createBdd();

// Shared stub steps used across multiple feature areas
// Implement alongside production features

Given(
  "a user {string} is registered with password {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string, _password: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  "a user {string} is registered with email {string} and password {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string, _email: string, _password: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  "a user {string} is registered and deactivated",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  "{string} has logged in and stored the access token",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  "an admin user {string} is registered and logged in",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string) => {
    throw new Error("TODO: not implemented");
  },
);

Then(
  "alice's account status should be {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _status: string) => {
    throw new Error("TODO: not implemented");
  },
);

Then("the response body should contain an error message about account deactivation", async () => {
  throw new Error("TODO: not implemented");
});

Then("the response body should contain an error message about token expiration", async () => {
  throw new Error("TODO: not implemented");
});

Then("the response body should contain an error message about invalid token", async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends POST \/api\/v1\/auth\/logout with her access token$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^the client sends GET \/api\/v1\/users\/me with alice's access token$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends GET \/api\/v1\/expenses\/\{expenseId\}$/, async () => {
  throw new Error("TODO: not implemented");
});

When(
  /^alice sends POST \/api\/v1\/expenses with body (.+)$/,
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _body: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  /^alice has created an entry with body (.+)$/,
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _body: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  /^alice has created an expense with body (.+)$/,
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _body: string) => {
    throw new Error("TODO: not implemented");
  },
);

When(/^the client sends POST \/api\/v1\/auth\/login with body (.+)$/, async () => {
  throw new Error("TODO: not implemented");
});

Given(
  /^bob has created an entry with body (.+)$/,
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _body: string) => {
    throw new Error("TODO: not implemented");
  },
);
