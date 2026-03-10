import { createBdd } from "playwright-bdd";

const { Given, When, Then } = createBdd();

// Stubs — implement alongside production features

Given(
  "users {string}, {string}, and {string} are registered",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _user1: string, _user2: string, _user3: string) => {
    throw new Error("TODO: not implemented");
  },
);

When(/^the admin sends GET \/api\/v1\/admin\/users$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^the admin sends GET \/api\/v1\/admin\/users\?email=alice@example\.com$/, async () => {
  throw new Error("TODO: not implemented");
});

When(
  /^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/disable with body \{ "reason": "Policy violation" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

Given("alice's account has been disabled by the admin", async () => {
  throw new Error("TODO: not implemented");
});

Given("alice's account has been disabled", async () => {
  throw new Error("TODO: not implemented");
});

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/enable$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/force-password-reset$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/unlock$/, async () => {
  throw new Error("TODO: not implemented");
});

Then(
  "the response body should contain at least one user with {string} equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _field: string, _value: string) => {
    throw new Error("TODO: not implemented");
  },
);
