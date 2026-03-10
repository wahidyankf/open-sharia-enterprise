import { createBdd } from "playwright-bdd";

const { When, Then } = createBdd();

// Stubs — implement alongside production features

When(/^alice sends GET \/api\/v1\/reports\/pl\?from=2025-01-01&to=2025-01-31&currency=USD$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends GET \/api\/v1\/reports\/pl\?from=2025-02-01&to=2025-02-28&currency=USD$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends GET \/api\/v1\/reports\/pl\?from=2025-03-01&to=2025-03-31&currency=USD$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends GET \/api\/v1\/reports\/pl\?from=2025-04-01&to=2025-04-30&currency=USD$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends GET \/api\/v1\/reports\/pl\?from=2025-05-01&to=2025-05-31&currency=USD$/, async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends GET \/api\/v1\/reports\/pl\?from=2099-01-01&to=2099-01-31&currency=USD$/, async () => {
  throw new Error("TODO: not implemented");
});

Then(
  "the income breakdown should contain {string} with amount {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _category: string, _amount: string) => {
    throw new Error("TODO: not implemented");
  },
);

Then(
  "the expense breakdown should contain {string} with amount {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _category: string, _amount: string) => {
    throw new Error("TODO: not implemented");
  },
);
