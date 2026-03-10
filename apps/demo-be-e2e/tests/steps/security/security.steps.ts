import { createBdd } from "playwright-bdd";

const { Given, When } = createBdd();

// Stubs — implement alongside production features

When(
  /^the client sends POST \/api\/v1\/auth\/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "Short1!Ab" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(
  /^the client sends POST \/api\/v1\/auth\/register with body \{ "username": "alice", "email": "alice@example\.com", "password": "AllUpperCase1234" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  "{string} has had the maximum number of failed login attempts",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given(
  "a user {string} is registered and locked after too many failed logins",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _username: string) => {
    throw new Error("TODO: not implemented");
  },
);

Given("an admin has unlocked alice's account", async () => {
  throw new Error("TODO: not implemented");
});
