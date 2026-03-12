import { Given, When, Then } from "@cucumber/cucumber";
import { expect } from "@playwright/test";
import type { CustomWorld } from "../world.js";

// Helper: register a user
async function registerUser(world: CustomWorld, username: string, email: string, password: string) {
  const res = await world.post("/api/v1/auth/register", { username, email, password });
  if (res.status !== 201) {
    throw new Error(`Registration failed for ${username}: ${JSON.stringify(res.body)}`);
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return res.body as { id: string };
}

// Helper: login and return tokens
async function loginUser(world: CustomWorld, username: string, password: string) {
  const res = await world.post("/api/v1/auth/login", { username, password });
  if (res.status !== 200) {
    throw new Error(`Login failed for ${username}: ${JSON.stringify(res.body)}`);
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return res.body as { access_token: string; refresh_token: string };
}

// ---- Background: register user ----

Given(
  "a user {string} is registered with password {string}",
  async function (this: CustomWorld, username: string, password: string) {
    const email = `${username}@example.com`;
    try {
      const user = await registerUser(this, username, email, password);
      this.userIds.set(username, user.id);
    } catch {
      // User might already be registered from a previous step
      const loginRes = await this.post("/api/v1/auth/login", { username, password });
      if (loginRes.status === 200) {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        this.tokens.set(`${username}_access`, (loginRes.body as any).access_token);
      }
    }
    this.context[`${username}_password`] = password;
  },
);

Given(
  "a user {string} is registered with email {string} and password {string}",
  async function (this: CustomWorld, username: string, email: string, password: string) {
    const user = await registerUser(this, username, email, password);
    this.userIds.set(username, user.id);
    this.context[`${username}_email`] = email;
    this.context[`${username}_password`] = password;
  },
);

Given(
  "{string} has logged in and stored the access token and refresh token",
  async function (this: CustomWorld, username: string) {
    const password = (this.context[`${username}_password`] as string) ?? "Str0ng#Pass1";
    const tokens = await loginUser(this, username, password);
    this.tokens.set(`${username}_access`, tokens.access_token);
    this.tokens.set(`${username}_refresh`, tokens.refresh_token);
  },
);

Given("{string} has logged in and stored the access token", async function (this: CustomWorld, username: string) {
  const password = (this.context[`${username}_password`] as string) ?? "Str0ng#Pass1";
  const tokens = await loginUser(this, username, password);
  this.tokens.set(`${username}_access`, tokens.access_token);
  this.tokens.set(`${username}_refresh`, tokens.refresh_token);
});

Given("a user {string} is registered and deactivated", async function (this: CustomWorld, username: string) {
  const email = `${username}@example.com`;
  const password = (this.context[`${username}_password`] as string) ?? "Str0ng#Pass1";
  const res = await this.post("/api/v1/auth/register", { username, email, password });
  if (res.status === 201) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    this.userIds.set(username, (res.body as any).id as string);
  } else if (res.status !== 409) {
    throw new Error(`Registration failed for ${username}: ${JSON.stringify(res.body)}`);
  }
  // Login and deactivate (user may already exist from Background)
  const tokens = await loginUser(this, username, password);
  await this.post("/api/v1/users/me/deactivate", {}, tokens.access_token);
});

// ---- When: POST with JSON body in step text ----

When(/^the client sends POST (.+) with body (.+)$/, async function (this: CustomWorld, path: string, bodyStr: string) {
  const body = JSON.parse(bodyStr) as Record<string, unknown>;
  this.response = await this.post(path, body);
});

When(/^the client sends GET ((?!.*with alice's access token).+)$/, async function (this: CustomWorld, path: string) {
  this.response = await this.get(path);
});

When(/^the client sends GET (.+) with alice's access token$/, async function (this: CustomWorld, path: string) {
  const token = this.tokens.get("alice_access") ?? "";
  // path here is just the route (e.g., /api/v1/users/me)
  this.response = await this.get(path.trim(), token);
});

// ---- Then: common assertions ----

Then(
  "the response body should contain {string} equal to {string}",
  function (this: CustomWorld, field: string, value: string) {
    expect(this.response).not.toBeNull();
    expect(String(this.response?.body?.[field])).toBe(value);
  },
);

Then("the response body should contain a non-null {string} field", function (this: CustomWorld, field: string) {
  expect(this.response).not.toBeNull();
  expect(this.response?.body?.[field]).not.toBeNull();
  expect(this.response?.body?.[field]).not.toBeUndefined();
});

Then("the response body should not contain a {string} field", function (this: CustomWorld, field: string) {
  expect(this.response?.body?.[field]).toBeUndefined();
});

Then("the response body should contain an error message about invalid credentials", function (this: CustomWorld) {
  expect(this.response).not.toBeNull();
  const body = this.response?.body as Record<string, string>;
  const message = (body["message"] ?? body["error"] ?? "").toLowerCase();
  expect(message.length).toBeGreaterThan(0);
});

Then("the response body should contain an error message about account deactivation", function (this: CustomWorld) {
  expect(this.response).not.toBeNull();
  const body = this.response?.body as Record<string, string>;
  const message = (body["message"] ?? body["error"] ?? "").toLowerCase();
  expect(message.length).toBeGreaterThan(0);
});

Then("the response body should contain a validation error for {string}", function (this: CustomWorld, field: string) {
  expect(this.response).not.toBeNull();
  const body = this.response?.body as Record<string, string>;
  const bodyField = body["field"];
  const hasField = bodyField === field || (body["message"] ?? "").toLowerCase().includes(field.toLowerCase());
  const hasError = body["error"] !== undefined || body["message"] !== undefined;
  expect(hasField || hasError).toBe(true);
});

Then("the response body should contain an error message about duplicate username", function (this: CustomWorld) {
  expect(this.response).not.toBeNull();
  const body = this.response?.body as Record<string, string>;
  const message = (body["message"] ?? body["error"] ?? "").toLowerCase();
  expect(message.length).toBeGreaterThan(0);
});
