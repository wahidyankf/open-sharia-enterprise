import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { setResponse, getResponse } from "../../utils/response-store";
import { setToken } from "../../utils/token-store";

const { Given, When, Then } = createBdd();

When(/^a client sends POST \/api\/v1\/auth\/register with body:$/, async ({ request }, body: string) => {
  setResponse(
    await request.post("/api/v1/auth/register", {
      data: JSON.parse(body) as Record<string, unknown>,
      headers: { "Content-Type": "application/json" },
    }),
  );
});

When(/^a client sends POST \/api\/v1\/auth\/login with body:$/, async ({ request }, body: string) => {
  setResponse(
    await request.post("/api/v1/auth/login", {
      data: JSON.parse(body) as Record<string, unknown>,
      headers: { "Content-Type": "application/json" },
    }),
  );
});

Given("a user {string} is already registered", async ({ request }, username: string) => {
  await request.post("/api/v1/auth/register", {
    data: { username, password: "s3cur3Pass!" },
    headers: { "Content-Type": "application/json" },
  });
});

Given(
  "a user {string} is already registered with password {string}",
  async ({ request }, username: string, password: string) => {
    await request.post("/api/v1/auth/register", {
      data: { username, password },
      headers: { "Content-Type": "application/json" },
    });
  },
);

Given("the client has logged in as {string} and stored the JWT token", async ({ request }, username: string) => {
  const response = await request.post("/api/v1/auth/login", {
    data: { username, password: "s3cur3Pass!" },
    headers: { "Content-Type": "application/json" },
  });
  const body = (await response.json()) as { token: string };
  setToken(body.token);
});

Then(
  "the response body should contain {string} equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string, value: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).toBe(value);
  },
);

Then(
  "the response body should not contain a {string} field",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).toBeUndefined();
  },
);

Then(
  "the response body should contain a non-null {string} field",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).not.toBeNull();
    expect(body[field]).toBeDefined();
  },
);

Then(
  "the response body should contain a {string} field",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).toBeDefined();
  },
);

Then("the response body should contain an error message about duplicate username", async () => {
  const body = (await getResponse().json()) as { message: string };
  expect(body.message).toMatch(/already exists/i);
});

Then("the response body should contain an error message about invalid credentials", async () => {
  const body = (await getResponse().json()) as { message: string };
  expect(body.message).toMatch(/invalid/i);
});

Then(
  "the response body should contain a validation error for {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _field: string) => {
    const status = getResponse().status();
    expect(status).toBe(400);
  },
);
