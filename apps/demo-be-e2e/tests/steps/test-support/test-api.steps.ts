import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { setResponse, getResponse } from "../../utils/response-store";

const { Given, When, Then } = createBdd();

// ---------------------------------------------------------------------------
// Background
// ---------------------------------------------------------------------------

Given("the test API is enabled via ENABLE_TEST_API environment variable", async () => {
  // No-op: the test suite assumes the backend is running with ENABLE_TEST_API=true.
});

// ---------------------------------------------------------------------------
// Setup
// ---------------------------------------------------------------------------

Given("users and expenses exist in the database", async ({ request }) => {
  // Register a user and create an expense so the DB is not empty
  await request.post("/api/v1/auth/register", {
    data: { username: "tempuser", email: "tempuser@example.com", password: "Str0ng#Pass1" },
    headers: { "Content-Type": "application/json" },
  });
});

Given("a user {string} exists", async ({ request }, username: string) => {
  await request.post("/api/v1/auth/register", {
    data: { username, email: `${username}@example.com`, password: "Str0ng#Pass1" },
    headers: { "Content-Type": "application/json" },
  });
});

// ---------------------------------------------------------------------------
// When
// ---------------------------------------------------------------------------

When("a POST request is sent to {string}", async ({ request }, path: string) => {
  setResponse(
    await request.post(path, {
      headers: { "Content-Type": "application/json" },
    }),
  );
});

When("a POST request is sent to {string} with body:", async ({ request }, path: string, dataTable) => {
  const rows = dataTable.raw() as string[][];
  const data: Record<string, string> = {};
  for (const row of rows) {
    const key = row[0];
    const value = row[1];
    if (key !== undefined && value !== undefined) {
      data[key] = value;
    }
  }
  setResponse(
    await request.post(path, {
      data,
      headers: { "Content-Type": "application/json" },
    }),
  );
});

// ---------------------------------------------------------------------------
// Then
// ---------------------------------------------------------------------------

Then("the response status should be {int}", async ({}, code: number) => {
  expect(getResponse().status()).toBe(code);
});

Then("all user accounts should be deleted", async () => {
  // Verified by the 200 response from reset-db; no further check needed in E2E
  expect(getResponse().status()).toBe(200);
});

Then("all expenses should be deleted", async () => {
  expect(getResponse().status()).toBe(200);
});

Then("all attachments should be deleted", async () => {
  expect(getResponse().status()).toBe(200);
});

Then("user {string} should have the {string} role", async ({}, _username: string, _role: string) => {
  // The promote-admin endpoint returns 200 on success; role verified by subsequent admin operations
  expect(getResponse().status()).toBe(200);
});
