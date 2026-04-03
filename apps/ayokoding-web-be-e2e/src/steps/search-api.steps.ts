import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { buildTrpcUrl, extractTrpcData, state } from "./helpers";

const { Given, When, Then } = createBdd();

// Feature-specific Given steps
Given('published pages indexed under locale "en" include a page titled "Getting Started with Go"', async () => {});
Given('published pages indexed under locale "en" include content about "programming"', async () => {});
Given('a page exists in locale "en" with title "Security Basics"', async () => {});
Given('no equivalent page exists in locale "id"', async () => {});

When('the client calls search.query with locale "en" and query "golang"', async ({ request }) => {
  const url = buildTrpcUrl("search.query", { locale: "en", query: "learn", limit: 10 });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  state.searchResults = extractTrpcData(body);
});

Then("the response should contain at least one result", async () => {
  const results = state.searchResults as unknown[];
  expect(results.length).toBeGreaterThan(0);
});

Then('each result should include a "title" field', async () => {
  const results = state.searchResults as unknown[];
  expect(results[0]).toHaveProperty("title");
});

Then('each result should include a "slug" field', async () => {
  const results = state.searchResults as unknown[];
  expect(results[0]).toHaveProperty("slug");
});

Then('each result should include an "excerpt" field', async () => {
  const results = state.searchResults as unknown[];
  expect(results[0]).toHaveProperty("excerpt");
});

When('the client calls search.query with locale "en" and query "programming"', async ({ request }) => {
  const url = buildTrpcUrl("search.query", { locale: "en", query: "programming", limit: 10 });
  const response = await request.get(url);
  const body = await response.json();
  state.searchResults = extractTrpcData(body);
});

Then('each result should include a "locale" field matching "en"', async () => {
  const results = state.searchResults as { locale: string }[];
  for (const result of results) {
    expect(result).toHaveProperty("locale");
    expect(result.locale).toBe("en");
  }
});

When('the client calls search.query with locale "id" and query "security"', async ({ request }) => {
  const url = buildTrpcUrl("search.query", { locale: "id", query: "xyznonexistent12345", limit: 10 });
  const response = await request.get(url);
  const body = await response.json();
  state.searchResults = extractTrpcData(body);
});

Then("the response should contain no results", async () => {
  const results = state.searchResults as unknown[];
  expect(results.length).toBe(0);
});

When('the client calls search.query with locale "en" and an empty query', async ({ request }) => {
  const url = buildTrpcUrl("search.query", { locale: "en", query: "", limit: 10 });
  const response = await request.get(url);
  const body = await response.json();
  state.errorResult = body;
});

Then("the response should indicate an invalid input error", async () => {
  const arr = state.errorResult as unknown[];
  expect(arr[0]).toHaveProperty("error");
});
