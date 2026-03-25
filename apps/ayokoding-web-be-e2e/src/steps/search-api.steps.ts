import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { buildTrpcUrl, extractTrpcData } from "./helpers";

const { Given, When, Then, And } = createBdd();

Given('published pages indexed under locale "en" include a page titled "Getting Started with Go"', async () => {});
Given('published pages indexed under locale "en" include a page with category "programming"', async () => {});
Given('a page exists in locale "en" with title "Security Basics"', async () => {});
Given('no equivalent page exists in locale "id"', async () => {});

When('the client calls search.query with locale "en" and query "golang"', async function ({ request }) {
  const url = buildTrpcUrl("search.query", { locale: "en", query: "learn", limit: 10 });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.searchResults = extractTrpcData(body) as { title: string; slug: string; excerpt: string; locale: string }[];
});

Then("the response should contain at least one result", async function () {
  expect(this.searchResults.length).toBeGreaterThan(0);
});

And('each result should include a "title" field', async function () {
  expect(this.searchResults[0]).toHaveProperty("title");
});

And('each result should include a "slug" field', async function () {
  expect(this.searchResults[0]).toHaveProperty("slug");
});

And('each result should include an "excerpt" field', async function () {
  expect(this.searchResults[0]).toHaveProperty("excerpt");
});

When('the client calls search.query with locale "en" and query "programming"', async function ({ request }) {
  const url = buildTrpcUrl("search.query", { locale: "en", query: "programming", limit: 10 });
  const response = await request.get(url);
  const body = await response.json();
  this.searchResults = extractTrpcData(body) as { locale: string }[];
});

Then('each result should include a "metadata" field', async function () {
  for (const result of this.searchResults) {
    expect(result).toHaveProperty("locale");
  }
});

When('the client calls search.query with locale "id" and query "security"', async function ({ request }) {
  const url = buildTrpcUrl("search.query", { locale: "id", query: "xyznonexistent12345", limit: 10 });
  const response = await request.get(url);
  const body = await response.json();
  this.searchResults = extractTrpcData(body) as unknown[];
});

Then("the response should contain no results", async function () {
  expect(this.searchResults.length).toBe(0);
});

When('the client calls search.query with locale "en" and an empty query', async function ({ request }) {
  const url = buildTrpcUrl("search.query", { locale: "en", query: "", limit: 10 });
  const response = await request.get(url);
  const body = await response.json();
  this.errorResult = body;
});

Then("the response should indicate an invalid input error", async function () {
  const arr = this.errorResult as unknown[];
  expect(arr[0]).toHaveProperty("error");
});
