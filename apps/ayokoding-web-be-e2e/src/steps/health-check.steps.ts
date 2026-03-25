import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { buildTrpcUrl, extractTrpcData } from "./helpers";

const { Given, When, Then, And } = createBdd();

Given("the API is running", async () => {});

When("the client calls meta.health", async function ({ request }) {
  const url = buildTrpcUrl("meta.health", undefined);
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.healthResult = extractTrpcData(body);
});

Then('the response should contain "status" equal to "ok"', async function () {
  expect(this.healthResult).toMatchObject({ status: "ok" });
});

When("the client calls meta.languages", async function ({ request }) {
  const url = buildTrpcUrl("meta.languages", undefined);
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.languagesResult = extractTrpcData(body) as { code: string; label: string }[];
});

Then('the response should contain a non-null "languages" array', async function () {
  expect(this.languagesResult).not.toBeNull();
  expect(Array.isArray(this.languagesResult)).toBe(true);
});

And('the "languages" array should include "en"', async function () {
  expect(this.languagesResult.some((l: { code: string }) => l.code === "en")).toBe(true);
});

And('the "languages" array should include "id"', async function () {
  expect(this.languagesResult.some((l: { code: string }) => l.code === "id")).toBe(true);
});
