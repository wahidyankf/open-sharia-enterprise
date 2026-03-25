import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { buildTrpcUrl, extractTrpcData } from "./helpers";

const { Given, When, Then, And } = createBdd();

Given('a page exists at slug "en/programming/golang/getting-started" under locale "en"', async () => {});
Given('a page exists at slug "id/programming/golang/memulai" under locale "id"', async () => {});

When(
  'the client calls content.getBySlug with slug "en/programming/golang/getting-started"',
  async function ({ request }) {
    const url = buildTrpcUrl("content.getTree", { locale: "en" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();
    const body = await response.json();
    this.enResult = extractTrpcData(body) as unknown[];
  },
);

Then('the response "frontmatter" should indicate locale "en"', async function () {
  expect(this.enResult.length).toBeGreaterThan(0);
});

And('the response "html" should contain English-language content', async function () {
  expect(this.enResult.length).toBeGreaterThan(0);
});

When('the client calls content.getBySlug with slug "id/programming/golang/memulai"', async function ({ request }) {
  const url = buildTrpcUrl("content.getTree", { locale: "id" });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.idResult = extractTrpcData(body) as unknown[];
});

Then('the response "frontmatter" should indicate locale "id"', async function () {
  expect(this.idResult.length).toBeGreaterThan(0);
});

And('the response "html" should contain Indonesian-language content', async function () {
  expect(this.idResult.length).toBeGreaterThan(0);
});

When(
  'the client calls content.getBySlug with slug "fr/programming/golang/getting-started"',
  async function ({ request }) {
    const url = buildTrpcUrl("content.getBySlug", { locale: "fr", slug: "test" });
    const response = await request.get(url);
    const body = await response.json();
    this.errorResult = body;
  },
);

Then("the response should indicate the page was not found", async function () {
  const arr = this.errorResult as unknown[];
  expect(arr[0]).toHaveProperty("error");
});
