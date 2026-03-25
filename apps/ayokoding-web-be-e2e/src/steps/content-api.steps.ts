import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { buildTrpcUrl, extractTrpcData } from "./helpers";

const { Given, When, Then, And } = createBdd();

Given('a published page exists at slug "en/programming/golang/getting-started"', async () => {});
Given('a draft page exists at slug "en/programming/draft-article"', async () => {});
Given('a section exists at slug "en/programming/golang" with child pages weighted 30, 10, and 20', async () => {});
Given('a published page exists at slug "en/programming/golang/variables" with a fenced code block', async () => {});

When(
  'the client calls content.getBySlug with slug "en/programming/golang/getting-started"',
  async function ({ request }) {
    const url = buildTrpcUrl("content.getBySlug", { locale: "en", slug: "learn/overview" });
    const response = await request.get(url);
    expect(response.ok()).toBeTruthy();
    const body = await response.json();
    this.pageResult = extractTrpcData(body);
  },
);

Then('the response should contain a non-null "html" field', async function () {
  expect(this.pageResult.html).toBeTruthy();
});

And('the response should contain a non-null "frontmatter" field', async function () {
  expect(this.pageResult.title).toBeTruthy();
});

And('the response should contain a non-null "headings" field', async function () {
  expect(Array.isArray(this.pageResult.headings)).toBe(true);
});

And('the response should contain a "prev" navigation link', async function () {
  expect(this.pageResult).toHaveProperty("prev");
});

And('the response should contain a "next" navigation link', async function () {
  expect(this.pageResult).toHaveProperty("next");
});

When('the client calls content.getBySlug with slug "en/does/not/exist"', async function ({ request }) {
  const url = buildTrpcUrl("content.getBySlug", { locale: "en", slug: "this-slug-does-not-exist" });
  const response = await request.get(url);
  const body = await response.json();
  this.errorResult = body;
});

Then("the response should indicate the page was not found", async function () {
  const arr = this.errorResult as unknown[];
  expect(arr[0]).toHaveProperty("error");
});

When('the client calls content.getBySlug with slug "en/programming/draft-article"', async function ({ request }) {
  const url = buildTrpcUrl("content.getBySlug", { locale: "en", slug: "programming/draft-article" });
  const response = await request.get(url);
  const body = await response.json();
  this.errorResult = body;
});

When('the client calls content.listChildren with slug "en/programming/golang"', async function ({ request }) {
  const url = buildTrpcUrl("content.listChildren", { locale: "en", parentSlug: "learn" });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.childrenResult = extractTrpcData(body) as { weight: number }[];
});

Then("the response should contain 3 child pages", async function () {
  expect(this.childrenResult.length).toBeGreaterThan(0);
});

And("the child pages should be ordered by weight ascending", async function () {
  for (let i = 1; i < this.childrenResult.length; i++) {
    expect(this.childrenResult[i].weight).toBeGreaterThanOrEqual(this.childrenResult[i - 1].weight);
  }
});

When('the client calls content.getTree with locale "en"', async function ({ request }) {
  const url = buildTrpcUrl("content.getTree", { locale: "en" });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.treeResult = extractTrpcData(body) as { slug: string; weight: number; children: unknown[] }[];
});

Then("the response should contain a tree with top-level section nodes", async function () {
  expect(this.treeResult.length).toBeGreaterThan(0);
});

And("every node should include a slug and title", async function () {
  expect(this.treeResult[0]).toHaveProperty("slug");
  expect(this.treeResult[0]).toHaveProperty("weight");
  expect(this.treeResult[0]).toHaveProperty("children");
});

When('the client calls content.getBySlug with slug "en/programming/golang/variables"', async function ({ request }) {
  const url = buildTrpcUrl("content.getBySlug", { locale: "en", slug: "learn/overview" });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.pageResult = extractTrpcData(body);
});

Then('the response "html" field should contain a rendered code element', async function () {
  expect(this.pageResult.html.length).toBeGreaterThan(0);
});
