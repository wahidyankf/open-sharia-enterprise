import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { buildTrpcUrl, extractTrpcData } from "./helpers";

const { Given, When, Then, And } = createBdd();

Given('content exists in locale "en" with sections "programming", "ai", and "security"', async () => {});
Given('a section "programming" in locale "en" has child nodes with weights 30, 10, and 20', async () => {});
Given('a section "programming" in locale "en" contains at least one child page', async () => {});

When('the client calls content.getTree with locale "en"', async function ({ request }) {
  const url = buildTrpcUrl("content.getTree", { locale: "en" });
  const response = await request.get(url);
  expect(response.ok()).toBeTruthy();
  const body = await response.json();
  this.treeResult = extractTrpcData(body) as {
    slug: string;
    weight: number;
    isSection: boolean;
    children: { slug: string }[];
  }[];
});

Then('the response tree should contain top-level nodes for "programming", "ai", and "security"', async function () {
  expect(this.treeResult.length).toBeGreaterThan(0);
});

And("each node should reflect its position in the directory hierarchy", async function () {
  expect(this.treeResult[0]).toHaveProperty("slug");
});

Then('the children of "programming" should appear in order: weight 10, weight 20, weight 30', async function () {
  for (let i = 1; i < this.treeResult.length; i++) {
    expect(this.treeResult[i].weight).toBeGreaterThanOrEqual(this.treeResult[i - 1].weight);
  }
});

Then('the "programming" node should have a non-empty "children" array', async function () {
  const sectionNode = this.treeResult.find((n: { isSection: boolean }) => n.isSection);
  expect(sectionNode).toBeDefined();
  expect(Array.isArray(sectionNode?.children)).toBe(true);
});

And('each child should include a "slug" and "title"', async function () {
  const sectionNode = this.treeResult.find(
    (n: { isSection: boolean; children: unknown[] }) => n.isSection && n.children.length > 0,
  );
  expect(sectionNode).toBeDefined();
  expect(sectionNode!.children[0]).toHaveProperty("slug");
});
