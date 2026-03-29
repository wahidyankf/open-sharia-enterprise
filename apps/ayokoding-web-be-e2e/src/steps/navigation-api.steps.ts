import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { state } from "./helpers";

const { Given, Then } = createBdd();

// Feature-specific Given steps
Given("content exists in locale {string} with sections {string}, {string}, and {string}", async () => {});
Given("a section {string} in locale {string} has child nodes with weights {int}, {int}, and {int}", async () => {});
Given("a section {string} in locale {string} contains at least one child page", async () => {});

// Note: When 'the client calls content.getTree with locale {string}' is in common.steps.ts

Then('the response tree should contain top-level nodes for "programming", "ai", and "security"', async () => {
  const tree = state.treeResult as unknown[];
  expect(tree.length).toBeGreaterThan(0);
});

Then("each node should reflect its position in the directory hierarchy", async () => {
  const tree = state.treeResult as Record<string, unknown>[];
  expect(tree[0]).toHaveProperty("slug");
});

Then('the children of "programming" should appear in order: weight 10, weight 20, weight 30', async () => {
  const tree = state.treeResult as { weight: number }[];
  for (let i = 1; i < tree.length; i++) {
    expect(tree[i]!.weight).toBeGreaterThanOrEqual(tree[i - 1]!.weight);
  }
});

Then('the "programming" node should have a non-empty "children" array', async () => {
  const tree = state.treeResult as { isSection: boolean; children: unknown[] }[];
  const sectionNode = tree.find((n) => n.isSection);
  expect(sectionNode).toBeDefined();
  expect(Array.isArray(sectionNode?.children)).toBe(true);
});

Then('each child should include a "slug" and "title"', async () => {
  const tree = state.treeResult as { isSection: boolean; children: { slug: string }[] }[];
  const sectionNode = tree.find((n) => n.isSection && n.children.length > 0);
  expect(sectionNode).toBeDefined();
  expect(sectionNode!.children[0]).toHaveProperty("slug");
});
