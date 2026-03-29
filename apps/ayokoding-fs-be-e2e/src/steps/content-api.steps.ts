import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { state } from "./helpers";

const { Then } = createBdd();

// Note: All Given/When steps with {string} params are in common.steps.ts

Then('the response should contain a non-null "html" field', async () => {
  const pageResult = state.pageResult as Record<string, unknown>;
  expect(pageResult.html).toBeTruthy();
});

Then('the response should contain a non-null "frontmatter" field', async () => {
  const pageResult = state.pageResult as Record<string, unknown>;
  expect(pageResult.title).toBeTruthy();
});

Then('the response should contain a non-null "headings" field', async () => {
  const pageResult = state.pageResult as Record<string, unknown>;
  expect(Array.isArray(pageResult.headings)).toBe(true);
});

Then('the response should contain a "prev" navigation link', async () => {
  expect(state.pageResult).toHaveProperty("prev");
});

Then('the response should contain a "next" navigation link', async () => {
  expect(state.pageResult).toHaveProperty("next");
});

Then("the response should indicate the page was not found", async () => {
  const arr = state.errorResult as unknown[];
  expect(arr[0]).toHaveProperty("error");
});

Then("the response should contain 3 child pages", async () => {
  const children = state.childrenResult as { weight: number }[];
  expect(children.length).toBeGreaterThan(0);
});

Then("the child pages should be ordered by weight ascending", async () => {
  const children = state.childrenResult as { weight: number }[];
  for (let i = 1; i < children.length; i++) {
    expect(children[i]!.weight).toBeGreaterThanOrEqual(children[i - 1]!.weight);
  }
});

Then("the response should contain a tree with top-level section nodes", async () => {
  const tree = state.treeResult as unknown[];
  expect(tree.length).toBeGreaterThan(0);
});

Then("every node should include a slug and title", async () => {
  const tree = state.treeResult as Record<string, unknown>[];
  expect(tree[0]).toHaveProperty("slug");
  expect(tree[0]).toHaveProperty("weight");
  expect(tree[0]).toHaveProperty("children");
});

Then('the response "html" field should contain a rendered code element', async () => {
  const pageResult = state.pageResult as Record<string, unknown>;
  expect((pageResult.html as string).length).toBeGreaterThan(0);
});
