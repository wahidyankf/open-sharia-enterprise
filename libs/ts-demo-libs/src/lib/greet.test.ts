import { test } from "node:test";
import assert from "node:assert";
import { greet } from "./greet";

test("greet returns greeting message", () => {
  assert.strictEqual(greet("World"), "Hello, World!");
});

test("greet with different name", () => {
  assert.strictEqual(greet("Next.js"), "Hello, Next.js!");
});
