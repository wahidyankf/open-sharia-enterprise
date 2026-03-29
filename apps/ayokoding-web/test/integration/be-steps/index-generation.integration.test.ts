import path from "path";
import { describe, it, expect } from "vitest";
import { processAllIndexFiles } from "../../../src/server/content/index-generator";

const CONTENT_DIR = path.resolve(process.cwd(), "content");

describe("integration: index generation", () => {
  it("all _index.md files are up to date with the content directory", async () => {
    const result = await processAllIndexFiles(CONTENT_DIR, "validate");

    if (result.changed.length > 0) {
      const files = result.changed.map((f) => path.relative(CONTENT_DIR, f));
      throw new Error(
        `${files.length} _index.md file(s) are out of date:\n${files.map((f) => `  ${f}`).join("\n")}\n\nRun "npx tsx src/scripts/generate-indexes.ts" to fix.`,
      );
    }

    expect(result.errors).toHaveLength(0);
  });
});
