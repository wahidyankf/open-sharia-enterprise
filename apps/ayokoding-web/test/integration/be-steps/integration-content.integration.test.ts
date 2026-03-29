import { describe, it, expect } from "vitest";
import path from "path";
import fs from "fs/promises";

const CONTENT_DIR = path.resolve(process.cwd(), "content");

describe("integration: content directory", () => {
  it("content directory exists", async () => {
    const stat = await fs.stat(CONTENT_DIR);
    expect(stat.isDirectory()).toBe(true);
  });

  it("contains en and id locales", async () => {
    const entries = await fs.readdir(CONTENT_DIR);
    expect(entries).toContain("en");
    expect(entries).toContain("id");
  });

  it("contains markdown files", async () => {
    const enDir = path.join(CONTENT_DIR, "en");
    const entries = await fs.readdir(enDir);
    const mdFiles = entries.filter((f) => f.endsWith(".md"));
    expect(mdFiles.length).toBeGreaterThan(0);
  });
});
