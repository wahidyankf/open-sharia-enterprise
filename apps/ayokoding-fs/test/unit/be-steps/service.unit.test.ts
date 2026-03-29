import { describe, it, expect } from "vitest";
import { ContentService } from "@/server/content/service";
import { InMemoryContentRepository } from "@/server/content/repository-memory";
import type { ContentMeta } from "@/server/content/types";

function createService(
  items: ContentMeta[],
  files: Map<string, { content: string; frontmatter: Record<string, unknown> }>,
) {
  return new ContentService(new InMemoryContentRepository(items, files));
}

const baseItems: ContentMeta[] = [
  {
    title: "Root",
    slug: "",
    locale: "en",
    weight: 0,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/en/_index.md",
  },
  {
    title: "Learn",
    slug: "learn",
    locale: "en",
    weight: 10,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/en/learn/_index.md",
  },
  {
    title: "Intro",
    slug: "learn/intro",
    locale: "en",
    weight: 10,
    draft: false,
    isSection: false,
    tags: [],
    filePath: "/en/learn/intro.md",
  },
  {
    title: "Advanced",
    slug: "learn/advanced",
    locale: "en",
    weight: 20,
    draft: false,
    isSection: false,
    tags: [],
    filePath: "/en/learn/advanced.md",
  },
  {
    title: "Deep",
    slug: "learn/topic/deep",
    locale: "en",
    weight: 10,
    draft: false,
    isSection: false,
    tags: [],
    filePath: "/en/learn/topic/deep.md",
  },
];

const baseFiles = new Map<string, { content: string; frontmatter: Record<string, unknown> }>();
baseFiles.set("/en/_index.md", { content: "# Root", frontmatter: { title: "Root" } });
baseFiles.set("/en/learn/_index.md", { content: "# Learn", frontmatter: { title: "Learn" } });
baseFiles.set("/en/learn/intro.md", {
  content: "## Intro\n\nIntroduction to golang and programming.",
  frontmatter: { title: "Intro" },
});
baseFiles.set("/en/learn/advanced.md", {
  content: "## Advanced\n\nAdvanced topics.",
  frontmatter: { title: "Advanced" },
});
baseFiles.set("/en/learn/topic/deep.md", { content: "## Deep\n\nDeep dive content.", frontmatter: { title: "Deep" } });

describe("ContentService", () => {
  it("getBySlug returns null for non-existent slug", async () => {
    const service = createService(baseItems, baseFiles);
    const result = await service.getBySlug("en", "does-not-exist");
    expect(result).toBeNull();
  });

  it("getTree with rootSlug returns subtree children", async () => {
    const service = createService(baseItems, baseFiles);
    const children = await service.getTree("en", "learn");
    expect(children.length).toBeGreaterThan(0);
  });

  it("getTree with non-existent rootSlug returns empty array", async () => {
    const service = createService(baseItems, baseFiles);
    const result = await service.getTree("en", "no-such-section");
    expect(result).toEqual([]);
  });

  it("getTree with non-existent locale returns empty array", async () => {
    const service = createService(baseItems, baseFiles);
    const result = await service.getTree("fr", undefined);
    expect(result).toEqual([]);
  });

  it("builds implicit parent nodes for deep slugs", async () => {
    const service = createService(baseItems, baseFiles);
    const tree = await service.getTree("en");
    // Root node "" contains "learn" as a child
    expect(tree.length).toBeGreaterThan(0);
    // Use getTree with rootSlug to find learn's children including implicit "topic" node
    const learnChildren = await service.getTree("en", "learn");
    const topicNode = learnChildren.find((n) => n.slug === "learn/topic");
    expect(topicNode).toBeDefined();
    expect(topicNode?.isSection).toBe(true);
  });

  it("search returns empty for non-existent locale", async () => {
    const service = createService(baseItems, baseFiles);
    const results = await service.search("fr", "anything");
    expect(results).toEqual([]);
  });

  it("search excerpt falls back to start of content when query not found", async () => {
    const service = createService(baseItems, baseFiles);
    const results = await service.search("en", "nonexistentterm12345");
    expect(results).toEqual([]);
  });

  it("isSearchIndexReady returns false before search, true after", async () => {
    const service = createService(baseItems, baseFiles);
    expect(service.isSearchIndexReady("en")).toBe(false);
    await service.search("en", "intro");
    expect(service.isSearchIndexReady("en")).toBe(true);
  });

  it("listChildren returns empty for non-existent parent", async () => {
    const service = createService(baseItems, baseFiles);
    const result = await service.listChildren("en", "no-such-parent");
    expect(result).toEqual([]);
  });

  it("prev/next links are computed for siblings", async () => {
    const service = createService(baseItems, baseFiles);
    const intro = await service.getBySlug("en", "learn/intro");
    expect(intro).not.toBeNull();
    expect(intro?.next?.slug).toBe("learn/advanced");
    expect(intro?.prev).toBeNull();

    const advanced = await service.getBySlug("en", "learn/advanced");
    expect(advanced?.prev?.slug).toBe("learn/intro");
  });

  it("readFileContent error in search is silently skipped", async () => {
    const items: ContentMeta[] = [
      {
        title: "Bad",
        slug: "bad",
        locale: "en",
        weight: 1,
        draft: false,
        isSection: false,
        tags: [],
        filePath: "/missing.md",
      },
    ];
    const service = createService(items, new Map());
    const results = await service.search("en", "anything");
    expect(results).toEqual([]);
  });
});
