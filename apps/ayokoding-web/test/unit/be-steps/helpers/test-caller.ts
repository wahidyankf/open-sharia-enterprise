import { vi } from "vitest";
import type { ContentMeta } from "@/server/content/types";

const { mockContentMap, mockIndex, mockContentHtml, mockHeadings } = vi.hoisted(() => {
  const mockContentMeta = [
    {
      title: "Learn",
      slug: "learn",
      locale: "en",
      weight: 10,
      draft: false,
      isSection: true,
      tags: [],
      filePath: "/mock/en/learn/_index.md",
    },
    {
      title: "Overview",
      slug: "learn/overview",
      locale: "en",
      weight: 100000,
      description: "Learning path overview",
      draft: false,
      isSection: false,
      tags: ["learning"],
      filePath: "/mock/en/learn/overview.md",
    },
    {
      title: "Software Engineering",
      slug: "learn/software-engineering",
      locale: "en",
      weight: 200,
      draft: false,
      isSection: true,
      tags: [],
      filePath: "/mock/en/learn/software-engineering/_index.md",
    },
    {
      title: "Belajar",
      slug: "belajar",
      locale: "id",
      weight: 10,
      draft: false,
      isSection: true,
      tags: [],
      filePath: "/mock/id/belajar/_index.md",
    },
    {
      title: "Ikhtisar",
      slug: "belajar/ikhtisar",
      locale: "id",
      weight: 100000,
      draft: false,
      isSection: false,
      tags: [],
      filePath: "/mock/id/belajar/ikhtisar.md",
    },
  ];

  const contentMap = new Map();
  for (const meta of mockContentMeta) {
    contentMap.set(`${meta.locale}:${meta.slug}`, meta);
  }

  const prevNext = new Map();
  prevNext.set("en:learn/overview", { prev: null, next: null });

  const enTree = [
    {
      title: "Learn",
      slug: "learn",
      weight: 10,
      isSection: true,
      children: [
        { title: "Overview", slug: "learn/overview", weight: 100000, isSection: false, children: [] },
        {
          title: "Software Engineering",
          slug: "learn/software-engineering",
          weight: 200,
          isSection: true,
          children: [],
        },
      ],
    },
  ];

  const idTree = [
    {
      title: "Belajar",
      slug: "belajar",
      weight: 10,
      isSection: true,
      children: [{ title: "Ikhtisar", slug: "belajar/ikhtisar", weight: 100000, isSection: false, children: [] }],
    },
  ];

  return {
    mockContentMap: contentMap,
    mockIndex: { contentMap, trees: { en: enTree, id: idTree }, prevNext },
    mockContentHtml: `<h2 id="getting-started">Getting Started</h2><p>Test content</p><pre><code class="language-go">package main</code></pre>`,
    mockHeadings: [{ id: "getting-started", text: "Getting Started", level: 2 }],
  };
});

vi.mock("@/server/content/index", () => ({
  getContentIndex: vi.fn().mockResolvedValue(mockIndex),
  getContentMeta: vi.fn((_index: unknown, locale: string, slug: string) => {
    return mockContentMap.get(`${locale}:${slug}`);
  }),
  listChildren: vi.fn((_index: unknown, locale: string, parentSlug: string) => {
    const children: ContentMeta[] = [];
    for (const [key, meta] of mockContentMap) {
      if (!key.startsWith(`${locale}:`)) continue;
      const typedMeta = meta as ContentMeta;
      const parts = typedMeta.slug.split("/");
      const parent = parts.slice(0, -1).join("/");
      if (parent === parentSlug && typedMeta.slug !== parentSlug) {
        children.push(typedMeta);
      }
    }
    return children.sort((a: ContentMeta, b: ContentMeta) => a.weight - b.weight);
  }),
}));

vi.mock("@/server/content/reader", () => ({
  readFileContent: vi.fn().mockResolvedValue({ content: "# Test\nContent", frontmatter: { title: "Test" } }),
  readAllContent: vi.fn().mockResolvedValue([]),
  stripMarkdown: vi.fn((c: string) => c),
  getContentDir: vi.fn(() => "/mock/content"),
}));

vi.mock("@/server/content/parser", () => ({
  parseMarkdown: vi.fn().mockResolvedValue({ html: mockContentHtml, headings: mockHeadings }),
}));

vi.mock("@/server/content/search-index", () => ({
  buildSearchIndex: vi.fn().mockResolvedValue(undefined),
  searchContent: vi.fn((_locale: string, query: string) => {
    if (query.toLowerCase().includes("golang") || query.toLowerCase().includes("go")) {
      return [{ title: "Overview", slug: "learn/overview", excerpt: "...golang...", locale: "en" }];
    }
    return [];
  }),
  isSearchIndexReady: vi.fn().mockReturnValue(true),
}));

import { createCallerFactory } from "@/server/trpc/init";
import type { TRPCContext } from "@/server/trpc/init";
import { appRouter } from "@/server/trpc/router";
import { ContentService } from "@/server/content/service";
import { InMemoryContentRepository } from "@/server/content/repository-memory";

// Provide a typed context — actual calls are intercepted by vi.mock() above
const stubRepository = new InMemoryContentRepository([], new Map());
const stubContext: TRPCContext = { contentService: new ContentService(stubRepository) };

const createCaller = createCallerFactory(appRouter);
export const testCaller = createCaller(stubContext);
