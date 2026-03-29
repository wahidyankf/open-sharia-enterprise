import type { ContentMeta } from "@/server/content/types";
import { ContentService } from "@/server/content/service";
import { InMemoryContentRepository } from "@/server/content/repository-memory";

const mockContentMeta: ContentMeta[] = [
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
    title: "Programming Languages",
    slug: "learn/software-engineering/programming-languages",
    locale: "en",
    weight: 100,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/mock/en/learn/software-engineering/programming-languages/_index.md",
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

const mockFiles = new Map<string, { content: string; frontmatter: Record<string, unknown> }>();

mockFiles.set("/mock/en/learn/_index.md", {
  content: "# Learn\n\nWelcome to the learning path.",
  frontmatter: { title: "Learn", weight: 10 },
});

mockFiles.set("/mock/en/learn/overview.md", {
  content:
    '## Getting Started\n\nThis is the overview page about golang programming.\n\n```go\npackage main\n\nfunc main() {\n    fmt.Println("Hello")\n}\n```',
  frontmatter: { title: "Overview", weight: 100000, description: "Learning path overview", tags: ["learning"] },
});

mockFiles.set("/mock/en/learn/software-engineering/_index.md", {
  content: "# Software Engineering\n\nSoftware engineering fundamentals.",
  frontmatter: { title: "Software Engineering", weight: 200 },
});

mockFiles.set("/mock/en/learn/software-engineering/programming-languages/_index.md", {
  content: "# Programming Languages\n\nLearn programming languages.",
  frontmatter: { title: "Programming Languages", weight: 100 },
});

mockFiles.set("/mock/id/belajar/_index.md", {
  content: "# Belajar\n\nSelamat datang di jalur pembelajaran.",
  frontmatter: { title: "Belajar", weight: 10 },
});

mockFiles.set("/mock/id/belajar/ikhtisar.md", {
  content: "## Memulai\n\nIni adalah halaman ikhtisar.",
  frontmatter: { title: "Ikhtisar", weight: 100000 },
});

const repository = new InMemoryContentRepository(mockContentMeta, mockFiles);

export const testContentService = new ContentService(repository);
