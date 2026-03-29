import type { ContentMeta } from "@/server/content/types";

export const mockUpdateMetas: ContentMeta[] = [
  {
    title: "Phase 0 Week 4",
    slug: "updates/2025-12-14-phase-0-week-4-initial-commit",
    date: new Date("2025-12-14T00:00:00+07:00"),
    draft: false,
    description: "Week 4 update",
    tags: ["infrastructure"],
    summary: "Initial commit and project setup",
    weight: 0,
    isSection: false,
    filePath: "/mock/content/updates/2025-12-14-phase-0-week-4-initial-commit.md",
    readingTime: 5,
    category: "updates",
  },
  {
    title: "End of Phase 0",
    slug: "updates/2026-02-08-phase-0-end-of-phase-0",
    date: new Date("2026-02-08T00:00:00+07:00"),
    draft: false,
    description: "Phase 0 complete",
    tags: ["milestone", "phase-0"],
    summary: "1,200+ commits completing Phase 0",
    weight: 0,
    isSection: false,
    filePath: "/mock/content/updates/2026-02-08-phase-0-end-of-phase-0.md",
    readingTime: 10,
    category: "updates",
  },
];

export const mockAboutMeta: ContentMeta = {
  title: "About OSE Platform",
  slug: "about",
  date: new Date("2026-02-22T00:00:00+07:00"),
  draft: false,
  description: "About the platform",
  tags: [],
  summary: "Learn about the Open Sharia Enterprise Platform",
  weight: 0,
  isSection: false,
  filePath: "/mock/content/about.md",
  readingTime: 3,
  category: undefined,
};

export const mockDraftMeta: ContentMeta = {
  title: "Draft Post",
  slug: "updates/draft-post",
  date: new Date("2026-03-01T00:00:00+07:00"),
  draft: true,
  description: "This is a draft",
  tags: [],
  summary: "A draft post",
  weight: 0,
  isSection: false,
  filePath: "/mock/content/updates/draft-post.md",
  readingTime: 1,
  category: "updates",
};

export const mockSectionMeta: ContentMeta = {
  title: "Project Updates",
  slug: "updates",
  draft: false,
  tags: [],
  weight: 0,
  isSection: true,
  filePath: "/mock/content/updates/_index.md",
  readingTime: 1,
  category: undefined,
};

export const mockContentHtml = `<h2 id="getting-started">Getting Started</h2>
<p>This is a test page with <strong>bold</strong> and <code>code</code>.</p>`;

export const mockHeadings = [{ id: "getting-started", text: "Getting Started", level: 2 }];
