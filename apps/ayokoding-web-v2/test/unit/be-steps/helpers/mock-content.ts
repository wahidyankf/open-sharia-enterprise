import type { ContentMeta, TreeNode } from "@/server/content/types";

export const mockContentMeta: ContentMeta[] = [
  {
    title: "English Content",
    slug: "",
    locale: "en",
    weight: 1,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/mock/content/en/_index.md",
  },
  {
    title: "Learn",
    slug: "learn",
    locale: "en",
    weight: 10,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/mock/content/en/learn/_index.md",
  },
  {
    title: "Overview",
    slug: "learn/overview",
    locale: "en",
    weight: 100000,
    description: "Complete learning path overview",
    draft: false,
    isSection: false,
    tags: ["learning"],
    filePath: "/mock/content/en/learn/overview.md",
  },
  {
    title: "Software Engineering",
    slug: "learn/software-engineering",
    locale: "en",
    weight: 200,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/mock/content/en/learn/software-engineering/_index.md",
  },
  {
    title: "Programming Languages",
    slug: "learn/software-engineering/programming-languages",
    locale: "en",
    weight: 100,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/mock/content/en/learn/software-engineering/programming-languages/_index.md",
  },
  {
    title: "Draft Page",
    slug: "learn/draft-page",
    locale: "en",
    weight: 999,
    draft: true,
    isSection: false,
    tags: [],
    filePath: "/mock/content/en/learn/draft-page.md",
  },
  {
    title: "Belajar",
    slug: "belajar",
    locale: "id",
    weight: 10,
    draft: false,
    isSection: true,
    tags: [],
    filePath: "/mock/content/id/belajar/_index.md",
  },
  {
    title: "Ikhtisar",
    slug: "belajar/ikhtisar",
    locale: "id",
    weight: 100000,
    description: "Ikhtisar jalur pembelajaran",
    draft: false,
    isSection: false,
    tags: [],
    filePath: "/mock/content/id/belajar/ikhtisar.md",
  },
];

export const mockEnTree: TreeNode[] = [
  {
    title: "Learn",
    slug: "learn",
    weight: 10,
    isSection: true,
    children: [
      {
        title: "Overview",
        slug: "learn/overview",
        weight: 100000,
        isSection: false,
        children: [],
      },
      {
        title: "Software Engineering",
        slug: "learn/software-engineering",
        weight: 200,
        isSection: true,
        children: [
          {
            title: "Programming Languages",
            slug: "learn/software-engineering/programming-languages",
            weight: 100,
            isSection: true,
            children: [],
          },
        ],
      },
    ],
  },
];

export const mockIdTree: TreeNode[] = [
  {
    title: "Belajar",
    slug: "belajar",
    weight: 10,
    isSection: true,
    children: [
      {
        title: "Ikhtisar",
        slug: "belajar/ikhtisar",
        weight: 100000,
        isSection: false,
        children: [],
      },
    ],
  },
];

export const mockContentHtml = `<h2 id="getting-started">Getting Started</h2>
<p>This is a test page with <strong>bold</strong> and <code>code</code>.</p>
<pre><code class="language-go">package main

func main() {
    fmt.Println("Hello, World!")
}
</code></pre>`;

export const mockHeadings = [{ id: "getting-started", text: "Getting Started", level: 2 }];
