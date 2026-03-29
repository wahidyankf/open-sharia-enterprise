import type { ContentMeta } from "./types";
import type { ContentRepository } from "./repository";

export class InMemoryContentRepository implements ContentRepository {
  private pages: Map<string, { meta: ContentMeta; content: string }>;

  constructor(pages: { meta: ContentMeta; content: string }[]) {
    this.pages = new Map(pages.map((p) => [p.meta.filePath, p]));
  }

  async readAllContent(): Promise<ContentMeta[]> {
    return [...this.pages.values()].map((p) => p.meta);
  }

  async readFileContent(filePath: string): Promise<{ content: string; frontmatter: Record<string, unknown> }> {
    const page = this.pages.get(filePath);
    if (!page) throw new Error(`File not found: ${filePath}`);
    return { content: page.content, frontmatter: {} };
  }
}
