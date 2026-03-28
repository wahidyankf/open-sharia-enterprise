import fs from "node:fs/promises";
import FlexSearch from "flexsearch";
import type { ContentRepository } from "./repository";
import type { ContentMeta, Heading, PageLink, SearchResult } from "./types";
import { parseMarkdown } from "./parser";
import { stripMarkdown } from "./reader";

export interface SearchDoc {
  id: string;
  title: string;
  content: string;
  slug: string;
}

export class ContentService {
  private readonly repository: ContentRepository;
  private readonly searchDataPath: string | null;
  private contentMap: Map<string, ContentMeta> | null = null;
  private searchIndex: FlexSearch.Document<SearchDoc, true> | null = null;
  private docStore = new Map<string, SearchDoc>();

  constructor(repository: ContentRepository, searchDataPath?: string) {
    this.repository = repository;
    this.searchDataPath = searchDataPath ?? null;
  }

  async getIndex(): Promise<{
    contentMap: Map<string, ContentMeta>;
    updates: ContentMeta[];
  }> {
    if (!this.contentMap) {
      const allContent = await this.repository.readAllContent();
      this.contentMap = new Map<string, ContentMeta>();
      for (const meta of allContent) {
        this.contentMap.set(meta.slug, meta);
      }
    }
    const updates = this.getUpdatesFromMap(this.contentMap);
    return { contentMap: this.contentMap, updates };
  }

  async getBySlug(
    slug: string,
  ): Promise<
    | (ContentMeta & {
        html: string;
        headings: Heading[];
        prev?: PageLink;
        next?: PageLink;
      })
    | null
  > {
    const { contentMap } = await this.getIndex();
    const meta = contentMap.get(slug);
    if (!meta) return null;

    const { content } = await this.repository.readFileContent(meta.filePath);
    const { html, headings } = await parseMarkdown(content);

    let prev: PageLink | undefined;
    let next: PageLink | undefined;

    if (meta.category === "updates" && !meta.isSection) {
      const updates = this.getUpdatesFromMap(contentMap);
      const idx = updates.findIndex((u) => u.slug === slug);
      if (idx > 0) {
        const p = updates[idx - 1];
        if (p) next = { title: p.title, slug: p.slug };
      }
      if (idx < updates.length - 1) {
        const n = updates[idx + 1];
        if (n) prev = { title: n.title, slug: n.slug };
      }
    }

    return { ...meta, html, headings, prev, next };
  }

  async listUpdates(): Promise<ContentMeta[]> {
    const { updates } = await this.getIndex();
    return updates;
  }

  async search(query: string, limit: number = 20): Promise<SearchResult[]> {
    await this.ensureSearchIndex();

    if (!this.searchIndex) return [];

    const results = this.searchIndex.search(query, { limit, enrich: true });
    const seen = new Set<string>();
    const output: SearchResult[] = [];

    for (const field of results) {
      for (const result of field.result) {
        const id = String(
          typeof result === "object" &&
            result !== null &&
            "id" in result
            ? (result as { id: unknown }).id
            : result,
        );
        if (seen.has(id)) continue;
        seen.add(id);

        const doc = this.docStore.get(id);
        if (!doc) continue;

        output.push({
          title: doc.title,
          slug: doc.slug,
          excerpt: createExcerpt(doc.content, query),
        });
      }
    }

    return output.slice(0, limit);
  }

  isSearchIndexReady(): boolean {
    return this.searchIndex !== null;
  }

  private getUpdatesFromMap(contentMap: Map<string, ContentMeta>): ContentMeta[] {
    const showDrafts = process.env["SHOW_DRAFTS"] === "true";
    return [...contentMap.values()]
      .filter((m) => m.category === "updates" && !m.isSection && (!m.draft || showDrafts))
      .sort((a, b) => {
        const dateA = a.date?.getTime() ?? 0;
        const dateB = b.date?.getTime() ?? 0;
        return dateB - dateA;
      });
  }

  private async ensureSearchIndex(): Promise<void> {
    if (this.searchIndex) return;

    const preBuiltDocs = await this.tryLoadPreBuiltSearchData();

    if (preBuiltDocs) {
      this.buildSearchIndexFromDocs(preBuiltDocs);
    } else {
      await this.buildSearchIndexFromFiles();
    }
  }

  private async tryLoadPreBuiltSearchData(): Promise<SearchDoc[] | null> {
    if (!this.searchDataPath) return null;
    try {
      const raw = await fs.readFile(this.searchDataPath, "utf-8");
      return JSON.parse(raw) as SearchDoc[];
    } catch {
      return null;
    }
  }

  private buildSearchIndexFromDocs(docs: SearchDoc[]): void {
    const index = new FlexSearch.Document<SearchDoc, true>({
      document: { id: "id", index: ["title", "content"], store: true },
      tokenize: "forward",
    });

    for (const doc of docs) {
      index.add(doc);
      this.docStore.set(doc.id, doc);
    }

    this.searchIndex = index;
  }

  private async buildSearchIndexFromFiles(): Promise<void> {
    const { contentMap } = await this.getIndex();
    const items = [...contentMap.values()].filter((i) => !i.isSection);

    const index = new FlexSearch.Document<SearchDoc, true>({
      document: { id: "id", index: ["title", "content"], store: true },
      tokenize: "forward",
    });

    for (const item of items) {
      try {
        const { content } = await this.repository.readFileContent(item.filePath);
        const plainText = stripMarkdown(content);
        const doc: SearchDoc = {
          id: item.slug,
          title: item.title,
          content: plainText.slice(0, 2000),
          slug: item.slug,
        };
        index.add(doc);
        this.docStore.set(doc.id, doc);
      } catch {
        // Skip files that can't be read
      }
    }

    this.searchIndex = index;
  }
}

function createExcerpt(content: string, query: string): string {
  const lowerContent = content.toLowerCase();
  const lowerQuery = query.toLowerCase();
  const idx = lowerContent.indexOf(lowerQuery);

  if (idx === -1) {
    return content.slice(0, 150) + (content.length > 150 ? "..." : "");
  }

  const start = Math.max(0, idx - 60);
  const end = Math.min(content.length, idx + query.length + 60);
  let excerpt = content.slice(start, end);

  if (start > 0) excerpt = "..." + excerpt;
  if (end < content.length) excerpt = excerpt + "...";

  return excerpt;
}
