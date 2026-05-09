import type { ContentService, SearchDoc } from "@/contexts/content/application/service";
import type { SearchResult } from "@/contexts/content/application/types";
import FlexSearch from "flexsearch";

function createExcerpt(content: string, query: string): string {
  const lowerContent = content.toLowerCase();
  const lowerQuery = query.toLowerCase();
  const idx = lowerContent.indexOf(lowerQuery);
  if (idx === -1) return content.slice(0, 150) + (content.length > 150 ? "..." : "");
  const start = Math.max(0, idx - 60);
  const end = Math.min(content.length, idx + query.length + 60);
  let excerpt = content.slice(start, end);
  if (start > 0) excerpt = "..." + excerpt;
  if (end < content.length) excerpt = excerpt + "...";
  return excerpt;
}

export class SearchService {
  private searchIndex: FlexSearch.Document<SearchDoc, true> | null = null;
  private docStore = new Map<string, SearchDoc>();

  constructor(
    private readonly contentService: ContentService,
    private readonly searchDataPath?: string,
  ) {}

  async search(query: string, limit = 20): Promise<SearchResult[]> {
    await this.ensureSearchIndex();
    if (!this.searchIndex) return [];

    const results = this.searchIndex.search(query, { limit, enrich: true });
    const seen = new Set<string>();
    const output: SearchResult[] = [];

    for (const field of results) {
      for (const result of field.result) {
        const id = String(
          typeof result === "object" && result !== null && "id" in result ? (result as { id: unknown }).id : result,
        );
        if (seen.has(id)) continue;
        seen.add(id);
        const doc = this.docStore.get(id);
        if (!doc) continue;
        output.push({ title: doc.title, slug: doc.slug, excerpt: createExcerpt(doc.content, query) });
      }
    }
    return output.slice(0, limit);
  }

  isSearchIndexReady(): boolean {
    return this.searchIndex !== null;
  }

  private async ensureSearchIndex(): Promise<void> {
    if (this.searchIndex) return;
    const preBuilt = this.searchDataPath ? await this.tryLoadPreBuilt() : null;
    if (preBuilt) {
      this.buildFromDocs(preBuilt);
    } else {
      await this.buildFromFiles();
    }
  }

  private async tryLoadPreBuilt(): Promise<SearchDoc[] | null> {
    try {
      const { readFile } = await import("node:fs/promises");
      const raw = await readFile(this.searchDataPath!, "utf-8");
      return JSON.parse(raw) as SearchDoc[];
    } catch {
      return null;
    }
  }

  private buildFromDocs(docs: SearchDoc[]): void {
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

  private async buildFromFiles(): Promise<void> {
    const { contentMap } = await this.contentService.getIndex();
    const items = [...contentMap.values()].filter((i) => !i.isSection);
    const index = new FlexSearch.Document<SearchDoc, true>({
      document: { id: "id", index: ["title", "content"], store: true },
      tokenize: "forward",
    });
    for (const item of items) {
      try {
        const { stripMarkdown } = await import("@/contexts/content/infrastructure/reader");
        const { readFileContent } = this.contentService as unknown as {
          readFileContent: (path: string) => Promise<{ content: string }>;
        };
        const data = readFileContent ? await readFileContent(item.filePath) : { content: "" };
        const plainText = stripMarkdown(data.content).slice(0, 2000);
        const doc: SearchDoc = { id: item.slug, title: item.title, content: plainText, slug: item.slug };
        index.add(doc);
        this.docStore.set(doc.id, doc);
      } catch {
        // skip unreadable files
      }
    }
    this.searchIndex = index;
  }
}
