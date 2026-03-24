import FlexSearch from "flexsearch";
import type { ContentMeta, SearchResult } from "./types";
import { readFileContent, stripMarkdown } from "./reader";

interface SearchDoc {
  id: string;
  title: string;
  content: string;
  slug: string;
  locale: string;
}

const indexes = new Map<string, FlexSearch.Document<SearchDoc, true>>();
const docStore = new Map<string, SearchDoc>();

export async function buildSearchIndex(items: ContentMeta[]): Promise<void> {
  const locales = [...new Set(items.map((i) => i.locale))];

  for (const locale of locales) {
    const index = new FlexSearch.Document<SearchDoc, true>({
      document: {
        id: "id",
        index: ["title", "content"],
        store: true,
      },
      tokenize: "full",
    });

    const localeItems = items.filter((i) => i.locale === locale && !i.isSection);

    for (const item of localeItems) {
      try {
        const { content } = await readFileContent(item.filePath);
        const plainText = stripMarkdown(content);
        const doc: SearchDoc = {
          id: `${locale}:${item.slug}`,
          title: item.title,
          content: plainText,
          slug: item.slug,
          locale,
        };
        index.add(doc);
        docStore.set(doc.id, doc);
      } catch {
        // Skip files that can't be read
      }
    }

    indexes.set(locale, index);
  }
}

export function searchContent(locale: string, query: string, limit: number = 20): SearchResult[] {
  const index = indexes.get(locale);
  if (!index) return [];

  const results = index.search(query, { limit, enrich: true });
  const seen = new Set<string>();
  const output: SearchResult[] = [];

  for (const field of results) {
    for (const result of field.result) {
      const id = String(
        typeof result === "object" && result !== null && "id" in result ? (result as { id: unknown }).id : result,
      );
      if (seen.has(id)) continue;
      seen.add(id);

      const doc = docStore.get(id);
      if (!doc) continue;

      const excerpt = createExcerpt(doc.content, query);
      output.push({
        title: doc.title,
        slug: doc.slug,
        excerpt,
        locale: doc.locale,
      });
    }
  }

  return output.slice(0, limit);
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

export function isSearchIndexReady(locale: string): boolean {
  return indexes.has(locale);
}
