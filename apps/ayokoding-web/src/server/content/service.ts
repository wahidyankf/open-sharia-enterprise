import FlexSearch from "flexsearch";
import type { ContentRepository } from "./repository";
import type { ContentIndex, ContentMeta, TreeNode, PageLink, SearchResult, Heading } from "./types";
import { parseMarkdown } from "./parser";
import { stripMarkdown } from "./reader";

interface SearchDoc {
  id: string;
  title: string;
  content: string;
  slug: string;
  locale: string;
}

export class ContentService {
  private readonly repository: ContentRepository;
  private contentIndex: ContentIndex | null = null;
  private searchIndexes = new Map<string, FlexSearch.Document<SearchDoc, true>>();
  private docStore = new Map<string, SearchDoc>();

  constructor(repository: ContentRepository) {
    this.repository = repository;
  }

  async getIndex(): Promise<ContentIndex> {
    if (!this.contentIndex) {
      this.contentIndex = await this.buildContentIndex();
    }
    return this.contentIndex;
  }

  async getBySlug(
    locale: string,
    slug: string,
  ): Promise<
    | (ContentMeta & {
        html: string;
        headings: Heading[];
        prev: PageLink | null;
        next: PageLink | null;
      })
    | null
  > {
    const index = await this.getIndex();
    const meta = index.contentMap.get(`${locale}:${slug}`);
    if (!meta) return null;

    const { content } = await this.repository.readFileContent(meta.filePath);
    const { html, headings } = await parseMarkdown(content);
    const prevNext = index.prevNext.get(`${locale}:${slug}`);

    return {
      ...meta,
      html,
      headings,
      prev: prevNext?.prev ?? null,
      next: prevNext?.next ?? null,
    };
  }

  async listChildren(locale: string, parentSlug: string): Promise<ContentMeta[]> {
    const index = await this.getIndex();
    const children: ContentMeta[] = [];

    for (const [key, meta] of index.contentMap) {
      if (!key.startsWith(`${locale}:`)) continue;
      const parent = getParentSlug(meta.slug);
      if (parent === parentSlug && meta.slug !== parentSlug) {
        children.push(meta);
      }
    }

    return children.sort((a, b) => a.weight - b.weight);
  }

  async getTree(locale: string, rootSlug?: string): Promise<TreeNode[]> {
    const index = await this.getIndex();
    const tree = index.trees[locale] ?? [];

    if (rootSlug) {
      const subtree = findSubtree(tree, rootSlug);
      return subtree ? subtree.children : [];
    }

    return tree;
  }

  async search(locale: string, query: string, limit: number = 20): Promise<SearchResult[]> {
    await this.ensureSearchIndex(locale);

    const index = this.searchIndexes.get(locale);
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

        const doc = this.docStore.get(id);
        if (!doc) continue;

        output.push({
          title: doc.title,
          slug: doc.slug,
          excerpt: createExcerpt(doc.content, query),
          locale: doc.locale,
        });
      }
    }

    return output.slice(0, limit);
  }

  isSearchIndexReady(locale: string): boolean {
    return this.searchIndexes.has(locale);
  }

  private async ensureSearchIndex(locale: string): Promise<void> {
    if (this.searchIndexes.has(locale)) return;

    const contentIndex = await this.getIndex();
    const items = [...contentIndex.contentMap.values()];
    const locales = [...new Set(items.map((i) => i.locale))];

    for (const loc of locales) {
      if (this.searchIndexes.has(loc)) continue;

      const index = new FlexSearch.Document<SearchDoc, true>({
        document: {
          id: "id",
          index: ["title", "content"],
          store: true,
        },
        tokenize: "full",
      });

      const localeItems = items.filter((i) => i.locale === loc && !i.isSection);

      for (const item of localeItems) {
        try {
          const { content } = await this.repository.readFileContent(item.filePath);
          const plainText = stripMarkdown(content);
          const doc: SearchDoc = {
            id: `${loc}:${item.slug}`,
            title: item.title,
            content: plainText,
            slug: item.slug,
            locale: loc,
          };
          index.add(doc);
          this.docStore.set(doc.id, doc);
        } catch {
          // Skip files that can't be read
        }
      }

      this.searchIndexes.set(loc, index);
    }
  }

  private async buildContentIndex(): Promise<ContentIndex> {
    const allContent = await this.repository.readAllContent();
    const contentMap = new Map<string, ContentMeta>();

    for (const meta of allContent) {
      contentMap.set(`${meta.locale}:${meta.slug}`, meta);
    }

    const trees = buildTrees(allContent);
    const prevNext = computePrevNext(allContent);

    return { contentMap, trees, prevNext };
  }
}

function buildTrees(allContent: ContentMeta[]): Record<string, TreeNode[]> {
  const trees: Record<string, TreeNode[]> = {};
  const locales = [...new Set(allContent.map((c) => c.locale))];

  for (const locale of locales) {
    const items = allContent.filter((c) => c.locale === locale);
    trees[locale] = buildTreeForLocale(items);
  }

  return trees;
}

function buildTreeForLocale(items: ContentMeta[]): TreeNode[] {
  const nodeMap = new Map<string, TreeNode>();
  const roots: TreeNode[] = [];

  for (const item of items) {
    nodeMap.set(item.slug, {
      title: item.title,
      slug: item.slug,
      weight: item.weight,
      isSection: item.isSection,
      children: [],
    });
  }

  for (const item of items) {
    const parts = item.slug.split("/");
    for (let i = 1; i < parts.length; i++) {
      const ancestorSlug = parts.slice(0, i).join("/");
      if (!nodeMap.has(ancestorSlug)) {
        const lastPart = parts[i - 1] ?? ancestorSlug;
        nodeMap.set(ancestorSlug, {
          title: lastPart.charAt(0).toUpperCase() + lastPart.slice(1).replace(/-/g, " "),
          slug: ancestorSlug,
          weight: 0,
          isSection: true,
          children: [],
        });
      }
    }
  }

  for (const [, node] of nodeMap) {
    const parentSlug = getParentSlug(node.slug);
    if (parentSlug === null) {
      roots.push(node);
    } else {
      const parent = nodeMap.get(parentSlug);
      if (parent) {
        if (!parent.children.some((c) => c.slug === node.slug)) {
          parent.children.push(node);
        }
      } else {
        roots.push(node);
      }
    }
  }

  sortTreeByWeight(roots);
  return roots;
}

function getParentSlug(slug: string): string | null {
  if (slug === "") return null;
  const parts = slug.split("/");
  if (parts.length <= 1) return "";
  return parts.slice(0, -1).join("/");
}

function sortTreeByWeight(nodes: TreeNode[]): void {
  nodes.sort((a, b) => a.weight - b.weight);
  for (const node of nodes) {
    if (node.children.length > 0) {
      sortTreeByWeight(node.children);
    }
  }
}

function computePrevNext(allContent: ContentMeta[]): Map<string, { prev: PageLink | null; next: PageLink | null }> {
  const result = new Map<string, { prev: PageLink | null; next: PageLink | null }>();
  const locales = [...new Set(allContent.map((c) => c.locale))];

  for (const locale of locales) {
    const items = allContent.filter((c) => c.locale === locale);
    const groups = new Map<string, ContentMeta[]>();

    for (const item of items) {
      if (item.isSection) continue;
      const parent = getParentSlug(item.slug) ?? "";
      const group = groups.get(parent) ?? [];
      group.push(item);
      groups.set(parent, group);
    }

    for (const siblings of groups.values()) {
      siblings.sort((a, b) => a.weight - b.weight);
      for (let i = 0; i < siblings.length; i++) {
        const item = siblings[i];
        if (!item) continue;
        const key = `${locale}:${item.slug}`;
        const prev = i > 0 ? siblings[i - 1] : null;
        const next = i < siblings.length - 1 ? siblings[i + 1] : null;
        result.set(key, {
          prev: prev ? { title: prev.title, slug: prev.slug } : null,
          next: next ? { title: next.title, slug: next.slug } : null,
        });
      }
    }
  }

  return result;
}

function findSubtree(nodes: TreeNode[], slug: string): TreeNode | null {
  for (const node of nodes) {
    if (node.slug === slug) return node;
    const found = findSubtree(node.children, slug);
    if (found) return found;
  }
  return null;
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
