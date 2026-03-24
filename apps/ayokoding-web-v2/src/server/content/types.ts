export interface ContentMeta {
  title: string;
  slug: string;
  locale: string;
  weight: number;
  date?: Date;
  description?: string;
  tags: string[];
  draft: boolean;
  isSection: boolean;
  filePath: string;
}

export interface ContentPage extends ContentMeta {
  html: string;
  headings: Heading[];
  prev: PageLink | null;
  next: PageLink | null;
}

export interface Heading {
  id: string;
  text: string;
  level: number;
}

export interface PageLink {
  title: string;
  slug: string;
}

export interface TreeNode {
  title: string;
  slug: string;
  weight: number;
  isSection: boolean;
  children: TreeNode[];
}

export interface SearchResult {
  title: string;
  slug: string;
  excerpt: string;
  locale: string;
}

export interface ContentIndex {
  contentMap: Map<string, ContentMeta>;
  trees: Record<string, TreeNode[]>;
  prevNext: Map<string, { prev: PageLink | null; next: PageLink | null }>;
}
