export interface ContentMeta {
  title: string;
  slug: string;
  date?: Date;
  draft: boolean;
  description?: string;
  tags: string[];
  summary?: string;
  weight: number;
  isSection: boolean;
  filePath: string;
  readingTime: number;
  category?: string;
}

export interface ContentPage extends ContentMeta {
  html: string;
  headings: Heading[];
  prev?: PageLink;
  next?: PageLink;
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

export interface SearchResult {
  title: string;
  slug: string;
  excerpt: string;
}
