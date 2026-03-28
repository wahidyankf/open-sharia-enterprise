/**
 * Generate search data JSON for FlexSearch indexing at build time.
 * Reads all non-draft content files and outputs generated/search-data.json.
 */

import path from "node:path";
import fs from "node:fs";
import matter from "gray-matter";
import { frontmatterSchema } from "../lib/schemas/content";
import { stripMarkdown } from "../server/content/reader";

const CONTENT_DIR = path.resolve(__dirname, "../../content");
const OUTPUT_DIR = path.resolve(__dirname, "../../generated");
const OUTPUT_FILE = path.join(OUTPUT_DIR, "search-data.json");

interface SearchDoc {
  id: string;
  title: string;
  content: string;
  slug: string;
}

function globMarkdownFiles(dir: string): string[] {
  const files: string[] = [];
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...globMarkdownFiles(fullPath));
    } else if (entry.name.endsWith(".md")) {
      files.push(fullPath);
    }
  }
  return files;
}

function deriveSlug(filePath: string): { slug: string; isSection: boolean } {
  const relative = path
    .relative(CONTENT_DIR, filePath)
    .split(path.sep)
    .join("/");
  const isSection = relative.endsWith("_index.md");
  let slug = relative.replace(/\.md$/, "");
  slug = slug.replace(/\/_index$/, "");
  if (slug === "_index") slug = "";
  return { slug, isSection };
}

const files = globMarkdownFiles(CONTENT_DIR);
const docs: SearchDoc[] = [];

for (const filePath of files) {
  const raw = fs.readFileSync(filePath, "utf-8");
  const { data, content } = matter(raw);
  const parsed = frontmatterSchema.safeParse(data);
  if (!parsed.success) continue;
  if (parsed.data.draft) continue;

  const { slug, isSection } = deriveSlug(filePath);
  if (isSection) continue;

  const plainText = stripMarkdown(content).slice(0, 2000);
  docs.push({ id: slug, title: parsed.data.title, content: plainText, slug });
}

fs.mkdirSync(OUTPUT_DIR, { recursive: true });
fs.writeFileSync(OUTPUT_FILE, JSON.stringify(docs, null, 2));
