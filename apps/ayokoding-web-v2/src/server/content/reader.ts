import path from "node:path";
import fs from "node:fs/promises";
import matter from "gray-matter";
import { frontmatterSchema } from "@/lib/schemas/content";
import type { ContentMeta } from "./types";

const CONTENT_DIR = process.env.CONTENT_DIR ?? path.resolve(process.cwd(), "../../apps/ayokoding-web/content");

export function getContentDir(): string {
  return CONTENT_DIR;
}

export function deriveSlug(filePath: string, contentDir: string): { locale: string; slug: string; isSection: boolean } {
  const relative = path.relative(contentDir, filePath).split(path.sep).join("/");
  const parts = relative.split("/");
  const locale = parts[0] ?? "";
  const rest = parts.slice(1).join("/");

  const isSection = rest.endsWith("_index.md");
  let slug = rest.replace(/\.md$/, "");
  slug = slug.replace(/\/_index$/, "");
  if (slug === "_index") slug = "";

  return { locale, slug, isSection };
}

async function globMarkdownFiles(dir: string): Promise<string[]> {
  const files: string[] = [];
  const entries = await fs.readdir(dir, { withFileTypes: true });
  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      const subFiles = await globMarkdownFiles(fullPath);
      files.push(...subFiles);
    } else if (entry.name.endsWith(".md")) {
      files.push(fullPath);
    }
  }
  return files;
}

export async function readAllContent(contentDir?: string): Promise<ContentMeta[]> {
  const dir = contentDir ?? CONTENT_DIR;
  const files = await globMarkdownFiles(dir);
  const results: ContentMeta[] = [];

  for (const filePath of files) {
    try {
      const raw = await fs.readFile(filePath, "utf-8");
      const { data } = matter(raw);
      const parsed = frontmatterSchema.safeParse(data);

      if (!parsed.success) {
        console.warn(`[content] Invalid frontmatter in ${filePath}:`, parsed.error.issues);
        continue;
      }

      const frontmatter = parsed.data;
      if (frontmatter.draft && process.env.SHOW_DRAFTS !== "true") {
        continue;
      }

      const { locale, slug, isSection } = deriveSlug(filePath, dir);

      results.push({
        title: frontmatter.title,
        slug,
        locale,
        weight: frontmatter.weight,
        date: frontmatter.date,
        description: frontmatter.description,
        tags: frontmatter.tags,
        draft: frontmatter.draft,
        isSection,
        filePath,
      });
    } catch (err) {
      console.warn(`[content] Error reading ${filePath}:`, err);
    }
  }

  return results;
}

export async function readFileContent(
  filePath: string,
): Promise<{ content: string; frontmatter: Record<string, unknown> }> {
  const raw = await fs.readFile(filePath, "utf-8");
  const { content, data } = matter(raw);
  return { content, frontmatter: data as Record<string, unknown> };
}

export function stripMarkdown(content: string): string {
  return content
    .replace(/```[\s\S]*?```/g, "")
    .replace(/`[^`]*`/g, "")
    .replace(/#{1,6}\s/g, "")
    .replace(/\*\*([^*]+)\*\*/g, "$1")
    .replace(/\*([^*]+)\*/g, "$1")
    .replace(/\[([^\]]+)\]\([^)]+\)/g, "$1")
    .replace(/!\[([^\]]*)\]\([^)]+\)/g, "$1")
    .replace(/{{<[^>]*>}}/g, "")
    .replace(/{{% [^%]* %}}/g, "")
    .replace(/<[^>]+>/g, "")
    .replace(/\n{2,}/g, "\n")
    .trim();
}
