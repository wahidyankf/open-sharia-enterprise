import fs from "node:fs/promises";
import matter from "gray-matter";
import { FileSystemContentRepository } from "./repository-fs";
import { buildTrees } from "./tree-builder";
import type { TreeNode } from "./types";

export function generateChildList(locale: string, children: TreeNode[], knownSlugs: Set<string>): string {
  const lines: string[] = [];

  for (const child of children) {
    if (!knownSlugs.has(`${locale}:${child.slug}`)) continue;
    lines.push(`- [${child.title}](/${locale}/${child.slug})`);
    for (const grandchild of child.children) {
      if (!knownSlugs.has(`${locale}:${grandchild.slug}`)) continue;
      lines.push(`  - [${grandchild.title}](/${locale}/${grandchild.slug})`);
    }
  }

  return lines.join("\n");
}

export function extractRawFrontmatter(rawContent: string): string {
  const match = rawContent.match(/^---\n([\s\S]*?)\n---/);
  return match ? (match[1] ?? "") : "";
}

export function rebuildIndexFile(rawContent: string, newChildList: string): string {
  const rawFm = extractRawFrontmatter(rawContent);
  const frontmatterBlock = `---\n${rawFm}\n---`;

  if (newChildList.length === 0) {
    return frontmatterBlock + "\n";
  }
  return frontmatterBlock + "\n\n" + newChildList + "\n";
}

export function ensureFrontmatterFields(rawContent: string): string {
  const { data } = matter(rawContent);
  const rawFm = extractRawFrontmatter(rawContent);
  const lines: string[] = [];

  if (data.date === undefined) {
    lines.push(`date: ${new Date().toISOString()}`);
  }

  if (data.draft === undefined) {
    lines.push("draft: false");
  }

  if (lines.length === 0) return rawContent;

  const body = rawContent.slice(rawContent.indexOf("---", 3) + 3).replace(/^\n+/, "");
  const newFm = rawFm + "\n" + lines.join("\n");
  if (body.length === 0) {
    return `---\n${newFm}\n---\n`;
  }
  return `---\n${newFm}\n---\n\n${body}`;
}

export interface ProcessResult {
  changed: string[];
  errors: string[];
}

export async function processAllIndexFiles(contentDir: string, mode: "generate" | "validate"): Promise<ProcessResult> {
  const repository = new FileSystemContentRepository(contentDir);
  const allContent = await repository.readAllContent();
  const trees = buildTrees(allContent);

  const knownSlugs = new Set(allContent.map((c) => `${c.locale}:${c.slug}`));
  const sectionFiles = allContent.filter((c) => c.isSection);
  const changed: string[] = [];
  const errors: string[] = [];

  for (const section of sectionFiles) {
    try {
      const localeTree = trees[section.locale];
      if (!localeTree) continue;

      const children = section.slug === "" ? localeTree : (findNodeBySlug(localeTree, section.slug)?.children ?? null);
      if (!children) continue;

      const rawContent = await fs.readFile(section.filePath, "utf-8");
      const withFields = ensureFrontmatterFields(rawContent);
      const childList = generateChildList(section.locale, children, knownSlugs);
      const expected = rebuildIndexFile(withFields, childList);

      if (rawContent !== expected) {
        changed.push(section.filePath);
        if (mode === "generate") {
          await fs.writeFile(section.filePath, expected, "utf-8");
        }
      }
    } catch (err) {
      errors.push(`${section.filePath}: ${err instanceof Error ? err.message : String(err)}`);
    }
  }

  return { changed, errors };
}

function findNodeBySlug(nodes: TreeNode[], slug: string): TreeNode | null {
  for (const node of nodes) {
    if (node.slug === slug) return node;
    const found = findNodeBySlug(node.children, slug);
    if (found) return found;
  }
  return null;
}
