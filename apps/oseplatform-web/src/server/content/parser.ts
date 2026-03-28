import { unified } from "unified";
import remarkParse from "remark-parse";
import remarkGfm from "remark-gfm";
import remarkRehype from "remark-rehype";
import rehypeRaw from "rehype-raw";
import rehypePrettyCode from "rehype-pretty-code";
import rehypeSlug from "rehype-slug";
import rehypeAutolinkHeadings from "rehype-autolink-headings";
import rehypeStringify from "rehype-stringify";
import type { Heading } from "./types";

interface ParseResult {
  html: string;
  headings: Heading[];
}

export async function parseMarkdown(content: string): Promise<ParseResult> {
  const headings: Heading[] = [];

  const file = await unified()
    .use(remarkParse)
    .use(remarkGfm)
    .use(remarkRehype, { allowDangerousHtml: true })
    .use(rehypeRaw)
    .use(rehypePrettyCode, {
      theme: { dark: "github-dark", light: "github-light" },
      keepBackground: true,
    })
    .use(rehypeSlug)
    .use(rehypeAutolinkHeadings, { behavior: "wrap" })
    .use(() => (tree) => {
      extractHeadings(tree as HastNode, headings);
    })
    .use(rehypeStringify, { allowDangerousHtml: true })
    .process(content);

  return { html: String(file), headings };
}

interface HastNode {
  type: string;
  tagName?: string;
  properties?: Record<string, unknown>;
  children?: HastNode[];
  value?: string;
}

function extractHeadings(tree: HastNode, headings: Heading[]): void {
  if (!tree.children) return;

  for (const node of tree.children) {
    if (
      node.type === "element" &&
      node.tagName &&
      ["h2", "h3", "h4"].includes(node.tagName)
    ) {
      const id = (node.properties?.id as string) ?? "";
      const text = getTextContent(node);
      const level = parseInt(node.tagName.slice(1), 10);
      headings.push({ id, text, level });
    }
    if (node.children) {
      extractHeadings(node, headings);
    }
  }
}

function getTextContent(node: HastNode): string {
  if (node.type === "text") return node.value ?? "";
  if (!node.children) return "";
  return node.children.map(getTextContent).join("");
}
