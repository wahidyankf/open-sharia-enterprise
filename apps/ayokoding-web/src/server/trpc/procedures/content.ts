import { z } from "zod";
import { router, publicProcedure } from "../init";
import { localeSchema } from "@/lib/schemas/navigation";
import { getContentIndex, getContentMeta, listChildren as listChildrenFn } from "@/server/content/index";
import { readFileContent } from "@/server/content/reader";
import { parseMarkdown } from "@/server/content/parser";
import { TRPCError } from "@trpc/server";

export const contentRouter = router({
  getBySlug: publicProcedure
    .input(
      z.object({
        locale: localeSchema,
        slug: z.string(),
      }),
    )
    .query(async ({ input }) => {
      const index = await getContentIndex();
      const meta = getContentMeta(index, input.locale, input.slug);

      if (!meta) {
        throw new TRPCError({ code: "NOT_FOUND", message: "Page not found" });
      }

      const { content } = await readFileContent(meta.filePath);
      const { html, headings } = await parseMarkdown(content);
      const prevNext = index.prevNext.get(`${input.locale}:${input.slug}`);

      return {
        ...meta,
        html,
        headings,
        prev: prevNext?.prev ?? null,
        next: prevNext?.next ?? null,
      };
    }),

  listChildren: publicProcedure
    .input(
      z.object({
        locale: localeSchema,
        parentSlug: z.string(),
      }),
    )
    .query(async ({ input }) => {
      const index = await getContentIndex();
      return listChildrenFn(index, input.locale, input.parentSlug);
    }),

  getTree: publicProcedure
    .input(
      z.object({
        locale: localeSchema,
        rootSlug: z.string().optional(),
      }),
    )
    .query(async ({ input }) => {
      const index = await getContentIndex();
      const tree = index.trees[input.locale] ?? [];

      if (input.rootSlug) {
        const subtree = findSubtree(tree, input.rootSlug);
        return subtree ? subtree.children : [];
      }

      return tree;
    }),
});

function findSubtree(nodes: { slug: string; children: typeof nodes }[], slug: string): (typeof nodes)[number] | null {
  for (const node of nodes) {
    if (node.slug === slug) return node;
    const found = findSubtree(node.children, slug);
    if (found) return found;
  }
  return null;
}
