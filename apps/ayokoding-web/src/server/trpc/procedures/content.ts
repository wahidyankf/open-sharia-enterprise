import { z } from "zod";
import { router, publicProcedure } from "../init";
import { localeSchema } from "@/lib/schemas/navigation";
import { TRPCError } from "@trpc/server";

export const contentRouter = router({
  getBySlug: publicProcedure
    .input(
      z.object({
        locale: localeSchema,
        slug: z.string(),
      }),
    )
    .query(async ({ ctx, input }) => {
      const result = await ctx.contentService.getBySlug(input.locale, input.slug);

      if (!result) {
        throw new TRPCError({ code: "NOT_FOUND", message: "Page not found" });
      }

      return result;
    }),

  listChildren: publicProcedure
    .input(
      z.object({
        locale: localeSchema,
        parentSlug: z.string(),
      }),
    )
    .query(async ({ ctx, input }) => {
      return ctx.contentService.listChildren(input.locale, input.parentSlug);
    }),

  getTree: publicProcedure
    .input(
      z.object({
        locale: localeSchema,
        rootSlug: z.string().optional(),
      }),
    )
    .query(async ({ ctx, input }) => {
      return ctx.contentService.getTree(input.locale, input.rootSlug);
    }),
});
