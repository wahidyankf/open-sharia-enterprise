import { z } from "zod";
import { publicProcedure } from "@/contexts/app-shell/application/trpc-init";
import { localeSchema } from "@/contexts/i18n/application/schemas";

export const navigationProcedures = {
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
};
