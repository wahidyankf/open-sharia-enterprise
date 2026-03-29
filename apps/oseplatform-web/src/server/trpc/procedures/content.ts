import { z } from "zod";
import { router, publicProcedure } from "../init";

export const contentRouter = router({
  getBySlug: publicProcedure.input(z.object({ slug: z.string() })).query(async ({ ctx, input }) => {
    return ctx.contentService.getBySlug(input.slug);
  }),

  listUpdates: publicProcedure.query(async ({ ctx }) => {
    return ctx.contentService.listUpdates();
  }),
});
