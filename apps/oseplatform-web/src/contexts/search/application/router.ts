import { searchQuerySchema } from "@/contexts/search/application/schemas";
import { router, publicProcedure } from "@/lib/trpc/init";

export const searchRouter = router({
  query: publicProcedure.input(searchQuerySchema).query(async ({ ctx, input }) => {
    return ctx.contentService.search(input.query, input.limit);
  }),
});
