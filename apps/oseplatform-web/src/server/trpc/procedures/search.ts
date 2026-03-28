import { searchQuerySchema } from "@/lib/schemas/search";
import { router, publicProcedure } from "../init";

export const searchRouter = router({
  query: publicProcedure
    .input(searchQuerySchema)
    .query(async ({ ctx, input }) => {
      return ctx.contentService.search(input.query, input.limit);
    }),
});
