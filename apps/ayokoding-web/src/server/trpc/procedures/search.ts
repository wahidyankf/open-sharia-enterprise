import { router, publicProcedure } from "../init";
import { searchQuerySchema } from "@/lib/schemas/search";
import { TRPCError } from "@trpc/server";

export const searchRouter = router({
  query: publicProcedure.input(searchQuerySchema).query(async ({ ctx, input }) => {
    if (input.query.trim().length === 0) {
      throw new TRPCError({ code: "BAD_REQUEST", message: "Query must not be empty" });
    }

    return ctx.contentService.search(input.locale, input.query, input.limit);
  }),
});
