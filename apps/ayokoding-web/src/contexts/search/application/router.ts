import { TRPCError } from "@trpc/server";
import { publicProcedure } from "@/contexts/app-shell/application/trpc-init";
import { searchQuerySchema } from "@/contexts/search/application/schemas";

export const searchProcedures = {
  query: publicProcedure.input(searchQuerySchema).query(async ({ ctx, input }) => {
    if (input.query.trim().length === 0) {
      throw new TRPCError({ code: "BAD_REQUEST", message: "Query must not be empty" });
    }

    return ctx.contentService.search(input.locale, input.query, input.limit);
  }),
};
