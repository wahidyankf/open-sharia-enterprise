import { router, publicProcedure } from "../init";
import { searchQuerySchema } from "@/lib/schemas/search";
import { searchContent, isSearchIndexReady, buildSearchIndex } from "@/server/content/search-index";
import { getContentIndex } from "@/server/content/index";
import { TRPCError } from "@trpc/server";

export const searchRouter = router({
  query: publicProcedure.input(searchQuerySchema).query(async ({ input }) => {
    // Ensure search index is built
    if (!isSearchIndexReady(input.locale)) {
      const index = await getContentIndex();
      await buildSearchIndex([...index.contentMap.values()]);
    }

    if (input.query.trim().length === 0) {
      throw new TRPCError({ code: "BAD_REQUEST", message: "Query must not be empty" });
    }

    return searchContent(input.locale, input.query, input.limit);
  }),
});
