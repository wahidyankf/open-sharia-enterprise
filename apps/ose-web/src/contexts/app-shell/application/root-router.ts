import { router } from "@/lib/trpc/init";
import { contentRouter } from "@/contexts/content/application/router";
import { searchRouter } from "@/contexts/search/application/router";
import { healthRouter } from "@/contexts/health/application/router";

export const appRouter = router({
  content: contentRouter,
  search: searchRouter,
  health: healthRouter,
});

export type AppRouter = typeof appRouter;
