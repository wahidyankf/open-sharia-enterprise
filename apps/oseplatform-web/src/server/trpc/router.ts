import { router } from "./init";
import { contentRouter } from "./procedures/content";
import { searchRouter } from "./procedures/search";
import { metaRouter } from "./procedures/meta";

export const appRouter = router({
  content: contentRouter,
  search: searchRouter,
  meta: metaRouter,
});

export type AppRouter = typeof appRouter;
