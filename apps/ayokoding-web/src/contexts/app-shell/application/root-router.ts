import { router } from "./trpc-init";
import { contentProcedures } from "@/contexts/content/application/router";
import { navigationProcedures } from "@/contexts/navigation/application/router";
import { searchProcedures } from "@/contexts/search/application/router";
import { healthProcedures } from "@/contexts/health/application/router";
import { i18nProcedures } from "@/contexts/i18n/application/router";

export const appRouter = router({
  content: router({ ...contentProcedures, ...navigationProcedures }),
  search: router({ ...searchProcedures }),
  meta: router({ ...healthProcedures, ...i18nProcedures }),
});

export type AppRouter = typeof appRouter;
