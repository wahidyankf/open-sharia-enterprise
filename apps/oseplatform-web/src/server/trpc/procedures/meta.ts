import { router, publicProcedure } from "../init";

export const metaRouter = router({
  health: publicProcedure.query(() => {
    return { status: "ok" as const };
  }),
});
