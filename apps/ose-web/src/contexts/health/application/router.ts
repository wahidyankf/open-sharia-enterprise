import { router, publicProcedure } from "@/lib/trpc/init";

export const healthRouter = router({
  check: publicProcedure.query(() => {
    return { status: "ok" as const };
  }),
});
