import { router, publicProcedure } from "../init";

export const metaRouter = router({
  health: publicProcedure.query(() => {
    return { status: "ok" as const };
  }),

  languages: publicProcedure.query(() => {
    return [
      { code: "en", label: "English" },
      { code: "id", label: "Bahasa Indonesia" },
    ];
  }),
});
