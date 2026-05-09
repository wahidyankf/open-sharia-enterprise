import { publicProcedure } from "@/contexts/app-shell/application/trpc-init";

export const healthProcedures = {
  health: publicProcedure.query(() => {
    return { status: "ok" as const };
  }),
};
