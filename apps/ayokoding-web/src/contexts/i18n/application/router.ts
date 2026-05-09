import { publicProcedure } from "@/contexts/app-shell/application/trpc-init";

export const i18nProcedures = {
  languages: publicProcedure.query(() => {
    return [
      { code: "en", label: "English" },
      { code: "id", label: "Bahasa Indonesia" },
    ];
  }),
};
