import { fetchRequestHandler } from "@trpc/server/adapters/fetch";
import { appRouter } from "@/contexts/app-shell/application/root-router";
import { createTRPCContext } from "@/contexts/app-shell/application/trpc-init";

const handler = (req: Request) =>
  fetchRequestHandler({
    endpoint: "/api/trpc",
    req,
    router: appRouter,
    createContext: createTRPCContext,
  });

export { handler as GET, handler as POST };
