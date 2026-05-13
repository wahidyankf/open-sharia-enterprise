import { fetchRequestHandler } from "@trpc/server/adapters/fetch";
import { appRouter } from "@/contexts/app-shell/application/root-router";
import { createTRPCContext } from "@/lib/trpc/init";

function handler(req: Request) {
  return fetchRequestHandler({
    endpoint: "/api/trpc",
    req,
    router: appRouter,
    createContext: () => createTRPCContext(),
  });
}

export { handler as GET, handler as POST };
