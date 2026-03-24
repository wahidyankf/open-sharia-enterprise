"use client";

import { createTRPCClient, httpBatchLink } from "@trpc/client";
import superjson from "superjson";
import type { AppRouter } from "@/server/trpc/router";

function getBaseUrl() {
  if (typeof window !== "undefined") return "";
  return `http://localhost:${process.env.PORT ?? 3101}`;
}

export const trpcClient = createTRPCClient<AppRouter>({
  links: [
    httpBatchLink({
      url: `${getBaseUrl()}/api/trpc`,
      transformer: superjson,
    }),
  ],
});
