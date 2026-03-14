import { Links, Meta, Outlet, Scripts, ScrollRestoration } from "react-router";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { useState } from "react";
import { AuthProvider } from "./lib/auth/auth-provider";

function makeQueryClient() {
  return new QueryClient({
    defaultOptions: {
      queries: { staleTime: 60 * 1000, retry: 1 },
    },
  });
}

let browserQueryClient: QueryClient | undefined;
function getQueryClient() {
  if (typeof window === "undefined") return makeQueryClient();
  if (!browserQueryClient) browserQueryClient = makeQueryClient();
  return browserQueryClient;
}

export default function Root() {
  const [queryClient] = useState(() => getQueryClient());

  return (
    <html lang="en">
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <Meta />
        <Links />
      </head>
      <body>
        <QueryClientProvider client={queryClient}>
          <AuthProvider>
            <Outlet />
          </AuthProvider>
        </QueryClientProvider>
        <ScrollRestoration />
        <Scripts />
      </body>
    </html>
  );
}
