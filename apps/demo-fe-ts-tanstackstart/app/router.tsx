import { createRouter as createTanStackRouter } from "@tanstack/react-router";
import { routeTree } from "./routeTree.gen";

export function createRouter() {
  return createTanStackRouter({ routeTree });
}

let router: ReturnType<typeof createRouter> | undefined;

export function getRouter() {
  if (!router) {
    router = createRouter();
  }
  return router;
}

declare module "@tanstack/react-router" {
  interface Register {
    router: ReturnType<typeof createRouter>;
  }
}
