import "server-only";
import { createCallerFactory, createTRPCContext } from "@/contexts/app-shell/application/trpc-init";
import { appRouter } from "@/contexts/app-shell/application/root-router";

const createCaller = createCallerFactory(appRouter);

export const serverCaller = createCaller(createTRPCContext());
