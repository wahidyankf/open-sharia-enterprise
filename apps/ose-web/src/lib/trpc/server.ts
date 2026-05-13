import "server-only";
import { createCallerFactory, createTRPCContext } from "@/lib/trpc/init";
import { appRouter } from "@/contexts/app-shell/application/root-router";

const createCaller = createCallerFactory(appRouter);
export const serverCaller = createCaller(createTRPCContext());
