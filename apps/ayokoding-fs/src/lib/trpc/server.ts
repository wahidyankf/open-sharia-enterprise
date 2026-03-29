import "server-only";
import { createCallerFactory, createTRPCContext } from "@/server/trpc/init";
import { appRouter } from "@/server/trpc/router";

const createCaller = createCallerFactory(appRouter);

export const serverCaller = createCaller(createTRPCContext());
