import { createCallerFactory } from "@/server/trpc/init";
import type { TRPCContext } from "@/server/trpc/init";
import { appRouter } from "@/server/trpc/router";
import { integrationContentService } from "./test-service";

const context: TRPCContext = { contentService: integrationContentService };

const createCaller = createCallerFactory(appRouter);
export const integrationCaller = createCaller(context);
