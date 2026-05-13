import { createCallerFactory } from "@/lib/trpc/init";
import type { TRPCContext } from "@/lib/trpc/init";
import { appRouter } from "@/contexts/app-shell/application/root-router";
import { integrationContentService } from "./test-service";

const context: TRPCContext = { contentService: integrationContentService };

const createCaller = createCallerFactory(appRouter);
export const integrationCaller = createCaller(context);
