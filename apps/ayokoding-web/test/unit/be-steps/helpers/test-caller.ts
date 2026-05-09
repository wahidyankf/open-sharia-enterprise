import { createCallerFactory } from "@/contexts/app-shell/application/trpc-init";
import type { TRPCContext } from "@/contexts/app-shell/application/trpc-init";
import { appRouter } from "@/contexts/app-shell/application/root-router";
import { testContentService } from "./test-service";

const context: TRPCContext = { contentService: testContentService };

const createCaller = createCallerFactory(appRouter);
export const testCaller = createCaller(context);
