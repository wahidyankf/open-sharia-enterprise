import { createCallerFactory } from "@/lib/trpc/init";
import type { TRPCContext } from "@/lib/trpc/init";
import { appRouter } from "@/contexts/app-shell/application/root-router";
import { testContentService } from "./test-service";

const context: TRPCContext = { contentService: testContentService };

const createCaller = createCallerFactory(appRouter);
export const testCaller = createCaller(context);
