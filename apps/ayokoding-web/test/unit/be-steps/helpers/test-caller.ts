import { createCallerFactory } from "@/server/trpc/init";
import type { TRPCContext } from "@/server/trpc/init";
import { appRouter } from "@/server/trpc/router";
import { testContentService } from "./test-service";

const context: TRPCContext = { contentService: testContentService };

const createCaller = createCallerFactory(appRouter);
export const testCaller = createCaller(context);
