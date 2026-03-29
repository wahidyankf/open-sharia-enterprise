import { initTRPC } from "@trpc/server";
import superjson from "superjson";
import path from "node:path";
import { ContentService } from "@/server/content/service";
import { FileSystemContentRepository } from "@/server/content/repository-fs";

export interface TRPCContext {
  contentService: ContentService;
}

const t = initTRPC.context<TRPCContext>().create({
  transformer: superjson,
});

export const router = t.router;
export const publicProcedure = t.procedure;
export const createCallerFactory = t.createCallerFactory;

const defaultRepository = new FileSystemContentRepository();
const searchDataPath = path.resolve(process.cwd(), "generated/search-data.json");
const defaultContentService = new ContentService(defaultRepository, searchDataPath);

export function createTRPCContext(contentService?: ContentService): TRPCContext {
  return { contentService: contentService ?? defaultContentService };
}
