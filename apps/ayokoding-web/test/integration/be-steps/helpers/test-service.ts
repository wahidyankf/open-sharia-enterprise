import { ContentService } from "@/server/content/service";
import { FileSystemContentRepository } from "@/server/content/repository-fs";
import path from "node:path";

const contentDir = path.resolve(process.cwd(), "content");
const repository = new FileSystemContentRepository(contentDir);

export const testContentService = new ContentService(repository);
