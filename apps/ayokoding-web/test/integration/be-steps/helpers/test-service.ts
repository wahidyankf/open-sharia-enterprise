import { ContentService } from "@/contexts/content/infrastructure/service";
import { FileSystemContentRepository } from "@/contexts/content/infrastructure/repository-fs";
import path from "node:path";

const contentDir = path.resolve(process.cwd(), "content");
const repository = new FileSystemContentRepository(contentDir);

export const testContentService = new ContentService(repository);
