import path from "node:path";
import { ContentService } from "@/server/content/service";
import { FileSystemContentRepository } from "@/server/content/repository-fs";

const contentDir = path.resolve(process.cwd(), "content");
const searchDataPath = path.resolve(process.cwd(), "generated/search-data.json");
const repository = new FileSystemContentRepository(contentDir);

export const integrationContentService = new ContentService(repository, searchDataPath);
