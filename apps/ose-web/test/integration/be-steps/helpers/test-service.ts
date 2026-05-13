import path from "node:path";
import { ContentService } from "@/contexts/content/application/service";
import { FileSystemContentRepository } from "@/contexts/content/infrastructure/repository-fs";

const contentDir = path.resolve(process.cwd(), "content");
const searchDataPath = path.resolve(process.cwd(), "generated/search-data.json");
const repository = new FileSystemContentRepository(contentDir);

export const integrationContentService = new ContentService(repository, searchDataPath);
