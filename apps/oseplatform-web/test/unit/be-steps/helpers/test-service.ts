import type { ContentMeta } from "@/contexts/content/application/types";
import { ContentService } from "@/contexts/content/application/service";
import { InMemoryContentRepository } from "@/contexts/content/infrastructure/repository-memory";
import { mockUpdateMetas, mockAboutMeta, mockSectionMeta, mockDraftMeta } from "./mock-content";

const allMetas: ContentMeta[] = [
  mockAboutMeta,
  mockSectionMeta,
  ...mockUpdateMetas,
  // Note: draft NOT included — draft filtering is tested separately
];

const repository = new InMemoryContentRepository(
  allMetas.map((meta) => ({
    meta,
    content: `## ${meta.title}\n\nTest content for ${meta.title}. This contains enterprise and compliance topics.`,
  })),
);

export const testContentService = new ContentService(repository);

// Service with draft included (for draft filtering test)
const allWithDraft: ContentMeta[] = [...allMetas, mockDraftMeta];
const repoWithDraft = new InMemoryContentRepository(
  allWithDraft.map((meta) => ({
    meta,
    content: `## ${meta.title}\n\nTest content for ${meta.title}.`,
  })),
);
export const testContentServiceWithDraft = new ContentService(repoWithDraft);

// Service with multiple "phase" pages for search limit test
const phaseMetas: ContentMeta[] = [1, 2, 3, 4, 5].map((n) => ({
  title: `Phase ${n} Update`,
  slug: `updates/phase-${n}`,
  date: new Date(`2026-0${n}-01T00:00:00Z`),
  draft: false,
  description: `Phase ${n} description`,
  tags: [],
  summary: `Phase ${n} summary`,
  weight: 0,
  isSection: false,
  filePath: `/mock/content/updates/phase-${n}.md`,
  readingTime: 2,
  category: "updates",
}));

const repoWithPhase = new InMemoryContentRepository(
  phaseMetas.map((meta) => ({
    meta,
    content: `## ${meta.title}\n\nThis phase update covers important milestones.`,
  })),
);
export const testContentServiceWithPhase = new ContentService(repoWithPhase);
