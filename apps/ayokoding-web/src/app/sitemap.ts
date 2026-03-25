import type { MetadataRoute } from "next";
import { createTRPCContext } from "@/server/trpc/init";

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const { contentService } = createTRPCContext();
  const index = await contentService.getIndex();
  const entries: MetadataRoute.Sitemap = [];

  for (const [, meta] of index.contentMap) {
    entries.push({
      url: `https://ayokoding.com/${meta.locale}/${meta.slug}`,
      lastModified: meta.date ?? new Date(),
      changeFrequency: "weekly",
      priority: meta.isSection ? 0.8 : 0.6,
    });
  }

  return entries;
}
