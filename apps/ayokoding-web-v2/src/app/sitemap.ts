import type { MetadataRoute } from "next";
import { getContentIndex } from "@/server/content/index";

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const index = await getContentIndex();
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
