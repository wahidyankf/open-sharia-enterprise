import type { MetadataRoute } from "next";
import { serverCaller } from "@/lib/trpc/server";

const SITE_URL = "https://oseplatform.com";

export async function buildSitemap(): Promise<MetadataRoute.Sitemap> {
  const updates = await serverCaller.content.listUpdates();

  const staticPages: MetadataRoute.Sitemap = [
    { url: SITE_URL, lastModified: new Date(), changeFrequency: "weekly", priority: 1 },
    { url: `${SITE_URL}/about/`, lastModified: new Date(), changeFrequency: "monthly", priority: 0.8 },
    { url: `${SITE_URL}/updates/`, lastModified: new Date(), changeFrequency: "weekly", priority: 0.8 },
  ];

  const updatePages: MetadataRoute.Sitemap = updates.map((update) => ({
    url: `${SITE_URL}/${update.slug}/`,
    lastModified: update.date ?? new Date(),
    changeFrequency: "monthly" as const,
    priority: 0.6,
  }));

  return [...staticPages, ...updatePages];
}
