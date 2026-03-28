import type { MetadataRoute } from "next";
import { serverCaller } from "@/lib/trpc/server";

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const updates = await serverCaller.content.listUpdates();
  const siteUrl = "https://oseplatform.com";

  const staticPages: MetadataRoute.Sitemap = [
    {
      url: siteUrl,
      lastModified: new Date(),
      changeFrequency: "weekly",
      priority: 1,
    },
    {
      url: `${siteUrl}/about/`,
      lastModified: new Date(),
      changeFrequency: "monthly",
      priority: 0.8,
    },
    {
      url: `${siteUrl}/updates/`,
      lastModified: new Date(),
      changeFrequency: "weekly",
      priority: 0.8,
    },
  ];

  const updatePages: MetadataRoute.Sitemap = updates.map((update) => ({
    url: `${siteUrl}/${update.slug}/`,
    lastModified: update.date ?? new Date(),
    changeFrequency: "monthly" as const,
    priority: 0.6,
  }));

  return [...staticPages, ...updatePages];
}
