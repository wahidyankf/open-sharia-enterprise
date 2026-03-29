import { z } from "zod";

export const searchQuerySchema = z.object({
  query: z.string().min(1, "Query must not be empty"),
  locale: z.enum(["en", "id"]),
  limit: z.number().min(1).max(50).default(20),
});

export const searchResultSchema = z.object({
  title: z.string(),
  slug: z.string(),
  excerpt: z.string(),
  locale: z.string(),
});

export type SearchQuery = z.infer<typeof searchQuerySchema>;
export type SearchResultItem = z.infer<typeof searchResultSchema>;
