import { z } from "zod";

export const searchQuerySchema = z.object({
  query: z.string().min(1, "Query must not be empty"),
  limit: z.number().min(1).max(50).default(20),
});

export const searchResultSchema = z.object({
  title: z.string(),
  slug: z.string(),
  excerpt: z.string(),
});

export type SearchQuery = z.infer<typeof searchQuerySchema>;
export type SearchResultItem = z.infer<typeof searchResultSchema>;
