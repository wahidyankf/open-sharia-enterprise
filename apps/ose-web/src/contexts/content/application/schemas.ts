import { z } from "zod";

export const frontmatterSchema = z.object({
  title: z.string(),
  date: z.coerce.date().optional(),
  draft: z.boolean().default(false),
  weight: z.number().default(0),
  description: z.string().optional(),
  tags: z.array(z.string()).default([]),
  summary: z.string().optional(),
  categories: z.array(z.string()).default([]),
  showtoc: z.boolean().default(false),
  url: z.string().optional(),
});

export type Frontmatter = z.infer<typeof frontmatterSchema>;
