import { z } from "zod";

export const frontmatterSchema = z.object({
  title: z.string(),
  date: z.coerce.date().optional(),
  draft: z.boolean().default(false),
  weight: z.number().default(0),
  description: z.string().optional(),
  tags: z.array(z.string()).default([]),
  layout: z.string().optional(),
  type: z.string().optional(),
  cascade: z.record(z.unknown()).optional(),
  breadcrumbs: z.boolean().optional(),
  bookCollapseSection: z.boolean().optional(),
  bookFlatSection: z.boolean().optional(),
});

export type Frontmatter = z.infer<typeof frontmatterSchema>;
