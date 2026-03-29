import { z } from "zod";

export const treeNodeSchema: z.ZodType<TreeNodeType> = z.lazy(() =>
  z.object({
    title: z.string(),
    slug: z.string(),
    weight: z.number(),
    isSection: z.boolean(),
    children: z.array(treeNodeSchema),
  }),
);

interface TreeNodeType {
  title: string;
  slug: string;
  weight: number;
  isSection: boolean;
  children: TreeNodeType[];
}

export const localeSchema = z.enum(["en", "id"]);

export type Locale = z.infer<typeof localeSchema>;
