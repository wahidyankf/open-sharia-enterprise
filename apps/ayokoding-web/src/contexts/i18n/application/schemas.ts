import { z } from "zod";

export const localeSchema = z.enum(["en", "id"]);

export type Locale = z.infer<typeof localeSchema>;
