"use client";

import { useParams } from "next/navigation";
import type { Locale } from "@/contexts/i18n/application/config";

export function useLocale(): Locale {
  const params = useParams();
  return (params.locale as Locale) ?? "en";
}
