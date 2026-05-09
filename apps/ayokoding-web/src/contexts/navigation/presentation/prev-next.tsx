import Link from "next/link";
import { ChevronLeft, ChevronRight } from "lucide-react";
import type { PageLink } from "@/contexts/content/infrastructure/types";
import type { Locale } from "@/contexts/i18n/application/config";
import { t } from "@/contexts/i18n/application/translations";

interface PrevNextProps {
  locale: string;
  prev: PageLink | null;
  next: PageLink | null;
}

export function PrevNext({ locale, prev, next }: PrevNextProps) {
  if (!prev && !next) return null;

  return (
    <nav
      aria-label="Page navigation"
      className="mt-12 flex flex-col gap-4 border-t border-border pt-6 sm:flex-row sm:justify-between"
    >
      {prev ? (
        <Link
          href={`/${locale}/${prev.slug}`}
          className="group flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground"
        >
          <ChevronLeft className="h-4 w-4" />
          <div>
            <div className="text-xs">{t(locale as Locale, "previous")}</div>
            <div className="font-medium text-foreground group-hover:text-primary">{prev.title}</div>
          </div>
        </Link>
      ) : (
        <div />
      )}
      {next ? (
        <Link
          href={`/${locale}/${next.slug}`}
          className="group flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground sm:text-right"
        >
          <div>
            <div className="text-xs">{t(locale as Locale, "next")}</div>
            <div className="font-medium text-foreground group-hover:text-primary">{next.title}</div>
          </div>
          <ChevronRight className="h-4 w-4" />
        </Link>
      ) : (
        <div />
      )}
    </nav>
  );
}
