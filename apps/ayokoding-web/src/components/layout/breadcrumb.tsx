import Link from "next/link";
import { ChevronRight } from "lucide-react";

interface BreadcrumbProps {
  locale: string;
  slug: string;
  segments: { label: string; slug: string }[];
}

export function Breadcrumb({ locale, segments }: BreadcrumbProps) {
  if (segments.length === 0) return null;

  return (
    <nav aria-label="Breadcrumb" className="mb-4 text-sm text-muted-foreground">
      <ol className="flex flex-wrap items-center gap-1">
        {segments.map((segment, i) => (
          <li key={segment.slug} className="flex items-center gap-1">
            {i > 0 && <ChevronRight className="h-3 w-3 shrink-0" />}
            {i < segments.length - 1 ? (
              <Link href={`/${locale}/${segment.slug}`} className="truncate hover:text-foreground">
                {segment.label}
              </Link>
            ) : (
              <span className="truncate font-medium text-foreground">{segment.label}</span>
            )}
          </li>
        ))}
      </ol>
    </nav>
  );
}
