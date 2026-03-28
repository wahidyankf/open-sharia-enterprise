import Link from "next/link";
import { ChevronRight } from "lucide-react";

interface BreadcrumbProps {
  segments: { label: string; href: string }[];
}

export function Breadcrumb({ segments }: BreadcrumbProps) {
  if (segments.length === 0) return null;

  return (
    <nav aria-label="Breadcrumb" className="mb-4 text-sm text-muted-foreground">
      <ol className="flex flex-wrap items-center gap-1">
        <li className="flex items-center gap-1">
          <Link href="/" className="hover:text-foreground">
            Home
          </Link>
        </li>
        {segments.map((segment, i) => (
          <li key={segment.href} className="flex items-center gap-1">
            <ChevronRight className="h-3 w-3 shrink-0" />
            {i < segments.length - 1 ? (
              <Link
                href={segment.href}
                className="truncate hover:text-foreground"
              >
                {segment.label}
              </Link>
            ) : (
              <span className="truncate font-medium text-foreground">
                {segment.label}
              </span>
            )}
          </li>
        ))}
      </ol>
    </nav>
  );
}
