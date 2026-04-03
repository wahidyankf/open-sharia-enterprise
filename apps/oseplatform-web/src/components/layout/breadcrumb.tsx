import Link from "next/link";
import { ChevronRight } from "lucide-react";

interface BreadcrumbProps {
  segments: { label: string; href: string }[];
}

export function Breadcrumb({ segments }: BreadcrumbProps) {
  // Exclude last segment — the current page title is already shown in the h1
  const ancestorSegments = segments.slice(0, -1);

  return (
    <nav aria-label="Breadcrumb" className="mb-4 text-sm text-muted-foreground">
      <ol className="flex flex-wrap items-center gap-1">
        <li className="flex items-center gap-1">
          <Link href="/" className="hover:text-foreground">
            Home
          </Link>
        </li>
        {ancestorSegments.map((segment) => (
          <li key={segment.href} className="flex items-center gap-1">
            <ChevronRight className="h-3 w-3 shrink-0" />
            <Link href={segment.href} className="hover:text-foreground">
              {segment.label}
            </Link>
          </li>
        ))}
      </ol>
    </nav>
  );
}
