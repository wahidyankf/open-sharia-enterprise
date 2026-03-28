import Link from "next/link";
import { ChevronLeft, ChevronRight } from "lucide-react";
import type { PageLink } from "@/server/content/types";

interface PrevNextProps {
  prev?: PageLink;
  next?: PageLink;
}

export function PrevNext({ prev, next }: PrevNextProps) {
  if (!prev && !next) return null;

  return (
    <nav
      aria-label="Page navigation"
      className="mt-12 flex flex-col gap-4 border-t border-border pt-6 sm:flex-row sm:justify-between"
    >
      {prev ? (
        <Link
          href={`/${prev.slug}/`}
          className="group flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground"
        >
          <ChevronLeft className="h-4 w-4" />
          <div>
            <div className="text-xs">Previous</div>
            <div className="font-medium text-foreground group-hover:text-primary">
              {prev.title}
            </div>
          </div>
        </Link>
      ) : (
        <div />
      )}
      {next ? (
        <Link
          href={`/${next.slug}/`}
          className="group flex items-center gap-2 text-sm text-muted-foreground hover:text-foreground sm:text-right"
        >
          <div>
            <div className="text-xs">Next</div>
            <div className="font-medium text-foreground group-hover:text-primary">
              {next.title}
            </div>
          </div>
          <ChevronRight className="h-4 w-4" />
        </Link>
      ) : (
        <div />
      )}
    </nav>
  );
}
