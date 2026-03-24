"use client";

import { useEffect, useState } from "react";
import { cn } from "@/lib/utils";
import type { Heading } from "@/server/content/types";

interface TocProps {
  headings: Heading[];
  label: string;
}

export function TableOfContents({ headings, label }: TocProps) {
  const [activeId, setActiveId] = useState<string>("");

  useEffect(() => {
    const observer = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          if (entry.isIntersecting) {
            setActiveId(entry.target.id);
          }
        }
      },
      { rootMargin: "-80px 0px -80% 0px" },
    );

    for (const heading of headings) {
      const element = document.getElementById(heading.id);
      if (element) observer.observe(element);
    }

    return () => observer.disconnect();
  }, [headings]);

  if (headings.length === 0) return null;

  return (
    <nav aria-label="Table of contents" className="text-sm">
      <h3 className="mb-3 font-semibold text-foreground">{label}</h3>
      <ul className="space-y-1">
        {headings.map((heading) => (
          <li key={heading.id} style={{ paddingLeft: `${(heading.level - 2) * 12}px` }}>
            <a
              href={`#${heading.id}`}
              className={cn(
                "block truncate py-1 text-muted-foreground transition-colors hover:text-foreground",
                activeId === heading.id && "font-medium text-primary",
              )}
            >
              {heading.text}
            </a>
          </li>
        ))}
      </ul>
    </nav>
  );
}
