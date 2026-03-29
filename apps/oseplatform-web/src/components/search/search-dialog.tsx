"use client";

import { useEffect, useState, useCallback } from "react";
import { useRouter } from "next/navigation";
import {
  CommandDialog,
  CommandEmpty,
  CommandGroup,
  CommandInput,
  CommandItem,
  CommandList,
} from "@/components/ui/command";
import { useSearchOpen } from "@/lib/hooks/use-search";
import { trpcClient } from "@/lib/trpc/client";
import type { SearchResult } from "@/server/content/types";

export function SearchDialog() {
  const router = useRouter();
  const { open, setOpen } = useSearchOpen();
  const [query, setQuery] = useState("");
  const [results, setResults] = useState<SearchResult[]>([]);

  useEffect(() => {
    function onKeyDown(e: KeyboardEvent) {
      if (e.key === "k" && (e.metaKey || e.ctrlKey)) {
        e.preventDefault();
        setOpen(!open);
      }
    }
    document.addEventListener("keydown", onKeyDown);
    return () => document.removeEventListener("keydown", onKeyDown);
  }, [open, setOpen]);

  useEffect(() => {
    if (!query || query.length < 2) {
      setResults([]);
      return;
    }

    const timer = setTimeout(async () => {
      try {
        const data = await trpcClient.search.query.query({ query, limit: 10 });
        setResults(data);
      } catch {
        setResults([]);
      }
    }, 200);

    return () => clearTimeout(timer);
  }, [query]);

  const handleSelect = useCallback(
    (slug: string) => {
      setOpen(false);
      setQuery("");
      setResults([]);
      router.push(`/${slug}/`);
    },
    [router, setOpen],
  );

  return (
    <CommandDialog open={open} onOpenChange={setOpen}>
      <CommandInput placeholder="Search pages..." value={query} onValueChange={setQuery} />
      <CommandList>
        <CommandEmpty>{query.length < 2 ? "Type to search..." : "No results found."}</CommandEmpty>
        {results.length > 0 && (
          <CommandGroup heading="Results">
            {results.map((result) => (
              <CommandItem key={result.slug} value={result.slug} onSelect={() => handleSelect(result.slug)}>
                <div>
                  <div className="font-medium">{result.title}</div>
                  <div className="line-clamp-1 text-xs text-muted-foreground">{result.excerpt}</div>
                </div>
              </CommandItem>
            ))}
          </CommandGroup>
        )}
      </CommandList>
    </CommandDialog>
  );
}
