"use client";

import { useState, type ReactNode } from "react";
import { SearchContext } from "@/contexts/search/presentation/use-search";
import { SearchDialog } from "./search-dialog";

export function SearchProvider({ children }: { children: ReactNode }) {
  const [open, setOpen] = useState(false);
  return (
    <SearchContext.Provider value={{ open, setOpen }}>
      {children}
      <SearchDialog />
    </SearchContext.Provider>
  );
}
