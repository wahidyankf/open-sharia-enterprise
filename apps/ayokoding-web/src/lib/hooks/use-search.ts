"use client";

import { createContext, useContext } from "react";

interface SearchState {
  open: boolean;
  setOpen: (open: boolean) => void;
}

export const SearchContext = createContext<SearchState>({
  open: false,
  setOpen: () => {},
});

export function useSearchOpen() {
  return useContext(SearchContext);
}
