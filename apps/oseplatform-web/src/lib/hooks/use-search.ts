"use client";
import { createContext, useContext } from "react";

interface SearchContextType {
  open: boolean;
  setOpen: (open: boolean) => void;
}

export const SearchContext = createContext<SearchContextType>({
  open: false,
  setOpen: () => {},
});

export function useSearchOpen() {
  return useContext(SearchContext);
}
