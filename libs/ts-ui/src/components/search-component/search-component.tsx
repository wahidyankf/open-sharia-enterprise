import React from "react";
import { Search, X } from "lucide-react";

interface SearchComponentProps {
  searchTerm: string;
  setSearchTerm: (term: string) => void;
  updateURL: (term: string) => void;
  placeholder: string;
  className?: string;
  inputClassName?: string;
  clearButtonClassName?: string;
}

export const SearchComponent: React.FC<SearchComponentProps> = ({
  searchTerm,
  setSearchTerm,
  updateURL,
  placeholder,
  className = "relative mb-8",
  inputClassName = "w-full border border-green-400 bg-gray-800 px-4 py-2 pr-12 pl-10 text-green-400 focus:ring-2 focus:ring-green-400 focus:outline-none",
  clearButtonClassName = "absolute top-1/2 right-3 -translate-y-1/2 transform rounded-full bg-yellow-400 p-1 text-gray-900 transition-colors duration-200 hover:bg-yellow-300",
}) => {
  const handleSearchChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const newTerm = e.target.value;
    setSearchTerm(newTerm);
    updateURL(newTerm);
  };

  const clearSearch = () => {
    setSearchTerm("");
    updateURL("");
  };

  return (
    <div className={className}>
      <input
        type="text"
        placeholder={placeholder}
        value={searchTerm}
        onChange={handleSearchChange}
        className={inputClassName}
      />
      <Search className="absolute top-1/2 left-3 -translate-y-1/2 transform text-green-400" />
      {searchTerm && (
        <button onClick={clearSearch} className={clearButtonClassName} aria-label="Clear search">
          <X className="h-4 w-4" />
        </button>
      )}
    </div>
  );
};
