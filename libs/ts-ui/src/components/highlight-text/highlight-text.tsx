import React from "react";

interface HighlightTextProps {
  text: string;
  searchTerm: string;
  highlightClassName?: string;
}

export const highlightText = (
  text: string,
  searchTerm: string,
  highlightClassName = "bg-yellow-300 text-gray-900",
): React.ReactNode => {
  if (!searchTerm) return text;
  const regex = new RegExp(`(${searchTerm})`, "gi");
  return text.split(regex).map((part, index) =>
    regex.test(part) ? (
      <mark key={index} className={highlightClassName}>
        {part}
      </mark>
    ) : (
      part
    ),
  );
};

export const HighlightText: React.FC<HighlightTextProps> = ({ text, searchTerm, highlightClassName }) => {
  return <>{highlightText(text, searchTerm, highlightClassName)}</>;
};
