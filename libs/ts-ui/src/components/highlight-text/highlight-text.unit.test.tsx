import React from "react";
import { describe, it, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { HighlightText, highlightText } from "./highlight-text";

describe("HighlightText", () => {
  it("renders text without highlighting when no search term is provided", () => {
    render(<HighlightText text="Hello, world!" searchTerm="" />);
    expect(screen.getByText("Hello, world!")).toBeDefined();
    expect(screen.queryByRole("mark")).toBeNull();
  });

  it("highlights matching text when search term is provided", () => {
    render(<HighlightText text="Hello, world!" searchTerm="world" />);
    const highlightedText = screen.getByText((content, element) => {
      return element?.tagName.toLowerCase() === "mark" && content === "world";
    });
    expect(highlightedText).toBeDefined();
    expect(Array.from(highlightedText.classList)).toContain("bg-yellow-300");
    expect(Array.from(highlightedText.classList)).toContain("text-gray-900");
  });

  it("is case-insensitive when highlighting", () => {
    render(<HighlightText text="Hello, World!" searchTerm="world" />);
    const highlightedText = screen.getByText((content, element) => {
      return element?.tagName.toLowerCase() === "mark" && content === "World";
    });
    expect(highlightedText).toBeDefined();
  });

  it("handles multiple occurrences of search term", () => {
    render(<HighlightText text="Hello, hello, hello!" searchTerm="hello" />);
    const highlightedTexts = screen.getAllByText((content, element) => {
      return element?.tagName.toLowerCase() === "mark" && content.toLowerCase() === "hello";
    });
    expect(highlightedTexts).toHaveLength(3);
  });

  it("accepts custom highlightClassName override", () => {
    render(<HighlightText text="Hello, world!" searchTerm="world" highlightClassName="custom-class" />);
    const highlightedText = screen.getByText((content, element) => {
      return element?.tagName.toLowerCase() === "mark" && content === "world";
    });
    expect(Array.from(highlightedText.classList)).toContain("custom-class");
  });
});

describe("highlightText", () => {
  it("returns original text when no search term is provided", () => {
    const result = highlightText("Hello, world!", "");
    expect(result).toBe("Hello, world!");
  });

  it("returns React elements with highlighted parts", () => {
    const result = highlightText("Hello, world!", "world");

    expect(Array.isArray(result)).toBe(true);

    if (Array.isArray(result)) {
      const highlightedPart = result[1] as React.ReactElement<{ children: string }>;

      expect(React.isValidElement(highlightedPart)).toBe(true);
      expect(highlightedPart.type).toBe("mark");
      expect(highlightedPart.props.children).toBe("world");
    }
  });

  it("uses custom highlightClassName when provided", () => {
    const result = highlightText("Hello, world!", "world", "custom-class");

    expect(Array.isArray(result)).toBe(true);

    if (Array.isArray(result)) {
      const highlightedPart = result[1] as React.ReactElement<{
        children: string;
        className: string;
      }>;
      expect(highlightedPart.props.className).toBe("custom-class");
    }
  });
});
