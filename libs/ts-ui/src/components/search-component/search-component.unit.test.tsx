import { describe, it, expect, vi } from "vitest";
import { render, fireEvent, screen } from "@testing-library/react";
import { SearchComponent } from "./search-component";

describe("SearchComponent", () => {
  const mockSetSearchTerm = vi.fn();
  const mockUpdateURL = vi.fn();
  const placeholder = "Search...";

  const renderComponent = (searchTerm = "") => {
    return render(
      <SearchComponent
        searchTerm={searchTerm}
        setSearchTerm={mockSetSearchTerm}
        updateURL={mockUpdateURL}
        placeholder={placeholder}
      />,
    );
  };

  it("renders with correct placeholder", () => {
    renderComponent();
    expect(screen.getByPlaceholderText(placeholder)).toBeDefined();
  });

  it("updates search term on input change", () => {
    renderComponent();
    const input = screen.getByPlaceholderText(placeholder);
    fireEvent.change(input, { target: { value: "test" } });
    expect(mockSetSearchTerm).toHaveBeenCalledWith("test");
    expect(mockUpdateURL).toHaveBeenCalledWith("test");
  });

  it("clears search when clear button is clicked", () => {
    renderComponent("initial search");
    const clearButton = screen.getByLabelText("Clear search");
    fireEvent.click(clearButton);
    expect(mockSetSearchTerm).toHaveBeenCalledWith("");
    expect(mockUpdateURL).toHaveBeenCalledWith("");
  });

  it("does not show clear button when search term is empty", () => {
    renderComponent();
    expect(screen.queryByLabelText("Clear search")).toBeNull();
  });

  it("shows clear button when search term is not empty", () => {
    renderComponent("test");
    expect(screen.getByLabelText("Clear search")).toBeDefined();
  });

  it("has correct accessibility attributes", () => {
    renderComponent("test");
    const clearButton = screen.getByLabelText("Clear search");
    expect(clearButton.getAttribute("aria-label")).toBe("Clear search");
  });

  it("accepts custom className for container", () => {
    const { container } = render(
      <SearchComponent
        searchTerm=""
        setSearchTerm={mockSetSearchTerm}
        updateURL={mockUpdateURL}
        placeholder={placeholder}
        className="custom-container"
      />,
    );
    const firstChild = container.firstChild as HTMLElement;
    expect(Array.from(firstChild.classList)).toContain("custom-container");
  });
});
