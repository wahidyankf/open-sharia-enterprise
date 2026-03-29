"use client";

import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { domToReact, type HTMLReactParserOptions, type DOMNode, Element } from "html-react-parser";

interface ContentTabsProps {
  items: string;
  children: DOMNode[];
  options: HTMLReactParserOptions;
}

export function ContentTabs({ items, children, options }: ContentTabsProps) {
  const labels = items.split(",").map((s) => s.trim());

  // Collect tab content children (data-tab divs)
  const tabChildren = children.filter(
    (child): child is Element => child instanceof Element && child.name === "div" && "data-tab" in child.attribs,
  );

  const defaultValue = labels[0] ?? "tab-0";

  return (
    <Tabs defaultValue={defaultValue} className="my-4">
      <TabsList>
        {labels.map((label) => (
          <TabsTrigger key={label} value={label}>
            {label}
          </TabsTrigger>
        ))}
      </TabsList>
      {labels.map((label, i) => {
        const tabChild = tabChildren[i];
        return (
          <TabsContent key={label} value={label}>
            {tabChild ? domToReact(tabChild.children as DOMNode[], options) : null}
          </TabsContent>
        );
      })}
    </Tabs>
  );
}
