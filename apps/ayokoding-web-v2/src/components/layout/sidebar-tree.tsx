"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { ChevronRight } from "lucide-react";
import { useState } from "react";
import { cn } from "@/lib/utils";
import type { TreeNode } from "@/server/content/types";

interface SidebarTreeProps {
  nodes: TreeNode[];
  locale: string;
  depth?: number;
}

export function SidebarTree({ nodes, locale, depth = 0 }: SidebarTreeProps) {
  return (
    <ul className={cn("space-y-0.5", depth > 0 && "ml-3 border-l border-border pl-2")}>
      {nodes.map((node) => (
        <SidebarNode key={node.slug} node={node} locale={locale} depth={depth} />
      ))}
    </ul>
  );
}

function SidebarNode({ node, locale, depth }: { node: TreeNode; locale: string; depth: number }) {
  const pathname = usePathname();
  const href = `/${locale}/${node.slug}`;
  const isActive = pathname === href;
  const isParent = pathname.startsWith(href + "/");
  const [expanded, setExpanded] = useState(isActive || isParent);

  const hasChildren = node.children.length > 0;

  return (
    <li>
      <div className="flex items-center">
        <Link
          href={href}
          className={cn(
            "flex-1 truncate rounded-md px-2 py-1.5 text-sm transition-colors",
            isActive
              ? "bg-primary/10 font-medium text-primary"
              : "text-muted-foreground hover:bg-accent hover:text-foreground",
          )}
        >
          {node.title}
        </Link>
        {hasChildren && (
          <button
            onClick={() => setExpanded(!expanded)}
            className="p-1 text-muted-foreground hover:text-foreground"
            aria-label={expanded ? "Collapse section" : "Expand section"}
          >
            <ChevronRight className={cn("h-4 w-4 transition-transform", expanded && "rotate-90")} />
          </button>
        )}
      </div>
      {hasChildren && expanded && <SidebarTree nodes={node.children} locale={locale} depth={depth + 1} />}
    </li>
  );
}
