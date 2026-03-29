"use client";

import { Sheet, SheetContent, SheetHeader, SheetTitle } from "@/components/ui/sheet";
import { SidebarTree } from "./sidebar-tree";
import { useEffect, useState } from "react";
import type { TreeNode } from "@/server/content/types";
import { trpcClient } from "@/lib/trpc/client";

interface MobileNavProps {
  locale: string;
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export function MobileNav({ locale, open, onOpenChange }: MobileNavProps) {
  const [tree, setTree] = useState<TreeNode[]>([]);

  useEffect(() => {
    if (open && tree.length === 0) {
      trpcClient.content.getTree.query({ locale: locale as "en" | "id" }).then((data) => setTree(data as TreeNode[]));
    }
  }, [open, locale, tree.length]);

  return (
    <Sheet open={open} onOpenChange={onOpenChange}>
      <SheetContent side="left" className="w-[280px] overflow-y-auto p-4">
        <SheetHeader>
          <SheetTitle className="text-left text-lg font-bold">AyoKoding</SheetTitle>
        </SheetHeader>
        <nav className="mt-4" aria-label="Mobile navigation">
          <SidebarTree nodes={tree} locale={locale} />
        </nav>
      </SheetContent>
    </Sheet>
  );
}
