import { serverCaller } from "@/lib/trpc/server";
import { SidebarTree } from "./sidebar-tree";
import type { TreeNode } from "@/server/content/types";

interface SidebarProps {
  locale: string;
}

export async function Sidebar({ locale }: SidebarProps) {
  const tree = (await serverCaller.content.getTree({
    locale: locale as "en" | "id",
  })) as TreeNode[];

  // Skip the root locale node (e.g., "English Content") and show its children directly
  const rootNode = tree.find((n) => n.slug === "");
  const sidebarNodes = rootNode ? rootNode.children : tree;

  return (
    <nav aria-label="Sidebar navigation">
      <SidebarTree nodes={sidebarNodes} locale={locale} />
    </nav>
  );
}
