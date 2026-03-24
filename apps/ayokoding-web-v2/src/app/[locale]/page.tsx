import { serverCaller } from "@/lib/trpc/server";
import Link from "next/link";
import type { Locale } from "@/lib/i18n/config";
import type { TreeNode } from "@/server/content/types";

interface Props {
  params: Promise<{ locale: string }>;
}

export default async function LocaleHomePage({ params }: Props) {
  const { locale } = await params;

  const tree = (await serverCaller.content.getTree({
    locale: locale as Locale,
  })) as TreeNode[];

  return (
    <div className="mx-auto max-w-3xl px-4 py-8">
      <h1 className="mb-6 text-3xl font-bold">{locale === "id" ? "Konten Bahasa Indonesia" : "English Content"}</h1>
      <ul className="space-y-2">
        {tree.map((node) => (
          <li key={node.slug}>
            <Link href={`/${locale}/${node.slug}`} className="text-primary hover:underline">
              {node.title}
            </Link>
            {node.children.length > 0 && (
              <ul className="ml-4 mt-1 space-y-1">
                {node.children.map((child) => (
                  <li key={child.slug}>
                    <Link href={`/${locale}/${child.slug}`} className="text-primary hover:underline">
                      {child.title}
                    </Link>
                  </li>
                ))}
              </ul>
            )}
          </li>
        ))}
      </ul>
    </div>
  );
}
