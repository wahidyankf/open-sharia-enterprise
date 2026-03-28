import type { Metadata } from "next";
import { notFound } from "next/navigation";
import { Badge } from "@/components/ui/badge";
import { Header } from "@/components/layout/header";
import { Footer } from "@/components/layout/footer";
import { Breadcrumb } from "@/components/layout/breadcrumb";
import { TableOfContents } from "@/components/layout/toc";
import { MarkdownRenderer } from "@/components/content/markdown-renderer";
import { PrevNext } from "@/components/layout/prev-next";
import { serverCaller } from "@/lib/trpc/server";

export const dynamicParams = false;

export async function generateStaticParams() {
  const updates = await serverCaller.content.listUpdates();
  return updates.map((u) => ({
    slug: u.slug.replace("updates/", ""),
  }));
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{ slug: string }>;
}): Promise<Metadata> {
  const { slug } = await params;
  const page = await serverCaller.content.getBySlug({
    slug: `updates/${slug}`,
  });
  if (!page) return {};
  return {
    title: page.title,
    description: page.description ?? page.summary,
  };
}

export default async function UpdateDetailPage({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = await params;
  const page = await serverCaller.content.getBySlug({
    slug: `updates/${slug}`,
  });
  if (!page) notFound();

  const dateStr = page.date
    ? new Intl.DateTimeFormat("en-US", {
        year: "numeric",
        month: "long",
        day: "numeric",
      }).format(page.date)
    : "";

  const showToc = page.headings.length > 0;

  return (
    <>
      <Header />
      <main className="mx-auto max-w-screen-xl px-4 py-8">
        <Breadcrumb
          segments={[
            { label: "Updates", href: "/updates/" },
            { label: page.title, href: `/${page.slug}/` },
          ]}
        />
        <div
          className={
            showToc ? "lg:grid lg:grid-cols-[1fr_250px] lg:gap-8" : ""
          }
        >
          <article>
            <h1 className="mb-4 text-3xl font-bold">{page.title}</h1>
            <div className="mb-8 flex flex-wrap items-center gap-3">
              {dateStr && (
                <span className="font-mono text-sm text-muted-foreground">
                  {dateStr}
                </span>
              )}
              <span className="text-sm text-muted-foreground">&middot;</span>
              <span className="text-sm text-muted-foreground">
                {page.readingTime} min read
              </span>
              {page.tags.length > 0 && (
                <div className="flex flex-wrap gap-1">
                  {page.tags.map((tag) => (
                    <Badge key={tag} variant="secondary" className="text-xs">
                      {tag}
                    </Badge>
                  ))}
                </div>
              )}
            </div>
            <MarkdownRenderer html={page.html} />
            <PrevNext prev={page.prev} next={page.next} />
          </article>
          {showToc && (
            <aside className="hidden lg:block">
              <div className="sticky top-20">
                <TableOfContents headings={page.headings} />
              </div>
            </aside>
          )}
        </div>
      </main>
      <Footer />
    </>
  );
}
