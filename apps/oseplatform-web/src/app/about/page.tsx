import type { Metadata } from "next";
import { notFound } from "next/navigation";
import { Header } from "@/components/layout/header";
import { Footer } from "@/components/layout/footer";
import { Breadcrumb } from "@/components/layout/breadcrumb";
import { TableOfContents } from "@/components/layout/toc";
import { MarkdownRenderer } from "@/components/content/markdown-renderer";
import { serverCaller } from "@/lib/trpc/server";

export async function generateMetadata(): Promise<Metadata> {
  const page = await serverCaller.content.getBySlug({ slug: "about" });
  if (!page) return {};
  return {
    title: page.title,
    description: page.description ?? page.summary,
  };
}

export default async function AboutPage() {
  const page = await serverCaller.content.getBySlug({ slug: "about" });
  if (!page) notFound();

  const showToc = page.headings.length > 0;

  return (
    <>
      <Header />
      <main className="mx-auto max-w-screen-xl px-4 py-8">
        <Breadcrumb segments={[{ label: "About", href: "/about/" }]} />
        <div className={showToc ? "lg:grid lg:grid-cols-[1fr_250px] lg:gap-8" : ""}>
          <article>
            <h1 className="mb-4 text-3xl font-bold">{page.title}</h1>
            <p className="mb-8 font-mono text-sm text-muted-foreground">{page.readingTime} min read</p>
            <MarkdownRenderer html={page.html} />
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
