import type { Metadata } from "next";
import { Header } from "@/components/layout/header";
import { Footer } from "@/components/layout/footer";
import { Breadcrumb } from "@/components/layout/breadcrumb";
import { UpdateCard } from "@/components/content/update-card";
import { serverCaller } from "@/lib/trpc/server";

export const metadata: Metadata = {
  title: "Updates",
  description: "Weekly and monthly updates on OSE Platform development",
};

export default async function UpdatesPage() {
  const updates = await serverCaller.content.listUpdates();

  return (
    <>
      <Header />
      <main className="mx-auto max-w-screen-xl px-4 py-8">
        <Breadcrumb segments={[{ label: "Updates", href: "/updates/" }]} />
        <h1 className="mb-2 text-3xl font-bold">Project Updates</h1>
        <p className="mb-8 text-muted-foreground">
          Stay informed about the latest developments in the Open Sharia
          Enterprise Platform.
        </p>
        <div className="grid gap-4 sm:grid-cols-2">
          {updates.map((update) => (
            <UpdateCard key={update.slug} update={update} />
          ))}
        </div>
      </main>
      <Footer />
    </>
  );
}
