import { Sidebar } from "@/components/layout/sidebar";

interface Props {
  children: React.ReactNode;
  params: Promise<{ locale: string }>;
}

export default async function ContentLayout({ children, params }: Props) {
  const { locale } = await params;

  return (
    <div className="mx-auto flex w-full max-w-screen-2xl">
      <aside className="hidden w-[250px] shrink-0 border-r border-border md:block">
        <div className="sticky top-16 h-[calc(100vh-4rem)] overflow-y-auto p-4">
          <Sidebar locale={locale} />
        </div>
      </aside>
      <div className="flex min-w-0 flex-1">{children}</div>
    </div>
  );
}
