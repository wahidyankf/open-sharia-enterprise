import { notFound } from "next/navigation";
import { ThemeProvider } from "next-themes";
import { SUPPORTED_LOCALES } from "@/contexts/i18n/application/config";
import { TRPCProvider } from "@/lib/trpc/provider";
import { SearchProvider } from "@/contexts/search/presentation/search-provider";
import { Header } from "@/contexts/app-shell/presentation/header";
import { Footer } from "@/contexts/app-shell/presentation/footer";

export function generateStaticParams() {
  return SUPPORTED_LOCALES.map((locale) => ({ locale }));
}

interface Props {
  children: React.ReactNode;
  params: Promise<{ locale: string }>;
}

export default async function LocaleLayout({ children, params }: Props) {
  const { locale } = await params;

  if (!(SUPPORTED_LOCALES as readonly string[]).includes(locale)) {
    notFound();
  }

  return (
    <ThemeProvider attribute="class" defaultTheme="light" enableSystem>
      <TRPCProvider>
        <SearchProvider>
          <a
            href="#main-content"
            className="sr-only focus:not-sr-only focus:absolute focus:top-4 focus:left-4 focus:z-50 focus:rounded focus:bg-primary focus:px-4 focus:py-2 focus:text-primary-foreground"
          >
            Skip to content
          </a>
          <div className="flex min-h-screen flex-col">
            <Header locale={locale} />
            <main id="main-content" className="flex-1">
              {children}
            </main>
            <Footer locale={locale} />
          </div>
        </SearchProvider>
      </TRPCProvider>
    </ThemeProvider>
  );
}
