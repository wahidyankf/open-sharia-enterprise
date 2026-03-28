import type { Metadata } from "next";
import { ThemeProvider } from "next-themes";
import { TRPCProvider } from "@/lib/trpc/provider";
import { SearchProvider } from "@/components/search/search-provider";
import { TooltipProvider } from "@/components/ui/tooltip";
import "./globals.css";

export const metadata: Metadata = {
  title: {
    default: "OSE Platform",
    template: "%s | OSE Platform",
  },
  description:
    "Open-source platform for Sharia-compliant enterprise solutions. Starting with Indonesian regulations, expanding to ERP, fintech, and global markets.",
  metadataBase: new URL("https://oseplatform.com"),
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="min-h-screen antialiased">
        <ThemeProvider attribute="class" defaultTheme="light" enableSystem>
          <TRPCProvider>
            <TooltipProvider>
              <SearchProvider>{children}</SearchProvider>
            </TooltipProvider>
          </TRPCProvider>
        </ThemeProvider>
      </body>
    </html>
  );
}
