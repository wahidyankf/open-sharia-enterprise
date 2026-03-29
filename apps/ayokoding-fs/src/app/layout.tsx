import type { Metadata } from "next";
import { GoogleAnalytics } from "@next/third-parties/google";
import "katex/dist/katex.min.css";
import "./globals.css";

export const metadata: Metadata = {
  title: {
    default: "AyoKoding",
    template: "%s | AyoKoding",
  },
  description:
    "Bilingual educational platform for software engineering - helping the Indonesian tech community learn and grow",
  metadataBase: new URL("https://ayokoding.com"),
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en" suppressHydrationWarning>
      <body className="min-h-screen antialiased">
        {children}
        <GoogleAnalytics gaId="G-1NHDR7S3GV" />
      </body>
    </html>
  );
}
