import "./globals.css";
import type { Metadata } from "next";

export const metadata: Metadata = {
  title: "Demo Next.js App",
  description: "Demo Next.js app for Nx monorepo",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body>{children}</body>
    </html>
  );
}
