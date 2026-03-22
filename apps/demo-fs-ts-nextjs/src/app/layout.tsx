import type { Metadata } from "next";

export const metadata: Metadata = {
  title: "Demo Fullstack - Next.js",
  description: "Fullstack Next.js demo application",
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
