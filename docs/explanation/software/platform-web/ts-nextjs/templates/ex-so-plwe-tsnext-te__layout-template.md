---
title: Next.js Layout Component Template
description: Production-ready template for Next.js App Router layout components with fonts, providers, navigation, and nested layout patterns
category: explanation
tags:
  - nextjs
  - template
  - layout
  - providers
  - navigation
created: 2026-01-26
updated: 2026-01-26
---

# Next.js Layout Component Template

Production-ready template for creating Next.js App Router layout components with font optimization, global providers, navigation, metadata, and nested layout patterns for OSE Platform.

## 📋 Template Usage

**File location**: `app/*/layout.tsx`

**Use this template when**:

- Creating app structure with persistent UI
- Setting up global providers (auth, theme, analytics)
- Configuring navigation and sidebars
- Managing nested layouts for different sections

## 🎯 Root Layout Template

```typescript
// app/layout.tsx
import type { Metadata } from 'next';
import { Inter, Amiri } from 'next/font/google';
import { SessionProvider } from '@/components/providers/SessionProvider';
import { ThemeProvider } from '@/components/providers/ThemeProvider';
import { Analytics } from '@/components/Analytics';
import './globals.css';

// Font optimization
const inter = Inter({
  subsets: ['latin'],
  display: 'swap',
  variable: '--font-inter',
});

const amiri = Amiri({
  weight: ['400', '700'],
  subsets: ['arabic'],
  display: 'swap',
  variable: '--font-amiri',
});

// Root metadata
export const metadata: Metadata = {
  title: {
    default: 'OSE Platform - Islamic Finance Solutions',
    template: '%s | OSE Platform',
  },
  description: 'Comprehensive Islamic finance platform for Zakat, Murabaha, and Waqf management',
  keywords: ['Islamic finance', 'Zakat', 'Murabaha', 'Waqf', 'Shariah-compliant'],
  authors: [{ name: 'OSE Platform Team' }],
  creator: 'OSE Platform',
  metadataBase: new URL('https://oseplatform.com'),
  openGraph: {
    type: 'website',
    locale: 'en_US',
    url: 'https://oseplatform.com',
    siteName: 'OSE Platform',
    title: 'OSE Platform - Islamic Finance Solutions',
    description: 'Comprehensive Islamic finance platform',
  },
  twitter: {
    card: 'summary_large_image',
    title: 'OSE Platform',
    description: 'Islamic Finance Solutions',
  },
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html
      lang="en"
      className={`${inter.variable} ${amiri.variable}`}
      suppressHydrationWarning
    >
      <body className="min-h-screen bg-background font-sans antialiased">
        <SessionProvider>
          <ThemeProvider attribute="class" defaultTheme="system" enableSystem>
            {children}
            <Analytics />
          </ThemeProvider>
        </SessionProvider>
      </body>
    </html>
  );
}
```

## 🎨 Layout with Navigation

```typescript
// app/(platform)/layout.tsx
import { auth } from '@/lib/auth';
import { redirect } from 'next/navigation';
import { Header } from '@/components/layout/Header';
import { Sidebar } from '@/components/layout/Sidebar';
import { Footer } from '@/components/layout/Footer';

export default async function PlatformLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  return (
    <div className="flex min-h-screen flex-col">
      <Header user={session.user} />

      <div className="flex flex-1">
        <Sidebar />

        <main className="flex-1 p-6">
          {children}
        </main>
      </div>

      <Footer />
    </div>
  );
}
```

## 📱 Responsive Layout with Mobile Menu

```typescript
// app/(dashboard)/layout.tsx
'use client';

import { useState } from 'react';
import { usePathname } from 'next/navigation';
import Link from 'next/link';
import { MenuIcon, XIcon } from 'lucide-react';

const navItems = [
  { href: '/dashboard', label: 'Dashboard' },
  { href: '/zakat', label: 'Zakat' },
  { href: '/murabaha', label: 'Murabaha' },
  { href: '/waqf', label: 'Waqf' },
  { href: '/settings', label: 'Settings' },
];

export default function DashboardLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);
  const pathname = usePathname();

  return (
    <div className="flex min-h-screen">
      {/* Desktop Sidebar */}
      <aside className="hidden md:flex md:w-64 md:flex-col md:border-r md:bg-gray-50">
        <div className="flex flex-col gap-2 p-4">
          {navItems.map((item) => (
            <Link
              key={item.href}
              href={item.href}
              className={`px-4 py-2 rounded-lg transition-colors ${
                pathname === item.href
                  ? 'bg-primary text-white'
                  : 'hover:bg-gray-200'
              }`}
            >
              {item.label}
            </Link>
          ))}
        </div>
      </aside>

      {/* Mobile Menu */}
      {mobileMenuOpen && (
        <div className="fixed inset-0 z-50 md:hidden">
          <div
            className="fixed inset-0 bg-black/50"
            onClick={() => setMobileMenuOpen(false)}
          />
          <nav className="fixed left-0 top-0 bottom-0 w-64 bg-white p-4">
            <button
              onClick={() => setMobileMenuOpen(false)}
              className="mb-4"
            >
              <XIcon className="h-6 w-6" />
            </button>

            <div className="flex flex-col gap-2">
              {navItems.map((item) => (
                <Link
                  key={item.href}
                  href={item.href}
                  onClick={() => setMobileMenuOpen(false)}
                  className={`px-4 py-2 rounded-lg ${
                    pathname === item.href
                      ? 'bg-primary text-white'
                      : 'hover:bg-gray-100'
                  }`}
                >
                  {item.label}
                </Link>
              ))}
            </div>
          </nav>
        </div>
      )}

      {/* Main Content */}
      <div className="flex-1">
        {/* Mobile Header */}
        <header className="flex items-center justify-between border-b p-4 md:hidden">
          <button onClick={() => setMobileMenuOpen(true)}>
            <MenuIcon className="h-6 w-6" />
          </button>
          <h1 className="text-xl font-bold">OSE Platform</h1>
          <div className="w-6" /> {/* Spacer for centering */}
        </header>

        <main className="p-6">{children}</main>
      </div>
    </div>
  );
}
```

## 🔐 Protected Layout with Authorization

```typescript
// app/(admin)/layout.tsx
import { auth } from '@/lib/auth';
import { redirect } from 'next/navigation';
import { AdminSidebar } from '@/components/admin/AdminSidebar';

export default async function AdminLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const session = await auth();

  // Authentication check
  if (!session) {
    redirect('/login');
  }

  // Authorization check (admin only)
  if (session.user.role !== 'ADMIN') {
    redirect('/dashboard');
  }

  return (
    <div className="flex min-h-screen">
      <AdminSidebar user={session.user} />

      <main className="flex-1 bg-gray-50">
        <div className="border-b bg-white p-4">
          <h1 className="text-2xl font-bold">Admin Panel</h1>
        </div>

        <div className="p-6">{children}</div>
      </main>
    </div>
  );
}
```

## 📊 Layout with Parallel Routes

```typescript
// app/(dashboard)/layout.tsx
export default function DashboardLayout({
  children,
  analytics,
  notifications,
}: {
  children: React.ReactNode;
  analytics: React.ReactNode;
  notifications: React.ReactNode;
}) {
  return (
    <div className="grid grid-cols-12 gap-6 p-6">
      {/* Main Content */}
      <div className="col-span-12 lg:col-span-8">
        {children}
      </div>

      {/* Sidebar with parallel routes */}
      <aside className="col-span-12 lg:col-span-4 space-y-6">
        {/* @analytics slot */}
        <section className="rounded-lg border bg-white p-4">
          {analytics}
        </section>

        {/* @notifications slot */}
        <section className="rounded-lg border bg-white p-4">
          {notifications}
        </section>
      </aside>
    </div>
  );
}
```

## 🎯 Complete OSE Platform Layout

```typescript
// app/(platform)/layout.tsx
import type { Metadata } from 'next';
import { Inter, Amiri } from 'next/font/google';
import { auth } from '@/lib/auth';
import { redirect } from 'next/navigation';
import { PlatformHeader } from '@/components/platform/PlatformHeader';
import { PlatformSidebar } from '@/components/platform/PlatformSidebar';
import { PlatformFooter } from '@/components/platform/PlatformFooter';
import { Toaster } from '@/components/ui/Toaster';

const inter = Inter({
  subsets: ['latin'],
  display: 'swap',
  variable: '--font-inter',
});

const amiri = Amiri({
  weight: ['400', '700'],
  subsets: ['arabic'],
  display: 'swap',
  variable: '--font-amiri',
});

export const metadata: Metadata = {
  title: {
    default: 'Platform - OSE',
    template: '%s | OSE Platform',
  },
};

export default async function PlatformLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  const session = await auth();

  if (!session) {
    redirect('/login');
  }

  return (
    <div className={`${inter.variable} ${amiri.variable} min-h-screen flex flex-col`}>
      <PlatformHeader user={session.user} />

      <div className="flex flex-1">
        <PlatformSidebar />

        <main className="flex-1 bg-gray-50">
          <div className="container mx-auto px-4 py-6">
            {children}
          </div>
        </main>
      </div>

      <PlatformFooter />
      <Toaster />
    </div>
  );
}
```

## 🌍 Internationalized Layout

```typescript
// app/[lang]/layout.tsx
import { notFound } from 'next/navigation';
import { Inter, Amiri } from 'next/font/google';

const inter = Inter({ subsets: ['latin'], variable: '--font-inter' });
const amiri = Amiri({ weight: ['400', '700'], subsets: ['arabic'], variable: '--font-amiri' });

const languages = ['en', 'ar'];

export async function generateStaticParams() {
  return languages.map((lang) => ({ lang }));
}

export default function LangLayout({
  children,
  params,
}: {
  children: React.ReactNode;
  params: { lang: string };
}) {
  if (!languages.includes(params.lang)) {
    notFound();
  }

  const isRTL = params.lang === 'ar';

  return (
    <html
      lang={params.lang}
      dir={isRTL ? 'rtl' : 'ltr'}
      className={`${inter.variable} ${amiri.variable}`}
    >
      <body className={isRTL ? 'font-arabic' : 'font-sans'}>
        {children}
      </body>
    </html>
  );
}
```

## 🎨 Layout Components

### Header Component

```typescript
// components/platform/PlatformHeader.tsx
import Link from 'next/link';
import { UserMenu } from './UserMenu';
import { ThemeToggle } from './ThemeToggle';

interface PlatformHeaderProps {
  user: {
    name: string;
    email: string;
    image?: string;
  };
}

export function PlatformHeader({ user }: PlatformHeaderProps) {
  return (
    <header className="sticky top-0 z-50 border-b bg-white">
      <div className="container mx-auto flex items-center justify-between px-4 py-3">
        <Link href="/dashboard" className="text-2xl font-bold">
          OSE Platform
        </Link>

        <nav className="hidden md:flex items-center gap-6">
          <Link href="/zakat" className="hover:text-primary">
            Zakat
          </Link>
          <Link href="/murabaha" className="hover:text-primary">
            Murabaha
          </Link>
          <Link href="/waqf" className="hover:text-primary">
            Waqf
          </Link>
        </nav>

        <div className="flex items-center gap-4">
          <ThemeToggle />
          <UserMenu user={user} />
        </div>
      </div>
    </header>
  );
}
```

### Sidebar Component

```typescript
// components/platform/PlatformSidebar.tsx
'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';
import {
  LayoutDashboardIcon,
  CalculatorIcon,
  FileTextIcon,
  HeartHandshakeIcon,
  SettingsIcon,
} from 'lucide-react';

const navItems = [
  {
    href: '/dashboard',
    label: 'Dashboard',
    icon: LayoutDashboardIcon,
  },
  {
    href: '/zakat',
    label: 'Zakat',
    icon: CalculatorIcon,
  },
  {
    href: '/murabaha',
    label: 'Murabaha',
    icon: FileTextIcon,
  },
  {
    href: '/waqf',
    label: 'Waqf',
    icon: HeartHandshakeIcon,
  },
  {
    href: '/settings',
    label: 'Settings',
    icon: SettingsIcon,
  },
];

export function PlatformSidebar() {
  const pathname = usePathname();

  return (
    <aside className="hidden md:flex w-64 flex-col border-r bg-white">
      <nav className="flex-1 p-4 space-y-2">
        {navItems.map((item) => {
          const Icon = item.icon;
          const isActive = pathname.startsWith(item.href);

          return (
            <Link
              key={item.href}
              href={item.href}
              className={`flex items-center gap-3 px-4 py-2 rounded-lg transition-colors ${
                isActive
                  ? 'bg-primary text-white'
                  : 'hover:bg-gray-100'
              }`}
            >
              <Icon className="h-5 w-5" />
              <span>{item.label}</span>
            </Link>
          );
        })}
      </nav>
    </aside>
  );
}
```

### Footer Component

```typescript
// components/platform/PlatformFooter.tsx
import Link from 'next/link';

export function PlatformFooter() {
  return (
    <footer className="border-t bg-white">
      <div className="container mx-auto px-4 py-6">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
          <div>
            <h3 className="font-bold mb-4">OSE Platform</h3>
            <p className="text-sm text-gray-600">
              Comprehensive Islamic finance solutions for the modern era
            </p>
          </div>

          <div>
            <h4 className="font-semibold mb-4">Products</h4>
            <ul className="space-y-2 text-sm">
              <li>
                <Link href="/zakat" className="text-gray-600 hover:text-primary">
                  Zakat Calculator
                </Link>
              </li>
              <li>
                <Link href="/murabaha" className="text-gray-600 hover:text-primary">
                  Murabaha Financing
                </Link>
              </li>
              <li>
                <Link href="/waqf" className="text-gray-600 hover:text-primary">
                  Waqf Projects
                </Link>
              </li>
            </ul>
          </div>

          <div>
            <h4 className="font-semibold mb-4">Company</h4>
            <ul className="space-y-2 text-sm">
              <li>
                <Link href="/about" className="text-gray-600 hover:text-primary">
                  About Us
                </Link>
              </li>
              <li>
                <Link href="/contact" className="text-gray-600 hover:text-primary">
                  Contact
                </Link>
              </li>
              <li>
                <Link href="/careers" className="text-gray-600 hover:text-primary">
                  Careers
                </Link>
              </li>
            </ul>
          </div>

          <div>
            <h4 className="font-semibold mb-4">Legal</h4>
            <ul className="space-y-2 text-sm">
              <li>
                <Link href="/privacy" className="text-gray-600 hover:text-primary">
                  Privacy Policy
                </Link>
              </li>
              <li>
                <Link href="/terms" className="text-gray-600 hover:text-primary">
                  Terms of Service
                </Link>
              </li>
            </ul>
          </div>
        </div>

        <div className="mt-8 pt-8 border-t text-center text-sm text-gray-600">
          © {new Date().getFullYear()} OSE Platform. All rights reserved.
        </div>
      </div>
    </footer>
  );
}
```

## 📚 Best Practices

### 1. Root Layout Required

Every app needs exactly one root layout in `app/layout.tsx`.

### 2. Use Font Optimization

```typescript
import { Inter } from "next/font/google";

const inter = Inter({ subsets: ["latin"] });
```

### 3. Wrap Providers in Root Layout

```typescript
<SessionProvider>
  <ThemeProvider>
    {children}
  </ThemeProvider>
</SessionProvider>
```

### 4. Use Route Groups for Organization

```
app/
├── (marketing)/
│   ├── layout.tsx
│   └── page.tsx
└── (platform)/
    ├── layout.tsx
    └── dashboard/
        └── page.tsx
```

### 5. Server Components by Default

Layouts are Server Components unless marked with `'use client'`.

### 6. Nested Layouts Inherit

Child layouts inherit parent layouts automatically.

## 🔗 Related Templates

- [Page Template](./ex-so-plwe-tsnext-te__page-template.md) - Page components
- [API Route Template](./ex-so-plwe-tsnext-te__api-route-template.md) - API handlers
- [Server Action Template](./ex-so-plwe-tsnext-te__server-action-template.md) - Form actions

---

This template provides comprehensive layout patterns for Next.js App Router with OSE Platform conventions and best practices.
