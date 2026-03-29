"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";

interface NavItem {
  href: string;
  label: string;
  icon: string;
}

const NAV_ITEMS: NavItem[] = [
  { href: "/", label: "Home", icon: "&#127968;" },
  { href: "/expenses", label: "Expenses", icon: "&#128181;" },
  { href: "/expenses/summary", label: "Summary", icon: "&#128202;" },
  { href: "/admin/users", label: "Admin", icon: "&#128101;" },
  { href: "/tokens", label: "Tokens", icon: "&#128272;" },
  { href: "/profile", label: "Profile", icon: "&#128100;" },
];

interface SidebarProps {
  isOpen: boolean;
  onClose: () => void;
  variant: "desktop" | "tablet" | "mobile";
}

export function Sidebar({ isOpen, onClose, variant }: SidebarProps) {
  const pathname = usePathname();

  const isTablet = variant === "tablet";
  const isMobile = variant === "mobile";

  if (isMobile && !isOpen) return null;

  return (
    <>
      {isMobile && isOpen && <div aria-hidden="true" onClick={onClose} className="fixed inset-0 z-[150] bg-black/50" />}
      <nav
        aria-label="Main navigation"
        data-testid={isMobile ? "nav-drawer" : undefined}
        className={[
          isMobile ? "fixed top-0 bottom-0 left-0 z-[160]" : "relative",
          isTablet ? "w-16" : "w-56",
          "flex shrink-0 flex-col overflow-y-auto bg-slate-800 text-[#e0e0e0]",
          isMobile ? "pt-14" : "pt-4",
        ].join(" ")}
      >
        <ul className="m-0 list-none py-2">
          {NAV_ITEMS.map((item) => {
            const isActive = pathname === item.href;
            return (
              <li key={item.href}>
                <Link
                  href={item.href}
                  onClick={isMobile ? onClose : undefined}
                  title={isTablet ? item.label : undefined}
                  aria-current={isActive ? "page" : undefined}
                  className={[
                    "mx-1 flex items-center gap-3 rounded no-underline transition-colors duration-150",
                    isTablet ? "justify-center px-3 py-3" : "px-4 py-3",
                    isActive ? "bg-white/10 font-semibold text-white" : "bg-transparent font-normal text-[#b0b8c8]",
                  ].join(" ")}
                >
                  <span
                    aria-hidden="true"
                    className="shrink-0 text-[1.2rem]"
                    dangerouslySetInnerHTML={{ __html: item.icon }}
                  />
                  {!isTablet && <span className="text-[0.9rem]">{item.label}</span>}
                </Link>
              </li>
            );
          })}
        </ul>
      </nav>
    </>
  );
}
