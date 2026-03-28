"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";
import { useCurrentUser } from "@/lib/queries/use-user";
import { useLogout, useLogoutAll } from "@/lib/queries/use-auth";

interface HeaderProps {
  onMenuToggle: () => void;
}

export function Header({ onMenuToggle }: HeaderProps) {
  const router = useRouter();
  const { data: user } = useCurrentUser();
  const logoutMutation = useLogout();
  const logoutAllMutation = useLogoutAll();
  const [userMenuOpen, setUserMenuOpen] = useState(false);

  const handleLogout = () => {
    logoutMutation.mutate(undefined, {
      onSettled: () => {
        router.push("/login");
      },
    });
  };

  const handleLogoutAll = () => {
    logoutAllMutation.mutate(undefined, {
      onSettled: () => {
        router.push("/login");
      },
    });
  };

  return (
    <header className="sticky top-0 z-[100] flex h-14 items-center justify-between bg-slate-900 px-4 text-white shadow-md">
      <div className="flex items-center gap-4">
        <button
          aria-label="Toggle navigation menu"
          onClick={onMenuToggle}
          className="flex cursor-pointer items-center border-none bg-transparent p-1 text-2xl text-white"
        >
          &#9776;
        </button>
        <span className="text-[1.1rem] font-bold">Demo Frontend</span>
      </div>

      <div className="relative">
        <button
          aria-label="User menu"
          aria-expanded={userMenuOpen}
          aria-haspopup="true"
          onClick={() => setUserMenuOpen((open) => !open)}
          className="cursor-pointer rounded border border-[#444] bg-transparent px-[0.8rem] py-[0.4rem] text-[0.9rem] text-white"
        >
          {user?.username ?? "Account"} &#9660;
        </button>

        {userMenuOpen && (
          <div
            role="menu"
            className="absolute top-[calc(100%+0.25rem)] right-0 z-[200] min-w-[12rem] rounded border border-gray-300 bg-white text-gray-800 shadow-lg"
          >
            <button
              role="menuitem"
              onClick={handleLogout}
              disabled={logoutMutation.isPending}
              className="block w-full cursor-pointer border-none bg-transparent px-4 py-3 text-left text-[0.9rem]"
            >
              {logoutMutation.isPending ? "Logging out..." : "Log out"}
            </button>
            <button
              role="menuitem"
              onClick={handleLogoutAll}
              disabled={logoutAllMutation.isPending}
              className="block w-full cursor-pointer border-t border-none border-gray-200 bg-transparent px-4 py-3 text-left text-[0.9rem]"
            >
              {logoutAllMutation.isPending ? "Logging out..." : "Log out all devices"}
            </button>
          </div>
        )}
      </div>
    </header>
  );
}
