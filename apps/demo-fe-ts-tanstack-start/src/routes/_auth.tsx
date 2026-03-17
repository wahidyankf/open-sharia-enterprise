import { createFileRoute, Outlet, redirect } from "@tanstack/react-router";
import { useEffect } from "react";
import { AppShell } from "../components/layout/app-shell";
import { useAuth } from "../lib/auth/auth-provider";
import { getAccessToken } from "../lib/api/client";

export const Route = createFileRoute("/_auth")({
  beforeLoad: () => {
    if (typeof window !== "undefined" && !getAccessToken()) {
      // Set session expired message unless user explicitly logged out
      const isExplicitLogout = sessionStorage.getItem("explicit_logout") === "true";
      if (!isExplicitLogout && !sessionStorage.getItem("auth_error")) {
        sessionStorage.setItem(
          "auth_error",
          "Your session has expired or your account has been disabled. Please log in again.",
        );
      }
      sessionStorage.removeItem("explicit_logout");
      throw redirect({ to: "/login", search: { registered: undefined } });
    }
  },
  component: AuthLayout,
});

function AuthLayout() {
  const { isAuthenticated, isLoading, error: authError } = useAuth();

  useEffect(() => {
    if (!isLoading && !isAuthenticated) {
      // Store the auth error in sessionStorage so it survives the page reload
      if (authError && !sessionStorage.getItem("auth_error")) {
        sessionStorage.setItem("auth_error", authError);
      }
      if (!sessionStorage.getItem("auth_error")) {
        sessionStorage.setItem(
          "auth_error",
          "Your session has expired or your account has been disabled. Please log in again.",
        );
      }
      window.location.href = "/login";
    }
  }, [isAuthenticated, isLoading, authError]);

  if (isLoading) return <div>Loading...</div>;
  if (!isAuthenticated) return null;

  return (
    <AppShell>
      <Outlet />
    </AppShell>
  );
}
