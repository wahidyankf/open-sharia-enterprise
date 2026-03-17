import { createFileRoute, Outlet } from "@tanstack/react-router";
import { useEffect } from "react";
import { AppShell } from "../components/layout/app-shell";
import { useAuth } from "../lib/auth/auth-provider";

export const Route = createFileRoute("/_auth")({
  component: AuthLayout,
});

function AuthLayout() {
  const { isAuthenticated, isLoading } = useAuth();

  const { error: authError, setError: setAuthError } = useAuth();

  useEffect(() => {
    if (!isLoading && !isAuthenticated) {
      // Set session expired message if not already set
      if (!authError) {
        const storedError = sessionStorage.getItem("auth_error");
        if (storedError) {
          setAuthError(storedError);
          sessionStorage.removeItem("auth_error");
        } else {
          setAuthError("Your session has expired or your account has been disabled. Please log in again.");
        }
      }
      window.location.href = "/login";
    }
  }, [isAuthenticated, isLoading, authError, setAuthError]);

  if (isLoading) return <div>Loading...</div>;
  if (!isAuthenticated) return null;

  return (
    <AppShell>
      <Outlet />
    </AppShell>
  );
}
