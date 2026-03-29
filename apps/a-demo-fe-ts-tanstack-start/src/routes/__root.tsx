import { createRootRoute, Outlet } from "@tanstack/react-router";

export const Route = createRootRoute({
  component: function RootComponent() {
    return <Outlet />;
  },
});
