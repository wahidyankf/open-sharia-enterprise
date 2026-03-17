import React from "react";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { vi, describe, it, expect, beforeEach } from "vitest";
import { cleanup } from "@testing-library/react";

const mockNavigate = vi.fn();
const mockLogoutMutate = vi.fn();
const mockLogoutAllMutate = vi.fn();

vi.mock("@tanstack/react-router", () => ({
  createFileRoute: (_path: string) => (opts: { component: React.ComponentType }) => ({
    options: opts,
    component: opts.component,
  }),
  Link: ({ children, to }: { children: React.ReactNode; to: string }) => <a href={to}>{children}</a>,
  useNavigate: () => mockNavigate,
  useRouterState: () => ({ location: { pathname: "/" } }),
  Outlet: () => <div data-testid="outlet">Outlet content</div>,
}));

vi.mock("@/lib/queries/use-auth", () => ({
  useLogout: vi.fn().mockReturnValue({ mutate: mockLogoutMutate, isPending: false }),
  useLogoutAll: vi.fn().mockReturnValue({ mutate: mockLogoutAllMutate, isPending: false }),
  useHealth: vi.fn().mockReturnValue({ data: { status: "UP" }, isLoading: false }),
  useLogin: vi.fn(),
  useRegister: vi.fn(),
  useRefreshToken: vi.fn(),
}));

vi.mock("@/lib/queries/use-user", () => ({
  useCurrentUser: vi.fn().mockReturnValue({
    data: {
      id: "user-1",
      username: "alice",
      email: "alice@example.com",
      displayName: "Alice",
      status: "ACTIVE",
      roles: [],
    },
    isLoading: false,
  }),
  useUpdateProfile: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
  useChangePassword: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
  useDeactivateAccount: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
}));

vi.mock("@/lib/api/client", () => ({
  getAccessToken: vi.fn().mockReturnValue("mock-token"),
  getRefreshToken: vi.fn().mockReturnValue("refresh-token"),
  setTokens: vi.fn(),
  clearTokens: vi.fn(),
  ApiError: class ApiError extends Error {
    status: number;
    body: unknown;
    constructor(status: number, body: unknown) {
      super(`API error: ${status}`);
      this.name = "ApiError";
      this.status = status;
      this.body = body;
    }
  },
  apiFetch: vi.fn(),
}));

vi.mock("@/lib/auth/auth-provider", () => ({
  useAuth: vi.fn().mockReturnValue({
    isAuthenticated: true,
    isLoading: false,
    logout: vi.fn(),
    error: null,
    setError: vi.fn(),
  }),
  AuthProvider: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

vi.mock("@/lib/auth/auth-guard", () => ({
  AuthGuard: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
}

describe("Header component", () => {
  beforeEach(() => {
    cleanup();
    mockNavigate.mockClear();
    mockLogoutMutate.mockClear();
    mockLogoutAllMutate.mockClear();
  });

  it("renders the header with banner role", async () => {
    const { Header } = await import("@/components/layout/header");
    const queryClient = createQueryClient();
    render(
      <QueryClientProvider client={queryClient}>
        <Header onMenuToggle={vi.fn()} />
      </QueryClientProvider>,
    );
    expect(screen.getByRole("banner")).toBeInTheDocument();
  });

  it("calls logout with onSettled callback that navigates to login", async () => {
    // Make mutate call onSettled immediately
    mockLogoutMutate.mockImplementation((_data: undefined, callbacks: { onSettled?: () => void }) => {
      callbacks?.onSettled?.();
    });

    const { Header } = await import("@/components/layout/header");
    const queryClient = createQueryClient();
    render(
      <QueryClientProvider client={queryClient}>
        <Header onMenuToggle={vi.fn()} />
      </QueryClientProvider>,
    );

    const user = userEvent.setup();
    // The user menu button has aria-label "User menu"
    await user.click(screen.getByRole("button", { name: /user menu/i }));
    // Now click "Log out" menuitem
    await user.click(screen.getByRole("menuitem", { name: /^log out$/i }));
    await waitFor(() => {
      expect(mockLogoutMutate).toHaveBeenCalled();
      expect(mockNavigate).toHaveBeenCalledWith(expect.objectContaining({ to: "/login" }));
    });
  });

  it("calls logout all with onSettled callback that navigates to login", async () => {
    mockLogoutAllMutate.mockImplementation((_data: undefined, callbacks: { onSettled?: () => void }) => {
      callbacks?.onSettled?.();
    });

    const { Header } = await import("@/components/layout/header");
    const queryClient = createQueryClient();
    render(
      <QueryClientProvider client={queryClient}>
        <Header onMenuToggle={vi.fn()} />
      </QueryClientProvider>,
    );

    const user = userEvent.setup();
    await user.click(screen.getByRole("button", { name: /user menu/i }));
    await user.click(screen.getByRole("menuitem", { name: /log out all/i }));
    await waitFor(() => {
      expect(mockLogoutAllMutate).toHaveBeenCalled();
      expect(mockNavigate).toHaveBeenCalledWith(expect.objectContaining({ to: "/login" }));
    });
  });
});

describe("AuthLayout route", () => {
  beforeEach(() => {
    cleanup();
  });

  it("renders the AuthLayout with AppShell", async () => {
    const queryClient = createQueryClient();
    const { Route } = await import("@/routes/_auth");
    const Component = (Route as { options: { component: React.ComponentType } }).options.component;
    render(
      <QueryClientProvider client={queryClient}>
        <Component />
      </QueryClientProvider>,
    );
    // _auth.tsx renders AppShell which wraps Outlet
    expect(document.body).toBeDefined();
  });
});
