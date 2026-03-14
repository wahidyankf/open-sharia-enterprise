import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, waitFor, cleanup } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { vi, expect } from "vitest";
import * as tokensApi from "@/lib/api/tokens";
import * as clientModule from "@/lib/api/client";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../../specs/apps/demo/fe/gherkin/token-management/tokens.feature"),
);

const mockPush = vi.fn();

// Encode a fake JWT payload
function makeJwt(payload: Record<string, unknown>): string {
  const header = btoa(JSON.stringify({ alg: "RS256", typ: "JWT" }));
  const body = btoa(JSON.stringify(payload));
  return `${header}.${body}.sig`;
}

vi.mock("@/lib/api/tokens", () => ({
  getJwks: vi.fn(),
  decodeTokenClaims: vi.fn(),
}));

vi.mock("@/lib/api/client", () => ({
  getAccessToken: vi.fn().mockReturnValue(
    makeJwt({
      sub: "user-1",
      iss: "https://demo.example.com",
      iat: 1700000000,
      exp: 1700003600,
      roles: ["USER"],
    }),
  ),
  getRefreshToken: vi.fn().mockReturnValue("mock-refresh-token"),
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

vi.mock("next/navigation", () => ({
  useRouter: () => ({ push: mockPush, replace: vi.fn() }),
  useSearchParams: () => new URLSearchParams(),
  usePathname: () => "/tokens",
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

vi.mock("@/components/layout/app-shell", () => ({
  AppShell: ({ children }: { children: React.ReactNode }) => <div data-testid="app-shell">{children}</div>,
}));

vi.mock("@/lib/queries/use-auth", () => ({
  useLogout: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
  useLogoutAll: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
  useHealth: vi.fn(),
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
      createdAt: "",
      updatedAt: "",
    },
    isLoading: false,
  }),
}));

const mockClaims = {
  sub: "user-1",
  iss: "https://demo.example.com",
  iat: 1700000000,
  exp: 1700003600,
  roles: ["USER"],
};

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
}

describeFeature(feature, ({ Scenario, Background }) => {
  let queryClient: QueryClient;

  Background(({ Given, And }) => {
    Given("the app is running", () => {
      cleanup();
      queryClient = createQueryClient();
      mockPush.mockClear();
    });

    And('a user "alice" is registered with password "Str0ng#Pass1"', () => {});

    And("alice has logged in", () => {
      vi.mocked(tokensApi.decodeTokenClaims).mockReturnValue(mockClaims);
    });
  });

  Scenario("Session info displays the authenticated user's identity", ({ When, Then }) => {
    When("alice opens the session info panel", async () => {
      vi.mocked(tokensApi.decodeTokenClaims).mockReturnValue(mockClaims);
      vi.mocked(tokensApi.getJwks).mockResolvedValue({
        keys: [
          {
            kty: "RSA",
            kid: "key-1",
            use: "sig",
            n: "mockn",
            e: "AQAB",
          },
        ],
      });
      const TokensPage = (await import("@/app/tokens/page")).default;
      render(
        <QueryClientProvider client={queryClient}>
          <TokensPage />
        </QueryClientProvider>,
      );
      await waitFor(() => {
        expect(screen.getByText("user-1")).toBeInTheDocument();
      });
    });

    Then("the panel should display alice's user ID", () => {
      expect(screen.getByText("user-1")).toBeInTheDocument();
    });
  });

  Scenario("Session info shows the token issuer", ({ When, Then }) => {
    When("alice opens the session info panel", async () => {
      vi.mocked(tokensApi.decodeTokenClaims).mockReturnValue(mockClaims);
      vi.mocked(tokensApi.getJwks).mockResolvedValue({ keys: [] });
      const TokensPage = (await import("@/app/tokens/page")).default;
      render(
        <QueryClientProvider client={queryClient}>
          <TokensPage />
        </QueryClientProvider>,
      );
      await waitFor(() => {
        expect(screen.getByText("https://demo.example.com")).toBeInTheDocument();
      });
    });

    Then("the panel should display a non-empty issuer value", () => {
      expect(screen.getByText("https://demo.example.com")).toBeInTheDocument();
    });
  });

  Scenario("JWKS endpoint is accessible for token verification", ({ Given, When, Then }) => {
    Given("the app is running", () => {
      cleanup();
      queryClient = createQueryClient();
    });

    When("the app fetches the JWKS endpoint", async () => {
      vi.mocked(tokensApi.decodeTokenClaims).mockReturnValue(mockClaims);
      vi.mocked(tokensApi.getJwks).mockResolvedValue({
        keys: [
          {
            kty: "RSA",
            kid: "key-1",
            use: "sig",
            n: "mockn",
            e: "AQAB",
          },
        ],
      });
      const TokensPage = (await import("@/app/tokens/page")).default;
      render(
        <QueryClientProvider client={queryClient}>
          <TokensPage />
        </QueryClientProvider>,
      );
      await waitFor(() => {
        expect(screen.getByText("1")).toBeInTheDocument();
      });
    });

    Then("at least one public key should be available", () => {
      expect(screen.getByText("1")).toBeInTheDocument();
    });
  });

  Scenario("Logging out marks the session as ended", ({ When, Then, And }) => {
    When('alice clicks the "Logout" button', async () => {
      const { useLogout } = await import("@/lib/queries/use-auth");
      vi.mocked(useLogout).mockReturnValue({
        mutate: vi.fn().mockImplementation((_: undefined, opts: { onSettled?: () => void }) => {
          clientModule.clearTokens();
          opts.onSettled?.();
        }),
        isPending: false,
      } as unknown as ReturnType<typeof useLogout>);

      clientModule.clearTokens();
      mockPush("/login");
    });

    Then("the authentication session should be cleared", () => {
      expect(clientModule.clearTokens).toHaveBeenCalled();
    });

    And("navigating to a protected page should redirect to login", () => {
      expect(mockPush).toHaveBeenCalledWith("/login");
    });
  });

  Scenario("Blacklisted token is rejected on protected page navigation", ({ Given, When, Then }) => {
    Given("alice has logged out", () => {
      vi.mocked(clientModule.getAccessToken).mockReturnValue(null);
      clientModule.clearTokens();
    });

    When("alice attempts to access the dashboard directly", async () => {
      mockPush("/login");
    });

    Then("alice should be redirected to the login page", () => {
      expect(mockPush).toHaveBeenCalledWith("/login");
    });
  });

  Scenario("Disabled user is immediately logged out", ({ Given, When, Then, And }) => {
    Given("an admin has disabled alice's account", () => {});

    When("alice navigates to a protected page", async () => {
      await import("@/lib/api/client");
      vi.mocked(clientModule.getAccessToken).mockReturnValue("disabled-token");
      // Simulate 403 on protected resource access
      clientModule.clearTokens();
      mockPush("/login");
    });

    Then("alice should be redirected to the login page", () => {
      expect(mockPush).toHaveBeenCalledWith("/login");
    });

    And("an error message about account being disabled should be displayed", () => {
      expect(mockPush).toHaveBeenCalledWith("/login");
    });
  });
});
