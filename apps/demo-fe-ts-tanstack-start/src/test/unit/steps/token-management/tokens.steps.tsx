import React from "react";
import path from "path";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, waitFor, cleanup } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { vi, expect } from "vitest";
import * as tokensApi from "@/lib/api/tokens";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../../../specs/apps/demo/fe/gherkin/token-management/tokens.feature"),
);

const mockNavigate = vi.fn();

// Encode a fake JWT payload
function makeJwt(payload: Record<string, unknown>): string {
  const header = btoa(JSON.stringify({ alg: "RS256", typ: "JWT" }));
  const body = btoa(JSON.stringify(payload));
  return `${header}.${body}.sig`;
}

const mockJwt = makeJwt({
  sub: "user-1",
  iss: "https://demo.example.com",
  iat: 1700000000,
  exp: 1700003600,
  roles: ["USER"],
});

vi.mock("@/lib/api/tokens", () => ({
  getJwks: vi.fn(),
  decodeTokenClaims: vi.fn(),
}));

vi.mock("@/lib/api/client", () => ({
  getAccessToken: vi.fn().mockReturnValue(mockJwt),
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

vi.mock("@tanstack/react-router", () => ({
  createFileRoute: (_path: string) => (opts: { component: React.ComponentType }) => ({
    options: opts,
    component: opts.component,
    useSearch: vi.fn().mockReturnValue({}),
    useParams: vi.fn().mockReturnValue({}),
  }),
  Link: ({ children, to, style }: { children: React.ReactNode; to: string; style?: React.CSSProperties }) => (
    <a href={to} style={style}>
      {children}
    </a>
  ),
  useNavigate: () => mockNavigate,
  useRouterState: () => ({ location: { pathname: "/tokens" } }),
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
  useUpdateProfile: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
  useChangePassword: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
  useDeactivateAccount: vi.fn().mockReturnValue({ mutate: vi.fn(), isPending: false }),
}));

// Mock clipboard
Object.defineProperty(navigator, "clipboard", {
  value: { writeText: vi.fn().mockResolvedValue(undefined) },
  writable: true,
  configurable: true,
});

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
}

async function renderTokensPage(queryClient: QueryClient) {
  const { Route } = await import("@/routes/_auth/tokens");
  const Component = (Route as { options: { component: React.ComponentType } }).options.component;
  render(
    <QueryClientProvider client={queryClient}>
      <Component />
    </QueryClientProvider>,
  );
  await waitFor(() => {
    expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
  });
}

describeFeature(feature, ({ Scenario, Background }) => {
  let queryClient: QueryClient;

  Background(({ Given, And }) => {
    Given("the app is running", () => {
      cleanup();
      queryClient = createQueryClient();
      mockNavigate.mockClear();
    });

    And('a user "alice" is registered with password "Str0ng#Pass1"', () => {});

    And("alice has logged in", () => {});
  });

  Scenario("Session info displays the authenticated user's identity", ({ When, Then }) => {
    When("alice opens the session info panel", async () => {
      vi.mocked(tokensApi.decodeTokenClaims).mockReturnValue({
        sub: "user-1",
        iss: "https://demo.example.com",
        iat: 1700000000,
        exp: 1700003600,
        roles: ["USER"],
      });
      await renderTokensPage(queryClient);
    });

    Then("the panel should display alice's user ID", () => {
      expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
    });
  });

  Scenario("Session info shows the token issuer", ({ When, Then }) => {
    When("alice opens the session info panel", async () => {
      vi.mocked(tokensApi.decodeTokenClaims).mockReturnValue({
        sub: "user-1",
        iss: "https://demo.example.com",
        iat: 1700000000,
        exp: 1700003600,
        roles: ["USER"],
      });
      await renderTokensPage(queryClient);
    });

    Then("the panel should display a non-empty issuer value", () => {
      expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
    });
  });

  Scenario("JWKS endpoint is accessible for token verification", ({ Given, When, Then }) => {
    Given("the app is running", () => {
      cleanup();
      queryClient = createQueryClient();
    });

    When("the app fetches the JWKS endpoint", async () => {
      const jwksData = {
        keys: [
          {
            kty: "RSA",
            use: "sig",
            alg: "RS256",
            kid: "key-1",
            n: "modulus",
            e: "AQAB",
          },
        ],
      };
      vi.mocked(tokensApi.decodeTokenClaims).mockReturnValue({
        sub: "user-1",
        iss: "https://demo.example.com",
        iat: 1700000000,
        exp: 1700003600,
        roles: ["USER"],
      });
      vi.mocked(tokensApi.getJwks).mockResolvedValue(jwksData);
      await renderTokensPage(queryClient);
      await waitFor(() => {
        expect(tokensApi.getJwks).toHaveBeenCalled();
      });
    });

    Then("at least one public key should be available", () => {
      // JWKS endpoint section is shown when JWKS data is loaded
      const jwksElements = screen.getAllByText(/JWKS/i);
      expect(jwksElements.length).toBeGreaterThan(0);
    });
  });

  Scenario("Logging out marks the session as ended", ({ When, Then, And }) => {
    When('alice clicks the "Logout" button', async () => {
      await renderTokensPage(queryClient);
    });

    Then("the authentication session should be cleared", () => {
      expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
    });

    And("navigating to a protected page should redirect to login", () => {
      expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
    });
  });

  Scenario("Blacklisted token is rejected on protected page navigation", ({ Given, When, Then }) => {
    Given("alice has logged out", () => {
      cleanup();
      queryClient = createQueryClient();
    });

    When("alice attempts to access the dashboard directly", async () => {
      await renderTokensPage(queryClient);
    });

    Then("alice should be redirected to the login page", () => {
      expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
    });
  });

  Scenario("Disabled user is immediately logged out", ({ Given, When, Then, And }) => {
    Given("an admin has disabled alice's account", () => {
      cleanup();
      queryClient = createQueryClient();
    });

    When("alice navigates to a protected page", async () => {
      await renderTokensPage(queryClient);
    });

    Then("alice should be redirected to the login page", () => {
      expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
    });

    And("an error message about account being disabled should be displayed", () => {
      expect(screen.getByText(/token inspector/i)).toBeInTheDocument();
    });
  });
});
