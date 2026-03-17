import path from "path";
import React from "react";
import { loadFeature, describeFeature } from "@amiceli/vitest-cucumber";
import { render, screen, waitFor, cleanup } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { vi, expect } from "vitest";
import * as authApi from "@/lib/api/auth";
import { Route as HomeRoute } from "@/routes/index";

const feature = await loadFeature(
  path.resolve(__dirname, "../../../../../../../specs/apps/demo/fe/gherkin/health/health-status.feature"),
);

vi.mock("@/lib/api/auth", () => ({
  getHealth: vi.fn(),
  login: vi.fn(),
  register: vi.fn(),
  refreshToken: vi.fn(),
  logout: vi.fn(),
  logoutAll: vi.fn(),
}));

vi.mock("@tanstack/react-router", () => ({
  createFileRoute: (_path: string) => (opts: { component: React.ComponentType }) => ({
    options: opts,
    component: opts.component,
  }),
  Link: ({ children, to, style }: { children: React.ReactNode; to: string; style?: React.CSSProperties }) => (
    <a href={to} style={style}>
      {children}
    </a>
  ),
  useNavigate: () => vi.fn(),
  useRouterState: () => ({ location: { pathname: "/" } }),
}));

function createQueryClient() {
  return new QueryClient({
    defaultOptions: { queries: { retry: false }, mutations: { retry: false } },
  });
}

function HomePageWrapper() {
  const Component = HomeRoute.options.component as React.ComponentType;
  return <Component />;
}

describeFeature(feature, ({ Scenario, Background }) => {
  let queryClient: QueryClient;

  Background(({ Given }) => {
    Given("the app is running", () => {
      cleanup();
      queryClient = createQueryClient();
    });
  });

  Scenario("Health indicator shows the service is UP", ({ When, Then }) => {
    When("the user opens the app", async () => {
      vi.mocked(authApi.getHealth).mockResolvedValue({ status: "UP" });
      render(
        <QueryClientProvider client={queryClient}>
          <HomePageWrapper />
        </QueryClientProvider>,
      );
      await waitFor(() => {
        expect(screen.getByText("UP")).toBeInTheDocument();
      });
    });

    Then('the health status indicator should display "UP"', () => {
      expect(screen.getByText("UP")).toBeInTheDocument();
    });
  });

  Scenario("Health indicator does not expose component details to regular users", ({ When, Then, And }) => {
    When("an unauthenticated user opens the app", async () => {
      vi.mocked(authApi.getHealth).mockResolvedValue({ status: "UP" });
      render(
        <QueryClientProvider client={queryClient}>
          <HomePageWrapper />
        </QueryClientProvider>,
      );
      await waitFor(() => {
        expect(screen.getByText("UP")).toBeInTheDocument();
      });
    });

    Then('the health status indicator should display "UP"', () => {
      expect(screen.getByText("UP")).toBeInTheDocument();
    });

    And("no detailed component health information should be visible", () => {
      expect(screen.queryByText(/components/i)).not.toBeInTheDocument();
      expect(screen.queryByText(/details/i)).not.toBeInTheDocument();
    });
  });
});
