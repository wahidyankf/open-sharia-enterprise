"use client";

import { useHealth } from "@/lib/queries/use-auth";
import { useAuth } from "@/lib/auth/auth-provider";

export default function HomePage() {
  const { data, isLoading, isError } = useHealth();
  const { isAuthenticated } = useAuth();

  return (
    <main className="mx-auto mt-16 max-w-[40rem] p-8 text-center">
      <h1 className="mb-6">Demo Fullstack</h1>

      <div className="rounded-lg border border-gray-300 bg-white p-8 shadow-md">
        <h2 className="mt-0 mb-4">Backend Status</h2>

        {isLoading && <p className="text-gray-500">Checking backend status...</p>}

        {isError && (
          <p role="alert" className="rounded bg-red-50 px-3 py-3 text-red-700">
            Backend unavailable
          </p>
        )}

        {data && (
          <p
            data-testid="health-status"
            className={`m-0 flex items-center justify-center gap-2 font-bold ${data.status === "UP" ? "text-green-700" : "text-red-700"}`}
          >
            <span
              aria-hidden="true"
              className={`inline-block h-3 w-3 rounded-full ${data.status === "UP" ? "bg-green-700" : "bg-red-700"}`}
            />
            {data.status}
          </p>
        )}
      </div>

      {isAuthenticated ? (
        <p className="mt-8 text-gray-500">
          <a href="/expenses" className="text-blue-700">
            Go to Dashboard
          </a>
        </p>
      ) : (
        <p className="mt-8 text-gray-500">
          <a href="/login" className="text-blue-700">
            Log in
          </a>{" "}
          or{" "}
          <a href="/register" className="text-blue-700">
            Register
          </a>{" "}
          to access the full dashboard.
        </p>
      )}
    </main>
  );
}
