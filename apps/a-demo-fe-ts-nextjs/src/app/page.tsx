"use client";

import { useHealth } from "@/lib/queries/use-auth";

export default function HomePage() {
  const { data, isLoading, isError } = useHealth();

  return (
    <main className="mx-auto mt-16 max-w-[40rem] p-8 text-center">
      <h1 className="mb-6">Demo Frontend</h1>

      <div className="rounded-lg border border-gray-300 bg-white p-8 shadow-md">
        <h2 className="mt-0 mb-4">Backend Status</h2>

        {isLoading && <p className="text-gray-500">Checking backend status...</p>}

        {isError && (
          <p role="alert" className="rounded bg-red-50 p-3 text-red-700">
            Backend unavailable
          </p>
        )}

        {data && (
          <div className="flex items-center justify-center gap-2">
            <span
              aria-hidden="true"
              className={`inline-block h-3 w-3 rounded-full ${data.status === "UP" ? "bg-green-700" : "bg-red-700"}`}
            />
            <span className={`font-bold ${data.status === "UP" ? "text-green-700" : "text-red-700"}`}>
              {data.status}
            </span>
          </div>
        )}
      </div>

      <p className="mt-8 text-gray-500">
        <a href="/login" className="text-blue-700">
          Log in
        </a>{" "}
        to access the full dashboard.
      </p>
    </main>
  );
}
