"use client";

import { Suspense, useState, useEffect } from "react";
import { useRouter, useSearchParams } from "next/navigation";
import { useLogin } from "@/lib/queries/use-auth";
import { useAuth } from "@/lib/auth/auth-provider";
import { ApiError } from "@/lib/api/client";

function LoginContent() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { isAuthenticated } = useAuth();
  const loginMutation = useLogin();

  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [fieldErrors, setFieldErrors] = useState<{
    username?: string;
    password?: string;
  }>({});
  const [successMessage] = useState<string | null>(
    searchParams.get("registered") === "true" ? "Registration successful. Please log in." : null,
  );

  useEffect(() => {
    if (isAuthenticated) {
      router.push("/expenses");
    }
  }, [isAuthenticated, router]);

  const validate = (): boolean => {
    const errors: { username?: string; password?: string } = {};
    if (!username.trim()) errors.username = "Username is required";
    if (!password) errors.password = "Password is required";
    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (!validate()) return;

    loginMutation.mutate(
      { username, password },
      {
        onSuccess: () => {
          router.push("/expenses");
        },
      },
    );
  };

  const getErrorMessage = (): string | null => {
    if (!loginMutation.isError) return null;
    const err = loginMutation.error;
    if (err instanceof ApiError) {
      if (err.status === 401) {
        const msg = (err.body as { message?: string } | null)?.message ?? "";
        if (msg.toLowerCase().includes("locked")) return "Your account is locked. Please contact an administrator.";
        return "Invalid username or password.";
      }
      if (err.status === 403) return "Your account is deactivated or disabled.";
    }
    return "Login failed. Please try again.";
  };

  const errorMessage = getErrorMessage();

  return (
    <main className="mx-auto mt-16 max-w-[28rem] p-8">
      <h1 className="mb-6">Log In</h1>

      {successMessage && (
        <div role="status" className="mb-4 rounded border border-green-300 bg-green-50 px-4 py-3 text-green-700">
          {successMessage}
        </div>
      )}

      {errorMessage && (
        <div
          id="login-error"
          role="alert"
          className="mb-4 rounded border border-red-300 bg-red-50 px-4 py-3 text-red-700"
        >
          {errorMessage}
        </div>
      )}

      <form
        onSubmit={handleSubmit}
        noValidate
        aria-describedby={errorMessage ? "login-error" : undefined}
        className="rounded-lg border border-gray-300 bg-white p-8 shadow-md"
      >
        <div className="mb-5">
          <label htmlFor="username" className="mb-1.5 block font-semibold">
            Username
          </label>
          <input
            id="username"
            type="text"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            autoComplete="username"
            aria-required="true"
            aria-describedby={fieldErrors.username ? "username-error" : undefined}
            aria-invalid={!!fieldErrors.username}
            className={`box-border w-full rounded border px-3 py-3 text-base ${fieldErrors.username ? "border-red-700" : "border-gray-400"}`}
          />
          {fieldErrors.username && (
            <span id="username-error" role="alert" className="mt-1 block text-sm text-red-700">
              {fieldErrors.username}
            </span>
          )}
        </div>

        <div className="mb-6">
          <label htmlFor="password" className="mb-1.5 block font-semibold">
            Password
          </label>
          <input
            id="password"
            type="password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            autoComplete="current-password"
            aria-required="true"
            aria-describedby={fieldErrors.password ? "password-error" : undefined}
            aria-invalid={!!fieldErrors.password}
            className={`box-border w-full rounded border px-3 py-2.5 text-base ${fieldErrors.password ? "border-red-700" : "border-gray-400"}`}
          />
          {fieldErrors.password && (
            <span id="password-error" role="alert" className="mt-1 block text-sm text-red-700">
              {fieldErrors.password}
            </span>
          )}
        </div>

        <button
          type="submit"
          disabled={loginMutation.isPending}
          className={`w-full rounded border-none bg-blue-600 py-3 text-base font-semibold text-white ${loginMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"}`}
        >
          {loginMutation.isPending ? "Logging in..." : "Log In"}
        </button>
      </form>

      <p className="mt-4 text-center text-gray-500">
        Don&apos;t have an account?{" "}
        <a href="/register" className="text-blue-600">
          Register
        </a>
      </p>
    </main>
  );
}

export default function LoginPage() {
  return (
    <Suspense fallback={<div className="mx-auto mt-16 max-w-[28rem] p-8">Loading...</div>}>
      <LoginContent />
    </Suspense>
  );
}
