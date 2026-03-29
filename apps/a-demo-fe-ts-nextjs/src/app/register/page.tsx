"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";
import { useRegister } from "@/lib/queries/use-auth";
import { ApiError } from "@/lib/api/client";

function validatePassword(password: string): string[] {
  const errors: string[] = [];
  if (password.length < 12) errors.push("At least 12 characters");
  if (!/[A-Z]/.test(password)) errors.push("At least one uppercase letter");
  if (!/[^a-zA-Z0-9]/.test(password)) errors.push("At least one special character");
  return errors;
}

export default function RegisterPage() {
  const router = useRouter();
  const registerMutation = useRegister();

  const [username, setUsername] = useState("");
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [fieldErrors, setFieldErrors] = useState<{
    username?: string;
    email?: string;
    password?: string;
  }>({});

  const validate = (): boolean => {
    const errors: { username?: string; email?: string; password?: string } = {};
    if (!username.trim()) errors.username = "Username is required";
    if (!email.trim()) {
      errors.email = "Email is required";
    } else if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) {
      errors.email = "Enter a valid email address";
    }
    const pwErrors = validatePassword(password);
    if (pwErrors.length > 0) {
      errors.password = "Password must meet the following requirements: " + pwErrors.join(", ");
    }
    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (!validate()) return;

    registerMutation.mutate(
      { username, email, password },
      {
        onSuccess: () => {
          router.push("/login?registered=true");
        },
      },
    );
  };

  const getErrorMessage = (): string | null => {
    if (!registerMutation.isError) return null;
    const err = registerMutation.error;
    if (err instanceof ApiError) {
      if (err.status === 409) return "Username or email already exists.";
      if (err.status === 400) return "Invalid registration data. Check your inputs.";
    }
    return "Registration failed. Please try again.";
  };

  const errorMessage = getErrorMessage();
  const passwordErrors = password ? validatePassword(password) : [];

  return (
    <main className="mx-auto mt-16 max-w-[28rem] p-8">
      <h1 className="mb-6">Create Account</h1>

      {errorMessage && (
        <div
          id="register-error"
          role="alert"
          className="mb-4 rounded border border-red-200 bg-red-50 px-4 py-3 text-red-700"
        >
          {errorMessage}
        </div>
      )}

      <form
        onSubmit={handleSubmit}
        noValidate
        aria-describedby={errorMessage ? "register-error" : undefined}
        className="rounded-lg border border-gray-300 bg-white p-8 shadow-md"
      >
        <div className="mb-5">
          <label htmlFor="username" className="mb-[0.4rem] block font-semibold">
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
            className={`box-border w-full rounded px-3 py-[0.6rem] text-base ${
              fieldErrors.username ? "border border-red-700" : "border border-gray-400"
            }`}
          />
          {fieldErrors.username && (
            <span id="username-error" role="alert" className="mt-1 block text-[0.85rem] text-red-700">
              {fieldErrors.username}
            </span>
          )}
        </div>

        <div className="mb-5">
          <label htmlFor="email" className="mb-[0.4rem] block font-semibold">
            Email
          </label>
          <input
            id="email"
            type="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            autoComplete="email"
            aria-required="true"
            aria-describedby={fieldErrors.email ? "email-error" : undefined}
            aria-invalid={!!fieldErrors.email}
            className={`box-border w-full rounded px-3 py-[0.6rem] text-base ${
              fieldErrors.email ? "border border-red-700" : "border border-gray-400"
            }`}
          />
          {fieldErrors.email && (
            <span id="email-error" role="alert" className="mt-1 block text-[0.85rem] text-red-700">
              {fieldErrors.email}
            </span>
          )}
        </div>

        <div className="mb-6">
          <label htmlFor="password" className="mb-[0.4rem] block font-semibold">
            Password
          </label>
          <input
            id="password"
            type="password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            autoComplete="new-password"
            aria-required="true"
            aria-describedby={fieldErrors.password ? "password-error" : "password-hint"}
            aria-invalid={!!fieldErrors.password}
            className={`box-border w-full rounded px-3 py-[0.6rem] text-base ${
              fieldErrors.password ? "border border-red-700" : "border border-gray-400"
            }`}
          />
          {fieldErrors.password ? (
            <span id="password-error" role="alert" className="mt-1 block text-[0.85rem] text-red-700">
              {fieldErrors.password}
            </span>
          ) : (
            <span id="password-hint" className="mt-1 block text-[0.85rem] text-gray-500">
              Min 12 chars, 1 uppercase, 1 special character
            </span>
          )}
          {password.length > 0 && passwordErrors.length > 0 && !fieldErrors.password && (
            <ul aria-live="polite" className="mt-2 pl-5 text-[0.85rem] text-orange-500">
              {passwordErrors.map((e) => (
                <li key={e}>{e}</li>
              ))}
            </ul>
          )}
        </div>

        <button
          type="submit"
          disabled={registerMutation.isPending}
          className={`w-full rounded border-none bg-blue-600 py-3 text-base font-semibold text-white ${
            registerMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"
          }`}
        >
          {registerMutation.isPending ? "Creating account..." : "Create Account"}
        </button>
      </form>

      <p className="mt-4 text-center text-gray-500">
        Already have an account?{" "}
        <a href="/login" className="text-blue-600">
          Log in
        </a>
      </p>
    </main>
  );
}
