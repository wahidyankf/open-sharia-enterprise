"use client";

import { AppShell } from "@/components/layout/app-shell";
import { useTokenClaims, useJwks } from "@/lib/queries/use-tokens";

const cardClassName = "bg-white p-6 rounded-lg border border-gray-300 shadow-md mb-6";

function formatTimestamp(ts: unknown): string {
  if (typeof ts !== "number") return "—";
  return new Date(ts * 1000).toLocaleString();
}

export default function TokensPage() {
  const { data: claims, isLoading: claimsLoading, isError: claimsError } = useTokenClaims();
  const { data: jwks, isLoading: jwksLoading, isError: jwksError } = useJwks();

  return (
    <AppShell>
      <h1 className="mb-6">Token Inspector</h1>

      <div className={cardClassName}>
        <h2 className="mt-0">Access Token Claims</h2>

        {claimsLoading && <p>Decoding token...</p>}
        {claimsError && (
          <p role="alert" className="text-red-700">
            Failed to decode token. You may not be logged in.
          </p>
        )}

        {claims && (
          <dl className="m-0">
            {[
              ["Subject (User ID)", claims["sub"]],
              ["Issuer", claims["iss"]],
              ["Issued At", formatTimestamp(claims["iat"])],
              ["Expires At", formatTimestamp(claims["exp"])],
              ["Roles", Array.isArray(claims["roles"]) ? (claims["roles"] as string[]).join(", ") : "—"],
            ].map(([label, value]) => (
              <div key={String(label)} className="mb-3 flex gap-4 border-b border-gray-100 pb-3">
                <dt className="min-w-[10rem] text-[0.9rem] font-semibold text-gray-600">{String(label)}</dt>
                <dd
                  data-testid={String(label) === "Subject (User ID)" ? "token-subject" : undefined}
                  className="m-0 font-mono text-[0.9rem] break-all"
                >
                  {String(value ?? "—")}
                </dd>
              </div>
            ))}
          </dl>
        )}

        {claims && (
          <details className="mt-4">
            <summary className="cursor-pointer font-semibold text-blue-600">Raw Claims (JSON)</summary>
            <pre className="mt-3 overflow-x-auto rounded bg-gray-50 p-4 text-[0.8rem]">
              {JSON.stringify(claims, null, 2)}
            </pre>
          </details>
        )}
      </div>

      <div className={cardClassName}>
        <h2 className="mt-0">JWKS Endpoint</h2>

        {jwksLoading && <p>Loading JWKS...</p>}
        {jwksError && (
          <p role="alert" className="text-red-700">
            Failed to load JWKS.
          </p>
        )}

        {jwks && (
          <>
            <p className="mt-0">
              <strong>Key count:</strong>{" "}
              <span className="rounded-full bg-blue-600 px-[0.6rem] py-[0.1rem] font-semibold text-white">
                {jwks.keys.length}
              </span>
            </p>

            <p className="text-[0.9rem] text-gray-500">
              JWKS endpoint: <code>/.well-known/jwks.json</code>
            </p>

            <ul className="m-0 list-none p-0">
              {jwks.keys.map((key) => (
                <li key={key.kid} className="mb-3 rounded-md border border-gray-200 bg-gray-50 p-4">
                  <dl className="m-0">
                    {[
                      ["Key ID (kid)", key.kid],
                      ["Key Type (kty)", key.kty],
                      ["Use", key.use],
                    ].map(([label, value]) => (
                      <div key={String(label)} className="mb-[0.4rem] flex gap-4">
                        <dt className="min-w-[9rem] text-[0.85rem] font-semibold text-gray-600">{String(label)}</dt>
                        <dd className="m-0 font-mono text-[0.85rem]">{String(value)}</dd>
                      </div>
                    ))}
                  </dl>
                </li>
              ))}
            </ul>
          </>
        )}
      </div>
    </AppShell>
  );
}
