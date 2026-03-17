import { createFileRoute, Link } from "@tanstack/react-router";
import { useHealth } from "../lib/queries/use-auth";

export const Route = createFileRoute("/")({
  component: HomePage,
});

function HomePage() {
  const { data, isLoading, isError } = useHealth();

  return (
    <main
      style={{
        maxWidth: "40rem",
        margin: "4rem auto",
        padding: "2rem",
        textAlign: "center",
      }}
    >
      <h1 style={{ marginBottom: "1.5rem" }}>Demo Frontend</h1>

      <div
        style={{
          border: "1px solid #ddd",
          borderRadius: "8px",
          padding: "2rem",
          backgroundColor: "#ffffff",
          boxShadow: "0 2px 8px rgba(0,0,0,0.08)",
        }}
      >
        <h2 style={{ marginTop: 0, marginBottom: "1rem" }}>Backend Status</h2>

        {isLoading && <p style={{ color: "#666" }}>Checking backend status...</p>}

        {isError && (
          <p
            role="alert"
            style={{
              color: "#c0392b",
              backgroundColor: "#fdf2f2",
              padding: "0.75rem",
              borderRadius: "4px",
            }}
          >
            Backend unavailable
          </p>
        )}

        {data && (
          <div style={{ display: "flex", alignItems: "center", gap: "0.5rem", justifyContent: "center" }}>
            <span
              aria-hidden="true"
              style={{
                width: "0.75rem",
                height: "0.75rem",
                borderRadius: "50%",
                backgroundColor: data.status === "UP" ? "#2d7a2d" : "#c0392b",
                display: "inline-block",
              }}
            />
            <span
              style={{
                fontWeight: "bold",
                color: data.status === "UP" ? "#2d7a2d" : "#c0392b",
              }}
            >
              {data.status}
            </span>
          </div>
        )}
      </div>

      <p style={{ marginTop: "2rem", color: "#666" }}>
        <Link to="/login" search={{ registered: undefined }} style={{ color: "#1558c0" }}>
          Log in
        </Link>{" "}
        to access the full dashboard.
      </p>
    </main>
  );
}
