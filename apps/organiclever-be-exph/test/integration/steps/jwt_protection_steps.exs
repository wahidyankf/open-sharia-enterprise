defmodule OrganicleverBeExphWeb.Integration.JwtProtectionSteps do
  use Cabbage.Feature, async: false, file: "auth/jwt-protection.feature"

  use OrganicleverBeExphWeb.ConnCase

  import Mox

  alias OrganicleverBeExph.Accounts.User
  alias OrganicleverBeExph.Auth.Guardian
  alias OrganicleverBeExph.Integration.Helpers
  alias OrganicleverBeExph.MockAccounts

  @moduletag :integration

  setup :set_mox_global

  defgiven ~r/^the OrganicLever API is running$/, _vars, state do
    {:ok, state}
  end

  defgiven ~r/^a user "(?<username>[^"]+)" is already registered with password "(?<password>[^"]+)"$/,
           %{username: username, password: _password},
           state do
    registered_user = %User{id: 99, username: username}
    {:ok, Map.put(state, :registered_user, registered_user)}
  end

  defgiven ~r/^the client has logged in as "(?<username>[^"]+)" and stored the JWT token$/,
           %{username: _username},
           %{registered_user: registered_user} = state do
    token = Helpers.generate_token(registered_user.id)
    {:ok, Map.put(state, :token, token)}
  end

  defwhen ~r/^a client sends GET \/api\/v1\/hello without an Authorization header$/,
          _vars,
          state do
    conn = get(build_conn(), "/api/v1/hello")
    {:ok, Map.put(state, :conn, conn)}
  end

  defwhen ~r/^a client sends GET \/api\/v1\/hello with the stored Bearer token$/,
          _vars,
          %{token: token} = state do
    conn =
      build_conn()
      |> put_req_header("authorization", "Bearer #{token}")
      |> get("/api/v1/hello")

    {:ok, Map.put(state, :conn, conn)}
  end

  defwhen ~r/^a client sends GET \/api\/v1\/hello with an expired Bearer token$/, _vars, state do
    expired_token = build_expired_token()

    conn =
      build_conn()
      |> put_req_header("authorization", "Bearer #{expired_token}")
      |> get("/api/v1/hello")

    {:ok, Map.put(state, :conn, conn)}
  end

  defwhen ~r/^a client sends GET \/api\/v1\/hello with Authorization header "(?<header_value>[^"]+)"$/,
          %{header_value: header_value},
          state do
    conn =
      build_conn()
      |> put_req_header("authorization", header_value)
      |> get("/api/v1/hello")

    {:ok, Map.put(state, :conn, conn)}
  end

  defwhen ~r/^a client sends GET \/health$/, _vars, state do
    conn = get(build_conn(), "/health")
    {:ok, Map.put(state, :conn, conn)}
  end

  defwhen ~r/^a client sends POST \/api\/v1\/auth\/register with body:$/,
          %{doc_string: body},
          state do
    params = Jason.decode!(body)

    expect(MockAccounts, :register_user, fn _attrs ->
      {:ok, %User{id: 1, username: params["username"]}}
    end)

    conn =
      build_conn()
      |> put_req_header("content-type", "application/json")
      |> post("/api/v1/auth/register", body)

    {:ok, Map.put(state, :conn, conn)}
  end

  defthen ~r/^the response status code should be (?<code>\d+)$/,
          %{code: code},
          %{conn: conn} = state do
    assert conn.status == String.to_integer(code)
    {:ok, state}
  end

  defp build_expired_token do
    case Guardian.encode_and_sign(%{id: 1}, %{}, ttl: {-1, :second}) do
      {:ok, token, _} ->
        token

      _ ->
        "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJleHAiOjF9.invalid"
    end
  end
end
