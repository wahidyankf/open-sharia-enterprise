defmodule OrganicleverBeExphWeb.Integration.HelloSteps do
  use Cabbage.Feature, async: false, file: "hello/hello-endpoint.feature"

  use OrganicleverBeExphWeb.ConnCase

  import Mox

  alias OrganicleverBeExph.Integration.Helpers

  @moduletag :integration

  # Note: Cabbage does not execute Gherkin Background steps automatically.
  # The background (hellouser registered and logged in) is provided via setup.

  setup :set_mox_global

  setup do
    # Background: the client has logged in as "hellouser" and stored the JWT token.
    # Since Cabbage does not run Background steps, we generate the token in setup.
    token = Helpers.generate_token(42)
    {:ok, token: token}
  end

  defgiven ~r/^the OrganicLever API is running$/, _vars, state do
    {:ok, state}
  end

  defgiven ~r/^a user "(?<username>[^"]+)" is already registered with password "(?<password>[^"]+)"$/,
           %{username: _username, password: _password},
           state do
    {:ok, state}
  end

  defgiven ~r/^the client has logged in as "(?<username>[^"]+)" and stored the JWT token$/,
           %{username: _username},
           state do
    {:ok, state}
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

  defwhen ~r/^a client sends GET \/api\/v1\/hello with the stored Bearer token and Origin header http:\/\/localhost:3200$/,
          _vars,
          %{token: token} = state do
    conn =
      build_conn()
      |> put_req_header("authorization", "Bearer #{token}")
      |> put_req_header("origin", "http://localhost:3200")
      |> get("/api/v1/hello")

    {:ok, Map.put(state, :conn, conn)}
  end

  defthen ~r/^the response status code should be (?<code>\d+)$/,
          %{code: code},
          %{conn: conn} = state do
    assert conn.status == String.to_integer(code)
    {:ok, state}
  end

  defthen ~r/^the response body should be \{"message":"world!"\}$/,
          _vars,
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert body == %{"message" => "world!"}
    {:ok, state}
  end

  defthen ~r/^the response Content-Type should be application\/json$/,
          _vars,
          %{conn: conn} = state do
    content_type = conn |> get_resp_header("content-type") |> List.first()
    assert content_type =~ "application/json"
    {:ok, state}
  end

  defthen ~r/^the response should include an Access-Control-Allow-Origin header permitting the request$/,
          _vars,
          %{conn: conn} = state do
    acao = conn |> get_resp_header("access-control-allow-origin") |> List.first()
    assert acao == "http://localhost:3200"
    {:ok, state}
  end
end
