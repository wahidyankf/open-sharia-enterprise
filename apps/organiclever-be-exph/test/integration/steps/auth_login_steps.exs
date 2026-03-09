defmodule OrganicleverBeExphWeb.Integration.AuthLoginSteps do
  use Cabbage.Feature, async: false, file: "auth/login.feature"

  use OrganicleverBeExphWeb.ConnCase

  import Mox

  alias OrganicleverBeExph.Accounts.User
  alias OrganicleverBeExph.MockAccounts

  @moduletag :integration

  # Note: Cabbage does not execute Gherkin Background steps automatically.
  # The background (alice registered with "s3cur3Pass!") is represented here
  # as module-level test data. The Given steps for background are defined but
  # won't run from the Background section.
  @test_user %User{id: 1, username: "alice"}
  @test_password "s3cur3Pass!"

  setup :set_mox_global

  defgiven ~r/^the OrganicLever API is running$/, _vars, state do
    {:ok, state}
  end

  defgiven ~r/^a user "(?<username>[^"]+)" is already registered with password "(?<password>[^"]+)"$/,
           %{username: _username, password: _password},
           state do
    {:ok, state}
  end

  defwhen ~r/^a client sends POST \/api\/v1\/auth\/login with body:$/,
          %{doc_string: body},
          state do
    params = Jason.decode!(body)
    username = Map.get(params, "username", "")
    password = Map.get(params, "password", "")

    # Only set up Mox expectation when both fields are non-empty.
    # The controller validates blank fields before calling authenticate_user.
    unless username == "" or password == "" do
      valid_login? =
        username == @test_user.username and password == @test_password

      if valid_login? do
        expect(MockAccounts, :authenticate_user, fn _u, _p -> {:ok, @test_user} end)
      else
        expect(MockAccounts, :authenticate_user, fn _u, _p -> {:error, :invalid_credentials} end)
      end
    end

    conn =
      build_conn()
      |> put_req_header("content-type", "application/json")
      |> post("/api/v1/auth/login", body)

    {:ok, Map.put(state, :conn, conn)}
  end

  defthen ~r/^the response status code should be (?<code>\d+)$/,
          %{code: code},
          %{conn: conn} = state do
    assert conn.status == String.to_integer(code)
    {:ok, state}
  end

  defthen ~r/^the response body should contain a "(?<field>[^"]+)" field$/,
          %{field: field},
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert Map.has_key?(body, field)
    {:ok, state}
  end

  defthen ~r/^the response body should contain "(?<field>[^"]+)" equal to "(?<value>[^"]+)"$/,
          %{field: field, value: value},
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert body[field] == value
    {:ok, state}
  end

  defthen ~r/^the response body should contain an error message about invalid credentials$/,
          _vars,
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert body["error"] =~ ~r/[Ii]nvalid|[Cc]redential/i
    {:ok, state}
  end

  defthen ~r/^the response body should contain a validation error for "(?<field>[^"]+)"$/,
          %{field: field},
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert Map.has_key?(body, "errors")
    errors = body["errors"]
    assert Map.has_key?(errors, field)
    assert errors[field] != []
    {:ok, state}
  end
end
