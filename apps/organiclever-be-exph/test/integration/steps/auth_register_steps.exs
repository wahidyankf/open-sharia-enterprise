defmodule OrganicleverBeExphWeb.Integration.AuthRegisterSteps do
  use Cabbage.Feature, async: false, file: "auth/register.feature"

  use OrganicleverBeExphWeb.ConnCase

  import Mox

  alias OrganicleverBeExph.Accounts.User
  alias OrganicleverBeExph.Integration.Helpers
  alias OrganicleverBeExph.MockAccounts

  @moduletag :integration

  setup :set_mox_global

  defgiven ~r/^the OrganicLever API is running$/, _vars, state do
    {:ok, state}
  end

  defgiven ~r/^a user "(?<username>[^"]+)" is already registered$/,
           %{username: _username},
           state do
    {:ok, Map.put(state, :alice_registered, true)}
  end

  defwhen ~r/^a client sends POST \/api\/v1\/auth\/register with body:$/,
          %{doc_string: body},
          state do
    params = Jason.decode!(body)

    mock_result =
      if Map.get(state, :alice_registered) do
        {:error, Helpers.unique_username_changeset()}
      else
        case User.changeset(%User{}, params) do
          %{valid?: true} ->
            {:ok, %User{id: 1, username: params["username"]}}

          invalid_changeset ->
            {:error, invalid_changeset}
        end
      end

    expect(MockAccounts, :register_user, fn _attrs -> mock_result end)

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

  defthen ~r/^the response body should contain "(?<field>[^"]+)" equal to "(?<value>[^"]+)"$/,
          %{field: field, value: value},
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert body[field] == value
    {:ok, state}
  end

  defthen ~r/^the response body should not contain a "(?<field>[^"]+)" field$/,
          %{field: field},
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    refute Map.has_key?(body, field)
    {:ok, state}
  end

  defthen ~r/^the response body should contain a non-null "(?<field>[^"]+)" field$/,
          %{field: field},
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert Map.has_key?(body, field)
    assert body[field] != nil
    {:ok, state}
  end

  defthen ~r/^the response body should contain an error message about duplicate username$/,
          _vars,
          %{conn: conn} = state do
    body = Jason.decode!(conn.resp_body)
    assert body["error"] =~ ~r/[Uu]sername.*exist|already|[Dd]uplicate/i
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
