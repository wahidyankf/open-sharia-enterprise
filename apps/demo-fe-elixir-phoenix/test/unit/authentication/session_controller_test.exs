defmodule DemoFeExphWeb.SessionControllerTest do
  use DemoFeExphWeb.ConnCase

  @moduletag :unit

  defp sign_tokens(access_token, refresh_token) do
    secret =
      Application.get_env(:demo_fe_exph, DemoFeExphWeb.Endpoint)[:secret_key_base]

    Phoenix.Token.sign(
      %{secret_key_base: secret},
      "session_tokens",
      %{"access_token" => access_token, "refresh_token" => refresh_token}
    )
  end

  test "creates session and redirects home with valid token", %{conn: conn} do
    signed = sign_tokens("access-abc", "refresh-xyz")

    conn = get(conn, "/auth/session?token=#{signed}")

    assert redirected_to(conn) == "/"
    assert get_session(conn, :access_token) == "access-abc"
    assert get_session(conn, :refresh_token) == "refresh-xyz"
  end

  test "redirects to login with invalid token", %{conn: conn} do
    conn = get(conn, "/auth/session?token=bad_token")

    assert redirected_to(conn) == "/login"
  end

  test "redirects to login when token param is missing", %{conn: conn} do
    conn = get(conn, "/auth/session")

    assert redirected_to(conn) == "/login"
  end
end
