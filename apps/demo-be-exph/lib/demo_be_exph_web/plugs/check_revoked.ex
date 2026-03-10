defmodule DemoBeExphWeb.Plugs.CheckRevoked do
  @moduledoc """
  Plug that rejects requests carrying a blacklisted access token.
  Must be placed after Guardian.Plug.EnsureAuthenticated.
  """

  import Plug.Conn

  alias DemoBeExph.Token.TokenContext
  alias Guardian.Plug, as: GuardianPlug

  def init(opts), do: opts

  def call(conn, _opts) do
    claims = GuardianPlug.current_claims(conn)
    jti = claims && Map.get(claims, "jti")

    if jti && TokenContext.revoked?(jti) do
      conn
      |> put_resp_content_type("application/json")
      |> send_resp(401, Jason.encode!(%{error: "Token has been revoked"}))
      |> halt()
    else
      if claims do
        user_id = claims |> Map.get("sub") |> String.to_integer()
        assign(conn, :current_user_id, user_id)
      else
        conn
      end
    end
  end
end
