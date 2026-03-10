defmodule DemoBeExphWeb.Plugs.CheckUserActive do
  @moduledoc """
  Plug that loads the current user from DB and rejects requests for
  deactivated, disabled, or locked users.
  Must be placed after CheckRevoked.
  """

  import Plug.Conn

  alias DemoBeExph.Accounts
  alias Guardian.Plug, as: GuardianPlug

  def init(opts), do: opts

  def call(conn, _opts) do
    claims = GuardianPlug.current_claims(conn)

    if claims do
      user_id = claims |> Map.get("sub") |> String.to_integer()
      user = Accounts.get_user(user_id)

      cond do
        is_nil(user) ->
          conn
          |> put_resp_content_type("application/json")
          |> send_resp(401, Jason.encode!(%{error: "User not found"}))
          |> halt()

        user.status not in ["ACTIVE"] ->
          conn
          |> put_resp_content_type("application/json")
          |> send_resp(401, Jason.encode!(%{error: "Account is not active"}))
          |> halt()

        true ->
          GuardianPlug.put_current_resource(conn, user)
      end
    else
      conn
    end
  end
end
