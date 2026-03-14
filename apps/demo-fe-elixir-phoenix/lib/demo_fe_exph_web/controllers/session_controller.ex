defmodule DemoFeExphWeb.SessionController do
  @moduledoc "Sets session tokens after a successful LiveView login."

  use DemoFeExphWeb, :controller

  @max_age 60

  @doc """
  Receives a signed token (set by LoginLive) that contains the access and
  refresh tokens, stores them in the Plug session, and redirects to the home
  page.  The signed token expires after #{@max_age} seconds so it cannot be
  replayed.
  """
  def create(conn, %{"token" => signed}) do
    secret = Application.get_env(:demo_fe_exph, DemoFeExphWeb.Endpoint)[:secret_key_base]

    case Phoenix.Token.verify(%{secret_key_base: secret}, "session_tokens", signed,
           max_age: @max_age
         ) do
      {:ok, %{"access_token" => access_token, "refresh_token" => refresh_token}} ->
        conn
        |> put_session(:access_token, access_token)
        |> put_session(:refresh_token, refresh_token)
        |> redirect(to: "/")

      _error ->
        redirect(conn, to: "/login")
    end
  end

  def create(conn, _params) do
    redirect(conn, to: "/login")
  end
end
