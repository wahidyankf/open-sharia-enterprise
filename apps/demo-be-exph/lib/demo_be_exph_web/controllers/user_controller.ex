defmodule DemoBeExphWeb.UserController do
  use DemoBeExphWeb, :controller

  alias DemoBeExph.Accounts
  alias Guardian.Plug, as: GuardianPlug

  def me(conn, _params) do
    user = GuardianPlug.current_resource(conn)

    json(conn, %{
      id: user.id,
      username: user.username,
      email: user.email,
      display_name: user.display_name || user.username,
      role: user.role,
      status: user.status
    })
  end

  def update_me(conn, params) do
    user = GuardianPlug.current_resource(conn)

    case Accounts.update_user(user, params) do
      {:ok, updated_user} ->
        json(conn, %{
          id: updated_user.id,
          username: updated_user.username,
          email: updated_user.email,
          display_name: updated_user.display_name || updated_user.username
        })

      {:error, changeset} ->
        conn
        |> put_status(:bad_request)
        |> json(%{errors: format_errors(changeset)})
    end
  end

  def change_password(conn, params) do
    user = GuardianPlug.current_resource(conn)
    old_password = Map.get(params, "old_password", "")
    new_password = Map.get(params, "new_password", "")

    case Accounts.change_password(user, old_password, new_password) do
      {:ok, _user} ->
        json(conn, %{message: "Password changed successfully"})

      {:error, :invalid_credentials} ->
        conn
        |> put_status(:unauthorized)
        |> json(%{message: "Invalid credentials"})

      {:error, changeset} ->
        conn
        |> put_status(:bad_request)
        |> json(%{errors: format_errors(changeset)})
    end
  end

  def deactivate(conn, _params) do
    user = GuardianPlug.current_resource(conn)

    case Accounts.deactivate_user(user) do
      {:ok, _user} ->
        json(conn, %{message: "Account deactivated successfully"})

      {:error, changeset} ->
        conn
        |> put_status(:bad_request)
        |> json(%{errors: format_errors(changeset)})
    end
  end

  defp format_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, _opts} -> msg end)
  end
end
