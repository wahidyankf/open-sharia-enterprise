defmodule DemoBeExphWeb.AdminController do
  use DemoBeExphWeb, :controller

  alias DemoBeExph.Accounts
  alias DemoBeExph.Token.TokenContext
  alias Guardian.Plug, as: GuardianPlug

  def list_users(conn, params) do
    current_user = GuardianPlug.current_resource(conn)

    if current_user.role != "ADMIN" do
      conn
      |> put_status(:forbidden)
      |> json(%{message: "Admin access required"})
    else
      email = Map.get(params, "email")
      raw_page = params |> Map.get("page", "1") |> String.to_integer()
      page = max(raw_page, 1)

      opts = [page: page]
      opts = if email, do: Keyword.put(opts, :email, email), else: opts

      result = Accounts.list_users(opts)

      conn
      |> json(%{
        data: Enum.map(result.data, &user_json/1),
        total: result.total,
        page: result.page
      })
    end
  end

  def disable_user(conn, %{"id" => id} = params) do
    with :ok <- require_admin(conn),
         user when not is_nil(user) <- Accounts.get_user(String.to_integer(id)) do
      _reason = Map.get(params, "reason", "")

      case Accounts.disable_user(user) do
        {:ok, _} ->
          TokenContext.revoke_all_refresh_tokens(user.id)
          json(conn, %{message: "User disabled"})

        {:error, _} ->
          conn |> put_status(:internal_server_error) |> json(%{message: "Failed to disable user"})
      end
    else
      :forbidden ->
        conn |> put_status(:forbidden) |> json(%{message: "Admin access required"})

      nil ->
        conn |> put_status(:not_found) |> json(%{message: "User not found"})
    end
  end

  def enable_user(conn, %{"id" => id}) do
    with :ok <- require_admin(conn),
         user when not is_nil(user) <- Accounts.get_user(String.to_integer(id)) do
      case Accounts.enable_user(user) do
        {:ok, _} -> json(conn, %{message: "User enabled"})
        {:error, _} -> conn |> put_status(:internal_server_error) |> json(%{message: "Failed"})
      end
    else
      :forbidden -> conn |> put_status(:forbidden) |> json(%{message: "Admin access required"})
      nil -> conn |> put_status(:not_found) |> json(%{message: "User not found"})
    end
  end

  def unlock_user(conn, %{"id" => id}) do
    with :ok <- require_admin(conn),
         user when not is_nil(user) <- Accounts.get_user(String.to_integer(id)) do
      case Accounts.unlock_user(user) do
        {:ok, _} -> json(conn, %{message: "User unlocked"})
        {:error, _} -> conn |> put_status(:internal_server_error) |> json(%{message: "Failed"})
      end
    else
      :forbidden -> conn |> put_status(:forbidden) |> json(%{message: "Admin access required"})
      nil -> conn |> put_status(:not_found) |> json(%{message: "User not found"})
    end
  end

  def force_password_reset(conn, %{"id" => id}) do
    with :ok <- require_admin(conn),
         user when not is_nil(user) <- Accounts.get_user(String.to_integer(id)) do
      reset_token = :crypto.strong_rand_bytes(24) |> Base.url_encode64(padding: false)

      json(conn, %{
        message: "Password reset token generated",
        reset_token: reset_token,
        user_id: user.id
      })
    else
      :forbidden -> conn |> put_status(:forbidden) |> json(%{message: "Admin access required"})
      nil -> conn |> put_status(:not_found) |> json(%{message: "User not found"})
    end
  end

  defp require_admin(conn) do
    user = GuardianPlug.current_resource(conn)

    if user.role == "ADMIN" do
      :ok
    else
      :forbidden
    end
  end

  defp user_json(user) do
    %{
      id: user.id,
      username: user.username,
      email: user.email,
      role: user.role,
      status: user.status,
      display_name: user.display_name || user.username
    }
  end
end
