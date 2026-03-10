defmodule OrganicleverBeExphWeb.AuthController do
  use OrganicleverBeExphWeb, :controller

  alias OrganicleverBeExph.Auth.Guardian

  @accounts Application.compile_env!(:organiclever_be_exph, :accounts_impl)

  def register(conn, params) do
    case @accounts.register_user(params) do
      {:ok, user} ->
        conn
        |> put_status(:created)
        |> json(%{id: user.id, username: user.username})

      {:error, changeset} ->
        if username_taken?(changeset) do
          conn
          |> put_status(:conflict)
          |> json(%{message: "Username already exists"})
        else
          conn
          |> put_status(:bad_request)
          |> json(%{errors: format_errors(changeset)})
        end
    end
  end

  def login(conn, params) do
    username = Map.get(params, "username", "")
    password = Map.get(params, "password", "")

    cond do
      username == "" ->
        conn
        |> put_status(:bad_request)
        |> json(%{errors: %{username: ["can't be blank"]}})

      password == "" ->
        conn
        |> put_status(:bad_request)
        |> json(%{errors: %{password: ["can't be blank"]}})

      true ->
        case @accounts.authenticate_user(username, password) do
          {:ok, user} ->
            {:ok, token, _claims} = Guardian.encode_and_sign(user)
            json(conn, %{token: token, type: "Bearer"})

          {:error, :invalid_credentials} ->
            conn
            |> put_status(:unauthorized)
            |> json(%{message: "Invalid credentials"})
        end
    end
  end

  defp username_taken?(%Ecto.Changeset{} = changeset) do
    changeset.errors
    |> Keyword.get_values(:username)
    |> Enum.any?(fn {_, opts} -> opts[:constraint] == :unique end)
  end

  defp format_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, _opts} -> msg end)
  end
end
