defmodule AAAADemoBeExphWeb.TestApiController do
  use AAAADemoBeExphWeb, :controller

  alias AADemoBeExph.Accounts.User
  alias AADemoBeExph.Attachment.Attachment
  alias AADemoBeExph.Expense.Expense
  alias AADemoBeExph.Repo
  alias AADemoBeExph.Token.RefreshToken
  alias AADemoBeExph.Token.RevokedToken

  @doc """
  Deletes all data in dependency order to respect foreign key constraints.
  Order: attachments → expenses → refresh_tokens → revoked_tokens → users
  """
  def reset_db(conn, _params) do
    Repo.delete_all(Attachment)
    Repo.delete_all(Expense)
    Repo.delete_all(RefreshToken)
    Repo.delete_all(RevokedToken)
    Repo.delete_all(User)

    json(conn, %{"message" => "Database reset successful"})
  end

  @doc """
  Promotes a user to the ADMIN role by username.
  Returns 404 if the user is not found.
  """
  def promote_admin(conn, %{"username" => username}) do
    case Repo.get_by(User, username: username) do
      nil ->
        conn
        |> put_status(:not_found)
        |> json(%{"message" => "User not found"})

      user ->
        user
        |> User.status_changeset(%{role: "ADMIN"})
        |> Repo.update!()

        json(conn, %{"message" => "User #{username} promoted to ADMIN"})
    end
  end
end
