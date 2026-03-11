defmodule DemoBeExph.Accounts do
  @moduledoc """
  Accounts context for user registration, authentication, and profile management.
  """

  @behaviour DemoBeExph.Accounts.Behaviour

  import Ecto.Query

  alias DemoBeExph.Accounts.User
  alias DemoBeExph.Repo

  @max_failed_attempts 5

  @doc "Register a new user."
  def register_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Authenticate user by username and password.
  Enforces lockout after @max_failed_attempts failures.
  Returns {:ok, user}, {:error, :invalid_credentials}, {:error, :account_locked},
  or {:error, :account_deactivated}.
  """
  def authenticate_user(username, password) do
    user = Repo.get_by(User, username: username)
    verify_password(user, password)
  end

  @doc "Get a user by ID."
  def get_user(id) do
    Repo.get(User, id)
  end

  @doc "Get a user by username."
  def get_user_by_username(username) do
    Repo.get_by(User, username: username)
  end

  @doc "Get a user by email."
  def get_user_by_email(email) do
    Repo.get_by(User, email: email)
  end

  @doc "List all users, optionally filtered by email."
  def list_users(opts \\ []) do
    email_filter = Keyword.get(opts, :email)
    page = Keyword.get(opts, :page, 1)
    page_size = Keyword.get(opts, :page_size, 20)
    offset = (page - 1) * page_size

    query =
      if email_filter do
        from u in User, where: u.email == ^email_filter
      else
        from(u in User)
      end

    total = Repo.aggregate(query, :count, :id)
    users = Repo.all(from u in query, limit: ^page_size, offset: ^offset, order_by: [asc: u.id])
    %{data: users, total: total, page: page, page_size: page_size}
  end

  @doc "Update the display_name of a user."
  def update_user(user, attrs) do
    user
    |> User.update_changeset(attrs)
    |> Repo.update()
  end

  @doc "Change the password of a user after verifying the old password."
  def change_password(user, old_password, new_password) do
    if Bcrypt.verify_pass(old_password, user.password_hash) do
      user
      |> User.password_changeset(%{password: new_password})
      |> Repo.update()
    else
      {:error, :invalid_credentials}
    end
  end

  @doc "Deactivate a user account (status -> INACTIVE, used for self-deactivation)."
  def deactivate_user(user) do
    user
    |> User.status_changeset(%{status: "INACTIVE"})
    |> Repo.update()
  end

  @doc "Disable a user account via admin (status -> DISABLED)."
  def disable_user(user) do
    user
    |> User.status_changeset(%{status: "DISABLED"})
    |> Repo.update()
  end

  @doc "Enable (re-activate) a user account (status -> ACTIVE)."
  def enable_user(user) do
    user
    |> User.status_changeset(%{status: "ACTIVE", failed_login_attempts: 0, locked_at: nil})
    |> Repo.update()
  end

  @doc "Set role to ADMIN."
  def set_admin_role(user) do
    user
    |> User.status_changeset(%{role: "ADMIN"})
    |> Repo.update()
  end

  @doc "Unlock a locked user (clears failed_login_attempts and locked_at)."
  def unlock_user(user) do
    user
    |> User.status_changeset(%{
      status: "ACTIVE",
      failed_login_attempts: 0,
      locked_at: nil
    })
    |> Repo.update()
  end

  # Private helpers

  defp verify_password(nil, _password) do
    Bcrypt.no_user_verify()
    {:error, :invalid_credentials}
  end

  defp verify_password(%User{status: "LOCKED"} = _user, _password) do
    {:error, :account_locked}
  end

  defp verify_password(%User{status: status} = _user, _password)
       when status in ["INACTIVE", "DISABLED"] do
    {:error, :account_deactivated}
  end

  defp verify_password(user, password) do
    if Bcrypt.verify_pass(password, user.password_hash) do
      reset_failed_attempts(user)
      {:ok, user}
    else
      handle_failed_attempt(user)
    end
  end

  defp reset_failed_attempts(user) do
    if user.failed_login_attempts > 0 do
      user
      |> User.status_changeset(%{failed_login_attempts: 0, locked_at: nil})
      |> Repo.update()
    end
  end

  defp handle_failed_attempt(user) do
    new_attempts = user.failed_login_attempts + 1

    if new_attempts >= @max_failed_attempts do
      user
      |> User.status_changeset(%{
        status: "LOCKED",
        failed_login_attempts: new_attempts,
        locked_at: DateTime.utc_now() |> DateTime.truncate(:second)
      })
      |> Repo.update()

      {:error, :account_locked}
    else
      user
      |> User.status_changeset(%{failed_login_attempts: new_attempts})
      |> Repo.update()

      {:error, :invalid_credentials}
    end
  end
end
