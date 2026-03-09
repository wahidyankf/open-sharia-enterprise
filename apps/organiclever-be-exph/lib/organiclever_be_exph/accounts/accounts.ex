defmodule OrganicleverBeExph.Accounts do
  @moduledoc """
  Accounts context for user registration and authentication.
  """

  @behaviour OrganicleverBeExph.Accounts.Behaviour

  alias OrganicleverBeExph.Accounts.User
  alias OrganicleverBeExph.Repo

  @impl true
  def register_user(attrs) do
    %User{}
    |> User.changeset(attrs)
    |> Repo.insert()
  end

  @impl true
  def authenticate_user(username, password) do
    user = Repo.get_by(User, username: username)
    verify_password(user, password)
  end

  defp verify_password(nil, _password) do
    Bcrypt.no_user_verify()
    {:error, :invalid_credentials}
  end

  defp verify_password(user, password) do
    if Bcrypt.verify_pass(password, user.password_hash) do
      {:ok, user}
    else
      {:error, :invalid_credentials}
    end
  end
end
