defmodule OrganicleverBeExph.Integration.Helpers do
  @moduledoc """
  Shared helpers for integration tests using Cabbage + Mox.
  """

  import Ecto.Changeset

  alias OrganicleverBeExph.Accounts.User
  alias OrganicleverBeExph.Auth.Guardian

  @doc """
  Generates a valid JWT token for a fake user struct (no DB required).
  """
  def generate_token(user_id \\ 1) do
    {:ok, token, _claims} = Guardian.encode_and_sign(%{id: user_id})
    token
  end

  @doc """
  Builds a unique constraint error changeset on the username field to simulate
  a database unique constraint violation.
  """
  def unique_username_changeset do
    %User{}
    |> change(%{})
    |> add_error(:username, "has already been taken",
      constraint: :unique,
      constraint_name: "users_username_index"
    )
  end
end
