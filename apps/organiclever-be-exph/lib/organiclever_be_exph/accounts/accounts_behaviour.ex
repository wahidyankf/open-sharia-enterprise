defmodule OrganicleverBeExph.Accounts.Behaviour do
  @moduledoc """
  Behaviour contract for the Accounts context.
  Enables Mox-based in-process mocking for integration tests.
  """

  alias OrganicleverBeExph.Accounts.User

  @callback register_user(map()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback authenticate_user(String.t(), String.t()) ::
              {:ok, User.t()} | {:error, :invalid_credentials}
end
