defmodule DemoBeExph.Accounts.Behaviour do
  @moduledoc """
  Behaviour contract for the Accounts context.
  Kept for documentation purposes — all tests now use real Ecto sandbox.
  """

  alias DemoBeExph.Accounts.User

  @callback register_user(map()) :: {:ok, User.t()} | {:error, Ecto.Changeset.t()}
  @callback authenticate_user(String.t(), String.t()) ::
              {:ok, User.t()}
              | {:error, :invalid_credentials}
              | {:error, :account_locked}
              | {:error, :account_deactivated}
end
