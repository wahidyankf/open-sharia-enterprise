defmodule DemoBeExph.Token.TokenBehaviour do
  @moduledoc """
  Behaviour contract for the Token context.
  Allows swapping real Ecto implementation for in-memory implementation in tests.
  """

  @callback create_refresh_token(integer()) :: {:ok, binary()} | {:error, any()}
  @callback validate_refresh_token(binary()) :: {:ok, map()} | {:error, atom()}
  @callback consume_refresh_token(binary()) :: {:ok, any()} | {:error, atom()}
  @callback revoke_all_refresh_tokens(integer()) :: :ok
  @callback revoke_access_token(binary(), integer() | nil) :: :ok
  @callback revoked?(binary()) :: boolean()
  @callback revoke_all_access_tokens_for_user(integer()) :: :ok
end
