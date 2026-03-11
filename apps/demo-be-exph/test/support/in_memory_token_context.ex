defmodule DemoBeExph.Test.InMemoryTokenContext do
  @moduledoc """
  In-memory implementation of DemoBeExph.Token.TokenBehaviour backed by InMemoryStore Agent.
  Used in test environment to avoid real PostgreSQL.
  """

  @behaviour DemoBeExph.Token.TokenBehaviour

  alias DemoBeExph.Test.InMemoryStore

  @refresh_token_ttl_days 30

  @impl true
  def create_refresh_token(user_id) do
    raw_token = :crypto.strong_rand_bytes(32) |> Base.url_encode64(padding: false)
    token_hash = hash_token(raw_token)

    expires_at =
      DateTime.utc_now()
      |> DateTime.add(@refresh_token_ttl_days, :day)
      |> DateTime.truncate(:second)

    record = %{
      user_id: user_id,
      token_hash: token_hash,
      expires_at: expires_at,
      inserted_at: DateTime.utc_now() |> DateTime.truncate(:second)
    }

    store_refresh_token(token_hash, record)
    {:ok, raw_token}
  end

  @impl true
  def validate_refresh_token(raw_token) do
    token_hash = hash_token(raw_token)
    state = InMemoryStore.get_state()
    record = Map.get(state.refresh_tokens, token_hash)
    check_token_valid(record, token_hash)
  end

  @impl true
  def consume_refresh_token(raw_token) do
    token_hash = hash_token(raw_token)
    state = InMemoryStore.get_state()
    record = Map.get(state.refresh_tokens, token_hash)
    delete_refresh_token_record(record, token_hash)
  end

  @impl true
  def revoke_all_refresh_tokens(user_id) do
    InMemoryStore.update_state(fn s ->
      Map.update!(s, :refresh_tokens, &reject_user_tokens(&1, user_id))
    end)

    :ok
  end

  @impl true
  def revoke_access_token(jti, _user_id \\ nil) do
    InMemoryStore.update_state(fn s ->
      Map.update!(s, :revoked_jtis, &MapSet.put(&1, jti))
    end)

    :ok
  end

  @impl true
  def revoked?(jti) do
    MapSet.member?(InMemoryStore.get_state().revoked_jtis, jti)
  end

  @impl true
  def revoke_all_access_tokens_for_user(user_id) do
    revoke_all_refresh_tokens(user_id)
  end

  @doc "Expire a refresh token (for test setup only) — sets expires_at to the past."
  def expire_refresh_token!(raw_token) do
    token_hash = hash_token(raw_token)
    past = ~U[2020-01-01 00:00:00Z]

    InMemoryStore.update_state(fn s ->
      Map.update!(s, :refresh_tokens, &set_token_expired(&1, token_hash, past))
    end)

    :ok
  end

  # Private helpers

  defp store_refresh_token(token_hash, record) do
    InMemoryStore.update_state(fn s ->
      Map.update!(s, :refresh_tokens, &Map.put(&1, token_hash, record))
    end)
  end

  defp check_token_valid(nil, _token_hash), do: {:error, :invalid_token}

  defp check_token_valid(record, token_hash) do
    if DateTime.compare(record.expires_at, DateTime.utc_now()) == :lt do
      delete_token_from_store(token_hash)
      {:error, :token_expired}
    else
      {:ok, record}
    end
  end

  defp delete_token_from_store(token_hash) do
    InMemoryStore.update_state(fn s ->
      Map.update!(s, :refresh_tokens, &Map.delete(&1, token_hash))
    end)
  end

  defp delete_refresh_token_record(nil, _token_hash), do: {:error, :invalid_token}

  defp delete_refresh_token_record(record, token_hash) do
    delete_token_from_store(token_hash)
    {:ok, record}
  end

  defp reject_user_tokens(tokens, user_id) do
    Enum.reject(tokens, fn {_hash, record} -> record.user_id == user_id end)
    |> Enum.into(%{})
  end

  defp set_token_expired(tokens, token_hash, past) do
    Map.update!(tokens, token_hash, &Map.put(&1, :expires_at, past))
  end

  defp hash_token(raw_token) do
    :crypto.hash(:sha256, raw_token) |> Base.encode16(case: :lower)
  end
end
