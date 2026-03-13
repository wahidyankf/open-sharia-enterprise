defmodule DemoBeExph.Token.TokenContext do
  @moduledoc """
  Token context for refresh token rotation and access token revocation.
  """

  @behaviour DemoBeExph.Token.TokenBehaviour

  import Ecto.Query

  alias DemoBeExph.Repo
  alias DemoBeExph.Token.RefreshToken
  alias DemoBeExph.Token.RevokedToken

  @refresh_token_ttl_days 30

  # --- Refresh tokens ---

  @doc "Create and store a new refresh token for the given user_id."
  def create_refresh_token(user_id) do
    raw_token = :crypto.strong_rand_bytes(32) |> Base.url_encode64(padding: false)
    token_hash = hash_token(raw_token)

    expires_at =
      DateTime.utc_now()
      |> DateTime.add(@refresh_token_ttl_days, :day)
      |> DateTime.truncate(:second)

    attrs = %{
      user_id: user_id,
      token_hash: token_hash,
      expires_at: expires_at
    }

    case %RefreshToken{} |> RefreshToken.changeset(attrs) |> Repo.insert() do
      {:ok, _record} -> {:ok, raw_token}
      {:error, changeset} -> {:error, changeset}
    end
  end

  @doc "Validate a raw refresh token. Returns {:ok, record} or {:error, reason}."
  def validate_refresh_token(raw_token) do
    token_hash = hash_token(raw_token)
    record = Repo.get_by(RefreshToken, token_hash: token_hash)

    cond do
      is_nil(record) ->
        {:error, :invalid_token}

      DateTime.compare(record.expires_at, DateTime.utc_now()) == :lt ->
        Repo.delete(record)
        {:error, :token_expired}

      true ->
        {:ok, record}
    end
  end

  @doc "Consume (delete) a refresh token by its hash — single-use rotation."
  def consume_refresh_token(raw_token) do
    token_hash = hash_token(raw_token)

    case Repo.get_by(RefreshToken, token_hash: token_hash) do
      nil -> {:error, :invalid_token}
      record -> Repo.delete(record)
    end
  end

  @doc "Delete all refresh tokens for a user (logout-all)."
  def revoke_all_refresh_tokens(user_id) do
    Repo.delete_all(from t in RefreshToken, where: t.user_id == ^user_id)
    :ok
  end

  # --- Revoked access tokens (JTI blacklist) ---

  @doc "Record an access token JTI as revoked."
  def revoke_access_token(jti, user_id \\ nil) do
    %RevokedToken{}
    |> RevokedToken.changeset(%{jti: jti, user_id: user_id})
    |> Repo.insert(on_conflict: :nothing)

    :ok
  end

  @doc "Check if an access token JTI is revoked."
  def revoked?(jti) do
    Repo.exists?(from t in RevokedToken, where: t.jti == ^jti)
  end

  @doc "Revoke all access tokens for a user by inserting placeholder entries."
  def revoke_all_access_tokens_for_user(user_id) do
    Repo.delete_all(from t in RefreshToken, where: t.user_id == ^user_id)
    :ok
  end

  @doc """
  Expire a refresh token for test setup only — sets expires_at to the past.
  Not part of TokenBehaviour; used in integration test step definitions.
  """
  def expire_refresh_token!(raw_token) do
    token_hash = hash_token(raw_token)
    past = ~U[2020-01-01 00:00:00Z]

    case Repo.get_by(RefreshToken, token_hash: token_hash) do
      nil ->
        :ok

      record ->
        record
        |> Ecto.Changeset.change(expires_at: past)
        |> Repo.update!()

        :ok
    end
  end

  # Private

  defp hash_token(raw_token) do
    :crypto.hash(:sha256, raw_token) |> Base.encode16(case: :lower)
  end
end
