defmodule DemoBeExph.Integration.Helpers do
  @moduledoc """
  Shared helpers for Cabbage integration tests using real Ecto sandbox.
  """

  alias DemoBeExph.Accounts
  alias DemoBeExph.Auth.Guardian
  alias DemoBeExph.Token.TokenContext

  @doc "Register a user and return the User struct."
  def register_user!(username, email, password) do
    {:ok, user} =
      Accounts.register_user(%{
        "username" => username,
        "email" => email,
        "password" => password
      })

    user
  end

  @doc "Log in a user and return {access_token, refresh_token}."
  def login_user!(user) do
    {:ok, access_token, _claims} = Guardian.encode_and_sign(user)
    {:ok, refresh_token} = TokenContext.create_refresh_token(user.id)
    {access_token, refresh_token}
  end

  @doc "Build an authorization header value for the given token."
  def bearer_header(token), do: "Bearer #{token}"

  @doc "Decode JWT payload (Base64url) without verifying signature."
  def decode_jwt_payload(token) do
    [_header, payload_b64 | _rest] = String.split(token, ".")
    payload_b64 |> Base.url_decode64!(padding: false) |> Jason.decode!()
  end

  @doc "Make user an admin."
  def make_admin!(user) do
    {:ok, admin_user} = Accounts.set_admin_role(user)
    admin_user
  end
end
