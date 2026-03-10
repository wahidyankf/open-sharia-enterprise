defmodule DemoBeExphWeb.JwksController do
  use DemoBeExphWeb, :controller

  @doc """
  Returns a minimal JWKS representation.
  For HS256 (symmetric HMAC), the public key is the same as the secret.
  This endpoint returns the algorithm metadata for service integrators.
  """
  def index(conn, _params) do
    # HS256 is a symmetric algorithm — there is no public key to expose.
    # Return a minimal JWKS document indicating the algorithm in use.
    json(conn, %{
      keys: [
        %{
          kty: "oct",
          use: "sig",
          alg: "HS256",
          kid: "default"
        }
      ]
    })
  end
end
