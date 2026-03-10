defmodule DemoBeExph.Auth.Guardian do
  @moduledoc """
  Guardian implementation for JWT authentication.
  Encodes and decodes user tokens for protected endpoints.
  """

  use Guardian, otp_app: :demo_be_exph

  alias DemoBeExph.Accounts.User
  alias DemoBeExph.Repo

  def subject_for_token(%{id: id}, _claims), do: {:ok, to_string(id)}
  def subject_for_token(_, _), do: {:error, :unknown_resource}

  def resource_from_claims(%{"sub" => id}) do
    user = Repo.get(User, String.to_integer(id))

    case user do
      nil -> {:error, :resource_not_found}
      user -> {:ok, user}
    end
  end

  def resource_from_claims(_), do: {:error, :missing_subject}
end
