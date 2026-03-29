defmodule ADemoBeExph.Auth.Guardian do
  @moduledoc """
  Guardian implementation for JWT authentication.
  Encodes and decodes user tokens for protected endpoints.
  """

  use Guardian, otp_app: :a_demo_be_exph

  defp accounts, do: Application.get_env(:a_demo_be_exph, :accounts_module, ADemoBeExph.Accounts)

  def subject_for_token(%{id: id}, _claims), do: {:ok, to_string(id)}
  def subject_for_token(_, _), do: {:error, :unknown_resource}

  def resource_from_claims(%{"sub" => id}) do
    user = accounts().get_user(id)

    case user do
      nil -> {:error, :resource_not_found}
      user -> {:ok, user}
    end
  end

  def resource_from_claims(_), do: {:error, :missing_subject}
end
