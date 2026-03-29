defmodule ADemoBeExph.Repo do
  use Ecto.Repo,
    otp_app: :a_demo_be_exph,
    adapter: Ecto.Adapters.Postgres
end
