defmodule DemoBeExph.Repo do
  use Ecto.Repo,
    otp_app: :demo_be_exph,
    adapter: Ecto.Adapters.Postgres
end
