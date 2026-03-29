# This file is responsible for configuring your application
# and its dependencies with the aid of the Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
import Config

config :a_demo_be_exph,
  ecto_repos: [AADemoBeExph.Repo],
  generators: [timestamp_type: :utc_datetime]

config :a_demo_be_exph, :accounts_module, AADemoBeExph.Accounts
config :a_demo_be_exph, :token_module, AADemoBeExph.Token.TokenContext
config :a_demo_be_exph, :expense_module, AADemoBeExph.Expense.ExpenseContext
config :a_demo_be_exph, :attachment_module, AADemoBeExph.Attachment.AttachmentContext

# Configure the endpoint
config :a_demo_be_exph, AAAADemoBeExphWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [json: AAAADemoBeExphWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: AADemoBeExph.PubSub,
  live_view: [signing_salt: "8pJ4Iu2a"]

# Configure Elixir's Logger
config :logger, :default_formatter,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{config_env()}.exs"
