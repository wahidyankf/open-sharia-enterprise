import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :a_demo_be_exph, AADemoBeExphWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "9nYAOfW0t4BGQgRMTz9Wo7C9vwlrhrSLoIfWbZK8fxxz6E4ehacBRt29myR/Hi3o",
  server: false

# Guardian test secret — overrides runtime.exs so APP_JWT_SECRET is not required during tests
# config/test.exs is evaluated before runtime.exs, taking precedence in the test env.
config :a_demo_be_exph, ADemoBeExph.Auth.Guardian,
  secret_key: "test_secret_do_not_use_in_production",
  ttl: {24, :hours}

# In-memory module overrides — no PostgreSQL required for tests
config :a_demo_be_exph, :accounts_module, ADemoBeExph.Test.InMemoryAccounts
config :a_demo_be_exph, :token_module, ADemoBeExph.Test.InMemoryTokenContext
config :a_demo_be_exph, :expense_module, ADemoBeExph.Test.InMemoryExpenseContext
config :a_demo_be_exph, :attachment_module, ADemoBeExph.Test.InMemoryAttachmentContext

# Elixir Cabbage BDD — feature files relative to workspace root
config :elixir_cabbage,
  features: Path.expand("../../../specs/apps/a-demo/be/gherkin/", __DIR__) <> "/"

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Sort query params output of verified routes for robust url comparisons
config :phoenix,
  sort_verified_routes_query_params: true
