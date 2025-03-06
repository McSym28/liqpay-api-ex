import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :liqpay_api_ex, LiqPayAPIWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "7qBgGfcXYK2QwZNhzSO3iDDKUz0n8qqK2+mnerAoUq0ytMUfRhzo6h71UFjjabet",
  server: false

# Initialize plugs at runtime for faster test compilation
# Use Jason for JSON parsing in Phoenix
config :phoenix,
  plug_init_mode: :runtime,
  json_library: Jason

# Print only warnings and errors during test
config :logger, level: :warning

config :open_api_client_ex,
  "$base": [
    httpoison: OpenAPIClient.HTTPoisonMock,
    client: OpenAPIClientMock
  ]

config :liqpay_api_ex,
  private_key: "your_private_key",
  public_key: "your_public_key"
