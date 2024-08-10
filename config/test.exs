import Config

# Print only warnings and errors during test
config :logger, level: :warning

config :open_api_client_ex,
  "$base": [
    httpoison: OpenAPIClient.HTTPoisonMock
  ]
