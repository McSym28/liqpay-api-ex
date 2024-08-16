import Config

# Print only warnings and errors during test
config :logger, level: :warning

config :open_api_client_ex,
  "$base": [
    httpoison: OpenAPIClient.HTTPoisonMock,
    client: OpenAPIClient.ClientMock
  ]

config :liqpay_api_ex, private_key: "a4825234f4bae72a0be04eafe9e8e2bada209255"
