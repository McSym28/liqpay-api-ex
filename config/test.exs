import Config

# Print only warnings and errors during test
config :logger, level: :warning

config :open_api_client_ex,
  "$base": [
    client_pipeline: OpenAPIClient.BasicHTTPoisonPipeline,
    httpoison: OpenAPIClient.HTTPoisonMock,
    decoders: [
      {"application/json", {Jason, :decode, []}}
    ],
    encoders: [
      {"application/json", {Jason, :encode, []}}
    ]
  ]
