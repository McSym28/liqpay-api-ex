import Config

config :floki, :html_parser, Floki.HTMLParser.FastHtml

config :oapi_generator,
  default: [
    processor: LiqpayAPI.Generator.Processor,
    renderer: LiqpayAPI.Generator.Renderer,
    output: [
      base_module: LiqpayAPI,
      location: "lib",
      schema_subdirectory: "schemas"
    ]
  ]

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
  ],
  default: [
    base_url: "https://www.liqpay.ua",
    test_location: "test"
  ]
