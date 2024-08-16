import Config

config :floki, :html_parser, Floki.HTMLParser.FastHtml

config :oapi_generator,
  default: [
    processor: LiqpayAPI.Generator.Processor,
    renderer: LiqpayAPI.Generator.Renderer,
    naming: [
      rename: [
        {~r/^(.+\.(?:Request|Response))([^\.].+)$/, "\\1.\\2"}
      ]
    ],
    output: [
      base_module: LiqpayAPI,
      location: "lib"
    ]
  ]

config :open_api_client_ex,
  "$base": [
    test_renderer: LiqpayAPI.Generator.TestRenderer
  ],
  default: [
    base_url: "https://www.liqpay.ua",
    test_location: "test"
  ]
