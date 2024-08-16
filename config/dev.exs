import Config

config :floki, :html_parser, Floki.HTMLParser.FastHtml

config :oapi_generator,
  default: [
    processor: LiqPayAPI.Generator.Processor,
    renderer: LiqPayAPI.Generator.Renderer,
    naming: [
      rename: [
        {~r/^(.+\.(?:Request|Response))([^\.].+)$/, "\\1.\\2"}
      ]
    ],
    output: [
      base_module: LiqPayAPI,
      location: "lib"
    ]
  ]

config :open_api_client_ex,
  "$base": [
    test_renderer: LiqPayAPI.Generator.TestRenderer
  ],
  default: [
    base_url: "https://www.liqpay.ua",
    operations: [
      {{~r/^\/api\/request(?:\?path=.*)?/, :post},
       [
         params: [
           {{"private_key", :new},
            [
              spec: %{
                "schema" => %{"type" => "string"},
                "description" =>
                  "Private key of the created company (not available to anyone except your developer)",
                "required" => true
              },
              default: {Application, :get_env, [:liqpay_api_ex, :private_key]},
              example: "a4825234f4bae72a0be04eafe9e8e2bada209255"
            ]}
         ]
       ]}
    ],
    test_location: "test"
  ]
