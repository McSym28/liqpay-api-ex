import Config

config :floki, :html_parser, Floki.HTMLParser.FastHtml

config :oapi_generator,
  default: [
    processor: LiqPayAPI.Generator.Processor,
    renderer: LiqPayAPI.Generator.Renderer,
    naming: [
      rename: [
        {"OperationsCallbackRequest", "Callback"},
        {~r/^(InternetAcquiring|Confirmation|Information|P2PDebit|Partnership|Public|Tokens|TransferringToCard)([^\.].+)$/,
         "\\1.\\2"},
        {~r/^(InternetAcquiring\.)Apay([^\.].+)$/, "\\1APay.\\2"},
        {~r/^(InternetAcquiring\.)Dcc([^\.].+)$/, "\\1DCC.\\2"},
        {~r/^(InternetAcquiring\.)Gpay([^\.].+)$/, "\\1GPay.\\2"},
        {~r/^(InternetAcquiring\.)Qr([^\.].+)$/, "\\1QR.\\2"},
        {~r/^(InternetAcquiring\.(?:APay|CardPayment|Cash|Checkout|Invoice|GPay|PrivatPay|Refund|Subscription|Tokens|TwoStep|Widget))([^\.].+)$/,
         "\\1.\\2"},
        {~r/^(Confirmation\.)Cardverification([^\.].+)$/, "\\1CardVerification.\\2"},
        {~r/^(Confirmation\.)Cvv([^\.].+)$/, "\\1CVV.\\2"},
        {~r/^(Confirmation\.)Ds([^\.].+)$/, "\\1ThreeDS.\\2"},
        {~r/^(Confirmation\.)Mpi([^\.].+)$/, "\\1MPI.\\2"},
        {~r/^(Confirmation\.)Otp([^\.].+)$/, "\\1OTP.\\2"},
        {~r/^(Confirmation\.(?:ReceiverVerify|SenderVerify))([^\.].+)$/, "\\1.\\2"},
        {~r/^(Information\.(?:AddingData|PaymentArchive|Register))([^\.].+)$/, "\\1.\\2"},
        {~r/^(Information\.)ReceiveAReceipt([^\.].+)$/, "\\1ReceiveReceipt.\\2"},
        {~r/^(Information\.Register\.)CompensationReportP2pStatus([^\.].+)$/,
         "\\1CompensationReportP2PStatus.\\2"},
        {~r/^(Information\.Register\.)CompensationReportP2p([^\.].+)$/,
         "\\1CompensationReportP2P.\\2"},
        {~r/^P2pDebit([^\.].+)$/, "P2PDebit.\\1"},
        {~r/^(Partnership\.(?:InfoMerchant|InfoUser|ShopCreate|ShopEdit))([^\.].+)$/, "\\1.\\2"},
        {~r/^(Partnership\.ShopCreate\.)AvailableMcc([^\.].+)$/, "\\1AvailableMCC.\\2"},
        {~r/^(Public\.(?:Archive|DiscountRate|Exchange))([^\.].+)$/, "\\1.\\2"},
        {~r/^(.+[^\.])(Request|Response)$/, "\\1.\\2"},
        {~r/^(.+\.(?:Request|Response))([^\.].+)$/, "\\1.\\2"},
        {~r/^(InternetAcquiring\.(?:CardPayment|Checkout|TwoStep\.Complete)\.Request\.)RroInfo$/,
         "\\1RROInfo"},
        {~r/^(InternetAcquiring\.(?:CardPayment|Checkout|TwoStep\.Complete)\.Request\.RROInfo)([^\.].+)$/,
         "\\1.\\2"},
        {~r/^(InternetAcquiring\.(?:CardPayment|DCC)\.Response\.)DccAllowed$/, "\\1DCCAllowed"},
        {~r/^(Partnership\.ShopCreate\.AvailableMCC\.Response\.)MccCodes$/, "\\1MCCCodes"},
        {~r/^(Partnership\.ShopCreate\.(?:Create|Register)\.Request\.)Aggregator([^\.].+)$/,
         "\\1Aggregator.\\2"},
        {~r/^(Partnership\.ShopCreate\.(?:Create|Register)\.Request\.Aggregator\.)LawCtoInfo$/,
         "\\1LawCTOInfo"},
        {~r/^(Tokens\.Obtain\.Request\.)VcehTokenization$/, "\\1VCEHTokenization"}
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
