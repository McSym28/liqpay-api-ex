defmodule LiqPayAPIWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :liqpay_api_ex

  plug(Plug.Parsers,
    parsers: [:json],
    json_decoder: Phoenix.json_library()
  )

  plug(LiqPayAPIWeb.Router)
end
