defmodule LiqPayAPIWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :liqpay_api_ex

  plug(Plug.Parsers, parsers: [:urlencoded])
  plug(LiqPayAPIWeb.Router)
end
