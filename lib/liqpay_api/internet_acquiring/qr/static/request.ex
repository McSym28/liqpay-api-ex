defmodule LiqPayAPI.InternetAcquiring.QR.Static.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.QR.Static.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :static_qr_create,
          amount: number,
          currency: :eur | :uah | :usd,
          description: String.t(),
          final_date: String.t() | nil,
          order_id: String.t(),
          public_key: String.t(),
          server_url: String.t() | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :order_id]
  defstruct [
    :amount,
    :currency,
    :description,
    :final_date,
    :order_id,
    :public_key,
    :server_url,
    action: :static_qr_create,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, static_qr_create: "staticQrCreate"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      description: {"description", {:string, :generic}},
      final_date: {"final_date", {:string, :generic}},
      order_id: {"order_id", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      server_url: {"server_url", {:string, :uri}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
