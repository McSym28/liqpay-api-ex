defmodule LiqPayAPI.InternetAcquiring.Invoice.Issue.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Issue.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :invoice_send,
          action_payment: :hold | :pay | :paydonate | :subscribe | nil,
          amount: number,
          currency: :eur | :uah | :usd,
          description: String.t(),
          email: String.t(),
          expired_date: DateTime.t() | nil,
          goods: [LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.Goods.t()] | nil,
          language: :en | :uk | nil,
          order_id: String.t(),
          phone: String.t(),
          public_key: String.t(),
          result_url: String.t() | nil,
          server_url: String.t() | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :email, :order_id, :phone]
  defstruct [
    :action_payment,
    :amount,
    :currency,
    :description,
    :email,
    :expired_date,
    :goods,
    :language,
    :order_id,
    :phone,
    :public_key,
    :result_url,
    :server_url,
    action: :invoice_send,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, invoice_send: "invoice_send"}},
      action_payment:
        {"action_payment",
         {:enum, hold: "hold", pay: "pay", paydonate: "paydonate", subscribe: "subscribe"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      description: {"description", {:string, :generic}},
      email: {"email", {:string, :generic}},
      expired_date: {"expired_date", {:string, "date-time-liqpay"}},
      goods: {"goods", [{LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.Goods, :t}]},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      public_key: {"public_key", {:string, :generic}},
      result_url: {"result_url", {:string, :uri}},
      server_url: {"server_url", {:string, :uri}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
