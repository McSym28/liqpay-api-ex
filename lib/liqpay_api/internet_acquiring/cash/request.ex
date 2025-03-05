defmodule LiqPayAPI.InternetAcquiring.Cash.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Cash.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :paycash,
          amount: number,
          currency: :eur | :uah | :usd,
          customer: String.t() | nil,
          dae: String.t() | nil,
          description: String.t(),
          expired_date: DateTime.t() | nil,
          info: String.t() | nil,
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          phone: String.t(),
          prepare: true | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t(),
          server_url: String.t() | nil,
          split_rules: [LiqPayAPI.InternetAcquiring.Cash.Request.SplitRules.t()] | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :ip, :order_id, :phone]
  defstruct [
    :amount,
    :currency,
    :customer,
    :dae,
    :description,
    :expired_date,
    :info,
    :ip,
    :language,
    :order_id,
    :phone,
    :prepare,
    :product_category,
    :product_description,
    :product_name,
    :product_url,
    :public_key,
    :server_url,
    :split_rules,
    action: :paycash,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, paycash: "paycash"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      customer: {"customer", {:string, :generic}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
      expired_date: {"expired_date", {:string, "date-time-liqpay"}},
      info: {"info", {:string, :generic}},
      ip: {"ip", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      prepare: {"prepare", {:enum, true: "1"}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key: {"public_key", {:string, :generic}},
      server_url: {"server_url", {:string, :uri}},
      split_rules: {"split_rules", [{LiqPayAPI.InternetAcquiring.Cash.Request.SplitRules, :t}]},
      version: {"version", {:enum, [3]}}
    ]
  end
end
