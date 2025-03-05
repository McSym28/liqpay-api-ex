defmodule LiqPayAPI.InternetAcquiring.QR.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.QR.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :payqr,
          amount: number,
          currency: :eur | :uah | :usd,
          customer: String.t() | nil,
          dae: String.t() | nil,
          description: String.t(),
          info: String.t() | nil,
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          prepare: true | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t(),
          recurringbytoken: true | nil,
          server_url: String.t() | nil,
          split_rules: [LiqPayAPI.InternetAcquiring.QR.Request.SplitRules.t()] | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :ip, :order_id]
  defstruct [
    :amount,
    :currency,
    :customer,
    :dae,
    :description,
    :info,
    :ip,
    :language,
    :order_id,
    :prepare,
    :product_category,
    :product_description,
    :product_name,
    :product_url,
    :public_key,
    :recurringbytoken,
    :server_url,
    :split_rules,
    action: :payqr,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, payqr: "payqr"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      customer: {"customer", {:string, :generic}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
      info: {"info", {:string, :generic}},
      ip: {"ip", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      prepare: {"prepare", {:enum, true: "1"}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key: {"public_key", {:string, :generic}},
      recurringbytoken: {"recurringbytoken", {:enum, true: "1"}},
      server_url: {"server_url", {:string, :uri}},
      split_rules: {"split_rules", [{LiqPayAPI.InternetAcquiring.QR.Request.SplitRules, :t}]},
      version: {"version", {:enum, [3]}}
    ]
  end
end
