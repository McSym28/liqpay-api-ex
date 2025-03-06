defmodule LiqPayAPI.InternetAcquiring.Token.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Token.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :paytoken,
          amount: number,
          card_token: String.t(),
          currency: :eur | :uah | :usd,
          customer: String.t() | nil,
          dae: String.t() | nil,
          description: String.t(),
          info: String.t() | nil,
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          phone: String.t() | nil,
          prepare: true | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t(),
          sender: LiqPayAPI.InternetAcquiring.Token.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          split_rules: [LiqPayAPI.InternetAcquiring.Token.Request.SplitRules.t()] | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :card_token, :currency, :description, :ip, :order_id]
  defstruct [
    :amount,
    :card_token,
    :currency,
    :customer,
    :dae,
    :description,
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
    :sender,
    :server_url,
    :split_rules,
    action: :paytoken,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, paytoken: "paytoken"}},
      amount: {"amount", :number},
      card_token: {"card_token", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      customer: {"customer", {:string, :generic}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
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
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.Token.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      split_rules: {"split_rules", [{LiqPayAPI.InternetAcquiring.Token.Request.SplitRules, :t}]},
      version: {"version", {:enum, [3]}}
    ]
  end
end
