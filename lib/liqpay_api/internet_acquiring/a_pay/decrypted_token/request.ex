defmodule LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.APay.DecryptedToken.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :hold | :pay,
          amount: number,
          applepay_token: String.t(),
          currency: :eur | :uah | :usd,
          dae: String.t() | nil,
          description: String.t(),
          info: String.t() | nil,
          language: :en | :uk | nil,
          order_id: String.t(),
          paytype: :apay,
          phone: String.t() | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t(),
          result_url: String.t() | nil,
          sender: LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          split_rules:
            [LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.SplitRules.t()] | nil,
          split_tickets_only: boolean | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:action, :amount, :applepay_token, :currency, :description, :order_id]
  defstruct [
    :action,
    :amount,
    :applepay_token,
    :currency,
    :dae,
    :description,
    :info,
    :language,
    :order_id,
    :phone,
    :product_category,
    :product_description,
    :product_name,
    :product_url,
    :public_key,
    :result_url,
    :sender,
    :server_url,
    :split_rules,
    :split_tickets_only,
    paytype: :apay,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, hold: "hold", pay: "pay"}},
      amount: {"amount", :number},
      applepay_token: {"applepay_token", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
      info: {"info", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      paytype: {"paytype", {:enum, apay: "apay"}},
      phone: {"phone", {:string, :generic}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      result_url: {"result_url", {:string, :uri}},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      split_rules:
        {"split_rules",
         [{LiqPayAPI.InternetAcquiring.APay.DecryptedToken.Request.SplitRules, :t}]},
      split_tickets_only: {"split_tickets_only", :boolean},
      version: {"version", {:enum, [3]}}
    ]
  end
end
