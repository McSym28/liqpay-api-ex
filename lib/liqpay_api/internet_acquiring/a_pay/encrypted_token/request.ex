defmodule LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.APay.EncryptedToken.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :hold | :pay,
          amount: number,
          card: String.t(),
          card_exp_month: String.t(),
          card_exp_year: String.t(),
          currency: :eur | :uah | :usd,
          dae: String.t() | nil,
          description: String.t(),
          info: String.t() | nil,
          language: :en | :uk | nil,
          order_id: String.t(),
          paytype: :apay_tavv,
          phone: String.t() | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t(),
          result_url: String.t() | nil,
          sender: LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          split_rules:
            [LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request.SplitRules.t()] | nil,
          tavv: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [
    :action,
    :amount,
    :card,
    :card_exp_month,
    :card_exp_year,
    :currency,
    :description,
    :order_id,
    :tavv
  ]
  defstruct [
    :action,
    :amount,
    :card,
    :card_exp_month,
    :card_exp_year,
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
    :tavv,
    paytype: :apay_tavv,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, hold: "hold", pay: "pay"}},
      amount: {"amount", :number},
      card: {"card", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
      info: {"info", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      paytype: {"paytype", {:enum, apay_tavv: "apay_tavv"}},
      phone: {"phone", {:string, :generic}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      result_url: {"result_url", {:string, :uri}},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      split_rules:
        {"split_rules",
         [{LiqPayAPI.InternetAcquiring.APay.EncryptedToken.Request.SplitRules, :t}]},
      tavv: {"tavv", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
