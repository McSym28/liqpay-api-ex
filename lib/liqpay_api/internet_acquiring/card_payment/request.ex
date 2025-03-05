defmodule LiqPayAPI.InternetAcquiring.CardPayment.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.CardPayment.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :pay,
          amount: number,
          card: String.t(),
          card_cvv: String.t() | nil,
          card_exp_month: String.t() | nil,
          card_exp_year: String.t() | nil,
          currency: :eur | :uah | :usd,
          customer: String.t() | nil,
          dae: String.t() | nil,
          description: String.t(),
          info: String.t() | nil,
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          paytype: :apay | :apay_tavv | :gpay | :gpay_tavv | :tavv | nil,
          phone: String.t(),
          prepare: true | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t(),
          recurring: boolean | nil,
          recurringbytoken: true | nil,
          regular_payment:
            LiqPayAPI.InternetAcquiring.CardPayment.Request.RegularPayment.t() | nil,
          result_url: String.t() | nil,
          rro_info: LiqPayAPI.InternetAcquiring.CardPayment.Request.RROInfo.t() | nil,
          sender: LiqPayAPI.InternetAcquiring.CardPayment.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          split_rules: [LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRules.t()] | nil,
          tavv: String.t() | nil,
          tid: String.t() | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :card, :currency, :description, :ip, :order_id, :phone]
  defstruct [
    :amount,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :currency,
    :customer,
    :dae,
    :description,
    :info,
    :ip,
    :language,
    :order_id,
    :paytype,
    :phone,
    :prepare,
    :product_category,
    :product_description,
    :product_name,
    :product_url,
    :public_key,
    :recurring,
    :recurringbytoken,
    :regular_payment,
    :result_url,
    :rro_info,
    :sender,
    :server_url,
    :split_rules,
    :tavv,
    :tid,
    action: :pay,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, pay: "pay"}},
      amount: {"amount", :number},
      card: {"card", {:string, :generic}},
      card_cvv: {"card_cvv", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      customer: {"customer", {:string, :generic}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
      info: {"info", {:string, :generic}},
      ip: {"ip", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      paytype:
        {"paytype",
         {:enum,
          apay: "apay", apay_tavv: "apay_tavv", gpay: "gpay", gpay_tavv: "gpay_tavv", tavv: "tavv"}},
      phone: {"phone", {:string, :generic}},
      prepare: {"prepare", {:enum, true: "1"}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key: {"public_key", {:string, :generic}},
      recurring: {"recurring", :boolean},
      recurringbytoken: {"recurringbytoken", {:enum, true: "1"}},
      regular_payment:
        {"regular_payment", {LiqPayAPI.InternetAcquiring.CardPayment.Request.RegularPayment, :t}},
      result_url: {"result_url", {:string, :uri}},
      rro_info: {"rro_info", {LiqPayAPI.InternetAcquiring.CardPayment.Request.RROInfo, :t}},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.CardPayment.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      split_rules:
        {"split_rules", [{LiqPayAPI.InternetAcquiring.CardPayment.Request.SplitRules, :t}]},
      tavv: {"tavv", {:string, :generic}},
      tid: {"tid", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
