defmodule LiqPayAPI.InternetAcquiring.Checkout.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Checkout.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :hold | :pay | :paydonate | :subscribe,
          amount: number,
          currency: :eur | :uah | :usd,
          dae: String.t() | nil,
          description: String.t(),
          expired_date: DateTime.t() | nil,
          info: String.t() | nil,
          language: :en | :uk | nil,
          one_click_payment:
            LiqPayAPI.InternetAcquiring.Checkout.Request.OneClickPayment.t() | nil,
          order_id: String.t(),
          paytypes:
            :apay
            | :card
            | :cash
            | :gpay
            | :invoice
            | :moment_part
            | :paypart
            | :privat24
            | :qr
            | nil,
          product_category: String.t() | nil,
          product_description: String.t() | nil,
          product_name: String.t() | nil,
          product_url: String.t() | nil,
          public_key: String.t(),
          regular_payment: LiqPayAPI.InternetAcquiring.Checkout.Request.RegularPayment.t() | nil,
          result_url: String.t() | nil,
          rro_info: LiqPayAPI.InternetAcquiring.Checkout.Request.RROInfo.t() | nil,
          sender: LiqPayAPI.InternetAcquiring.Checkout.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          split_rules: [LiqPayAPI.InternetAcquiring.Checkout.Request.SplitRules.t()] | nil,
          verifycode: true | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:action, :amount, :currency, :description, :order_id]
  defstruct [
    :action,
    :amount,
    :currency,
    :dae,
    :description,
    :expired_date,
    :info,
    :language,
    :one_click_payment,
    :order_id,
    :paytypes,
    :product_category,
    :product_description,
    :product_name,
    :product_url,
    :public_key,
    :regular_payment,
    :result_url,
    :rro_info,
    :sender,
    :server_url,
    :split_rules,
    :verifycode,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action:
        {"action",
         {:enum, hold: "hold", pay: "pay", paydonate: "paydonate", subscribe: "subscribe"}},
      amount: {"amount", :number},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
      expired_date: {"expired_date", {:string, "date-time-liqpay"}},
      info: {"info", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      one_click_payment:
        {"one_click_payment", {LiqPayAPI.InternetAcquiring.Checkout.Request.OneClickPayment, :t}},
      order_id: {"order_id", {:string, :generic}},
      paytypes:
        {"paytypes",
         {:enum,
          apay: "apay",
          card: "card",
          cash: "cash",
          gpay: "gpay",
          invoice: "invoice",
          moment_part: "moment_part",
          paypart: "paypart",
          privat24: "privat24",
          qr: "qr"}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      regular_payment:
        {"regular_payment", {LiqPayAPI.InternetAcquiring.Checkout.Request.RegularPayment, :t}},
      result_url: {"result_url", {:string, :uri}},
      rro_info: {"rro_info", {LiqPayAPI.InternetAcquiring.Checkout.Request.RROInfo, :t}},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.Checkout.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      split_rules:
        {"split_rules", [{LiqPayAPI.InternetAcquiring.Checkout.Request.SplitRules, :t}]},
      verifycode: {"verifycode", {:enum, true: "Y"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
