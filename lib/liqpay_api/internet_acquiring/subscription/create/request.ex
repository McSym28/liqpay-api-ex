defmodule LiqPayAPI.InternetAcquiring.Subscription.Create.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Subscription.Create.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :subscribe,
          amount: number,
          card: String.t(),
          card_cvv: String.t(),
          card_exp_month: String.t(),
          card_exp_year: String.t(),
          currency: :eur | :uah | :usd,
          customer: String.t() | nil,
          dae: String.t() | nil,
          description: String.t(),
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
          recurring: boolean | nil,
          recurringbytoken: true | nil,
          regular_payment:
            LiqPayAPI.InternetAcquiring.Subscription.Create.Request.RegularPayment.t() | nil,
          sender: LiqPayAPI.InternetAcquiring.Subscription.Create.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [
    :amount,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :currency,
    :description,
    :ip,
    :order_id,
    :phone
  ]
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
    :sender,
    :server_url,
    action: :subscribe,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, subscribe: "subscribe"}},
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
      phone: {"phone", {:string, :generic}},
      prepare: {"prepare", {:enum, true: "1"}},
      product_category: {"product_category", {:string, :generic}},
      product_description: {"product_description", {:string, :generic}},
      product_name: {"product_name", {:string, :generic}},
      product_url: {"product_url", {:string, :uri}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      recurring: {"recurring", :boolean},
      recurringbytoken: {"recurringbytoken", {:enum, true: "1"}},
      regular_payment:
        {"regular_payment",
         {LiqPayAPI.InternetAcquiring.Subscription.Create.Request.RegularPayment, :t}},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.Subscription.Create.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
