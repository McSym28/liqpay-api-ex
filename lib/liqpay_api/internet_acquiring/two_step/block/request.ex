defmodule LiqPayAPI.InternetAcquiring.TwoStep.Block.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.TwoStep.Block.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :hold,
          amount: number,
          apay_token: String.t() | nil,
          card: String.t() | nil,
          card_cvv: String.t() | nil,
          card_exp_month: String.t() | nil,
          card_exp_year: String.t() | nil,
          currency: :eur | :uah | :usd,
          customer: String.t() | nil,
          dae: String.t() | nil,
          description: String.t(),
          gpay_token: String.t() | nil,
          info: String.t() | nil,
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          paytype: :apay | :apay_tavv | :gpay | :gpay_tavv | :tavv | nil,
          phone: String.t(),
          prepare: true | nil,
          public_key: String.t(),
          recurring: boolean | nil,
          recurringbytoken: true | nil,
          sender: LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.Sender.t() | nil,
          server_url: String.t() | nil,
          split_rules: [LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.SplitRules.t()] | nil,
          tavv: String.t(),
          tid: String.t() | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:amount, :currency, :description, :ip, :order_id, :phone, :tavv]
  defstruct [
    :amount,
    :apay_token,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :currency,
    :customer,
    :dae,
    :description,
    :gpay_token,
    :info,
    :ip,
    :language,
    :order_id,
    :paytype,
    :phone,
    :prepare,
    :public_key,
    :recurring,
    :recurringbytoken,
    :sender,
    :server_url,
    :split_rules,
    :tavv,
    :tid,
    action: :hold,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, hold: "hold"}},
      amount: {"amount", :number},
      apay_token: {"apay_token", {:string, :generic}},
      card: {"card", {:string, :generic}},
      card_cvv: {"card_cvv", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      customer: {"customer", {:string, :generic}},
      dae: {"dae", {:string, :generic}},
      description: {"description", {:string, :generic}},
      gpay_token: {"gpay_token", {:string, :generic}},
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
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      recurring: {"recurring", :boolean},
      recurringbytoken: {"recurringbytoken", {:enum, true: "1"}},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      split_rules:
        {"split_rules", [{LiqPayAPI.InternetAcquiring.TwoStep.Block.Request.SplitRules, :t}]},
      tavv: {"tavv", {:string, :generic}},
      tid: {"tid", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
