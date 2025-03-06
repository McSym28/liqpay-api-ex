defmodule LiqPayAPI.InternetAcquiring.DCC.Request do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.DCC.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :pay,
          amount: number,
          card: String.t(),
          card_cvv: String.t(),
          card_exp_month: String.t(),
          card_exp_year: String.t(),
          card_token: String.t(),
          currency: :eur | :uah | :usd,
          description: String.t(),
          is_dcc_debit: boolean | nil,
          language: :en | :uk | nil,
          order_id: String.t(),
          phone: String.t(),
          prepare: :tariffs | nil,
          public_key: String.t(),
          recurringbytoken: true | nil,
          result_url: String.t() | nil,
          sandbox: true,
          sender: LiqPayAPI.InternetAcquiring.DCC.Request.Sender.t() | nil,
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
    :card_token,
    :currency,
    :description,
    :order_id,
    :phone
  ]
  defstruct [
    :amount,
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :card_token,
    :currency,
    :description,
    :is_dcc_debit,
    :language,
    :order_id,
    :phone,
    :prepare,
    :public_key,
    :recurringbytoken,
    :result_url,
    :sender,
    :server_url,
    action: :pay,
    sandbox: true,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, pay: "pay"}},
      amount: {"amount", :number},
      card: {"card", {:string, :generic}},
      card_cvv: {"card_cvv", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}},
      card_token: {"card_token", {:string, :generic}},
      currency: {"currency", {:enum, eur: "EUR", uah: "UAH", usd: "USD"}},
      description: {"description", {:string, :generic}},
      is_dcc_debit: {"is_dcc_debit", :boolean},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      phone: {"phone", {:string, :generic}},
      prepare: {"prepare", {:enum, tariffs: "tariffs"}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      recurringbytoken: {"recurringbytoken", {:enum, true: "1"}},
      result_url: {"result_url", {:string, :uri}},
      sandbox: {"sandbox", {:enum, true: "1"}},
      sender: {"sender", {LiqPayAPI.InternetAcquiring.DCC.Request.Sender, :t}},
      server_url: {"server_url", {:string, :uri}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
