defmodule LiqPayAPI.Confirmation.CardVerification.Request do
  @moduledoc """
  Provides struct and type for a Confirmation.CardVerification.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :cardverification,
          card: String.t(),
          card_cvv: String.t(),
          card_exp_month: String.t(),
          card_exp_year: String.t(),
          description: String.t(),
          ip: String.t(),
          language: :en | :uk | nil,
          order_id: String.t(),
          public_key: String.t(),
          verifycode: true | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:card, :card_cvv, :card_exp_month, :card_exp_year, :description, :ip, :order_id]
  defstruct [
    :card,
    :card_cvv,
    :card_exp_month,
    :card_exp_year,
    :description,
    :ip,
    :language,
    :order_id,
    :public_key,
    :verifycode,
    action: :cardverification,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, cardverification: "cardverification"}},
      card: {"card", {:string, :generic}},
      card_cvv: {"card_cvv", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}},
      description: {"description", {:string, :generic}},
      ip: {"ip", {:string, :generic}},
      language: {"language", {:enum, en: "en", uk: "uk"}},
      order_id: {"order_id", {:string, :generic}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      verifycode: {"verifycode", {:enum, true: "Y"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
