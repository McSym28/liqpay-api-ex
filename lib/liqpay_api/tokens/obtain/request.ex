defmodule LiqPayAPI.Tokens.Obtain.Request do
  @moduledoc """
  Provides struct and type for a Tokens.Obtain.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :token_create | :token_create_unique,
          card_tokenization: LiqPayAPI.Tokens.Obtain.Request.CardTokenization.t() | nil,
          connect_control_tokenization:
            LiqPayAPI.Tokens.Obtain.Request.ConnectControlTokenization.t() | nil,
          is_credit: boolean,
          is_debit: boolean,
          public_key: String.t(),
          vceh_tokenization: LiqPayAPI.Tokens.Obtain.Request.VCEHTokenization.t() | nil,
          version: 3
        }
  @type types :: :t

  @enforce_keys [:action, :is_credit, :is_debit]
  defstruct [
    :action,
    :card_tokenization,
    :connect_control_tokenization,
    :is_credit,
    :is_debit,
    :public_key,
    :vceh_tokenization,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action:
        {"action",
         {:enum, token_create: "token_create", token_create_unique: "token_create_unique"}},
      card_tokenization:
        {"card_tokenization", {LiqPayAPI.Tokens.Obtain.Request.CardTokenization, :t}},
      connect_control_tokenization:
        {"connect_control_tokenization",
         {LiqPayAPI.Tokens.Obtain.Request.ConnectControlTokenization, :t}},
      is_credit: {"is_credit", :boolean},
      is_debit: {"is_debit", :boolean},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      vceh_tokenization:
        {"vceh_tokenization", {LiqPayAPI.Tokens.Obtain.Request.VCEHTokenization, :t}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
