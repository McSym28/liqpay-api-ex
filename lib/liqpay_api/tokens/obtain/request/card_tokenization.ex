defmodule LiqPayAPI.Tokens.Obtain.Request.CardTokenization do
  @moduledoc """
  Provides struct and type for a Tokens.Obtain.Request.CardTokenization
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          card: String.t(),
          card_cvv: String.t(),
          card_exp_month: String.t(),
          card_exp_year: String.t()
        }
  @type types :: :t

  @enforce_keys [:card, :card_cvv, :card_exp_month, :card_exp_year]
  defstruct [:card, :card_cvv, :card_exp_month, :card_exp_year]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      card: {"card", {:string, :generic}},
      card_cvv: {"card_cvv", {:string, :generic}},
      card_exp_month: {"card_exp_month", {:string, :generic}},
      card_exp_year: {"card_exp_year", {:string, :generic}}
    ]
  end
end
