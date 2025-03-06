defmodule LiqPayAPI.Public.Archive.Response do
  @moduledoc """
  Provides struct and type for a Public.Archive.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          bank: String.t() | nil,
          base_currency: integer | nil,
          base_currency_lit: String.t() | nil,
          date: Date.t() | nil,
          exchange_rate: [LiqPayAPI.Public.Archive.Response.ExchangeRate.t()] | nil
        }
  @type types :: :t

  defstruct [:bank, :base_currency, :base_currency_lit, :date, :exchange_rate]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      bank: {"bank", {:string, :generic}},
      base_currency: {"baseCurrency", :integer},
      base_currency_lit: {"baseCurrencyLit", {:string, :generic}},
      date: {"date", {:string, "date-liqpay"}},
      exchange_rate: {"exchangeRate", [{LiqPayAPI.Public.Archive.Response.ExchangeRate, :t}]}
    ]
  end
end
