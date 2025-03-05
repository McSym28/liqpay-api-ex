defmodule LiqPayAPI.Public.Archive.Response.ExchangeRate do
  @moduledoc """
  Provides struct and type for a Public.Archive.Response.ExchangeRate
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          base_currency: String.t() | nil,
          currency: String.t() | nil,
          purchase_rate: number | nil,
          purchase_rate_nb: number | nil,
          sale_rate: number | nil,
          sale_rate_nb: number | nil
        }
  @type types :: :t

  defstruct [
    :base_currency,
    :currency,
    :purchase_rate,
    :purchase_rate_nb,
    :sale_rate,
    :sale_rate_nb
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      base_currency: {"baseCurrency", {:string, :generic}},
      currency: {"currency", {:string, :generic}},
      purchase_rate: {"purchaseRate", :number},
      purchase_rate_nb: {"purchaseRateNB", :number},
      sale_rate: {"saleRate", :number},
      sale_rate_nb: {"saleRateNB", :number}
    ]
  end
end
