defmodule LiqPayAPI.InternetAcquiring.DCC.Response.DCCAllowed do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.DCC.Response.DCCAllowed
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          amount: number | nil,
          commission: number | nil,
          currency: String.t() | nil,
          rate: number | nil
        }
  @type types :: :t

  defstruct [:amount, :commission, :currency, :rate]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      amount: {"amount", :number},
      commission: {"commission", :number},
      currency: {"currency", {:string, :generic}},
      rate: {"rate", :number}
    ]
  end
end
