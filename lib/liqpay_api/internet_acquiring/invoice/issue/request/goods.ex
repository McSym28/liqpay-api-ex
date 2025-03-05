defmodule LiqPayAPI.InternetAcquiring.Invoice.Issue.Request.Goods do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Invoice.Issue.Request.Goods
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{amount: number, count: integer, name: String.t() | nil, unit: String.t()}
  @type types :: :t

  @enforce_keys [:amount, :count, :unit]
  defstruct [:amount, :count, :name, :unit]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      amount: {"amount", :number},
      count: {"count", :integer},
      name: {"name", {:string, :generic}},
      unit: {"unit", {:string, :generic}}
    ]
  end
end
