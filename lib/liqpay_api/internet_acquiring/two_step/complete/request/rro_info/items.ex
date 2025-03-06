defmodule LiqPayAPI.InternetAcquiring.TwoStep.Complete.Request.RROInfo.Items do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.TwoStep.Complete.Request.RROInfo.Items
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{amount: number, cost: number, id: integer, price: number}
  @type types :: :t

  @enforce_keys [:amount, :cost, :id, :price]
  defstruct [:amount, :cost, :id, :price]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      amount: {"amount", :number},
      cost: {"cost", :number},
      id: {"id", :integer},
      price: {"price", :number}
    ]
  end
end
