defmodule LiqPayAPI.Public.DiscountRate.Response do
  @moduledoc """
  Provides struct and type for a Public.DiscountRate.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{rate_date: Date.t() | nil, rate_value: number | nil}
  @type types :: :t

  defstruct [:rate_date, :rate_value]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [rate_date: {"rate_date", {:string, "date-liqpay"}}, rate_value: {"rate_value", :number}]
  end
end
