defmodule LiqPayAPI.Information.Register.CompensationPerDay.Response.Data.OnlyCompensationId do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationPerDay.Response.Data.OnlyCompensationId
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{compensation_id: String.t() | nil, create_date: DateTime.t() | nil}
  @type types :: :t

  defstruct [:compensation_id, :create_date]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      compensation_id: {"compensation_id", {:string, :generic}},
      create_date: {"create_date", {:string, "date-time-liqpay"}}
    ]
  end
end
