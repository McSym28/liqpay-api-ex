defmodule LiqPayAPI.InternetAcquiring.Widget.Request.RegularPayment do
  @moduledoc """
  Provides struct and type for a InternetAcquiring.Widget.Request.RegularPayment
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          subscribe: :"1" | nil,
          subscribe_date_start: DateTime.t(),
          subscribe_periodicity: :day | :month | :week | :year | nil
        }
  @type types :: :t

  @enforce_keys [:subscribe_date_start]
  defstruct [:subscribe, :subscribe_date_start, :subscribe_periodicity]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      subscribe: {"subscribe", {:enum, "1": "1"}},
      subscribe_date_start: {"subscribe_date_start", {:string, "date-time-liqpay"}},
      subscribe_periodicity:
        {"subscribe_periodicity", {:enum, day: "day", month: "month", week: "week", year: "year"}}
    ]
  end
end
