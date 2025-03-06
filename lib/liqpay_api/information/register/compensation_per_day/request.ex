defmodule LiqPayAPI.Information.Register.CompensationPerDay.Request do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationPerDay.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :reports_compensation,
          compensation_id: String.t() | nil,
          date: Date.t() | nil,
          public_key: String.t(),
          resp_format: :csv | :json | :xml | nil,
          version: 3
        }
  @type types :: :t

  defstruct [
    :compensation_id,
    :date,
    :public_key,
    :resp_format,
    action: :reports_compensation,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, reports_compensation: "reports_compensation"}},
      compensation_id: {"compensation_id", {:string, :generic}},
      date: {"date", {:string, :date}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      resp_format: {"resp_format", {:enum, csv: "csv", json: "json", xml: "xml"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
