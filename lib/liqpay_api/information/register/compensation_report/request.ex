defmodule LiqPayAPI.Information.Register.CompensationReport.Request do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationReport.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :reports_compensation_file,
          compensation_id: String.t() | nil,
          date: Date.t() | nil,
          public_key: String.t(),
          resp_format: :csv | nil,
          version: 3
        }
  @type types :: :t

  defstruct [
    :compensation_id,
    :date,
    :public_key,
    :resp_format,
    action: :reports_compensation_file,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, reports_compensation_file: "reports_compensation_file"}},
      compensation_id: {"compensation_id", {:string, :generic}},
      date: {"date", {:string, :date}},
      public_key: {"public_key", {:string, :generic}},
      resp_format: {"resp_format", {:enum, csv: "csv"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
