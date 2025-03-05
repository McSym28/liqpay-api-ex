defmodule LiqPayAPI.Information.PaymentArchive.Request do
  @moduledoc """
  Provides struct and type for a Information.PaymentArchive.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :reports,
          date_from: DateTime.t() | nil,
          date_to: DateTime.t() | nil,
          public_key: String.t(),
          resp_format: :csv | :json | :xml | nil,
          version: 3
        }
  @type types :: :t

  defstruct [:date_from, :date_to, :public_key, :resp_format, action: :reports, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, reports: "reports"}},
      date_from: {"date_from", {:integer, "timestamp-ms"}},
      date_to: {"date_to", {:integer, "timestamp-ms"}},
      public_key: {"public_key", {:string, :generic}},
      resp_format: {"resp_format", {:enum, csv: "csv", json: "json", xml: "xml"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
