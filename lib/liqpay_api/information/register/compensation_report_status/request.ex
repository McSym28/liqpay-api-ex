defmodule LiqPayAPI.Information.Register.CompensationReportStatus.Request do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationReportStatus.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :reports_compensation_file_status,
          public_key: String.t(),
          register_token: String.t(),
          version: 3
        }
  @type types :: :t

  @enforce_keys [:register_token]
  defstruct [:public_key, :register_token, action: :reports_compensation_file_status, version: 3]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      action:
        {"action", {:enum, reports_compensation_file_status: "reports_compensation_file_status"}},
      public_key: {"public_key", {:string, :generic}},
      register_token: {"register_token", {:string, :generic}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
