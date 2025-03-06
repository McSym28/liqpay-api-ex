defmodule LiqPayAPI.Information.Register.CompensationReportP2P.Request do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationReportP2P.Request
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          action: :reports_compensation_file,
          date: Date.t() | nil,
          public_key: String.t(),
          resp_format: :csv | nil,
          type: :p2p | :p2pcredit | nil,
          version: 3
        }
  @type types :: :t

  defstruct [
    :date,
    :public_key,
    :resp_format,
    :type,
    action: :reports_compensation_file,
    version: 3
  ]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.field_type())
  def __fields__(:t) do
    [
      action: {"action", {:enum, reports_compensation_file: "reports_compensation_file"}},
      date: {"date", {:string, :date}},
      public_key:
        {"public_key", {:string, :generic},
         fn -> Application.get_env(:liqpay_api_ex, :public_key) end},
      resp_format: {"resp_format", {:enum, csv: "csv"}},
      type: {"type", {:enum, p2p: "p2p", p2pcredit: "p2pcredit"}},
      version: {"version", {:enum, [3]}}
    ]
  end
end
