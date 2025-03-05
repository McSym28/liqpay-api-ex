defmodule LiqPayAPI.Information.Register.CompensationReport.Response do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationReport.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{register_token: String.t(), result: String.t(), status: String.t()}
  @type types :: :t

  @enforce_keys [:register_token, :result, :status]
  defstruct [:register_token, :result, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      register_token: {"register_token", {:string, :generic}},
      result: {"result", {:string, :generic}},
      status: {"status", {:string, :generic}}
    ]
  end
end
