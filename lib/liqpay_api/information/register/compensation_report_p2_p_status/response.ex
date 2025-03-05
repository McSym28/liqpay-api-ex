defmodule LiqPayAPI.Information.Register.CompensationReportP2PStatus.Response do
  @moduledoc """
  Provides struct and type for a Information.Register.CompensationReportP2PStatus.Response
  """

  @behaviour OpenAPIClient.Schema

  @type t :: %__MODULE__{
          filelink: String.t() | nil,
          result: :error | :ok | String.t() | nil,
          status: :error | :processing | :success | String.t() | nil
        }
  @type types :: :t

  defstruct [:filelink, :result, :status]

  @doc false
  @impl OpenAPIClient.Schema
  @spec __fields__(types()) :: keyword(OpenAPIClient.Schema.schema_type())
  def __fields__(:t) do
    [
      filelink: {"filelink", {:string, :uri}},
      result: {"result", {:enum, [{:error, "error"}, {:ok, "ok"}, :not_strict]}},
      status:
        {"status",
         {:enum,
          [{:error, "error"}, {:processing, "processing"}, {:success, "success"}, :not_strict]}}
    ]
  end
end
